// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxsempass.h"
#include "onyxutils.h"
#include "onyxwasm.h"

#define VERSION "0.1"

static const char* docstring = "Onyx compiler version " VERSION "\n"
    "\n"
    "The compiler for the Onyx programming language.\n"
    "\n"
    "Usage:\n"
    "\tonyx [-o <target file>] [-ast] [-verbose] <input files>\n"
    "\tonyx -help\n"
    "\nFlags:\n"
    "\t-o <target_file>        Specify the target file (default: out.wasm)\n"
    "\t-ast                    Print the abstract syntax tree after parsing\n"
    "\t-verbose                Verbose output\n"
    "\t-help                   Print this help message\n";

typedef enum CompileAction {
    ONYX_COMPILE_ACTION_COMPILE,
    ONYX_COMPILE_ACTION_CHECK_ERRORS,
    ONYX_COMPILE_ACTION_PRINT_HELP,
} CompileAction;

typedef struct OnyxCompileOptions {
    bh_allocator allocator;
    CompileAction action;

    u32 verbose_output : 1;
    u32 print_ast : 1;

    bh_arr(const char *) files;
    const char* target_file;
} OnyxCompileOptions;

static OnyxCompileOptions compile_opts_parse(bh_allocator alloc, int argc, char *argv[]) {
    OnyxCompileOptions options = {
        .allocator = alloc,
        .action = ONYX_COMPILE_ACTION_PRINT_HELP,

        .verbose_output = 0,
        .print_ast = 0,

        .files = NULL,
        .target_file = "out.wasm",
    };

    bh_arr_new(alloc, options.files, 1);

    fori(i, 1, argc - 1) {
        if (!strcmp(argv[i], "-help")) {
            options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
            break;
        }
        else if (!strcmp(argv[i], "-o")) {
            options.action = ONYX_COMPILE_ACTION_COMPILE;
            options.target_file = argv[++i];
        }
        else if (!strcmp(argv[i], "-ast")) {
            options.print_ast = 1;
        }
        else if (!strcmp(argv[i], "-verbose")) {
            options.verbose_output = 1;
        }
        else {
            options.action = ONYX_COMPILE_ACTION_COMPILE;
            bh_arr_push(options.files, argv[i]);
        }
    }

    return options;
}

static void compile_opts_free(OnyxCompileOptions* opts) {
    bh_arr_free(opts->files);
}





typedef enum CompilerProgress {
    ONYX_COMPILER_PROGRESS_FAILED_READ,
    ONYX_COMPILER_PROGRESS_FAILED_PARSE,
    ONYX_COMPILER_PROGRESS_FAILED_SEMPASS,
    ONYX_COMPILER_PROGRESS_FAILED_BINARY_GEN,
    ONYX_COMPILER_PROGRESS_FAILED_OUTPUT,
    ONYX_COMPILER_PROGRESS_SUCCESS
} CompilerProgress;

typedef struct CompilerState {
    OnyxCompileOptions* options;

    bh_arena                  ast_arena, msg_arena, sp_arena;
    bh_allocator token_alloc, ast_alloc, msg_alloc, sp_alloc;

    bh_table(bh_file_contents) loaded_files;
    bh_arr(const char *) queued_files;

    ProgramInfo prog_info;
    OnyxWasmModule wasm_mod;
} CompilerState;

static void compiler_state_init(CompilerState* compiler_state, OnyxCompileOptions* opts) {
    compiler_state->options = opts;

    program_info_init(&compiler_state->prog_info, global_heap_allocator);

    bh_arena_init(&compiler_state->msg_arena, opts->allocator, 4096);
    compiler_state->msg_alloc = bh_arena_allocator(&compiler_state->msg_arena);

    bh_table_init(opts->allocator, compiler_state->loaded_files, 15);

    onyx_message_init(compiler_state->msg_alloc, &compiler_state->loaded_files);

    compiler_state->token_alloc = opts->allocator;

    // NOTE: Create the arena where AST nodes will exist
    // Prevents nodes from being scattered across memory due to fragmentation
    bh_arena_init(&compiler_state->ast_arena, opts->allocator, 16 * 1024 * 1024); // 16MB
    compiler_state->ast_alloc = bh_arena_allocator(&compiler_state->ast_arena);

    bh_arena_init(&compiler_state->sp_arena, opts->allocator, 16 * 1024);
    compiler_state->sp_alloc = bh_arena_allocator(&compiler_state->sp_arena);

    bh_arr_new(opts->allocator, compiler_state->queued_files, 4);

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files)
        bh_arr_push(compiler_state->queued_files, (char *) *filename);
}

static void compiler_state_free(CompilerState* cs) {
    bh_arena_free(&cs->ast_arena);
    bh_arena_free(&cs->msg_arena);
    bh_arena_free(&cs->sp_arena);
    bh_table_free(cs->loaded_files);
    onyx_wasm_module_free(&cs->wasm_mod);
}





static ParseResults parse_source_file(CompilerState* compiler_state, bh_file_contents* file_contents) {
    // NOTE: Maybe don't want to recreate the tokenizer and parser for every file
    if (compiler_state->options->verbose_output)
        bh_printf("[Lexing]       %s\n", file_contents->filename);

    OnyxTokenizer tokenizer = onyx_tokenizer_create(compiler_state->token_alloc, file_contents);
    onyx_lex_tokens(&tokenizer);

    if (compiler_state->options->verbose_output)
        bh_printf("[Parsing]      %s\n", file_contents->filename);

    OnyxParser parser = onyx_parser_create(compiler_state->ast_alloc, &tokenizer, &compiler_state->prog_info);
    return onyx_parse(&parser);
}

static i32 sort_entities(const void* e1, const void* e2) {
    return ((Entity *)e1)->type - ((Entity *)e2)->type;
}

static void merge_parse_results(CompilerState* compiler_state, ParseResults* results) {
    bh_arr_each(AstIncludeFile *, include, results->files) {
        char* formatted_name = bh_aprintf(
                global_heap_allocator,
                "%b.onyx",
                (*include)->filename->text, (*include)->filename->length);

        bh_arr_push(compiler_state->queued_files, formatted_name);
    }

    Entity ent;
    bh_arr_each(NodeToProcess, n, results->nodes_to_process) {
        AstNode* node = n->node;
        AstKind nkind = node->kind;

        ent.package = n->package;

        switch (nkind) {
            case Ast_Kind_Function: {
                ent.type     = Entity_Type_Function_Header;
                ent.function = (AstFunction *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);

                ent.type     = Entity_Type_Function;
                ent.function = (AstFunction *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Overloaded_Function: {
                ent.type                = Entity_Type_Overloaded_Function;
                ent.overloaded_function = (AstOverloadedFunction *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Global: {
                ent.type   = Entity_Type_Global_Header;
                ent.global = (AstGlobal *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);

                ent.type   = Entity_Type_Global;
                ent.global = (AstGlobal *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_StrLit: {
                ent.type   = Entity_Type_String_Literal;
                ent.strlit = (AstStrLit *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Struct_Type: {
                ent.type = Entity_Type_Struct;
                ent.struct_type = (AstStructType *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Enum_Type: {
                ent.type = Entity_Type_Enum;
                ent.enum_type = (AstEnumType *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Use_Package: {
                ent.type = Entity_Type_Use_Package;
                ent.use_package = (AstUsePackage *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            default: {
                ent.type = Entity_Type_Expression;
                ent.expr = (AstTyped *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }
        }
    }

    qsort(compiler_state->prog_info.entities,
            bh_arr_length(compiler_state->prog_info.entities),
            sizeof(Entity),
            sort_entities);
}

static CompilerProgress process_source_file(CompilerState* compiler_state, char* filename) {
    if (bh_table_has(bh_file_contents, compiler_state->loaded_files, filename)) return ONYX_COMPILER_PROGRESS_SUCCESS;

    bh_file file;

    bh_file_error err = bh_file_open(&file, filename);
    if (err != BH_FILE_ERROR_NONE) {
        bh_printf_err("Failed to open file %s\n", filename);
        return ONYX_COMPILER_PROGRESS_FAILED_READ;
    }

    if (compiler_state->options->verbose_output)
        bh_printf("[Reading]      %s\n", file.filename);

    bh_file_contents fc = bh_file_read_contents(compiler_state->token_alloc, &file);
    bh_file_close(&file);

    // NOTE: Need to reget the value out of the table so token references work
    bh_table_put(bh_file_contents, compiler_state->loaded_files, (char *) filename, fc);
    fc = bh_table_get(bh_file_contents, compiler_state->loaded_files, (char *) filename);

    ParseResults results = parse_source_file(compiler_state, &fc);
    merge_parse_results(compiler_state, &results);

    if (onyx_message_has_errors()) {
        return ONYX_COMPILER_PROGRESS_FAILED_PARSE;
    } else {
        return ONYX_COMPILER_PROGRESS_SUCCESS;
    }
}


static i32 onyx_compile(CompilerState* compiler_state) {

    // NOTE: While the queue is not empty, process the next file
    while (!bh_arr_is_empty(compiler_state->queued_files)) {
        CompilerProgress result = process_source_file(compiler_state, (char *) compiler_state->queued_files[0]);

        if (result != ONYX_COMPILER_PROGRESS_SUCCESS)
            return result;

        bh_arr_fastdelete(compiler_state->queued_files, 0);
    }


    // NOTE: Check types and semantic rules
    if (compiler_state->options->verbose_output)
        bh_printf("[Checking semantics]\n");

    onyx_sempass_init(compiler_state->sp_alloc, compiler_state->ast_alloc);
    onyx_sempass(&compiler_state->prog_info);

    if (onyx_message_has_errors()) {
        return ONYX_COMPILER_PROGRESS_FAILED_SEMPASS;
    }


    // NOTE: Generate WASM instructions
    if (compiler_state->options->verbose_output)
        bh_printf("[Generating WASM]\n");

    compiler_state->wasm_mod = onyx_wasm_module_create(compiler_state->options->allocator);
    onyx_wasm_module_compile(&compiler_state->wasm_mod, &compiler_state->prog_info);

    if (onyx_message_has_errors()) {
        return ONYX_COMPILER_PROGRESS_FAILED_BINARY_GEN;
    }


    // NOTE: Output to file
    bh_file output_file;
    if (bh_file_create(&output_file, compiler_state->options->target_file) != BH_FILE_ERROR_NONE) {
        return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;
    }

    if (compiler_state->options->verbose_output)
        bh_printf("[Writing WASM] %s\n", output_file.filename);

    onyx_wasm_module_write_to_file(&compiler_state->wasm_mod, output_file);

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

int main(int argc, char *argv[]) {

    bh_scratch_init(&global_scratch, bh_heap_allocator(), 16 * 1024); // NOTE: 16 KB
    global_scratch_allocator = bh_scratch_allocator(&global_scratch);

    bh_managed_heap_init(&global_heap);
    global_heap_allocator = bh_managed_heap_allocator(&global_heap);

    OnyxCompileOptions compile_opts = compile_opts_parse(global_heap_allocator, argc, argv);

    CompilerState compile_state = { 0 };
    compiler_state_init(&compile_state, &compile_opts);

    CompilerProgress compiler_progress = ONYX_COMPILER_PROGRESS_FAILED_READ;

    switch (compile_opts.action) {
        case ONYX_COMPILE_ACTION_PRINT_HELP:
            // NOTE: This could probably be made better
            bh_printf(docstring);
            return 1;

        case ONYX_COMPILE_ACTION_COMPILE:
            compiler_progress = onyx_compile(&compile_state);
            break;

        default: break;
    }

    switch (compiler_progress) {
        case ONYX_COMPILER_PROGRESS_FAILED_READ:
            // NOTE: Do nothing since it was already printed above
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_PARSE:
        case ONYX_COMPILER_PROGRESS_FAILED_SEMPASS:
        case ONYX_COMPILER_PROGRESS_FAILED_BINARY_GEN:
            onyx_message_print();
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_OUTPUT:
            bh_printf_err("Failed to open file for writing: '%s'\n", compile_opts.target_file);
            break;

        case ONYX_COMPILER_PROGRESS_SUCCESS:
            if (compile_opts.verbose_output) bh_printf("Successfully compiled to '%s'\n", compile_opts.target_file);
            break;
    }

    compiler_state_free(&compile_state);

    bh_managed_heap_free(&global_heap);

    return compiler_progress != ONYX_COMPILER_PROGRESS_SUCCESS;
}

// NOTE: Old bits of code that may be useful again at some point.
#if 0
    bh_printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

    bh_arr_each(OnyxToken, it, token_arr) {
        onyx_token_null_toggle(*it);
        bh_printf("%s (%s:%l:%l)\n", onyx_get_token_type_name(it->type), it->pos.filename, it->pos.line, it->pos.column);
        onyx_token_null_toggle(*it);
    }
#endif

#if 0
    // NOTE: Ensure type table made correctly

    bh_printf("Type map:\n");
    bh_hash_each_start(i32, wasm_mod.type_map);
        bh_printf("%s -> %d\n", key, value);
    bh_hash_each_end;

    bh_printf("Type list:\n");
    WasmFuncType** func_type = wasm_mod.functypes;
    while (!bh_arr_end(wasm_mod.functypes, func_type)) {
        for (int p = 0; p < (*func_type)->param_count; p++) {
            bh_printf("%c ", (*func_type)->param_types[p]);
        }
        bh_printf("-> ");
        bh_printf("%c\n", (*func_type)->return_type);

        func_type++;
    }
#endif

#if 0
    // NOTE: Ensure the export table was built correctly

    bh_printf("Function types:\n");
    bh_arr_each(WasmFunc, func_it, wasm_mod.funcs) {
        bh_printf("%d\n", func_it->type_idx);
    }

    bh_printf("Exports:\n");
    bh_hash_each_start(WasmExport, wasm_mod.exports);
        bh_printf("%s: %d %d\n", key, value.kind, value.idx);
    bh_hash_each_end;
#endif
