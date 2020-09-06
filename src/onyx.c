// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxerrors.h"
#include "onyxparser.h"
#include "onyxsempass.h"
#include "onyxutils.h"
#include "onyxwasm.h"
#include "onyxdoc.h"

#define VERSION "0.1"


#ifndef CORE_INSTALLATION
    #ifdef __unix__
    #define CORE_INSTALLATION "/usr/share/onyx"
    #endif
#endif


static const char* docstring = "Onyx compiler version " VERSION "\n"
    "\n"
    "The compiler for the Onyx programming language.\n"
    "\n"
    "Usage:\n"
    "\tonyx [-o <target file>] [-verbose] <input files>\n"
    "\tonyx doc <input files>\n"
    "\tonyx -help\n"
    "\nFlags:\n"
    "\t-o <target_file>        Specify the target file (default: out.wasm)\n"
    "\t-ast                    Print the abstract syntax tree after parsing\n"
    "\t-verbose                Verbose output\n"
    "\t-help                   Print this help message\n";

typedef enum CompileAction {
    ONYX_COMPILE_ACTION_COMPILE,
    ONYX_COMPILE_ACTION_DOCUMENT,
    ONYX_COMPILE_ACTION_PRINT_HELP,
} CompileAction;

typedef struct OnyxCompileOptions {
    bh_allocator allocator;
    CompileAction action;

    u32 verbose_output : 1;

    bh_arr(const char *) included_folders;
    bh_arr(const char *) files;
    const char* target_file;
} OnyxCompileOptions;

static OnyxCompileOptions compile_opts_parse(bh_allocator alloc, int argc, char *argv[]) {
    OnyxCompileOptions options = {
        .allocator = alloc,
        .action = ONYX_COMPILE_ACTION_PRINT_HELP,

        .verbose_output = 0,

        .files = NULL,
        .target_file = "out.wasm",
    };

    bh_arr_new(alloc, options.files, 2);
    bh_arr_new(alloc, options.included_folders, 2);

    // NOTE: Add the current folder
    bh_arr_push(options.included_folders, CORE_INSTALLATION);
    bh_arr_push(options.included_folders, ".");

    fori(i, 1, argc) {
        if (!strcmp(argv[i], "doc") && i == 1) {
            options.action = ONYX_COMPILE_ACTION_DOCUMENT;
        }
        else if (!strcmp(argv[i], "-help")) {
            options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
            break;
        }
        else if (!strcmp(argv[i], "-o")) {
            options.target_file = argv[++i];
        }
        else if (!strcmp(argv[i], "-verbose")) {
            options.verbose_output = 1;
        }
        else if (!strcmp(argv[i], "-I")) {
            bh_arr_push(options.included_folders, argv[++i]);
        }
        else {
            if (options.action == ONYX_COMPILE_ACTION_PRINT_HELP)
                options.action = ONYX_COMPILE_ACTION_COMPILE;

            bh_arr_push(options.files, argv[i]);
        }
    }

    return options;
}

static void compile_opts_free(OnyxCompileOptions* opts) {
    bh_arr_free(opts->files);
    bh_arr_free(opts->included_folders);
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

    bh_arena                  ast_arena, sp_arena;
    bh_allocator token_alloc, ast_alloc, sp_alloc;

    bh_table(bh_file_contents) loaded_files;
    bh_arr(const char *) queued_files;

    ProgramInfo prog_info;
    OnyxWasmModule wasm_mod;
} CompilerState;

static char* lookup_included_file(CompilerState* cs, char* filename);

static void compiler_state_init(CompilerState* compiler_state, OnyxCompileOptions* opts) {
    compiler_state->options = opts;

    program_info_init(&compiler_state->prog_info, global_heap_allocator);

    bh_table_init(opts->allocator, compiler_state->loaded_files, 15);
    onyx_errors_init(&compiler_state->loaded_files);

    compiler_state->token_alloc = opts->allocator;

    // NOTE: Create the arena where AST nodes will exist
    // Prevents nodes from being scattered across memory due to fragmentation
    bh_arena_init(&compiler_state->ast_arena, opts->allocator, 16 * 1024 * 1024); // 16MB
    compiler_state->ast_alloc = bh_arena_allocator(&compiler_state->ast_arena);

    bh_arena_init(&compiler_state->sp_arena, opts->allocator, 16 * 1024);
    compiler_state->sp_alloc = bh_arena_allocator(&compiler_state->sp_arena);

    bh_arr_new(opts->allocator, compiler_state->queued_files, 4);

    bh_arr_push(compiler_state->queued_files, lookup_included_file(compiler_state, "core/builtin"));

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files)
        bh_arr_push(compiler_state->queued_files, (char *) *filename);
}

static void compiler_state_free(CompilerState* cs) {
    bh_arena_free(&cs->ast_arena);
    bh_arena_free(&cs->sp_arena);
    bh_table_free(cs->loaded_files);
    onyx_wasm_module_free(&cs->wasm_mod);
}



static char* lookup_included_file(CompilerState* cs, char* filename) {
    static char path[256];
    fori (i, 0, 256) path[i] = 0;

    static char fn[128];
    fori (i, 0, 128) fn[i] = 0;
    if (!bh_str_ends_with(filename, ".onyx")) {
        bh_snprintf(fn, 128, "%s.onyx", filename);
    } else {
        bh_snprintf(fn, 128, "%s", filename);
    }

    bh_arr_each(const char *, folder, cs->options->included_folders) {
        if ((*folder)[strlen(*folder) - 1] != '/')
            bh_snprintf(path, 256, "%s/%s", *folder, fn);
        else
            bh_snprintf(path, 256, "%s%s", *folder, fn);

        if (bh_file_exists(path)) return path;
    }

    return fn;
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

static void merge_parse_results(CompilerState* compiler_state, ParseResults* results) {
    bh_arr_each(AstInclude *, include, results->includes) {
        if ((*include)->kind == Ast_Kind_Include_File) {
            token_toggle_end((*include)->name);
            char* filename = lookup_included_file(compiler_state, (*include)->name->text);
            token_toggle_end((*include)->name);

            char* formatted_name = bh_strdup(global_heap_allocator, filename);
            bh_arr_push(compiler_state->queued_files, formatted_name);
        } else if ((*include)->kind == Ast_Kind_Include_Folder) {
            const char* folder = bh_aprintf(global_heap_allocator, "%b", (*include)->name->text, (*include)->name->length);
            bh_arr_push(compiler_state->options->included_folders, folder);
        }
    }

    Entity ent;
    bh_arr_each(NodeToProcess, n, results->nodes_to_process) {
        AstNode* node = n->node;
        AstKind nkind = node->kind;

        ent.package = n->package;

        switch (nkind) {
            case Ast_Kind_Function: {
                if ((node->flags & Ast_Flag_Foreign) != 0) {
                    ent.type     = Entity_Type_Foreign_Function_Header;
                    ent.function = (AstFunction *) node;
                    bh_arr_push(compiler_state->prog_info.entities, ent);

                } else {
                    ent.type     = Entity_Type_Function_Header;
                    ent.function = (AstFunction *) node;
                    bh_arr_push(compiler_state->prog_info.entities, ent);

                    ent.type     = Entity_Type_Function;
                    ent.function = (AstFunction *) node;
                    bh_arr_push(compiler_state->prog_info.entities, ent);
                }
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

            case Ast_Kind_File_Contents: {
                ent.type = Entity_Type_File_Contents;
                ent.file_contents = (AstFileContents *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Type_Alias:
            case Ast_Kind_Struct_Type: {
                ent.type = Entity_Type_Type_Alias;
                ent.type_alias = (AstType *) node;
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

            case Ast_Kind_Memres: {
                ent.type = Entity_Type_Memory_Reservation;
                ent.mem_res = (AstMemRes *) node;
                bh_arr_push(compiler_state->prog_info.entities, ent);
                break;
            }

            case Ast_Kind_Polymorphic_Proc: {
                ent.type = Entity_Type_Polymorphic_Proc;
                ent.poly_proc = (AstPolyProc *) node;
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

    // POTENTIAL BUG: If there are too many files and too many collisions in the table,
    // there is a chance that the inner arrays of the table will be repositioned. That
    // would completely break the pointer taken here, which would break all references
    // to file contents anywhere else in the program.
    //
    // A good solution would be to not use a table and just use a array of char* and
    // ensure that the filename is not in that list.
    //                                                      - brendanfh 2020/09/03


    // NOTE: Need to reget the value out of the table so token references work
    bh_table_put(bh_file_contents, compiler_state->loaded_files, (char *) filename, fc);
    fc = bh_table_get(bh_file_contents, compiler_state->loaded_files, (char *) filename);

    ParseResults results = parse_source_file(compiler_state, &fc);
    merge_parse_results(compiler_state, &results);

    if (onyx_has_errors()) {
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

    initialize_builtins(compiler_state->ast_alloc, &compiler_state->prog_info);
    if (onyx_has_errors()) {
        return ONYX_COMPILER_PROGRESS_FAILED_SEMPASS;
    }

    // Add builtin one-time entities
    bh_arr_push(compiler_state->prog_info.entities, ((Entity) {
        .type = Entity_Type_Global_Header,
        .global = &builtin_stack_top
    }));
    bh_arr_push(compiler_state->prog_info.entities, ((Entity) {
        .type = Entity_Type_Global,
        .global = &builtin_stack_top
    }));

    qsort(compiler_state->prog_info.entities,
            bh_arr_length(compiler_state->prog_info.entities),
            sizeof(Entity),
            sort_entities);

    // NOTE: Check types and semantic rules
    if (compiler_state->options->verbose_output)
        bh_printf("[Checking semantics]\n");

    onyx_sempass_init(compiler_state->sp_alloc, compiler_state->ast_alloc);
    onyx_sempass(&compiler_state->prog_info);

    if (onyx_has_errors()) {
        return ONYX_COMPILER_PROGRESS_FAILED_SEMPASS;
    }

    if (compiler_state->options->action == ONYX_COMPILE_ACTION_DOCUMENT) {
        OnyxDocumentation docs = onyx_docs_generate(&compiler_state->prog_info);
        docs.format = Doc_Format_Tags;
        onyx_docs_emit(&docs);

        return ONYX_COMPILER_PROGRESS_SUCCESS;
    }

    // NOTE: Generate WASM instructions
    if (compiler_state->options->verbose_output)
        bh_printf("[Generating WASM]\n");

    compiler_state->wasm_mod = onyx_wasm_module_create(compiler_state->options->allocator);
    onyx_wasm_module_compile(&compiler_state->wasm_mod, &compiler_state->prog_info);

    if (onyx_has_errors()) {
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
        case ONYX_COMPILE_ACTION_DOCUMENT:
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
            onyx_errors_print();
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_OUTPUT:
            bh_printf_err("Failed to open file for writing: '%s'\n", compile_opts.target_file);
            break;

        case ONYX_COMPILER_PROGRESS_SUCCESS:
            bh_printf("Successfully compiled to '%s'\n", compile_opts.target_file);
            break;
    }

    compiler_state_free(&compile_state);

    bh_scratch_free(&global_scratch);
    bh_managed_heap_free(&global_heap);

    return compiler_progress != ONYX_COMPILER_PROGRESS_SUCCESS;
}
