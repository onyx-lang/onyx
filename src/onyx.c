// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxerrors.h"
#include "onyxparser.h"
#include "onyxutils.h"
#include "onyxwasm.h"
#include "onyxdoc.h"

#define VERSION "v0.1.0-beta"


#ifndef CORE_INSTALLATION
    #ifdef _BH_LINUX
    #define CORE_INSTALLATION "/usr/share/onyx"
    #elif defined(_WIN32) || defined(_WIN64)
    #define CORE_INSTALLATION "\\dev\\onyx\\"
    #endif
#endif




Context context;


static const char* docstring = "Onyx compiler version " VERSION "\n"
    "\n"
    "The compiler for the Onyx programming language, created by Brendan Hansen.\n"
    "\n"
    "Usage:\n"
    "\tonyx [-o <target file>] [--verbose] <input files>\n"
    // "\tonyx doc <input files>\n"
    "\tonyx help\n"
    "\n"
    "Flags:\n"
    "\t<input files>           List of initial files\n"
    "\t-o <target_file>        Specify the target file (default: out.wasm)\n"
    "\t--verbose               Verbose output\n";

static CompileOptions compile_opts_parse(bh_allocator alloc, int argc, char *argv[]) {
    CompileOptions options = {
        .allocator = alloc,
        .action = ONYX_COMPILE_ACTION_PRINT_HELP,

        .verbose_output          = 0,
        .fun_output              = 0,
        .print_function_mappings = 0,
        .use_post_mvp_features   = 0,

        .files = NULL,
        .target_file = "out.wasm",
    };

    bh_arr_new(alloc, options.files, 2);
    bh_arr_new(alloc, options.included_folders, 2);

    // NOTE: Add the current folder
    bh_arr_push(options.included_folders, CORE_INSTALLATION);
    bh_arr_push(options.included_folders, ".");

    if (argc == 1) return options;

    if (!strcmp(argv[1], "help")) options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
    // else if (!strcmp(argv[1], "doc")) {
    //     options.action = ONYX_COMPILE_ACTION_DOCUMENT;
    // }
    else options.action = ONYX_COMPILE_ACTION_COMPILE;

    if (options.action == ONYX_COMPILE_ACTION_COMPILE) {
        fori(i, 1, argc) {
            if (!strcmp(argv[i], "-o")) {
                options.target_file = argv[++i];
            }
            else if (!strcmp(argv[i], "--verbose") || !strcmp(argv[i], "-V")) {
                options.verbose_output = 1;
            }
            else if (!strcmp(argv[i], "-VV")) {
                options.verbose_output = 2;
            }
            else if (!strcmp(argv[i], "-VVV")) {
                options.verbose_output = 3;
            }
            else if (!strcmp(argv[i], "--print-function-mappings")) {
                options.print_function_mappings = 1;
            }
            else if (!strcmp(argv[i], "--use-post-mvp-features")) {
                options.use_post_mvp_features = 1;
            }
            else if (!strcmp(argv[i], "-I")) {
                bh_arr_push(options.included_folders, argv[++i]);
            }
#if defined(_BH_LINUX)
            // NOTE: Fun output is only enabled for Linux because Windows command line
            // is not ANSI compatible and for a silly feature, I don't want to learn
            // how to properly do arbitrary graphics in it.
            else if (!strcmp(argv[i], "--fun") || !strcmp(argv[i], "-F")) {
                options.fun_output = 1;
            }
#endif
            else {
                bh_arr_push(options.files, argv[i]);
            }
        }
    }

    return options;
}

static void compile_opts_free(CompileOptions* opts) {
    bh_arr_free(opts->files);
    bh_arr_free(opts->included_folders);
}





typedef enum CompilerProgress {
    ONYX_COMPILER_PROGRESS_ERROR,
    ONYX_COMPILER_PROGRESS_FAILED_OUTPUT,
    ONYX_COMPILER_PROGRESS_SUCCESS
} CompilerProgress;

static AstInclude* create_load(bh_allocator alloc, char* filename) {
    AstInclude* include_node = onyx_ast_node_new(alloc, sizeof(AstInclude), Ast_Kind_Load_File);
    include_node->name = filename;

    return include_node;
}

static void context_init(CompileOptions* opts) {
    context.options = opts;

    context.global_scope = scope_create(global_heap_allocator, NULL, (OnyxFilePos) { 0 });
    bh_table_init(global_heap_allocator, context.packages, 16);

    // NOTE: This will be initialized upon the first call to entity_heap_insert.
    context.entities.entities = NULL;

    onyx_errors_init(&context.loaded_files);

    context.token_alloc = global_heap_allocator;

    // NOTE: Create the arena where tokens and AST nodes will exist
    // Prevents nodes from being scattered across memory due to fragmentation
    bh_arena_init(&context.ast_arena, global_heap_allocator, 16 * 1024 * 1024); // 16MB
    context.ast_alloc = bh_arena_allocator(&context.ast_arena);

    context.wasm_module = bh_alloc_item(global_heap_allocator, OnyxWasmModule);
    *context.wasm_module = onyx_wasm_module_create(global_heap_allocator);

    // NOTE: Add builtin entities to pipeline.
    entity_heap_insert(&context.entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context.ast_alloc, "core/builtin"),
    }));
    
    add_entities_for_node((AstNode *) &builtin_stack_top, context.global_scope, NULL);

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files) {
        AstInclude* load_node = create_load(context.ast_alloc, (char *) *filename);
        add_entities_for_node((AstNode *) load_node, context.global_scope, NULL);
    }
}

static void context_free() {
    bh_arena_free(&context.ast_arena);
    bh_arr_free(context.loaded_files);

    compile_opts_free(context.options);
}

// NOTE: This should not be called until immediately before using the return value.
// This function can return a static variable which will change if this is called
// another time.                                        -brendanfh 2020/10/09
static char* lookup_included_file(char* filename) {
    static char path[256];
    fori (i, 0, 256) path[i] = 0;

    static char fn[128];
    fori (i, 0, 128) fn[i] = 0;

    if (!bh_str_ends_with(filename, ".onyx")) {
        bh_snprintf(fn, 128, "%s.onyx", filename);
    } else {
        bh_snprintf(fn, 128, "%s", filename);
    }

#if defined(_BH_LINUX)
    #define DIR_SEPARATOR '/'
#elif defined(_BH_WINDOWS)
    #define DIR_SEPARATOR '\\'
#endif

    fori (i, 0, 128) if (fn[i] == '/') fn[i] = DIR_SEPARATOR;

    bh_arr_each(const char *, folder, context.options->included_folders) {
        if ((*folder)[strlen(*folder) - 1] != DIR_SEPARATOR)
            bh_snprintf(path, 256, "%s%c%s", *folder, DIR_SEPARATOR, fn);
        else
            bh_snprintf(path, 256, "%s%s", *folder, fn);

        if (bh_file_exists(path)) return path;
    }

    return fn;

#undef DIR_SEPARATOR
}

static void parse_source_file(bh_file_contents* file_contents) {
    // :Remove passing the allocators as parameters
    OnyxTokenizer tokenizer = onyx_tokenizer_create(context.token_alloc, file_contents);
    onyx_lex_tokens(&tokenizer);

    OnyxParser parser = onyx_parser_create(context.ast_alloc, &tokenizer);
    onyx_parse(&parser);
    onyx_parser_free(&parser);
}

static void process_source_file(char* filename) {
    bh_arr_each(bh_file_contents, fc, context.loaded_files) {
        // CLEANUP: Add duplicate resolutions, such as
        //          ./foo and ./test/../foo
        // should be the same thing.
        if (!strcmp(fc->filename, filename)) return;
    }

    bh_file file;
    bh_file_error err = bh_file_open(&file, filename);
    if (err != BH_FILE_ERROR_NONE) {
        onyx_report_error((OnyxFilePos) { 0 }, "Failed to open file %s\n", filename);
        return;
    }

    bh_file_contents fc = bh_file_read_contents(context.token_alloc, &file);
    bh_file_close(&file);

    bh_arr_push(context.loaded_files, fc);

    if (context.options->verbose_output == 2)
        bh_printf("Processing source file:    %s (%d bytes)\n", file.filename, fc.length);

    parse_source_file(&fc);
}

static void process_load_entity(Entity* ent) {
    assert(ent->type == Entity_Type_Load_File || ent->type == Entity_Type_Load_Path);
    AstInclude* include = ent->include;

    if (include->kind == Ast_Kind_Load_File) {
        char* filename = lookup_included_file(include->name);
        char* formatted_name = bh_strdup(global_heap_allocator, filename);

        process_source_file(formatted_name);

    } else if (include->kind == Ast_Kind_Load_Path) {
        bh_arr_push(context.options->included_folders, include->name);
    }
}

static b32 process_entity(Entity* ent) {
    i32 changed = 1;

    if (context.options->verbose_output == 3) {
        if (ent->expr && ent->expr->token)
            printf("%s | %s | %s:%i:%i\n",
                entity_state_strings[ent->state],
                entity_type_strings[ent->type],
                ent->expr->token->pos.filename,
                ent->expr->token->pos.line,
                ent->expr->token->pos.column);
    }

    switch (ent->state) {
        case Entity_State_Parse_Builtin:
            process_load_entity(ent);
            ent->state = Entity_State_Finalized;

            if (onyx_has_errors()) return 0;

            initialize_builtins(context.ast_alloc);
            break;

        case Entity_State_Parse:
            process_load_entity(ent);
            ent->state = Entity_State_Finalized;
            break;

        case Entity_State_Resolve_Symbols: symres_entity(ent); break;
        case Entity_State_Check_Types:     check_entity(ent);  break;
        case Entity_State_Code_Gen:        emit_entity(ent);   break;

        default:
            changed = 0;
    }

    return changed;
}

// Just having fun with some visual output - brendanfh 2020/12/14
#if defined(_BH_LINUX)
static void output_dummy_progress_bar() {
    EntityHeap* eh = &context.entities;

    printf("\e[2;1H");
    for (i32 i = 0; i < Entity_State_Count - 1; i++) {
        printf("%20s (%4d) | ", entity_state_strings[i], eh->state_count[i]);
        
        printf("\e[0K");
        for (i32 c = 0; c < eh->state_count[i] * 50 / bh_arr_length(eh->entities); c++) printf("\xe2\x96\x88");
        printf("\n");
    }
}
#endif

static i32 onyx_compile() {
    u64 start_time = bh_time_curr();

    if (context.options->fun_output)
        printf("\e[2J");

    while (!bh_arr_is_empty(context.entities.entities)) {
        Entity ent = entity_heap_top(&context.entities);
        entity_heap_remove_top(&context.entities);

#if defined(_BH_LINUX)
        if (context.options->fun_output) {
            output_dummy_progress_bar();
            
            if (ent.expr->token) {
                OnyxFilePos pos = ent.expr->token->pos;
                printf("\e[0K%s on %s in %s:%d:%d\n", entity_state_strings[ent.state], entity_type_strings[ent.type], pos.filename, pos.line, pos.column);
            }
            
            // Slowing things down for the effect
            usleep(1000);
        }
#endif
        
        b32 changed = process_entity(&ent);

        if (onyx_has_errors()) return ONYX_COMPILER_PROGRESS_ERROR;

        if (changed && ent.state != Entity_State_Finalized)
            entity_heap_insert(&context.entities, ent);
    }

    // NOTE: Output to file
    bh_file output_file;
    if (bh_file_create(&output_file, context.options->target_file) != BH_FILE_ERROR_NONE)
        return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;

    if (context.options->verbose_output)
        bh_printf("Outputting to WASM file:   %s\n", output_file.filename);

    onyx_wasm_module_write_to_file(context.wasm_module, output_file);

    u64 duration = bh_time_duration(start_time);
    
    if (context.options->verbose_output > 0) {
        // TODO: Replace these with bh_printf when padded formatting is added.
        printf("\nStatistics:\n");
        printf("    Time taken: %lf seconds\n", (double) duration / 1000);
        printf("    Processed %ld lines (%f lines/second).\n", lexer_lines_processed, ((f32) 1000 * lexer_lines_processed) / (duration));
        printf("    Processed %ld tokens (%f tokens/second).\n", lexer_tokens_processed, ((f32) 1000 * lexer_tokens_processed) / (duration));
        printf("\n");
    }

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

int main(int argc, char *argv[]) {

    bh_scratch_init(&global_scratch, bh_heap_allocator(), 256 * 1024); // NOTE: 256 KiB
    global_scratch_allocator = bh_scratch_allocator(&global_scratch);

    bh_managed_heap_init(&global_heap);
    global_heap_allocator = bh_managed_heap_allocator(&global_heap);

    CompileOptions compile_opts = compile_opts_parse(global_heap_allocator, argc, argv);
    context_init(&compile_opts);

    CompilerProgress compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
    switch (compile_opts.action) {
        case ONYX_COMPILE_ACTION_PRINT_HELP: bh_printf(docstring); return 1;

        case ONYX_COMPILE_ACTION_COMPILE:
            compiler_progress = onyx_compile();
            break;

        default: break;
    }

    switch (compiler_progress) {
        case ONYX_COMPILER_PROGRESS_ERROR:
            onyx_errors_print();
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_OUTPUT:
            bh_printf_err("Failed to open file for writing: '%s'\n", compile_opts.target_file);
            break;

        case ONYX_COMPILER_PROGRESS_SUCCESS:
            break;
    }

    context_free();

    bh_scratch_free(&global_scratch);
    bh_managed_heap_free(&global_heap);

    return compiler_progress != ONYX_COMPILER_PROGRESS_SUCCESS;
}
