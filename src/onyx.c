// #define BH_DEBUG
#define BH_DEFINE
#define BH_NO_TABLE
#define STB_DS_IMPLEMENTATION
#include "bh.h"

#include "lex.h"
#include "errors.h"
#include "parser.h"
#include "utils.h"
#include "wasm_emit.h"
#include "doc.h"

#define VERSION "v0.1.0"


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
    "\tonyx compile [-o <target file>] [--verbose] <input files>\n"
#ifdef ENABLE_RUN_WITH_WASMER
    "\tonyx run <input files> -- <args>\n"
#endif
    // "\tonyx doc <input files>\n"
    "\tonyx help\n"
    "\n"
    "Flags:\n"
    "\t<input files>           List of initial files\n"
    "\t-o <target_file>        Specify the target file (default: out.wasm).\n"
    "\t--runtime, -r <runtime> Specifies a runtime. Can be: wasi, js, custom.\n"
    "\t--verbose, -V           Verbose output.\n"
    "\t           -VV          Very verbose output.\n"
    "\t           -VVV         Very very verbose output (to be used by compiler developers).\n"
    "\t--wasm-mvp              Use only WebAssembly MVP features.\n"
    "\t---multi-threaded       Enables multi-threading for this compilation.\n"
    "\t--doc <doc_file>\n"
    "\n"
    "Developer flags:\n"
    "\t--print-function-mappings Prints a mapping from WASM function index to source location.\n"
    "\t--print-static-if-results Prints the conditional result of each #if statement. Useful for debugging.\n"
    "\t--print-notes             Prints the location of notes throughout the loaded code.\n"
    "\t--no-colors               Disables colors in the error message.\n"
    "\t--no-file-contents        Disables '#file_contents' for security.\n"
    "\n";


static CompileOptions compile_opts_parse(bh_allocator alloc, int argc, char *argv[]) {
    CompileOptions options = {
        .allocator = alloc,
        .action = ONYX_COMPILE_ACTION_PRINT_HELP,

        .verbose_output          = 0,
        .fun_output              = 0,
        .print_function_mappings = 0,
        .no_file_contents        = 0,

        .use_post_mvp_features   = 1,
        .use_multi_threading     = 0,

        .runtime = Runtime_Onyx,

        .files = NULL,
        .target_file = "out.wasm",

        .documentation_file = NULL,

        .passthrough_argument_count = 0,
        .passthrough_argument_data  = NULL,
    };

    bh_arr_new(alloc, options.files, 2);
    bh_arr_new(alloc, options.included_folders, 2);

    // NOTE: Add the current folder
    bh_arr_push(options.included_folders, CORE_INSTALLATION);
    bh_arr_push(options.included_folders, ".");

    if (argc == 1) return options;
    i32 arg_parse_start = 1;

    if (!strcmp(argv[1], "help"))     options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
    if (!strcmp(argv[1], "compile")) {
        options.action = ONYX_COMPILE_ACTION_COMPILE;
        arg_parse_start = 2;
    }
    #ifdef ENABLE_RUN_WITH_WASMER
    else if (!strcmp(argv[1], "run")) {
        options.action = ONYX_COMPILE_ACTION_RUN;
        arg_parse_start = 2;
    }
    #endif
    else options.action = ONYX_COMPILE_ACTION_COMPILE;

    if (options.action != ONYX_COMPILE_ACTION_PRINT_HELP) {
        fori(i, arg_parse_start, argc) {
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
            else if (!strcmp(argv[i], "--print-static-if-results")) {
                options.print_static_if_results = 1;
            }
            else if (!strcmp(argv[i], "--print-notes")) {
                options.print_notes = 1;
            }
            else if (!strcmp(argv[i], "--no-colors")) {
                options.no_colors = 1;
            }
            else if (!strcmp(argv[i], "--no-file-contents")) {
                options.no_file_contents = 1;
            }
            else if (!strcmp(argv[i], "--wasm-mvp")) {
                options.use_post_mvp_features = 0;
            }
            else if (!strcmp(argv[i], "--multi-threaded")) {
                options.use_multi_threading = 1;
            }
            else if (!strcmp(argv[i], "-I")) {
                bh_arr_push(options.included_folders, argv[++i]);
            }
            else if (!strcmp(argv[i], "-r") || !strcmp(argv[i], "--runtime")) {
                i += 1;
                if      (!strcmp(argv[i], "onyx"))   options.runtime = Runtime_Onyx;
                else if (!strcmp(argv[i], "wasi"))   options.runtime = Runtime_Wasi;
                else if (!strcmp(argv[i], "js"))     options.runtime = Runtime_Js;
                else if (!strcmp(argv[i], "custom")) options.runtime = Runtime_Custom;
                else {
                    bh_printf("WARNING: '%s' is not a valid runtime. Defaulting to 'onyx'.\n", argv[i]);
                    options.runtime = Runtime_Wasi;
                }
            }
            else if (!strcmp(argv[i], "--doc")) {
                options.documentation_file = argv[++i];
            }
            else if (!strcmp(argv[i], "--")) {
                options.passthrough_argument_count = argc - i - 1;
                options.passthrough_argument_data  = &argv[i + 1];
                break;
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

    // NOTE: Always enable multi-threading for the Onyx runtime.
    if (options.runtime == Runtime_Onyx) {
        options.use_multi_threading = 1;
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

static OnyxToken implicit_load_token = { '#', 1, 0, { 0, 0, 0, 0, 0 } };
static AstInclude* create_load(bh_allocator alloc, char* filename) {
    AstInclude* include_node = onyx_ast_node_new(alloc, sizeof(AstInclude), Ast_Kind_Load_File);
    include_node->name = filename;
    include_node->token = &implicit_load_token;

    return include_node;
}

static void context_init(CompileOptions* opts) {
    types_init();

    context.options = opts;
    context.cycle_detected = 0;

    context.global_scope = scope_create(global_heap_allocator, NULL, (OnyxFilePos) { 0 });
    sh_new_arena(context.packages);

    // NOTE: This will be initialized upon the first call to entity_heap_insert.
    context.entities.next_id  = 0;
    context.entities.entities = NULL;

    onyx_errors_init(&context.loaded_files);

    context.token_alloc = global_heap_allocator;

    // NOTE: Create the arena where tokens and AST nodes will exist
    // Prevents nodes from being scattered across memory due to fragmentation
    bh_arena_init(&context.ast_arena, global_heap_allocator, 16 * 1024 * 1024); // 16MB
    context.ast_alloc = bh_arena_allocator(&context.ast_arena);

    context.wasm_module = bh_alloc_item(global_heap_allocator, OnyxWasmModule);
    *context.wasm_module = onyx_wasm_module_create(global_heap_allocator);

    entity_heap_init(&context.entities);

    // NOTE: Add builtin entities to pipeline.
    entity_heap_insert(&context.entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context.ast_alloc, "core/builtin"),
    }));

    entity_heap_insert(&context.entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context.ast_alloc, "core/type_info/type_info"),
    }));
    
    add_entities_for_node(NULL, (AstNode *) &builtin_stack_top, context.global_scope, NULL);
    add_entities_for_node(NULL, (AstNode *) &builtin_tls_base, context.global_scope, NULL);

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files) {
        AstInclude* load_node = create_load(context.ast_alloc, (char *) *filename);
        add_entities_for_node(NULL, (AstNode *) load_node, context.global_scope, NULL);
    }
}

static void context_free() {
    bh_arena_free(&context.ast_arena);
    bh_arr_free(context.loaded_files);

    compile_opts_free(context.options);
}

static void parse_source_file(bh_file_contents* file_contents) {
    // :Remove passing the allocators as parameters
    OnyxTokenizer tokenizer = onyx_tokenizer_create(context.token_alloc, file_contents);
    onyx_lex_tokens(&tokenizer);

    OnyxParser parser = onyx_parser_create(context.ast_alloc, &tokenizer);
    onyx_parse(&parser);
    onyx_parser_free(&parser);
}

static void process_source_file(char* filename, OnyxFilePos error_pos) {
    bh_arr_each(bh_file_contents, fc, context.loaded_files) {
        // Duplicates are detected here and since these filenames will be the full path,
        // string comparing them should be all that is necessary.
        if (!strcmp(fc->filename, filename)) return;
    }

    bh_file file;
    bh_file_error err = bh_file_open(&file, filename);
    if (err != BH_FILE_ERROR_NONE) {
        onyx_report_error(error_pos, "Failed to open file %s", filename);
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
        // :RelativeFiles
        const char* parent_file = include->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, global_scratch_allocator);
        
        char* filename = lookup_included_file(include->name, parent_folder, 1, 1);
        char* formatted_name = bh_strdup(global_heap_allocator, filename);

        process_source_file(formatted_name, include->token->pos);

    } else if (include->kind == Ast_Kind_Load_Path) {
        bh_arr_push(context.options->included_folders, include->name);
    }
}

static b32 process_entity(Entity* ent) {
    static char verbose_output_buffer[512];
    if (context.options->verbose_output == 3) {
        if (ent->expr && ent->expr->token)
            snprintf(verbose_output_buffer, 511,
                    "%20s | %24s (%d, %d) | %s:%i:%i \n",
                   entity_state_strings[ent->state],
                   entity_type_strings[ent->type],
                   (u32) ent->macro_attempts,
                   (u32) ent->micro_attempts,
                   ent->expr->token->pos.filename,
                   ent->expr->token->pos.line,
                   ent->expr->token->pos.column);
        
        else if (ent->expr)
            snprintf(verbose_output_buffer, 511,
                    "%20s | %24s (%d, %d) \n",
                   entity_state_strings[ent->state],
                   entity_type_strings[ent->type],
                   (u32) ent->macro_attempts,
                   (u32) ent->micro_attempts);
    }
    
    // CLEANUP: There should be a nicer way to track if the builtins have
    // already been initialized.
    static b32 builtins_initialized = 0;

    EntityState before_state = ent->state;
    switch (before_state) {
        case Entity_State_Error:
            if (ent->type != Entity_Type_Error) {
                onyx_report_error(ent->expr->token->pos, "Error entity unexpected. This is definitely a compiler bug");
            } else {
                onyx_report_error(ent->error->token->pos, "Static error occured: '%b'", ent->error->error_msg->text, ent->error->error_msg->length);
            }
            break;

        case Entity_State_Parse_Builtin:
            process_load_entity(ent);
            ent->state = Entity_State_Finalized;
            break;

        case Entity_State_Introduce_Symbols:
            // Currently, introducing symbols is handled in the symbol resolution
            // function. Maybe there should be a different place where that happens?
            symres_entity(ent);
            break;
        
        case Entity_State_Parse:
            if (!builtins_initialized) {
                builtins_initialized = 1;
                initialize_builtins(context.ast_alloc);
                introduce_build_options(context.ast_alloc);
            }
         
            process_load_entity(ent);
            ent->state = Entity_State_Finalized;
            break;

        case Entity_State_Resolve_Symbols: symres_entity(ent); break;
        case Entity_State_Check_Types:     check_entity(ent);  break;
        case Entity_State_Code_Gen:        emit_entity(ent);   break;
    }

    b32 changed = ent->state != before_state;
    if (context.options->verbose_output == 3) {
        if (changed) printf("SUCCESS to %20s | %s", entity_state_strings[ent->state], verbose_output_buffer);
        else         printf("YIELD   to %20s | %s", entity_state_strings[ent->state], verbose_output_buffer);
    }

    return changed;
}

// Just having fun with some visual output - brendanfh 2020/12/14
#if defined(_BH_LINUX)
static void output_dummy_progress_bar() {
    EntityHeap* eh = &context.entities;
    if (bh_arr_length(eh->entities) == 0) return;

    static const char* state_colors[] = {
        "\e[91m", "\e[93m", "\e[94m", "\e[93m", 
        "\e[97m", "\e[95m", "\e[96m", "\e[92m", "\e[91m",
    };

    printf("\e[2;1H");

    for (i32 i = 0; i < Entity_State_Count - 1; i++) {
        if (i % 2 == 0) printf("\n");
        printf("%s %25s \xe2\x96\x88 ", state_colors[i], entity_state_strings[i]);
    }

    printf("\n\n");
    
    for (i32 i = 0; i < Entity_Type_Count; i++) {
        if      (eh->type_count[i] == 0)           printf("\e[90m");
        else if ((i32) eh->entities[0]->type == i) printf("\e[92m");
        else                                       printf("\e[97m");

        printf("%25s (%4d) | ", entity_type_strings[i], eh->type_count[i]);

        printf("\e[0K");
        for (i32 j = 0; j < Entity_State_Count; j++) {
            if (eh->all_count[j][i] == 0) continue;

            printf("%s", state_colors[j]);

            i32 count = (eh->all_count[j][i] >> 5) + 1;
            for (i32 c = 0; c < count * 2; c++) printf("\xe2\x96\x88");

            printf("\e[0m");
        }
        printf("\n");
    }
}
#endif

static void dump_cycles() {
    context.cycle_detected = 1;
    Entity* ent;

    onyx_report_error((OnyxFilePos) { 0 }, "Cycle detected. Dumping all stuck processing units.");

    while (1) {
        ent = entity_heap_top(&context.entities);
        entity_heap_remove_top(&context.entities);
        if (ent->state < Entity_State_Code_Gen) process_entity(ent);
        else break;

        if (bh_arr_length(context.entities.entities) == 0) {
            break;
        }
    }
}

static i32 onyx_compile() {
    u64 start_time = bh_time_curr();

    if (context.options->fun_output)
        printf("\e[2J");

    while (!bh_arr_is_empty(context.entities.entities)) {
        Entity* ent = entity_heap_top(&context.entities);

#if defined(_BH_LINUX)
        if (context.options->fun_output) {
            output_dummy_progress_bar();
            
            if (ent->expr->token) {
                OnyxFilePos pos = ent->expr->token->pos;
                printf("\e[0K%s on %s in %s:%d:%d\n", entity_state_strings[ent->state], entity_type_strings[ent->type], pos.filename, pos.line, pos.column);
            }
            
            // Slowing things down for the effect
            usleep(1000);
        }
#endif

        entity_heap_remove_top(&context.entities);
        b32 changed = process_entity(ent);

        // NOTE: VERY VERY dumb cycle breaking. Basically, remember the first entity that did
        // not change (i.e. did not make any progress). Then everytime an entity doesn't change,
        // check if it is the same entity. If it is, it means all other entities that were processed
        // between the two occurences didn't make any progress either, and there must be a cycle.
        //                                                              - brendanfh 2021/02/06
        //
        // Because of the recent changes to the compiler architecture (again), this condition
        // does not always hold anymore. There can be nodes that get scheduled multiple times
        // before the "key" node that will unblock the progress. This means a more sophisticated
        // cycle detection algorithm must be used.
        //
        static Entity* watermarked_node = NULL;
        static u32 highest_watermark = 0;
        if (!changed) {
            if (!watermarked_node) {
                watermarked_node = ent;
                highest_watermark = bh_max(highest_watermark, ent->macro_attempts);
            }
            else if (watermarked_node == ent) {
                if (ent->macro_attempts > highest_watermark) {
                    entity_heap_insert_existing(&context.entities, ent);
                    dump_cycles();
                }
            }
            else if (watermarked_node->macro_attempts < ent->macro_attempts) {
                watermarked_node = ent;
                highest_watermark = bh_max(highest_watermark, ent->macro_attempts);
            }
        } else {
            watermarked_node = NULL;
        }

        if (onyx_has_errors()) return ONYX_COMPILER_PROGRESS_ERROR;

        if (ent->state != Entity_State_Finalized && ent->state != Entity_State_Failed)
            entity_heap_insert_existing(&context.entities, ent);
    }

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

static CompilerProgress onyx_flush_module() {

    // NOTE: Output to file
    bh_file output_file;
    if (bh_file_create(&output_file, context.options->target_file) != BH_FILE_ERROR_NONE)
        return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;

    if (context.options->verbose_output)
        bh_printf("Outputting to WASM file:   %s\n", output_file.filename);

    // APPARENTLY... the WebAssembly Threading proposal says that the data segment initializations
    // in a WASM module are copied into the linear memory EVERY time the module is instantiated, not
    // just the first time. This means that if we are happily chugging along and modifying global state
    // and then we spawn a thread, that thread will completely wipe all changes to the global and return
    // it to its original state. This is horrible obviously, but the only thing that is more horrible is
    // that the best way around this is to create a second WASM module that simply initializes the given
    // data section. Then have a section module that is actually your code. For right now, this is going
    // to be fine since the browser is really the only place that multi-threading can be used to any
    // degree of competency. But still... This is god awful and I hope that there is some other way to
    // around this down the line.
    if (context.options->use_multi_threading && !context.options->use_post_mvp_features) {
        bh_file data_file;
        if (bh_file_create(&data_file, bh_aprintf(global_scratch_allocator, "%s.data", context.options->target_file)) != BH_FILE_ERROR_NONE)
            return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;

        OnyxWasmModule* data_module = bh_alloc_item(global_heap_allocator, OnyxWasmModule);
        *data_module = onyx_wasm_module_create(global_heap_allocator);

        data_module->data = context.wasm_module->data;
        context.wasm_module->data = NULL;

        onyx_wasm_module_write_to_file(data_module, data_file);
        onyx_wasm_module_write_to_file(context.wasm_module, output_file);

    } else {
        onyx_wasm_module_write_to_file(context.wasm_module, output_file);
    }

    if (context.options->documentation_file != NULL) {
        OnyxDocumentation docs = onyx_docs_generate();
        docs.format = Doc_Format_Human;
        onyx_docs_emit(&docs, context.options->documentation_file);
    }

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

#ifdef ENABLE_RUN_WITH_WASMER
static b32 onyx_run() {
    bh_buffer code_buffer;
    onyx_wasm_module_write_to_buffer(context.wasm_module, &code_buffer);

    if (context.options->verbose_output > 0)
        bh_printf("Running program:\n");

    return onyx_run_wasm(code_buffer);
}
#endif

int main(int argc, char *argv[]) {

    bh_scratch_init(&global_scratch, bh_heap_allocator(), 256 * 1024); // NOTE: 256 KiB
    global_scratch_allocator = bh_scratch_allocator(&global_scratch);

    // SPEED: This used to be a managed heap allocator where all allocations
    // were tracked and would be automatically freed at the end of execution.
    // I don't know why I ever did that because that is the job of the operating
    // system when a process exits.
    global_heap_allocator = bh_heap_allocator();

    CompileOptions compile_opts = compile_opts_parse(global_heap_allocator, argc, argv);
    context_init(&compile_opts);

    CompilerProgress compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
    switch (compile_opts.action) {
        case ONYX_COMPILE_ACTION_PRINT_HELP: bh_printf(docstring); return 1;

        case ONYX_COMPILE_ACTION_COMPILE:
            compiler_progress = onyx_compile();
            if (compiler_progress == ONYX_COMPILER_PROGRESS_SUCCESS) {
                onyx_flush_module();
            }
            break;

        #ifdef ENABLE_RUN_WITH_WASMER
        case ONYX_COMPILE_ACTION_RUN:
            compiler_progress = onyx_compile();
            if (compiler_progress == ONYX_COMPILER_PROGRESS_SUCCESS) {
                if (!onyx_run()) {
                    compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
                }
            }
            break;
        #endif

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
    // bh_managed_heap_free(&global_heap);

    return compiler_progress != ONYX_COMPILER_PROGRESS_SUCCESS;
}
