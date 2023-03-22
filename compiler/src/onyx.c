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


Context context;

#define DOCSTRING_HEADER "Onyx toolchain version " VERSION "\n" \
    "\n" \
    "The toolchain for the Onyx programming language, created by Brendan Hansen.\n" \
    "\n"



static const char* top_level_docstring = DOCSTRING_HEADER
    "Usage:\n"
    "\tonyx <subcommand>\n"
    "\n"
    "Subcommands:\n"
    "\thelp      Shows this help message. Use \"onyx help <subcommand>\".\n"
    "\tbuild     Compiles an Onyx program into an executable.\n"
#ifdef ENABLE_RUN_WITH_WASMER
    "\trun       Compiles and runs an Onyx program, all at once.\n"
#endif
    "\tcheck     Checks syntax and types of an Onyx program.\n"
    "\tpackage   Package manager\n";
    // "\tdoc <input files>\n"

static const char *build_docstring = DOCSTRING_HEADER
    "Usage:\n"
    "\tonyx %s <input files> [-o target_file] OPTIONS\n"
    "\n"
    "Required:\n"
    "\t<input files>           One or more Onyx files to include in the program.\n"
    "\n"
    "Options:\n"
    "\t-o <target_file>        Specify the target file (default: out.wasm).\n"
    "\t   --output <target_file>\n"
    "\t-I <dir>                Include a directory in the search path.\n"
    "\t--runtime, -r <runtime> Specifies the runtime. Can be: onyx, wasi, js, custom.\n"
    "\t                        (default: onyx)\n"
    "\t--verbose, -V           Verbose output.\n"
    "\t           -VV          Very verbose output.\n"
    "\t           -VVV         Very very verbose output (to be used by compiler developers).\n"
    "\t--no-std                Disable automatically including \"core/std\".\n"
    "\t--wasm-mvp              Use only WebAssembly MVP features.\n"
    "\t--multi-threaded        Enables multi-threading for this compilation.\n"
    "\t                        Automatically enabled for \"onyx\" runtime.\n"
    "\t--tag                   Generates a C-Tag file.\n"
    "\t--syminfo <target_file> Generates a symbol resolution information file. Used by onyx-lsp.\n"
    "\t--doc <doc_file>\n"
    "\t--generate-foreign-info\n"
    "\n"
    "Developer options:\n"
    "\t--no-colors               Disables colors in the error message.\n"
    "\t--no-file-contents        Disables '#file_contents' for security.\n"
    "\t--print-function-mappings Prints a mapping from WASM function index to source location.\n"
    "\t--print-static-if-results Prints the conditional result of each #if statement. Useful for debugging.\n"
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
        .no_std                  = 0,

        .runtime = Runtime_Onyx,

        .files = NULL,
        .target_file = "out.wasm",

        .documentation_file = NULL,
        .symbol_info_file   = NULL,
        .help_subcommand    = NULL,

        .debug_enabled = 0,

        .passthrough_argument_count = 0,
        .passthrough_argument_data  = NULL,

        .generate_tag_file = 0,
        .generate_symbol_info_file = 0,
    };

    bh_arr_new(alloc, options.files, 2);
    bh_arr_new(alloc, options.included_folders, 2);

    char* core_installation;
    #ifdef _BH_LINUX
    core_installation = CORE_INSTALLATION;
    #endif
    #ifdef _BH_WINDOWS
    core_installation = bh_alloc_array(global_heap_allocator, u8, 512);
    GetEnvironmentVariableA("ONYX_PATH", core_installation, 512);
    #endif

    // NOTE: Add the current folder
    bh_arr_push(options.included_folders, core_installation);
    bh_arr_push(options.included_folders, ".");

    if (argc == 1) return options;
    i32 arg_parse_start = 1;

    if (!strcmp(argv[1], "help")) {
        options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
        options.help_subcommand = argc > 2 ? argv[2] : NULL;
    }
    else if (!strcmp(argv[1], "compile") || !strcmp(argv[1], "build")) {
        options.action = ONYX_COMPILE_ACTION_COMPILE;
        arg_parse_start = 2;
    }
    else if (!strcmp(argv[1], "check")) {
        options.action = ONYX_COMPILE_ACTION_CHECK;
        arg_parse_start = 2;
    }
    else if (!strcmp(argv[1], "pkg") || !strcmp(argv[1], "package")) {
        options.action = ONYX_COMPILE_ACTION_RUN;
        options.passthrough_argument_count = argc - 2;
        options.passthrough_argument_data  = &argv[2];
        arg_parse_start = argc;

        bh_arr_push(options.files, bh_aprintf(global_heap_allocator, "%s/tools/onyx-pkg.onyx", core_installation));
    }
    #ifdef ENABLE_RUN_WITH_WASMER
    else if (!strcmp(argv[1], "run")) {
        options.action = ONYX_COMPILE_ACTION_RUN;
        arg_parse_start = 2;
    }
    #endif
    else {
        bh_printf("Unknown subcommand: '%s'\n", argv[1]);
        bh_printf("Run \"onyx help\" for valid subcommands.\n");
        exit(1);
    }

    if (options.action != ONYX_COMPILE_ACTION_PRINT_HELP) {
        fori(i, arg_parse_start, argc) {
            if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output")) {
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
            else if (!strcmp(argv[i], "--generate-foreign-info")) {
                options.generate_foreign_info = 1;
            }
            else if (!strcmp(argv[i], "--no-std")) {
                options.no_std = 1;
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
                    options.runtime = Runtime_Onyx;
                }
            }
            else if (!strcmp(argv[i], "--doc")) {
                options.documentation_file = argv[++i];
            }
            else if (!strcmp(argv[i], "--tag")) {
                options.generate_tag_file = 1;
            }
            else if (!strcmp(argv[i], "--syminfo")) {
                options.generate_symbol_info_file = 1;
                options.symbol_info_file = argv[++i];
            }
            else if (!strcmp(argv[i], "--debug")) {
                options.debug_enabled = 1;
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
                if (bh_str_ends_with(argv[i], ".wasm") && options.action == ONYX_COMPILE_ACTION_RUN) {
                    if (bh_arr_length(options.files) > 0) {
                        bh_printf("Expected only one '.wasm', or multiple '.onyx' files to be given, not a mixture.\n");
                        exit(1);
                    }

                    options.action = ONYX_COMPILE_ACTION_RUN_WASM;
                    options.target_file = argv[i];

                    options.passthrough_argument_count = argc - i - 1;
                    options.passthrough_argument_data  = &argv[i + 1];
                    break;

                } else {
                    bh_arr_push(options.files, argv[i]);
                }
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

static void print_subcommand_help(const char *subcommand) {
    if (!strcmp(subcommand, "build")
        || !strcmp(subcommand, "run")
        || !strcmp(subcommand, "check")) {
        bh_printf(build_docstring, subcommand);
    }

    else  {
        bh_printf("Unknown subcommand: '%s'\n", subcommand);
        bh_printf("Run \"onyx help\" for valid subcommands.\n");
        exit(1);
    }
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

// HACK
static u32 special_global_entities_remaining = 3;
static Entity *runtime_info_types_entity;
static Entity *runtime_info_foreign_entity;
static Entity *runtime_info_proc_tags_entity;

static void context_init(CompileOptions* opts) {
    types_init();

    context.options = opts;
    context.cycle_detected = 0;
    context.cycle_almost_detected = 0;

    OnyxFilePos internal_location = { 0 };
    internal_location.filename = "<compiler internal>";
    internal_location.line     = 1;
    internal_location.column   = 1;
    context.global_scope = scope_create(global_heap_allocator, NULL, internal_location);
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
        .include = create_load(context.ast_alloc, "core/runtime/build_opts"),
    }));

    if (context.options->runtime != Runtime_Custom) {
        runtime_info_types_entity = entity_heap_insert(&context.entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context.ast_alloc, "core/runtime/info/types"),
        }));
        runtime_info_foreign_entity = entity_heap_insert(&context.entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context.ast_alloc, "core/runtime/info/foreign_blocks"),
        }));
        runtime_info_proc_tags_entity = entity_heap_insert(&context.entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context.ast_alloc, "core/runtime/info/proc_tags"),
        }));
    }

    add_entities_for_node(NULL, (AstNode *) &builtin_stack_top, context.global_scope, NULL);
    add_entities_for_node(NULL, (AstNode *) &builtin_heap_start, context.global_scope, NULL);
    add_entities_for_node(NULL, (AstNode *) &builtin_tls_base, context.global_scope, NULL);
    add_entities_for_node(NULL, (AstNode *) &builtin_tls_size, context.global_scope, NULL);

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files) {
        AstInclude* load_node = create_load(context.ast_alloc, (char *) *filename);
        add_entities_for_node(NULL, (AstNode *) load_node, context.global_scope, NULL);
    }

    if (!context.options->no_std) {
        entity_heap_insert(&context.entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context.ast_alloc, "core/std"),
        }));
    }

    if (context.options->generate_symbol_info_file) {
        context.symbol_info = bh_alloc_item(global_heap_allocator, SymbolInfoTable);
        bh_imap_init(&context.symbol_info->node_to_id, global_heap_allocator, 512);
        bh_arr_new(global_heap_allocator, context.symbol_info->symbols, 128);
        bh_arr_new(global_heap_allocator, context.symbol_info->symbols_resolutions, 128);
        sh_new_arena(context.symbol_info->files);
    }

    if (context.options->documentation_file) {
        context.doc_info = bh_alloc_item(global_heap_allocator, OnyxDocInfo);
        memset(context.doc_info, 0, sizeof(OnyxDocInfo));
        bh_arr_new(global_heap_allocator, context.doc_info->procedures, 128);
        bh_arr_new(global_heap_allocator, context.doc_info->structures, 128);
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

    file_contents->line_count = tokenizer.line_number;

    OnyxParser parser = onyx_parser_create(context.ast_alloc, &tokenizer);
    onyx_parse(&parser);
    onyx_parser_free(&parser);
}

static b32 process_source_file(char* filename, OnyxFilePos error_pos) {
    bh_arr_each(bh_file_contents, fc, context.loaded_files) {
        // Duplicates are detected here and since these filenames will be the full path,
        // string comparing them should be all that is necessary.
        if (!strcmp(fc->filename, filename)) return 1;
    }

    bh_file file;
    bh_file_error err = bh_file_open(&file, filename);
    if (err != BH_FILE_ERROR_NONE) {
        if (context.cycle_detected) {
            onyx_report_error(error_pos, Error_Critical, "Failed to open file %s", filename);
        }
        return 0;
    }

    bh_file_contents fc = bh_file_read_contents(context.token_alloc, &file);
    bh_file_close(&file);

    bh_arr_push(context.loaded_files, fc);

    if (context.options->verbose_output == 2)
        bh_printf("Processing source file:    %s (%d bytes)\n", file.filename, fc.length);

    parse_source_file(&bh_arr_last(context.loaded_files));
    return 1;
}

static b32 process_load_entity(Entity* ent) {
    assert(ent->type == Entity_Type_Load_File || ent->type == Entity_Type_Load_Path);
    AstInclude* include = ent->include;

    if (include->kind == Ast_Kind_Load_File) {
        // :RelativeFiles
        const char* parent_file = include->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, global_scratch_allocator);

        char* filename = bh_lookup_file(include->name, parent_folder, ".onyx", 1, context.options->included_folders, 1);
        char* formatted_name = bh_strdup(global_heap_allocator, filename);

        return process_source_file(formatted_name, include->token->pos);

    } else if (include->kind == Ast_Kind_Load_All) {
        const char* parent_file = include->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, global_scratch_allocator);
        char folder[512];
        if (bh_str_starts_with(include->name, "./")) {
            bh_snprintf(folder, 511, "%s/%s", parent_folder, include->name + 2);
        } else {
            bh_snprintf(folder, 511, "%s", include->name);
        }

        // This does not take into account #load_path'd folders...

        bh_path_convert_separators(folder);
        bh_dir dir = bh_dir_open(folder);
        if (dir == NULL) {
            onyx_report_error(include->token->pos, Error_Critical, "Could not find folder '%s'.", folder);
            return 0;
        }

        bh_dirent entry;
        b32 success = 1;
        char fullpath[512];
        while (bh_dir_read(dir, &entry)) {
            if (entry.type == BH_DIRENT_FILE && bh_str_ends_with(entry.name, ".onyx")) {
                bh_snprintf(fullpath, 511, "%s/%s", folder, entry.name);
                bh_path_convert_separators(fullpath);
                u8* formatted_name = bh_path_get_full_name(fullpath, global_heap_allocator);
                success = process_source_file(formatted_name, include->token->pos);
                if (!success) break;
            }
        }

        bh_dir_close(dir);
        return success;

    } else if (include->kind == Ast_Kind_Load_Path) {
        bh_arr_push(context.options->included_folders, include->name);

    } else if (include->kind == Ast_Kind_Library_Path) {
        bh_arr_push(context.wasm_module->library_paths, include->name);
    }

    return 1;
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
                onyx_report_error(ent->expr->token->pos, Error_Critical, "Error entity unexpected. This is definitely a compiler bug");
            } else {
                onyx_report_error(ent->error->token->pos, Error_Critical, "Static error occured: '%b'", ent->error->error_msg->text, ent->error->error_msg->length);
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

            // GROSS
            if (special_global_entities_remaining == 0) {
                special_global_entities_remaining--;
                initalize_special_globals();
            }

            if (process_load_entity(ent)) {
                // GROSS
                if (ent == runtime_info_types_entity
                    || ent == runtime_info_proc_tags_entity
                    || ent == runtime_info_foreign_entity) {
                    special_global_entities_remaining--;
                }

                ent->state = Entity_State_Finalized;
            } else {
                ent->macro_attempts++;
            }
            break;

        case Entity_State_Resolve_Symbols: symres_entity(ent); break;
        case Entity_State_Check_Types:     check_entity(ent);  break;

        case Entity_State_Code_Gen: {
            if (context.options->action == ONYX_COMPILE_ACTION_CHECK) {
                ent->state = Entity_State_Finalized;
                break;
            }

            emit_entity(ent);
            break;
        }
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

        /*
        struct timespec spec;
        clock_gettime(CLOCK_REALTIME, &spec);
        u64 nano_time = spec.tv_nsec + 1000000000 * (spec.tv_sec % 100);
        printf("%lu %d %d %d %d %d %d %d\n",
                nano_time,
                bh_arr_length(context.entities.entities),
                context.entities.state_count[Entity_State_Introduce_Symbols],
                context.entities.state_count[Entity_State_Parse],
                context.entities.state_count[Entity_State_Resolve_Symbols],
                context.entities.state_count[Entity_State_Check_Types],
                context.entities.state_count[Entity_State_Code_Gen],
                context.entities.state_count[Entity_State_Finalized]);
        */

        // Mostly a preventative thing to ensure that even if somehow
        // errors were left disabled, they are re-enabled in this cycle.
        onyx_errors_enable();

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

                    if (context.cycle_almost_detected == 3) {
                        dump_cycles();
                    } else {
                        context.cycle_almost_detected += 1;
                    }
                }
            }
            else if (watermarked_node->macro_attempts < ent->macro_attempts) {
                watermarked_node = ent;
                highest_watermark = bh_max(highest_watermark, ent->macro_attempts);
            }
        } else {
            watermarked_node = NULL;
            context.cycle_almost_detected = 0;
        }

        if (onyx_has_errors()) {
            onyx_errors_print();
            return ONYX_COMPILER_PROGRESS_ERROR;
        }

        if (ent->state != Entity_State_Finalized && ent->state != Entity_State_Failed)
            entity_heap_insert_existing(&context.entities, ent);
    }

    //
    // There should not be any errors printing here, but there might be warnings.
    onyx_errors_print();

    u64 duration = bh_time_duration(start_time);

    if (context.options->verbose_output > 0) {
        // TODO: Replace these with bh_printf when padded formatting is added.
        printf("\nStatistics:\n");
        printf("    Time taken: %lf seconds\n", (double) duration / 1000);
        printf("    Processed %ld lines (%f lines/second).\n", lexer_lines_processed, ((f32) 1000 * lexer_lines_processed) / (duration));
        printf("    Processed %ld tokens (%f tokens/second).\n", lexer_tokens_processed, ((f32) 1000 * lexer_tokens_processed) / (duration));
        printf("\n");
    }

    if (context.options->generate_tag_file) {
        onyx_docs_emit_tags("./tags");
    }

    if (context.options->generate_symbol_info_file) {
        onyx_docs_emit_symbol_info(context.options->symbol_info_file);
    }

    if (context.options->documentation_file != NULL) {
        onyx_docs_emit_odoc(context.options->documentation_file);
    }

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

static void link_wasm_module() {
    Package *runtime_var_package = package_lookup("runtime.vars");
    assert(runtime_var_package);

    AstTyped *link_options_node = (AstTyped *) symbol_raw_resolve(runtime_var_package->scope, "link_options");
    Type *link_options_type = type_build_from_ast(context.ast_alloc, builtin_link_options_type);

    assert(unify_node_and_type(&link_options_node, link_options_type) == TYPE_MATCH_SUCCESS);

    OnyxWasmLinkOptions link_opts;
    // CLEANUP: Properly handle this case.
    assert(onyx_wasm_build_link_options_from_node(&link_opts, link_options_node));

    onyx_wasm_module_link(context.wasm_module, &link_opts);
}

static CompilerProgress onyx_flush_module() {
    link_wasm_module();

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

        bh_file_close(&data_file);
    } else {
        onyx_wasm_module_write_to_file(context.wasm_module, output_file);
    }

    bh_file_close(&output_file);

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

#ifdef ENABLE_RUN_WITH_WASMER
static b32 onyx_run_module(bh_buffer code_buffer) {
    onyx_run_initialize(context.options->debug_enabled);

    if (context.options->verbose_output > 0)
        bh_printf("Running program:\n");

    return onyx_run_wasm(code_buffer, context.options->passthrough_argument_count, context.options->passthrough_argument_data);
}

static b32 onyx_run_wasm_file(const char *filename) {
    bh_file_contents contents = bh_file_read_contents(global_heap_allocator, filename);

    bh_buffer code_buffer;
    code_buffer.data = contents.data;
    code_buffer.length = contents.length;

    return onyx_run_module(code_buffer);
}

static b32 onyx_run() {
    link_wasm_module();

    bh_buffer code_buffer;
    onyx_wasm_module_write_to_buffer(context.wasm_module, &code_buffer);

    return onyx_run_module(code_buffer);
    
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
        case ONYX_COMPILE_ACTION_PRINT_HELP: {
            if (compile_opts.help_subcommand) {
                print_subcommand_help(compile_opts.help_subcommand);
            } else {
                bh_printf(top_level_docstring);
            }
            return 1;
        }

        case ONYX_COMPILE_ACTION_CHECK:
            compiler_progress = onyx_compile();
            break;

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

        #ifdef ENABLE_RUN_WITH_WASMER
        case ONYX_COMPILE_ACTION_RUN_WASM:
            compiler_progress = ONYX_COMPILER_PROGRESS_SUCCESS;
            if (!onyx_run_wasm_file(context.options->target_file)) {
                compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
            }
        #endif

        default: break;
    }

    switch (compiler_progress) {
        case ONYX_COMPILER_PROGRESS_ERROR:
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
