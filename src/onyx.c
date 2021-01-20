// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxerrors.h"
#include "onyxparser.h"
#include "onyxutils.h"
#include "onyxwasm.h"
#include "onyxdoc.h"

#define VERSION "v0.0.5"


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
    "\tonyx doc <input files>\n"
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

        .verbose_output = 0,

        .files = NULL,
        .target_file = "out.wasm",
    };

    bh_arr_new(alloc, options.files, 2);
    bh_arr_new(alloc, options.included_folders, 2);

    // NOTE: Add the current folder
    bh_arr_push(options.included_folders, CORE_INSTALLATION);
    bh_arr_push(options.included_folders, ".");

    if (argc == 1) return options;

    if (!strcmp(argv[1], "doc")) {
        options.action = ONYX_COMPILE_ACTION_DOCUMENT;
    }
    else if (!strcmp(argv[1], "help")) {
        options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
    }
    else {
        options.action = ONYX_COMPILE_ACTION_COMPILE;
    }

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
            else if (!strcmp(argv[i], "--fun") || !strcmp(argv[i], "-F")) {
                options.fun_output = 1;
            }
            else if (!strcmp(argv[i], "-I")) {
                bh_arr_push(options.included_folders, argv[++i]);
            }
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
    ONYX_COMPILER_PROGRESS_FAILED_READ,
    ONYX_COMPILER_PROGRESS_FAILED_PARSE,
    ONYX_COMPILER_PROGRESS_ERROR,
    ONYX_COMPILER_PROGRESS_FAILED_OUTPUT,
    ONYX_COMPILER_PROGRESS_SUCCESS
} CompilerProgress;

static char* lookup_included_file(char* filename);

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

    // NOTE: Create the arena where tokens and AST nodes will exist
    // Prevents nodes from being scattered across memory due to fragmentation
    bh_arena_init(&context.token_arena, global_heap_allocator, 16 * 1024 * 1024); // 16MB
    context.token_alloc = bh_arena_allocator(&context.token_arena);

    bh_arena_init(&context.ast_arena, global_heap_allocator, 16 * 1024 * 1024); // 16MB
    context.ast_alloc = bh_arena_allocator(&context.ast_arena);

    // HACK
    // MOVE TO CONTEXT
    global_wasm_module = onyx_wasm_module_create(context.options->allocator);

    // NOTE: Add builtin entities to pipeline.
    entity_heap_insert(&context.entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context.ast_alloc, "core/builtin"),
    }));

    entity_heap_insert(&context.entities, ((Entity) {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Global_Header,
        .global = &builtin_stack_top
    }));
    
    entity_heap_insert(&context.entities, ((Entity) {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Global,
        .global = &builtin_stack_top
    }));

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files) {
        entity_heap_insert(&context.entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context.ast_alloc, (char *) *filename),
        }));
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

static ParseResults parse_source_file(bh_file_contents* file_contents) {
    // :Remove passing the allocators as parameters
    OnyxTokenizer tokenizer = onyx_tokenizer_create(context.token_alloc, file_contents);
    onyx_lex_tokens(&tokenizer);

    OnyxParser parser = onyx_parser_create(context.ast_alloc, &tokenizer);
    return onyx_parse(&parser);
}

static void merge_parse_results(ParseResults* results) {
    Entity ent;
    bh_arr_each(NodeToProcess, n, results->nodes_to_process) {
        AstNode* node = n->node;
        AstKind nkind = node->kind;

        ent.state = Entity_State_Resolve_Symbols;
        ent.package = n->package;
        ent.scope   = n->scope;

        switch (nkind) {
            case Ast_Kind_Load_File: {
                ent.state = Entity_State_Parse;
                ent.type = Entity_Type_Load_File;
                ent.include = (AstInclude *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }
                                   
            case Ast_Kind_Load_Path: {
                ent.state = Entity_State_Parse;
                ent.type = Entity_Type_Load_Path;
                ent.include = (AstInclude *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Function: {
                if ((node->flags & Ast_Flag_Foreign) != 0) {
                    ent.type     = Entity_Type_Foreign_Function_Header;
                    ent.function = (AstFunction *) node;
                    entity_heap_insert(&context.entities, ent);

                } else {
                    ent.type     = Entity_Type_Function_Header;
                    ent.function = (AstFunction *) node;
                    entity_heap_insert(&context.entities, ent);

                    ent.type     = Entity_Type_Function;
                    ent.function = (AstFunction *) node;
                    entity_heap_insert(&context.entities, ent);
                }
                break;
            }

            case Ast_Kind_Overloaded_Function: {
                ent.type                = Entity_Type_Overloaded_Function;
                ent.overloaded_function = (AstOverloadedFunction *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Global: {
                if ((node->flags & Ast_Flag_Foreign) != 0) {
                    ent.type   = Entity_Type_Foreign_Global_Header;
                    ent.global = (AstGlobal *) node;
                    entity_heap_insert(&context.entities, ent);

                } else {
                    ent.type   = Entity_Type_Global_Header;
                    ent.global = (AstGlobal *) node;
                    entity_heap_insert(&context.entities, ent);

                    ent.type   = Entity_Type_Global;
                    ent.global = (AstGlobal *) node;
                    entity_heap_insert(&context.entities, ent);
                }
                break;
            }

            case Ast_Kind_StrLit: {
                ent.type   = Entity_Type_String_Literal;
                ent.strlit = (AstStrLit *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_File_Contents: {
                ent.type = Entity_Type_File_Contents;
                ent.file_contents = (AstFileContents *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Struct_Type: {
                ent.type = Entity_Type_Struct_Member_Default;
                ent.type_alias = (AstType *) node;
                entity_heap_insert(&context.entities, ent);
                // fallthrough
            }

            case Ast_Kind_Poly_Struct_Type:
            case Ast_Kind_Type_Alias: {
                ent.type = Entity_Type_Type_Alias;
                ent.type_alias = (AstType *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Enum_Type: {
                ent.type = Entity_Type_Enum;
                ent.enum_type = (AstEnumType *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Use_Package: {
                ent.type = Entity_Type_Use_Package;
                ent.use_package = (AstUsePackage *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Use: {
                ent.type = Entity_Type_Use;
                ent.use = (AstUse *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Memres: {
                ent.type = Entity_Type_Memory_Reservation_Type;
                ent.mem_res = (AstMemRes *) node;
                entity_heap_insert(&context.entities, ent);

                ent.type = Entity_Type_Memory_Reservation;
                ent.mem_res = (AstMemRes *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            case Ast_Kind_Polymorphic_Proc: {
                ent.type = Entity_Type_Polymorphic_Proc;
                ent.poly_proc = (AstPolyProc *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }

            default: {
                ent.type = Entity_Type_Expression;
                ent.expr = (AstTyped *) node;
                entity_heap_insert(&context.entities, ent);
                break;
            }
        }
    }
}

static CompilerProgress process_source_file(char* filename) {
    bh_arr_each(bh_file_contents, fc, context.loaded_files) {
        // CLEANUP: Add duplicate resolutions, such as
        //          ./foo and ./test/../foo
        // should be the same thing.
        if (!strcmp(fc->filename, filename)) {
            return ONYX_COMPILER_PROGRESS_SUCCESS;
        }
    }

    bh_file file;
    bh_file_error err = bh_file_open(&file, filename);
    if (err != BH_FILE_ERROR_NONE) {
        // bh_printf_err("Failed to open file %s\n", filename);
        onyx_report_error((OnyxFilePos) { 0 }, "Failed to open file %s\n", filename);
        return ONYX_COMPILER_PROGRESS_FAILED_READ;
    }

    bh_file_contents fc = bh_file_read_contents(context.token_alloc, &file);
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
    bh_arr_push(context.loaded_files, fc);

    if (context.options->verbose_output == 2) {
        bh_printf("Processing source file:    %s (%d bytes)\n", file.filename, fc.length);
    }

    ParseResults results = parse_source_file(&fc);
    merge_parse_results(&results);

    if (onyx_has_errors()) {
        return ONYX_COMPILER_PROGRESS_FAILED_PARSE;
    } else {
        return ONYX_COMPILER_PROGRESS_SUCCESS;
    }
}

static b32 process_load_entity(Entity* ent) {
    assert(ent->type == Entity_Type_Load_File || ent->type == Entity_Type_Load_Path);
    AstInclude* include = ent->include;

    if (include->kind == Ast_Kind_Load_File) {
        char* filename = lookup_included_file(include->name);
        char* formatted_name = bh_strdup(global_heap_allocator, filename);

        process_source_file(formatted_name);

    } else if (include->kind == Ast_Kind_Load_Path) {
        bh_arr_push(context.options->included_folders, include->name);
    }

    return 1;
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

static i32 onyx_compile() {
    u64 start_time = bh_time_curr();

    if (context.options->fun_output)
        printf("\e[2J");

    while (!bh_arr_is_empty(context.entities.entities)) {
        Entity ent = entity_heap_top(&context.entities);
        entity_heap_remove_top(&context.entities);
        if (ent.state == Entity_State_Finalized) continue;

        if (context.options->fun_output) {
            output_dummy_progress_bar();

            // Slowing things down for the effect
#if defined(_BH_WINDOWS)
            Sleep(1);
#elif defined(_BH_LINUX)
            usleep(1000);
#endif

            if (ent.expr->token) {
                OnyxFilePos pos = ent.expr->token->pos;
                printf("\e[0K%s on %s in %s:%d:%d\n", entity_state_strings[ent.state], entity_type_strings[ent.type], pos.filename, pos.line, pos.column);
            }
        }

        b32 changed = process_entity(&ent);

        if (onyx_has_errors()) return ONYX_COMPILER_PROGRESS_ERROR;

        // SPEED: Not checking if the entity is already finalized does diminish speeds
        // a little bit, but it makes the fun visualization look better... so... I'm
        // gonna keep this how it is for now.                - brendanfh 2020/12/15

        // if (changed && ent.state != Entity_State_Finalized)
        entity_heap_insert(&context.entities, ent);
    }

    // NOTE: Output to file
    bh_file output_file;
    if (bh_file_create(&output_file, context.options->target_file) != BH_FILE_ERROR_NONE) {
        return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;
    }

    if (context.options->verbose_output)
        bh_printf("Outputting to WASM file:   %s\n", output_file.filename);

    onyx_wasm_module_write_to_file(&global_wasm_module, output_file);

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

    CompilerProgress compiler_progress = ONYX_COMPILER_PROGRESS_FAILED_READ;
    switch (compile_opts.action) {
        case ONYX_COMPILE_ACTION_PRINT_HELP:
            // NOTE: This could probably be made better
            bh_printf(docstring);
            return 1;

        case ONYX_COMPILE_ACTION_COMPILE:
            compiler_progress = onyx_compile();
            break;

        case ONYX_COMPILE_ACTION_DOCUMENT:
            bh_printf("Documentation has not been fully implemented yet.\n");
            exit(EXIT_FAILURE);
            break;

        default: break;
    }

    switch (compiler_progress) {
        case ONYX_COMPILER_PROGRESS_FAILED_READ:
            // NOTE: Do nothing since it was already printed above
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_PARSE:
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
