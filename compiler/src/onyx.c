// #define BH_DEBUG

// It would preferable to set the internal allocator used by STBDS to
// be context.gp_alloc. However, STBDS does not provide a context parameter
// or any mechanism to retrieve the context dynamically. For now, any
// tables or datastructures allocated from STBDS need to be manually freed.
//
// #define STBDS_REALLOC(_,p,s) (bh_resize(context.gp_alloc, p, s))
// #define STBDS_FREE(_,p) (bh_free(context.gp_alloc, p))

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


#define VERSION__(m,i,p) "v" #m "." #i "." #p
#define VERSION_(m,i,p) VERSION__(m,i,p)
#define VERSION VERSION_(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH)

#ifdef ONYX_RUNTIME_LIBRARY
    #define ONYX_RUNTIME_LIBRARY_MAPPED ONYX_RUNTIME_LIBRARY
#else
    #define ONYX_RUNTIME_LIBRARY_MAPPED none
#endif

#include "./cli.c"

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

static void create_and_add_defined_variable(Context *context, char *name, char *value) {
    OnyxToken *value_token = bh_alloc_item(context->ast_alloc, OnyxToken);
    value_token->text = value;
    value_token->length = strlen(value);

    OnyxToken *name_token = bh_alloc_item(context->ast_alloc, OnyxToken);
    name_token->text = name;
    name_token->length = strlen(name);

    Package *p = package_lookup(context, "runtime.vars");
    assert(p);

    AstStrLit *value_node = make_string_literal(context, value_token);
    add_entities_for_node(&context->entities, NULL, (AstNode *) value_node, NULL, NULL);

    AstBinding *binding = onyx_ast_node_new(context->ast_alloc, sizeof(AstBinding), Ast_Kind_Binding);
    binding->token = name_token;
    binding->node = (AstNode *) value_node;

    add_entities_for_node(&context->entities, NULL, (AstNode *) binding, p->scope, p);
}

static void introduce_defined_variables(Context *context) {
    bh_arr_each(DefinedVariable, dv, context->options->defined_variables) {
        create_and_add_defined_variable(context, dv->key, dv->value);
    }
}

static void context_init(Context *context, CompileOptions* opts) {
    memset(context, 0, sizeof *context);

    bh_scratch_init(&context->scratch, bh_heap_allocator(), 256 * 1024); // NOTE: 256 KiB
    context->scratch_alloc = bh_scratch_allocator(&context->scratch);

    bh_managed_heap_init(&context->heap);
    context->gp_alloc = bh_managed_heap_allocator(&context->heap);

    context->token_alloc = context->gp_alloc;

    // NOTE: Create the arena where tokens and AST nodes will exist
    // Prevents nodes from being scattered across memory due to fragmentation
    bh_arena_init(&context->ast_arena, context->gp_alloc, 16 * 1024 * 1024); // 16MB
    context->ast_alloc = bh_arena_allocator(&context->ast_arena);

    types_init(context);
    prepare_builtins(context);

    context->options = opts;
    context->cycle_detected = 0;
    context->cycle_almost_detected = 0;

    OnyxFilePos internal_location = { 0 };
    internal_location.filename = "<compiler internal>";
    internal_location.line     = 1;
    internal_location.column   = 1;
    context->global_scope = scope_create(context, NULL, internal_location);
    sh_new_arena(context->packages);

    onyx_errors_init(context, &context->loaded_files);

    context->wasm_module = bh_alloc_item(context->gp_alloc, OnyxWasmModule);
    onyx_wasm_module_initialize(context, context->wasm_module);

    entity_heap_init(context->gp_alloc, &context->entities);

    // NOTE: Add builtin entities to pipeline.
    entity_heap_insert(&context->entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context->ast_alloc, "core:builtin"),
    }));

    entity_heap_insert(&context->entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context->ast_alloc, "core:runtime/build_opts"),
    }));

    if (context->options->runtime != Runtime_Custom) {
        // HACK
        context->special_global_entities.remaining = 5;

        context->special_global_entities.runtime_info_types_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context->ast_alloc, "core:runtime/info/types"),
        }));
        context->special_global_entities.runtime_info_foreign_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context->ast_alloc, "core:runtime/info/foreign_blocks"),
        }));
        context->special_global_entities.runtime_info_proc_tags_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context->ast_alloc, "core:runtime/info/proc_tags"),
        }));
        context->special_global_entities.runtime_info_global_tags_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context->ast_alloc, "core:runtime/info/global_tags"),
        }));
        context->special_global_entities.runtime_info_stack_trace_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context->ast_alloc, "core:runtime/info/stack_trace"),
        }));
    }

    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.stack_top, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.heap_start, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.tls_base, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.tls_size, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.closure_base, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.stack_trace, context->global_scope, NULL);

    // NOTE: Add all files passed by command line to the queue
    bh_arr_each(const char *, filename, opts->files) {
        AstInclude* load_node = create_load(context->ast_alloc, (char *) *filename);
        add_entities_for_node(&context->entities, NULL, (AstNode *) load_node, context->global_scope, NULL);
    }

    if (!context->options->no_core) {
        entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context->ast_alloc, "core:module"),
        }));
    }

    if (context->options->generate_symbol_info_file) {
        context->symbol_info = bh_alloc_item(context->gp_alloc, SymbolInfoTable);
        bh_imap_init(&context->symbol_info->node_to_id, context->gp_alloc, 512);
        bh_arr_new(context->gp_alloc, context->symbol_info->symbols, 128);
        bh_arr_new(context->gp_alloc, context->symbol_info->symbols_resolutions, 128);
        sh_new_arena(context->symbol_info->files);
    }

    if (context->options->documentation_file) {
        context->doc_info = bh_alloc_item(context->gp_alloc, OnyxDocInfo);
        memset(context->doc_info, 0, sizeof(OnyxDocInfo));
        bh_arr_new(context->gp_alloc, context->doc_info->procedures, 128);
        bh_arr_new(context->gp_alloc, context->doc_info->structures, 128);
        bh_arr_new(context->gp_alloc, context->doc_info->enumerations, 128);
    }

    if (context->options->verbose_output > 0) {
        bh_printf("Mapped folders:\n");
        bh_arr_each(bh_mapped_folder, p, context->options->mapped_folders) {
            bh_printf("\t%s: %s\n", p->name, p->folder);
        }
        bh_printf("\n");
    }
}

static void context_free(Context *context) {
    bh_arena_free(&context->ast_arena);
    bh_arr_free(context->loaded_files);
    bh_scratch_free(&context->scratch);
    bh_managed_heap_free(&context->heap);
}

static void parse_source_file(Context *context, bh_file_contents* file_contents) {
    // :Remove passing the allocators as parameters
    OnyxTokenizer tokenizer = onyx_tokenizer_create(context, file_contents);
    onyx_lex_tokens(&tokenizer);

    file_contents->line_count = tokenizer.line_number;

    OnyxParser parser = onyx_parser_create(context, &tokenizer);
    onyx_parse(&parser);
    onyx_parser_free(&parser);
}

static b32 process_source_file(Context *context, char* filename) {
    bh_arr_each(bh_file_contents, fc, context->loaded_files) {
        // Duplicates are detected here and since these filenames will be the full path,
        // string comparing them should be all that is necessary.
        if (!strcmp(fc->filename, filename)) return 1;
    }

    bh_file file;
    bh_file_error err = bh_file_open(&file, filename);
    if (err != BH_FILE_ERROR_NONE) {
        return 0;
    }

    bh_file_contents fc = bh_file_read_contents(context->token_alloc, &file);
    bh_file_close(&file);

    bh_arr_push(context->loaded_files, fc);

    if (context->options->verbose_output == 2)
        bh_printf("Processing source file:    %s (%d bytes)\n", file.filename, fc.length);

    parse_source_file(context, &bh_arr_last(context->loaded_files));
    return 1;
}

static b32 process_load_entity(Context *context, Entity* ent) {
    assert(ent->type == Entity_Type_Load_File || ent->type == Entity_Type_Load_Path);
    AstInclude* include = ent->include;

    if (include->kind == Ast_Kind_Load_File) {
        // :RelativeFiles
        const char* parent_file = include->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, context->scratch_alloc);
        char* filename      = bh_search_for_mapped_file(
            include->name,
            parent_folder,
            ".onyx",
            context->options->mapped_folders,
            context->gp_alloc
        );

        if (filename == NULL) {
            OnyxFilePos error_pos = include->token->pos;
            if (error_pos.filename == NULL) {
                ONYX_ERROR(error_pos, Error_Command_Line_Arg, "Failed to open file '%s'", include->name);
            } else {
                ONYX_ERROR(error_pos, Error_Critical, "Failed to open file '%s'", include->name);
            }
            return 0;
        }

        return process_source_file(context, filename);

    } else if (include->kind == Ast_Kind_Load_All) {
        const char* parent_file = include->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, context->scratch_alloc);

        char* folder = bh_search_for_mapped_file(
            include->name,
            parent_folder,
            "",
            context->options->mapped_folders,
            context->gp_alloc
        );
        bh_path_convert_separators(folder);

        bh_arr(char *) folders_to_process = NULL;
        bh_arr_new(context->gp_alloc, folders_to_process, 2);

        bh_arr_push(folders_to_process, bh_strdup(context->scratch_alloc, folder));

        while (bh_arr_length(folders_to_process) > 0) {
            char *folder = bh_arr_pop(folders_to_process);
            bh_dir dir = bh_dir_open(folder);
            if (dir == NULL) {
                ONYX_ERROR(include->token->pos, Error_Critical, "Could not find or open folder '%s'.", folder);
                return 0;
            }

            bh_dirent entry;
            char fullpath[512];
            while (bh_dir_read(dir, &entry)) {
                if (entry.type == BH_DIRENT_FILE && bh_str_ends_with(entry.name, ".onyx")) {
                    bh_snprintf(fullpath, 511, "%s/%s", folder, entry.name);
                    bh_path_convert_separators(fullpath);

                    char* formatted_name = bh_path_get_full_name(fullpath, context->gp_alloc);

                    AstInclude* new_include = onyx_ast_node_new(context->ast_alloc, sizeof(AstInclude), Ast_Kind_Load_File);
                    new_include->token = include->token;
                    new_include->name = formatted_name;
                    add_entities_for_node(&context->entities, NULL, (AstNode *) new_include, include->entity->scope, include->entity->package);
                }

                if (entry.type == BH_DIRENT_DIRECTORY && include->recursive) {
                    if (!strcmp(entry.name, ".") || !strcmp(entry.name, "..")) continue;

                    bh_snprintf(fullpath, 511, "%s/%s", folder, entry.name);
                    char* formatted_name = bh_path_get_full_name(fullpath, context->scratch_alloc); // Could this overflow the scratch allocator?

                    bh_arr_push(folders_to_process, formatted_name);
                }
            }

            bh_dir_close(dir);
        }

        return 1;

    } else if (include->kind == Ast_Kind_Load_Path) {
        ONYX_WARNING(include->token->pos, "'#load_path' has been deprecated and no longer does anything.");

    } else if (include->kind == Ast_Kind_Library_Path) {
        bh_arr_push(context->wasm_module->library_paths, include->name);
    }

    return 1;
}

static b32 process_entity(Context *context, Entity* ent) {
    static char verbose_output_buffer[512];
    if (context->options->verbose_output == 3) {
        if (ent->expr && ent->expr->token)
            snprintf(verbose_output_buffer, 511,
                    "%20s | %24s (%d, %d) | %5d | %s:%i:%i \n",
                   entity_state_strings[ent->state],
                   entity_type_strings[ent->type],
                   (u32) ent->macro_attempts,
                   (u32) ent->micro_attempts,
                   ent->id,
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

    EntityState before_state = ent->state;
    switch (before_state) {
        case Entity_State_Error:
            if (ent->type != Entity_Type_Error) {
                ONYX_ERROR(ent->expr->token->pos, Error_Critical, "Error entity unexpected. This is definitely a compiler bug");
            } else {
                ONYX_ERROR(ent->error->token->pos, Error_Critical, "Static error occured: '%b'", ent->error->error_msg->text, ent->error->error_msg->length);
            }
            break;

        case Entity_State_Parse_Builtin:
            process_load_entity(context, ent);
            ent->state = Entity_State_Finalized;
            break;

        case Entity_State_Introduce_Symbols:
            // Currently, introducing symbols is handled in the symbol resolution
            // function. Maybe there should be a different place where that happens?
            symres_entity(context, ent);
            break;

        case Entity_State_Parse:
            if (!context->builtins_initialized) {
                context->builtins_initialized = 1;
                initialize_builtins(context);
                introduce_build_options(context);
                introduce_defined_variables(context);
            }

            // GROSS
            if (context->special_global_entities.remaining == 0) {
                context->special_global_entities.remaining--;
                initalize_special_globals(context);
            }

            if (process_load_entity(context, ent)) {
                // GROSS
                if (   ent == context->special_global_entities.runtime_info_types_entity
                    || ent == context->special_global_entities.runtime_info_proc_tags_entity
                    || ent == context->special_global_entities.runtime_info_global_tags_entity
                    || ent == context->special_global_entities.runtime_info_foreign_entity
                    || ent == context->special_global_entities.runtime_info_stack_trace_entity) {
                    context->special_global_entities.remaining--;
                }

                ent->state = Entity_State_Finalized;
            } else {
                ent->macro_attempts++;
            }
            break;

        case Entity_State_Resolve_Symbols: symres_entity(context, ent); break;
        case Entity_State_Check_Types:     check_entity(context, ent);  break;

        case Entity_State_Code_Gen: {
            if (context->options->action == ONYX_COMPILE_ACTION_CHECK) {
                ent->state = Entity_State_Finalized;
                break;
            }

            emit_entity(context, ent);
            break;
        }

        default: break;
    }

    b32 changed = ent->state != before_state;
    if (context->options->verbose_output == 3) {
        if (changed) printf("SUCCESS to %20s | %s", entity_state_strings[ent->state], verbose_output_buffer);
        else         printf("YIELD   to %20s | %s", entity_state_strings[ent->state], verbose_output_buffer);
    }

    return changed;
}

// Just having fun with some visual output - brendanfh 2020/12/14
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static void output_dummy_progress_bar(Context *context) {
    EntityHeap* eh = &context->entities;
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

static void dump_cycles(Context *context) {
    context->cycle_detected = 1;
    Entity* ent;

    while (1) {
        ent = entity_heap_top(&context->entities);
        entity_heap_remove_top(&context->entities);
        if (ent->state < Entity_State_Code_Gen) process_entity(context, ent);
        else break;

        if (bh_arr_length(context->entities.entities) == 0) {
            break;
        }
    }
}

// TODO: relocate this function
static void send_stalled_hooks(Context *context) {
    bh_arr_each(CompilerExtension, ext, context->extensions) {
        compiler_extension_hook_stalled(context, ext->id);
    }
}

static i32 onyx_compile(Context *context) {
    u64 start_time = bh_time_curr();

    if (context->options->fun_output)
        printf("\e[2J");

    while (!bh_arr_is_empty(context->entities.entities)) {
        Entity* ent = entity_heap_top(&context->entities);

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
        if (context->options->fun_output) {
            output_dummy_progress_bar(context);

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
                bh_arr_length(context->entities.entities),
                context->entities.state_count[Entity_State_Introduce_Symbols],
                context->entities.state_count[Entity_State_Parse],
                context->entities.state_count[Entity_State_Resolve_Symbols],
                context->entities.state_count[Entity_State_Check_Types],
                context->entities.state_count[Entity_State_Code_Gen],
                context->entities.state_count[Entity_State_Finalized]);
        */

        // Mostly a preventative thing to ensure that even if somehow
        // errors were left disabled, they are re-enabled in this cycle.
        onyx_errors_enable(context);
        entity_heap_remove_top(&context->entities);

        u64 perf_start;
        EntityType perf_entity_type;
        EntityState perf_entity_state;
        if (context->options->running_perf) {
            perf_start = bh_time_curr_micro();
            perf_entity_type = ent->type;
            perf_entity_state = ent->state;
        }

        b32 changed = process_entity(context, ent);

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
        if (!changed) {
            if (!context->watermarked_node) {
                context->watermarked_node = ent;
                context->highest_watermark = bh_max(context->highest_watermark, ent->macro_attempts);
            }
            else if (context->watermarked_node == ent) {
                if (ent->macro_attempts > context->highest_watermark) {
                    entity_heap_insert_existing(&context->entities, ent);

                    if (context->cycle_almost_detected == 4) {
                        dump_cycles(context);
                    } else if (context->cycle_almost_detected == 3) {
                        send_stalled_hooks(context);
                    }

                    context->cycle_almost_detected += 1;
                }
            }
            else if (context->watermarked_node->macro_attempts < ent->macro_attempts) {
                context->watermarked_node = ent;
                context->highest_watermark = bh_max(context->highest_watermark, ent->macro_attempts);
            }
        } else {
            context->watermarked_node = NULL;
            context->cycle_almost_detected = 0;
        }

        if (onyx_has_errors(context)) {
            onyx_errors_print(context);
            return ONYX_COMPILER_PROGRESS_ERROR;
        }

        if (ent->state != Entity_State_Finalized && ent->state != Entity_State_Failed)
            entity_heap_insert_existing(&context->entities, ent);

        if (context->options->running_perf) {
            u64 perf_end = bh_time_curr_micro();

            u64 duration = perf_end - perf_start;
            context->stats.microseconds_per_type[perf_entity_type] += duration;
            context->stats.microseconds_per_state[perf_entity_state] += duration;
        }
    }

    //
    // There should not be any errors printing here, but there might be warnings.
    onyx_errors_print(context);

    u64 duration = bh_time_duration(start_time);

    if (context->options->verbose_output > 0) {
        printf("Type table size: %d bytes\n", context->wasm_module->type_info_size);

        // TODO: Replace these with bh_printf when padded formatting is added.
        printf("\nStatistics:\n");
        printf("    Time taken: %lf ms\n", (double) duration);
        printf("    Processed %llu lines (%f lines/second).\n", context->stats.lexer_lines_processed, ((f32) 1000 * context->stats.lexer_lines_processed) / (duration));
        printf("    Processed %llu tokens (%f tokens/second).\n", context->stats.lexer_tokens_processed, ((f32) 1000 * context->stats.lexer_tokens_processed) / (duration));
        printf("\n");
    }

    if (context->options->generate_symbol_info_file) {
        onyx_docs_emit_symbol_info(context, context->options->symbol_info_file);
    }

    if (context->options->documentation_file != NULL) {
        onyx_docs_emit_odoc(context, context->options->documentation_file);
    }

    if (context->options->running_perf) {
        fori (i, 0, Entity_State_Count) {
            printf("| %27s | %10llu us |\n", entity_state_strings[i], context->stats.microseconds_per_state[i]);
        }
        printf("\n");
        fori (i, 0, Entity_Type_Count) {
            printf("| %27s | %10llu us |\n", entity_type_strings[i], context->stats.microseconds_per_type[i]);
        }
        printf("\n");
    }

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

static void link_wasm_module(Context *context) {
    Package *runtime_var_package = package_lookup(context, "runtime.vars");
    assert(runtime_var_package);

    AstTyped *link_options_node = (AstTyped *) symbol_raw_resolve(context, runtime_var_package->scope, "link_options");
    Type *link_options_type = type_build_from_ast(context, context->builtins.link_options_type);

    assert(unify_node_and_type(context, &link_options_node, link_options_type) == TYPE_MATCH_SUCCESS);

    OnyxWasmLinkOptions link_opts;
    // CLEANUP: Properly handle this case.
    assert(onyx_wasm_build_link_options_from_node(context, &link_opts, link_options_node));

    onyx_wasm_module_link(context, context->wasm_module, &link_opts);
}

static CompilerProgress onyx_flush_module(Context *context) {
    link_wasm_module(context);

    // NOTE: Output to file
    bh_file output_file;
    if (bh_file_create(&output_file, context->options->target_file) != BH_FILE_ERROR_NONE)
        return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;

    if (context->options->verbose_output)
        bh_printf("Outputting to WASM file:   %s\n", output_file.filename);

    if (context->options->use_multi_threading && !context->options->use_post_mvp_features) {
        bh_file data_file;
        if (bh_file_create(&data_file, bh_aprintf(context->scratch_alloc, "%s.data", context->options->target_file)) != BH_FILE_ERROR_NONE)
            return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;

        OnyxWasmModule* data_module = bh_alloc_item(context->gp_alloc, OnyxWasmModule);
        onyx_wasm_module_initialize(context, data_module);

        data_module->data = context->wasm_module->data;
        context->wasm_module->data = NULL;

        onyx_wasm_module_write_to_file(data_module, data_file);
        onyx_wasm_module_write_to_file(context->wasm_module, output_file);

        bh_file_close(&data_file);
    } else {
        onyx_wasm_module_write_to_file(context->wasm_module, output_file);
    }

    if (bh_arr_length(context->wasm_module->js_partials) > 0) {
        bh_file js_file;
        if (
            bh_file_create(
                &js_file,
                bh_aprintf(context->gp_alloc, "%s.js", context->options->target_file)
            ) != BH_FILE_ERROR_NONE
        ) {
            return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;
        }

        onyx_wasm_module_write_js_partials_to_file(context->wasm_module, js_file);
        bh_file_close(&output_file);
    }

    bh_file_close(&output_file);

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

#ifdef ONYX_RUNTIME_LIBRARY
static b32 onyx_run_module(Context *context, bh_buffer code_buffer) {
    onyx_run_initialize(context->options->debug_session, context->options->debug_socket);

    if (context->options->verbose_output > 0)
        bh_printf("Running program:\n");

    return onyx_run_wasm(code_buffer, context->options->passthrough_argument_count, context->options->passthrough_argument_data);
}

static b32 onyx_run_wasm_file(Context *context, const char *filename) {
    bh_file_contents contents = bh_file_read_contents(bh_heap_allocator(), filename);

    bh_buffer code_buffer;
    code_buffer.data = contents.data;
    code_buffer.length = contents.length;

    return onyx_run_module(context, code_buffer);
}

static b32 onyx_run(Context *context) {
    link_wasm_module(context);

    bh_buffer code_buffer;
    onyx_wasm_module_write_to_buffer(context->wasm_module, &code_buffer);

    return onyx_run_module(context, code_buffer);

}
#endif

CompilerProgress do_compilation(Context *context, CompileOptions *compile_opts) {
    context_init(context, compile_opts);

    return onyx_compile(context);
}

void cleanup_compilation(Context *context) {
    context_free(context);
}

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <signal.h>
#include <sys/wait.h>

static bh_file_watch watches;
static i32 watch_run_pid = -1;

static void onyx_watch_stop(int sig) {
    bh_file_watch_stop(&watches);
}

static void onyx_watch_run_executable(const char *target) {
    watch_run_pid = fork();
    switch (watch_run_pid) {
        case -1: bh_printf("error: fork() failed\n"); break;
        case 0:
            setpgid(0, getpid());
            close(STDIN_FILENO);
            open("/dev/null", O_RDONLY);
            execlp("onyx", "onyx", "run", target, NULL);
            exit(1);
            break;
        default:
            break;
    }
}

static void onyx_watch(Context *context, CompileOptions *compile_opts) {
    signal(SIGINT, onyx_watch_stop);

    b32 run_the_program = compile_opts->action == ONYX_COMPILE_ACTION_WATCH_RUN;

    while (1) {
        bh_printf("\e[2J\e[?25l\n");
        bh_printf("\e[3;1H");

        b32 successful_compilation = do_compilation(context, compile_opts) == ONYX_COMPILER_PROGRESS_SUCCESS;

        if (successful_compilation) {
            onyx_flush_module(context);
            bh_printf("\e[92mNo errors!\n");
        }

        char time_buf[128] = {0};
        time_t now = time(NULL);
        strftime(time_buf, 128, "%X", localtime(&now));
        bh_printf("\e[1;1H\e[30;105m Onyx " VERSION " \e[30;104m Built %s \e[0m", time_buf);

        i32 errors = bh_arr_length(context->errors.errors);
        if (errors == 0) {
            bh_printf("\e[30;102m Errors 0 \e[0m");
        } else {
            bh_printf("\e[30;101m Error%s %d \e[0m", bh_num_plural(errors), errors);
        }

        if (run_the_program && successful_compilation) {
            bh_printf("\n\n\nRunning your program...\n");
            onyx_watch_run_executable(compile_opts->target_file);
        }

        watches = bh_file_watch_new();

        bh_arr_each(bh_file_contents, file, context->loaded_files) {
            bh_file_watch_add(&watches, file->filename);
        }

        cleanup_compilation(context);

        b32 wait_successful = bh_file_watch_wait(&watches);

        if (run_the_program && watch_run_pid > 0) {
            int status;
            killpg(watch_run_pid, SIGTERM);
            waitpid(watch_run_pid, &status, 0);
            watch_run_pid = -1;
        }

        bh_file_watch_free(&watches);

        if (!wait_successful) {
            break;
        }
    }


    bh_printf("\e[2J\e[1;1H\e[?25h\n");
}

#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <sys/wait.h>

static void perform_self_upgrade(CompileOptions *opts, char *version) {
    // TODO: cleanup    
    
    int curl_pid;
    bh_file upgrade_script;

    char file_path[512];
    bh_snprintf(file_path, 511, "%s/upgrade.sh", opts->core_installation);

    switch (curl_pid = fork()) {
        case -1: exit(1);
        case 0:
            if (bh_file_create(&upgrade_script, file_path)) {
                exit(1);
            }

            dup2(upgrade_script.fd, STDOUT_FILENO);
            bh_file_close(&upgrade_script);

            execlp("curl", "curl", "https://get.onyxlang.io", "-sSfL", NULL);
            exit(1);
            break;
    }

    int status;
    waitpid(curl_pid, &status, 0);

    if (status == 0) {
        execlp("sh", "sh", file_path, version, STRINGIFY(ONYX_RUNTIME_LIBRARY_MAPPED), NULL);
    }

    printf("error: Failed to download upgrade script.\n");
    printf(" hint: Ensure you have an active internet connection and 'curl' installed.\n");
    exit(1);
}
#endif


int main(int argc, char *argv[]) {
    Context context;
    CompileOptions compile_opts = compile_opts_parse(bh_heap_allocator(), argc, argv);

    CompilerProgress compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
    switch (compile_opts.action) {
        case ONYX_COMPILE_ACTION_PRINT_HELP: {
            if (compile_opts.help_subcommand) {
                print_subcommand_help(compile_opts.help_subcommand);
            } else {
                print_top_level_docs(&compile_opts);
            }
            return 0;
        }

        case ONYX_COMPILE_ACTION_PRINT_VERSION: {
            bh_printf(VERSION_STRING);
            return 0;
        }

        case ONYX_COMPILE_ACTION_CHECK:
            compiler_progress = do_compilation(&context, &compile_opts);
            break;

        case ONYX_COMPILE_ACTION_COMPILE:
            compiler_progress = do_compilation(&context, &compile_opts);
            if (compiler_progress == ONYX_COMPILER_PROGRESS_SUCCESS) {
                onyx_flush_module(&context);
            }
            break;

        #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        case ONYX_COMPILE_ACTION_WATCH:
            onyx_watch(&context, &compile_opts);
            break;
        #endif

        #ifdef ONYX_RUNTIME_LIBRARY
        case ONYX_COMPILE_ACTION_RUN:
            compiler_progress = do_compilation(&context, &compile_opts);
            if (compiler_progress == ONYX_COMPILER_PROGRESS_SUCCESS) {
                if (!onyx_run(&context)) {
                    compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
                }
            }
            break;

        case ONYX_COMPILE_ACTION_RUN_WASM:
            context.gp_alloc = bh_heap_allocator();
            context_init(&context, &compile_opts);
            compiler_progress = ONYX_COMPILER_PROGRESS_SUCCESS;
            if (!onyx_run_wasm_file(&context, context.options->target_file)) {
                compiler_progress = ONYX_COMPILER_PROGRESS_ERROR;
            }
            break;

        #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        case ONYX_COMPILE_ACTION_WATCH_RUN:
            onyx_watch(&context, &compile_opts);
            break;
        #endif
        #endif

        #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        case ONYX_COMPILE_ACTION_SELF_UPGRADE:
            perform_self_upgrade(&compile_opts, compile_opts.upgrade_version);
            break;
        #endif

        default: break;
    }

    if (compiler_progress == ONYX_COMPILER_PROGRESS_FAILED_OUTPUT) {
        bh_printf_err("Failed to open file for writing: '%s'\n", compile_opts.target_file);
    }

    cleanup_compilation(&context);
    compile_opts_free(&compile_opts);

    return compiler_progress != ONYX_COMPILER_PROGRESS_SUCCESS;
}
