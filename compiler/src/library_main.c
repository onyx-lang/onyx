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
#include "onyx.h"

#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

//
// Types
//

struct onyx_context_t {
	Context context;
};


//
// Metadata
//

int32_t onyx_version_major() {
    return VERSION_MAJOR;
}

int32_t onyx_version_minor() {
    return VERSION_MINOR;
}

int32_t onyx_version_patch() {
    return VERSION_PATCH;
}

char   *onyx_version_suffix() {
    return VERSION_SUFFIX;
}

char   *onyx_version_build_time() {
    return __TIMESTAMP__;
}

char   *onyx_version_runtime() {
#ifdef ONYX_RUNTIME_LIBRARY
    return STRINGIFY(ONYX_RUNTIME_LIBRARY);
#else
    return "none";
#endif
}


//
// Forward declarations of internal procedures
//

static AstInclude* create_load(Context *context, char* filename, int32_t length);
static void introduce_defined_variables(Context *context);
static void create_and_add_defined_variable(Context *context, DefinedVariable *var);
static void link_wasm_module(Context *context);


//
// Lifecycle
//


onyx_context_t *onyx_context_create() {
	onyx_context_t *ctx = malloc(sizeof(*ctx));

	Context *context = &ctx->context;
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
    compiler_events_init(context);

    // Options should be moved directly inside of the context instead of through a pointer...
    context->options = malloc(sizeof(* context->options));
    memset(context->options, 0, sizeof(* context->options));
    context->options->use_post_mvp_features = 1;
    context->options->enable_optional_semicolons = 1;
    context->options->generate_type_info = 1;

    OnyxFilePos internal_location = { 0 };
    internal_location.filename = "<compiler internal>";
    internal_location.line     = 1;
    internal_location.column   = 1;
    context->global_scope = scope_create(context, NULL, internal_location);

    sh_new_arena(context->packages);
    bh_arr_new(context->gp_alloc, context->scopes, 128);

    onyx_errors_init(context, &context->loaded_files);

    context->wasm_module = bh_alloc_item(context->gp_alloc, OnyxWasmModule);
    onyx_wasm_module_initialize(context, context->wasm_module);

    entity_heap_init(context->gp_alloc, &context->entities);

    return ctx;
}

void onyx_context_free(onyx_context_t *ctx) {
	Context *context = &ctx->context;

    bh_arr_each(Scope *, pscope, context->scopes) {
        shfree((*pscope)->symbols);
    }

    onyx_wasm_module_free(context->wasm_module);
    bh_arena_free(&context->ast_arena);
    bh_arr_free(context->loaded_files);
    bh_arr_free(context->scopes);
    bh_scratch_free(&context->scratch);
    bh_managed_heap_free(&context->heap);

    free(ctx);
}

void onyx_options_ready(onyx_context_t *ctx) {
	Context *context = &ctx->context;

    // NOTE: Add builtin entities to pipeline.
    entity_heap_insert(&context->entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context, "core:builtin", -1),
    }));

    entity_heap_insert(&context->entities, ((Entity) {
        .state = Entity_State_Parse_Builtin,
        .type = Entity_Type_Load_File,
        .package = NULL,
        .include = create_load(context, "core:runtime/build_opts", -1),
    }));

    if (context->options->runtime != Runtime_Custom) {
        // HACK
        context->special_global_entities.remaining = 5;

        context->special_global_entities.runtime_info_types_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context, "core:runtime/info/types", -1),
        }));
        context->special_global_entities.runtime_info_foreign_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context, "core:runtime/info/foreign_blocks", -1),
        }));
        context->special_global_entities.runtime_info_proc_tags_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context, "core:runtime/info/proc_tags", -1),
        }));
        context->special_global_entities.runtime_info_global_tags_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context, "core:runtime/info/global_tags", -1),
        }));
        context->special_global_entities.runtime_info_stack_trace_entity = entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context, "core:runtime/info/stack_trace", -1),
        }));
    }

    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.stack_top, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.heap_start, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.tls_base, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.tls_size, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.closure_base, context->global_scope, NULL);
    add_entities_for_node(&context->entities, NULL, (AstNode *) &context->builtins.stack_trace, context->global_scope, NULL);

    if (!context->options->no_core) {
        entity_heap_insert(&context->entities, ((Entity) {
            .state = Entity_State_Parse,
            .type = Entity_Type_Load_File,
            .package = NULL,
            .include = create_load(context, "core:module", -1),
        }));
    }

    if (context->options->generate_symbol_info_file) {
        context->symbol_info = bh_alloc_item(context->gp_alloc, SymbolInfoTable);
        bh_imap_init(&context->symbol_info->node_to_id, context->gp_alloc, 512);
        bh_arr_new(context->gp_alloc, context->symbol_info->symbols, 128);
        bh_arr_new(context->gp_alloc, context->symbol_info->symbols_resolutions, 128);
        sh_new_arena(context->symbol_info->files);
    }

    if (context->options->generate_odoc) {
        context->doc_info = bh_alloc_item(context->gp_alloc, OnyxDocInfo);
        memset(context->doc_info, 0, sizeof(OnyxDocInfo));
        bh_arr_new(context->gp_alloc, context->doc_info->procedures, 128);
        bh_arr_new(context->gp_alloc, context->doc_info->structures, 128);
        bh_arr_new(context->gp_alloc, context->doc_info->enumerations, 128);
    }
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

    // if (context->options->verbose_output == 2)
    //     bh_printf("Processing source file:    %s (%d bytes)\n", file.filename, fc.length);

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
            // Currently, introducing symbols is handled in the checker
            // function. Maybe there should be a different place where that happens?
            check_entity(context, ent);
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

        case Entity_State_Check_Types:     check_entity(context, ent);  break;
        case Entity_State_Code_Gen:        emit_entity(context, ent);   break;

        default: break;
    }

    b32 changed = ent->state != before_state;
    return changed;
}

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

onyx_pump_t onyx_pump(onyx_context_t *ctx) {
    Context *context = &ctx->context;

    compiler_events_clear(context);

    if (bh_arr_is_empty(context->entities.entities)) {
        // Once the module has been linked, we are all done and ready to say everything compiled successfully!
        if (context->wasm_module_linked) return ONYX_PUMP_DONE;

        link_wasm_module(context);
        context->wasm_module_linked = 1;
        return ONYX_PUMP_DONE;
    }

    Entity* ent = entity_heap_top(&context->entities);

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
        if (!context->watermarked_node || context->watermarked_node->macro_attempts < ent->macro_attempts) {
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
                } else if (context->cycle_almost_detected == 2) {
                    compiler_event_add(context, 4);
                }

                context->cycle_almost_detected += 1;
            }
        }
    } else {
        context->watermarked_node = NULL;
        context->cycle_almost_detected = 0;
    }

    if (onyx_has_errors(context)) {
        return ONYX_PUMP_ERRORED;
    }

    if (ent->state != Entity_State_Finalized && ent->state != Entity_State_Failed)
        entity_heap_insert_existing(&context->entities, ent);

    if (context->options->running_perf) {
        u64 perf_end = bh_time_curr_micro();

        u64 duration = perf_end - perf_start;
        context->stats.microseconds_per_type[perf_entity_type] += duration;
        context->stats.microseconds_per_state[perf_entity_state] += duration;
    }

    return ONYX_PUMP_CONTINUE;
}

//
// Events
//

int32_t onyx_event_count(onyx_context_t *ctx) {
    return ctx->context.events.event_count;
}

onyx_event_type_t onyx_event_type(onyx_context_t *ctx, int event_idx) {
    if (event_idx >= ctx->context.events.event_count) return ONYX_EVENT_UNKNOWN;

    CompilerEvent *ev = ctx->context.events.first;
    while (event_idx-- > 0 && ev) {
        ev = ev->next;
    }

    if (!ev) return ONYX_EVENT_UNKNOWN;

    return (onyx_event_type_t) ev->type;
}

int32_t onyx_event_field_int(onyx_context_t *ctx, int event_idx, char *field) {
    if (event_idx >= ctx->context.events.event_count) return 0;

    CompilerEvent *ev = ctx->context.events.first;
    while (ev && event_idx-- > 0) {
        ev = ev->next;
    }

    if (!ev) return 0;

    CompilerEventField *f = ev->first_field;
    while (f && strcmp(f->field, field) != 0) {
        f = f->next;
    }

    if (!f) return 0;
    if (f->type != 1) return 0;

    return f->i;
}

const char *onyx_event_field_str(onyx_context_t *ctx, int event_idx, char *field) {
    if (event_idx >= ctx->context.events.event_count) return "";

    CompilerEvent *ev = ctx->context.events.first;
    while (ev && event_idx-- > 0) {
        ev = ev->next;
    }

    if (!ev) return "";

    CompilerEventField *f = ev->first_field;
    while (f && strcmp(f->field, field) != 0) {
        f = f->next;
    }

    if (!f) return "";
    if (f->type != 0) return "";

    return f->s;
}





//
// Options
//
int32_t onyx_set_option_cstr(onyx_context_t *ctx, onyx_option_t opt, char *value) {
    return onyx_set_option_bytes(ctx, opt, value, strlen(value));
}

int32_t onyx_set_option_bytes(onyx_context_t *ctx, onyx_option_t opt, char *value, int32_t length) {
    if (length < 0) length = strlen(value);
    return 0;
}

int32_t onyx_set_option_int(onyx_context_t *ctx, onyx_option_t opt, int32_t value) {
    switch (opt) {
    case ONYX_OPTION_POST_MVP_FEATURES:      ctx->context.options->use_post_mvp_features = value; return 1;
    case ONYX_OPTION_MULTI_THREADING:        ctx->context.options->use_multi_threading = value;  return 1;
    case ONYX_OPTION_GENERATE_FOREIGN_INFO:  ctx->context.options->generate_foreign_info = value; return 1;
    case ONYX_OPTION_GENERATE_TYPE_INFO:     ctx->context.options->generate_type_info = value; return 1;
    case ONYX_OPTION_GENERATE_METHOD_INFO:   ctx->context.options->generate_method_info = value; return 1;
    case ONYX_OPTION_GENERATE_DEBUG_INFO:    ctx->context.options->debug_info_enabled = value; return 1;
    case ONYX_OPTION_GENERATE_STACK_TRACE:   ctx->context.options->stack_trace_enabled = value; return 1;
    case ONYX_OPTION_GENERATE_NAME_SECTION:  ctx->context.options->generate_name_section = value; return 1;
    case ONYX_OPTION_GENERATE_SYMBOL_INFO:   ctx->context.options->generate_symbol_info_file = value; return 1;
    case ONYX_OPTION_GENERATE_LSP_INFO:      ctx->context.options->generate_lsp_info_file = value; return 1;
    case ONYX_OPTION_GENERATE_DOC_INFO:      ctx->context.options->generate_odoc = value; return 1;
    case ONYX_OPTION_DISABLE_CORE:           ctx->context.options->no_core = value; return 1;
    case ONYX_OPTION_DISABLE_STALE_CODE:     ctx->context.options->no_stale_code = value; return 1;
    case ONYX_OPTION_OPTIONAL_SEMICOLONS:    ctx->context.options->enable_optional_semicolons = value; return 1;
    case ONYX_OPTION_DISABLE_FILE_CONTENTS:  ctx->context.options->no_file_contents = value; return 1;
    case ONYX_OPTION_DISABLE_EXTENSIONS:     ctx->context.options->no_compiler_extensions = value; return 1;
    case ONYX_OPTION_PLATFORM:               ctx->context.options->runtime = value; return 1;

    default:
        break;
    }

    return 0;
}

void onyx_add_defined_var(onyx_context_t *ctx, char *variable, int32_t variable_length, char *value, int32_t value_length) {
    if (variable_length < 0) variable_length = strlen(variable);
    if (value_length    < 0) value_length    = strlen(value);

    bh_arr_push(ctx->context.defined_variables, ((DefinedVariable) {
        .key   = bh_strdup_len(ctx->context.ast_alloc, variable, variable_length),
        .value = bh_strdup_len(ctx->context.ast_alloc, value, value_length),
    }));
}

//
// Loading code
//

/// Adds a file to the compilation, following typical `#load` rules.
/// 1. `foo:file.onyx` will search in the `foo` mapped folder.
/// 2. `file.onyx` will search in the current directory for `file.onyx`.
void onyx_include_file(onyx_context_t *ctx, char *filename, int32_t length) {
    if (length < 0) length = strlen(filename);

    AstInclude* load_node = create_load(&ctx->context, filename, -1);
    add_entities_for_node(&ctx->context.entities, NULL, (AstNode *) load_node, ctx->context.global_scope, NULL);
}

void onyx_add_mapped_dir(onyx_context_t *ctx, char *mapped_name, int32_t mapped_length, char *dir, int32_t dir_length) {
    if (mapped_length < 0) mapped_length = strlen(mapped_name);
    if (dir_length    < 0) dir_length    = strlen(dir);

    bh_arr_push(ctx->context.options->mapped_folders, ((bh_mapped_folder) {
        bh_strdup_len(ctx->context.ast_alloc, mapped_name, mapped_length),
        bh_strdup_len(ctx->context.ast_alloc, dir, dir_length)
    }));
}

/// Directly injects Onyx code as a new compilation unit
void onyx_inject_code(onyx_context_t *ctx, uint8_t *code, int32_t length) {
    assert(0 && "unimplemented");
}

//
// Output
//

int32_t onyx_error_count(onyx_context_t *ctx) {
    return bh_arr_length(ctx->context.errors.errors);
}

const char *onyx_error_message(onyx_context_t *ctx, int32_t error_idx) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return NULL;

    return ctx->context.errors.errors[error_idx].text;
}

const char *onyx_error_filename(onyx_context_t *ctx, int32_t error_idx) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return NULL;

    return ctx->context.errors.errors[error_idx].pos.filename;
}

int32_t onyx_error_line(onyx_context_t *ctx, int32_t error_idx) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return 0;

    return ctx->context.errors.errors[error_idx].pos.line;
}

int32_t onyx_error_column(onyx_context_t *ctx, int32_t error_idx) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return 0;

    return ctx->context.errors.errors[error_idx].pos.column;
}

int32_t onyx_error_length(onyx_context_t *ctx, int32_t error_idx) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return 0;

    return ctx->context.errors.errors[error_idx].pos.length;
}

int32_t onyx_error_line_text(onyx_context_t *ctx, int32_t error_idx, char *line_buffer, int max_length) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return 0;

    int line_length = 0;
    char *walker = ctx->context.errors.errors[error_idx].pos.line_start;
    if (!walker) return 0;

    while (*walker && *walker++ != '\n') line_length++;

    if (line_buffer != NULL && max_length > 0) {
        i32 to_copy = bh_min(max_length - 1, line_length);
        memcpy(line_buffer, ctx->context.errors.errors[error_idx].pos.line_start, to_copy);
        line_buffer[to_copy] = '\0';
    }

    return line_length;
}

onyx_error_t onyx_error_rank(onyx_context_t *ctx, int32_t error_idx) {
    int32_t error_count = onyx_error_count(ctx);
    if (error_idx < 0 || error_idx >= error_count) return 0;

    return (onyx_error_t) ctx->context.errors.errors[error_idx].rank;
}


//
// Code Generation
//

static void ensure_wasm_has_been_generated(onyx_context_t *ctx) {
    if (ctx->context.generated_wasm_buffer.length == 0) {
        if (onyx_has_errors(&ctx->context)) return;
        onyx_wasm_module_write_to_buffer(ctx->context.wasm_module, &ctx->context.generated_wasm_buffer);
    }
}

static void ensure_js_has_been_generated(onyx_context_t *ctx) {
    if (ctx->context.generated_js_buffer.data == NULL) {
        if (onyx_has_errors(&ctx->context)) return;
        onyx_wasm_module_write_js_partials_to_buffer(ctx->context.wasm_module, &ctx->context.generated_js_buffer);
    }
}

static void ensure_odoc_has_been_generated(onyx_context_t *ctx) {
    if (ctx->context.generated_odoc_buffer.data == NULL) {
        if (onyx_has_errors(&ctx->context)) return;
        onyx_docs_generate_odoc(&ctx->context, &ctx->context.generated_odoc_buffer);
    }
}

static void ensure_osym_has_been_generated(onyx_context_t *ctx) {
    if (ctx->context.generated_osym_buffer.data == NULL) {
        if (onyx_has_errors(&ctx->context)) return;
        onyx_docs_emit_symbol_info(&ctx->context, &ctx->context.generated_osym_buffer);
    }
}

int32_t onyx_output_length(onyx_context_t *ctx, onyx_output_type_t type) {
    switch (type) {
    case ONYX_OUTPUT_TYPE_WASM:
        ensure_wasm_has_been_generated(ctx);
        return ctx->context.generated_wasm_buffer.length;

    case ONYX_OUTPUT_TYPE_JS:
        ensure_js_has_been_generated(ctx);
        return ctx->context.generated_js_buffer.length;
    
    case ONYX_OUTPUT_TYPE_ODOC:
        ensure_odoc_has_been_generated(ctx);
        return ctx->context.generated_odoc_buffer.length;

    case ONYX_OUTPUT_TYPE_OSYM:
        ensure_osym_has_been_generated(ctx);
        return ctx->context.generated_osym_buffer.length;
    }

    return 0;
}

void onyx_output_write(onyx_context_t *ctx, onyx_output_type_t type, void *buffer) {
    switch (type) {
    case ONYX_OUTPUT_TYPE_WASM:
        ensure_wasm_has_been_generated(ctx);
        memcpy(buffer, ctx->context.generated_wasm_buffer.data, ctx->context.generated_wasm_buffer.length);
        break;

    case ONYX_OUTPUT_TYPE_JS:
        ensure_js_has_been_generated(ctx);
        memcpy(buffer, ctx->context.generated_js_buffer.data, ctx->context.generated_js_buffer.length);
        break;

    case ONYX_OUTPUT_TYPE_ODOC:
        ensure_odoc_has_been_generated(ctx);
        memcpy(buffer, ctx->context.generated_odoc_buffer.data, ctx->context.generated_odoc_buffer.length);
        break;

    case ONYX_OUTPUT_TYPE_OSYM:
        ensure_osym_has_been_generated(ctx);
        memcpy(buffer, ctx->context.generated_osym_buffer.data, ctx->context.generated_osym_buffer.length);
        break;
    }
}


//
// Compilation Info
//

int64_t onyx_stat(onyx_context_t *ctx, onyx_stat_t stat) {
    switch (stat) {
        case ONYX_STAT_FILE_COUNT:  return bh_arr_length(ctx->context.loaded_files);
        case ONYX_STAT_LINE_COUNT:  return ctx->context.stats.lexer_lines_processed;
        case ONYX_STAT_TOKEN_COUNT: return ctx->context.stats.lexer_tokens_processed;
        default: return -1;
    }
}

const char *onyx_stat_filepath(onyx_context_t *ctx, int32_t file_index) {
    if (file_index < 0 || file_index >= bh_arr_length(ctx->context.loaded_files)) {
        return NULL;
    }

    return ctx->context.loaded_files[file_index].filename;
}



//
// Running WASM
//

#ifdef ONYX_RUNTIME_LIBRARY
void onyx_run_wasm(void *buffer, int32_t buffer_length, int argc, char **argv) {
    onyx_run_initialize(0, NULL);

    bh_buffer wasm_bytes;
    wasm_bytes.data = buffer;
    wasm_bytes.length = buffer_length;

    onyx_run_wasm_code(wasm_bytes, argc, argv);
}

void onyx_run_wasm_with_debug(void *buffer, int32_t buffer_length, int argc, char **argv, char *socket_path) {
    onyx_run_initialize(1, socket_path);

    bh_buffer wasm_bytes;
    wasm_bytes.data = buffer;
    wasm_bytes.length = buffer_length;

    onyx_run_wasm_code(wasm_bytes, argc, argv);
}
#else
void onyx_run_wasm(void *buffer, int32_t buffer_length, int argc, char **argv) {
    printf("ERROR: Cannot run WASM code. No runtime was configured at the time Onyx was built");
}

void onyx_run_wasm_with_debug(void *buffer, int32_t buffer_length, int argc, char **argv, char *socket_path) {
    printf("ERROR: Cannot run WASM code. No runtime was configured at the time Onyx was built");
}
#endif



// Internal procedures

static AstInclude* create_load(Context *context, char* filename, int32_t length) {
    static OnyxToken implicit_load_token = { '#', 1, 0, { 0, 0, 0, 0, 0 } };
    
    AstInclude* include_node = onyx_ast_node_new(context->ast_alloc, sizeof(AstInclude), Ast_Kind_Load_File);
    include_node->name = bh_strdup_len(context->ast_alloc, filename, length);
    include_node->token = &implicit_load_token;

    return include_node;
}

static void introduce_defined_variables(Context *context) {
    bh_arr_each(DefinedVariable, var, context->defined_variables) {
        create_and_add_defined_variable(context, var);
    }
}

static void create_and_add_defined_variable(Context *context, DefinedVariable *var) {
    OnyxToken *value_token = bh_alloc_item(context->ast_alloc, OnyxToken);
    value_token->text = var->value;
    value_token->length = strlen(var->value);

    OnyxToken *name_token = bh_alloc_item(context->ast_alloc, OnyxToken);
    name_token->text = var->key;
    name_token->length = strlen(var->key);

    Package *p = package_lookup(context, "runtime.vars");
    assert(p);

    AstStrLit *value_node = make_string_literal(context, value_token);
    add_entities_for_node(&context->entities, NULL, (AstNode *) value_node, NULL, NULL);

    AstBinding *binding = onyx_ast_node_new(context->ast_alloc, sizeof(AstBinding), Ast_Kind_Binding);
    binding->token = name_token;
    binding->node = (AstNode *) value_node;

    add_entities_for_node(&context->entities, NULL, (AstNode *) binding, p->scope, p);
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
