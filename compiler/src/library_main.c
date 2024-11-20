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

struct onyx_context_t {
	Context context;
};

//
// Lifecycle
//

onyx_context_t *onyx_context_create() {
	onyx_content_t *ctx = malloc(sizeof(*ctx));

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

    context->options = malloc(sizeof(* context->options));

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
}

void onyx_context_free(onyx_context_t *ctx) {
	Context *context = &ctx->context;

    bh_arena_free(&context->ast_arena);
    bh_arr_free(context->loaded_files);
    bh_scratch_free(&context->scratch);
    bh_managed_heap_free(&context->heap);
}

void onyx_options_ready(onyx_context_t *ctx) {
	Context *context = &ctx->context;

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
    bh_arr_each(const char *, filename, context->options->files) {
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

    // if (context->options->verbose_output > 0) {
    //     bh_printf("Mapped folders:\n");
    //     bh_arr_each(bh_mapped_folder, p, context->options->mapped_folders) {
    //         bh_printf("\t%s: %s\n", p->name, p->folder);
    //     }
    //     bh_printf("\n");
    // }
}

onyx_pump_t onyx_pump(onyx_context_t *ctx) {

}

//
// Options
//
void onyx_set_option_cstr(onyx_context_t *ctx, onyx_option_t opt, char *value) {

}

void onyx_set_option_bytes(onyx_context_t *ctx, onyx_option_t opt, char *value, int32_t length) {

}

void onyx_set_option_int(onyx_context_t *ctx, onyx_option_t opt, int32_t value) {

}

//
// Loading code
//

/// Adds a file to the compilation, following typical `#load` rules.
/// 1. `foo:file.onyx` will search in the `foo` mapped folder.
/// 2. `file.onyx` will search in the current directory for `file.onyx`.
void onyx_include_file_cstr(onyx_context_t *ctx, char *filename) {

}

void onyx_include_file(onyx_context_t *ctx, char *filename, int32_t length) {

}

/// Directly injects Onyx code as a new compilation unit
void onyx_inject_code(onyx_context_t *ctx, uint32_t *code, u32 length) {
	
}

//
// Output
//

int32_t onyx_error_count(onyx_context_t *ctx) {

}

const char *onyx_error_message(onyx_context_t *ctx, i32 error_idx) {

}

const char *onyx_error_filename(onyx_context_t *ctx, i32 error_idx) {

}

int32_t onyx_error_line(onyx_context_t *ctx, i32 error_idx) {

}

int32_t onyx_error_column(onyx_context_t *ctx, i32 error_idx) {

}

int32_t onyx_error_length(onyx_context_t *ctx, i32 error_idx) {

}


int32_t onyx_wasm_output_length(onyx_context_t *ctx) {

}

void onyx_wasm_output_write(onyx_context_t *ctx, void *buffer) {

}
#endif



int main(int argc, char const *argv[]) {
	onyx_context_t *ctx = onyx_context_create();

	onyx_include_file_cstr(ctx, "hello.onyx");

	while (onyx_pump(ctx) == ONYX_PUMP_CONTINUE) {
		// Message processing, if enabled
	}

	int64_t output_length = onyx_wasm_output_length(ctx);
	void *output = malloc(output_length);
	onyx_wasm_output_write(ctx, output);

	FILE* output_fd = fopen("hello.wasm", "wb");
	fwrite(output_fd, output, output_length);
	fclose(output_fd);

	onyx_context_free(ctx);
}