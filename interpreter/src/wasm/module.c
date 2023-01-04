

#include "ovm_wasm.h"
#include "vm_codebuilder.h"
#include "stb_ds.h"

#include "./module_parsing.h"

static bool module_build(wasm_module_t *module, const wasm_byte_vec_t *binary) {
    wasm_engine_t *engine = module->store->engine;
    module->program = ovm_program_new(engine->store);

    build_context ctx;
    ctx.binary  = *binary;
    ctx.offset  = 8;  // Skip the magic bytes and version
    ctx.module  = module;
    ctx.program = module->program;
    ctx.store   = engine->store;
    ctx.next_external_func_idx = 0;

    debug_info_builder_init(&ctx.debug_builder, &module->debug_info);
    sh_new_arena(module->custom_sections);

    while (ctx.offset < binary->size) {
        parse_section(&ctx);
    }

    // TODO: This is not correct when the module imports a global.
    // But Onyx does not do this, so I don't care at the moment.
    module->program->register_count = module->globaltypes.size;

    #if 0
        printf("Program instruction count: %d\n", bh_arr_length(module->program->code));
    #endif

    return true;
}


#define WASM_MODULE_INDEX(k1, k2) \
    wasm_##k1##type_t *wasm_module_index_##k1##type(wasm_module_t *module, int index) { \
        fori (i, 0, (int) module->imports.size) { \
            if (module->imports.data[i]->type->kind == k2) { \
                if (index == 0) { \
                    return wasm_externtype_as_##k1##type(module->imports.data[i]->type); \
                } \
     \
                index -= 1; \
            } \
        } \
     \
        if (index < (int) module->k1##types.size) { \
            return module->k1##types.data[index]; \
        } \
     \
        return NULL; \
    }

WASM_MODULE_INDEX(func, WASM_EXTERN_FUNC)
WASM_MODULE_INDEX(memory, WASM_EXTERN_MEMORY)
WASM_MODULE_INDEX(table, WASM_EXTERN_TABLE)
WASM_MODULE_INDEX(global, WASM_EXTERN_GLOBAL)

#undef WASM_MODULE_INDEX


// Ommitting the "sharable ref" crap that I don't think will
// ever be needed for a module.


wasm_module_t *wasm_module_new(wasm_store_t *store, const wasm_byte_vec_t *binary) {
    wasm_module_t *module = bh_alloc(store->engine->store->arena_allocator, sizeof(*module));
    memset(module, 0, sizeof(*module));
    module->store = store;

    debug_info_init(&module->debug_info);

    if (store->engine->engine->debug) {
        assert(store->engine->engine->debug->info == NULL);
        store->engine->engine->debug->info = &module->debug_info;
    }

    bool success = module_build(module, binary); 
    return module;
}

void wasm_module_delete(wasm_module_t *module) {
    ovm_program_delete(module->program);
}

bool wasm_module_validate(wasm_store_t *store, const wasm_byte_vec_t *binary) {
    // Hmmm...
    return false;
}

void wasm_module_imports(const wasm_module_t *module, wasm_importtype_vec_t *out_imports) {
    *out_imports = module->imports;
}

void wasm_module_exports(const wasm_module_t *module, wasm_exporttype_vec_t *out_exports) {
    *out_exports = module->exports;
}



