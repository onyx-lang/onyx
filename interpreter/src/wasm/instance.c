

#include "ovm_wasm.h"
#include "vm.h"
#include <alloca.h>

static_assert(sizeof(ovm_value_t) == sizeof(wasm_val_t), "Size of ovm_value_t should match size of wasm_val_t");

typedef struct wasm_ovm_binding wasm_ovm_binding;
struct wasm_ovm_binding {
    int func_idx;
    ovm_engine_t  *engine;
    ovm_state_t   *state;
    ovm_program_t *program;

    wasm_instance_t *instance;
};

typedef struct ovm_wasm_binding ovm_wasm_binding;
struct ovm_wasm_binding {
    int param_count;
    int result_count;
    wasm_func_t *func;
    wasm_val_vec_t param_buffer;
};

#define WASM_TO_OVM(w, o) { \
    (o).u64 = 0;\
    switch ((w).kind) { \
        case WASM_I32: \
            (o).type = OVM_TYPE_I32; \
            (o).i32  = (w).of.i32; \
            break; \
 \
        case WASM_I64: \
            (o).type = OVM_TYPE_I64; \
            (o).i64  = (w).of.i64; \
            break; \
 \
        case WASM_F32: \
            (o).type = OVM_TYPE_F32; \
            (o).f32  = (w).of.f32; \
            break; \
 \
        case WASM_F64: \
            (o).type = OVM_TYPE_F64; \
            (o).f64  = (w).of.f64; \
            break; \
 \
        default: assert(0 && "invalid wasm value type for conversion"); \
    } }

#define OVM_TO_WASM(o, w) { \
    (w).of.i64 = 0;\
    switch ((o).type) { \
        case OVM_TYPE_NONE: \
            (w).kind = WASM_I32; \
            (w).of.i32 = 0; \
            break; \
 \
        case OVM_TYPE_I8: \
            (w).kind = WASM_I32; \
            (w).of.i32 = (i32) (o).i8; \
            break; \
 \
        case OVM_TYPE_I16: \
            (w).kind = WASM_I32; \
            (w).of.i32 = (i32) (o).i16; \
            break; \
 \
        case OVM_TYPE_I32: \
            (w).kind = WASM_I32; \
            (w).of.i32 = (i32) (o).i32; \
            break; \
 \
        case OVM_TYPE_I64: \
            (w).kind = WASM_I64; \
            (w).of.i64 = (o).i64; \
            break; \
 \
        case OVM_TYPE_F32: \
            (w).kind = WASM_F32; \
            (w).of.f32 = (o).f32; \
            break; \
 \
        case OVM_TYPE_F64: \
            (w).kind = WASM_F64; \
            (w).of.f64 = (o).f64; \
            break; \
 \
        case OVM_TYPE_ERR: \
            (w).kind = WASM_I32; \
            (w).of.i32 = 0; \
            break; \
 \
        default: \
            printf("INVALID: %d\n", (o).type); \
            assert(0 && "invalid ovm value type for conversion"); \
    } }

static wasm_trap_t *wasm_to_ovm_func_call_binding(void *vbinding, const wasm_val_vec_t *args, wasm_val_vec_t *res) {
    wasm_ovm_binding *binding = (wasm_ovm_binding *) vbinding;

    ovm_value_t *vals = alloca(sizeof(*vals) * args->size);
    fori (i, 0, (int) args->size) {
        WASM_TO_OVM(args->data[i], vals[i]);
    }

    ovm_value_t ovm_res = ovm_func_call(binding->engine, binding->state, binding->program, binding->func_idx, args->size, vals);

    // Check for error (trap).
    if (ovm_res.type == OVM_TYPE_ERR) {
        wasm_byte_vec_t msg;
        wasm_byte_vec_new(&msg, 9, "Hit error");
        wasm_trap_t *trap = wasm_trap_new(binding->instance->store, (void *) &msg);
        return trap;
    }

    if (!res || res->size == 0) return NULL;

    OVM_TO_WASM(ovm_res, res->data[0]);

    return NULL;
}

static void ovm_to_wasm_func_call_binding(void *env, ovm_value_t* params, ovm_value_t *res) {
    ovm_wasm_binding *binding = (ovm_wasm_binding *) env;

    fori (i, 0, binding->param_count) {
        OVM_TO_WASM(params[i], binding->param_buffer.data[i]);
    }

    wasm_val_t return_value;
    wasm_val_vec_t wasm_results;
    wasm_results.data = &return_value;
    wasm_results.size = binding->result_count;

    wasm_trap_t *trap = wasm_func_call(binding->func, &binding->param_buffer, &wasm_results);
    assert(!trap);

    if (binding->result_count > 0) {
        assert(wasm_results.data[0].kind == binding->func->inner.type->func.results.data[0]->kind);
        WASM_TO_OVM(return_value, *res);
    }
}

static void wasm_memory_init(void *env, ovm_value_t* params, ovm_value_t *res) {
    wasm_instance_t *instr = (wasm_instance_t *) env;

    assert(params[0].type == OVM_TYPE_I32);
    assert(params[1].type == OVM_TYPE_I32);
    assert(params[2].type == OVM_TYPE_I32);
    assert(params[3].type == OVM_TYPE_I32);

    ovm_engine_memory_copy(instr->store->engine->engine, params[0].i32, instr->module->data_entries[params[3].i32].data, params[2].i32);
}

static void prepare_instance(wasm_instance_t *instance, const wasm_extern_vec_t *imports) {
    ovm_store_t   *ovm_store   = instance->store->engine->store;
    ovm_engine_t  *ovm_engine  = instance->store->engine->engine;
    ovm_state_t   *ovm_state   = instance->state;
    ovm_program_t *ovm_program = instance->module->program;

    //
    // Place imports in their corresponding "bucket"
    fori (i, 0, (int) imports->size) {
        assert(instance->module->imports.data[i]->type->kind == imports->data[i]->type->kind);

        switch (wasm_extern_kind(imports->data[i])) {
            case WASM_EXTERN_FUNC: {
                wasm_importtype_t *importtype = instance->module->imports.data[i];
                struct wasm_functype_inner_t *functype = &importtype->type->func;

                if (!wasm_functype_equals(
                        wasm_externtype_as_functype(importtype->type),
                        wasm_externtype_as_functype((wasm_externtype_t *) imports->data[i]->type))) {
                    assert(0 && "MISMATCHED FUNCTION TYPE");
                }

                wasm_func_t *func = wasm_extern_as_func(imports->data[i]);
                bh_arr_push(instance->funcs, func);

                ovm_wasm_binding *binding = bh_alloc(ovm_store->arena_allocator, sizeof(*binding));
                binding->param_count  = functype->params.size;
                binding->result_count = functype->results.size;
                binding->func         = func;
                binding->param_buffer.data = bh_alloc(ovm_store->arena_allocator, sizeof(wasm_val_t) * binding->param_count);
                binding->param_buffer.size = binding->param_count;

                ovm_state_register_external_func(ovm_state, importtype->external_func_idx, ovm_to_wasm_func_call_binding, binding);
                break;
            }

            case WASM_EXTERN_MEMORY: {
                wasm_memory_t *memory = wasm_extern_as_memory(imports->data[i]);
                bh_arr_push(instance->memories, memory);

                memory->inner.memory.engine = ovm_engine;
                break;
            }

            case WASM_EXTERN_GLOBAL: {
                wasm_global_t *global = wasm_extern_as_global(imports->data[i]);

                global->inner.global.engine = ovm_engine;
                global->inner.global.state  = ovm_state;
                global->inner.global.register_index = bh_arr_length(instance->globals);

                ovm_value_t val = {0};
                WASM_TO_OVM(global->inner.global.initial_value, val);
                ovm_state_register_set(ovm_state, global->inner.global.register_index, val);

                bh_arr_push(instance->globals, global);
                break;
            }

            case WASM_EXTERN_TABLE: {
                wasm_table_t *table = wasm_extern_as_table(imports->data[i]);
                table->inner.table.engine = ovm_engine;
                table->inner.table.program = ovm_program;
                table->inner.table.static_arr = instance->module->imports.data[i]->type->table.static_arr;

                bh_arr_push(instance->tables, table);
                break;
            }
        }
    }

    ovm_state_register_external_func(ovm_state, instance->module->memory_init_external_idx, wasm_memory_init, instance);

    //
    // Create function objects
    fori (i, 0, (int) instance->module->functypes.size) {
        wasm_ovm_binding *binding = bh_alloc(instance->store->engine->store->arena_allocator, sizeof(*binding));
        binding->engine   = ovm_engine;
        binding->func_idx = bh_arr_length(instance->funcs);
        binding->program  = ovm_program;
        binding->state    = ovm_state;
        binding->instance = instance;

        wasm_func_t *func = wasm_func_new_with_env(instance->store, instance->module->functypes.data[i],
            wasm_to_ovm_func_call_binding, binding, NULL);

        bh_arr_push(instance->funcs, func);
    }

    //
    // Create memory objects
    fori (i, 0, (int) instance->module->memorytypes.size) {
        wasm_memory_t *memory = wasm_memory_new(instance->store, instance->module->memorytypes.data[i]);
        memory->inner.memory.engine = ovm_engine;

        bh_arr_push(instance->memories, memory);
    }

    //
    // Create table objects
    fori (i, 0, (int) instance->module->tabletypes.size) {
        wasm_table_t *table = wasm_table_new(instance->store, instance->module->tabletypes.data[i], NULL);
        table->inner.table.engine     = ovm_engine;
        table->inner.table.program    = ovm_program;
        table->inner.table.instance   = instance;
        table->inner.table.static_arr = instance->module->tabletypes.data[i]->type.table.static_arr;

        bh_arr_push(instance->tables, table);
    }

    //
    // Create global objects
    fori (i, 0, (int) instance->module->globaltypes.size) {
        wasm_global_t *global = wasm_global_new(instance->store, instance->module->globaltypes.data[i],
            &instance->module->globaltypes.data[i]->type.global.initial_value);

        global->inner.global.engine         = ovm_engine;
        global->inner.global.state          = ovm_state;
        global->inner.global.register_index = bh_arr_length(instance->globals);

        ovm_value_t val = {0};
        WASM_TO_OVM(global->inner.global.initial_value, val);
        ovm_state_register_set(ovm_state, global->inner.global.register_index, val);

        bh_arr_push(instance->globals, global);
    }


    //
    // Initialize all non-passive data segments
    fori (i, 0, (int) instance->module->data_count) {
        struct wasm_data_t *datum = &instance->module->data_entries[i];
        if (datum->passive) continue;

        ovm_engine_memory_copy(ovm_engine, datum->offset, datum->data, datum->length);
    }

    wasm_extern_vec_new_uninitialized(&instance->exports, instance->module->exports.size);
    fori (i, 0, (int) instance->module->exports.size) {
        wasm_exporttype_t *externtype = instance->module->exports.data[i];

        switch (externtype->type->kind) {
            case WASM_EXTERN_FUNC: {
                wasm_func_t *func = instance->funcs[externtype->index];
                instance->exports.data[i] = wasm_func_as_extern(func);
                break;
            }

            case WASM_EXTERN_MEMORY: {
                wasm_memory_t *memory = instance->memories[externtype->index];
                instance->exports.data[i] = wasm_memory_as_extern(memory);
                break;
            }

            case WASM_EXTERN_GLOBAL: {
                wasm_global_t *global = instance->globals[externtype->index];
                instance->exports.data[i] = wasm_global_as_extern(global);
                break;
            }

            case WASM_EXTERN_TABLE: {
                wasm_table_t *table = instance->tables[externtype->index];
                instance->exports.data[i] = wasm_table_as_extern(table);
                break;
            }
        }
    }
}

wasm_instance_t *wasm_instance_new(wasm_store_t *store, const wasm_module_t *module,
    const wasm_extern_vec_t *imports, wasm_trap_t **trap) {

    wasm_instance_t *instance = bh_alloc(store->engine->store->heap_allocator, sizeof(*instance));
    instance->store = store;
    instance->module = module;

    if (!store->instance) {
        store->instance = instance;
    }

    instance->funcs = NULL;
    instance->memories = NULL;
    instance->tables = NULL;
    instance->globals = NULL;
    bh_arr_new(store->engine->store->heap_allocator, instance->funcs, module->functypes.size);
    bh_arr_new(store->engine->store->heap_allocator, instance->memories, 1);
    bh_arr_new(store->engine->store->heap_allocator, instance->tables, 1);
    bh_arr_new(store->engine->store->heap_allocator, instance->globals, module->globaltypes.size);

    instance->state = ovm_state_new(store->engine->engine, module->program);

    prepare_instance(instance, imports);

    assert(bh_arr_length(instance->memories) == 1);
    u32 memory_size = (instance->memories[0]->inner.type->memory.limits.min) * MEMORY_PAGE_SIZE;
    ovm_engine_memory_ensure_capacity(store->engine->engine, memory_size);

    if (trap) *trap = NULL;

    return instance;
}

void wasm_instance_delete(wasm_instance_t *instance) {
    bh_arr_free(instance->funcs);
    bh_arr_free(instance->memories);
    bh_arr_free(instance->globals);
    bh_arr_free(instance->tables);

    wasm_extern_vec_delete(&instance->exports);
    ovm_state_delete(instance->state);
    bh_free(instance->store->engine->store->heap_allocator, instance);
}

void wasm_instance_exports(const wasm_instance_t *instance, wasm_extern_vec_t *out) {
    *out = instance->exports;
}
