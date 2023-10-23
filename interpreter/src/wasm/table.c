
#include "ovm_wasm.h"
#include "vm.h"

wasm_table_t *wasm_table_new(wasm_store_t *store, const wasm_tabletype_t *type, wasm_ref_t *init) {
    wasm_table_t *table = bh_alloc(store->engine->store->arena_allocator, sizeof(*table));
    table->inner.type = wasm_tabletype_as_externtype_const(type);
    table->inner.table.type = type;
    
    return table;
}

wasm_tabletype_t *wasm_table_type(const wasm_table_t *table) {
    return (wasm_tabletype_t *) table->inner.table.type;
}

wasm_ref_t *wasm_table_get(const wasm_table_t *table, unsigned int index) {
    const struct wasm_table_inner_t *tab = &table->inner.table;
    ovm_program_t *program = tab->program;
    ovm_engine_t  *engine  = tab->engine;

    // TODO bounds checking
    wasm_func_t *func = tab->instance->funcs[
        program->static_integers[program->static_data[tab->static_arr].start_idx + index]
    ];

    wasm_ref_t *new_ref = bh_alloc(engine->store->arena_allocator, sizeof(*new_ref));
    new_ref->stored_obj = wasm_ref_stored_obj_func;
    new_ref->func = func;

    return new_ref;
}