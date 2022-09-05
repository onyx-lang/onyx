
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

