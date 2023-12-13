
#include "ovm_wasm.h"
#include "vm.h"

wasm_global_t *wasm_global_new(wasm_store_t *store, const wasm_globaltype_t *type, const wasm_val_t *initial) {
    wasm_global_t *global = bh_alloc(store->engine->store->arena_allocator, sizeof(*global));
    global->inner.type = wasm_globaltype_as_externtype_const(type);
    global->inner.global.register_index = -1;
    global->inner.global.engine = NULL;

    if (initial) {
        global->inner.global.initial_value = *initial;
    }

    return global;
}

wasm_globaltype_t *wasm_global_type(const wasm_global_t *global) {
    return (wasm_globaltype_t *) global->inner.global.type;
}

void wasm_global_get(const wasm_global_t *global, wasm_val_t *value) {
    assert(0 && "unimplemented");
}

void wasm_global_set(wasm_global_t *global, const wasm_val_t *value) {
    assert(0 && "unimplemented");
}

