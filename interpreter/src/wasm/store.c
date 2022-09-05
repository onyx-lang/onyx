
#include "ovm_wasm.h"
#include "vm.h"

wasm_store_t *wasm_store_new(wasm_engine_t *engine) {
    wasm_store_t *store = bh_alloc(engine->store->arena_allocator, sizeof(wasm_store_t));
    store->engine = engine;
    store->instance = NULL;
    return store;
}

void wasm_store_delete(wasm_store_t *store) {
}

