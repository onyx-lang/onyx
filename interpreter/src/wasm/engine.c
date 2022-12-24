
#include "ovm_wasm.h"
#include "vm.h"

wasm_engine_t *wasm_engine_new() {
    wasm_engine_t *engine = wasm_engine_new_with_config(NULL);
    return engine;
}

wasm_engine_t *wasm_engine_new_with_config(wasm_config_t *config) {
    ovm_store_t *store = ovm_store_new();

    wasm_engine_t *engine = bh_alloc_item(store->heap_allocator, wasm_engine_t);
    engine->config = config;
    engine->store = store;
    
    ovm_engine_t *ovm_engine = ovm_engine_new(store);
    engine->engine = ovm_engine;

    if (config && config->debug_enabled) {
        // This should maybe be moved elsewhere?
        debug_state_t *debug  = bh_alloc_item(store->heap_allocator, debug_state_t);
        engine->engine->debug = debug;

        debug_host_init(engine->engine->debug, engine->engine);
        debug->listen_path = config->listen_path;

        debug_host_start(engine->engine->debug);
    }

    return engine;
}

void wasm_engine_delete(wasm_engine_t *engine) {
    if (engine->engine->debug) {
        debug_host_stop(engine->engine->debug);
    }

    ovm_store_t *store = engine->store;
    ovm_engine_delete(engine->engine);
    bh_free(store->heap_allocator, engine);
    ovm_store_delete(store);
}

