
#include "ovm_wasm.h"
#include "vm.h"

wasm_memory_t *wasm_memory_new(wasm_store_t *store, const wasm_memorytype_t *type) {
    wasm_memory_t *memory = bh_alloc(store->engine->store->arena_allocator, sizeof(*store));
    memory->inner.type = wasm_memorytype_as_externtype_const(type);
    memory->inner.memory.type = type;
    memory->inner.memory.engine = NULL;

    return memory;
}

wasm_memorytype_t *wasm_memory_type(const wasm_memory_t *memory) {
    return (wasm_memorytype_t *) memory->inner.memory.type;
}

byte_t *wasm_memory_data(wasm_memory_t *memory) {
    assert(memory && memory->inner.memory.engine);
    return memory->inner.memory.engine->memory;
}

size_t wasm_memory_data_size(const wasm_memory_t *memory) {
    assert(memory && memory->inner.memory.engine);
    return memory->inner.memory.engine->memory_size;
}

wasm_memory_pages_t wasm_memory_size(const wasm_memory_t *memory) {
    assert(memory && memory->inner.memory.engine);
    return memory->inner.memory.engine->memory_size / MEMORY_PAGE_SIZE;
}

bool wasm_memory_grow(wasm_memory_t *memory, wasm_memory_pages_t pages) {
    return ovm_engine_memory_ensure_capacity(memory->inner.memory.engine, pages * MEMORY_PAGE_SIZE);
}
