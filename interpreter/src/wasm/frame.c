
#include "ovm_wasm.h"


void wasm_frame_delete(wasm_frame_t *frame) {
    // Don't have to free the frame because it was allocated on the
    // arena allocator.
}

WASM_DECLARE_VEC_IMPL(frame, *)

wasm_frame_t *wasm_frame_copy(const wasm_frame_t* frame) {
    if (!frame) return NULL;
    assert(frame->instance);

    wasm_frame_t *new_frame = bh_alloc_item(frame->instance->store->engine->store->arena_allocator, wasm_frame_t);
    memcpy(new_frame, frame, sizeof(*frame));

    return new_frame;
}

wasm_instance_t* wasm_frame_instance(const wasm_frame_t* frame) {
    return frame->instance;
}

u32 wasm_frame_func_index(const wasm_frame_t *frame) {
    return frame->func_idx;
}

size_t wasm_frame_func_offset(const wasm_frame_t *frame) {
    return frame->func_offset;
}

size_t wasm_frame_module_offset(const wasm_frame_t *frame) {
    return frame->module_offset;
}

