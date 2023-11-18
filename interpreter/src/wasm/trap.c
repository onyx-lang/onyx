
#include "ovm_wasm.h"
#include "vm.h"

wasm_trap_t *wasm_trap_new(wasm_store_t *store, const wasm_message_t *msg) {
    wasm_trap_t *trap = bh_alloc(store->engine->store->arena_allocator, sizeof(*trap));
    trap->store = store;
    trap->msg = *msg;

    //
    // Generate frames
    bh_arr(ovm_stack_frame_t) ovm_frames = store->instance->state->stack_frames;
    int frame_count = bh_arr_length(ovm_frames);

    wasm_frame_vec_new_uninitialized(&trap->frames, frame_count);

    fori (i, 0, frame_count) {
        ovm_stack_frame_t *ovm_frame = &ovm_frames[frame_count - 1 - i];
        wasm_frame_t *frame = bh_alloc(store->engine->store->arena_allocator, sizeof(*frame));
        frame->instance = store->instance;
        frame->func_idx = ovm_frame->func->id;
        frame->func_offset = 0;
        frame->module_offset = 0;

        trap->frames.data[i] = frame;
    }

    return trap;
}

void wasm_trap_message(const wasm_trap_t *trap, wasm_message_t *out) {
    *out = trap->msg;
}

wasm_frame_t *wasm_trap_origin(const wasm_trap_t *trap) {
    return trap->frames.data[0];
}

void wasm_trap_trace(const wasm_trap_t *trap, wasm_frame_vec_t *frames) {
    frames->size = trap->frames.size;
    frames->data = trap->frames.data;
}
