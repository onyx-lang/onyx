
#include "ovm_wasm.h"
#include "vm.h"

wasm_func_t *wasm_ref_as_func(wasm_ref_t *ref) {
    if (ref->stored_obj == wasm_ref_stored_obj_func) {
        return ref->func;
    }

    return NULL;
}

const wasm_func_t *wasm_ref_as_func_const(const wasm_ref_t *ref) {
    if (ref->stored_obj == wasm_ref_stored_obj_func) {
        return (const wasm_func_t *) ref->func;
    }

    return NULL;
}

