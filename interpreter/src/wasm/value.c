
#include "ovm_wasm.h"

void wasm_val_delete(wasm_val_t* v) {
    // Apparently this is suppose to do nothing...
}

void wasm_val_copy(wasm_val_t* out, const wasm_val_t* in) {
    out->kind = in->kind;
    switch (out->kind) {
        case WASM_I32: out->of.i32 = in->of.i32; break;
        case WASM_I64: out->of.i64 = in->of.i64; break;
        case WASM_F32: out->of.f32 = in->of.i32; break;
        case WASM_F64: out->of.f64 = in->of.f64; break;
        case WASM_ANYREF:
        case WASM_FUNCREF:
            out->of.ref = in->of.ref;
            break;
    }
}

WASM_DECLARE_VEC_IMPL(val, )


