
//
// C-Pointers
//
// These are wildly unsafe and break the core principles of the security
// of WebAssembly, so there should be a way to turn them off!
//
ONYX_DEF(__cptr_make, (WASM_I32), (WASM_I64)) {
    wasm_val_init_ptr(&results->data[0], ONYX_PTR(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__cptr_read, (WASM_I64, WASM_I32, WASM_I32), ()) {
    memcpy(ONYX_PTR(params->data[1].of.i32), (void *) params->data[0].of.i64, params->data[2].of.i32);
    return NULL;
}

ONYX_DEF(__cptr_read_u8, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(*(u8 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_read_u16, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(*(u16 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_read_u32, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(*(u32 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_read_u64, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(*(u64 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_extract_str, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    unsigned int len = strlen((char *) params->data[0].of.i64);

    if (params->data[2].of.i32 != 0) {
        strncpy(ONYX_PTR(params->data[1].of.i32), (char *) params->data[0].of.i64, params->data[2].of.i32);
    }

    results->data[0] = WASM_I32_VAL(len);
    return NULL;
}

