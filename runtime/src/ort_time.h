
//
// Dates and Times
//
ONYX_DEF(__time_localtime, (WASM_I64, WASM_I32), ()) {
    u64 t = params->data[0].of.i64;
    *(struct tm *) ONYX_PTR(params->data[1].of.i32) = *localtime(&t);
    return NULL;
}

ONYX_DEF(__time_gmtime, (WASM_I64, WASM_I32), ()) {
    u64 t = params->data[0].of.i64;
    *(struct tm *) ONYX_PTR(params->data[1].of.i32) = *gmtime(&t);
    return NULL;
}

ONYX_DEF(__time_strftime, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    u32 len = strftime(ONYX_PTR(params->data[0].of.i32), params->data[1].of.i32, ONYX_PTR(params->data[2].of.i32), ONYX_PTR(params->data[3].of.i32));
    results->data[0] = WASM_I32_VAL(len); 
    return NULL;
}
