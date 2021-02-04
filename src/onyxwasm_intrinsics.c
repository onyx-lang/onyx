// This file is directly included in src/onxywasm.c
// It is here purely to decrease the amount of clutter in the main file.


// IMPROVE: This implementation assumes that the source and destination buffers do not overlap.
// The specification for memory.copy in WASM does work even if the buffers overlap.
// Also, this implementation copies byte-by-byte, which is terrible. It should copy
// quad word by quad word, and then the additional bytes if the count was not divisible by 8.
// :32BitPointers
EMIT_FUNC_NO_ARGS(intrinsic_memory_copy) {
    bh_arr(WasmInstruction) code = *pcode;
    
    // The stack should look like this:
    //     <count>
    //     <source>
    //     <dest>
    
    u64 count_local  = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    u64 source_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    u64 dest_local   = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    
    WIL(WI_LOCAL_SET, count_local);
    WIL(WI_LOCAL_SET, source_local);
    WIL(WI_LOCAL_SET, dest_local);
    
    // count is greater than 0
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 0);
    WI(WI_I32_GT_S);
    
    WID(WI_IF_START, 0x40);
    WID(WI_LOOP_START, 0x40);
    
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 1);
    WI(WI_I32_SUB);
    WIL(WI_LOCAL_SET, count_local);
    
    WIL(WI_LOCAL_GET, dest_local);
    WIL(WI_LOCAL_GET, count_local);
    WI(WI_I32_ADD);
    
    WIL(WI_LOCAL_GET, source_local);
    WIL(WI_LOCAL_GET, count_local);
    WI(WI_I32_ADD);
    
    WID(WI_I32_LOAD_8_U, ((WasmInstructionData) { 0, 0 }));
    WID(WI_I32_STORE_8, ((WasmInstructionData) { 0, 0 }));
    
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 0);
    WI(WI_I32_GT_S);
    WID(WI_COND_JUMP, 0x00);
    
    WI(WI_LOOP_END);
    WI(WI_IF_END);
    
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    
    *pcode = code;
}

EMIT_FUNC_NO_ARGS(intrinsic_memory_fill) {
    bh_arr(WasmInstruction) code = *pcode;
    
    // The stack should look like this:
    //     <count>
    //     <byte>
    //     <dest>
    
    u64 count_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    u64 byte_local  = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    u64 dest_local  = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    
    WIL(WI_LOCAL_SET, count_local);
    WIL(WI_LOCAL_SET, byte_local);
    WIL(WI_LOCAL_SET, dest_local);
    
    // count is greater than 0
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 0);
    WI(WI_I32_GT_S);
    
    WID(WI_IF_START, 0x40);
    WID(WI_LOOP_START, 0x40);
    
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 1);
    WI(WI_I32_SUB);
    WIL(WI_LOCAL_SET, count_local);
    
    WIL(WI_LOCAL_GET, dest_local);
    WIL(WI_LOCAL_GET, count_local);
    WI(WI_I32_ADD);
    
    WIL(WI_LOCAL_GET, byte_local);
    WID(WI_I32_STORE_8, ((WasmInstructionData) { 0, 0 }));
    
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 0);
    WI(WI_I32_GT_S);
    WID(WI_COND_JUMP, 0x00);
    
    WI(WI_LOOP_END);
    WI(WI_IF_END);
    
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    
    *pcode = code;
}