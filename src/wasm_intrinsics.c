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
    u64 source_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    u64 dest_local   = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    
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
    WI(WI_PTR_ADD);
    
    WIL(WI_LOCAL_GET, source_local);
    WIL(WI_LOCAL_GET, count_local);
    WI(WI_PTR_ADD);
    
    WID(WI_I32_LOAD_8_U, ((WasmInstructionData) { 0, 0 }));
    WID(WI_I32_STORE_8, ((WasmInstructionData) { 0, 0 }));
    
    WIL(WI_LOCAL_GET, count_local);
    WID(WI_I32_CONST, 0);
    WI(WI_I32_GT_S);
    WID(WI_COND_JUMP, 0x00);
    
    WI(WI_LOOP_END);
    WI(WI_IF_END);
    
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    
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
    u64 dest_local  = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    
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
    WI(WI_PTR_ADD);
    
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
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    
    *pcode = code;
}

EMIT_FUNC(initialize_type, Type* type, OnyxToken* where) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (type->kind) {
        case Type_Kind_Pointer:
        case Type_Kind_Basic: {
            WasmType basic_type = onyx_type_to_wasm_type(type);
            emit_zero_value(mod, &code, basic_type);
            emit_store_instruction(mod, &code, type, 0);
            break;
        }

        case Type_Kind_Struct: {
            u64 value_ptr = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
            WIL(WI_LOCAL_SET, value_ptr);

            bh_arr_each(StructMember *, psmem, type->Struct.memarr) {
                StructMember* smem = *psmem;
                if (smem->initial_value == NULL || *smem->initial_value == NULL) continue;

                WIL(WI_LOCAL_GET, value_ptr);
                emit_expression(mod, &code, *smem->initial_value);
                emit_store_instruction(mod, &code, smem->type, smem->offset);
            }
            
            local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
            break;
        }

        default:
            onyx_report_error(where->pos,
                    "Unable to initialize type, '%s'. The reason for this is largely due to the compiler not knowing what the initial value should be.",
                    type_get_name(type)); 
            break;
    }
    
    *pcode = code;
}
