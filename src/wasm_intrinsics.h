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
            onyx_report_error(where->pos, Error_Critical,
                    "Unable to initialize type, '%s'. The reason for this is largely due to the compiler not knowing what the initial value should be.",
                    type_get_name(type)); 
            break;
    }

    *pcode = code;
}

EMIT_FUNC(intrinsic_atomic_wait, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_WAIT32, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_WAIT64, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic wait, '%s'. Only i32 and i64 are supported.", type_get_name(type));
}

EMIT_FUNC_NO_ARGS(intrinsic_atomic_notify) {
    bh_arr(WasmInstruction) code = *pcode;
    WID(WI_ATOMIC_NOTIFY, ((WasmInstructionData) { 2, 0 }));
    *pcode = code;
}

EMIT_FUNC_NO_ARGS(intrinsic_atomic_fence) {
    bh_arr(WasmInstruction) code = *pcode;
    WI(WI_ATOMIC_FENCE);
    *pcode = code;
}

EMIT_FUNC(intrinsic_atomic_load, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_LOAD8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_LOAD16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_LOAD, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_LOAD, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic load, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_store, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_STORE8, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_STORE16, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_STORE, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_STORE, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic store, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_add, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_ADD8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_ADD16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_ADD, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_ADD, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic add, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_sub, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_SUB8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_SUB16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_SUB, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_SUB, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic sub, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_and, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_AND8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_AND16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_AND, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_AND, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic and, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_or, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_OR8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_OR16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_OR, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_OR, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic or, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_xor, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_XOR8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_XOR16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_XOR, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_XOR, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic xor, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_xchg, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_XCHG8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_XCHG16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_XCHG, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_XCHG, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic xchg, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC(intrinsic_atomic_cmpxchg, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(WI_ATOMIC_I32_CMPXCHG8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(WI_ATOMIC_I32_CMPXCHG16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(WI_ATOMIC_I32_CMPXCHG, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(WI_ATOMIC_I64_CMPXCHG, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    onyx_report_error(where->pos, Error_Critical, "Bad type for atomic cmpxchg, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(type));
}

EMIT_FUNC_NO_ARGS(initialize_data_segments_body) {
    // :ProperLinking
    if (!context.options->use_multi_threading || !context.options->use_post_mvp_features) return;

    bh_arr(WasmInstruction) code = *pcode;

    //
    // Because this code is generated direction in the function
    // it is assumed that EVERY data entry will be entered by
    // this point. If data section entries can be entered after
    // function body generation starts, this code will have to
    // move to a link phase thing.
    i32 index = 0;
    bh_arr_each(WasmDatum, datum, mod->data) {
        assert(datum->id > 0);
        if (datum->data == NULL) { index++; continue; }

        emit_data_relocation(mod, &code, datum->id);
        WID(WI_PTR_CONST,   0);
        WID(WI_I32_CONST,   datum->length);
        WID(WI_MEMORY_INIT, ((WasmInstructionData) { index, 0 }));

        index += 1;
    }

    *pcode = code;
}

EMIT_FUNC_NO_ARGS(run_init_procedures) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_arr_each(AstFunction *, func, init_procedures) {
        i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) *func);
        bh_arr_push(code, ((WasmInstruction){ WI_CALL, func_idx }));
    }

    *pcode = code;
}
