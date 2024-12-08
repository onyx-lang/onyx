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

    WIL(NULL, WI_LOCAL_SET, count_local);
    WIL(NULL, WI_LOCAL_SET, source_local);
    WIL(NULL, WI_LOCAL_SET, dest_local);

    // count is greater than 0
    WIL(NULL, WI_LOCAL_GET, count_local);
    WID(NULL, WI_I32_CONST, 0);
    WI(NULL, WI_I32_GT_S);

    WID(NULL, WI_IF_START, 0x40);
    WID(NULL, WI_LOOP_START, 0x40);

    WIL(NULL, WI_LOCAL_GET, count_local);
    WID(NULL, WI_I32_CONST, 1);
    WI(NULL, WI_I32_SUB);
    WIL(NULL, WI_LOCAL_SET, count_local);

    WIL(NULL, WI_LOCAL_GET, dest_local);
    WIL(NULL, WI_LOCAL_GET, count_local);
    WI(NULL, WI_PTR_ADD);

    WIL(NULL, WI_LOCAL_GET, source_local);
    WIL(NULL, WI_LOCAL_GET, count_local);
    WI(NULL, WI_PTR_ADD);

    WID(NULL, WI_I32_LOAD_8_U, ((WasmInstructionData) { 0, 0 }));
    WID(NULL, WI_I32_STORE_8, ((WasmInstructionData) { 0, 0 }));

    WIL(NULL, WI_LOCAL_GET, count_local);
    WID(NULL, WI_I32_CONST, 0);
    WI(NULL, WI_I32_GT_S);
    WID(NULL, WI_COND_JUMP, 0x00);

    WI(NULL, WI_LOOP_END);
    WI(NULL, WI_IF_END);

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

    WIL(NULL, WI_LOCAL_SET, count_local);
    WIL(NULL, WI_LOCAL_SET, byte_local);
    WIL(NULL, WI_LOCAL_SET, dest_local);

    // count is greater than 0
    WIL(NULL, WI_LOCAL_GET, count_local);
    WID(NULL, WI_I32_CONST, 0);
    WI(NULL, WI_I32_GT_S);

    WID(NULL, WI_IF_START, 0x40);
    WID(NULL, WI_LOOP_START, 0x40);

    WIL(NULL, WI_LOCAL_GET, count_local);
    WID(NULL, WI_I32_CONST, 1);
    WI(NULL, WI_I32_SUB);
    WIL(NULL, WI_LOCAL_SET, count_local);

    WIL(NULL, WI_LOCAL_GET, dest_local);
    WIL(NULL, WI_LOCAL_GET, count_local);
    WI(NULL, WI_PTR_ADD);

    WIL(NULL, WI_LOCAL_GET, byte_local);
    WID(NULL, WI_I32_STORE_8, ((WasmInstructionData) { 0, 0 }));

    WIL(NULL, WI_LOCAL_GET, count_local);
    WID(NULL, WI_I32_CONST, 0);
    WI(NULL, WI_I32_GT_S);
    WID(NULL, WI_COND_JUMP, 0x00);

    WI(NULL, WI_LOOP_END);
    WI(NULL, WI_IF_END);

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    *pcode = code;
}

// IMPROVE: This is a stripped down/unoptimized version of memcmp that only checks equality
// between two blocks of memory (and doesn't return -1/1). Returns 1 if the blocks of memory are equal,
// and 0 otherwise.
// NOTE: This implementation is unsafe and assumes both blocks of memory are at least size_in_bytes long.
EMIT_FUNC_NO_ARGS(intrinsic_memory_equal) {
    bh_arr(WasmInstruction) code = *pcode;

    // The stack should look like this:
    //     <size in bytes>
    //     <ptr>
    //     <ptr>

    u64 size_in_bytes = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    u64 b_addr        = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    u64 a_addr        = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);

    WIL(NULL, WI_LOCAL_SET, size_in_bytes);
    WIL(NULL, WI_LOCAL_SET, b_addr);
    WIL(NULL, WI_LOCAL_SET, a_addr);

    u64 memory_is_equal = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WID(NULL, WI_I32_CONST, 1); // set to 1 so size_in_bytes == 0 will return true
    WIL(NULL, WI_LOCAL_SET, memory_is_equal);

    // if size_in_bytes > 0
    WIL(NULL, WI_LOCAL_GET, size_in_bytes);
    WID(NULL, WI_I32_CONST, 0);
    WI (NULL, WI_I32_GT_S);
    WID(NULL, WI_IF_START, 0x40);
        u64 loop_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
        WIL(NULL, WI_I32_CONST, 0);
        WIL(NULL, WI_LOCAL_SET, loop_idx);

        WID(NULL, WI_LOOP_START, 0x40);
            // if loop_idx >= size_in_bytes, break
            WIL(NULL, WI_LOCAL_GET, loop_idx);
            WIL(NULL, WI_LOCAL_GET, size_in_bytes);
            WI (NULL, WI_I32_GE_S);
            WID(NULL, WI_COND_JUMP, 0x01);

            // a = *(a_addr + loop_idx)
            WIL(NULL, WI_LOCAL_GET, a_addr);
            WIL(NULL, WI_LOCAL_GET, loop_idx);
            WI (NULL, WI_PTR_ADD);
            WID(NULL, WI_I32_LOAD_8_U, ((WasmInstructionData) { 0, 0 }));

            // b = *(b_addr + loop_idx)
            WIL(NULL, WI_LOCAL_GET, b_addr);
            WIL(NULL, WI_LOCAL_GET, loop_idx);
            WI (NULL, WI_PTR_ADD);
            WID(NULL, WI_I32_LOAD_8_U, ((WasmInstructionData) { 0, 0 }));

            WI (NULL, WI_I32_EQ);
            WIL(NULL, WI_LOCAL_SET, memory_is_equal);

            // if bytes are not equal, break
            WIL(NULL, WI_LOCAL_GET, memory_is_equal);
            WI (NULL, WI_I32_EQZ);
            WID(NULL, WI_COND_JUMP, 0x01);

            // loop_idx += 1
            WIL(NULL, WI_LOCAL_GET, loop_idx);
            WIL(NULL, WI_I32_CONST, 1);
            WI (NULL, WI_I32_ADD);
            WIL(NULL, WI_LOCAL_SET, loop_idx);

            WID(NULL, WI_JUMP, 0x00);
        WI(NULL, WI_LOOP_END);
    WI(NULL, WI_IF_END);

    WIL(NULL, WI_LOCAL_GET, memory_is_equal);

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);   // a_addr
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);   // b_addr
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32); // size_in_bytes
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32); // memory_is_equal
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32); // loop_idx

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
            WIL(NULL, WI_LOCAL_SET, value_ptr);

            bh_arr_each(StructMember *, psmem, type->Struct.memarr) {
                StructMember* smem = *psmem;
                if (smem->initial_value == NULL || *smem->initial_value == NULL) continue;

                WIL(NULL, WI_LOCAL_GET, value_ptr);
                emit_expression(mod, &code, *smem->initial_value);
                emit_store_instruction(mod, &code, smem->type, smem->offset);
            }

            local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
            break;
        }

        default:
            //
            // If none of the above, simply zero the buffer.
            WIL(NULL, WI_I32_CONST, 0);
            WIL(NULL, WI_I32_CONST, type_size_of(type));
            emit_wasm_fill(mod, &code, NULL);
            break;
    }

    *pcode = code;
}

EMIT_FUNC(intrinsic_atomic_wait, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_WAIT32, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_WAIT64, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic wait, '%s'. Only i32 and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC_NO_ARGS(intrinsic_atomic_notify) {
    bh_arr(WasmInstruction) code = *pcode;
    WID(NULL, WI_ATOMIC_NOTIFY, ((WasmInstructionData) { 2, 0 }));
    *pcode = code;
}

EMIT_FUNC_NO_ARGS(intrinsic_atomic_fence) {
    bh_arr(WasmInstruction) code = *pcode;
    WI(NULL, WI_ATOMIC_FENCE);
    *pcode = code;
}

EMIT_FUNC(intrinsic_atomic_load, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_LOAD8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_LOAD16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_LOAD, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_LOAD, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic load, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_store, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_STORE8, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_STORE16, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_STORE, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_STORE, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic store, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_add, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_ADD8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_ADD16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_ADD, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_ADD, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic add, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_sub, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_SUB8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_SUB16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_SUB, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_SUB, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic sub, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_and, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_AND8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_AND16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_AND, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_AND, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic and, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_or, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_OR8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_OR16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_OR, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_OR, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic or, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_xor, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_XOR8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_XOR16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_XOR, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_XOR, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic xor, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_xchg, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_XCHG8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_XCHG16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_XCHG, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_XCHG, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic xchg, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC(intrinsic_atomic_cmpxchg, Type* type, OnyxToken* where) {
    if (type->kind != Type_Kind_Basic) goto bad_type;

    bh_arr(WasmInstruction) code = *pcode;

    switch (type->Basic.kind) {
        case Basic_Kind_U8:  WID(NULL, WI_ATOMIC_I32_CMPXCHG8_U, ((WasmInstructionData) { 0, 0 })); break;
        case Basic_Kind_U16: WID(NULL, WI_ATOMIC_I32_CMPXCHG16_U, ((WasmInstructionData) { 1, 0 })); break;

        case Basic_Kind_I32:
        case Basic_Kind_U32: WID(NULL, WI_ATOMIC_I32_CMPXCHG, ((WasmInstructionData) { 2, 0 })); break;

        case Basic_Kind_I64:
        case Basic_Kind_U64: WID(NULL, WI_ATOMIC_I64_CMPXCHG, ((WasmInstructionData) { 3, 0 })); break;

        default: goto bad_type;
    }

    *pcode = code;
    return;

bad_type:
    ONYX_ERROR(where->pos, Error_Critical, "Bad type for atomic cmpxchg, '%s'. Only u8, u16, u32, i32, u64, and i64 are supported.", type_get_name(mod->context, type));
}

EMIT_FUNC_NO_ARGS(initialize_data_segments_body) {
    // :ProperLinking
    if (!mod->context->options->use_multi_threading || !mod->context->options->use_post_mvp_features) return;

    bh_arr(WasmInstruction) code = *pcode;

    i32 index = 0;
    bh_arr_each(WasmDatum, datum, mod->data) {
        assert(datum->id > 0);
        if (datum->data == NULL) { index++; continue; }

        WIL(NULL, WI_PTR_CONST,   datum->offset_);
        WID(NULL, WI_PTR_CONST,   0);
        WID(NULL, WI_I32_CONST,   datum->length);
        WID(NULL, WI_MEMORY_INIT, ((WasmInstructionData) { index, 0 }));

        index += 1;
    }

    *pcode = code;
}

EMIT_FUNC_NO_ARGS(run_init_procedures) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_arr_each(AstFunction *, func, mod->context->builtins.init_procedures) {
        CodePatchInfo code_patch;
        code_patch.kind = Code_Patch_Callee;
        code_patch.func_idx = mod->current_func_idx;
        code_patch.instr = bh_arr_length(code);
        code_patch.node_related_to_patch = (AstNode *) *func;
        bh_arr_push(mod->code_patches, code_patch);

        ensure_node_has_been_submitted_for_emission(mod->context, (AstNode *) *func);

        debug_emit_instruction(mod, NULL);
        bh_arr_push(code, ((WasmInstruction){ WI_CALL, 0 }));
    }

    *pcode = code;
}
