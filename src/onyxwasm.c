#define BH_DEBUG
#include "onyxwasm.h"
#include "onyxutils.h"

OnyxWasmModule global_wasm_module;

// NOTE: Allows easier testing of types since most of the characters
// corresponding to these values are not printable
#if 1
#define WASM_TYPE_INT32   0x7F
#define WASM_TYPE_INT64   0x7E
#define WASM_TYPE_FLOAT32 0x7D
#define WASM_TYPE_FLOAT64 0x7C
#define WASM_TYPE_VAR128  0x7B
#define WASM_TYPE_VOID    0x00
#else
#define WASM_TYPE_INT32   'A'
#define WASM_TYPE_INT64   'B'
#define WASM_TYPE_FLOAT32 'C'
#define WASM_TYPE_FLOAT64 'D'
#define WASM_TYPE_VOID    'E'
#endif

static const char* wi_string(WasmInstructionType wit) {
    switch (wit) {
        case WI_UNREACHABLE: return "WI_UNREACHABLE";
        case WI_NOP: return "WI_NOP";
        case WI_BLOCK_START: return "WI_BLOCK_START";
        case WI_BLOCK_END: return "WI_BLOCK_END";
        case WI_LOOP_START: return "WI_LOOP_START";
        case WI_IF_START: return "WI_IF_START";
        case WI_ELSE: return "WI_ELSE";
        case WI_JUMP: return "WI_JUMP";
        case WI_COND_JUMP: return "WI_COND_JUMP";
        case WI_JUMP_TABLE: return "WI_JUMP_TABLE";
        case WI_RETURN: return "WI_RETURN";
        case WI_CALL: return "WI_CALL";
        case WI_CALL_INDIRECT: return "WI_CALL_INDIRECT";
        case WI_DROP: return "WI_DROP";
        case WI_SELECT: return "WI_SELECT";
        case WI_LOCAL_GET: return "WI_LOCAL_GET";
        case WI_LOCAL_SET: return "WI_LOCAL_SET";
        case WI_LOCAL_TEE: return "WI_LOCAL_TEE";
        case WI_GLOBAL_GET: return "WI_GLOBAL_GET";
        case WI_GLOBAL_SET: return "WI_GLOBAL_SET";
        case WI_I32_LOAD: return "WI_I32_LOAD";
        case WI_I64_LOAD: return "WI_I64_LOAD";
        case WI_F32_LOAD: return "WI_F32_LOAD";
        case WI_F64_LOAD: return "WI_F64_LOAD";
        case WI_I32_LOAD_8_S: return "WI_I32_LOAD_8_S";
        case WI_I32_LOAD_8_U: return "WI_I32_LOAD_8_U";
        case WI_I32_LOAD_16_S: return "WI_I32_LOAD_16_S";
        case WI_I32_LOAD_16_U: return "WI_I32_LOAD_16_U";
        case WI_I64_LOAD_8_S: return "WI_I64_LOAD_8_S";
        case WI_I64_LOAD_8_U: return "WI_I64_LOAD_8_U";
        case WI_I64_LOAD_16_S: return "WI_I64_LOAD_16_S";
        case WI_I64_LOAD_16_U: return "WI_I64_LOAD_16_U";
        case WI_I64_LOAD_32_S: return "WI_I64_LOAD_32_S";
        case WI_I64_LOAD_32_U: return "WI_I64_LOAD_32_U";
        case WI_I32_STORE: return "WI_I32_STORE";
        case WI_I64_STORE: return "WI_I64_STORE";
        case WI_F32_STORE: return "WI_F32_STORE";
        case WI_F64_STORE: return "WI_F64_STORE";
        case WI_I32_STORE_8: return "WI_I32_STORE_8";
        case WI_I32_STORE_16: return "WI_I32_STORE_16";
        case WI_I64_STORE_8: return "WI_I64_STORE_8";
        case WI_I64_STORE_16: return "WI_I64_STORE_16";
        case WI_I64_STORE_32: return "WI_I64_STORE_32";
        case WI_MEMORY_SIZE: return "WI_MEMORY_SIZE";
        case WI_MEMORY_GROW: return "WI_MEMORY_GROW";
        case WI_I32_CONST: return "WI_I32_CONST";
        case WI_I64_CONST: return "WI_I64_CONST";
        case WI_F32_CONST: return "WI_F32_CONST";
        case WI_F64_CONST: return "WI_F64_CONST";
        case WI_I32_EQZ: return "WI_I32_EQZ";
        case WI_I32_EQ: return "WI_I32_EQ";
        case WI_I32_NE: return "WI_I32_NE";
        case WI_I32_LT_S: return "WI_I32_LT_S";
        case WI_I32_LT_U: return "WI_I32_LT_U";
        case WI_I32_GT_S: return "WI_I32_GT_S";
        case WI_I32_GT_U: return "WI_I32_GT_U";
        case WI_I32_LE_S: return "WI_I32_LE_S";
        case WI_I32_LE_U: return "WI_I32_LE_U";
        case WI_I32_GE_S: return "WI_I32_GE_S";
        case WI_I32_GE_U: return "WI_I32_GE_U";
        case WI_I64_EQZ: return "WI_I64_EQZ";
        case WI_I64_EQ: return "WI_I64_EQ";
        case WI_I64_NE: return "WI_I64_NE";
        case WI_I64_LT_S: return "WI_I64_LT_S";
        case WI_I64_LT_U: return "WI_I64_LT_U";
        case WI_I64_GT_S: return "WI_I64_GT_S";
        case WI_I64_GT_U: return "WI_I64_GT_U";
        case WI_I64_LE_S: return "WI_I64_LE_S";
        case WI_I64_LE_U: return "WI_I64_LE_U";
        case WI_I64_GE_S: return "WI_I64_GE_S";
        case WI_I64_GE_U: return "WI_I64_GE_U";
        case WI_F32_EQ: return "WI_F32_EQ";
        case WI_F32_NE: return "WI_F32_NE";
        case WI_F32_LT: return "WI_F32_LT";
        case WI_F32_GT: return "WI_F32_GT";
        case WI_F32_LE: return "WI_F32_LE";
        case WI_F32_GE: return "WI_F32_GE";
        case WI_F64_EQ: return "WI_F64_EQ";
        case WI_F64_NE: return "WI_F64_NE";
        case WI_F64_LT: return "WI_F64_LT";
        case WI_F64_GT: return "WI_F64_GT";
        case WI_F64_LE: return "WI_F64_LE";
        case WI_F64_GE: return "WI_F64_GE";
        case WI_I32_CLZ: return "WI_I32_CLZ";
        case WI_I32_CTZ: return "WI_I32_CTZ";
        case WI_I32_POPCNT: return "WI_I32_POPCNT";
        case WI_I32_ADD: return "WI_I32_ADD";
        case WI_I32_SUB: return "WI_I32_SUB";
        case WI_I32_MUL: return "WI_I32_MUL";
        case WI_I32_DIV_S: return "WI_I32_DIV_S";
        case WI_I32_DIV_U: return "WI_I32_DIV_U";
        case WI_I32_REM_S: return "WI_I32_REM_S";
        case WI_I32_REM_U: return "WI_I32_REM_U";
        case WI_I32_AND: return "WI_I32_AND";
        case WI_I32_OR: return "WI_I32_OR";
        case WI_I32_XOR: return "WI_I32_XOR";
        case WI_I32_SHL: return "WI_I32_SHL";
        case WI_I32_SHR_S: return "WI_I32_SHR_S";
        case WI_I32_SHR_U: return "WI_I32_SHR_U";
        case WI_I32_ROTL: return "WI_I32_ROTL";
        case WI_I32_ROTR: return "WI_I32_ROTR";
        case WI_I64_CLZ: return "WI_I64_CLZ";
        case WI_I64_CTZ: return "WI_I64_CTZ";
        case WI_I64_POPCNT: return "WI_I64_POPCNT";
        case WI_I64_ADD: return "WI_I64_ADD";
        case WI_I64_SUB: return "WI_I64_SUB";
        case WI_I64_MUL: return "WI_I64_MUL";
        case WI_I64_DIV_S: return "WI_I64_DIV_S";
        case WI_I64_DIV_U: return "WI_I64_DIV_U";
        case WI_I64_REM_S: return "WI_I64_REM_S";
        case WI_I64_REM_U: return "WI_I64_REM_U";
        case WI_I64_AND: return "WI_I64_AND";
        case WI_I64_OR: return "WI_I64_OR";
        case WI_I64_XOR: return "WI_I64_XOR";
        case WI_I64_SHL: return "WI_I64_SHL";
        case WI_I64_SHR_S: return "WI_I64_SHR_S";
        case WI_I64_SHR_U: return "WI_I64_SHR_U";
        case WI_I64_ROTL: return "WI_I64_ROTL";
        case WI_I64_ROTR: return "WI_I64_ROTR";
        case WI_F32_ABS: return "WI_F32_ABS";
        case WI_F32_NEG: return "WI_F32_NEG";
        case WI_F32_CEIL: return "WI_F32_CEIL";
        case WI_F32_FLOOR: return "WI_F32_FLOOR";
        case WI_F32_TRUNC: return "WI_F32_TRUNC";
        case WI_F32_NEAREST: return "WI_F32_NEAREST";
        case WI_F32_SQRT: return "WI_F32_SQRT";
        case WI_F32_ADD: return "WI_F32_ADD";
        case WI_F32_SUB: return "WI_F32_SUB";
        case WI_F32_MUL: return "WI_F32_MUL";
        case WI_F32_DIV: return "WI_F32_DIV";
        case WI_F32_MIN: return "WI_F32_MIN";
        case WI_F32_MAX: return "WI_F32_MAX";
        case WI_F32_COPYSIGN: return "WI_F32_COPYSIGN";
        case WI_F64_ABS: return "WI_F64_ABS";
        case WI_F64_NEG: return "WI_F64_NEG";
        case WI_F64_CEIL: return "WI_F64_CEIL";
        case WI_F64_FLOOR: return "WI_F64_FLOOR";
        case WI_F64_TRUNC: return "WI_F64_TRUNC";
        case WI_F64_NEAREST: return "WI_F64_NEAREST";
        case WI_F64_SQRT: return "WI_F64_SQRT";
        case WI_F64_ADD: return "WI_F64_ADD";
        case WI_F64_SUB: return "WI_F64_SUB";
        case WI_F64_MUL: return "WI_F64_MUL";
        case WI_F64_DIV: return "WI_F64_DIV";
        case WI_F64_MIN: return "WI_F64_MIN";
        case WI_F64_MAX: return "WI_F64_MAX";
        case WI_F64_COPYSIGN: return "WI_F64_COPYSIGN";
        case WI_I32_FROM_I64: return "WI_I32_FROM_I64";
        case WI_I32_FROM_F32_S: return "WI_I32_FROM_F32_S";
        case WI_I32_FROM_F32_U: return "WI_I32_FROM_F32_U";
        case WI_I32_FROM_F64_S: return "WI_I32_FROM_F64_S";
        case WI_I32_FROM_F64_U: return "WI_I32_FROM_F64_U";
        case WI_I64_FROM_I32_S: return "WI_I64_FROM_I32_S";
        case WI_I64_FROM_I32_U: return "WI_I64_FROM_I32_U";
        case WI_I64_FROM_F32_S: return "WI_I64_FROM_F32_S";
        case WI_I64_FROM_F32_U: return "WI_I64_FROM_F32_U";
        case WI_I64_FROM_F64_S: return "WI_I64_FROM_F64_S";
        case WI_I64_FROM_F64_U: return "WI_I64_FROM_F64_U";
        case WI_F32_FROM_I32_S: return "WI_F32_FROM_I32_S";
        case WI_F32_FROM_I32_U: return "WI_F32_FROM_I32_U";
        case WI_F32_FROM_I64_S: return "WI_F32_FROM_I64_S";
        case WI_F32_FROM_I64_U: return "WI_F32_FROM_I64_U";
        case WI_F32_FROM_F64: return "WI_F32_FROM_F64";
        case WI_F64_FROM_I32_S: return "WI_F64_FROM_I32_S";
        case WI_F64_FROM_I32_U: return "WI_F64_FROM_I32_U";
        case WI_F64_FROM_I64_S: return "WI_F64_FROM_I64_S";
        case WI_F64_FROM_I64_U: return "WI_F64_FROM_I64_U";
        case WI_F64_FROM_F32: return "WI_F64_FROM_F32";
        case WI_I32_REINTERPRET_F32: return "WI_I32_REINTERPRET_F32";
        case WI_I64_REINTERPRET_F64: return "WI_I64_REINTERPRET_F64";
        case WI_F32_REINTERPRET_I32: return "WI_F32_REINTERPRET_I32";
        case WI_F64_REINTERPRET_I64: return "WI_F64_REINTERPRET_I64";
        case WI_I32_EXTEND_8_S: return "WI_I32_EXTEND_8_S";
        case WI_I32_EXTEND_16_S: return "WI_I32_EXTEND_16_S";
        case WI_I64_EXTEND_8_S: return "WI_I64_EXTEND_8_S";
        case WI_I64_EXTEND_16_S: return "WI_I64_EXTEND_16_S";
        case WI_I64_EXTEND_32_S: return "WI_I64_EXTEND_32_S";
    }
}

static WasmType onyx_type_to_wasm_type(Type* type) {
    if (type->kind == Type_Kind_Struct) {
        return WASM_TYPE_VOID;
    }

    if (type->kind == Type_Kind_Slice) {
        return WASM_TYPE_VOID;
    }

    if (type->kind == Type_Kind_Enum) {
        return onyx_type_to_wasm_type(type->Enum.backing);
    }

    if (type->kind == Type_Kind_Pointer) {
        return WASM_TYPE_INT32;
    }

    if (type->kind == Type_Kind_Array) {
        return WASM_TYPE_INT32;
    }

    if (type->kind == Type_Kind_Function) {
        return WASM_TYPE_INT32;
    }

    if (type->kind == Type_Kind_Basic) {
        TypeBasic* basic = &type->Basic;
        if (basic->flags & Basic_Flag_Boolean) return WASM_TYPE_INT32;
        if (basic->flags & Basic_Flag_Integer) {
            if (basic->size <= 4) return WASM_TYPE_INT32;
            if (basic->size == 8) return WASM_TYPE_INT64;
        }
        if (basic->flags & Basic_Flag_Pointer) return WASM_TYPE_INT32;
        if (basic->flags & Basic_Flag_Float) {
            if (basic->size <= 4) return WASM_TYPE_FLOAT32;
            if (basic->size == 8) return WASM_TYPE_FLOAT64;
        }
        if (basic->flags & Basic_Flag_SIMD) return WASM_TYPE_VAR128;
        if (basic->size == 0) return WASM_TYPE_VOID;
    }

    return WASM_TYPE_VOID;
}

static i32 generate_type_idx(OnyxWasmModule* mod, Type* ft);
static i32 get_element_idx(OnyxWasmModule* mod, AstFunction* func);

#define LOCAL_I32  0x000000000
#define LOCAL_I64  0x100000000
#define LOCAL_F32  0x300000000
#define LOCAL_F64  0x700000000
#define LOCAL_V128 0xf00000000

static b32 local_is_wasm_local(AstLocal* local) {
    if (local->flags & Ast_Flag_Address_Taken) return 0;
    if (local->type->kind == Type_Kind_Basic) return 1;
    if (local->type->kind == Type_Kind_Enum && local->type->Enum.backing->kind == Type_Kind_Basic) return 1;
    if (local->type->kind == Type_Kind_Pointer) return 1;
    return 0;
}

static u64 local_raw_allocate(LocalAllocator* la, WasmType wt) {
    i32 idx = 0;
    if (wt == WASM_TYPE_INT32)   idx = 0;
    if (wt == WASM_TYPE_INT64)   idx = 1;
    if (wt == WASM_TYPE_FLOAT32) idx = 2;
    if (wt == WASM_TYPE_FLOAT64) idx = 3;
    if (wt == WASM_TYPE_VAR128)  idx = 4;

    u64 flag_bits = LOCAL_IS_WASM;
    if (wt == WASM_TYPE_INT32)   flag_bits |= LOCAL_I32;
    if (wt == WASM_TYPE_INT64)   flag_bits |= LOCAL_I64;
    if (wt == WASM_TYPE_FLOAT32) flag_bits |= LOCAL_F32;
    if (wt == WASM_TYPE_FLOAT64) flag_bits |= LOCAL_F64;
    if (wt == WASM_TYPE_VAR128)  flag_bits |= LOCAL_V128;

    if (la->freed[idx] > 0) {
        la->freed[idx]--;
        return flag_bits | ((u64) (la->allocated[idx] - la->freed[idx] - 1 + la->param_count));

    } else {
        la->allocated[idx]++;
        return flag_bits | ((u64) (la->allocated[idx] - 1 + la->param_count));
    }
}

static void local_raw_free(LocalAllocator* la, WasmType wt) {
    i32 idx = 0;

    if (wt == WASM_TYPE_INT32)   idx = 0;
    if (wt == WASM_TYPE_INT64)   idx = 1;
    if (wt == WASM_TYPE_FLOAT32) idx = 2;
    if (wt == WASM_TYPE_FLOAT64) idx = 3;
    if (wt == WASM_TYPE_VAR128)  idx = 4;

    assert(la->allocated[idx] > 0 && la->freed[idx] < la->allocated[idx]);

    la->freed[idx]++;
}

static u64 local_allocate(LocalAllocator* la, AstLocal* local) {
    if (local_is_wasm_local(local)) {
        WasmType wt = onyx_type_to_wasm_type(local->type);
        return local_raw_allocate(la, wt);

    } else {
        u32 size = type_size_of(local->type);
        u32 alignment = type_alignment_of(local->type);

        if (la->curr_stack % alignment != 0)
            la->curr_stack += alignment - (la->curr_stack % alignment);

        if (la->max_stack < la->curr_stack)
            la->max_stack = la->curr_stack;

        if (size % alignment != 0)
            size += alignment - (size % alignment);

        if (la->max_stack - la->curr_stack >= size) {
            la->curr_stack += size;
        } else {
            la->max_stack += size - (la->max_stack - la->curr_stack);
            la->curr_stack = la->max_stack;
        }

        return la->curr_stack - size;
    }
}

static void local_free(LocalAllocator* la, AstLocal* local) {
    if (local_is_wasm_local(local)) {
        WasmType wt = onyx_type_to_wasm_type(local->type);
        local_raw_free(la, wt);

    } else {
        u32 size = type_size_of(local->type);
        u32 alignment = type_alignment_of(local->type);
        if (size % alignment != 0)
            size += alignment - (size % alignment);

        la->curr_stack -= size;
    }
}

static u64 local_lookup_idx(LocalAllocator* la, u64 value) {
    assert(value & LOCAL_IS_WASM);

    u32 idx = value & 0xFFFFFFFF;
    if (value & 0x100000000) idx += la->allocated[0];
    if (value & 0x200000000) idx += la->allocated[1];
    if (value & 0x400000000) idx += la->allocated[2];
    if (value & 0x800000000) idx += la->allocated[3];

    return (u64) idx;
}

#define WI(instr) bh_arr_push(code, ((WasmInstruction){ instr, 0x00 }))
#define WID(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, data }))
#define WIL(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, { .l = data } }))
#define WIP(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, { .p = data } }))
#define EMIT_FUNC(kind, ...) static void emit_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, __VA_ARGS__)

EMIT_FUNC(function_body,                 AstFunction* fd);
EMIT_FUNC(block,                         AstBlock* block, b32 generate_block_headers);
EMIT_FUNC(statement,                     AstNode* stmt);
EMIT_FUNC(assignment,                    AstBinaryOp* assign);
EMIT_FUNC(store_instruction,             Type* type, u32 offset);
EMIT_FUNC(load_instruction,              Type* type, u32 offset);
EMIT_FUNC(if,                            AstIfWhile* if_node);
EMIT_FUNC(while,                         AstIfWhile* while_node);
EMIT_FUNC(for,                           AstFor* for_node);
EMIT_FUNC(switch,                        AstSwitch* switch_node);
EMIT_FUNC(defer,                         AstDefer* defer);
EMIT_FUNC(deferred_stmts,                AstNode* node);
EMIT_FUNC(binop,                         AstBinaryOp* binop);
EMIT_FUNC(unaryop,                       AstUnaryOp* unop);
EMIT_FUNC(call,                          AstCall* call);
EMIT_FUNC(intrinsic_call,                AstIntrinsicCall* call);
EMIT_FUNC(array_access_location,         AstArrayAccess* aa, u64* offset_return);
EMIT_FUNC(field_access_location,         AstFieldAccess* field, u64* offset_return);
EMIT_FUNC(local_location,                AstLocal* local, u64* offset_return);
EMIT_FUNC(memory_reservation_location,   AstMemRes* memres);
EMIT_FUNC(location,                      AstTyped* expr);
EMIT_FUNC(struct_load,                   Type* type, u64 offset);
EMIT_FUNC(struct_lval,                   AstTyped* lval);
EMIT_FUNC(struct_store,                  Type* type, u64 offset);
EMIT_FUNC(struct_literal,                AstStructLiteral* sl);
EMIT_FUNC(expression,                    AstTyped* expr);
EMIT_FUNC(cast,                          AstUnaryOp* cast);
EMIT_FUNC(return,                        AstReturn* ret);
EMIT_FUNC(stack_enter,                   u64 stacksize);
EMIT_FUNC(stack_leave,                   u32 unused);

EMIT_FUNC(function_body, AstFunction* fd) {
    if (fd->body == NULL) return;

    emit_block(mod, pcode, fd->body, 0);
}

EMIT_FUNC(block, AstBlock* block, b32 generate_block_headers) {
    bh_arr(WasmInstruction) code = *pcode;

    if (generate_block_headers) {
        bh_arr_push(mod->structured_jump_target, 1);
        WID(WI_BLOCK_START, 0x40);
    }

    bh_arr_each(AstLocal *, local, block->locals)
        bh_imap_put(&mod->local_map, (u64) *local, local_allocate(mod->local_alloc, *local));

    forll (AstNode, stmt, block->body, next) {
        emit_statement(mod, &code, stmt);
    }

    emit_deferred_stmts(mod, &code, (AstNode *) block);

    bh_arr_each(AstLocal *, local, block->locals)
        local_free(mod->local_alloc, *local);

    if (generate_block_headers) {
        WI(WI_BLOCK_END);
        bh_arr_pop(mod->structured_jump_target);
    }

    *pcode = code;
}

EMIT_FUNC(structured_jump, AstJump* jump) {
    bh_arr(WasmInstruction) code = *pcode;

    static const u8 wants[Jump_Type_Count] = { 1, 2, 3 };

    i32 labelidx = 0;
    u8 wanted = wants[jump->jump];
    b32 success = 0;

    u32 jump_count = jump->count;

    i32 len = bh_arr_length(mod->structured_jump_target) - 1;
    for (u8* t = &bh_arr_last(mod->structured_jump_target); len >= 0; len--, t--) {
        if (*t == wanted) jump_count--;
        if (jump_count == 0) {
            success = 1;
            break;
        }

        labelidx++;
    }

    if (success) {
        // NOTE: If the previous instruction was a non conditional jump,
        // don't emit another jump since it will never be reached.
        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, labelidx);
    } else {
        onyx_report_error(jump->token->pos, "Invalid structured jump.");
    }

    *pcode = code;
}

EMIT_FUNC(statement, AstNode* stmt) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (stmt->kind) {
        case Ast_Kind_Return:     emit_return(mod, &code, (AstReturn *) stmt); break;
        case Ast_Kind_If:         emit_if(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_While:      emit_while(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_For:        emit_for(mod, &code, (AstFor *) stmt); break;
        case Ast_Kind_Switch:     emit_switch(mod, &code, (AstSwitch *) stmt); break;
        case Ast_Kind_Jump:       emit_structured_jump(mod, &code, (AstJump *) stmt); break;
        case Ast_Kind_Block:      emit_block(mod, &code, (AstBlock *) stmt, 1); break;
        case Ast_Kind_Defer:      emit_defer(mod, &code, (AstDefer *) stmt); break;
        default:                  emit_expression(mod, &code, (AstTyped *) stmt); break;
    }

    *pcode = code;
}

EMIT_FUNC(assignment, AstBinaryOp* assign) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type_is_structlike_strict(assign->right->type)) {
        emit_expression(mod, &code, assign->right);
        emit_struct_lval(mod, &code, assign->left);

        *pcode = code;
        return;
    }

    AstTyped* lval = assign->left;

    if (lval->kind == Ast_Kind_Local) {
        if (bh_imap_get(&mod->local_map, (u64) lval) & LOCAL_IS_WASM) {
            u64 localidx = bh_imap_get(&mod->local_map, (u64) lval);
            emit_expression(mod, &code, assign->right);
            WIL(WI_LOCAL_SET, localidx);

        } else {
            u64 offset = 0;
            emit_local_location(mod, &code, (AstLocal *) lval, &offset);
            emit_expression(mod, &code, assign->right);
            emit_store_instruction(mod, &code, lval->type, offset);
        }

    } else if (lval->kind == Ast_Kind_Global) {
        i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) lval);

        emit_expression(mod, &code, assign->right);
        WID(WI_GLOBAL_SET, globalidx);

    } else if (lval->kind == Ast_Kind_Dereference) {
        AstDereference* deref = (AstDereference *) lval;
        emit_expression(mod, &code, deref->expr);
        emit_expression(mod, &code, assign->right);

        emit_store_instruction(mod, &code, deref->type, 0);

    } else if (lval->kind == Ast_Kind_Array_Access) {
        AstArrayAccess* aa = (AstArrayAccess *) lval;

        u64 offset = 0;
        emit_array_access_location(mod, &code, aa, &offset);
        emit_expression(mod, &code, assign->right);

        emit_store_instruction(mod, &code, aa->type, offset);

    } else if (lval->kind == Ast_Kind_Field_Access) {
        AstFieldAccess* field = (AstFieldAccess *) lval;

        u64 offset = 0;
        emit_field_access_location(mod, &code, field, &offset);
        emit_expression(mod, &code, assign->right);

        emit_store_instruction(mod, &code, field->type, offset);

    } else if (lval->kind == Ast_Kind_Memres) {
        AstMemRes* memres = (AstMemRes *) lval;

        emit_memory_reservation_location(mod, &code, memres);
        emit_expression(mod, &code, assign->right);
        emit_store_instruction(mod, &code, memres->type, 0);

    } else {
        assert(("Invalid lval", 0));
    }

    *pcode = code;
}

EMIT_FUNC(store_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type_is_structlike_strict(type)) {
        emit_struct_store(mod, pcode, type, offset);
        return;
    }

    if (type->kind == Type_Kind_Enum) {
        type = type->Enum.backing;
    }

    if (type->kind == Type_Kind_Function) {
        type = &basic_types[Basic_Kind_U32];
    }

    u32 alignment = type_get_alignment_log2(type);

    i32 store_size  = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer;
    i32 is_pointer  = is_basic && (type->Basic.flags & Basic_Flag_Pointer);
    i32 is_integer  = is_basic && ((type->Basic.flags & Basic_Flag_Integer) || (type->Basic.flags & Basic_Flag_Boolean));
    i32 is_float    = is_basic && (type->Basic.flags & Basic_Flag_Float);
    i32 is_simd     = is_basic && (type->Basic.flags & Basic_Flag_SIMD);

    if (is_pointer) {
        WID(WI_I32_STORE, ((WasmInstructionData) { alignment, offset }));
    } else if (is_integer) {
        if      (store_size == 1)   WID(WI_I32_STORE_8,  ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 2)   WID(WI_I32_STORE_16, ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 4)   WID(WI_I32_STORE,    ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 8)   WID(WI_I64_STORE,    ((WasmInstructionData) { alignment, offset }));
    } else if (is_float) {
        if      (store_size == 4)   WID(WI_F32_STORE, ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 8)   WID(WI_F64_STORE, ((WasmInstructionData) { alignment, offset }));
    } else if (is_simd) {
        WID(WI_V128_STORE, ((WasmInstructionData) { alignment, offset }));
    } else {
        onyx_report_error((OnyxFilePos) { 0 },
            "Failed to generate store instruction for type '%s'.",
            type_get_name(type));
    }

    *pcode = code;
}

EMIT_FUNC(load_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type_is_structlike_strict(type)) {
        emit_struct_load(mod, pcode, type, offset);
        return;
    }

    if (type->kind == Type_Kind_Array) {
        if (offset != 0) {
            WID(WI_I32_CONST, offset);
            WI(WI_I32_ADD);
        }

        *pcode = code;
        return;
    }

    if (type->kind == Type_Kind_Enum) {
        type = type->Enum.backing;
    }

    if (type->kind == Type_Kind_Function) {
        type = &basic_types[Basic_Kind_U32];
    }

    i32 load_size   = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer;
    i32 is_pointer  = is_basic && (type->Basic.flags & Basic_Flag_Pointer);
    i32 is_integer  = is_basic && ((type->Basic.flags & Basic_Flag_Integer) || (type->Basic.flags & Basic_Flag_Boolean));
    i32 is_float    = is_basic && (type->Basic.flags & Basic_Flag_Float);
    i32 is_unsigned = is_basic && (type->Basic.flags & Basic_Flag_Unsigned);
    i32 is_simd     = is_basic && (type->Basic.flags & Basic_Flag_SIMD);

    WasmInstructionType instr = WI_NOP;
    i32 alignment = type_get_alignment_log2(type);

    if (is_pointer) {
        instr = WI_I32_LOAD;
    }
    else if (is_integer) {
        if      (load_size == 1) instr = WI_I32_LOAD_8_S;
        else if (load_size == 2) instr = WI_I32_LOAD_16_S;
        else if (load_size == 4) instr = WI_I32_LOAD;
        else if (load_size == 8) instr = WI_I64_LOAD;

        if (load_size < 4 && is_unsigned) instr += 1;
    }
    else if (is_float) {
        if      (load_size == 4) instr = WI_F32_LOAD;
        else if (load_size == 8) instr = WI_F64_LOAD;
    }
    else if (is_simd) {
        instr = WI_V128_LOAD;
    }

    WID(instr, ((WasmInstructionData) { alignment, offset }));

    if (instr == WI_NOP) {
        onyx_report_error((OnyxFilePos) { 0 },
            "Failed to generate load instruction for type '%s'.",
            type_get_name(type));
    }

    *pcode = code;
}

EMIT_FUNC(if, AstIfWhile* if_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (if_node->assignment != NULL) {
        bh_imap_put(&mod->local_map, (u64) if_node->local, local_allocate(mod->local_alloc, if_node->local));

        emit_assignment(mod, &code, if_node->assignment);
    }

    emit_expression(mod, &code, if_node->cond);
    WID(WI_IF_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 0);
    if (if_node->true_stmt) emit_block(mod, &code, if_node->true_stmt, 0);

    if (if_node->false_stmt) {
        WI(WI_ELSE);

        if (if_node->false_stmt->kind == Ast_Kind_If) {
            emit_if(mod, &code, (AstIfWhile *) if_node->false_stmt);
        } else {
            emit_block(mod, &code, if_node->false_stmt, 0);
        }
    }

    bh_arr_pop(mod->structured_jump_target);

    if (if_node->assignment != NULL) {
        local_free(mod->local_alloc, if_node->local);
    }

    WI(WI_IF_END);

    *pcode = code;
}

EMIT_FUNC(while, AstIfWhile* while_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (while_node->assignment != NULL) {
        bh_imap_put(&mod->local_map, (u64) while_node->local, local_allocate(mod->local_alloc, while_node->local));

        emit_assignment(mod, &code, while_node->assignment);
    }

    if (while_node->false_stmt == NULL) {
        WID(WI_BLOCK_START, 0x40);
        WID(WI_LOOP_START, 0x40);

        emit_expression(mod, &code, while_node->cond);
        WI(WI_I32_EQZ);
        WID(WI_COND_JUMP, 0x01);

        bh_arr_push(mod->structured_jump_target, 1);
        bh_arr_push(mod->structured_jump_target, 2);

        emit_block(mod, &code, while_node->true_stmt, 0);

        bh_arr_pop(mod->structured_jump_target);
        bh_arr_pop(mod->structured_jump_target);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, 0x00);

        WI(WI_LOOP_END);
        WI(WI_BLOCK_END);

    } else {
        emit_expression(mod, &code, while_node->cond);

        bh_arr_push(mod->structured_jump_target, 1);
        bh_arr_push(mod->structured_jump_target, 2);
        WID(WI_IF_START, 0x40);

        WID(WI_LOOP_START, 0x40);
        emit_block(mod, &code, while_node->true_stmt, 0);
        emit_expression(mod, &code, while_node->cond);
        WID(WI_COND_JUMP, 0x00);
        WI(WI_LOOP_END);

        WI(WI_ELSE);
        emit_block(mod, &code, while_node->false_stmt, 0);
        WID(WI_IF_END, 0x40);

        bh_arr_pop(mod->structured_jump_target);
        bh_arr_pop(mod->structured_jump_target);
    }

    if (while_node->assignment != NULL)
        local_free(mod->local_alloc, while_node->local);

    *pcode = code;
}

EMIT_FUNC(for_range, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    // NOTE: There are some aspects of the code below that rely on the
    // low, high, and step members to be i32's. This restriction can be lifted,
    // but it is important to change the code here.
    //                                              -brendanfh   2020/09/04

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    StructMember smem;
    type_lookup_member(builtin_range_type_type, "low", &smem);
    u64 low_local  = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(smem.type));
    type_lookup_member(builtin_range_type_type, "high", &smem);
    u64 high_local = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(smem.type));
    type_lookup_member(builtin_range_type_type, "step", &smem);
    u64 step_local = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(smem.type));

    WIL(WI_LOCAL_SET, step_local);
    WIL(WI_LOCAL_SET, high_local);

    if (it_is_local) {
        WIL(WI_LOCAL_TEE, low_local);
        WIL(WI_LOCAL_SET, iter_local);

    } else {
        WIL(WI_LOCAL_SET, low_local);
        emit_local_location(mod, &code, var, &offset);
        WIL(WI_LOCAL_GET, low_local);
        emit_store_instruction(mod, &code, var->type, offset);
    }

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);
    WID(WI_BLOCK_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 0);
    bh_arr_push(mod->structured_jump_target, 2);

    if (it_is_local) {
        WIL(WI_LOCAL_GET, iter_local);
    } else {
        offset = 0;
        emit_local_location(mod, &code, var, &offset);
        emit_load_instruction(mod, &code, var->type, offset);
    }
    WIL(WI_LOCAL_GET, high_local);
    WI(WI_I32_GE_S);
    WID(WI_COND_JUMP, 0x02);

    emit_block(mod, &code, for_node->stmt, 0);

    bh_arr_pop(mod->structured_jump_target);
    WI(WI_BLOCK_END);

    if (it_is_local) {
        WIL(WI_LOCAL_GET, iter_local);
        WIL(WI_LOCAL_GET, step_local);
        WI(WI_I32_ADD);
        WIL(WI_LOCAL_SET, iter_local);
    } else {
        offset = 0;
        emit_local_location(mod, &code, var, &offset);
        offset = 0;
        emit_local_location(mod, &code, var, &offset);
        emit_load_instruction(mod, &code, var->type, offset);
        WIL(WI_LOCAL_GET, step_local);
        WI(WI_I32_ADD);
        emit_store_instruction(mod, &code, var->type, offset);
    }

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    type_lookup_member(builtin_range_type_type, "low", &smem);
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(smem.type));
    type_lookup_member(builtin_range_type_type, "high", &smem);
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(smem.type));
    type_lookup_member(builtin_range_type_type, "step", &smem);
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(smem.type));

    *pcode = code;
}

EMIT_FUNC(for_array, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    // NOTE: This implementation is only for loops by value, not by pointer.

    // At this point the stack will look like:
    //      data

    u64 end_ptr_local, ptr_local;
    end_ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);

    if (for_node->by_pointer) {
        ptr_local = iter_local;
    } else {
        ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    }

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    u64 elem_size;
    if (for_node->by_pointer) elem_size = type_size_of(var->type->Pointer.elem);
    else                      elem_size = type_size_of(var->type);

    WIL(WI_LOCAL_TEE, ptr_local);
    WIL(WI_I32_CONST, for_node->iter->type->Array.count * elem_size);
    WI(WI_I32_ADD);
    WIL(WI_LOCAL_SET, end_ptr_local);

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);
    WID(WI_BLOCK_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 0);
    bh_arr_push(mod->structured_jump_target, 2);

    WIL(WI_LOCAL_GET, ptr_local);
    WIL(WI_LOCAL_GET, end_ptr_local);
    WI(WI_I32_GE_U);
    WID(WI_COND_JUMP, 0x02);

    if (!for_node->by_pointer) {
        // NOTE: Storing structs requires that the location to store it is,
        // the top most thing on the stack. Everything requires it to be
        // 'under' the other element being stored.  -brendanfh 2020/09/04
        if (!it_is_local && !type_is_structlike(var->type)) {
            emit_local_location(mod, &code, var, &offset);
        }

        WIL(WI_LOCAL_GET, ptr_local);
        emit_load_instruction(mod, &code, var->type, 0);
        if (it_is_local) {
            WIL(WI_LOCAL_SET, iter_local);
        } else {
            if (!type_is_structlike(var->type)) {
                emit_store_instruction(mod, &code, var->type, offset);
            } else {
                emit_local_location(mod, &code, var, &offset);
                emit_store_instruction(mod, &code, var->type, offset);
            }
        }
    }

    emit_block(mod, &code, for_node->stmt, 0);

    bh_arr_pop(mod->structured_jump_target);
    WI(WI_BLOCK_END);

    WIL(WI_LOCAL_GET, ptr_local);
    if (elem_size != 0) {
        WIL(WI_I32_CONST, elem_size);
        WI(WI_I32_ADD);
    }
    WIL(WI_LOCAL_SET, ptr_local);

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    if (!for_node->by_pointer) local_raw_free(mod->local_alloc, WASM_TYPE_INT32);

    *pcode = code;
}

EMIT_FUNC(for_slice, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    // NOTE: This implementation is only for loops by value, not by pointer.

    // At this point the stack will look like:
    //      data
    //      count

    u64 end_ptr_local, ptr_local;
    end_ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);

    if (for_node->by_pointer) {
        ptr_local = iter_local;
    } else {
        ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    }

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    u64 elem_size;
    if (for_node->by_pointer) elem_size = type_size_of(var->type->Pointer.elem);
    else                      elem_size = type_size_of(var->type);

    WIL(WI_LOCAL_SET, end_ptr_local);
    WIL(WI_LOCAL_TEE, ptr_local);
    WIL(WI_LOCAL_GET, end_ptr_local);
    if (elem_size != 1) {
        WID(WI_I32_CONST, elem_size);
        WI(WI_I32_MUL);
    }
    WI(WI_I32_ADD);
    WIL(WI_LOCAL_SET, end_ptr_local);

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);
    WID(WI_BLOCK_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 0);
    bh_arr_push(mod->structured_jump_target, 2);

    WIL(WI_LOCAL_GET, ptr_local);
    WIL(WI_LOCAL_GET, end_ptr_local);
    WI(WI_I32_GE_U);
    WID(WI_COND_JUMP, 0x02);

    if (!for_node->by_pointer) {
        // NOTE: Storing structs requires that the location to store it is,
        // the top most thing on the stack. Everything requires it to be
        // 'under' the other element being stored.  -brendanfh 2020/09/04
        if (!it_is_local && !type_is_structlike(var->type)) {
            emit_local_location(mod, &code, var, &offset);
        }

        WIL(WI_LOCAL_GET, ptr_local);
        emit_load_instruction(mod, &code, var->type, 0);
        if (it_is_local) {
            WIL(WI_LOCAL_SET, iter_local);
        } else {
            if (!type_is_structlike(var->type)) {
                emit_store_instruction(mod, &code, var->type, offset);
            } else {
                emit_local_location(mod, &code, var, &offset);
                emit_store_instruction(mod, &code, var->type, offset);
            }
        }
    }

    emit_block(mod, &code, for_node->stmt, 0);

    bh_arr_pop(mod->structured_jump_target);
    WI(WI_BLOCK_END);

    WIL(WI_LOCAL_GET, ptr_local);
    if (elem_size != 0) {
        WIL(WI_I32_CONST, elem_size);
        WI(WI_I32_ADD);
    }
    WIL(WI_LOCAL_SET, ptr_local);

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    if (!for_node->by_pointer) local_raw_free(mod->local_alloc, WASM_TYPE_INT32);

    *pcode = code;
}

EMIT_FUNC(for, AstFor* for_node) {
    bh_arr(WasmInstruction) code = *pcode;

    AstLocal* var = for_node->var;
    u64 iter_local = local_allocate(mod->local_alloc, var);
    bh_imap_put(&mod->local_map, (u64) var, iter_local);

    emit_expression(mod, &code, for_node->iter);

    if (for_node->loop_type == For_Loop_Range) {
        emit_for_range(mod, &code, for_node, iter_local);
    } else if (for_node->loop_type == For_Loop_Array) {
        emit_for_array(mod, &code, for_node, iter_local);
    } else if (for_node->loop_type == For_Loop_Slice) {
        emit_for_slice(mod, &code, for_node, iter_local);
    } else if (for_node->loop_type == For_Loop_DynArr) {
        // NOTE: A dynamic array is just a slice with an extra capacity field on the end.
        // Just dropping the capacity field will mean we can just use the slice implementation.
        //                                                  - brendanfh   2020/09/04
        WI(WI_DROP);
        emit_for_slice(mod, &code, for_node, iter_local);
    } else {
        onyx_report_error(for_node->token->pos, "Invalid for loop type. You should probably not be seeing this...");
    }

    local_free(mod->local_alloc, var);

    *pcode = code;
}

EMIT_FUNC(switch, AstSwitch* switch_node) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_imap block_map;
    bh_imap_init(&block_map, global_heap_allocator, bh_arr_length(switch_node->cases));

    if (switch_node->assignment != NULL) {
        bh_imap_put(&mod->local_map, (u64) switch_node->local, local_allocate(mod->local_alloc, switch_node->local));

        emit_assignment(mod, &code, switch_node->assignment);
    }

    WID(WI_BLOCK_START, 0x40);
    bh_arr_push(mod->structured_jump_target, 1);

    u64 block_num = 0;
    bh_arr_each(AstSwitchCase, sc, switch_node->cases) {
        if (bh_imap_has(&block_map, (u64) sc->block)) continue;

        WID(WI_BLOCK_START, 0x40);
        bh_arr_push(mod->structured_jump_target, 3);

        bh_imap_put(&block_map, (u64) sc->block, block_num);
        block_num++;
    }

    u64 count = switch_node->max_case + 1 - switch_node->min_case;
    BranchTable* bt = bh_alloc(mod->extended_instr_alloc, sizeof(BranchTable) + sizeof(u32) * count);
    bt->count = count;
    bt->default_case = block_num;
    fori (i, 0, bt->count) bt->cases[i] = bt->default_case;

    bh_arr_each(bh__imap_entry, sc, switch_node->case_map.entries) {
        bt->cases[sc->key - switch_node->min_case] = bh_imap_get(&block_map, (u64) sc->value);
    }

    WID(WI_BLOCK_START, 0x40);
    emit_expression(mod, &code, switch_node->expr);
    if (switch_node->min_case != 0) {
        WID(WI_I32_CONST, switch_node->min_case);
        WI(WI_I32_SUB);
    }
    WIL(WI_JUMP_TABLE, (u64) bt);
    WI(WI_BLOCK_END);

    bh_arr_each(AstSwitchCase, sc, switch_node->cases) {
        if (bh_imap_get(&block_map, (u64) sc->block) == 0xdeadbeef) continue;

        u64 bn = bh_imap_get(&block_map, (u64) sc->block);

        emit_block(mod, &code, sc->block, 0);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, block_num - bn);

        WI(WI_BLOCK_END);
        bh_arr_pop(mod->structured_jump_target);

        bh_imap_put(&block_map, (u64) sc->block, 0xdeadbeef);
    }

    if (switch_node->default_case != NULL) {
        emit_block(mod, &code, switch_node->default_case, 0);
    }

    WI(WI_BLOCK_END);
    bh_arr_pop(mod->structured_jump_target);

    if (switch_node->assignment != NULL)
        local_free(mod->local_alloc, switch_node->local);

    bh_imap_free(&block_map);
    *pcode = code;
}

EMIT_FUNC(defer, AstDefer* defer) {
    bh_arr_push(mod->deferred_stmts, ((DeferredStmt) {
        .depth = bh_arr_length(mod->structured_jump_target),
        .stmt = defer->stmt,
    }));
}

EMIT_FUNC(deferred_stmts, AstNode* node) {
    if (bh_arr_length(mod->deferred_stmts) == 0) return;

    bh_arr(WasmInstruction) code = *pcode;

    u64 depth = bh_arr_length(mod->structured_jump_target);

    while (bh_arr_last(mod->deferred_stmts).depth == depth) {
        emit_statement(mod, &code, bh_arr_last(mod->deferred_stmts).stmt);
        bh_arr_pop(mod->deferred_stmts);
    }

    *pcode = code;
}

// NOTE: These need to be in the same order as
// the OnyxBinaryOp enum
static const WasmInstructionType binop_map[][4] = {
    //           I32           I64           F32         F64
    /* ADD */  { WI_I32_ADD,   WI_I64_ADD,   WI_F32_ADD, WI_F64_ADD },
    /* SUB */  { WI_I32_SUB,   WI_I64_SUB,   WI_F32_SUB, WI_F64_SUB },
    /* MUL */  { WI_I32_MUL,   WI_I64_MUL,   WI_F32_MUL, WI_F64_MUL },
    /* DIV */  { WI_I32_DIV_S, WI_I64_DIV_S, WI_F32_DIV, WI_F64_DIV },
    /* REM */  { WI_I32_REM_S, WI_I64_REM_S, WI_NOP,     WI_NOP     },

    /* EQ  */  { WI_I32_EQ,    WI_I64_EQ,    WI_F32_EQ,  WI_F64_EQ },
    /* NEQ */  { WI_I32_NE,    WI_I64_NE,    WI_F32_NE , WI_F64_NE },
    /* LT  */  { WI_I32_LT_S,  WI_I64_LT_S,  WI_F32_LT,  WI_F64_LT },
    /* LTE */  { WI_I32_LE_S,  WI_I64_LE_S,  WI_F32_LE,  WI_F64_LE },
    /* GT  */  { WI_I32_GT_S,  WI_I64_GT_S,  WI_F32_GT,  WI_F64_GT },
    /* GTE */  { WI_I32_GE_S,  WI_I64_GE_S,  WI_F32_GE,  WI_F64_GE },

    /* AND */  { WI_I32_AND,   WI_I64_AND,   WI_NOP,     WI_NOP },
    /* OR  */  { WI_I32_OR,    WI_I64_OR,    WI_NOP,     WI_NOP },
    /* XOR */  { WI_I32_XOR,   WI_I64_XOR,   WI_NOP,     WI_NOP },
    /* SHL */  { WI_I32_SHL,   WI_I64_SHL,   WI_NOP,     WI_NOP },
    /* SHR */  { WI_I32_SHR_U, WI_I64_SHR_U, WI_NOP,     WI_NOP },
    /* SAR */  { WI_I32_SHR_S, WI_I64_SHR_S, WI_NOP,     WI_NOP },

    /* BAND */ { WI_I32_AND,   WI_I64_AND,   WI_NOP,     WI_NOP },
    /* BOR  */ { WI_I32_OR,    WI_I64_OR,    WI_NOP,     WI_NOP },
};

EMIT_FUNC(binop, AstBinaryOp* binop) {
    bh_arr(WasmInstruction) code = *pcode;

    if (binop_is_assignment(binop)) {
        emit_assignment(mod, &code, binop);
        *pcode = code;
        return;
    }

    b32 is_sign_significant = 0;

    switch (binop->operation) {
        case Binary_Op_Divide:
        case Binary_Op_Modulus:
        case Binary_Op_Less:
        case Binary_Op_Less_Equal:
        case Binary_Op_Greater:
        case Binary_Op_Greater_Equal:
            is_sign_significant = 1;

        default: break;
    }

    WasmType operator_type = onyx_type_to_wasm_type(binop->left->type);
    i32 optype = 0;
    if      (operator_type == WASM_TYPE_INT32)   optype = 0;
    else if (operator_type == WASM_TYPE_INT64)   optype = 1;
    else if (operator_type == WASM_TYPE_FLOAT32) optype = 2;
    else if (operator_type == WASM_TYPE_FLOAT64) optype = 3;

    WasmInstructionType binop_instr = binop_map[(i32) binop->operation][optype];

    if (binop_instr == WI_NOP) {
        assert(("Invalid type and operation", 0));
    }

    // NOTE: Use unsigned variant if needed
    // Unsigned instructions are always right after
    // the signed equivalent
    if (is_sign_significant) {
        if (binop->left->type->Basic.flags & Basic_Flag_Unsigned) {
            binop_instr = (WasmInstructionType) ((i32) binop_instr + 1);
        }
    }

    emit_expression(mod, &code, binop->left);
    emit_expression(mod, &code, binop->right);

    WI(binop_instr);

    *pcode = code;
}

EMIT_FUNC(unaryop, AstUnaryOp* unop) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (unop->operation) {
        case Unary_Op_Negate: {
            TypeBasic* type = &unop->type->Basic;

            if (type->kind == Basic_Kind_I32
                    || type->kind == Basic_Kind_I16
                    || type->kind == Basic_Kind_I8) {
                WID(WI_I32_CONST, 0x00);
                emit_expression(mod, &code, unop->expr);
                WI(WI_I32_SUB);

            }
            else if (type->kind == Basic_Kind_I64) {
                WID(WI_I64_CONST, 0x00);
                emit_expression(mod, &code, unop->expr);
                WI(WI_I64_SUB);

            }
            else {
                emit_expression(mod, &code, unop->expr);

                if (type->kind == Basic_Kind_F32)
                    WI(WI_F32_NEG);

                if (type->kind == Basic_Kind_F64)
                    WI(WI_F64_NEG);
            }

            break;
        }

        case Unary_Op_Not:
            emit_expression(mod, &code, unop->expr);

            WI(WI_I32_EQZ);
            break;

        case Unary_Op_Bitwise_Not: {
            emit_expression(mod, &code, unop->expr);

            TypeBasic* type = &unop->type->Basic;

            if (type->kind == Basic_Kind_I32
                    || type->kind == Basic_Kind_I16
                    || type->kind == Basic_Kind_I8) {
                WID(WI_I32_CONST, 0xffffffff);
                WI(WI_I32_XOR);

            }
            else if (type->kind == Basic_Kind_I64) {
                WIL(WI_I64_CONST, 0xffffffffffffffff);
                WI(WI_I64_XOR);
            }

            break;
        }

        case Unary_Op_Cast: emit_cast(mod, &code, unop); break;

        // NOTE: Any remaining auto casts can be ignored since it means that a cast was not necessary. - brendanfh 2020/09/19
        case Unary_Op_Auto_Cast: emit_expression(mod, &code, unop->expr); break;
    }

    *pcode = code;
}

EMIT_FUNC(call, AstCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    u32 stack_grow_amm = 0;
    u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);

    u32 vararg_count = 0;
    u32 vararg_offset = -1;
    u64 stack_top_store_local;

    bh_arr_each(AstArgument *, parg, call->arg_arr) {
        AstArgument* arg = *parg;

        b32 place_on_stack = 0;
        b32 arg_is_struct  = type_is_structlike(arg->value->type);

        if (arg->va_kind != VA_Kind_Not_VA) {
            if (vararg_offset == -1) vararg_offset = stack_grow_amm;
            place_on_stack = 1;
        }
        if (type_get_param_pass(arg->value->type) == Param_Pass_By_Implicit_Pointer) place_on_stack = 1;

        if (place_on_stack && !arg_is_struct) WID(WI_GLOBAL_GET, stack_top_idx);

        if (stack_grow_amm != 0) {
            stack_top_store_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
            WID(WI_GLOBAL_GET, stack_top_idx);
            WIL(WI_LOCAL_SET, stack_top_store_local);

            WID(WI_GLOBAL_GET, stack_top_idx);
            WID(WI_I32_CONST, stack_grow_amm);
            WI(WI_I32_ADD);
            WID(WI_GLOBAL_SET, stack_top_idx);
        }

        emit_expression(mod, &code, arg->value);

        if (stack_grow_amm != 0) {
            WIL(WI_LOCAL_GET, stack_top_store_local);
            WID(WI_GLOBAL_SET, stack_top_idx);

            local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
        }

        if (place_on_stack) {
            if (arg_is_struct) WID(WI_GLOBAL_GET, stack_top_idx);
            emit_store_instruction(mod, &code, arg->value->type, stack_grow_amm);

            if (arg->va_kind != VA_Kind_Not_VA) vararg_count += 1;
            else {
                WID(WI_GLOBAL_GET, stack_top_idx);
                WID(WI_I32_CONST, stack_grow_amm);
                WI(WI_I32_ADD);
            }

            stack_grow_amm += type_size_of(arg->value->type);
        }
    }

    switch (call->va_kind) {
        case VA_Kind_Typed: {
            WID(WI_GLOBAL_GET, stack_top_idx);
            WID(WI_I32_CONST, vararg_offset);
            WI(WI_I32_ADD);
            WID(WI_I32_CONST, vararg_count);
            break;
        }

        case VA_Kind_Untyped: {
            WID(WI_GLOBAL_GET, stack_top_idx);
            WID(WI_GLOBAL_GET, stack_top_idx);
            WID(WI_I32_CONST, vararg_offset);
            WI(WI_I32_ADD);
            emit_store_instruction(mod, &code, &basic_types[Basic_Kind_I32], stack_grow_amm);

            WID(WI_GLOBAL_GET, stack_top_idx);
            WID(WI_I32_CONST, vararg_count);
            emit_store_instruction(mod, &code, &basic_types[Basic_Kind_I32], stack_grow_amm + 4);

            WID(WI_GLOBAL_GET, stack_top_idx);
            WID(WI_I32_CONST, stack_grow_amm);
            WI(WI_I32_ADD);

            stack_grow_amm += 8;
            break;
        }

        default: break;
    }

    CallingConvention cc = type_function_get_cc(call->callee->type);
    assert(cc != CC_Undefined);

    b32 needs_stack = (cc == CC_Return_Stack) || (stack_grow_amm > 0);

    Type* return_type = call->callee->type->Function.return_type;
    u32 return_size = type_size_of(return_type);
    u32 return_align = type_alignment_of(return_type);
    bh_align(return_size, return_align);

    if (cc == CC_Return_Stack) {
        bh_align(stack_grow_amm, return_align);
        stack_grow_amm += return_size;
    }

    bh_align(stack_grow_amm, 16);

    if (needs_stack) {
        WID(WI_GLOBAL_GET, stack_top_idx);
        WID(WI_I32_CONST, stack_grow_amm);
        WI(WI_I32_ADD);
        WID(WI_GLOBAL_SET, stack_top_idx);
    }

    if (call->callee->kind == Ast_Kind_Function) {
        i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) call->callee);
        bh_arr_push(code, ((WasmInstruction){ WI_CALL, func_idx }));

    } else {
        emit_expression(mod, &code, call->callee);

        i32 type_idx = generate_type_idx(mod, call->callee->type);
        WID(WI_CALL_INDIRECT, ((WasmInstructionData) { type_idx, 0x00 }));
    }

    if (needs_stack) {
        WID(WI_GLOBAL_GET, stack_top_idx);
        WID(WI_I32_CONST, stack_grow_amm);
        WI(WI_I32_SUB);
        WID(WI_GLOBAL_SET, stack_top_idx);
    }

    if (cc == CC_Return_Stack) {
        WID(WI_GLOBAL_GET, stack_top_idx);
        emit_load_instruction(mod, &code, return_type, stack_grow_amm - return_size);
    }

    *pcode = code;
}

// BUG: This implementation assumes that the host system C's implementation is using
// little endian integers.
#define SIMD_INT_CONST_INTRINSIC(type, count) { \
        type* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16); \
        AstArgument* arg = call->arguments; \
        fori (i, 0, count) { \
            if (arg->value->kind != Ast_Kind_NumLit) { \
                onyx_report_error(arg->token->pos, \
                        "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.", \
                        i, bh_num_suffix(i)); \
                *pcode = code; \
                return; \
            } \
            byte_buffer[i] = (type) ((AstNumLit *) arg->value)->value.l; \
            arg = (AstArgument *) arg->next; \
        } \
        WIP(WI_V128_CONST, byte_buffer); \
    }

#define SIMD_EXTRACT_LANE_INSTR(instr, arg) \
    emit_expression(mod, &code, arg->value);\
    arg = (AstArgument *) arg->next; \
    if (arg->value->kind != Ast_Kind_NumLit) { \
        onyx_report_error(arg->token->pos, "SIMD lane instructions expect a compile time lane number."); \
        *pcode = code; \
        return; \
    } \
    WID(instr, (u8) ((AstNumLit *) arg->value)->value.i);

#define SIMD_REPLACE_LANE_INSTR(instr, arg) { \
        emit_expression(mod, &code, arg->value);\
        arg = (AstArgument *) arg->next; \
        if (arg->value->kind != Ast_Kind_NumLit) { \
            onyx_report_error(arg->token->pos, "SIMD lane instructions expect a compile time lane number."); \
            *pcode = code; \
            return; \
        } \
        u8 lane = (u8) ((AstNumLit *) arg->value)->value.i; \
        arg = (AstArgument *) arg->next; \
        emit_expression(mod, &code, arg->value); \
        WID(instr, lane); \
    }


EMIT_FUNC(intrinsic_call, AstIntrinsicCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    b32 place_arguments_normally = 1;

    switch (call->intrinsic) {
        case ONYX_INTRINSIC_V128_CONST:
        case ONYX_INTRINSIC_I8X16_CONST: case ONYX_INTRINSIC_I16X8_CONST:
        case ONYX_INTRINSIC_I32X4_CONST: case ONYX_INTRINSIC_I64X2_CONST:
        case ONYX_INTRINSIC_F32X4_CONST: case ONYX_INTRINSIC_F64X2_CONST:
        case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_S: case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_U:
        case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_S: case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_U:
        case ONYX_INTRINSIC_I32X4_EXTRACT_LANE:   case ONYX_INTRINSIC_I64X2_EXTRACT_LANE:
        case ONYX_INTRINSIC_F32X4_EXTRACT_LANE:   case ONYX_INTRINSIC_F64X2_EXTRACT_LANE:
        case ONYX_INTRINSIC_I8X16_REPLACE_LANE:   case ONYX_INTRINSIC_I16X8_REPLACE_LANE:
        case ONYX_INTRINSIC_I32X4_REPLACE_LANE:   case ONYX_INTRINSIC_I64X2_REPLACE_LANE:
        case ONYX_INTRINSIC_F32X4_REPLACE_LANE:   case ONYX_INTRINSIC_F64X2_REPLACE_LANE:
        case ONYX_INTRINSIC_I8X16_SHUFFLE:
            place_arguments_normally = 0;

        default: break;
    }

    if (place_arguments_normally) {
        for (AstArgument *arg = call->arguments;
                arg != NULL;
                arg = (AstArgument *) arg->next) {
            emit_expression(mod, &code, arg->value);
        }
    }

    switch (call->intrinsic) {
        case ONYX_INTRINSIC_MEMORY_SIZE:  WID(WI_MEMORY_SIZE, 0x00); break;
        case ONYX_INTRINSIC_MEMORY_GROW:  WID(WI_MEMORY_GROW, 0x00); break;

        case ONYX_INTRINSIC_I32_CLZ:      WI(WI_I32_CLZ); break;
        case ONYX_INTRINSIC_I32_CTZ:      WI(WI_I32_CTZ); break;
        case ONYX_INTRINSIC_I32_POPCNT:   WI(WI_I32_POPCNT); break;
        case ONYX_INTRINSIC_I32_AND:      WI(WI_I32_AND); break;
        case ONYX_INTRINSIC_I32_OR:       WI(WI_I32_OR); break;
        case ONYX_INTRINSIC_I32_XOR:      WI(WI_I32_XOR); break;
        case ONYX_INTRINSIC_I32_SHL:      WI(WI_I32_SHL); break;
        case ONYX_INTRINSIC_I32_SLR:      WI(WI_I32_SHR_U); break;
        case ONYX_INTRINSIC_I32_SAR:      WI(WI_I32_SHR_S); break;
        case ONYX_INTRINSIC_I32_ROTL:     WI(WI_I32_ROTL); break;
        case ONYX_INTRINSIC_I32_ROTR:     WI(WI_I32_ROTR); break;

        case ONYX_INTRINSIC_I64_CLZ:      WI(WI_I64_CLZ); break;
        case ONYX_INTRINSIC_I64_CTZ:      WI(WI_I64_CTZ); break;
        case ONYX_INTRINSIC_I64_POPCNT:   WI(WI_I64_POPCNT); break;
        case ONYX_INTRINSIC_I64_AND:      WI(WI_I64_AND); break;
        case ONYX_INTRINSIC_I64_OR:       WI(WI_I64_OR); break;
        case ONYX_INTRINSIC_I64_XOR:      WI(WI_I64_XOR); break;
        case ONYX_INTRINSIC_I64_SHL:      WI(WI_I64_SHL); break;
        case ONYX_INTRINSIC_I64_SLR:      WI(WI_I64_SHR_U); break;
        case ONYX_INTRINSIC_I64_SAR:      WI(WI_I64_SHR_S); break;
        case ONYX_INTRINSIC_I64_ROTL:     WI(WI_I64_ROTL); break;
        case ONYX_INTRINSIC_I64_ROTR:     WI(WI_I64_ROTR); break;

        case ONYX_INTRINSIC_F32_ABS:      WI(WI_F32_ABS); break;
        case ONYX_INTRINSIC_F32_CEIL:     WI(WI_F32_CEIL); break;
        case ONYX_INTRINSIC_F32_FLOOR:    WI(WI_F32_FLOOR); break;
        case ONYX_INTRINSIC_F32_TRUNC:    WI(WI_F32_TRUNC); break;
        case ONYX_INTRINSIC_F32_NEAREST:  WI(WI_F32_NEAREST); break;
        case ONYX_INTRINSIC_F32_SQRT:     WI(WI_F32_SQRT); break;
        case ONYX_INTRINSIC_F32_MIN:      WI(WI_F32_MIN); break;
        case ONYX_INTRINSIC_F32_MAX:      WI(WI_F32_MAX); break;
        case ONYX_INTRINSIC_F32_COPYSIGN: WI(WI_F32_COPYSIGN); break;

        case ONYX_INTRINSIC_F64_ABS:      WI(WI_F64_ABS); break;
        case ONYX_INTRINSIC_F64_CEIL:     WI(WI_F64_CEIL); break;
        case ONYX_INTRINSIC_F64_FLOOR:    WI(WI_F64_FLOOR); break;
        case ONYX_INTRINSIC_F64_TRUNC:    WI(WI_F64_TRUNC); break;
        case ONYX_INTRINSIC_F64_NEAREST:  WI(WI_F64_NEAREST); break;
        case ONYX_INTRINSIC_F64_SQRT:     WI(WI_F64_SQRT); break;
        case ONYX_INTRINSIC_F64_MIN:      WI(WI_F64_MIN); break;
        case ONYX_INTRINSIC_F64_MAX:      WI(WI_F64_MAX); break;
        case ONYX_INTRINSIC_F64_COPYSIGN: WI(WI_F64_COPYSIGN); break;

        case ONYX_INTRINSIC_I8X16_CONST:
        case ONYX_INTRINSIC_V128_CONST:   SIMD_INT_CONST_INTRINSIC(u8, 16);   break;
        case ONYX_INTRINSIC_I16X8_CONST:  SIMD_INT_CONST_INTRINSIC(u16, 8);   break;
        case ONYX_INTRINSIC_I32X4_CONST:  SIMD_INT_CONST_INTRINSIC(u32, 4);   break;
        case ONYX_INTRINSIC_I64X2_CONST:  SIMD_INT_CONST_INTRINSIC(u64, 2);   break;
        case ONYX_INTRINSIC_F32X4_CONST: {
            f32* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            AstArgument* arg = call->arguments;
            fori (i, 0, 4) {
                if (arg->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg->token->pos,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (f32) ((AstNumLit *) arg->value)->value.f;
                arg = (AstArgument *) arg->next;
            }
            WIP(WI_V128_CONST, byte_buffer);
            break;
        }

        case ONYX_INTRINSIC_F64X2_CONST: {
            f64* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            AstArgument* arg = call->arguments;
            fori (i, 0, 2) {
                if (arg->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg->token->pos,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (f64) ((AstNumLit *) arg->value)->value.d;
                arg = (AstArgument *) arg->next;
            }
            WIP(WI_V128_CONST, byte_buffer);
            break;
        }

        case ONYX_INTRINSIC_I8X16_SHUFFLE: {
            u8* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            AstArgument* arg = call->arguments;

            // NOTE: There are two parameters that have to be outputted before
            // the immediate bytes
            emit_expression(mod, &code, arg->value);
            arg = (AstArgument *) arg->next;
            emit_expression(mod, &code, arg->value);
            arg = (AstArgument *) arg->next;

            fori (i, 0, 16) {
                if (arg->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg->token->pos,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (u8) ((AstNumLit *) arg->value)->value.i;
                arg = (AstArgument *) arg->next;
            }
            WIP(WI_I8X16_SHUFFLE, byte_buffer);
            break;
        }

        case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_S: SIMD_EXTRACT_LANE_INSTR(WI_I8X16_EXTRACT_LANE_S, call->arguments); break;
        case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_U: SIMD_EXTRACT_LANE_INSTR(WI_I8X16_EXTRACT_LANE_U, call->arguments); break;
        case ONYX_INTRINSIC_I8X16_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I8X16_REPLACE_LANE, call->arguments); break;
        case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_S: SIMD_EXTRACT_LANE_INSTR(WI_I16X8_EXTRACT_LANE_S, call->arguments); break;
        case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_U: SIMD_EXTRACT_LANE_INSTR(WI_I16X8_EXTRACT_LANE_U, call->arguments); break;
        case ONYX_INTRINSIC_I16X8_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I16X8_REPLACE_LANE, call->arguments); break;
        case ONYX_INTRINSIC_I32X4_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_I32X4_EXTRACT_LANE, call->arguments); break;
        case ONYX_INTRINSIC_I32X4_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I32X4_REPLACE_LANE, call->arguments); break;
        case ONYX_INTRINSIC_I64X2_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_I64X2_EXTRACT_LANE, call->arguments); break;
        case ONYX_INTRINSIC_I64X2_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I64X2_REPLACE_LANE, call->arguments); break;
        case ONYX_INTRINSIC_F32X4_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_F32X4_EXTRACT_LANE, call->arguments); break;
        case ONYX_INTRINSIC_F32X4_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_F32X4_REPLACE_LANE, call->arguments); break;
        case ONYX_INTRINSIC_F64X2_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_F64X2_EXTRACT_LANE, call->arguments); break;
        case ONYX_INTRINSIC_F64X2_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_F64X2_REPLACE_LANE, call->arguments); break;

        case ONYX_INTRINSIC_I8X16_SWIZZLE: WI(WI_I8X16_SWIZZLE); break;
        case ONYX_INTRINSIC_I8X16_SPLAT:   WI(WI_I8X16_SPLAT); break;
        case ONYX_INTRINSIC_I16X8_SPLAT:   WI(WI_I16X8_SPLAT); break;
        case ONYX_INTRINSIC_I32X4_SPLAT:   WI(WI_I32X4_SPLAT); break;
        case ONYX_INTRINSIC_I64X2_SPLAT:   WI(WI_I64X2_SPLAT); break;
        case ONYX_INTRINSIC_F32X4_SPLAT:   WI(WI_F32X4_SPLAT); break;
        case ONYX_INTRINSIC_F64X2_SPLAT:   WI(WI_F64X2_SPLAT); break;

        case ONYX_INTRINSIC_I8X16_EQ:   WI(WI_I8X16_EQ); break;
        case ONYX_INTRINSIC_I8X16_NEQ:  WI(WI_I8X16_NEQ); break;
        case ONYX_INTRINSIC_I8X16_LT_S: WI(WI_I8X16_LT_S); break;
        case ONYX_INTRINSIC_I8X16_LT_U: WI(WI_I8X16_LT_U); break;
        case ONYX_INTRINSIC_I8X16_GT_S: WI(WI_I8X16_GT_S); break;
        case ONYX_INTRINSIC_I8X16_GT_U: WI(WI_I8X16_GT_U); break;
        case ONYX_INTRINSIC_I8X16_LE_S: WI(WI_I8X16_LE_S); break;
        case ONYX_INTRINSIC_I8X16_LE_U: WI(WI_I8X16_LE_U); break;
        case ONYX_INTRINSIC_I8X16_GE_S: WI(WI_I8X16_GE_S); break;
        case ONYX_INTRINSIC_I8X16_GE_U: WI(WI_I8X16_GE_U); break;

        case ONYX_INTRINSIC_I16X8_EQ:   WI(WI_I16X8_EQ); break;
        case ONYX_INTRINSIC_I16X8_NEQ:  WI(WI_I16X8_NEQ); break;
        case ONYX_INTRINSIC_I16X8_LT_S: WI(WI_I16X8_LT_S); break;
        case ONYX_INTRINSIC_I16X8_LT_U: WI(WI_I16X8_LT_U); break;
        case ONYX_INTRINSIC_I16X8_GT_S: WI(WI_I16X8_GT_S); break;
        case ONYX_INTRINSIC_I16X8_GT_U: WI(WI_I16X8_GT_U); break;
        case ONYX_INTRINSIC_I16X8_LE_S: WI(WI_I16X8_LE_S); break;
        case ONYX_INTRINSIC_I16X8_LE_U: WI(WI_I16X8_LE_U); break;
        case ONYX_INTRINSIC_I16X8_GE_S: WI(WI_I16X8_GE_S); break;
        case ONYX_INTRINSIC_I16X8_GE_U: WI(WI_I16X8_GE_U); break;

        case ONYX_INTRINSIC_I32X4_EQ:   WI(WI_I32X4_EQ); break;
        case ONYX_INTRINSIC_I32X4_NEQ:  WI(WI_I32X4_NEQ); break;
        case ONYX_INTRINSIC_I32X4_LT_S: WI(WI_I32X4_LT_S); break;
        case ONYX_INTRINSIC_I32X4_LT_U: WI(WI_I32X4_LT_U); break;
        case ONYX_INTRINSIC_I32X4_GT_S: WI(WI_I32X4_GT_S); break;
        case ONYX_INTRINSIC_I32X4_GT_U: WI(WI_I32X4_GT_U); break;
        case ONYX_INTRINSIC_I32X4_LE_S: WI(WI_I32X4_LE_S); break;
        case ONYX_INTRINSIC_I32X4_LE_U: WI(WI_I32X4_LE_U); break;
        case ONYX_INTRINSIC_I32X4_GE_S: WI(WI_I32X4_GE_S); break;
        case ONYX_INTRINSIC_I32X4_GE_U: WI(WI_I32X4_GE_U); break;

        case ONYX_INTRINSIC_F32X4_EQ:  WI(WI_F32X4_EQ); break;
        case ONYX_INTRINSIC_F32X4_NEQ: WI(WI_F32X4_NEQ); break;
        case ONYX_INTRINSIC_F32X4_LT:  WI(WI_F32X4_LT); break;
        case ONYX_INTRINSIC_F32X4_GT:  WI(WI_F32X4_GT); break;
        case ONYX_INTRINSIC_F32X4_LE:  WI(WI_F32X4_LE); break;
        case ONYX_INTRINSIC_F32X4_GE:  WI(WI_F32X4_GE); break;

        case ONYX_INTRINSIC_F64X2_EQ:  WI(WI_F64X2_EQ); break;
        case ONYX_INTRINSIC_F64X2_NEQ: WI(WI_F64X2_NEQ); break;
        case ONYX_INTRINSIC_F64X2_LT:  WI(WI_F64X2_LT); break;
        case ONYX_INTRINSIC_F64X2_GT:  WI(WI_F64X2_GT); break;
        case ONYX_INTRINSIC_F64X2_LE:  WI(WI_F64X2_LE); break;
        case ONYX_INTRINSIC_F64X2_GE:  WI(WI_F64X2_GE); break;

        case ONYX_INTRINSIC_V128_NOT:       WI(WI_V128_NOT); break;
        case ONYX_INTRINSIC_V128_AND:       WI(WI_V128_AND); break;
        case ONYX_INTRINSIC_V128_ANDNOT:    WI(WI_V128_ANDNOT); break;
        case ONYX_INTRINSIC_V128_OR:        WI(WI_V128_OR); break;
        case ONYX_INTRINSIC_V128_XOR:       WI(WI_V128_XOR); break;
        case ONYX_INTRINSIC_V128_BITSELECT: WI(WI_V128_BITSELECT); break;

        case ONYX_INTRINSIC_I8X16_ABS:            WI(WI_I8X16_ABS); break;
        case ONYX_INTRINSIC_I8X16_NEG:            WI(WI_I8X16_NEG); break;
        case ONYX_INTRINSIC_I8X16_ANY_TRUE:       WI(WI_I8X16_ANY_TRUE); break;
        case ONYX_INTRINSIC_I8X16_ALL_TRUE:       WI(WI_I8X16_ALL_TRUE); break;
        case ONYX_INTRINSIC_I8X16_BITMASK:        WI(WI_I8X16_BITMASK); break;
        case ONYX_INTRINSIC_I8X16_NARROW_I16X8_S: WI(WI_I8X16_NARROW_I16X8_S); break;
        case ONYX_INTRINSIC_I8X16_NARROW_I16X8_U: WI(WI_I8X16_NARROW_I16X8_U); break;
        case ONYX_INTRINSIC_I8X16_SHL:            WI(WI_I8X16_SHL); break;
        case ONYX_INTRINSIC_I8X16_SHR_S:          WI(WI_I8X16_SHR_S); break;
        case ONYX_INTRINSIC_I8X16_SHR_U:          WI(WI_I8X16_SHR_U); break;
        case ONYX_INTRINSIC_I8X16_ADD:            WI(WI_I8X16_ADD); break;
        case ONYX_INTRINSIC_I8X16_ADD_SAT_S:      WI(WI_I8X16_ADD_SAT_S); break;
        case ONYX_INTRINSIC_I8X16_ADD_SAT_U:      WI(WI_I8X16_ADD_SAT_U); break;
        case ONYX_INTRINSIC_I8X16_SUB:            WI(WI_I8X16_SUB); break;
        case ONYX_INTRINSIC_I8X16_SUB_SAT_S:      WI(WI_I8X16_SUB_SAT_S); break;
        case ONYX_INTRINSIC_I8X16_SUB_SAT_U:      WI(WI_I8X16_SUB_SAT_U); break;
        case ONYX_INTRINSIC_I8X16_MIN_S:          WI(WI_I8X16_MIN_S); break;
        case ONYX_INTRINSIC_I8X16_MIN_U:          WI(WI_I8X16_MIN_U); break;
        case ONYX_INTRINSIC_I8X16_MAX_S:          WI(WI_I8X16_MAX_S); break;
        case ONYX_INTRINSIC_I8X16_MAX_U:          WI(WI_I8X16_MAX_U); break;
        case ONYX_INTRINSIC_I8X16_AVGR_U:         WI(WI_I8X16_AVGR_U); break;

        case ONYX_INTRINSIC_I16X8_ABS:                WI(WI_I16X8_ABS); break;
        case ONYX_INTRINSIC_I16X8_NEG:                WI(WI_I16X8_NEG); break;
        case ONYX_INTRINSIC_I16X8_ANY_TRUE:           WI(WI_I16X8_ANY_TRUE); break;
        case ONYX_INTRINSIC_I16X8_ALL_TRUE:           WI(WI_I16X8_ALL_TRUE); break;
        case ONYX_INTRINSIC_I16X8_BITMASK:            WI(WI_I16X8_BITMASK); break;
        case ONYX_INTRINSIC_I16X8_NARROW_I32X4_S:     WI(WI_I16X8_NARROW_I32X4_S); break;
        case ONYX_INTRINSIC_I16X8_NARROW_I32X4_U:     WI(WI_I16X8_NARROW_I32X4_U); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_S:  WI(WI_I16X8_WIDEN_LOW_I8X16_S); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_S: WI(WI_I16X8_WIDEN_HIGH_I8X16_S); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_U:  WI(WI_I16X8_WIDEN_LOW_I8X16_U); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_U: WI(WI_I16X8_WIDEN_HIGH_I8X16_U); break;
        case ONYX_INTRINSIC_I16X8_SHL:                WI(WI_I16X8_SHL); break;
        case ONYX_INTRINSIC_I16X8_SHR_S:              WI(WI_I16X8_SHR_S); break;
        case ONYX_INTRINSIC_I16X8_SHR_U:              WI(WI_I16X8_SHR_U); break;
        case ONYX_INTRINSIC_I16X8_ADD:                WI(WI_I16X8_ADD); break;
        case ONYX_INTRINSIC_I16X8_ADD_SAT_S:          WI(WI_I16X8_ADD_SAT_S); break;
        case ONYX_INTRINSIC_I16X8_ADD_SAT_U:          WI(WI_I16X8_ADD_SAT_U); break;
        case ONYX_INTRINSIC_I16X8_SUB:                WI(WI_I16X8_SUB); break;
        case ONYX_INTRINSIC_I16X8_SUB_SAT_S:          WI(WI_I16X8_SUB_SAT_S); break;
        case ONYX_INTRINSIC_I16X8_SUB_SAT_U:          WI(WI_I16X8_SUB_SAT_U); break;
        case ONYX_INTRINSIC_I16X8_MUL:                WI(WI_I16X8_MUL); break;
        case ONYX_INTRINSIC_I16X8_MIN_S:              WI(WI_I16X8_MIN_S); break;
        case ONYX_INTRINSIC_I16X8_MIN_U:              WI(WI_I16X8_MIN_U); break;
        case ONYX_INTRINSIC_I16X8_MAX_S:              WI(WI_I16X8_MAX_S); break;
        case ONYX_INTRINSIC_I16X8_MAX_U:              WI(WI_I16X8_MAX_U); break;
        case ONYX_INTRINSIC_I16X8_AVGR_U:             WI(WI_I16X8_AVGR_U); break;

        case ONYX_INTRINSIC_I32X4_ABS:                WI(WI_I32X4_ABS); break;
        case ONYX_INTRINSIC_I32X4_NEG:                WI(WI_I32X4_NEG); break;
        case ONYX_INTRINSIC_I32X4_ANY_TRUE:           WI(WI_I32X4_ANY_TRUE); break;
        case ONYX_INTRINSIC_I32X4_ALL_TRUE:           WI(WI_I32X4_ALL_TRUE); break;
        case ONYX_INTRINSIC_I32X4_BITMASK:            WI(WI_I32X4_BITMASK); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_S:  WI(WI_I32X4_WIDEN_LOW_I16X8_S); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_S: WI(WI_I32X4_WIDEN_HIGH_I16X8_S); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_U:  WI(WI_I32X4_WIDEN_LOW_I16X8_U); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_U: WI(WI_I32X4_WIDEN_HIGH_I16X8_U); break;
        case ONYX_INTRINSIC_I32X4_SHL:                WI(WI_I32X4_SHL); break;
        case ONYX_INTRINSIC_I32X4_SHR_S:              WI(WI_I32X4_SHR_S); break;
        case ONYX_INTRINSIC_I32X4_SHR_U:              WI(WI_I32X4_SHR_U); break;
        case ONYX_INTRINSIC_I32X4_ADD:                WI(WI_I32X4_ADD); break;
        case ONYX_INTRINSIC_I32X4_SUB:                WI(WI_I32X4_SUB); break;
        case ONYX_INTRINSIC_I32X4_MUL:                WI(WI_I32X4_MUL); break;
        case ONYX_INTRINSIC_I32X4_MIN_S:              WI(WI_I32X4_MIN_S); break;
        case ONYX_INTRINSIC_I32X4_MIN_U:              WI(WI_I32X4_MIN_U); break;
        case ONYX_INTRINSIC_I32X4_MAX_S:              WI(WI_I32X4_MAX_S); break;
        case ONYX_INTRINSIC_I32X4_MAX_U:              WI(WI_I32X4_MAX_U); break;

        case ONYX_INTRINSIC_I64X2_NEG:   WI(WI_I64X2_NEG); break;
        case ONYX_INTRINSIC_I64X2_SHL:   WI(WI_I64X2_SHL); break;
        case ONYX_INTRINSIC_I64X2_SHR_S: WI(WI_I64X2_SHR_S); break;
        case ONYX_INTRINSIC_I64X2_SHR_U: WI(WI_I64X2_SHR_U); break;
        case ONYX_INTRINSIC_I64X2_ADD:   WI(WI_I64X2_ADD); break;
        case ONYX_INTRINSIC_I64X2_SUB:   WI(WI_I64X2_SUB); break;
        case ONYX_INTRINSIC_I64X2_MUL:   WI(WI_I64X2_MUL); break;

        case ONYX_INTRINSIC_F32X4_ABS:  WI(WI_F32X4_ABS); break;
        case ONYX_INTRINSIC_F32X4_NEG:  WI(WI_F32X4_NEG); break;
        case ONYX_INTRINSIC_F32X4_SQRT: WI(WI_F32X4_SQRT); break;
        case ONYX_INTRINSIC_F32X4_ADD:  WI(WI_F32X4_ADD); break;
        case ONYX_INTRINSIC_F32X4_SUB:  WI(WI_F32X4_SUB); break;
        case ONYX_INTRINSIC_F32X4_MUL:  WI(WI_F32X4_MUL); break;
        case ONYX_INTRINSIC_F32X4_DIV:  WI(WI_F32X4_DIV); break;
        case ONYX_INTRINSIC_F32X4_MIN:  WI(WI_F32X4_MIN); break;
        case ONYX_INTRINSIC_F32X4_MAX:  WI(WI_F32X4_MAX); break;

        case ONYX_INTRINSIC_F64X2_ABS:  WI(WI_F64X2_ABS); break;
        case ONYX_INTRINSIC_F64X2_NEG:  WI(WI_F64X2_NEG); break;
        case ONYX_INTRINSIC_F64X2_SQRT: WI(WI_F64X2_SQRT); break;
        case ONYX_INTRINSIC_F64X2_ADD:  WI(WI_F64X2_ADD); break;
        case ONYX_INTRINSIC_F64X2_SUB:  WI(WI_F64X2_SUB); break;
        case ONYX_INTRINSIC_F64X2_MUL:  WI(WI_F64X2_MUL); break;
        case ONYX_INTRINSIC_F64X2_DIV:  WI(WI_F64X2_DIV); break;
        case ONYX_INTRINSIC_F64X2_MIN:  WI(WI_F64X2_MIN); break;
        case ONYX_INTRINSIC_F64X2_MAX:  WI(WI_F64X2_MAX); break;

        case ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_S: WI(WI_I32X4_TRUNC_SAT_F32X4_S); break;
        case ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_U: WI(WI_I32X4_TRUNC_SAT_F32X4_U); break;
        case ONYX_INTRINSIC_F32X4_CONVERT_I32X4_S:   WI(WI_F32X4_CONVERT_I32X4_S); break;
        case ONYX_INTRINSIC_F32X4_CONVERT_I32X4_U:   WI(WI_F32X4_CONVERT_I32X4_U); break;

        default: assert(("Unsupported intrinsic", 0));
    }

    *pcode = code;
}

EMIT_FUNC(array_access_location, AstArrayAccess* aa, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    emit_expression(mod, &code, aa->expr);
    if (aa->elem_size != 1) {
        WID(WI_I32_CONST, aa->elem_size);
        WI(WI_I32_MUL);
    }

    u64 offset = 0;
    if (aa->addr->kind == Ast_Kind_Array_Access
        && aa->addr->type->kind == Type_Kind_Array) {
        emit_array_access_location(mod, &code, (AstArrayAccess *) aa->addr, &offset);
    } else if (aa->addr->kind == Ast_Kind_Field_Access
        && aa->addr->type->kind == Type_Kind_Array) {
        emit_field_access_location(mod, &code, (AstFieldAccess *) aa->addr, &offset);
    } else if ((aa->addr->kind == Ast_Kind_Local || aa->addr->kind == Ast_Kind_Param)
        && aa->addr->type->kind == Type_Kind_Array) {
        emit_local_location(mod, &code, (AstLocal *) aa->addr, &offset);
    } else if (aa->addr->kind == Ast_Kind_Memres
        && aa->addr->type->kind != Type_Kind_Array) {
        emit_memory_reservation_location(mod, &code, (AstMemRes *) aa->addr);
    } else {
        emit_expression(mod, &code, aa->addr);
    }
    WI(WI_I32_ADD);

    *offset_return += offset;

    *pcode = code;
}

EMIT_FUNC(field_access_location, AstFieldAccess* field, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 offset = field->offset;
    AstTyped* source_expr = field->expr;
    while (source_expr->kind == Ast_Kind_Field_Access
            && type_is_structlike_strict(source_expr->type)) {
        offset += ((AstFieldAccess *) source_expr)->offset;
        source_expr = (AstTyped *) ((AstFieldAccess *) source_expr)->expr;
    }

    if (source_expr->kind == Ast_Kind_Array_Access
        && source_expr->type->kind != Type_Kind_Pointer) {
        u64 o2 = 0;
        emit_array_access_location(mod, &code, (AstArrayAccess *) source_expr, &o2);
        offset += o2;

    } else if ((source_expr->kind == Ast_Kind_Local || source_expr->kind == Ast_Kind_Param)
        && source_expr->type->kind != Type_Kind_Pointer) {
        u64 o2 = 0;
        emit_local_location(mod, &code, (AstLocal *) source_expr, &o2);
        offset += o2;

    } else if (source_expr->kind == Ast_Kind_Memres
        && source_expr->type->kind != Type_Kind_Pointer) {
        emit_memory_reservation_location(mod, &code, (AstMemRes *) source_expr);

    } else {
        emit_expression(mod, &code, source_expr);
    }

    *offset_return = offset;

    *pcode = code;
}

EMIT_FUNC(memory_reservation_location, AstMemRes* memres) {
    bh_arr(WasmInstruction) code = *pcode;

    WID(WI_I32_CONST, memres->addr);

    *pcode = code;
}

EMIT_FUNC(local_location, AstLocal* local, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = (u64) bh_imap_get(&mod->local_map, (u64) local);

    if (local_offset & LOCAL_IS_WASM) {
        // CLEANUP structs-by-value
        // This is a weird condition but it is relied on in a couple places including
        // passing non-simple structs by value.             -brendanfh 2020/09/18
        WIL(WI_LOCAL_GET, local_offset);

    } else {
        WIL(WI_LOCAL_GET, mod->stack_base_idx);

        *offset_return += local_offset;
    }

    *pcode = code;
}

EMIT_FUNC(struct_load, Type* type, u64 offset) {
    // NOTE: Expects the stack to look like:
    //      <location>

    bh_arr(WasmInstruction) code = *pcode;

    assert(type_is_structlike_strict(type));

    u32 mem_count = type_structlike_mem_count(type);
    StructMember smem;

    if (mem_count == 1) {
        type_lookup_member_by_idx(type, 0, &smem);
        emit_load_instruction(mod, &code, smem.type, offset);
        *pcode = code;
        return;
    }

    u64 tmp_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WIL(WI_LOCAL_TEE, tmp_idx);

    fori (i, 0, mem_count) {
        type_lookup_member_by_idx(type, i, &smem);
        if (i != 0) WIL(WI_LOCAL_GET, tmp_idx);
        emit_load_instruction(mod, &code, smem.type, offset + smem.offset);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);

    *pcode = code;
}

EMIT_FUNC(struct_lval, AstTyped* lval) {
    // NOTE: Expects the stack to look like:
    //      mem_1
    //      mem_2
    //      ...
    //      mem_n

    bh_arr(WasmInstruction) code = *pcode;

    assert(type_is_structlike_strict(lval->type));

    u64 offset = 0;

    switch (lval->kind) {
        case Ast_Kind_Local:        emit_local_location(mod, &code, (AstLocal *) lval, &offset); break;
        case Ast_Kind_Dereference:  emit_expression(mod, &code, ((AstDereference *) lval)->expr); break;
        case Ast_Kind_Array_Access: emit_array_access_location(mod, &code, (AstArrayAccess *) lval, &offset); break;
        case Ast_Kind_Field_Access: emit_field_access_location(mod, &code, (AstFieldAccess *) lval, &offset); break;
        case Ast_Kind_Memres:       emit_memory_reservation_location(mod, &code, (AstMemRes *) lval); break;

        default: assert(0);
    }

    emit_struct_store(mod, &code, lval->type, offset);

    *pcode = code;
}

EMIT_FUNC(struct_store, Type* type, u64 offset) {
    // NOTE: Expects the stack to look like:
    //      mem_1
    //      mem_2
    //      ...
    //      mem_n
    //      loc

    bh_arr(WasmInstruction) code = *pcode;

    assert(type_is_structlike_strict(type));

    u64 loc_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WIL(WI_LOCAL_SET, loc_idx);

    StructMember smem;

    u32 mem_count = type_structlike_mem_count(type);
    forir (i, mem_count - 1, 0) {
        type_lookup_member_by_idx(type, i, &smem);

        if (type_is_structlike_strict(smem.type)) {
            if (bh_arr_last(code).type == WI_LOCAL_SET && bh_arr_last(code).data.l == loc_idx) {
                bh_arr_last(code).type = WI_LOCAL_TEE;
            } else {
                WIL(WI_LOCAL_GET, loc_idx);
            }

            emit_struct_store(mod, &code, smem.type, offset + smem.offset);

        } else {
            WasmType wt = onyx_type_to_wasm_type(smem.type);
            u64 tmp_idx = local_raw_allocate(mod->local_alloc, wt);

            WIL(WI_LOCAL_SET, tmp_idx);
            WIL(WI_LOCAL_GET, loc_idx);
            WIL(WI_LOCAL_GET, tmp_idx);

            emit_store_instruction(mod, &code, smem.type, offset + smem.offset);

            local_raw_free(mod->local_alloc, wt);
        }
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);

    *pcode = code;
}

EMIT_FUNC(struct_literal, AstStructLiteral* sl) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_arr_each(AstTyped *, val, sl->values) {
        emit_expression(mod, &code, *val);
    }

    *pcode = code;
}

EMIT_FUNC(location, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (expr->kind) {
        case Ast_Kind_Param:
        case Ast_Kind_Local: {
            u64 offset = 0;
            emit_local_location(mod, &code, (AstLocal *) expr, &offset);
            if (offset != 0) {
                WID(WI_I32_CONST, offset);
                WI(WI_I32_ADD);
            }
            break;
        }

        case Ast_Kind_Dereference: {
            emit_expression(mod, &code, ((AstDereference *) expr)->expr);
            break;
        }

        case Ast_Kind_Array_Access: {
            AstArrayAccess* aa = (AstArrayAccess *) expr;
            u64 offset = 0;
            emit_array_access_location(mod, &code, aa, &offset);
            if (offset != 0) {
                WID(WI_I32_CONST, offset);
                WI(WI_I32_ADD);
            }
            break;
        }

        case Ast_Kind_Field_Access: {
            AstFieldAccess* field = (AstFieldAccess *) expr;

            u64 offset = 0;
            emit_field_access_location(mod, &code, field, &offset);
            if (offset != 0) {
                WID(WI_I32_CONST, offset);
                WI(WI_I32_ADD);
            }
            break;
        }

        case Ast_Kind_Memres: {
            AstMemRes* memres = (AstMemRes *) expr;
            WID(WI_I32_CONST, memres->addr);
            break;
        }

        default: {
            onyx_report_error(expr->token->pos, "Unable to generate locate for '%s'.", onyx_ast_node_kind_string(expr->kind));
            break;
        }
    }

    *pcode = code;
}

EMIT_FUNC(expression, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (expr->kind) {
        case Ast_Kind_Param: {
            AstLocal* param = (AstLocal *) expr;
            u64 localidx = bh_imap_get(&mod->local_map, (u64) param);

            switch (type_get_param_pass(param->type)) {
                case Param_Pass_By_Value: {
                    if (type_is_structlike_strict(expr->type)) {
                        u32 mem_count = type_structlike_mem_count(expr->type);
                        fori (idx, 0, mem_count) WIL(WI_LOCAL_GET, localidx + idx);

                    } else {
                        WIL(WI_LOCAL_GET, localidx);
                    }
                    break;
                }

                case Param_Pass_By_Implicit_Pointer: {
                    WIL(WI_LOCAL_GET, localidx);
                    emit_load_instruction(mod, &code, expr->type, 0);
                    break;
                }

                default: assert(0);
            }

            break;
        }

        case Ast_Kind_Local: {
            u64 tmp = bh_imap_get(&mod->local_map, (u64) expr);

            if (tmp & LOCAL_IS_WASM) {
                if (bh_arr_last(code).type == WI_LOCAL_SET && bh_arr_last(code).data.l == tmp) {
                    bh_arr_last(code).type = WI_LOCAL_TEE;
                } else {
                    WIL(WI_LOCAL_GET, tmp);
                }

            } else {
                u64 offset = 0;
                emit_local_location(mod, &code, (AstLocal *) expr, &offset);

                if (expr->type->kind != Type_Kind_Array) {
                    emit_load_instruction(mod, &code, expr->type, offset);
                } else if (offset != 0) {
                    WID(WI_I32_CONST, offset);
                    WI(WI_I32_ADD);
                }
            }

            break;
        }

        case Ast_Kind_Global: {
            i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) expr);

            WID(WI_GLOBAL_GET, globalidx);
            break;
        }

        case Ast_Kind_NumLit: {
            AstNumLit* lit = (AstNumLit *) expr;
            WasmType lit_type = onyx_type_to_wasm_type(lit->type);
            WasmInstruction instr = { WI_NOP, 0 };

            if (lit_type == WASM_TYPE_INT32) {
                instr.type = WI_I32_CONST;
                instr.data.i1 = lit->value.i;
            } else if (lit_type == WASM_TYPE_INT64) {
                instr.type = WI_I64_CONST;
                instr.data.l = lit->value.l;
            } else if (lit_type == WASM_TYPE_FLOAT32) {
                instr.type = WI_F32_CONST;
                instr.data.f = lit->value.f;
            } else if (lit_type == WASM_TYPE_FLOAT64) {
                instr.type = WI_F64_CONST;
                instr.data.d = lit->value.d;
            }

            bh_arr_push(code, instr);
            break;
        }

        case Ast_Kind_StrLit: {
            WID(WI_I32_CONST, ((AstStrLit *) expr)->addr);
            WID(WI_I32_CONST, ((AstStrLit *) expr)->length);
            break;
        }

        case Ast_Kind_Struct_Literal: {
            emit_struct_literal(mod, &code, (AstStructLiteral *) expr);
            break;
        }

        case Ast_Kind_Function: {
            i32 elemidx = get_element_idx(mod, (AstFunction *) expr);
            WID(WI_I32_CONST, elemidx);
            break;
        }

        case Ast_Kind_Block:          emit_block(mod, &code, (AstBlock *) expr, 1); break;
        case Ast_Kind_Call:           emit_call(mod, &code, (AstCall *) expr); break;
        case Ast_Kind_Intrinsic_Call: emit_intrinsic_call(mod, &code, (AstIntrinsicCall *) expr); break;
        case Ast_Kind_Binary_Op:      emit_binop(mod, &code, (AstBinaryOp *) expr); break;
        case Ast_Kind_Unary_Op:       emit_unaryop(mod, &code, (AstUnaryOp *) expr); break;

        case Ast_Kind_Address_Of: {
            AstAddressOf* aof = (AstAddressOf *) expr;
            emit_location(mod, &code, aof->expr);
            break;
        }

        case Ast_Kind_Dereference: {
            AstDereference* deref = (AstDereference *) expr;
            emit_expression(mod, &code, deref->expr);
            emit_load_instruction(mod, &code, deref->type, 0);
            break;
        }

        case Ast_Kind_Array_Access: {
            AstArrayAccess* aa = (AstArrayAccess *) expr;
            u64 offset = 0;
            emit_array_access_location(mod, &code, aa, &offset);
            emit_load_instruction(mod, &code, aa->type, offset);
            break;
        }

        case Ast_Kind_Field_Access: {
            AstFieldAccess* field = (AstFieldAccess* ) expr;

            // CLEANUP structs-by-value
            if (field->expr->kind == Ast_Kind_Param) {
                AstLocal* param = (AstLocal *) field->expr;

                if (type_get_param_pass(param->type) == Param_Pass_By_Value && !type_is_pointer(param->type)) {
                    u64 localidx = bh_imap_get(&mod->local_map, (u64) field->expr) + field->idx;
                    WIL(WI_LOCAL_GET, localidx);
                    break;
                }
            }


            // HACK
            else if (field->expr->kind == Ast_Kind_StrLit) {
                StructMember smem;

                token_toggle_end(field->token);
                type_lookup_member(field->expr->type, field->token->text, &smem);
                token_toggle_end(field->token);

                if (smem.idx == 0)
                    WID(WI_I32_CONST, ((AstStrLit *) field->expr)->addr);

                if (smem.idx == 1)
                    WID(WI_I32_CONST, ((AstStrLit *) field->expr)->length);

                break;
            }

            u64 offset = 0;
            emit_field_access_location(mod, &code, field, &offset);
            emit_load_instruction(mod, &code, field->type, offset);
            break;
        }

        case Ast_Kind_Slice: {
            AstArrayAccess* sl = (AstArrayAccess *) expr;

            emit_expression(mod, &code, sl->expr);

            u64 lo_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
            u64 hi_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);

            WI(WI_DROP);
            WIL(WI_LOCAL_SET, hi_local);
            WIL(WI_LOCAL_TEE, lo_local);
            if (sl->elem_size != 1) {
                WID(WI_I32_CONST, sl->elem_size);
                WI(WI_I32_MUL);
            }
            emit_expression(mod, &code, sl->addr);
            WI(WI_I32_ADD);
            WIL(WI_LOCAL_GET, hi_local);
            WIL(WI_LOCAL_GET, lo_local);
            WI(WI_I32_SUB);

            local_raw_free(mod->local_alloc, lo_local);
            local_raw_free(mod->local_alloc, hi_local);
            break;
        }

        case Ast_Kind_Size_Of: {
            AstSizeOf* so = (AstSizeOf *) expr;
            WID(WI_I32_CONST, so->size);
            break;
        }

        case Ast_Kind_Align_Of: {
            AstAlignOf* ao = (AstAlignOf *) expr;
            WID(WI_I32_CONST, ao->alignment);
            break;
        }

        case Ast_Kind_Enum_Value: {
            AstEnumValue* ev = (AstEnumValue *) expr;
            WasmType backing_type = onyx_type_to_wasm_type(ev->type);
            if (backing_type == WASM_TYPE_INT32) {
                WID(WI_I32_CONST, ev->value->value.i);
            }
            else if (backing_type == WASM_TYPE_INT64) {
                WID(WI_I64_CONST, ev->value->value.l);
            }
            else {
                onyx_report_error(ev->token->pos, "Invalid backing type for enum.");
            }
            break;
        }

        case Ast_Kind_Memres: {
            AstMemRes* memres = (AstMemRes *) expr;
            WID(WI_I32_CONST, memres->addr);
            emit_load_instruction(mod, &code, memres->type, 0);
            break;
        }

        case Ast_Kind_File_Contents: {
            AstFileContents* fc = (AstFileContents *) expr;

            WID(WI_I32_CONST, fc->addr);
            WID(WI_I32_CONST, fc->size);
            break;
        }

        default:
            bh_printf("Unhandled case: %d\n", expr->kind);
            DEBUG_HERE;
            assert(0);
    }

    if (expr->flags & Ast_Flag_Expr_Ignored &&
        !type_results_in_void(expr->type))
        WI(WI_DROP);

    *pcode = code;
}

static const WasmInstructionType cast_map[][11] = {
    //          I8              U8                  I16                 U16                I32                 U32                I64                U64                F32                F64                PTR
    /* I8  */ { WI_NOP,         WI_NOP,             WI_I32_EXTEND_8_S,  WI_NOP,            WI_I32_EXTEND_8_S,  WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE },
    /* U8  */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE },
    /* I16 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_I32_EXTEND_16_S, WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE },
    /* U16 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE },
    /* I32 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_F32_FROM_I32_S, WI_F64_FROM_I32_S, WI_NOP },
    /* U32 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_F32_FROM_I32_U, WI_F64_FROM_I32_U, WI_NOP },
    /* I64 */ { WI_NOP,         WI_I32_FROM_I64,    WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_S, WI_F64_FROM_I64_S, WI_I32_FROM_I64 },
    /* U64 */ { WI_NOP,         WI_I32_FROM_I64,    WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_U, WI_F64_FROM_I64_U, WI_I32_FROM_I64 },
    /* F32 */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_I32_FROM_F32_S,  WI_I32_FROM_F32_U, WI_I64_FROM_F32_S, WI_I64_FROM_F32_U, WI_NOP,            WI_F64_FROM_F32,   WI_UNREACHABLE },
    /* F64 */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_I32_FROM_F64_S,  WI_I32_FROM_F64_U, WI_I64_FROM_F64_S, WI_I64_FROM_F64_U, WI_F32_FROM_F64,   WI_NOP,            WI_UNREACHABLE },
    /* PTR */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_NOP },
};

EMIT_FUNC(cast, AstUnaryOp* cast) {
    bh_arr(WasmInstruction) code = *pcode;

    emit_expression(mod, &code, cast->expr);

    Type* from = cast->expr->type;
    Type* to = cast->type;

    if (from->kind == Type_Kind_Struct || to->kind == Type_Kind_Struct) {
        onyx_report_error(cast->token->pos, "Cannot cast to or from a struct.");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (from->kind == Type_Kind_Slice || to->kind == Type_Kind_Slice) {
        onyx_report_error(cast->token->pos, "Cannot cast to or from a slice.");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (from->kind == Type_Kind_DynArray || to->kind == Type_Kind_DynArray) {
        onyx_report_error(cast->token->pos, "Cannot cast to or from a dynamic array.");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Function) {
        onyx_report_error(cast->token->pos, "Cannot cast to a function.");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (type_is_simd(to) && !type_is_simd(from)) {
        onyx_report_error(cast->token->pos, "Can only perform a SIMD cast between SIMD types.");
        return;
    }

    if (type_is_simd(from) && !type_is_simd(to)) {
        onyx_report_error(cast->token->pos, "Can only perform a SIMD cast between SIMD types.");
        return;
    }

    if (type_is_simd(from) && type_is_simd(to)) {
        *pcode = code;
        return;
    }

    if (from->kind == Type_Kind_Enum) from = from->Enum.backing;
    if (to->kind == Type_Kind_Enum) to = to->Enum.backing;

    if (from->kind == Type_Kind_Basic && from->Basic.kind == Basic_Kind_Void) {
        onyx_report_error(cast->token->pos, "Cannot cast from void.");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Basic && to->Basic.kind == Basic_Kind_Void) {
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    i32 fromidx = -1, toidx = -1;
    if (from->Basic.flags & Basic_Flag_Pointer || from->kind == Type_Kind_Array) {
        fromidx = 10;
    }
    else if (from->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (from->Basic.flags & Basic_Flag_Unsigned) != 0;

        fromidx = log2_dumb(from->Basic.size) * 2 + unsign;
    }
    else if (from->Basic.flags & Basic_Flag_Float) {
        if      (from->Basic.size == 4) fromidx = 8;
        else if (from->Basic.size == 8) fromidx = 9;
    }

    if (to->Basic.flags & Basic_Flag_Pointer || to->kind == Type_Kind_Array) {
        toidx = 10;
    }
    else if (to->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (to->Basic.flags & Basic_Flag_Unsigned) != 0;

        toidx = log2_dumb(to->Basic.size) * 2 + unsign;
    }
    else if (to->Basic.flags & Basic_Flag_Float) {
        if      (to->Basic.size == 4) toidx = 8;
        else if (to->Basic.size == 8) toidx = 9;
    }

    if (fromidx != -1 && toidx != -1) {
        WasmInstructionType cast_op = cast_map[fromidx][toidx];
        if (cast_op == WI_UNREACHABLE) {
            bh_printf("%d %d\n", fromidx, toidx);
            onyx_report_error(cast->token->pos, "Bad cast.");
        }
        else if (cast_op != WI_NOP) {
            WI(cast_op);
        }
    }

    *pcode = code;
}

EMIT_FUNC(return, AstReturn* ret) {
    bh_arr(WasmInstruction) code = *pcode;

    if (ret->expr) {
        if (mod->curr_cc == CC_Return_Stack) {
            if (type_is_structlike_strict(ret->expr->type)) {
                emit_expression(mod, &code, ret->expr);

                WIL(WI_LOCAL_GET, mod->stack_base_idx);
                WID(WI_I32_CONST, type_size_of(ret->expr->type));
                WI(WI_I32_SUB);

                emit_store_instruction(mod, &code, ret->expr->type, 0);

            } else {
                WIL(WI_LOCAL_GET, mod->stack_base_idx);
                WID(WI_I32_CONST, type_size_of(ret->expr->type));
                WI(WI_I32_SUB);

                emit_expression(mod, &code, ret->expr);
                emit_store_instruction(mod, &code, ret->expr->type, 0);
            }

        } else {
            emit_expression(mod, &code, ret->expr);
        }
    }

    emit_deferred_stmts(mod, &code, (AstNode *) ret);

    if (bh_arr_length(mod->deferred_stmts) != 0) {
        i32 i = bh_arr_length(mod->deferred_stmts) - 1;
        while (i >= 0) {
            emit_statement(mod, &code, mod->deferred_stmts[i].stmt);
            i--;
        }
    }

    if (mod->has_stack_locals)
        emit_stack_leave(mod, &code, 0);

    WI(WI_RETURN);

    *pcode = code;
}

EMIT_FUNC(stack_enter, u64 stacksize) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_align(stacksize, 16);

    u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);

    // HACK: slightly... There will be space for 5 instructions
    code[0] = (WasmInstruction) { WI_GLOBAL_GET, { .l = stack_top_idx } };
    code[1] = (WasmInstruction) { WI_LOCAL_TEE,  { .l = mod->stack_base_idx} };
    code[2] = (WasmInstruction) { WI_I32_CONST,  { .l = stacksize } };
    code[3] = (WasmInstruction) { WI_I32_ADD,    0 };
    code[4] = (WasmInstruction) { WI_GLOBAL_SET, { .l = stack_top_idx } };

    *pcode = code;
}

EMIT_FUNC(stack_leave, u32 unused) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);

    WIL(WI_LOCAL_GET, mod->stack_base_idx);
    WID(WI_GLOBAL_SET, stack_top_idx);

    *pcode = code;
}

static i32 generate_type_idx(OnyxWasmModule* mod, Type* ft) {
    if (ft->kind != Type_Kind_Function) return -1;

    static char type_repr_buf[128];
    char* t = type_repr_buf;

    Type** param_type = ft->Function.params;
    i32 param_count = ft->Function.param_count;
    i32 params_left = param_count;

    while (params_left-- > 0) {
        if (type_get_param_pass(*param_type) == Param_Pass_By_Implicit_Pointer) {
            *(t++) = (char) onyx_type_to_wasm_type(&basic_types[Basic_Kind_Rawptr]);

        } else {
            if (type_is_structlike_strict(*param_type)) {
                u32 mem_count = type_structlike_mem_count(*param_type);
                StructMember smem;

                fori (i, 0, mem_count) {
                    type_lookup_member_by_idx(*param_type, i, &smem);
                    *(t++) = (char) onyx_type_to_wasm_type(smem.type);
                }

                param_count += mem_count - 1;

            } else {
                *(t++) = (char) onyx_type_to_wasm_type(*param_type);
            }
        }

        param_type++;
    }
    *(t++) = ':';

    // HACK: Slightly: the wasm type for structs has to be 0x00
    WasmType return_type = onyx_type_to_wasm_type(ft->Function.return_type);
    *(t++) = (char) return_type;
    *t = '\0';

    i32 type_idx = 0;
    if (bh_table_has(i32, mod->type_map, type_repr_buf)) {
        type_idx = bh_table_get(i32, mod->type_map, type_repr_buf);
    } else {
        // NOTE: Make a new type
        // TODO: Ensure that this isn't going to break things because of alignment
        WasmFuncType* type = (WasmFuncType*) bh_alloc(mod->allocator, sizeof(WasmFuncType) + sizeof(WasmType) * param_count);
        type->return_type = return_type;
        type->param_count = param_count;

        // HACK ish thing
        memcpy(type->param_types, type_repr_buf, type->param_count);

        bh_arr_push(mod->types, type);

        bh_table_put(i32, mod->type_map, type_repr_buf, mod->next_type_idx);
        type_idx = mod->next_type_idx;
        mod->next_type_idx++;
    }

    return type_idx;
}

static i32 get_element_idx(OnyxWasmModule* mod, AstFunction* func) {
    if (bh_imap_has(&mod->elem_map, (u64) func)) {
        return bh_imap_get(&mod->elem_map, (u64) func);
    } else {
        i32 idx = mod->next_elem_idx;
        bh_imap_put(&mod->elem_map, (u64) func, idx);

        i32 func_idx = bh_imap_get(&mod->index_map, (u64) func);
        bh_arr_push(mod->elems, func_idx);

        mod->next_elem_idx++;

        return idx;
    }
}

static inline b32 should_emit_function(AstFunction* fd) {
    // NOTE: Don't output intrinsic functions
    if (fd->flags & Ast_Flag_Intrinsic) return 0;

    // NOTE: Don't output functions that are not used, only if
    // they are also not exported.
    if ((fd->flags & Ast_Flag_Function_Used) == 0) {
        if (fd->flags & Ast_Flag_Exported) {
            return 1;
        } else {
            return 0;
        }
    }

    return 1;
}


static void emit_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_emit_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    WasmFunc wasm_func = {
        .type_idx = type_idx,
        .locals = {
            .param_count = 0,

            .allocated = { 0 },
            .freed     = { 0 },

            .max_stack = 0,
            .curr_stack = 0,
        },
        .code = NULL,
    };

    bh_arr_new(mod->allocator, wasm_func.code, 4);

    i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) fd);

    if (fd->flags & Ast_Flag_Exported) {
        token_toggle_end(fd->exported_name);

        WasmExport wasm_export = {
            .kind = WASM_FOREIGN_FUNCTION,
            .idx = func_idx,
        };
        bh_table_put(WasmExport, mod->exports, fd->exported_name->text, wasm_export);
        mod->export_count++;

        token_toggle_end(fd->exported_name);
    }

    // If there is no body then don't process the code
    if (fd->body != NULL) {
        // NOTE: Generate the local map
        u64 localidx = 0;
        bh_arr_each(AstParam, param, fd->params) {
            switch (type_get_param_pass(param->local->type)) {
                case Param_Pass_By_Value: {
                    if (type_is_structlike_strict(param->local->type)) {
                        bh_imap_put(&mod->local_map, (u64) param->local, localidx | LOCAL_IS_WASM);
                        localidx += type_structlike_mem_count(param->local->type);

                        break;
                    }
                    // fallthrough
                }

                case Param_Pass_By_Implicit_Pointer: {
                    bh_imap_put(&mod->local_map, (u64) param->local, localidx++ | LOCAL_IS_WASM);
                    break;
                }

                default: assert(0);
            }
        }

        mod->local_alloc = &wasm_func.locals;
        mod->local_alloc->param_count = localidx;

        mod->curr_cc = type_function_get_cc(fd->type);
        assert(mod->curr_cc != CC_Undefined);

        mod->has_stack_locals = (mod->curr_cc == CC_Return_Stack);
        bh_arr_each(AstLocal *, local, fd->locals)
            mod->has_stack_locals |= !local_is_wasm_local(*local);

        if (mod->has_stack_locals) {
            // NOTE: '5' needs to match the number of instructions it takes
            // to setup a stack frame
            bh_arr_insert_end(wasm_func.code, 5);
            mod->stack_base_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
        }

        // Generate code
        emit_function_body(mod, &wasm_func.code, fd);

        if (mod->has_stack_locals) {
            emit_stack_enter(mod, &wasm_func.code, mod->local_alloc->max_stack);
            emit_stack_leave(mod, &wasm_func.code, 0);
        }
    }

    bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));

    // HACK: This is gross
    bh_arr_grow(mod->funcs, func_idx - mod->foreign_function_count + 1);
    mod->funcs[func_idx - mod->foreign_function_count] = wasm_func;
    bh_arr_set_length(mod->funcs, bh_max(bh_arr_length(mod->funcs), func_idx - mod->foreign_function_count + 1));

    // NOTE: Clear the local map on exit of generating this function
    bh_imap_clear(&mod->local_map);
}

static void emit_foreign_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_emit_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    WasmImport import = {
        .kind = WASM_FOREIGN_FUNCTION,
        .idx  = type_idx,
        .mod  = fd->foreign_module,
        .name = fd->foreign_name,
    };

    bh_arr_push(mod->imports, import);
    return;
}

static void emit_global(OnyxWasmModule* module, AstGlobal* global) {
    WasmType global_type = onyx_type_to_wasm_type(global->type);

    WasmGlobal glob = {
        .type = global_type,
        .mutable = (global->flags & Ast_Flag_Const) == 0,
        .initial_value = NULL,
    };

    i32 global_idx = (i32) bh_imap_get(&module->index_map, (u64) global);

    if ((global->flags & Ast_Flag_Exported) != 0) {
        token_toggle_end(global->exported_name);

        WasmExport wasm_export = {
            .kind = WASM_FOREIGN_GLOBAL,
            .idx = global_idx,
        };
        bh_table_put(WasmExport, module->exports, global->exported_name->text, wasm_export);
        module->export_count++;

        token_toggle_end(global->exported_name);
    }

    bh_arr_new(global_heap_allocator, glob.initial_value, 1);

    switch (global_type) {
        case WASM_TYPE_INT32:   bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_I32_CONST, 0 })); break;
        case WASM_TYPE_INT64:   bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_I64_CONST, 0 })); break;
        case WASM_TYPE_FLOAT32: bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_F32_CONST, 0 })); break;
        case WASM_TYPE_FLOAT64: bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_F64_CONST, 0 })); break;

        default: assert(("Invalid global type", 0)); break;
    }

    bh_arr_grow(module->globals, global_idx - module->foreign_global_count + 1);
    module->globals[global_idx - module->foreign_global_count] = glob;
    bh_arr_set_length(module->globals, bh_max(bh_arr_length(module->globals), global_idx - module->foreign_global_count + 1));

    if (global->flags & Ast_Flag_Global_Stack_Top)
        module->stack_top_ptr = &module->globals[global_idx - module->foreign_global_count].initial_value[0].data.i1;
}

static void emit_foreign_global(OnyxWasmModule* module, AstGlobal* global) {
    WasmType global_type = onyx_type_to_wasm_type(global->type);

    if (global->flags & Ast_Flag_Foreign) {
        WasmImport import = {
            .kind = WASM_FOREIGN_GLOBAL,
            .idx  = global_type,
            .mod  = global->foreign_module,
            .name = global->foreign_name,
        };

        bh_arr_push(module->imports, import);
    }
}

static void emit_string_literal(OnyxWasmModule* mod, AstStrLit* strlit) {

    // NOTE: Allocating more than necessary, but there are no cases
    // in a string literal that create more bytes than already
    // existed. You can create less however ('\n' => 0x0a).
    i8* strdata = bh_alloc_array(global_heap_allocator, i8, strlit->token->length + 1);

    i8* src = (i8 *) strlit->token->text;
    i8* des = strdata;
    for (i32 i = 0, len = strlit->token->length; i < len; i++) {
        if (src[i] == '\\') {
            i++;
            switch (src[i]) {
            case '0': *des++ = '\0'; break;
            case 'a': *des++ = '\a'; break;
            case 'b': *des++ = '\b'; break;
            case 'f': *des++ = '\f'; break;
            case 'n': *des++ = '\n'; break;
            case 't': *des++ = '\t'; break;
            case 'r': *des++ = '\r'; break;
            case 'v': *des++ = '\v'; break;
            case 'e': *des++ = '\e'; break;
            case '"': *des++ = '"';  break;
            case '\\': *des++ = '\\'; break;
            case 'x': {
                // HACK: This whole way of doing this
                i++;
                u8 buf[3];
                buf[0] = src[i + 0];
                buf[1] = src[i + 1];
                buf[2] = 0;
                *des++ = strtol((const char *) buf, NULL, 16);
                i++;
                break;
            }
            default:  *des++ = '\\';
                      *des++ = src[i];
            }
        } else {
            *des++ = src[i];
        }
    }

    if (bh_table_has(StrLitInfo, mod->string_literals, (char *) strdata)) {
        StrLitInfo sti = bh_table_get(StrLitInfo, mod->string_literals, (char *) strdata);
        strlit->addr   = sti.addr;
        strlit->length = sti.len;
        return;
    }

    u32 length = (u32) (des - strdata);

    WasmDatum datum = {
        .offset = mod->next_datum_offset,
        .length = length,
        .data = strdata,
    };

    strlit->addr = (u32) mod->next_datum_offset,
    strlit->length = length;
    mod->next_datum_offset += length;

    bh_table_put(StrLitInfo, mod->string_literals, (char *) strdata, ((StrLitInfo) { strlit->addr, strlit->length }));

    bh_arr_push(mod->data, datum);
}

static void emit_raw_data(OnyxWasmModule* mod, ptr data, AstTyped* node) {
    switch (node->kind) {
    case Ast_Kind_StrLit: {
        AstStrLit* sl = (AstStrLit *) node;

        // NOTE: This assumes the address and the length fields have been filled out
        // by emit_string_literal.
        u32* sdata = (u32 *) data;
        sdata[0] = sl->addr;
        sdata[1] = sl->length;
        break;
    }
    case Ast_Kind_NumLit: {
        switch (node->type->Basic.kind) {
        case Basic_Kind_Bool:
        case Basic_Kind_I8:
        case Basic_Kind_U8:
        case Basic_Kind_I16:
        case Basic_Kind_U16:
        case Basic_Kind_I32:
        case Basic_Kind_U32:
        case Basic_Kind_Rawptr:
            *((i32 *) data) = ((AstNumLit *) node)->value.i;
            return;

        case Basic_Kind_I64:
        case Basic_Kind_U64:
            *((i64 *) data) = ((AstNumLit *) node)->value.l;
            return;

        case Basic_Kind_F32:
            *((f32 *) data) = ((AstNumLit *) node)->value.f;
            return;

        case Basic_Kind_F64:
            *((f64 *) data) = ((AstNumLit *) node)->value.d;
            return;

        default: break;
        }

        //fallthrough
    }
    default: onyx_report_error(node->token->pos,
            "Cannot generate constant data for '%s'.",
            onyx_ast_node_kind_string(node->kind));
    }
}

static void emit_memory_reservation(OnyxWasmModule* mod, AstMemRes* memres) {
    Type* effective_type = memres->type;

    u64 alignment = type_alignment_of(effective_type);
    u64 size = type_size_of(effective_type);

    u32 offset = mod->next_datum_offset;
    if (offset % alignment != 0) {
        offset += alignment - (offset % alignment);
    }

    if (memres->initial_value != NULL) {
        u8* data = bh_alloc(global_heap_allocator, size);
        emit_raw_data(mod, data, memres->initial_value);

        WasmDatum datum = {
            .offset = offset,
            .length = size,
            .data = data,
        };

        bh_arr_push(mod->data, datum);
    }

    memres->addr = offset;
    mod->next_datum_offset = offset + size;
}

static void emit_file_contents(OnyxWasmModule* mod, AstFileContents* fc) {
    token_toggle_end(fc->filename);

    if (bh_table_has(StrLitInfo, mod->loaded_file_info, fc->filename->text)) {
        StrLitInfo info = bh_table_get(StrLitInfo, mod->loaded_file_info, fc->filename->text);
        fc->addr = info.addr;
        fc->size = info.len;

        token_toggle_end(fc->filename);
        return;
    }

    u32 offset = mod->next_datum_offset;
    if (offset % 16 != 0)
        offset += 16 - (offset % 16);

    if (!bh_file_exists(fc->filename->text)) {
        onyx_report_error(fc->filename->pos,
                "Unable to open file for reading, '%s'.",
                fc->filename->text);

        token_toggle_end(fc->filename);
        return;
    }

    bh_file_contents contents = bh_file_read_contents(global_heap_allocator, fc->filename->text);
    u8* actual_data = bh_alloc(global_heap_allocator, contents.length + 1);
    u32 length = contents.length + 1;
    memcpy(actual_data, contents.data, contents.length);
    actual_data[contents.length] = 0;
    bh_file_contents_free(&contents);

    bh_table_put(StrLitInfo, mod->loaded_file_info, fc->filename->text, ((StrLitInfo) {
        .addr = offset,
        .len  = length - 1,
    }));

    fc->addr = offset;
    fc->size = length - 1;

    WasmDatum datum = {
        .offset = offset,
        .length = length,
        .data = actual_data,
    };

    bh_arr_push(mod->data, datum);

    mod->next_datum_offset = offset + length;

    token_toggle_end(fc->filename);
}

OnyxWasmModule onyx_wasm_module_create(bh_allocator alloc) {
    OnyxWasmModule module = {
        .allocator = alloc,

        .type_map = NULL,
        .next_type_idx = 0,
        .types = NULL,

        .funcs = NULL,
        .next_func_idx = 0,

        .exports = NULL,
        .export_count = 0,

        .imports = NULL,

        .globals = NULL,
        .next_global_idx = 0,

        .data = NULL,
        .next_datum_offset = 0,

        .elems = NULL,
        .next_elem_idx = 0,

        .structured_jump_target = NULL,

        .stack_top_ptr = NULL,
        .stack_base_idx = 0,

        .foreign_function_count = 0,
        .foreign_global_count = 0,
    };

    bh_arena* eid = bh_alloc(global_heap_allocator, sizeof(bh_arena));
    bh_arena_init(eid, global_heap_allocator, 8196);
    module.extended_instr_data = eid;
    module.extended_instr_alloc = bh_arena_allocator(eid);

    bh_arr_new(alloc, module.types, 4);
    bh_arr_new(alloc, module.funcs, 4);
    bh_arr_new(alloc, module.imports, 4);
    bh_arr_new(alloc, module.globals, 4);
    bh_arr_new(alloc, module.data, 4);
    bh_arr_new(alloc, module.elems, 4);

    // NOTE: 16 is probably needlessly large
    bh_arr_new(global_heap_allocator, module.structured_jump_target, 16);
    bh_arr_set_length(module.structured_jump_target, 0);

    bh_table_init(global_heap_allocator, module.type_map, 61);
    bh_table_init(global_heap_allocator, module.exports, 61);
    bh_table_init(global_heap_allocator, module.loaded_file_info, 7);
    bh_table_init(global_heap_allocator, module.string_literals, 16);

    bh_imap_init(&module.index_map, global_heap_allocator, 128);
    bh_imap_init(&module.local_map, global_heap_allocator, 16);
    bh_imap_init(&module.elem_map,  global_heap_allocator, 16);

    bh_arr_new(global_heap_allocator, module.deferred_stmts, 4);

    WasmExport mem_export = {
        .kind = WASM_FOREIGN_MEMORY,
        .idx = 0,
    };
    bh_table_put(WasmExport, module.exports, "memory", mem_export);
    module.export_count++;

    return module;
}

void emit_entity(Entity* ent) {
    OnyxWasmModule* module = &global_wasm_module;

    if (module->stack_top_ptr) {
        *module->stack_top_ptr = module->next_datum_offset;

        if (*module->stack_top_ptr % 16 != 0) {
            *module->stack_top_ptr += 16 - (*module->stack_top_ptr % 16);
        }

        builtin_heap_start.value.i = *module->stack_top_ptr + (1 << 16);
        if (builtin_heap_start.value.i % 16 != 0) {
            builtin_heap_start.value.i += 16 - (builtin_heap_start.value.i % 16);
        }
    }

    switch (ent->type) {
        case Entity_Type_Foreign_Function_Header:
            if (!should_emit_function(ent->function)) break;
            
            module->foreign_function_count++;
            emit_foreign_function(module, ent->function);
            // fallthrough

        case Entity_Type_Function_Header:
            if (!should_emit_function(ent->function)) break;

            bh_imap_put(&module->index_map, (u64) ent->function, module->next_func_idx++);
            break;

        case Entity_Type_Foreign_Global_Header:
            module->foreign_global_count++;
            emit_foreign_global(module, ent->global);
            // fallthrough

        case Entity_Type_Global_Header:
            bh_imap_put(&module->index_map, (u64) ent->global, module->next_global_idx++);
            break;

        case Entity_Type_String_Literal: {
            emit_string_literal(module, (AstStrLit *) ent->strlit);
            break;
        }

        case Entity_Type_File_Contents: {
            emit_file_contents(module, (AstFileContents *) ent->file_contents);
            break;
        }

        case Entity_Type_Memory_Reservation: {
            emit_memory_reservation(module, (AstMemRes *) ent->mem_res);
            break;
        }

        case Entity_Type_Function: emit_function(module, ent->function); break;
        case Entity_Type_Global:   emit_global(module,   ent->global); break;

        default: break;
    }

    ent->state = Entity_State_Finalized;
}

void onyx_wasm_module_free(OnyxWasmModule* module) {
    if (module->extended_instr_data != NULL)
        bh_arena_free(module->extended_instr_data);

    bh_arr_free(module->types);
    bh_arr_free(module->funcs);
    bh_imap_free(&module->local_map);
    bh_imap_free(&module->index_map);
    bh_table_free(module->type_map);
    bh_table_free(module->exports);
}




//-------------------------------------------------
// BINARY OUPUT
//-------------------------------------------------

#define WASM_SECTION_ID_TYPE 1
#define WASM_SECTION_ID_IMPORT 2
#define WASM_SECTION_ID_FUNCTION 3
#define WASM_SECTION_ID_TABLE 4
#define WASM_SECTION_ID_MEMORY 5
#define WASM_SECTION_ID_GLOBAL 6
#define WASM_SECTION_ID_EXPORT 7
#define WASM_SECTION_ID_START 8
#define WASM_SECTION_ID_ELEMENT 9
#define WASM_SECTION_ID_CODE 10
#define WASM_SECTION_ID_DATA 11

typedef i32 vector_func(void*, bh_buffer*);

static const u8 WASM_MAGIC_STRING[] = { 0x00, 0x61, 0x73, 0x6D };
static const u8 WASM_VERSION[] = { 0x01, 0x00, 0x00, 0x00 };

static void output_instruction(WasmFunc* func, WasmInstruction* instr, bh_buffer* buff);

static i32 output_vector(void** arr, i32 stride, i32 arrlen, vector_func elem, bh_buffer* vec_buff) {
    i32 len;
    u8* leb = uint_to_uleb128((u64) arrlen, &len);
    bh_buffer_append(vec_buff, leb, len);

    i32 i = 0;
    while (i < arrlen) {
        elem(*arr, vec_buff);
        arr = bh_pointer_add(arr, stride);
        i++;
    }

    return vec_buff->length;
}

static i32 output_name(const char* start, i32 length, bh_buffer* buff) {
    i32 leb_len, prev_len = buff->length;
    u8* leb = uint_to_uleb128((u64) length, &leb_len);
    bh_buffer_append(buff, leb, leb_len);
    bh_buffer_append(buff, start, length);
    return buff->length - prev_len;
}

static i32 output_limits(i32 min, i32 max, bh_buffer* buff) {
    i32 leb_len, prev_len = buff->length;
    u8* leb;

    bh_buffer_write_byte(buff, (max >= 0) ? 0x01 : 0x00);

    leb = uint_to_uleb128((u64) min, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    if (max >= 0) {
        leb = uint_to_uleb128((u64) max, &leb_len);
        bh_buffer_append(buff, leb, leb_len);
    }

    return buff->length - prev_len;
}

static i32 output_functype(WasmFuncType* type, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, 0x60);

    i32 len;
    u8* leb_buff = uint_to_uleb128(type->param_count, &len);
    bh_buffer_append(buff, leb_buff, len);
    bh_buffer_append(buff, type->param_types, type->param_count);

    if (type->return_type != WASM_TYPE_VOID) {
        bh_buffer_write_byte(buff, 0x01);
        bh_buffer_write_byte(buff, type->return_type);
    } else {
        bh_buffer_write_byte(buff, 0x00);
    }

    return buff->length - prev_len;
}

static i32 output_typesection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, 0x01);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 vec_len = output_vector(
            (void**) module->types,
            sizeof(WasmFuncType*),
            bh_arr_length(module->types),
            (vector_func *) output_functype,
            &vec_buff);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) vec_len, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_funcsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_FUNCTION);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->funcs)), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmFunc, func, module->funcs) {
        leb = uint_to_uleb128((u64) (func->type_idx), &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_tablesection(OnyxWasmModule* module, bh_buffer* buff) {
    if (bh_arr_length(module->elems) == 0) return 0;

    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_TABLE);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) 1, &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    // NOTE: funcrefs are the only valid table element type
    bh_buffer_write_byte(&vec_buff, 0x70);
    output_limits(bh_arr_length(module->elems), -1, &vec_buff);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_memorysection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_MEMORY);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) 1, &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    output_limits(256, -1, &vec_buff);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_globalsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_GLOBAL);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->globals)), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmGlobal, global, module->globals) {
        bh_buffer_write_byte(&vec_buff, global->type);
        bh_buffer_write_byte(&vec_buff, 0x01);

        bh_arr_each(WasmInstruction, instr, global->initial_value)
            output_instruction(NULL, instr, &vec_buff);

        // NOTE: Initial value expression terminator
        bh_buffer_write_byte(&vec_buff, (u8) WI_BLOCK_END);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_importsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_IMPORT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->imports)), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmImport, import, module->imports) {
        output_name(import->mod->text, import->mod->length, &vec_buff);
        output_name(import->name->text, import->name->length, &vec_buff);
        bh_buffer_write_byte(&vec_buff, (u8) import->kind);

        leb = uint_to_uleb128((u64) import->idx, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);

        if (import->kind == WASM_FOREIGN_GLOBAL) {
            // NOTE: All foreign globals are mutable
            bh_buffer_write_byte(&vec_buff, 0x01);
        }
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_exportsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_EXPORT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (module->export_count), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    i32 key_len = 0;
    bh_table_each_start(WasmExport, module->exports);
        key_len = strlen(key);
        output_name(key, key_len, &vec_buff);

        bh_buffer_write_byte(&vec_buff, (u8) (value.kind));
        leb = uint_to_uleb128((u64) value.idx, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
    bh_table_each_end;

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_startsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    i32 start_idx = -1;
    bh_table_each_start(WasmExport, module->exports) {
        if (value.kind == WASM_FOREIGN_FUNCTION) {
            if (strncmp("main", key, 5) == 0) {
                start_idx = value.idx;
                break;
            }
        }
    } bh_table_each_end;

    if (start_idx != -1) {
        bh_buffer_write_byte(buff, WASM_SECTION_ID_START);

        i32 start_leb_len, section_leb_len;
        uint_to_uleb128((u64) start_idx, &start_leb_len);
        u8* section_leb = uint_to_uleb128((u64) start_leb_len, &section_leb_len);
        bh_buffer_append(buff, section_leb, section_leb_len);

        u8* start_leb = uint_to_uleb128((u64) start_idx, &start_leb_len);
        bh_buffer_append(buff, start_leb, start_leb_len);
    }

    return buff->length - prev_len;
}

static i32 output_elemsection(OnyxWasmModule* module, bh_buffer* buff) {
    if (bh_arr_length(module->elems) == 0) return 0;

    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_ELEMENT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb;

    // NOTE: 0x01 count of elems
    bh_buffer_write_byte(&vec_buff, 0x01);

    // NOTE: 0x00 table index
    bh_buffer_write_byte(&vec_buff, 0x00);

    bh_buffer_write_byte(&vec_buff, WI_I32_CONST);
    bh_buffer_write_byte(&vec_buff, 0x00);
    bh_buffer_write_byte(&vec_buff, WI_BLOCK_END);

    leb = uint_to_uleb128((u64) bh_arr_length(module->elems), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(i32, elem, module->elems) {
        leb = uint_to_uleb128((u64) *elem, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_locals(WasmFunc* func, bh_buffer* buff) {
    i32 prev_len = buff->length;

    // NOTE: Output vector length
    i32 total_locals =
        (i32) (func->locals.allocated[0] != 0) +
        (i32) (func->locals.allocated[1] != 0) +
        (i32) (func->locals.allocated[2] != 0) +
        (i32) (func->locals.allocated[3] != 0) +
        (i32) (func->locals.allocated[4] != 0);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) total_locals, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    if (func->locals.allocated[0] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[0], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_INT32);
    }
    if (func->locals.allocated[1] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[1], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_INT64);
    }
    if (func->locals.allocated[2] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[2], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_FLOAT32);
    }
    if (func->locals.allocated[3] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[3], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_FLOAT64);
    }
    if (func->locals.allocated[4] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[4], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_VAR128);
    }

    return buff->length - prev_len;
}

static void output_instruction(WasmFunc* func, WasmInstruction* instr, bh_buffer* buff) {
    i32 leb_len;
    u8* leb;

    if (instr->type & SIMD_INSTR_MASK) {
        bh_buffer_write_byte(buff, 0xFD);
        leb = uint_to_uleb128((u64) (instr->type &~ SIMD_INSTR_MASK), &leb_len);
        bh_buffer_append(buff, leb, leb_len);

    } else {
        bh_buffer_write_byte(buff, (u8) instr->type);
    }

    switch (instr->type) {
        case WI_LOCAL_GET:
        case WI_LOCAL_SET:
        case WI_LOCAL_TEE: {
            u64 actual_idx = local_lookup_idx(&func->locals, instr->data.l);
            leb = uint_to_uleb128(actual_idx, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        }

        case WI_GLOBAL_GET:
        case WI_GLOBAL_SET:
        case WI_CALL:
        case WI_BLOCK_START:
        case WI_LOOP_START:
        case WI_JUMP:
        case WI_COND_JUMP:
        case WI_IF_START:
        case WI_MEMORY_SIZE:
        case WI_MEMORY_GROW:
            leb = uint_to_uleb128((u64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;

        case WI_JUMP_TABLE: {
            BranchTable* bt = (BranchTable *) instr->data.p;

            leb = uint_to_uleb128((u64) bt->count, &leb_len);
            bh_buffer_append(buff, leb, leb_len);

            fori (i, 0, bt->count) {
                leb = uint_to_uleb128((u64) bt->cases[i], &leb_len);
                bh_buffer_append(buff, leb, leb_len);
            }

            leb = uint_to_uleb128((u64) bt->default_case, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        }


        case WI_CALL_INDIRECT:
        case WI_I32_STORE: case WI_I32_STORE_8: case WI_I32_STORE_16:
        case WI_I64_STORE: case WI_I64_STORE_8: case WI_I64_STORE_16: case WI_I64_STORE_32:
        case WI_F32_STORE: case WI_F64_STORE:
        case WI_V128_STORE:
        case WI_I32_LOAD:
        case WI_I32_LOAD_8_S: case WI_I32_LOAD_8_U:
        case WI_I32_LOAD_16_S: case WI_I32_LOAD_16_U:
        case WI_I64_LOAD:
        case WI_I64_LOAD_8_S: case WI_I64_LOAD_8_U:
        case WI_I64_LOAD_16_S: case WI_I64_LOAD_16_U:
        case WI_I64_LOAD_32_S: case WI_I64_LOAD_32_U:
        case WI_F32_LOAD: case WI_F64_LOAD:
        case WI_V128_LOAD:
            leb = uint_to_uleb128((u64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            leb = uint_to_uleb128((u64) instr->data.i2, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;

        case WI_I32_CONST:
            leb = int_to_leb128((i64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        case WI_I64_CONST:
            leb = int_to_leb128((i64) instr->data.l, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        case WI_F32_CONST:
            leb = float_to_ieee754(instr->data.f, 0);
            bh_buffer_append(buff, leb, 4);
            break;
        case WI_F64_CONST:
            leb = double_to_ieee754(instr->data.d, 0);
            bh_buffer_append(buff, leb, 8);
            break;

        case WI_V128_CONST:
        case WI_I8X16_SHUFFLE:
            fori (i, 0, 16) bh_buffer_write_byte(buff, ((u8*) instr->data.p)[i]);
            break;

        case WI_I8X16_EXTRACT_LANE_S: case WI_I8X16_EXTRACT_LANE_U: case WI_I8X16_REPLACE_LANE:
        case WI_I16X8_EXTRACT_LANE_S: case WI_I16X8_EXTRACT_LANE_U: case WI_I16X8_REPLACE_LANE:
        case WI_I32X4_EXTRACT_LANE: case WI_I32X4_REPLACE_LANE:
        case WI_I64X2_EXTRACT_LANE: case WI_I64X2_REPLACE_LANE:
        case WI_F32X4_EXTRACT_LANE: case WI_F32X4_REPLACE_LANE:
        case WI_F64X2_EXTRACT_LANE: case WI_F64X2_REPLACE_LANE:
            bh_buffer_write_byte(buff, (u8) instr->data.i1);
            break;

        default: break;
    }
}

static i32 output_code(WasmFunc* func, bh_buffer* buff) {

    bh_buffer code_buff;
    bh_buffer_init(&code_buff, buff->allocator, 128);

    // Output locals
    output_locals(func, &code_buff);

    // Output code
    bh_arr_each(WasmInstruction, instr, func->code) output_instruction(func, instr, &code_buff);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) code_buff.length, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, code_buff);
    bh_buffer_free(&code_buff);

    return 0;
}

static i32 output_codesection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CODE);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) bh_arr_length(module->funcs), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    // DEBUG_HERE;

    bh_arr_each(WasmFunc, func, module->funcs) output_code(func, &vec_buff);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_datasection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_DATA);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) bh_arr_length(module->data), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmDatum, datum, module->data) {
        if (datum->data == NULL) continue;

        // NOTE: 0x00 memory index
        bh_buffer_write_byte(&vec_buff, 0x00);

        bh_buffer_write_byte(&vec_buff, WI_I32_CONST);
        leb = int_to_leb128((i64) datum->offset, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
        bh_buffer_write_byte(&vec_buff, WI_BLOCK_END);

        leb = uint_to_uleb128((u64) datum->length, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
        fori (i, 0, datum->length) bh_buffer_write_byte(&vec_buff, ((u8 *) datum->data)[i]);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

void onyx_wasm_module_write_to_file(OnyxWasmModule* module, bh_file file) {
    bh_buffer master_buffer;
    bh_buffer_init(&master_buffer, global_heap_allocator, 128);
    bh_buffer_append(&master_buffer, WASM_MAGIC_STRING, 4);
    bh_buffer_append(&master_buffer, WASM_VERSION, 4);

    output_typesection(module, &master_buffer);
    output_importsection(module, &master_buffer);
    output_funcsection(module, &master_buffer);
    output_tablesection(module, &master_buffer);
    output_memorysection(module, &master_buffer);
    output_globalsection(module, &master_buffer);
    output_exportsection(module, &master_buffer);
    output_startsection(module, &master_buffer);
    output_elemsection(module, &master_buffer);
    output_codesection(module, &master_buffer);
    output_datasection(module, &master_buffer);

    bh_file_write(&file, master_buffer.data, master_buffer.length);
}
