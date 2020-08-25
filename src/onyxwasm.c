#define BH_DEBUG
#include "onyxwasm.h"
#include "onyxutils.h"

// NOTE: Allows easier testing of types since most of the characters
// corresponding to these values are not printable
#if 1
#define WASM_TYPE_INT32   0x7F
#define WASM_TYPE_INT64   0x7E
#define WASM_TYPE_FLOAT32 0x7D
#define WASM_TYPE_FLOAT64 0x7C
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
        if (basic->size == 0) return WASM_TYPE_VOID;
    }

    return WASM_TYPE_VOID;
}

static i32 generate_type_idx(OnyxWasmModule* mod, Type* ft);
static i32 get_element_idx(OnyxWasmModule* mod, AstFunction* func);

#define LOCAL_I32 0x000000000
#define LOCAL_I64 0x100000000
#define LOCAL_F32 0x300000000
#define LOCAL_F64 0x700000000

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

    u64 flag_bits = LOCAL_IS_WASM;
    if (wt == WASM_TYPE_INT32)   flag_bits |= LOCAL_I32;
    if (wt == WASM_TYPE_INT64)   flag_bits |= LOCAL_I64;
    if (wt == WASM_TYPE_FLOAT32) flag_bits |= LOCAL_F32;
    if (wt == WASM_TYPE_FLOAT64) flag_bits |= LOCAL_F64;

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

    return (u64) idx;
}

#define WI(instr) bh_arr_push(code, ((WasmInstruction){ instr, 0x00 }))
#define WID(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, data }))
#define WIL(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, { .l = data } }))
#define COMPILE_FUNC(kind, ...) static void compile_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, __VA_ARGS__)

COMPILE_FUNC(function_body,                 AstFunction* fd);
COMPILE_FUNC(block,                         AstBlock* block, b32 generate_block_headers);
COMPILE_FUNC(statement,                     AstNode* stmt);
COMPILE_FUNC(assignment,                    AstBinaryOp* assign);
COMPILE_FUNC(store_instruction,             Type* type, u32 offset);
COMPILE_FUNC(load_instruction,              Type* type, u32 offset);
COMPILE_FUNC(if,                            AstIfWhile* if_node);
COMPILE_FUNC(while,                         AstIfWhile* while_node);
COMPILE_FUNC(for,                           AstFor* for_node);
COMPILE_FUNC(switch,                        AstSwitch* switch_node);
COMPILE_FUNC(defer,                         AstDefer* defer);
COMPILE_FUNC(deferred_stmts,                AstNode* node);
COMPILE_FUNC(binop,                         AstBinaryOp* binop);
COMPILE_FUNC(unaryop,                       AstUnaryOp* unop);
COMPILE_FUNC(call,                          AstCall* call);
COMPILE_FUNC(intrinsic_call,                AstIntrinsicCall* call);
COMPILE_FUNC(array_access_location,         AstArrayAccess* aa, u64* offset_return);
COMPILE_FUNC(field_access_location,         AstFieldAccess* field, u64* offset_return);
COMPILE_FUNC(local_location,                AstLocal* local, u64* offset_return);
COMPILE_FUNC(memory_reservation_location,   AstMemRes* memres);
COMPILE_FUNC(location,                      AstTyped* expr);
COMPILE_FUNC(struct_load,                   Type* type, u64 offset);
COMPILE_FUNC(struct_lval,                   AstTyped* lval);
COMPILE_FUNC(struct_store,                  Type* type, u64 offset);
COMPILE_FUNC(struct_literal,                AstStructLiteral* sl);
COMPILE_FUNC(expression,                    AstTyped* expr);
COMPILE_FUNC(cast,                          AstUnaryOp* cast);
COMPILE_FUNC(return,                        AstReturn* ret);
COMPILE_FUNC(stack_enter,                   u64 stacksize);
COMPILE_FUNC(stack_leave,                   u32 unused);

COMPILE_FUNC(function_body, AstFunction* fd) {
    if (fd->body == NULL) return;

    compile_block(mod, pcode, fd->body, 0);
}

COMPILE_FUNC(block, AstBlock* block, b32 generate_block_headers) {
    bh_arr(WasmInstruction) code = *pcode;

    if (generate_block_headers) {
        bh_arr_push(mod->structured_jump_target, 1);
        WID(WI_BLOCK_START, 0x40);
    }

    bh_arr_each(AstLocal *, local, block->locals)
        bh_imap_put(&mod->local_map, (u64) *local, local_allocate(mod->local_alloc, *local));

    forll (AstNode, stmt, block->body, next) {
        compile_statement(mod, &code, stmt);
    }

    compile_deferred_stmts(mod, &code, (AstNode *) block);

    bh_arr_each(AstLocal *, local, block->locals)
        local_free(mod->local_alloc, *local);

    if (generate_block_headers) {
        WI(WI_BLOCK_END);
        bh_arr_pop(mod->structured_jump_target);
    }

    *pcode = code;
}

COMPILE_FUNC(structured_jump, i32 jump_count, JumpType jump) {
    bh_arr(WasmInstruction) code = *pcode;

    static const u8 wants[Jump_Type_Count] = { 1, 2, 3 };

    i32 labelidx = 0;
    u8 wanted = wants[jump];
    b32 success = 0;

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
        assert(("Invalid structured jump", 0));
    }

    *pcode = code;
}

COMPILE_FUNC(statement, AstNode* stmt) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (stmt->kind) {
        case Ast_Kind_Return:     compile_return(mod, &code, (AstReturn *) stmt); break;
        case Ast_Kind_If:         compile_if(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_While:      compile_while(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_For:        compile_for(mod, &code, (AstFor *) stmt); break;
        case Ast_Kind_Switch:     compile_switch(mod, &code, (AstSwitch *) stmt); break;
        case Ast_Kind_Jump:       compile_structured_jump(mod, &code, ((AstJump *) stmt)->count, ((AstJump *) stmt)->jump); break;
        case Ast_Kind_Block:      compile_block(mod, &code, (AstBlock *) stmt, 1); break;
        case Ast_Kind_Defer:      compile_defer(mod, &code, (AstDefer *) stmt); break;
        default:                  compile_expression(mod, &code, (AstTyped *) stmt); break;
    }

    *pcode = code;
}

COMPILE_FUNC(assignment, AstBinaryOp* assign) {
    bh_arr(WasmInstruction) code = *pcode;

    if (assign->right->type->kind == Type_Kind_Struct) {
        compile_expression(mod, &code, assign->right);
        compile_struct_lval(mod, &code, assign->left);

        *pcode = code;
        return;
    }

    AstTyped* lval = assign->left;

    if (lval->kind == Ast_Kind_Local) {
        if (bh_imap_get(&mod->local_map, (u64) lval) & LOCAL_IS_WASM) {
            u64 localidx = bh_imap_get(&mod->local_map, (u64) lval);
            compile_expression(mod, &code, assign->right);
            WIL(WI_LOCAL_SET, localidx);

        } else {
            u64 offset = 0;
            compile_local_location(mod, &code, (AstLocal *) lval, &offset);
            compile_expression(mod, &code, assign->right);
            compile_store_instruction(mod, &code, lval->type, offset);
        }

    } else if (lval->kind == Ast_Kind_Global) {
        i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) lval);

        compile_expression(mod, &code, assign->right);
        WID(WI_GLOBAL_SET, globalidx);

    } else if (lval->kind == Ast_Kind_Dereference) {
        AstDereference* deref = (AstDereference *) lval;
        compile_expression(mod, &code, deref->expr);
        compile_expression(mod, &code, assign->right);

        compile_store_instruction(mod, &code, deref->type, 0);

    } else if (lval->kind == Ast_Kind_Array_Access) {
        AstArrayAccess* aa = (AstArrayAccess *) lval;

        u64 offset = 0;
        compile_array_access_location(mod, &code, aa, &offset);
        compile_expression(mod, &code, assign->right);

        compile_store_instruction(mod, &code, aa->type, offset);

    } else if (lval->kind == Ast_Kind_Field_Access) {
        AstFieldAccess* field = (AstFieldAccess *) lval;

        u64 offset = 0;
        compile_field_access_location(mod, &code, field, &offset);
        compile_expression(mod, &code, assign->right);

        compile_store_instruction(mod, &code, field->type, offset);

    } else if (lval->kind == Ast_Kind_Memres) {
        AstMemRes* memres = (AstMemRes *) lval;

        compile_memory_reservation_location(mod, &code, memres);
        compile_expression(mod, &code, assign->right);
        compile_store_instruction(mod, &code, memres->type, 0);

    } else {
        assert(("Invalid lval", 0));
    }

    *pcode = code;
}

COMPILE_FUNC(store_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type->kind == Type_Kind_Struct) {
        compile_struct_store(mod, pcode, type, offset);
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
    i32 is_integer  = is_basic && (type->Basic.flags & Basic_Flag_Integer);
    i32 is_float    = is_basic && (type->Basic.flags & Basic_Flag_Float);

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
    } else {
        onyx_message_add(Msg_Type_Failed_Gen_Store,
            (OnyxFilePos) { 0 },
            type_get_name(type));
    }

    *pcode = code;
}

COMPILE_FUNC(load_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type->kind == Type_Kind_Struct) {
        compile_struct_load(mod, pcode, type, offset);
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
    i32 is_integer  = is_basic && (type->Basic.flags & Basic_Flag_Integer);
    i32 is_float    = is_basic && (type->Basic.flags & Basic_Flag_Float);
    i32 is_unsigned = is_basic && (type->Basic.flags & Basic_Flag_Unsigned);

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

    WID(instr, ((WasmInstructionData) { alignment, offset }));

    if (instr == WI_NOP) {
        onyx_message_add(Msg_Type_Failed_Gen_Load,
                (OnyxFilePos) { 0 },
                type_get_name(type));
    }

    *pcode = code;
}

COMPILE_FUNC(if, AstIfWhile* if_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (if_node->assignment != NULL) {
        bh_imap_put(&mod->local_map, (u64) if_node->local, local_allocate(mod->local_alloc, if_node->local));

        compile_assignment(mod, &code, if_node->assignment);
    }

    compile_expression(mod, &code, if_node->cond);
    WID(WI_IF_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 0);
    if (if_node->true_stmt) compile_block(mod, &code, if_node->true_stmt, 0);

    if (if_node->false_stmt) {
        WI(WI_ELSE);

        if (if_node->false_stmt->kind == Ast_Kind_If) {
            compile_if(mod, &code, (AstIfWhile *) if_node->false_stmt);
        } else {
            compile_block(mod, &code, if_node->false_stmt, 0);
        }
    }

    bh_arr_pop(mod->structured_jump_target);

    if (if_node->assignment != NULL) {
        local_free(mod->local_alloc, if_node->local);
    }

    WI(WI_IF_END);

    *pcode = code;
}

COMPILE_FUNC(while, AstIfWhile* while_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (while_node->assignment != NULL) {
        bh_imap_put(&mod->local_map, (u64) while_node->local, local_allocate(mod->local_alloc, while_node->local));

        compile_assignment(mod, &code, while_node->assignment);
    }

    if (while_node->false_stmt == NULL) {
        WID(WI_BLOCK_START, 0x40);
        WID(WI_LOOP_START, 0x40);

        compile_expression(mod, &code, while_node->cond);
        WI(WI_I32_EQZ);
        WID(WI_COND_JUMP, 0x01);

        bh_arr_push(mod->structured_jump_target, 1);
        bh_arr_push(mod->structured_jump_target, 2);

        compile_block(mod, &code, while_node->true_stmt, 0);

        bh_arr_pop(mod->structured_jump_target);
        bh_arr_pop(mod->structured_jump_target);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, 0x00);

        WI(WI_LOOP_END);
        WI(WI_BLOCK_END);

    } else {
        compile_expression(mod, &code, while_node->cond);

        bh_arr_push(mod->structured_jump_target, 1);
        bh_arr_push(mod->structured_jump_target, 2);
        WID(WI_IF_START, 0x40);

        WID(WI_LOOP_START, 0x40);
        compile_block(mod, &code, while_node->true_stmt, 0);
        compile_expression(mod, &code, while_node->cond);
        WID(WI_COND_JUMP, 0x00);
        WI(WI_LOOP_END);

        WI(WI_ELSE);
        compile_block(mod, &code, while_node->false_stmt, 0);
        WID(WI_IF_END, 0x40);

        bh_arr_pop(mod->structured_jump_target);
        bh_arr_pop(mod->structured_jump_target);
    }

    if (while_node->assignment != NULL)
        local_free(mod->local_alloc, while_node->local);

    *pcode = code;
}

COMPILE_FUNC(for, AstFor* for_node) {
    bh_arr(WasmInstruction) code = *pcode;

    AstLocal* var = for_node->var;
    u64 tmp = local_allocate(mod->local_alloc, var);
    bh_imap_put(&mod->local_map, (u64) var, tmp);

    b32 it_is_local = (b32) ((tmp & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    WasmType var_type = onyx_type_to_wasm_type(for_node->var->type);
    assert(var_type == WASM_TYPE_INT32 || var_type == WASM_TYPE_INT64);
    WasmInstructionType add_instr = var_type == WASM_TYPE_INT32 ? WI_I32_ADD  : WI_I64_ADD;
    WasmInstructionType ge_instr  = var_type == WASM_TYPE_INT32 ? WI_I32_GE_S : WI_I64_GE_S;

    if (it_is_local) {
        compile_expression(mod, &code, for_node->start);
        WIL(WI_LOCAL_SET, tmp);
    } else {
        compile_local_location(mod, &code, var, &offset);
        compile_expression(mod, &code, for_node->start);
        compile_store_instruction(mod, &code, var->type, offset);
    }

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 2);

    if (it_is_local) {
        WIL(WI_LOCAL_GET, tmp);
    } else {
        offset = 0;
        compile_local_location(mod, &code, var, &offset);
        compile_load_instruction(mod, &code, var->type, offset);
    }
    compile_expression(mod, &code, for_node->end);
    WI(ge_instr);
    WID(WI_COND_JUMP, 0x01);

    compile_block(mod, &code, for_node->stmt, 0);

    if (it_is_local) {
        WIL(WI_LOCAL_GET, tmp);
        compile_expression(mod, &code, for_node->step);
        WI(add_instr);
        WIL(WI_LOCAL_SET, tmp);
    } else {
        offset = 0;
        compile_local_location(mod, &code, var, &offset);
        offset = 0;
        compile_local_location(mod, &code, var, &offset);
        compile_load_instruction(mod, &code, var->type, offset);
        compile_expression(mod, &code, for_node->step);
        WI(add_instr);
        compile_store_instruction(mod, &code, var->type, offset);
    }

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    local_free(mod->local_alloc, var);

    *pcode = code;
}

COMPILE_FUNC(switch, AstSwitch* switch_node) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_imap block_map;
    bh_imap_init(&block_map, global_heap_allocator, bh_arr_length(switch_node->cases));

    if (switch_node->assignment != NULL) {
        bh_imap_put(&mod->local_map, (u64) switch_node->local, local_allocate(mod->local_alloc, switch_node->local));

        compile_assignment(mod, &code, switch_node->assignment);
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
    BranchTable* bt = bh_alloc(global_heap_allocator, sizeof(BranchTable) + sizeof(u32) * count);
    bt->count = count;
    bt->default_case = block_num;
    fori (i, 0, bt->count) bt->cases[i] = bt->default_case;

    bh_arr_each(bh__imap_entry, sc, switch_node->case_map.entries) {
        bt->cases[sc->key - switch_node->min_case] = bh_imap_get(&block_map, (u64) sc->value);
    }

    WID(WI_BLOCK_START, 0x40);
    compile_expression(mod, &code, switch_node->expr);
    if (switch_node->min_case != 0) {
        WID(WI_I32_CONST, switch_node->min_case);
        WI(WI_I32_SUB);
    }
    WIL(WI_JUMP_TABLE, (u64) bt);
    WI(WI_BLOCK_END);

    bh_arr_each(AstSwitchCase, sc, switch_node->cases) {
        if (bh_imap_get(&block_map, (u64) sc->block) == 0xdeadbeef) continue;

        u64 bn = bh_imap_get(&block_map, (u64) sc->block);

        compile_block(mod, &code, sc->block, 0);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, block_num - bn);

        WI(WI_BLOCK_END);
        bh_arr_pop(mod->structured_jump_target);

        bh_imap_put(&block_map, (u64) sc->block, 0xdeadbeef);
    }

    if (switch_node->default_case != NULL) {
        compile_block(mod, &code, switch_node->default_case, 0);
    }

    WI(WI_BLOCK_END);
    bh_arr_pop(mod->structured_jump_target);

    if (switch_node->assignment != NULL)
        local_free(mod->local_alloc, switch_node->local);

    bh_imap_free(&block_map);
    *pcode = code;
}

COMPILE_FUNC(defer, AstDefer* defer) {
    bh_arr_push(mod->deferred_stmts, ((DeferredStmt) {
        .depth = bh_arr_length(mod->structured_jump_target),
        .stmt = defer->stmt,
    }));
}

COMPILE_FUNC(deferred_stmts, AstNode* node) {
    if (bh_arr_length(mod->deferred_stmts) == 0) return;

    bh_arr(WasmInstruction) code = *pcode;

    u64 depth = bh_arr_length(mod->structured_jump_target);

    while (bh_arr_last(mod->deferred_stmts).depth == depth) {
        compile_statement(mod, &code, bh_arr_last(mod->deferred_stmts).stmt);
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

COMPILE_FUNC(binop, AstBinaryOp* binop) {
    bh_arr(WasmInstruction) code = *pcode;

    if (binop_is_assignment(binop)) {
        compile_assignment(mod, &code, binop);
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

    compile_expression(mod, &code, binop->left);
    compile_expression(mod, &code, binop->right);

    WI(binop_instr);

    *pcode = code;
}

COMPILE_FUNC(unaryop, AstUnaryOp* unop) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (unop->operation) {
        case Unary_Op_Negate: {
            TypeBasic* type = &unop->type->Basic;

            if (type->kind == Basic_Kind_I32
                    || type->kind == Basic_Kind_I16
                    || type->kind == Basic_Kind_I8) {
                WID(WI_I32_CONST, 0x00);
                compile_expression(mod, &code, unop->expr);
                WI(WI_I32_SUB);

            }
            else if (type->kind == Basic_Kind_I64) {
                WID(WI_I64_CONST, 0x00);
                compile_expression(mod, &code, unop->expr);
                WI(WI_I64_SUB);

            }
            else {
                compile_expression(mod, &code, unop->expr);

                if (type->kind == Basic_Kind_F32)
                    WI(WI_F32_NEG);

                if (type->kind == Basic_Kind_F64)
                    WI(WI_F64_NEG);
            }

            break;
        }

        case Unary_Op_Not:
            compile_expression(mod, &code, unop->expr);

            WI(WI_I32_EQZ);
            break;

        case Unary_Op_Cast: compile_cast(mod, &code, unop); break;
    }

    *pcode = code;
}

COMPILE_FUNC(call, AstCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    for (AstArgument *arg = call->arguments;
            arg != NULL;
            arg = (AstArgument *) arg->next) {
        compile_expression(mod, &code, arg->value);
    }

    CallingConvention cc = type_function_get_cc(call->callee->type);
    assert(cc != CC_Undefined);

    Type* return_type = call->callee->type->Function.return_type;
    u32 return_size = type_size_of(return_type);
    u32 return_align = type_alignment_of(return_type);
    bh_align(return_size, return_align);

    u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);

    if (cc == CC_Return_Stack) {
        WID(WI_GLOBAL_GET, stack_top_idx);
        WID(WI_I32_CONST, return_size);
        WI(WI_I32_ADD);
        WID(WI_GLOBAL_SET, stack_top_idx);
    }

    if (call->callee->kind == Ast_Kind_Function) {
        i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) call->callee);
        bh_arr_push(code, ((WasmInstruction){ WI_CALL, func_idx }));

    } else {
        compile_expression(mod, &code, call->callee);

        i32 type_idx = generate_type_idx(mod, call->callee->type);
        WID(WI_CALL_INDIRECT, ((WasmInstructionData) { type_idx, 0x00 }));
    }

    if (cc == CC_Return_Stack) {
        WID(WI_GLOBAL_GET, stack_top_idx);
        WID(WI_I32_CONST, return_size);
        WI(WI_I32_SUB);
        WID(WI_GLOBAL_SET, stack_top_idx);

        WID(WI_GLOBAL_GET, stack_top_idx);
        compile_load_instruction(mod, &code, return_type, 0);
    }

    *pcode = code;
}

COMPILE_FUNC(intrinsic_call, AstIntrinsicCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    i32 place_arguments_normally = 1;

    // NOTE: Doing this in case there becomes intrinsics that the arguments
    // are not placed as they normally would be
    if (0) place_arguments_normally = 0;

    if (place_arguments_normally) {
        for (AstArgument *arg = call->arguments;
                arg != NULL;
                arg = (AstArgument *) arg->next) {
            compile_expression(mod, &code, arg->value);
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

        default: assert(("Unsupported intrinsic", 0));
    }

    *pcode = code;
}

COMPILE_FUNC(array_access_location, AstArrayAccess* aa, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    compile_expression(mod, &code, aa->expr);
    if (aa->elem_size != 1) {
        WID(WI_I32_CONST, aa->elem_size);
        WI(WI_I32_MUL);
    }

    u64 offset = 0;
    if (aa->addr->kind == Ast_Kind_Array_Access
        && aa->addr->type->kind == Type_Kind_Array) {
        compile_array_access_location(mod, &code, (AstArrayAccess *) aa->addr, &offset);
    } else if (aa->addr->kind == Ast_Kind_Field_Access
        && aa->addr->type->kind == Type_Kind_Array) {
        compile_field_access_location(mod, &code, (AstFieldAccess *) aa->addr, &offset);
    } else if (aa->addr->kind == Ast_Kind_Local
        && aa->addr->type->kind == Type_Kind_Array) {
        compile_local_location(mod, &code, (AstLocal *) aa->addr, &offset);
    } else if (aa->addr->kind == Ast_Kind_Memres
        && aa->addr->type->kind != Type_Kind_Array) {
        compile_memory_reservation_location(mod, &code, (AstMemRes *) aa->addr);
    } else {
        compile_expression(mod, &code, aa->addr);
    }
    WI(WI_I32_ADD);

    *offset_return += offset;

    *pcode = code;
}

COMPILE_FUNC(field_access_location, AstFieldAccess* field, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 offset = field->offset;
    AstTyped* source_expr = field->expr;
    while (source_expr->kind == Ast_Kind_Field_Access
            && (source_expr->type->kind == Type_Kind_Struct)) {
        offset += ((AstFieldAccess *) source_expr)->offset;
        source_expr = (AstTyped *) ((AstFieldAccess *) source_expr)->expr;
    }

    if (source_expr->kind == Ast_Kind_Array_Access
        && source_expr->type->kind != Type_Kind_Pointer) {
        u64 o2 = 0;
        compile_array_access_location(mod, &code, (AstArrayAccess *) source_expr, &o2);
        offset += o2;
    } else if (source_expr->kind == Ast_Kind_Local
        && source_expr->type->kind != Type_Kind_Pointer) {
        u64 o2 = 0;
        compile_local_location(mod, &code, (AstLocal *) source_expr, &o2);
        offset += o2;
    } else if (source_expr->kind == Ast_Kind_Memres
        && source_expr->type->kind != Type_Kind_Pointer) {
        compile_memory_reservation_location(mod, &code, (AstMemRes *) source_expr);
    } else {
        compile_expression(mod, &code, source_expr);
    }

    *offset_return = offset;

    *pcode = code;
}

COMPILE_FUNC(memory_reservation_location, AstMemRes* memres) {
    bh_arr(WasmInstruction) code = *pcode;

    WID(WI_I32_CONST, memres->addr);

    *pcode = code;
}

COMPILE_FUNC(local_location, AstLocal* local, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = (u64) bh_imap_get(&mod->local_map, (u64) local);

    if (local_offset & LOCAL_IS_WASM) {
        // HACK: This case doesn't feel quite right. I think the
        // only way to end up here is if you are taking the slice
        // off a pointer that is a local. But this still feels
        // like the wrong long term solution.
        WIL(WI_LOCAL_GET, local_offset);

    } else {
        WIL(WI_LOCAL_GET, mod->stack_base_idx);

        *offset_return += local_offset;
    }

    *pcode = code;
}

COMPILE_FUNC(struct_load, Type* type, u64 offset) {
    // NOTE: Expects the stack to look like:
    //      <location>

    bh_arr(WasmInstruction) code = *pcode;

    assert(type->kind == Type_Kind_Struct);

    if (type->Struct.mem_count == 1) {
        compile_load_instruction(mod, &code, type->Struct.memarr[0]->type, 0);
        *pcode = code;
        return;
    }

    u64 tmp_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WIL(WI_LOCAL_TEE, tmp_idx);

    b32 first = 1;
    bh_arr_each(StructMember *, smem, type->Struct.memarr) {
        if (first) first = 0;
        else       WIL(WI_LOCAL_GET, tmp_idx);
        compile_load_instruction(mod, &code, (*smem)->type, offset + (*smem)->offset);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);

    *pcode = code;
}

COMPILE_FUNC(struct_lval, AstTyped* lval) {
    // NOTE: Expects the stack to look like:
    //      mem_1
    //      mem_2
    //      ...
    //      mem_n

    bh_arr(WasmInstruction) code = *pcode;

    assert(lval->type->kind == Type_Kind_Struct);

    u64 offset = 0;

    switch (lval->kind) {
        case Ast_Kind_Local:        compile_local_location(mod, &code, (AstLocal *) lval, &offset); break;
        case Ast_Kind_Dereference:  compile_expression(mod, &code, ((AstDereference *) lval)->expr); break;
        case Ast_Kind_Array_Access: compile_array_access_location(mod, &code, (AstArrayAccess *) lval, &offset); break;
        case Ast_Kind_Field_Access: compile_field_access_location(mod, &code, (AstFieldAccess *) lval, &offset); break;
        case Ast_Kind_Memres:       compile_memory_reservation_location(mod, &code, (AstMemRes *) lval); break;

        default: assert(0);
    }

    compile_struct_store(mod, &code, lval->type, offset);

    *pcode = code;
}

COMPILE_FUNC(struct_store, Type* type, u64 offset) {
    // NOTE: Expects the stack to look like:
    //      mem_1
    //      mem_2
    //      ...
    //      mem_n
    //      loc

    bh_arr(WasmInstruction) code = *pcode;

    assert(type->kind == Type_Kind_Struct);

    u64 loc_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WIL(WI_LOCAL_SET, loc_idx);

    bh_arr_rev_each(StructMember *, smem, type->Struct.memarr) {
        if ((*smem)->type->kind == Type_Kind_Struct) {
            if (bh_arr_last(code).type == WI_LOCAL_SET && bh_arr_last(code).data.l == loc_idx) {
                bh_arr_last(code).type = WI_LOCAL_TEE;
            } else {
                WIL(WI_LOCAL_GET, loc_idx);
            }

            compile_struct_store(mod, &code, (*smem)->type, offset + (*smem)->offset);

        } else {
            WasmType wt = onyx_type_to_wasm_type((*smem)->type);
            u64 tmp_idx = local_raw_allocate(mod->local_alloc, wt);

            WIL(WI_LOCAL_SET, tmp_idx);
            WIL(WI_LOCAL_GET, loc_idx);
            WIL(WI_LOCAL_GET, tmp_idx);

            compile_store_instruction(mod, &code, (*smem)->type, offset + (*smem)->offset);

            local_raw_free(mod->local_alloc, wt);
        }
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);

    *pcode = code;
}

COMPILE_FUNC(struct_literal, AstStructLiteral* sl) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_arr_each(AstTyped *, val, sl->values) {
        compile_expression(mod, &code, *val);
    }

    *pcode = code;
}

COMPILE_FUNC(location, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (expr->kind) {
        case Ast_Kind_Local: {
            u64 offset = 0;
            compile_local_location(mod, &code, (AstLocal *) expr, &offset);
            if (offset != 0) {
                WID(WI_I32_CONST, offset);
                WI(WI_I32_ADD);
            }
            break;
        }

        case Ast_Kind_Dereference: {
            compile_expression(mod, &code, ((AstDereference *) expr)->expr);
            break;
        }

        case Ast_Kind_Array_Access: {
            AstArrayAccess* aa = (AstArrayAccess *) expr;
            u64 offset = 0;
            compile_array_access_location(mod, &code, aa, &offset);
            if (offset != 0) {
                WID(WI_I32_CONST, offset);
                WI(WI_I32_ADD);
            }
            break;
        }

        case Ast_Kind_Field_Access: {
            AstFieldAccess* field = (AstFieldAccess *) expr;

            if (field->expr->kind == Ast_Kind_Param && field->expr->type->kind == Type_Kind_Struct) {
                StructMember smem;

                token_toggle_end(field->token);
                type_struct_lookup_member(field->expr->type, field->token->text, &smem);
                token_toggle_end(field->token);

                u64 localidx = bh_imap_get(&mod->local_map, (u64) field->expr) + smem.idx;

                WIL(WI_LOCAL_GET, localidx);
                break;
            }

            u64 offset = 0;
            compile_field_access_location(mod, &code, field, &offset);
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
            onyx_message_add(Msg_Type_Literal,
                    (OnyxFilePos) { 0 },
                    "location unknown");
            break;
        }
    }

    *pcode = code;
}

COMPILE_FUNC(expression, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (expr->kind) {
        case Ast_Kind_Param: {
            u64 localidx = bh_imap_get(&mod->local_map, (u64) expr);

            if (expr->type->kind == Type_Kind_Struct) {
                TypeStruct* st = &expr->type->Struct;

                fori (idx, 0, st->mem_count) {
                    WIL(WI_LOCAL_GET, localidx + idx);
                }

            } else {
                WIL(WI_LOCAL_GET, localidx);
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
                compile_local_location(mod, &code, (AstLocal *) expr, &offset);

                if (expr->type->kind != Type_Kind_Array) {
                    compile_load_instruction(mod, &code, expr->type, offset);
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
            compile_struct_literal(mod, &code, (AstStructLiteral *) expr);
            break;
        }

        case Ast_Kind_Function: {
            i32 elemidx = get_element_idx(mod, (AstFunction *) expr);
            WID(WI_I32_CONST, elemidx);
            break;
        }

        case Ast_Kind_Block:          compile_block(mod, &code, (AstBlock *) expr, 1); break;
        case Ast_Kind_Call:           compile_call(mod, &code, (AstCall *) expr); break;
        case Ast_Kind_Intrinsic_Call: compile_intrinsic_call(mod, &code, (AstIntrinsicCall *) expr); break;
        case Ast_Kind_Binary_Op:      compile_binop(mod, &code, (AstBinaryOp *) expr); break;
        case Ast_Kind_Unary_Op:       compile_unaryop(mod, &code, (AstUnaryOp *) expr); break;

        case Ast_Kind_Address_Of: {
            AstAddressOf* aof = (AstAddressOf *) expr;
            compile_location(mod, &code, aof->expr);
            break;
        }

        case Ast_Kind_Dereference: {
            AstDereference* deref = (AstDereference *) expr;
            compile_expression(mod, &code, deref->expr);
            compile_load_instruction(mod, &code, deref->type, 0);
            break;
        }

        case Ast_Kind_Array_Access: {
            AstArrayAccess* aa = (AstArrayAccess *) expr;
            u64 offset = 0;
            compile_array_access_location(mod, &code, aa, &offset);
            compile_load_instruction(mod, &code, aa->type, offset);
            break;
        }

        case Ast_Kind_Field_Access: {
            AstFieldAccess* field = (AstFieldAccess* ) expr;

            if (field->expr->kind == Ast_Kind_Param && field->expr->type->kind == Type_Kind_Struct) {
                StructMember smem;

                token_toggle_end(field->token);
                type_struct_lookup_member(field->expr->type, field->token->text, &smem);
                token_toggle_end(field->token);

                u64 localidx = bh_imap_get(&mod->local_map, (u64) field->expr) + smem.idx;

                WIL(WI_LOCAL_GET, localidx);
                break;
            }

            if (field->expr->kind == Ast_Kind_StrLit) {
                StructMember smem;

                token_toggle_end(field->token);
                type_struct_lookup_member(field->expr->type, field->token->text, &smem);
                token_toggle_end(field->token);

                if (smem.idx == 0)
                    WID(WI_I32_CONST, ((AstStrLit *) field->expr)->addr);

                if (smem.idx == 1)
                    WID(WI_I32_CONST, ((AstStrLit *) field->expr)->length);
                
                break;
            }

            u64 offset = 0;
            compile_field_access_location(mod, &code, field, &offset);
            compile_load_instruction(mod, &code, field->type, offset);
            break;
        }

        case Ast_Kind_Slice: {
            AstSlice* sl = (AstSlice *) expr;

            u64 tmp_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);

            compile_expression(mod, &code, sl->lo);
            WIL(WI_LOCAL_TEE, tmp_local);
            if (sl->elem_size != 1) {
                WID(WI_I32_CONST, sl->elem_size);
                WI(WI_I32_MUL);
            }
            compile_location(mod, &code, sl->addr);
            WI(WI_I32_ADD);
            compile_expression(mod, &code, sl->hi);
            WIL(WI_LOCAL_GET, tmp_local);
            WI(WI_I32_SUB);

            local_raw_free(mod->local_alloc, tmp_local);
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
                onyx_message_add(Msg_Type_Literal,
                    ev->token->pos,
                    "invalid backing type for enum");
            }
            break;
        }

        case Ast_Kind_Memres: {
            AstMemRes* memres = (AstMemRes *) expr;
            WID(WI_I32_CONST, memres->addr);
            compile_load_instruction(mod, &code, memres->type, 0);
            break;
        }

        case Ast_Kind_File_Contents: {
            AstFileContents* fc = (AstFileContents *) expr;
            token_toggle_end(fc->filename);

            u32 offset = bh_table_get(u32, mod->loaded_file_offsets, fc->filename->text);
            WID(WI_I32_CONST, offset);

            token_toggle_end(fc->filename);
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

COMPILE_FUNC(cast, AstUnaryOp* cast) {
    bh_arr(WasmInstruction) code = *pcode;

    compile_expression(mod, &code, cast->expr);

    Type* from = cast->expr->type;
    Type* to = cast->type;

    if (from->kind == Type_Kind_Struct || to->kind == Type_Kind_Struct) {
        onyx_message_add(Msg_Type_Literal,
                cast->token->pos,
                "cannot cast to or from a struct");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Function) {
        onyx_message_add(Msg_Type_Literal,
                cast->token->pos,
                "cannot cast to a function");
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (from->kind == Type_Kind_Enum) from = from->Enum.backing;
    if (to->kind == Type_Kind_Enum) to = to->Enum.backing;

    if (from->kind == Type_Kind_Basic && from->Basic.kind == Basic_Kind_Void) {
        onyx_message_add(Msg_Type_Literal,
                cast->token->pos,
                "cannot cast from void");
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
            onyx_message_add(Msg_Type_Literal,
                    cast->token->pos,
                    "bad cast");
        }
        else if (cast_op != WI_NOP) {
            WI(cast_op);
        }
    }

    *pcode = code;
}

COMPILE_FUNC(return, AstReturn* ret) {
    bh_arr(WasmInstruction) code = *pcode;

    if (ret->expr) {
        if (mod->curr_cc == CC_Return_Stack) {
            if (ret->expr->type->kind == Type_Kind_Struct) {
                compile_expression(mod, &code, ret->expr);

                WIL(WI_LOCAL_GET, mod->stack_base_idx);
                WID(WI_I32_CONST, type_size_of(ret->expr->type));
                WI(WI_I32_SUB);

                compile_store_instruction(mod, &code, ret->expr->type, 0);

            } else {
                WIL(WI_LOCAL_GET, mod->stack_base_idx);
                WID(WI_I32_CONST, type_size_of(ret->expr->type));
                WI(WI_I32_SUB);

                compile_expression(mod, &code, ret->expr);
                compile_store_instruction(mod, &code, ret->expr->type, 0);
            }

        } else {
            compile_expression(mod, &code, ret->expr);
        }
    }

    compile_deferred_stmts(mod, &code, (AstNode *) ret);

    if (bh_arr_length(mod->deferred_stmts) != 0) {
        i32 i = bh_arr_length(mod->deferred_stmts) - 1;
        while (i >= 0) {
            compile_statement(mod, &code, mod->deferred_stmts[i].stmt);
            i--;
        }
    }

    if (mod->has_stack_locals)
        compile_stack_leave(mod, &code, 0);

    WI(WI_RETURN);

    *pcode = code;
}

COMPILE_FUNC(stack_enter, u64 stacksize) {
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

COMPILE_FUNC(stack_leave, u32 unused) {
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
        if ((*param_type)->kind == Type_Kind_Struct) {
            bh_arr_each(StructMember *, smem, (*param_type)->Struct.memarr) {
                *(t++) = (char) onyx_type_to_wasm_type((*smem)->type);
            }

            param_count += (*param_type)->Struct.mem_count - 1;

        } else {
            *(t++) = (char) onyx_type_to_wasm_type(*param_type);
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

static inline b32 should_compile_function(AstFunction* fd) {
    // NOTE: Don't output intrinsic functions
    if (fd->flags & Ast_Flag_Intrinsic) return 0;

    if (fd->flags & Ast_Flag_Foreign) return 1;

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


static void compile_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_compile_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    if (fd->flags & Ast_Flag_Foreign) {
        WasmImport import = {
            .kind = WASM_FOREIGN_FUNCTION,
            .idx  = type_idx,
            .mod  = fd->foreign_module,
            .name = fd->foreign_name,
        };

        bh_arr_push(mod->imports, import);
        return;
    }

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

    if (fd->flags & Ast_Flag_Exported) {
        token_toggle_end(fd->exported_name);

        i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) fd);

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
        for (AstLocal *param = fd->params; param != NULL; param = (AstLocal *) param->next) {
            if (param->type->kind == Type_Kind_Struct) {
                bh_imap_put(&mod->local_map, (u64) param, localidx | LOCAL_IS_WASM);
                localidx += param->type->Struct.mem_count;

            } else {
                bh_imap_put(&mod->local_map, (u64) param, localidx++ | LOCAL_IS_WASM);
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
        compile_function_body(mod, &wasm_func.code, fd);

        if (mod->has_stack_locals) {
            compile_stack_enter(mod, &wasm_func.code, mod->local_alloc->max_stack);
            compile_stack_leave(mod, &wasm_func.code, 0);
        }
    }

    bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));

    bh_arr_push(mod->funcs, wasm_func);

    // NOTE: Clear the local map on exit of generating this function
    bh_imap_clear(&mod->local_map);
}

static void compile_global(OnyxWasmModule* module, AstGlobal* global) {
    WasmType global_type = onyx_type_to_wasm_type(global->type);

    if (global->flags & Ast_Flag_Foreign) {
        WasmImport import = {
            .kind = WASM_FOREIGN_GLOBAL,
            .idx  = global_type,
            .mod  = global->foreign_module,
            .name = global->foreign_name,
        };

        bh_arr_push(module->imports, import);
        return;
    }

    WasmGlobal glob = {
        .type = global_type,
        .mutable = (global->flags & Ast_Flag_Const) == 0,
        .initial_value = NULL,
    };

    if ((global->flags & Ast_Flag_Exported) != 0) {
        token_toggle_end(global->exported_name);

        i32 global_idx = (i32) bh_imap_get(&module->index_map, (u64) global);

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

    bh_arr_push(module->globals, glob);

    if (global->flags & Ast_Flag_Global_Stack_Top)
        module->stack_top_ptr = &bh_arr_last(module->globals).initial_value[0].data.i1;

}

static void compile_string_literal(OnyxWasmModule* mod, AstStrLit* strlit) {

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

static void compile_raw_data(OnyxWasmModule* mod, ptr data, AstTyped* node) {
    switch (node->kind) {
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
    default: onyx_message_add(Msg_Type_Literal,
            node->token->pos,
            "invalid data");
    }
}

static void compile_memory_reservation(OnyxWasmModule* mod, AstMemRes* memres) {
    Type* effective_type = memres->type;

    u64 alignment = type_alignment_of(effective_type);
    u64 size = type_size_of(effective_type);

    u32 offset = mod->next_datum_offset;
    if (offset % alignment != 0) {
        offset += alignment - (offset % alignment);
    }

    if (memres->initial_value != NULL) {
        u8* data = bh_alloc(global_heap_allocator, size);
        compile_raw_data(mod, data, memres->initial_value);

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

static void compile_file_contents(OnyxWasmModule* mod, AstFileContents* fc) {
    token_toggle_end(fc->filename);

    if (bh_table_has(u32, mod->loaded_file_offsets, fc->filename->text)) {
        token_toggle_end(fc->filename);
        return;
    }

    u32 offset = mod->next_datum_offset;
    if (offset % 16 != 0)
        offset += 16 - (offset % 16);
    bh_table_put(u32, mod->loaded_file_offsets, fc->filename->text, offset);

    if (!bh_file_exists(fc->filename->text)) {
        onyx_message_add(Msg_Type_File_Not_Found,
                fc->filename->pos,
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
        .next_foreign_func_idx = 0,

        .exports = NULL,
        .export_count = 0,

        .imports = NULL,

        .globals = NULL,
        .next_global_idx = 0,
        .next_foreign_global_idx = 0,

        .data = NULL,
        .next_datum_offset = 0,

        .elems = NULL,
        .next_elem_idx = 0,

        .structured_jump_target = NULL,

        .stack_top_ptr = NULL,
        .stack_base_idx = 0,
    };

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
    bh_table_init(global_heap_allocator, module.loaded_file_offsets, 7);
    bh_table_init(global_heap_allocator, module.string_literals, 16);

    bh_imap_init(&module.index_map, global_heap_allocator, 128);
    bh_imap_init(&module.local_map, global_heap_allocator, 16);
    bh_imap_init(&module.elem_map,  global_heap_allocator, 16);

    bh_arr_new(global_heap_allocator, module.deferred_stmts, 4);

    return module;
}

void onyx_wasm_module_compile(OnyxWasmModule* module, ProgramInfo* program) {
    module->next_func_idx   = program->foreign_func_count;
    module->next_global_idx = program->foreign_global_count;

    WasmExport mem_export = {
        .kind = WASM_FOREIGN_MEMORY,
        .idx = 0,
    };
    bh_table_put(WasmExport, module->exports, "memory", mem_export);
    module->export_count++;

    bh_arr_each(Entity, entity, program->entities) {

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

        switch (entity->type) {
            case Entity_Type_Function_Header: {
                if (!should_compile_function(entity->function)) break;

                u64 func_idx;
                if ((entity->function->flags & Ast_Flag_Foreign) != 0)
                    func_idx = module->next_foreign_func_idx++;
                else
                    func_idx = module->next_func_idx++;

                bh_imap_put(&module->index_map, (u64) entity->function, func_idx);
                break;
            }

            case Entity_Type_Global_Header: {
                u64 global_idx;
                if ((entity->global->flags & Ast_Flag_Foreign) != 0)
                    global_idx = module->next_foreign_global_idx++;
                else
                    global_idx = module->next_global_idx++;

                bh_imap_put(&module->index_map, (u64) entity->global, global_idx);
                break;
            }

            case Entity_Type_String_Literal: {
                compile_string_literal(module, (AstStrLit *) entity->strlit);
                break;
            }

            case Entity_Type_File_Contents: {
                compile_file_contents(module, (AstFileContents *) entity->file_contents);
                break;
            }

            case Entity_Type_Memory_Reservation: {
                compile_memory_reservation(module, (AstMemRes *) entity->mem_res);
                break;
            }

            case Entity_Type_Function: compile_function(module, entity->function); break;
            case Entity_Type_Global:   compile_global(module,   entity->global); break;

            default: break;
        }
    }
}

void onyx_wasm_module_free(OnyxWasmModule* module) {
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
        (i32) (func->locals.allocated[3] != 0);

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

    return buff->length - prev_len;
}

static void output_instruction(WasmFunc* func, WasmInstruction* instr, bh_buffer* buff) {
    i32 leb_len;
    u8* leb;

    bh_buffer_write_byte(buff, (u8) instr->type);

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
        case WI_I32_STORE:
        case WI_I32_STORE_8:
        case WI_I32_STORE_16:
        case WI_I64_STORE:
        case WI_I64_STORE_8:
        case WI_I64_STORE_16:
        case WI_I64_STORE_32:
        case WI_F32_STORE:
        case WI_F64_STORE:
        case WI_I32_LOAD:
        case WI_I32_LOAD_8_S:
        case WI_I32_LOAD_8_U:
        case WI_I32_LOAD_16_S:
        case WI_I32_LOAD_16_U:
        case WI_I64_LOAD:
        case WI_I64_LOAD_8_S:
        case WI_I64_LOAD_8_U:
        case WI_I64_LOAD_16_S:
        case WI_I64_LOAD_16_U:
        case WI_I64_LOAD_32_S:
        case WI_I64_LOAD_32_U:
        case WI_F32_LOAD:
        case WI_F64_LOAD:
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
