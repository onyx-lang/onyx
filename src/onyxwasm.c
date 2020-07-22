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
    if (type->kind == Type_Kind_Pointer) {
        return WASM_TYPE_INT32;
    }

    if (type->kind == Type_Kind_Array) {
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

#define WI(instr) bh_arr_push(code, ((WasmInstruction){ instr, 0x00 }))
#define WID(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, data }))
#define COMPILE_FUNC(kind, ...) static void compile_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, __VA_ARGS__)

COMPILE_FUNC(function_body,         AstFunction* fd);
COMPILE_FUNC(block,                 AstBlock* block);
COMPILE_FUNC(statement,             AstNode* stmt);
COMPILE_FUNC(assignment,            AstBinaryOp* assign);
COMPILE_FUNC(store_instruction,     Type* type, u32 alignment, u32 offset);
COMPILE_FUNC(load_instruction,      Type* type, u32 offset);
COMPILE_FUNC(if,                    AstIf* if_node);
COMPILE_FUNC(while,                 AstWhile* while_node);
COMPILE_FUNC(for,                   AstFor* for_node);
COMPILE_FUNC(binop,                 AstBinaryOp* binop);
COMPILE_FUNC(unaryop,               AstUnaryOp* unop);
COMPILE_FUNC(call,                  AstCall* call);
COMPILE_FUNC(intrinsic_call,        AstIntrinsicCall* call);
COMPILE_FUNC(array_access_location, AstArrayAccess* aa, u64* offset_return);
COMPILE_FUNC(field_access_location, AstFieldAccess* field, u64* offset_return);
COMPILE_FUNC(expression,            AstTyped* expr);
COMPILE_FUNC(cast,                  AstUnaryOp* cast);
COMPILE_FUNC(return,                AstReturn* ret);

COMPILE_FUNC(function_body, AstFunction* fd) {
    if (fd->body == NULL) return;

    bh_arr(WasmInstruction) code = *pcode;

    forll (AstNode, stmt, fd->body->body, next) {
        compile_statement(mod, &code, stmt);
    }

    WI(WI_BLOCK_END);

    *pcode = code;
}

COMPILE_FUNC(block, AstBlock* block) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_arr_push(mod->structured_jump_target, 1);
    WID(WI_BLOCK_START, 0x40);

    forll (AstNode, stmt, block->body, next) {
        compile_statement(mod, &code, stmt);
    }

    WI(WI_BLOCK_END);
    bh_arr_pop(mod->structured_jump_target);

    *pcode = code;
}

COMPILE_FUNC(structured_jump, i32 jump_count) {
    bh_arr(WasmInstruction) code = *pcode;

    i32 labelidx = 0;
    u8 wanted = (jump_count < 0) ? 2 : 1;
    b32 success = 0;

    if (jump_count < 0) jump_count = -jump_count;

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
        case Ast_Kind_If:         compile_if(mod, &code, (AstIf *) stmt); break;
        case Ast_Kind_While:      compile_while(mod, &code, (AstWhile *) stmt); break;
        case Ast_Kind_For:        compile_for(mod, &code, (AstFor *) stmt); break;
        case Ast_Kind_Break:      compile_structured_jump(mod, &code, ((AstBreak *) stmt)->count); break;
        case Ast_Kind_Continue:   compile_structured_jump(mod, &code, -((AstContinue *) stmt)->count); break;
        case Ast_Kind_Block:      compile_block(mod, &code, (AstBlock *) stmt); break;
        default:                  compile_expression(mod, &code, (AstTyped *) stmt); break;
    }

    *pcode = code;
}

COMPILE_FUNC(assignment, AstBinaryOp* assign) {
    bh_arr(WasmInstruction) code = *pcode;

    AstTyped* lval = assign->left;

    if (lval->kind == Ast_Kind_Local) {
        i32 localidx = (i32) bh_imap_get(&mod->local_map, (u64) lval);

        compile_expression(mod, &code, assign->right);
        WID(WI_LOCAL_SET, localidx);

    } else if (lval->kind == Ast_Kind_Global) {
        i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) lval);

        compile_expression(mod, &code, assign->right);
        WID(WI_GLOBAL_SET, globalidx);

    } else if (lval->kind == Ast_Kind_Dereference) {
        AstDereference* deref = (AstDereference *) lval;
        compile_expression(mod, &code, deref->expr);
        compile_expression(mod, &code, assign->right);

        compile_store_instruction(mod, &code,
                deref->type,
                type_get_alignment_log2(deref->type),
                0);

    } else if (lval->kind == Ast_Kind_Array_Access) {
        AstArrayAccess* aa = (AstArrayAccess *) lval;

        u64 offset = 0;
        compile_array_access_location(mod, &code, aa, &offset);
        compile_expression(mod, &code, assign->right);

        compile_store_instruction(mod, &code,
                aa->type,
                type_get_alignment_log2(aa->type),
                offset);

    } else if (lval->kind == Ast_Kind_Field_Access) {
        AstFieldAccess* field = (AstFieldAccess *) lval;

        u64 offset = 0;
        compile_field_access_location(mod, &code, field, &offset);
        compile_expression(mod, &code, assign->right);

        compile_store_instruction(mod, &code,
                field->type,
                type_get_alignment_log2(field->type),
                offset);
    } else {
        assert(("Invalid lval", 0));
    }

    *pcode = code;
}

COMPILE_FUNC(store_instruction, Type* type, u32 alignment, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    i32 store_size = type_size_of(type);
    i32 is_basic   = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer;
    i32 is_pointer  = is_basic && (type->Basic.flags & Basic_Flag_Pointer);
    i32 is_integer  = is_basic && (type->Basic.flags & Basic_Flag_Integer);
    i32 is_float    = is_basic && type->Basic.flags & Basic_Flag_Float;

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

    i32 load_size   = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer;
    i32 is_pointer  = is_basic && (type->Basic.flags & Basic_Flag_Pointer);
    i32 is_integer  = is_basic && (type->Basic.flags & Basic_Flag_Integer);
    i32 is_float    = is_basic && type->Basic.flags & Basic_Flag_Float;
    i32 is_unsigned = is_basic && type->Basic.flags & Basic_Flag_Unsigned;

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

COMPILE_FUNC(if, AstIf* if_node) {
    bh_arr(WasmInstruction) code = *pcode;

    compile_expression(mod, &code, if_node->cond);
    WID(WI_IF_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 0);

    if (if_node->true_stmt) {
        if (if_node->true_stmt->kind == Ast_Kind_Block) {
            forll (AstNode, stmt, ((AstBlock *) if_node->true_stmt)->body, next) {
                compile_statement(mod, &code, stmt);
            }
        } else {
            compile_statement(mod, &code, if_node->true_stmt);
        }
    }

    if (if_node->false_stmt) {
        WI(WI_ELSE);

        if (if_node->false_stmt->kind == Ast_Kind_Block) {
            forll (AstNode, stmt, ((AstBlock *) if_node->false_stmt)->body, next) {
                compile_statement(mod, &code, stmt);
            }
        } else {
            compile_statement(mod, &code, if_node->false_stmt);
        }
    }

    bh_arr_pop(mod->structured_jump_target);

    WI(WI_IF_END);

    *pcode = code;
}

COMPILE_FUNC(while, AstWhile* while_node) {
    bh_arr(WasmInstruction) code = *pcode;

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);

    compile_expression(mod, &code, while_node->cond);
    WI(WI_I32_EQZ);
    WID(WI_COND_JUMP, 0x01);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 2);

    if (while_node->stmt->kind == Ast_Kind_Block) {
        forll (AstNode, stmt, ((AstBlock *) while_node->stmt)->body, next) {
            compile_statement(mod, &code, stmt);
        }
    } else {
        compile_statement(mod, &code, while_node->stmt);
    }

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    *pcode = code;
}

COMPILE_FUNC(for, AstFor* for_node) {
    bh_arr(WasmInstruction) code = *pcode;

    i32 it_idx = (i32) bh_imap_get(&mod->local_map, (u64) for_node->var);

    compile_expression(mod, &code, for_node->start);
    WID(WI_LOCAL_SET, it_idx);

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 2);

    WID(WI_LOCAL_GET, it_idx);
    compile_expression(mod, &code, for_node->end);
    WI(WI_I32_GE_S);
    WID(WI_COND_JUMP, 0x01);

    if (for_node->stmt->kind == Ast_Kind_Block) {
        forll (AstNode, stmt, ((AstBlock *) for_node->stmt)->body, next) {
            compile_statement(mod, &code, stmt);
        }
    } else {
        compile_statement(mod, &code, for_node->stmt);
    }

    if (for_node->step == NULL)
        WID(WI_I32_CONST, 0x01);
    else
        compile_expression(mod, &code, for_node->step);
    WID(WI_LOCAL_GET, it_idx);
    WI(WI_I32_ADD);
    WID(WI_LOCAL_SET, it_idx);

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    *pcode = code;
}

// NOTE: These need to be in the same order as
// the OnyxBinaryOp enum
static const WasmInstructionType binop_map[][4] = {
    //          I32           I64           F32         F64
    /* ADD */ { WI_I32_ADD,   WI_I64_ADD,   WI_F32_ADD, WI_F64_ADD },
    /* SUB */ { WI_I32_SUB,   WI_I64_SUB,   WI_F32_SUB, WI_F64_SUB },
    /* MUL */ { WI_I32_MUL,   WI_I64_MUL,   WI_F32_MUL, WI_F64_MUL },
    /* DIV */ { WI_I32_DIV_S, WI_I64_DIV_S, WI_F32_DIV, WI_F64_DIV },
    /* REM */ { WI_I32_REM_S, WI_I64_REM_S, WI_NOP,     WI_NOP     },

    /* EQ  */ { WI_I32_EQ,    WI_I64_EQ,    WI_F32_EQ,  WI_F64_EQ },
    /* NEQ */ { WI_I32_NE,    WI_I64_NE,    WI_F32_NE , WI_F64_NE },
    /* LT  */ { WI_I32_LT_S,  WI_I64_LT_S,  WI_F32_LT,  WI_F64_LT },
    /* LTE */ { WI_I32_LE_S,  WI_I64_LE_S,  WI_F32_LE,  WI_F64_LE },
    /* GT  */ { WI_I32_GT_S,  WI_I64_GT_S,  WI_F32_GT,  WI_F64_GT },
    /* GTE */ { WI_I32_GE_S,  WI_I64_GE_S,  WI_F32_GE,  WI_F64_GE },
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

    i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) call->callee);
    bh_arr_push(code, ((WasmInstruction){ WI_CALL, func_idx }));

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
    } else {
        compile_expression(mod, &code, source_expr);
    }

    *offset_return = offset;

    *pcode = code;
}

COMPILE_FUNC(expression, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (expr->kind) {
        case Ast_Kind_Local:
        case Ast_Kind_Param: {
            i32 localidx = (i32) bh_imap_get(&mod->local_map, (u64) expr);

            WID(WI_LOCAL_GET, localidx);
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
            break;
        }

        case Ast_Kind_Block:          compile_block(mod, &code, (AstBlock *) expr); break;
        case Ast_Kind_Call:           compile_call(mod, &code, (AstCall *) expr); break;
        case Ast_Kind_Intrinsic_Call: compile_intrinsic_call(mod, &code, (AstIntrinsicCall *) expr); break;
        case Ast_Kind_Binary_Op:      compile_binop(mod, &code, (AstBinaryOp *) expr); break;
        case Ast_Kind_Unary_Op:       compile_unaryop(mod, &code, (AstUnaryOp *) expr); break;

        case Ast_Kind_Address_Of: {
            AstAddressOf* aof = (AstAddressOf *) expr;

            switch (aof->expr->kind) {
                case Ast_Kind_Dereference: {
                    compile_expression(mod, &code, ((AstDereference *) aof->expr)->expr);
                    break;
                }

                case Ast_Kind_Array_Access: {
                    AstArrayAccess* aa = (AstArrayAccess *) aof->expr;
                    u64 offset = 0;
                    compile_array_access_location(mod, &code, aa, &offset);
                    if (offset != 0) {
                        WID(WI_I32_CONST, offset);
                        WI(WI_I32_ADD);
                    }
                    break;
                }

                case Ast_Kind_Field_Access: {
                    AstFieldAccess* field = (AstFieldAccess *) aof->expr;
                    u64 offset = 0;
                    compile_field_access_location(mod, &code, field, &offset);
                    if (offset != 0) {
                        WID(WI_I32_CONST, offset);
                        WI(WI_I32_ADD);
                    }
                    break;
                }

                default:
                    onyx_message_add(Msg_Type_Literal,
                            aof->token->pos,
                            "unsupported address of");
                }

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

            u64 offset = 0;
            compile_field_access_location(mod, &code, field, &offset);
            compile_load_instruction(mod, &code, field->type, offset);
            break;
        }

        case Ast_Kind_Size_Of: {
            AstSizeOf* so = (AstSizeOf *) expr;
            WID(WI_I32_CONST, so->size);
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

static const WasmInstructionType cast_map[][9] = {
    //          I8              I16                 I32                 U32                I64                U64                F32                F64                PTR
    /* I8  */ { WI_NOP,         WI_I32_EXTEND_8_S,  WI_I32_EXTEND_8_S,  WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE },
    /* I16 */ { WI_NOP,         WI_NOP,             WI_I32_EXTEND_16_S, WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_F32_FROM_I32_U, WI_F64_FROM_I32_U, WI_UNREACHABLE },
    /* I32 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_F32_FROM_I32_S, WI_F64_FROM_I32_S, WI_NOP },
    /* U32 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_F32_FROM_I32_U, WI_F64_FROM_I32_U, WI_NOP },
    /* I64 */ { WI_NOP,         WI_NOP,             WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_S, WI_F64_FROM_I64_S, WI_I32_FROM_I64 },
    /* U64 */ { WI_NOP,         WI_NOP,             WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_U, WI_F64_FROM_I64_U, WI_I32_FROM_I64 },
    /* F32 */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_I32_FROM_F32_S,  WI_I32_FROM_F32_U, WI_I64_FROM_F32_S, WI_I64_FROM_F32_U, WI_NOP,            WI_F64_FROM_F32,   WI_UNREACHABLE },
    /* F64 */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_I32_FROM_F64_S,  WI_I32_FROM_F64_U, WI_I64_FROM_F64_S, WI_I64_FROM_F64_U, WI_F32_FROM_F64,   WI_NOP,            WI_UNREACHABLE },
    /* PTR */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_NOP },
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
        return;
    }

    if (to->kind == Type_Kind_Basic && to->Basic.kind == Basic_Kind_Void) {
        WI(WI_DROP);
        return;
    }

    i32 fromidx = -1, toidx = -1;
    if (from->Basic.flags & Basic_Flag_Pointer) {
        fromidx = 8;
    }
    else if (from->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (from->Basic.flags & Basic_Flag_Unsigned) != 0;

        if      (from->Basic.size == 1 && !unsign) fromidx = 0;
        else if (from->Basic.size == 1 && unsign)  fromidx = -1;
        else if (from->Basic.size == 2 && !unsign) fromidx = 1;
        else if (from->Basic.size == 2 && unsign)  fromidx = -1;
        else if (from->Basic.size == 4 && !unsign) fromidx = 2;
        else if (from->Basic.size == 4 && unsign)  fromidx = 3;
        else if (from->Basic.size == 8 && !unsign) fromidx = 4;
        else if (from->Basic.size == 8 && unsign)  fromidx = 5;
    }
    else if (from->Basic.flags & Basic_Flag_Float) {
        if      (from->Basic.size == 4) fromidx = 6;
        else if (from->Basic.size == 8) fromidx = 7;
    }

    if (to->Basic.flags & Basic_Flag_Pointer) {
        toidx = 8;
    }
    else if (to->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (to->Basic.flags & Basic_Flag_Unsigned) != 0;

        if      (to->Basic.size == 1 && !unsign) toidx = 0;
        else if (to->Basic.size == 1 && unsign)  toidx = -1;
        else if (to->Basic.size == 2 && !unsign) toidx = 1;
        else if (to->Basic.size == 2 && unsign)  toidx = -1;
        else if (to->Basic.size == 4 && !unsign) toidx = 2;
        else if (to->Basic.size == 4 && unsign)  toidx = 3;
        else if (to->Basic.size == 8 && !unsign) toidx = 4;
        else if (to->Basic.size == 8 && unsign)  toidx = 5;
    }
    else if (to->Basic.flags & Basic_Flag_Float) {
        if      (to->Basic.size == 4) toidx = 6;
        else if (to->Basic.size == 8) toidx = 7;
    }

    if (fromidx != -1 && toidx != -1) {
        WasmInstructionType cast_op = cast_map[fromidx][toidx];
        if (cast_op == WI_UNREACHABLE) {
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
        compile_expression(mod, &code, ret->expr);
    }

    WI(WI_RETURN);

    *pcode = code;
}

static i32 generate_type_idx(OnyxWasmModule* mod, AstFunction* fd) {
    static char type_repr_buf[128];

    char* t = type_repr_buf;
    Type** param_type = fd->type->Function.params;
    i32 param_count = fd->type->Function.param_count;
    i32 params_left = param_count;
    while (params_left-- > 0) {
        // HACK: Using these directly as part of a string feels weird but they are
        // valid characters so I don't think it is going to be much of an issue
        *(t++) = (char) onyx_type_to_wasm_type(*param_type);
        param_type++;
    }
    *(t++) = ':';

    WasmType return_type = onyx_type_to_wasm_type(fd->type->Function.return_type);
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

static void compile_function(OnyxWasmModule* mod, AstFunction* fd) {
    // NOTE: Don't compile intrinsics
    if (fd->flags & Ast_Flag_Intrinsic) return;

    i32 type_idx = generate_type_idx(mod, fd);

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
            .i32_count = 0,
            .i64_count = 0,
            .f32_count = 0,
            .f64_count = 0,
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
        i32 localidx = 0;
        for (AstLocal *param = fd->params; param != NULL; param = (AstLocal *) param->next) {
            bh_imap_put(&mod->local_map, (u64) param, localidx++);
        }

        static const WasmType local_types[4] = { WASM_TYPE_INT32, WASM_TYPE_INT64, WASM_TYPE_FLOAT32, WASM_TYPE_FLOAT64 };

        // HACK: This assumes that the order of the count members
        // is the same as the order of the local_types above
        u8* count = &wasm_func.locals.i32_count;
        fori (ti, 0, 3) {
            bh_arr_each(AstLocal *, local, fd->locals) {
                if (onyx_type_to_wasm_type((*local)->type) == local_types[ti]) {
                    bh_imap_put(&mod->local_map, (u64) *local, localidx++);

                    (*count)++;
                }
            }

            count++;
        }

        // Generate code
        compile_function_body(mod, &wasm_func.code, fd);

    } else {
        // NOTE: Empty bodies still need a block end instruction
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
    }

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
            case 'n': *des++ = '\n'; break;
            case 't': *des++ = '\t'; break;
            case 'r': *des++ = '\r'; break;
            case 'v': *des++ = '\v'; break;
            case 'e': *des++ = '\e'; break;
            default:  *des++ = '\\';
                      *des++ = src[i];
            }
        } else {
            *des++ = src[i];
        }
    }
    *des++ = '\0';

    u32 length = (u32) (des - strdata);

    WasmDatum datum = {
        .offset = mod->next_datum_offset,
        .length = length,
        .data = strdata,
    };

    strlit->addr = (u32) mod->next_datum_offset,
    mod->next_datum_offset += length;

    bh_arr_push(mod->data, datum);
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

        .structured_jump_target = NULL,
    };

    bh_arr_new(alloc, module.types, 4);
    bh_arr_new(alloc, module.funcs, 4);
    bh_arr_new(alloc, module.imports, 4);
    bh_arr_new(alloc, module.globals, 4);
    bh_arr_new(alloc, module.data, 4);

    // NOTE: 16 is probably needlessly large
    bh_arr_new(global_heap_allocator, module.structured_jump_target, 16);
    bh_arr_set_length(module.structured_jump_target, 0);

    bh_table_init(global_heap_allocator, module.type_map, 61);
    bh_table_init(global_heap_allocator, module.exports, 61);

    bh_imap_init(&module.index_map, global_heap_allocator, 128);
    bh_imap_init(&module.local_map, global_heap_allocator, 16);

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
        switch (entity->type) {
            case Entity_Type_Function_Header: {
                if (entity->function->flags & Ast_Flag_Intrinsic) break;

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

                // HACK: To put this here
                // NOTE: Round up to the nearest multiple of 16
                builtin_heap_start.value.i =
                    (module->next_datum_offset & 15)
                    ? ((module->next_datum_offset >> 4) + 1) << 4
                    : module->next_datum_offset;
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

static void output_instruction(WasmInstruction* instr, bh_buffer* buff);

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

static i32 output_memorysection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_MEMORY);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) 1, &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    output_limits(4, -1, &vec_buff);

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
        bh_buffer_write_byte(&vec_buff, global->mutable ? 0x01 : 0x00);

        bh_arr_each(WasmInstruction, instr, global->initial_value)
            output_instruction(instr, &vec_buff);

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

static i32 output_locals(WasmFunc* func, bh_buffer* buff) {
    i32 prev_len = buff->length;

    // NOTE: Output vector length
    i32 total_locals =
        (i32) (func->locals.i32_count != 0) +
        (i32) (func->locals.i64_count != 0) +
        (i32) (func->locals.f32_count != 0) +
        (i32) (func->locals.f64_count != 0);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) total_locals, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    if (func->locals.i32_count != 0) {
        leb = uint_to_uleb128((u64) func->locals.i32_count, &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_INT32);
    }
    if (func->locals.i64_count != 0) {
        leb = uint_to_uleb128((u64) func->locals.i64_count, &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_INT64);
    }
    if (func->locals.f32_count != 0) {
        leb = uint_to_uleb128((u64) func->locals.f32_count, &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_FLOAT32);
    }
    if (func->locals.f64_count != 0) {
        leb = uint_to_uleb128((u64) func->locals.f64_count, &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_FLOAT64);
    }

    return buff->length - prev_len;
}

static void output_instruction(WasmInstruction* instr, bh_buffer* buff) {
    i32 leb_len;
    u8* leb;

    bh_buffer_write_byte(buff, (u8) instr->type);

    switch (instr->type) {
        case WI_LOCAL_GET:
        case WI_LOCAL_SET:
        case WI_LOCAL_TEE:
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
    bh_arr_each(WasmInstruction, instr, func->code) output_instruction(instr, &code_buff);

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
        // NOTE: 0x00 memory index
        bh_buffer_write_byte(&vec_buff, 0x00);

        bh_buffer_write_byte(&vec_buff, WI_I32_CONST);
        leb = int_to_leb128((i64) datum->offset, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
        bh_buffer_write_byte(&vec_buff, WI_BLOCK_END);

        leb = uint_to_uleb128((u64) datum->length, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
        fori (i, 0, datum->length - 1) bh_buffer_write_byte(&vec_buff, ((u8 *) datum->data)[i]);
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
    output_memorysection(module, &master_buffer);
    output_globalsection(module, &master_buffer);
    output_exportsection(module, &master_buffer);
    output_startsection(module, &master_buffer);
    output_codesection(module, &master_buffer);
    output_datasection(module, &master_buffer);

    bh_file_write(&file, master_buffer.data, master_buffer.length);
}
