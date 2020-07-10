#define BH_DEBUG
#include "onyxwasm.h"
#include "onyxutils.h"

// NOTE: Allows easier testing of types since most of the characters
// corresponding to these values are not printable
#if 1
const WasmType WASM_TYPE_INT32 = 0x7F;
const WasmType WASM_TYPE_INT64 = 0x7E;
const WasmType WASM_TYPE_FLOAT32 = 0x7D;
const WasmType WASM_TYPE_FLOAT64 = 0x7C;
const WasmType WASM_TYPE_VOID = 0x00;
#else
const WasmType WASM_TYPE_INT32 = 'A';
const WasmType WASM_TYPE_INT64 = 'B';
const WasmType WASM_TYPE_FLOAT32 = 'C';
const WasmType WASM_TYPE_FLOAT64 = 'D';
const WasmType WASM_TYPE_VOID = '\0';
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
    }
}

static WasmType onyx_type_to_wasm_type(Type* type) {
    if (type->kind == Type_Kind_Pointer) {
        return WASM_TYPE_INT32;
    }

    if (type->kind == Type_Kind_Basic) {
        TypeBasic* basic = &type->Basic;
        if (basic->flags & Basic_Flag_Boolean) return WASM_TYPE_INT32;
        if (basic->flags & Basic_Flag_Integer) {
            if (basic->size <= 4) return WASM_TYPE_INT32;
            if (basic->size == 8) return WASM_TYPE_INT64;
        }
        if (basic->flags & Basic_Flag_Float) {
            if (basic->size <= 4) return WASM_TYPE_FLOAT32;
            if (basic->size == 8) return WASM_TYPE_FLOAT64;;
        }
        if (basic->size == 0) return WASM_TYPE_VOID;
    }

    return WASM_TYPE_VOID;
}

#define WI(instr) bh_arr_push(code, ((WasmInstruction){ instr, 0x00 }));
#define WID(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, data }));

static void compile_function_body(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstFunction* fd);
static void compile_block(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstBlock* block);
static void compile_statement(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstNode* stmt);
static void compile_assign_lval(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstTyped* lval);
static void compile_assignment(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstAssign* assign);
static void compile_if(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstIf* if_node);
static void compile_while(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstWhile* while_node);
static void compile_binop(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstBinaryOp* binop);
static void compile_unaryop(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstUnaryOp* unop);
static void compile_call(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstCall* call);
static void compile_intrinsic_call(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstIntrinsicCall* call);
static void compile_expression(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstTyped* expr);
static void compile_cast(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstUnaryOp* cast);
static void compile_return(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstReturn* ret);

static void compile_function_body(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstFunction* fd) {
    if (fd->body == NULL) return;

    bh_arr(WasmInstruction) code = *pcode;

    forll (AstNode, stmt, fd->body->body, next) {
        compile_statement(mod, &code, stmt);
    }

    WI(WI_BLOCK_END);

    *pcode = code;
}

static void compile_block(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstBlock* block) {
    bh_arr(WasmInstruction) code = *pcode;

    WID(WI_BLOCK_START, 0x40);

    forll (AstNode, stmt, block->body, next) {
        compile_statement(mod, &code, stmt);
    }

    WI(WI_BLOCK_END);

    *pcode = code;
}

static void compile_structured_jump(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, b32 jump_backward) {
    bh_arr(WasmInstruction) code = *pcode;

    i32 labelidx = 0;
    u8 wanted = jump_backward ? 2 : 1;
    b32 success = 0;

    i32 len = bh_arr_length(mod->structured_jump_target) - 1;
    for (u8* t = &bh_arr_last(mod->structured_jump_target); len >= 0; len--, t--) {
        if (*t == wanted) {
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

static void compile_statement(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstNode* stmt) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (stmt->kind) {
        case Ast_Kind_Return: compile_return(mod, &code, (AstReturn *) stmt); break;
        case Ast_Kind_Assignment: compile_assignment(mod, &code, (AstAssign *) stmt); break;
        case Ast_Kind_If: compile_if(mod, &code, (AstIf *) stmt); break;
        case Ast_Kind_While: compile_while(mod, &code, (AstWhile *) stmt); break;
        case Ast_Kind_Break: compile_structured_jump(mod, &code, 0); break;
        case Ast_Kind_Continue: compile_structured_jump(mod, &code, 1); break;
        case Ast_Kind_Block: compile_block(mod, &code, (AstBlock *) stmt); break;

        case Ast_Kind_Call:
        case Ast_Kind_Intrinsic_Call:
            compile_expression(mod, &code, (AstTyped *) stmt);
            break;


        default: break;
    }

    *pcode = code;
}

static void compile_assign_lval(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstTyped* lval) {
    bh_arr(WasmInstruction) code = *pcode;

    if (lval->kind == Ast_Kind_Local || lval->kind == Ast_Kind_Param) {
        i32 localidx = (i32) bh_imap_get(&mod->local_map, (u64) lval);

        WID(WI_LOCAL_SET, localidx);

    } else if (lval->kind == Ast_Kind_Global) {
        i32 globalidx = (i32) bh_imap_get(&mod->global_map, (u64) lval);

        WID(WI_GLOBAL_SET, globalidx);

    } else {
        assert(("Invalid lval", 0));
    }

    *pcode = code;
}

static void compile_if(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstIf* if_node) {
    bh_arr(WasmInstruction) code = *pcode;

    compile_expression(mod, &code, if_node->cond);
    WID(WI_IF_START, 0x40);

    bh_arr_push(mod->structured_jump_target, 0);

    if (if_node->true_block.as_if) {
        // NOTE: This is kind of gross, but making a function for this doesn't feel right

        if (if_node->true_block.as_if->base.kind == Ast_Kind_If) {
            forll (AstNode, stmt, (AstNode *) if_node->true_block.as_if, next) {
                compile_statement(mod, &code, stmt);
            }
        } else if (if_node->true_block.as_if->base.kind == Ast_Kind_Block) {
            forll (AstNode, stmt, if_node->true_block.as_block->body, next) {
                compile_statement(mod, &code, stmt);
            }
        }
    }

    if (if_node->false_block.as_if) {
        WI(WI_ELSE);

        if (if_node->false_block.as_if->base.kind == Ast_Kind_If) {
            forll (AstNode, stmt, (AstNode *) if_node->false_block.as_if, next) {
                compile_statement(mod, &code, stmt);
            }
        } else if (if_node->false_block.as_if->base.kind == Ast_Kind_Block) {
            forll (AstNode, stmt, if_node->false_block.as_block->body, next) {
                compile_statement(mod, &code, stmt);
            }
        }
    }

    bh_arr_pop(mod->structured_jump_target);

    WI(WI_IF_END);

    *pcode = code;
}

static void compile_while(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstWhile* while_node) {
    bh_arr(WasmInstruction) code = *pcode;

    WID(WI_BLOCK_START, 0x40);
    WID(WI_LOOP_START, 0x40);

    compile_expression(mod, &code, while_node->cond);
    WI(WI_I32_EQZ);
    WID(WI_COND_JUMP, 0x01);

    bh_arr_push(mod->structured_jump_target, 1);
    bh_arr_push(mod->structured_jump_target, 2);

    forll (AstNode, stmt, while_node->body->body, next) {
        compile_statement(mod, &code, stmt);
    }

    bh_arr_pop(mod->structured_jump_target);
    bh_arr_pop(mod->structured_jump_target);

    WID(WI_JUMP, 0x00);

    WI(WI_LOOP_END);
    WI(WI_BLOCK_END);

    *pcode = code;
}

static void compile_assignment(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstAssign* assign) {
    bh_arr(WasmInstruction) code = *pcode;

    compile_expression(mod, &code, assign->expr);
    compile_assign_lval(mod, &code, assign->lval);

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
    /* NEQ */ { WI_NOP,       WI_NOP,       WI_F32_NE , WI_F64_NE },
    /* LT  */ { WI_I32_LT_S,  WI_I64_LT_S,  WI_F32_LT,  WI_F64_LT },
    /* LTE */ { WI_I32_LE_S,  WI_I64_LE_S,  WI_F32_LE,  WI_F64_LE },
    /* GT  */ { WI_I32_GT_S,  WI_I64_GT_S,  WI_F32_GT,  WI_F64_GT },
    /* GTE */ { WI_I32_GE_S,  WI_I64_GE_S,  WI_F32_GE,  WI_F64_GE },
};

static void compile_binop(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstBinaryOp* binop) {
    bh_arr(WasmInstruction) code = *pcode;

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
    if (operator_type == WASM_TYPE_INT32) optype = 0;
    else if (operator_type == WASM_TYPE_INT64) optype = 1;
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

static void compile_unaryop(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstUnaryOp* unop) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (unop->operation) {
        case Unary_Op_Negate:
            {
                TypeBasic* type = &unop->base.type->Basic;

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

static void compile_call(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    for (AstArgument *arg = call->arguments;
            arg != NULL;
            arg = (AstArgument *) arg->base.next) {
        compile_expression(mod, &code, arg->value);
    }

    i32 func_idx = (i32) bh_imap_get(&mod->func_map, (u64) call->callee);
    bh_arr_push(code, ((WasmInstruction){ WI_CALL, func_idx }));

    *pcode = code;
}

static void compile_intrinsic_call(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstIntrinsicCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    i32 place_arguments_normally = 1;

    // NOTE: Doing this in case there becomes intrinsics that the arguments
    // are not placed as they normally would be
    if (0) place_arguments_normally = 0;

    if (place_arguments_normally) {
        for (AstArgument *arg = call->arguments;
                arg != NULL;
                arg = (AstArgument *) arg->base.next) {
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

static void compile_expression(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (expr->kind) {
        case Ast_Kind_Binary_Op:
            compile_binop(mod, &code, (AstBinaryOp *) expr);
            break;

        case Ast_Kind_Unary_Op:
            compile_unaryop(mod, &code, (AstUnaryOp *) expr);
            break;

        case Ast_Kind_Local:
        case Ast_Kind_Param:
            {
                i32 localidx = (i32) bh_imap_get(&mod->local_map, (u64) expr);

                WID(WI_LOCAL_GET, localidx);
                break;
            }

        case Ast_Kind_Global:
            {
                i32 globalidx = (i32) bh_imap_get(&mod->global_map, (u64) expr);

                WID(WI_GLOBAL_GET, globalidx);
                break;
            }

        case Ast_Kind_Literal:
            {
                AstNumLit* lit = (AstNumLit *) expr;
                WasmType lit_type = onyx_type_to_wasm_type(lit->base.type);
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

        case Ast_Kind_Block: compile_block(mod, &code, (AstBlock *) expr); break;


        case Ast_Kind_Call:
            compile_call(mod, &code, (AstCall *) expr);
            break;

        case Ast_Kind_Intrinsic_Call:
            compile_intrinsic_call(mod, &code, (AstIntrinsicCall *) expr);
            break;

        default:
            DEBUG_HERE;
            bh_printf("Unhandled case: %d\n", expr->kind);
            assert(0);
    }

    *pcode = code;
}

static const WasmInstructionType cast_map[][6] = {
    //          I32                  U32                I64              U64                F32                F64
    /* I32 */ { WI_NOP,            WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_F32_FROM_I32_S, WI_F64_FROM_I32_S },
    /* U32 */ { WI_NOP,            WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_F32_FROM_I32_U, WI_F64_FROM_I32_U },
    /* I64 */ { WI_I32_FROM_I64,   WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_S, WI_F64_FROM_I64_S },
    /* U64 */ { WI_I32_FROM_I64,   WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_U, WI_F64_FROM_I64_U },
    /* F32 */ { WI_I32_FROM_F32_S, WI_I32_FROM_F32_U, WI_I64_FROM_F32_S, WI_I64_FROM_F32_U, WI_NOP,            WI_F64_FROM_F32   },
    /* F64 */ { WI_I32_FROM_F64_S, WI_I32_FROM_F64_U, WI_I64_FROM_F64_S, WI_I64_FROM_F64_U, WI_F32_FROM_F64,   WI_NOP,           },
};

static void compile_cast(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstUnaryOp* cast) {
    bh_arr(WasmInstruction) code = *pcode;

    compile_expression(mod, &code, cast->expr);

    Type* from = cast->expr->type;
    Type* to = cast->base.type;

    i32 fromidx = 0, toidx = 0;
    if (from->Basic.flags & Basic_Flag_Numeric) {
        b32 unsign = (from->Basic.flags & Basic_Flag_Unsigned) != 0;

        if (from->Basic.size == 4 && !unsign) fromidx = 0;
        else if (from->Basic.size == 4 && unsign) fromidx = 1;
        else if (from->Basic.size == 8 && !unsign) fromidx = 2;
        else if (from->Basic.size == 8 && unsign) fromidx = 3;
    }
    else if (from->Basic.flags & Basic_Flag_Float) {
        if (from->Basic.size == 4) fromidx = 4;
        else if (from->Basic.size == 8) fromidx = 5;
    }

    if (to->Basic.flags & Basic_Flag_Numeric) {
        b32 unsign = (to->Basic.flags & Basic_Flag_Unsigned) != 0;

        if (to->Basic.size == 4 && !unsign) toidx = 0;
        else if (to->Basic.size == 4 && unsign) toidx = 1;
        else if (to->Basic.size == 8 && !unsign) toidx = 2;
        else if (to->Basic.size == 8 && unsign) toidx = 3;
    }
    else if (to->Basic.flags & Basic_Flag_Float) {
        if (to->Basic.size == 4) toidx = 4;
        else if (to->Basic.size == 8) toidx = 5;
    }

    WasmInstructionType cast_op = cast_map[fromidx][toidx];
    if (cast_op != WI_NOP) {
        WI(cast_op);
    }

    *pcode = code;
}

static void compile_return(OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, AstReturn* ret) {
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
    AstLocal* param = fd->params;
    i32 param_count = 0;
    while (param) {
        // HACK: Using these directly as part of a string feels weird but they are
        // valid characters so I don't think it is going to be much of an issue
        *(t++) = (char) onyx_type_to_wasm_type(param->base.type);
        param_count++;
        param = (AstLocal *) param->base.next;
    }
    *(t++) = ':';

    WasmType return_type = onyx_type_to_wasm_type(fd->base.type);
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

        bh_arr_push(mod->functypes, type);

        bh_table_put(i32, mod->type_map, type_repr_buf, mod->next_type_idx);
        type_idx = mod->next_type_idx;
        mod->next_type_idx++;
    }

    return type_idx;
}

static void compile_function(OnyxWasmModule* mod, AstFunction* fd) {
    // NOTE: Don't compile intrinsics
    if (fd->base.flags & Ast_Flag_Intrinsic) return;

    i32 type_idx = generate_type_idx(mod, fd);

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

    if (fd->base.flags & Ast_Flag_Exported) {
        onyx_token_null_toggle(fd->base.token);

        i32 func_idx = (i32) bh_imap_get(&mod->func_map, (u64) fd);

        WasmExport wasm_export = {
            .kind = WASM_FOREIGN_FUNCTION,
            .idx = func_idx,
        };
        bh_table_put(WasmExport, mod->exports, fd->base.token->text, wasm_export);
        mod->export_count++;

        onyx_token_null_toggle(fd->base.token);
    }

    // If there is no body then don't process the code
    if (fd->body != NULL) {
        // NOTE: Generate the local map
        i32 localidx = 0;
        for (AstLocal *param = fd->params; param != NULL; param = (AstLocal *) param->base.next) {
            bh_imap_put(&mod->local_map, (u64) param, localidx++);
        }

        static const WasmType local_types[4] = { WASM_TYPE_INT32, WASM_TYPE_INT64, WASM_TYPE_FLOAT32, WASM_TYPE_FLOAT64 };

        // HACK: This assumes that the order of the count members
        // is the same as the order of the local_types above
        u8* count = &wasm_func.locals.i32_count;
        fori (ti, 0, 3) {
            forll (AstLocal, local, fd->body->locals->last_local, prev_local) {
                if (onyx_type_to_wasm_type(local->base.type) == local_types[ti]) {
                    bh_imap_put(&mod->local_map, (u64) local, localidx++);

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

static void compile_global_declaration(OnyxWasmModule* module, AstGlobal* global) {
    WasmGlobal glob = {
        .type = onyx_type_to_wasm_type(global->base.type),
        .mutable = (global->base.flags & Ast_Flag_Const) == 0,
        .initial_value = NULL,
    };

    if (!global->initial_value) {
        onyx_message_add(module->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                global->base.token->pos,
                "global without initial value");
        return;
    }

    if ((global->base.flags & Ast_Flag_Exported) != 0) {
        onyx_token_null_toggle(global->base.token);

        i32 global_idx = (i32) bh_imap_get(&module->func_map, (u64) global);

        WasmExport wasm_export = {
            .kind = WASM_FOREIGN_GLOBAL,
            .idx = global_idx,
        };
        bh_table_put(WasmExport, module->exports, global->base.token->text, wasm_export);
        module->export_count++;

        onyx_token_null_toggle(global->base.token);
    }

    compile_expression(module, &glob.initial_value, global->initial_value);
    bh_arr_push(module->globals, glob);
}

static void compile_foreign(OnyxWasmModule* module, AstForeign* foreign) {
    if (foreign->import->kind == Ast_Kind_Function) {
        i32 type_idx = generate_type_idx(module, (AstFunction *) foreign->import);

        WasmImport import = {
            .kind = WASM_FOREIGN_FUNCTION,
            .idx = type_idx,
            .mod = foreign->mod_token,
            .name = foreign->name_token,
        };

        bh_arr_push(module->imports, import);

    } else if (foreign->import->kind == Ast_Kind_Global) {
        WasmType global_type = onyx_type_to_wasm_type(((AstGlobal *) foreign->import)->base.type);

        WasmImport import = {
            .kind = WASM_FOREIGN_GLOBAL,
            .idx = global_type,
            .mod = foreign->mod_token,
            .name = foreign->name_token,
        };

        bh_arr_push(module->imports, import);

    } else {
        DEBUG_HERE;
        // NOTE: Invalid foreign
        assert(0);
    }
}

OnyxWasmModule onyx_wasm_module_create(bh_allocator alloc, OnyxMessages* msgs) {
    OnyxWasmModule module = {
        .allocator = alloc,
        .msgs = msgs,

        .type_map = NULL,
        .next_type_idx = 0,
        .functypes = NULL,

        .funcs = NULL,
        .next_func_idx = 0,

        .exports = NULL,
        .export_count = 0,

        .imports = NULL,
        .next_import_func_idx = 0,
        .next_import_global_idx = 0,

        .globals = NULL,
        .next_global_idx = 0,

        .structured_jump_target = NULL,
    };

    bh_arr_new(alloc, module.functypes, 4);
    bh_arr_new(alloc, module.funcs, 4);
    bh_arr_new(alloc, module.imports, 4);
    bh_arr_new(alloc, module.globals, 4);

    // NOTE: 16 is probably needlessly large
    bh_arr_new(global_heap_allocator, module.structured_jump_target, 16);
    bh_arr_set_length(module.structured_jump_target, 0);

    bh_table_init(global_heap_allocator, module.type_map, 61);
    bh_table_init(global_heap_allocator, module.exports, 61);

    bh_imap_init(&module.local_map,  global_heap_allocator);
    bh_imap_init(&module.func_map,   global_heap_allocator);
    bh_imap_init(&module.global_map, global_heap_allocator);

    return module;
}

void onyx_wasm_module_compile(OnyxWasmModule* module, OnyxProgram* program) {

    // NOTE: First, introduce all indicies for globals and functions
    bh_arr_each(AstForeign *, foreign, program->foreigns) {
        AstKind import_kind = (*foreign)->import->kind;

        if (import_kind == Ast_Kind_Function) {
            module->next_func_idx++;
            bh_imap_put(&module->func_map, (u64) (*foreign)->import, module->next_import_func_idx++);
        }
        else if (import_kind == Ast_Kind_Global) {
            module->next_global_idx++;
            bh_imap_put(&module->global_map, (u64) (*foreign)->import, module->next_import_global_idx++);
        }

        compile_foreign(module, *foreign);
    }

    bh_arr_each(AstFunction *, function, program->functions) {
        if (((*function)->base.flags & Ast_Flag_Intrinsic) == 0)
            bh_imap_put(&module->func_map, (u64) *function, module->next_func_idx++);
    }

    bh_arr_each(AstGlobal *, global, program->globals)
        bh_imap_put(&module->global_map, (u64) *global, module->next_global_idx++);


    // NOTE: Then, compile everything
    bh_arr_each(AstFunction *, function, program->functions)
        compile_function(module, *function);

    bh_arr_each(AstGlobal *, global, program->globals)
        compile_global_declaration(module, *global);
}

void onyx_wasm_module_free(OnyxWasmModule* module) {
    bh_arr_free(module->functypes);
    bh_arr_free(module->funcs);
    bh_imap_free(&module->local_map);
    bh_imap_free(&module->func_map);
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
            (void**) module->functypes,
            sizeof(WasmFuncType*),
            bh_arr_length(module->functypes),
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

void onyx_wasm_module_write_to_file(OnyxWasmModule* module, bh_file file) {
    bh_buffer master_buffer;
    bh_buffer_init(&master_buffer, global_heap_allocator, 128);
    bh_buffer_append(&master_buffer, WASM_MAGIC_STRING, 4);
    bh_buffer_append(&master_buffer, WASM_VERSION, 4);

    output_typesection(module, &master_buffer);
    output_importsection(module, &master_buffer);
    output_funcsection(module, &master_buffer);
    output_globalsection(module, &master_buffer);
    output_exportsection(module, &master_buffer);
    output_startsection(module, &master_buffer);
    output_codesection(module, &master_buffer);

    bh_file_write(&file, master_buffer.data, master_buffer.length);
}
