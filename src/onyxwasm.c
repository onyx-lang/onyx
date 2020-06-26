#define BH_DEBUG
#include "onyxwasm.h"

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

static WasmType onyx_type_to_wasm_type(OnyxTypeInfo* type) {
	if (type->is_bool) return WASM_TYPE_INT32;
	else if (type->is_int) {
		if (type->size == 4) return WASM_TYPE_INT32;
		if (type->size == 8) return WASM_TYPE_INT64;
	}
	else if (type->is_float) {
		if (type->size == 4) return WASM_TYPE_FLOAT32;
		if (type->size == 8) return WASM_TYPE_FLOAT64;
	}

	return WASM_TYPE_VOID;
}

static void compile_function_body(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeFuncDef* fd);
static void compile_block(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeBlock* block);
static void compile_statement(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* stmt);
static void compile_assign_lval(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* lval);
static void compile_assignment(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* assign);
static void compile_if(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeIf* if_node);
static void compile_while(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeWhile* while_node);
static void compile_binop(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeBinOp* binop);
static void compile_unaryop(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeUnaryOp* unop);
static void compile_expression(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* expr);
static void compile_cast(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeUnaryOp* cast);
static void compile_return(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* ret);

static void compile_function_body(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeFuncDef* fd) {
	if (fd->body == NULL) return;

	forll (OnyxAstNode, stmt, fd->body->body, next) {
		compile_statement(mod, func, stmt);
	}

	bh_arr_push(func->code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
}

static void compile_block(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeBlock* block) {
	bh_arr_push(func->code, ((WasmInstruction){ WI_BLOCK_START, 0x40 }));

	forll (OnyxAstNode, stmt, block->body, next) {
		compile_statement(mod, func, stmt);
	}

	bh_arr_push(func->code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
}

static void compile_statement(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* stmt) {
	switch (stmt->kind) {
		case ONYX_AST_NODE_KIND_SCOPE: break;
		case ONYX_AST_NODE_KIND_RETURN: compile_return(mod, func, stmt); break;
		case ONYX_AST_NODE_KIND_ASSIGNMENT: compile_assignment(mod, func, stmt); break;
        case ONYX_AST_NODE_KIND_IF: compile_if(mod, func, (OnyxAstNodeIf *) stmt); break;
        case ONYX_AST_NODE_KIND_WHILE: compile_while(mod, func, (OnyxAstNodeWhile *) stmt); break;
        case ONYX_AST_NODE_KIND_CALL: compile_expression(mod, func, stmt); break;
        case ONYX_AST_NODE_KIND_BLOCK: compile_block(mod, func, (OnyxAstNodeBlock *) stmt); break;

		default: break;
	}
}

static void compile_assign_lval(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* lval) {
	if (lval->kind == ONYX_AST_NODE_KIND_LOCAL || lval->kind == ONYX_AST_NODE_KIND_PARAM) {
		i32 localidx = (i32) bh_imap_get(&mod->local_map, (u64) lval);

		bh_arr_push(func->code, ((WasmInstruction){ WI_LOCAL_SET, localidx }));
	} else {
		assert(("Invalid lval", 0));
	}
}

static void compile_if(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeIf* if_node) {
    compile_expression(mod, func, if_node->cond);
    bh_arr_push(func->code, ((WasmInstruction){ WI_IF_START, 0x40 }));

    if (if_node->true_block) {
        // NOTE: This is kind of gross, but making a function for this doesn't feel right

        if (if_node->true_block->kind == ONYX_AST_NODE_KIND_IF) {
            forll (OnyxAstNode, stmt, if_node->true_block, next) {
                compile_statement(mod, func, stmt);
            }
        } else if (if_node->true_block->kind == ONYX_AST_NODE_KIND_BLOCK) {
            forll (OnyxAstNode, stmt, if_node->true_block->as_block.body, next) {
                compile_statement(mod, func, stmt);
            }
        }
    }

    if (if_node->false_block) {
        bh_arr_push(func->code, ((WasmInstruction){ WI_ELSE, 0x00 }));

        if (if_node->false_block->kind == ONYX_AST_NODE_KIND_IF) {
            forll (OnyxAstNode, stmt, if_node->false_block, next) {
                compile_statement(mod, func, stmt);
            }
        } else if (if_node->false_block->kind == ONYX_AST_NODE_KIND_BLOCK) {
            forll (OnyxAstNode, stmt, if_node->false_block->as_block.body, next) {
                compile_statement(mod, func, stmt);
            }
        }
    }

    bh_arr_push(func->code, ((WasmInstruction){ WI_IF_END, 0x00 }));
}

static void compile_while(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeWhile* while_node) {
    bh_arr_push(func->code, ((WasmInstruction){ WI_BLOCK_START, 0x40 }));
    bh_arr_push(func->code, ((WasmInstruction){ WI_LOOP_START, 0x40 }));

    compile_expression(mod, func, while_node->cond);
    bh_arr_push(func->code, ((WasmInstruction){ WI_I32_EQZ, 0x00 }));
    bh_arr_push(func->code, ((WasmInstruction){ WI_COND_JUMP, 0x01 }));

	forll (OnyxAstNode, stmt, while_node->body->body, next) {
		compile_statement(mod, func, stmt);
	}

    bh_arr_push(func->code, ((WasmInstruction){ WI_JUMP, 0x00 }));

    bh_arr_push(func->code, ((WasmInstruction){ WI_LOOP_END, 0x00 }));
    bh_arr_push(func->code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
}

static void compile_assignment(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* assign) {
	compile_expression(mod, func, assign->right);
	compile_assign_lval(mod, func, assign->left);
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

static void compile_binop(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeBinOp* binop) {
    b32 is_sign_significant = 0;

    switch (binop->operation) {
        case ONYX_BINARY_OP_DIVIDE:
        case ONYX_BINARY_OP_MODULUS:
        case ONYX_BINARY_OP_LESS:
        case ONYX_BINARY_OP_LESS_EQUAL:
        case ONYX_BINARY_OP_GREATER:
        case ONYX_BINARY_OP_GREATER_EQUAL:
            is_sign_significant = 1;

        default: break;
    }

    WasmType operator_type = onyx_type_to_wasm_type(binop->left->type);
    WasmInstructionType binop_instr = binop_map[binop->operation][operator_type];

    if (binop_instr == WI_NOP) {
        assert(("Invalid type and operation", 0));
    }

    // NOTE: Use unsigned variant if needed
    // Unsigned instructions are always right after
    // the signed equivalent
    if (is_sign_significant) {
        if (binop->left->type->is_unsigned) {
            binop_instr += 1;
        }
    }

    compile_expression(mod, func, binop->left);
    compile_expression(mod, func, binop->right);

    bh_arr_push(func->code, ((WasmInstruction){ binop_instr, 0x00 }));
}

static void compile_unaryop(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeUnaryOp* unop) {
    switch (unop->operation) {
        case ONYX_UNARY_OP_NEGATE:
            {
                OnyxTypeInfoKind type_kind = unop->type->kind;

                if (type_kind == ONYX_TYPE_INFO_KIND_INT32) {
                    bh_arr_push(func->code, ((WasmInstruction){ WI_I32_CONST, 0x00 }));
                    compile_expression(mod, func, unop->left);
                    bh_arr_push(func->code, ((WasmInstruction){ WI_I32_SUB, 0x00 }));

                } else if (type_kind == ONYX_TYPE_INFO_KIND_INT64) {
                    bh_arr_push(func->code, ((WasmInstruction){ WI_I64_CONST, 0x00 }));
                    compile_expression(mod, func, unop->left);
                    bh_arr_push(func->code, ((WasmInstruction){ WI_I64_SUB, 0x00 }));

                } else {
                    compile_expression(mod, func, unop->left);

                    if (type_kind == ONYX_TYPE_INFO_KIND_FLOAT32)
                        bh_arr_push(func->code, ((WasmInstruction){ WI_F32_NEG, 0x00 }));

                    if (type_kind == ONYX_TYPE_INFO_KIND_FLOAT64)
                        bh_arr_push(func->code, ((WasmInstruction){ WI_F32_NEG, 0x00 }));
                }

                break;
            }

        case ONYX_UNARY_OP_NOT:
            compile_expression(mod, func, unop->left);
            bh_arr_push(func->code, ((WasmInstruction){ WI_I32_EQZ, 0x00 }));
            break;

        case ONYX_UNARY_OP_CAST: compile_cast(mod, func, unop); break;
    }
}

static void compile_expression(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* expr) {
	switch (expr->kind) {
        case ONYX_AST_NODE_KIND_BIN_OP:
            compile_binop(mod, func, &expr->as_binop);
            break;

        case ONYX_AST_NODE_KIND_UNARY_OP:
            compile_unaryop(mod, func, &expr->as_unaryop);
            break;

		case ONYX_AST_NODE_KIND_LOCAL:
		case ONYX_AST_NODE_KIND_PARAM:
			{
				i32 localidx = (i32) bh_imap_get(&mod->local_map, (u64) expr);

				bh_arr_push(func->code, ((WasmInstruction){ WI_LOCAL_GET, localidx }));
				break;
			}

		case ONYX_AST_NODE_KIND_LITERAL:
			{
                OnyxAstNodeNumLit* lit = &expr->as_numlit;
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

				bh_arr_push(func->code, instr);
				break;
			}

		case ONYX_AST_NODE_KIND_BLOCK: compile_block(mod, func, (OnyxAstNodeBlock *) expr); break;

		case ONYX_AST_NODE_KIND_CALL:
			{
				OnyxAstNodeCall* call = &expr->as_call;
				forll (OnyxAstNode, arg, call->arguments, next) {
					compile_expression(mod, func, arg->left);
				}

                i32 func_idx = (i32) bh_imap_get(&mod->func_map, (u64) call->callee);
                bh_arr_push(func->code, ((WasmInstruction){ WI_CALL, func_idx }));

				break;
			}

		default:
			DEBUG_HERE;
			bh_printf("Unhandled case: %d\n", expr->kind);
			assert(0);
	}
}

static const WasmInstructionType cast_map[][6] = {
	// 			I32					U32					I64					U64					F32					F64
	/* I32 */ {	WI_NOP, 			WI_NOP,				WI_I64_FROM_I32_S, 	WI_I64_FROM_I32_S,	WI_F32_FROM_I32_S,	WI_F64_FROM_I32_S },
	/* U32 */ {	WI_NOP, 			WI_NOP,				WI_I64_FROM_I32_U, 	WI_I64_FROM_I32_U,	WI_F32_FROM_I32_U,	WI_F64_FROM_I32_U },
	/* I64 */ { WI_I32_FROM_I64,	WI_I32_FROM_I64,	WI_NOP,				WI_NOP,				WI_F32_FROM_I64_S,	WI_F64_FROM_I64_S },
	/* U64 */ { WI_I32_FROM_I64,	WI_I32_FROM_I64,	WI_NOP,				WI_NOP,				WI_F32_FROM_I64_U,	WI_F64_FROM_I64_U },
	/* F32 */ { WI_I32_FROM_F32_S,	WI_I32_FROM_F32_U,	WI_I64_FROM_F32_S,	WI_I64_FROM_F32_U,	WI_NOP,				WI_F64_FROM_F32	  },
	/* F64 */ { WI_I32_FROM_F64_S,	WI_I32_FROM_F64_U,	WI_I64_FROM_F64_S,	WI_I64_FROM_F64_U,	WI_F32_FROM_F64,	WI_NOP,			  },
};

static void compile_cast(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNodeUnaryOp* cast) {
	compile_expression(mod, func, cast->left);

	OnyxTypeInfo* from = cast->left->type;
	OnyxTypeInfo* to = cast->type;

	i32 fromidx = 0, toidx = 0;
	if (from->is_int) {
		if (from->size == 4 && !from->is_unsigned) fromidx = 0;
		else if (from->size == 4 && from->is_unsigned) fromidx = 1;
		else if (from->size == 8 && !from->is_unsigned) fromidx = 2;
		else if (from->size == 8 && from->is_unsigned) fromidx = 3;
	} else if (from->is_float) {
		if (from->size == 4) fromidx = 4;
		else if (from->size == 8) fromidx = 5;
	}

	if (to->is_int) {
		if (to->size == 4 && !to->is_unsigned) toidx = 0;
		else if (to->size == 4 && to->is_unsigned) toidx = 1;
		else if (to->size == 8 && !to->is_unsigned) toidx = 2;
		else if (to->size == 8 && to->is_unsigned) toidx = 3;
	} else if (to->is_float) {
		if (to->size == 4) toidx = 4;
		else if (to->size == 8) toidx = 5;
	}

	WasmInstructionType cast_op = cast_map[fromidx][toidx];
	if (cast_op != WI_NOP) {
		bh_arr_push(func->code, ((WasmInstruction){ cast_op, 0x00 }));
	}
}

static void compile_return(OnyxWasmModule* mod, WasmFunc* func, OnyxAstNode* ret) {
	if (ret->left) {
		compile_expression(mod, func, ret->left);
	}

	bh_arr_push(func->code, ((WasmInstruction){ WI_RETURN, 0x00 }));
}

static i32 generate_type_idx(OnyxWasmModule* mod, OnyxAstNodeFuncDef* fd) {
	static char type_repr_buf[128];

	char* t = type_repr_buf;
	OnyxAstNodeParam* param = fd->params;
	i32 param_count = 0;
	while (param) {
		// HACK: Using these directly as part of a string feels weird but they are
		// valid characters so I don't think it is going to be much of an issue
		*(t++) = (char) onyx_type_to_wasm_type(param->type);
		param_count++;
		param = param->next;
	}
	*(t++) = ':';

	WasmType return_type = onyx_type_to_wasm_type(fd->return_type);
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

static void compile_function_definition(OnyxWasmModule* mod, OnyxAstNodeFuncDef* fd) {
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

	if (fd->flags & ONYX_AST_FLAG_EXPORTED) {
		onyx_token_null_toggle(*fd->token);

        i32 func_idx = (i32) bh_imap_get(&mod->func_map, (u64) fd);

		WasmExport wasm_export = {
			.kind = WASM_FOREIGN_FUNCTION,
			.idx = func_idx,
		};
		bh_table_put(WasmExport, mod->exports, fd->token->token, wasm_export);
		mod->export_count++;

		onyx_token_null_toggle(*fd->token);
	}

	// If there is no body then don't process the code
	if (fd->body != NULL) {
        // NOTE: Generate the local map
        i32 localidx = 0;
        forll (OnyxAstNodeParam, param, fd->params, next) {
            bh_imap_put(&mod->local_map, (u64) param, localidx++);
        }

        static const WasmType local_types[4] = { WASM_TYPE_INT32, WASM_TYPE_INT64, WASM_TYPE_FLOAT32, WASM_TYPE_FLOAT64 };

        // HACK: This assumes that the order of the count members
        // is the same as the order of the local_types above
        u8* count = &wasm_func.locals.i32_count;
        fori (ti, 0, 3) {
            forll (OnyxAstNodeLocal, local, fd->body->scope->last_local, prev_local) {
                if (onyx_type_to_wasm_type(local->type) == local_types[ti]) {
                    bh_imap_put(&mod->local_map, (u64) local, localidx++);

                    (*count)++;
                }
            }

            count++;
        }

        // Generate code
        compile_function_body(mod, &wasm_func, fd);
    } else {
        // NOTE: Empty bodies still need a block end instruction
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
    }

	bh_arr_push(mod->funcs, wasm_func);

	// NOTE: Clear the local map on exit of generating this function
	bh_imap_clear(&mod->local_map);
}

static void compile_foreign(OnyxWasmModule* module, OnyxAstNodeForeign* foreign) {
    if (foreign->import->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
        i32 type_idx = generate_type_idx(module, &foreign->import->as_funcdef);

        WasmImport import = {
            .kind = WASM_FOREIGN_FUNCTION,
            .idx = type_idx,
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

OnyxWasmModule onyx_wasm_module_create(bh_allocator alloc) {
	OnyxWasmModule module = {
		.allocator = alloc,

		.type_map = NULL,
		.next_type_idx = 0,
		.functypes = NULL,

		.funcs = NULL,
		.next_func_idx = 0,

		.exports = NULL,
		.export_count = 0,

        .imports = NULL,
        .next_import_func_idx = 0,
	};

	bh_arr_new(alloc, module.functypes, 4);
	bh_arr_new(alloc, module.funcs, 4);
    bh_arr_new(alloc, module.imports, 4);

	bh_table_init(bh_heap_allocator(), module.type_map, 61);
	bh_table_init(bh_heap_allocator(), module.exports, 61);

	bh_imap_init(&module.local_map, bh_heap_allocator());
    bh_imap_init(&module.func_map, bh_heap_allocator());

    return module;
}

void onyx_wasm_module_compile(OnyxWasmModule* module, OnyxAstNodeFile* program) {
	OnyxAstNode* walker;
    OnyxAstNodeFile* top_walker = program;
    while (top_walker) {

        walker = top_walker->contents;
        while (walker) {
            if (walker->kind == ONYX_AST_NODE_KIND_FOREIGN
                && walker->as_foreign.import->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
                module->next_func_idx++;
            }

            walker = walker->next;
        }

        top_walker = top_walker->next;
    }

    top_walker = program;
    while (top_walker) {

        walker = top_walker->contents;
        while (walker) {
            if (walker->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
                i32 func_idx = module->next_func_idx++;
                bh_imap_put(&module->func_map, (u64) walker, func_idx);
            }

            if (walker->kind == ONYX_AST_NODE_KIND_FOREIGN) {
                OnyxAstNodeForeign* foreign = &walker->as_foreign;

                if (foreign->import->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
                    i32 func_idx = module->next_import_func_idx++;
                    bh_imap_put(&module->func_map, (u64) foreign->import, func_idx);
                }
            }

            walker = walker->next;
        }

        top_walker = top_walker->next;
    }

	top_walker = program;
    while (top_walker) {

        walker = top_walker->contents;
        while (walker) {
            switch (walker->kind) {
                case ONYX_AST_NODE_KIND_FUNCDEF:
                    compile_function_definition(module, &walker->as_funcdef);
                    break;
                case ONYX_AST_NODE_KIND_FOREIGN:
                    compile_foreign(module, &walker->as_foreign);
                    break;
                default: break;
            }

            walker = walker->next;
        }

        top_walker = top_walker->next;
    }
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

static i32 output_importsection(OnyxWasmModule* module, bh_buffer* buff) {
	i32 prev_len = buff->length;
	bh_buffer_write_byte(buff, WASM_SECTION_ID_IMPORT);

	bh_buffer vec_buff;
	bh_buffer_init(&vec_buff, buff->allocator, 128);

	i32 leb_len;
	u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->imports)), &leb_len);
	bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmImport, import, module->imports) {
        output_name(import->mod->token, import->mod->length, &vec_buff);
        output_name(import->name->token, import->name->length, &vec_buff);
        bh_buffer_write_byte(&vec_buff, (u8) import->kind);

        leb = uint_to_uleb128((u64) import->idx, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
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
        case WI_CALL:
		case WI_BLOCK_START:
        case WI_LOOP_START:
        case WI_JUMP:
        case WI_COND_JUMP:
		case WI_IF_START:
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
	bh_buffer_init(&master_buffer, bh_heap_allocator(), 128);
	bh_buffer_append(&master_buffer, WASM_MAGIC_STRING, 4);
	bh_buffer_append(&master_buffer, WASM_VERSION, 4);

	output_typesection(module, &master_buffer);
    output_importsection(module, &master_buffer);
	output_funcsection(module, &master_buffer);
	output_exportsection(module, &master_buffer);
	output_startsection(module, &master_buffer);
	output_codesection(module, &master_buffer);

	bh_file_write(&file, master_buffer.data, master_buffer.length);
}
