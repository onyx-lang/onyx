#define BH_DEBUG
#include "onyxsempass.h"

static void check_function(OnyxSemPassState* state, AstNodeFunction* func);
static void check_block(OnyxSemPassState* state, AstNodeBlock* block);
static void check_statement_chain(OnyxSemPassState* state, AstNode* start);
static void check_statement(OnyxSemPassState* state, AstNode* stmt);
static void check_assignment(OnyxSemPassState* state, AstNodeAssign* assign);
static void check_return(OnyxSemPassState* state, AstNodeReturn* retnode);
static void check_if(OnyxSemPassState* state, AstNodeIf* ifnode);
static void check_while(OnyxSemPassState* state, AstNodeWhile* whilenode);
static void check_call(OnyxSemPassState* state, AstNodeCall* call);
static void check_expression(OnyxSemPassState* state, AstNodeTyped* expr);
static void check_global(OnyxSemPassState* state, AstNodeGlobal* global);

static void check_assignment(OnyxSemPassState* state, AstNodeAssign* assign) {
    if (assign->lval->kind == AST_NODE_KIND_SYMBOL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                assign->lval->token->pos,
                assign->lval->token->token, assign->lval->token->length);
        return;
    }

    if ((assign->lval->flags & ONYX_AST_FLAG_CONST) != 0 && assign->lval->type->is_known) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_ASSIGN_CONST,
                assign->base.token->pos,
                assign->lval->token->token, assign->lval->token->length);
        return;
    }

    if ((assign->lval->flags & ONYX_AST_FLAG_LVAL) == 0) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_NOT_LVAL,
                assign->base.token->pos,
                assign->lval->token->token, assign->lval->token->length);
        return;
    }

    check_expression(state, assign->expr);

    if (!assign->lval->type->is_known) {
        assign->lval->type = assign->expr->type;
    } else {
        if (assign->lval->type != assign->expr->type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_ASSIGNMENT_TYPE_MISMATCH,
                    assign->base.token->pos,
                    assign->lval->type->name, assign->expr->type->name);
            return;
        }
    }
}

static void check_return(OnyxSemPassState* state, AstNodeReturn* retnode) {
    if (retnode->expr) {
        check_expression(state, retnode->expr);

        if (retnode->expr->type != state->expected_return_type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_RETURN_MISMATCH,
                    retnode->expr->token->pos,
                    retnode->expr->type->name, state->expected_return_type->name);
        }
    } else {
        if (state->expected_return_type->size > 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    retnode->base.token->pos,
                    "returning from non-void function without value");
        }
    }
}

static void check_if(OnyxSemPassState* state, AstNodeIf* ifnode) {
    check_expression(state, ifnode->cond);
    if (ifnode->cond->type != &builtin_types[TYPE_INFO_KIND_BOOL]) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                ifnode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    if (ifnode->true_block.as_if) check_statement(state,  (AstNode *) ifnode->true_block.as_block);
    if (ifnode->false_block.as_if) check_statement(state, (AstNode *) ifnode->false_block.as_block);
}

static void check_while(OnyxSemPassState* state, AstNodeWhile* whilenode) {
    check_expression(state, whilenode->cond);
    if (whilenode->cond->type != &builtin_types[TYPE_INFO_KIND_BOOL]) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                whilenode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    check_block(state, whilenode->body);
}

static void check_call(OnyxSemPassState* state, AstNodeCall* call) {
    AstNodeFunction* callee = (AstNodeFunction *) call->callee;

    if (callee->base.kind == AST_NODE_KIND_SYMBOL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                callee->base.token->pos,
                callee->base.token->token, callee->base.token->length);
        return;
    }

    if (callee->base.kind != AST_NODE_KIND_FUNCTION) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CALL_NON_FUNCTION,
                call->base.token->pos,
                callee->base.token->token, callee->base.token->length);
        return;
    }

    // NOTE: If we calling an intrinsic function, translate the
    // call into an intrinsic call node.
    if (callee->base.flags & ONYX_AST_FLAG_INTRINSIC) {
        call->base.kind = AST_NODE_KIND_INTRINSIC_CALL;
        call->callee = NULL;

        onyx_token_null_toggle(callee->base.token);

        char* intr_name = callee->base.token->token;
        OnyxIntrinsic intrinsic = ONYX_INTRINSIC_UNDEFINED;

        if (!strcmp("memory_size", intr_name))       intrinsic = ONYX_INTRINSIC_MEMORY_SIZE;
        else if (!strcmp("memory_grow", intr_name))  intrinsic = ONYX_INTRINSIC_MEMORY_GROW;

        else if (!strcmp("clz_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_CLZ;
        else if (!strcmp("ctz_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_CTZ;
        else if (!strcmp("popcnt_i32", intr_name))   intrinsic = ONYX_INTRINSIC_I32_POPCNT;
        else if (!strcmp("and_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_AND;
        else if (!strcmp("or_i32", intr_name))       intrinsic = ONYX_INTRINSIC_I32_OR;
        else if (!strcmp("xor_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_XOR;
        else if (!strcmp("shl_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_SHL;
        else if (!strcmp("slr_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_SLR;
        else if (!strcmp("sar_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_SAR;
        else if (!strcmp("rotl_i32", intr_name))     intrinsic = ONYX_INTRINSIC_I32_ROTL;
        else if (!strcmp("rotr_i32", intr_name))     intrinsic = ONYX_INTRINSIC_I32_ROTR;

        else if (!strcmp("clz_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_CLZ;
        else if (!strcmp("ctz_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_CTZ;
        else if (!strcmp("popcnt_i64", intr_name))   intrinsic = ONYX_INTRINSIC_I64_POPCNT;
        else if (!strcmp("and_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_AND;
        else if (!strcmp("or_i64", intr_name))       intrinsic = ONYX_INTRINSIC_I64_OR;
        else if (!strcmp("xor_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_XOR;
        else if (!strcmp("shl_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_SHL;
        else if (!strcmp("slr_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_SLR;
        else if (!strcmp("sar_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_SAR;
        else if (!strcmp("rotl_i64", intr_name))     intrinsic = ONYX_INTRINSIC_I64_ROTL;
        else if (!strcmp("rotr_i64", intr_name))     intrinsic = ONYX_INTRINSIC_I64_ROTR;

        else if (!strcmp("abs_f32", intr_name))      intrinsic = ONYX_INTRINSIC_F32_ABS;
        else if (!strcmp("ceil_f32", intr_name))     intrinsic = ONYX_INTRINSIC_F32_CEIL;
        else if (!strcmp("floor_f32", intr_name))    intrinsic = ONYX_INTRINSIC_F32_FLOOR;
        else if (!strcmp("trunc_f32", intr_name))    intrinsic = ONYX_INTRINSIC_F32_TRUNC;
        else if (!strcmp("nearest_f32", intr_name))  intrinsic = ONYX_INTRINSIC_F32_NEAREST;
        else if (!strcmp("sqrt_f32", intr_name))     intrinsic = ONYX_INTRINSIC_F32_SQRT;
        else if (!strcmp("min_f32", intr_name))      intrinsic = ONYX_INTRINSIC_F32_MIN;
        else if (!strcmp("max_f32", intr_name))      intrinsic = ONYX_INTRINSIC_F32_MAX;
        else if (!strcmp("copysign_f32", intr_name)) intrinsic = ONYX_INTRINSIC_F32_COPYSIGN;

        else if (!strcmp("abs_f64", intr_name))      intrinsic = ONYX_INTRINSIC_F64_ABS;
        else if (!strcmp("ceil_f64", intr_name))     intrinsic = ONYX_INTRINSIC_F64_CEIL;
        else if (!strcmp("floor_f64", intr_name))    intrinsic = ONYX_INTRINSIC_F64_FLOOR;
        else if (!strcmp("trunc_f64", intr_name))    intrinsic = ONYX_INTRINSIC_F64_TRUNC;
        else if (!strcmp("nearest_f64", intr_name))  intrinsic = ONYX_INTRINSIC_F64_NEAREST;
        else if (!strcmp("sqrt_f64", intr_name))     intrinsic = ONYX_INTRINSIC_F64_SQRT;
        else if (!strcmp("min_f64", intr_name))      intrinsic = ONYX_INTRINSIC_F64_MIN;
        else if (!strcmp("max_f64", intr_name))      intrinsic = ONYX_INTRINSIC_F64_MAX;
        else if (!strcmp("copysign_f64", intr_name)) intrinsic = ONYX_INTRINSIC_F64_COPYSIGN;

        ((AstNodeIntrinsicCall *)call)->intrinsic = intrinsic;

        onyx_token_null_toggle(callee->base.token);
    }

    call->base.type = callee->base.type;

    AstNodeLocal* formal_param = callee->params;
    AstNodeArgument* actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        check_expression(state, (AstNodeTyped *) actual_param);

        if (formal_param->base.type != actual_param->base.type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_PARAM_TYPE_MISMATCH,
                    call->base.token->pos,
                    callee->base.token->token, callee->base.token->length,
                    formal_param->base.type->name, arg_pos,
                    actual_param->base.type->name);
            return;
        }

        arg_pos++;
        formal_param = (AstNodeLocal *) formal_param->base.next;
        actual_param = (AstNodeArgument *) actual_param->base.next;
    }

    if (formal_param != NULL && actual_param == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                call->base.token->pos,
                "too few arguments to function call");
        return;
    }

    if (formal_param == NULL && actual_param != NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                call->base.token->pos,
                "too many arguments to function call");
        return;
    }
}

static void check_expression(OnyxSemPassState* state, AstNodeTyped* expr) {
    switch (expr->kind) {
        case AST_NODE_KIND_BIN_OP:
            expr->type = &builtin_types[TYPE_INFO_KIND_UNKNOWN];

            check_expression(state, ((AstNodeBinOp *) expr)->left);
            check_expression(state, ((AstNodeBinOp *) expr)->right);

            if (((AstNodeBinOp *) expr)->left->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                        expr->token->pos,
                        NULL, 0);
                return;
            }

            if (((AstNodeBinOp *) expr)->right->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                        expr->token->pos,
                        NULL, 0);
                return;
            }

            if (((AstNodeBinOp *) expr)->left->type != ((AstNodeBinOp *) expr)->right->type) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
                        expr->token->pos,
                        ((AstNodeBinOp *) expr)->left->type->name,
                        ((AstNodeBinOp *) expr)->right->type->name);
                return;
            }

            if (((AstNodeBinOp *) expr)->operation >= ONYX_BINARY_OP_EQUAL
                    && ((AstNodeBinOp *) expr)->operation <= ONYX_BINARY_OP_GREATER_EQUAL) {
                expr->type = &builtin_types[TYPE_INFO_KIND_BOOL];
            } else {
                expr->type = ((AstNodeBinOp *) expr)->left->type;
            }

            break;

        case AST_NODE_KIND_UNARY_OP:
            if (((AstNodeUnaryOp *) expr)->operation != ONYX_UNARY_OP_CAST) {
                check_expression(state, ((AstNodeUnaryOp *) expr)->expr);
                expr->type = ((AstNodeUnaryOp *) expr)->expr->type;
            }
            break;

        case AST_NODE_KIND_CALL:
            check_call(state, (AstNodeCall *) expr);
            break;

        case AST_NODE_KIND_BLOCK:
            check_block(state, (AstNodeBlock *) expr);
            break;

        case AST_NODE_KIND_SYMBOL:
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                    expr->token->pos,
                    expr->token->token, expr->token->length);
            break;

        case AST_NODE_KIND_LOCAL:
        case AST_NODE_KIND_PARAM:
            if (!expr->type->is_known) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "local variable with unknown type");
            }
            break;

        case AST_NODE_KIND_GLOBAL:
            if (!expr->type->is_known) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "global with unknown type");
            }
            break;

        case AST_NODE_KIND_ARGUMENT:
            check_expression(state, ((AstNodeArgument *) expr)->value);
            expr->type = ((AstNodeArgument *) expr)->value->type;
            break;

        case AST_NODE_KIND_LITERAL:
            // NOTE: Literal types should have been decided
            // in the parser (for now).
            assert(expr->type->is_known);
            break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void check_global(OnyxSemPassState* state, AstNodeGlobal* global) {
    if (global->initial_value) {
        check_expression(state, global->initial_value);

        if (global->base.type->is_known) {
            if (global->base.type != global->initial_value->type) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_GLOBAL_TYPE_MISMATCH,
                        global->base.token->pos,
                        global->base.token->token, global->base.token->length,
                        global->base.type->name, global->initial_value->type->name);
                return;
            }
        } else {
            if (global->initial_value->type)
                global->base.type = global->initial_value->type;
        }

    } else {
        if (!global->base.type || !global->base.type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    global->base.token->pos,
                    "global variable with unknown type");
        }
    }
}

static void check_statement(OnyxSemPassState* state, AstNode* stmt) {
    switch (stmt->kind) {
        case AST_NODE_KIND_ASSIGNMENT: check_assignment(state, (AstNodeAssign *) stmt); break;
        case AST_NODE_KIND_RETURN:     check_return(state, (AstNodeReturn *) stmt); break;
        case AST_NODE_KIND_IF:         check_if(state, (AstNodeIf *) stmt); break;
        case AST_NODE_KIND_WHILE:      check_while(state, (AstNodeWhile *) stmt); break;
        case AST_NODE_KIND_CALL:       check_call(state, (AstNodeCall *) stmt); break;
        case AST_NODE_KIND_BLOCK:      check_block(state, (AstNodeBlock *) stmt); break;

        default: break;
    }
}

static void check_statement_chain(OnyxSemPassState* state, AstNode* start) {
    while (start) {
        check_statement(state, start);
        start = start->next;
    }
}

static void check_block(OnyxSemPassState* state, AstNodeBlock* block) {
    check_statement_chain(state, block->body);

    forll(AstNodeLocal, local, block->locals->last_local, prev_local) {
        if (!local->base.type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                    local->base.token->pos,
                    local->base.token->token, local->base.token->length);
            return;
        }
    }
}

static void check_function(OnyxSemPassState* state, AstNodeFunction* func) {
    for (AstNodeLocal *param = func->params; param != NULL; param = (AstNodeLocal *) param->base.next) {
        if (!param->base.type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->base.token->pos,
                    "function parameter types must be known");
            return;
        }

        if (param->base.type->size == 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->base.token->pos,
                    "function parameters must have non-void types");
            return;
        }
    }

    state->expected_return_type = func->base.type;
    if (func->body) {
        check_block(state, func->body);
    }
}

void onyx_type_check(OnyxSemPassState* state, OnyxProgram* program) {

    bh_arr_each(AstNodeGlobal *, global, program->globals)
        check_global(state, *global);

    bh_arr_each(AstNodeFunction *, function, program->functions)
        check_function(state, *function);
}
