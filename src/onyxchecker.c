#define BH_DEBUG
#include "onyxsempass.h"

static void check_function(OnyxSemPassState* state, AstFunction* func);
static void check_block(OnyxSemPassState* state, AstBlock* block);
static void check_statement_chain(OnyxSemPassState* state, AstNode* start);
static void check_statement(OnyxSemPassState* state, AstNode* stmt);
static void check_assignment(OnyxSemPassState* state, AstAssign* assign);
static void check_return(OnyxSemPassState* state, AstReturn* retnode);
static void check_if(OnyxSemPassState* state, AstIf* ifnode);
static void check_while(OnyxSemPassState* state, AstWhile* whilenode);
static void check_call(OnyxSemPassState* state, AstCall* call);
static void check_expression(OnyxSemPassState* state, AstTyped* expr);
static void check_global(OnyxSemPassState* state, AstGlobal* global);

static void check_assignment(OnyxSemPassState* state, AstAssign* assign) {
    if (assign->lval->kind == Ast_Kind_Symbol) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                assign->lval->token->pos,
                assign->lval->token->text, assign->lval->token->length);
        return;
    }

    if ((assign->lval->flags & Ast_Flag_Const) != 0 && assign->lval->type->is_known) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_ASSIGN_CONST,
                assign->base.token->pos,
                assign->lval->token->text, assign->lval->token->length);
        return;
    }

    if ((assign->lval->flags & Ast_Flag_Lval) == 0) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_NOT_LVAL,
                assign->base.token->pos,
                assign->lval->token->text, assign->lval->token->length);
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

static void check_return(OnyxSemPassState* state, AstReturn* retnode) {
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

static void check_if(OnyxSemPassState* state, AstIf* ifnode) {
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

static void check_while(OnyxSemPassState* state, AstWhile* whilenode) {
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

static void check_call(OnyxSemPassState* state, AstCall* call) {
    AstFunction* callee = (AstFunction *) call->callee;

    if (callee->base.kind == Ast_Kind_Symbol) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                callee->base.token->pos,
                callee->base.token->text, callee->base.token->length);
        return;
    }

    if (callee->base.kind != Ast_Kind_Function) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CALL_NON_FUNCTION,
                call->base.token->pos,
                callee->base.token->text, callee->base.token->length);
        return;
    }

    // NOTE: If we calling an intrinsic function, translate the
    // call into an intrinsic call node.
    if (callee->base.flags & Ast_Flag_Intrinsic) {
        call->base.kind = Ast_Kind_Intrinsic_Call;
        call->callee = NULL;

        onyx_token_null_toggle(callee->base.token);

        char* intr_name = callee->base.token->text;
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

        ((AstIntrinsicCall *)call)->intrinsic = intrinsic;

        onyx_token_null_toggle(callee->base.token);
    }

    call->base.type = callee->base.type;

    AstLocal* formal_param = callee->params;
    AstArgument* actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        check_expression(state, (AstTyped *) actual_param);

        if (formal_param->base.type != actual_param->base.type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_PARAM_TYPE_MISMATCH,
                    actual_param->value->token->pos,
                    callee->base.token->text, callee->base.token->length,
                    formal_param->base.type->name, arg_pos,
                    actual_param->base.type->name);
            return;
        }

        arg_pos++;
        formal_param = (AstLocal *) formal_param->base.next;
        actual_param = (AstArgument *) actual_param->base.next;
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

static void check_expression(OnyxSemPassState* state, AstTyped* expr) {
    switch (expr->kind) {
        case Ast_Kind_Binary_Op:
            expr->type = &builtin_types[TYPE_INFO_KIND_UNKNOWN];

            check_expression(state, ((AstBinaryOp *) expr)->left);
            check_expression(state, ((AstBinaryOp *) expr)->right);

            if (((AstBinaryOp *) expr)->left->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                        expr->token->pos,
                        NULL, 0);
                return;
            }

            if (((AstBinaryOp *) expr)->right->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                        expr->token->pos,
                        NULL, 0);
                return;
            }

            if (((AstBinaryOp *) expr)->left->type != ((AstBinaryOp *) expr)->right->type) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
                        expr->token->pos,
                        ((AstBinaryOp *) expr)->left->type->name,
                        ((AstBinaryOp *) expr)->right->type->name);
                return;
            }

            if (((AstBinaryOp *) expr)->operation >= Binary_Op_Equal
                    && ((AstBinaryOp *) expr)->operation <= Binary_Op_Greater_Equal) {
                expr->type = &builtin_types[TYPE_INFO_KIND_BOOL];
            } else {
                expr->type = ((AstBinaryOp *) expr)->left->type;
            }

            break;

        case Ast_Kind_Unary_Op:
            if (((AstUnaryOp *) expr)->operation != Unary_Op_Cast) {
                check_expression(state, ((AstUnaryOp *) expr)->expr);
                expr->type = ((AstUnaryOp *) expr)->expr->type;
            }
            break;

        case Ast_Kind_Call:
            check_call(state, (AstCall *) expr);
            break;

        case Ast_Kind_Block:
            check_block(state, (AstBlock *) expr);
            break;

        case Ast_Kind_Symbol:
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                    expr->token->pos,
                    expr->token->text, expr->token->length);
            break;

        case Ast_Kind_Local:
        case Ast_Kind_Param:
            if (!expr->type->is_known) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "local variable with unknown type");
            }
            break;

        case Ast_Kind_Global:
            if (!expr->type->is_known) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "global with unknown type");
            }
            break;

        case Ast_Kind_Argument:
            check_expression(state, ((AstArgument *) expr)->value);
            expr->type = ((AstArgument *) expr)->value->type;
            break;

        case Ast_Kind_Literal:
            // NOTE: Literal types should have been decided
            // in the parser (for now).
            assert(expr->type->is_known);
            break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void check_global(OnyxSemPassState* state, AstGlobal* global) {
    if (global->initial_value) {
        check_expression(state, global->initial_value);

        if (global->base.type->is_known) {
            if (global->base.type != global->initial_value->type) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_GLOBAL_TYPE_MISMATCH,
                        global->base.token->pos,
                        global->base.token->text, global->base.token->length,
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
        case Ast_Kind_Assignment: check_assignment(state, (AstAssign *) stmt); break;
        case Ast_Kind_Return:     check_return(state, (AstReturn *) stmt); break;
        case Ast_Kind_If:         check_if(state, (AstIf *) stmt); break;
        case Ast_Kind_While:      check_while(state, (AstWhile *) stmt); break;
        case Ast_Kind_Call:       check_call(state, (AstCall *) stmt); break;
        case Ast_Kind_Block:      check_block(state, (AstBlock *) stmt); break;

        default: break;
    }
}

static void check_statement_chain(OnyxSemPassState* state, AstNode* start) {
    while (start) {
        check_statement(state, start);
        start = start->next;
    }
}

static void check_block(OnyxSemPassState* state, AstBlock* block) {
    check_statement_chain(state, block->body);

    forll(AstLocal, local, block->locals->last_local, prev_local) {
        if (!local->base.type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                    local->base.token->pos,
                    local->base.token->text, local->base.token->length);
            return;
        }
    }
}

static void check_function(OnyxSemPassState* state, AstFunction* func) {
    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->base.next) {
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

    bh_arr_each(AstGlobal *, global, program->globals)
        check_global(state, *global);

    bh_arr_each(AstFunction *, function, program->functions)
        check_function(state, *function);
}
