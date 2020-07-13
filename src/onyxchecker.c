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
static void check_binaryop(OnyxSemPassState* state, AstBinaryOp* binop);
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

    if (assign->lval->type == NULL) {
        assign->lval->type = type_build_from_ast(state->node_allocator, assign->lval->type_node);
    }

    if ((assign->lval->flags & Ast_Flag_Const) != 0 && assign->lval->type != NULL) {
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

    if (assign->lval->type == NULL) {
        assign->lval->type = assign->expr->type;
    } else {
        if (!types_are_compatible(assign->lval->type, assign->expr->type)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_ASSIGNMENT_TYPE_MISMATCH,
                    assign->base.token->pos,
                    type_get_name(assign->lval->type),
                    type_get_name(assign->expr->type));
            return;
        }
    }
}

static void check_return(OnyxSemPassState* state, AstReturn* retnode) {
    if (retnode->expr) {
        check_expression(state, retnode->expr);

        if (!types_are_compatible(retnode->expr->type, state->expected_return_type)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_RETURN_MISMATCH,
                    retnode->expr->token->pos,
                    type_get_name(retnode->expr->type),
                    type_get_name(state->expected_return_type));
        }
    } else {
        if (state->expected_return_type->Basic.size > 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    retnode->base.token->pos,
                    "returning from non-void function without value");
        }
    }
}

static void check_if(OnyxSemPassState* state, AstIf* ifnode) {
    check_expression(state, ifnode->cond);

    if (ifnode->cond->type == NULL
            || ifnode->cond->type->kind != Type_Kind_Basic
            || ifnode->cond->type->Basic.kind != Basic_Kind_Bool) {

        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                ifnode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    if (ifnode->true_block.as_if)  check_statement(state, (AstNode *) ifnode->true_block.as_block);
    if (ifnode->false_block.as_if) check_statement(state, (AstNode *) ifnode->false_block.as_block);
}

static void check_while(OnyxSemPassState* state, AstWhile* whilenode) {
    check_expression(state, whilenode->cond);

    if (whilenode->cond->type == NULL
            || whilenode->cond->type->kind != Type_Kind_Basic
            || whilenode->cond->type->Basic.kind != Basic_Kind_Bool) {

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

    if (callee->base.type == NULL) {
        callee->base.type = type_build_from_ast(state->node_allocator, callee->base.type_node);
    }
    call->base.type = callee->base.type;

    AstLocal* formal_param = callee->params;
    AstArgument* actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        check_expression(state, (AstTyped *) actual_param);

        if (formal_param->base.type == NULL) {
            formal_param->base.type = type_build_from_ast(state->node_allocator, formal_param->base.type_node);
        }

        if (!types_are_compatible(formal_param->base.type, actual_param->base.type)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_PARAM_TYPE_MISMATCH,
                    actual_param->value->token->pos,
                    callee->base.token->text, callee->base.token->length,
                    type_get_name(formal_param->base.type),
                    arg_pos,
                    type_get_name(actual_param->base.type));
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

static void check_binaryop(OnyxSemPassState* state, AstBinaryOp* binop) {
    check_expression(state, binop->left);
    check_expression(state, binop->right);

    if (binop->left->type == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                binop->base.token->pos,
                NULL, 0);
        return;
    }

    if (binop->right->type == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                binop->base.token->pos,
                NULL, 0);
        return;
    }

    if (binop->left->type->kind == Type_Kind_Pointer
            || binop->right->type->kind == Type_Kind_Pointer
            || (binop->left->type->Basic.flags & Basic_Flag_Pointer)
            || (binop->right->type->Basic.flags & Basic_Flag_Pointer)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                binop->base.token->pos,
                "binary operations are not supported for pointers (yet).");
        return;
    }

    if (!types_are_compatible(binop->left->type, binop->right->type)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
                binop->base.token->pos,
                type_get_name(binop->left->type),
                type_get_name(binop->right->type));
        return;
    }

    if (binop->operation >= Binary_Op_Equal
            && binop->operation <= Binary_Op_Greater_Equal) {
        binop->base.type = &basic_types[Basic_Kind_Bool];
    } else {
        binop->base.type = binop->left->type;
    }
}

static void check_expression(OnyxSemPassState* state, AstTyped* expr) {
    if (expr->type == NULL) {
        expr->type = type_build_from_ast(state->node_allocator, expr->type_node);
    }

    switch (expr->kind) {
        case Ast_Kind_Binary_Op:
            check_binaryop(state, (AstBinaryOp *) expr);
            break;

        case Ast_Kind_Unary_Op:
            check_expression(state, ((AstUnaryOp *) expr)->expr);

            if (((AstUnaryOp *) expr)->operation != Unary_Op_Cast) {
                expr->type = ((AstUnaryOp *) expr)->expr->type;
            } else {
                expr->type = type_build_from_ast(state->node_allocator, expr->type_node);
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
            if (expr->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "local variable with unknown type");
            }
            break;

        case Ast_Kind_Global:
            if (expr->type == NULL) {
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
            assert(expr->type != NULL);
            break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void check_global(OnyxSemPassState* state, AstGlobal* global) {
    if (global->initial_value) {
        check_expression(state, global->initial_value);

        if (global->base.type == NULL) {
            global->base.type = type_build_from_ast(state->node_allocator, global->base.type_node);
        }

        if (global->base.type != NULL) {
            if (!types_are_compatible(global->base.type, global->initial_value->type)) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_GLOBAL_TYPE_MISMATCH,
                        global->base.token->pos,
                        global->base.token->text, global->base.token->length,
                        type_get_name(global->base.type),
                        type_get_name(global->initial_value->type));
                return;
            }
        } else {
            if (global->initial_value->type)
                global->base.type = global->initial_value->type;
        }
    }

    if (global->base.type == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                global->base.token->pos,
                "global variable with unknown type");
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
        if (local->base.type == NULL) {
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
        if (param->base.type == NULL) {
            param->base.type = type_build_from_ast(state->node_allocator, param->base.type_node);
        }

        if (param->base.type == NULL) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->base.token->pos,
                    "function parameter types must be known");
            return;
        }

        if (param->base.type->Basic.size == 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->base.token->pos,
                    "function parameters must have non-void types");
            return;
        }
    }

    if (func->base.type == NULL) {
        func->base.type = type_build_from_ast(state->node_allocator, func->base.type_node);
    }

    state->expected_return_type = func->base.type;
    if (func->body) {
        check_block(state, func->body);
    }
}

void onyx_type_check(OnyxSemPassState* state, OnyxProgram* program) {

    bh_arr_each(AstForeign *, foreign, program->foreigns)
        if ((*foreign)->import->kind == Ast_Kind_Function)
            check_function(state, (AstFunction *) (*foreign)->import);

    bh_arr_each(AstGlobal *, global, program->globals)
        check_global(state, *global);

    bh_arr_each(AstFunction *, function, program->functions)
        check_function(state, *function);
}
