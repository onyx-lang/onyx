#define BH_DEBUG
#include "onyxsempass.h"

static b32 check_function(SemState* state, AstFunction* func);
static b32 check_block(SemState* state, AstBlock* block);
static b32 check_statement_chain(SemState* state, AstNode* start);
static b32 check_statement(SemState* state, AstNode* stmt);
static b32 check_assignment(SemState* state, AstAssign* assign);
static b32 check_return(SemState* state, AstReturn* retnode);
static b32 check_if(SemState* state, AstIf* ifnode);
static b32 check_while(SemState* state, AstWhile* whilenode);
static b32 check_call(SemState* state, AstCall* call);
static b32 check_binaryop(SemState* state, AstBinaryOp* binop);
static b32 check_expression(SemState* state, AstTyped* expr);
static b32 check_global(SemState* state, AstGlobal* global);

static b32 check_assignment(SemState* state, AstAssign* assign) {
    if (assign->lval->kind == Ast_Kind_Symbol) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                assign->lval->token->pos,
                assign->lval->token->text, assign->lval->token->length);
        return 1;
    }

    if ((assign->lval->flags & Ast_Flag_Const) != 0 && assign->lval->type != NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_ASSIGN_CONST,
                assign->token->pos,
                assign->lval->token->text, assign->lval->token->length);
        return 1;
    }

    if ((assign->lval->flags & Ast_Flag_Lval) == 0) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_NOT_LVAL,
                assign->token->pos,
                assign->lval->token->text, assign->lval->token->length);
        return 1;
    }

    if (assign->lval->type == NULL) {
        assign->lval->type = type_build_from_ast(state->node_allocator, assign->lval->type_node);
    }

    if (check_expression(state, assign->expr)) return 1;

    if (assign->lval->type == NULL) {
        assign->lval->type = assign->expr->type;
    } else {
        if (!types_are_compatible(assign->lval->type, assign->expr->type)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_ASSIGNMENT_TYPE_MISMATCH,
                    assign->token->pos,
                    type_get_name(assign->lval->type),
                    type_get_name(assign->expr->type));
            return 1;
        }
    }

    return 0;
}

static b32 check_return(SemState* state, AstReturn* retnode) {
    if (retnode->expr) {
        if (check_expression(state, retnode->expr)) return 1;

        if (!types_are_compatible(retnode->expr->type, state->expected_return_type)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_RETURN_MISMATCH,
                    retnode->expr->token->pos,
                    type_get_name(retnode->expr->type),
                    type_get_name(state->expected_return_type));
            return 1;
        }
    } else {
        if (state->expected_return_type->Basic.size > 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    retnode->token->pos,
                    "returning from non-void function without value");
            return 1;
        }
    }

    return 0;
}

static b32 check_if(SemState* state, AstIf* ifnode) {
    if (check_expression(state, ifnode->cond)) return 1;

    if (!type_is_bool(ifnode->cond->type)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                ifnode->cond->token->pos,
                "expected boolean type for condition");
        return 1;
    }

    if (ifnode->true_block.as_if)  if (check_statement(state, (AstNode *) ifnode->true_block.as_block))  return 1;
    if (ifnode->false_block.as_if) if (check_statement(state, (AstNode *) ifnode->false_block.as_block)) return 1;

    return 0;
}

static b32 check_while(SemState* state, AstWhile* whilenode) {
    if (check_expression(state, whilenode->cond)) return 1;

    if (!type_is_bool(whilenode->cond->type)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                whilenode->cond->token->pos,
                "expected boolean type for condition");
        return 1;
    }

    return check_block(state, whilenode->body);
}

static b32 check_call(SemState* state, AstCall* call) {
    AstFunction* callee = (AstFunction *) call->callee;

    if (callee->kind == Ast_Kind_Symbol) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                callee->token->pos,
                callee->token->text, callee->token->length);
        return 1;
    }

    if (callee->type == NULL) {
        callee->type = type_build_from_ast(state->node_allocator, callee->type_node);
    }

    if (callee->type->kind != Type_Kind_Function) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CALL_NON_FUNCTION,
                call->token->pos,
                callee->token->text, callee->token->length);
        return 1;
    }

    // NOTE: If we calling an intrinsic function, translate the
    // call into an intrinsic call node.
    if (callee->flags & Ast_Flag_Intrinsic) {
        call->kind = Ast_Kind_Intrinsic_Call;
        call->callee = NULL;

        token_toggle_end(callee->intrinsic_name);

        char* intr_name = callee->intrinsic_name->text;
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

        token_toggle_end(callee->intrinsic_name);
    }

    call->type = callee->type->Function.return_type;

    AstLocal* formal_param = callee->params;
    AstArgument* actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        if (check_expression(state, (AstTyped *) actual_param)) return 1;

        if (formal_param->type == NULL) {
            formal_param->type = type_build_from_ast(state->node_allocator, formal_param->type_node);
        }

        if (!types_are_compatible(formal_param->type, actual_param->type)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_PARAM_TYPE_MISMATCH,
                    actual_param->token->pos,
                    callee->token->text, callee->token->length,
                    type_get_name(formal_param->type),
                    arg_pos,
                    type_get_name(actual_param->type));
            return 1;
        }

        arg_pos++;
        formal_param = (AstLocal *) formal_param->next;
        actual_param = (AstArgument *) actual_param->next;
    }

    if (formal_param != NULL && actual_param == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                call->token->pos,
                "too few arguments to function call");
        return 1;
    }

    if (formal_param == NULL && actual_param != NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                call->token->pos,
                "too many arguments to function call");
        return 1;
    }

    return 0;
}

static b32 check_binaryop(SemState* state, AstBinaryOp* binop) {
    if (check_expression(state, binop->left)) return 1;
    if (check_expression(state, binop->right)) return 1;

    if (binop->left->type == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                binop->token->pos,
                NULL, 0);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                binop->token->pos,
                NULL, 0);
        return 1;
    }

    if (type_is_pointer(binop->left->type)
            || type_is_pointer(binop->right->type)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                binop->token->pos,
                "binary operations are not supported for pointers (yet).");
        return 1;
    }

    if (!types_are_compatible(binop->left->type, binop->right->type)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
                binop->token->pos,
                type_get_name(binop->left->type),
                type_get_name(binop->right->type));
        return 1;
    }

    if (binop->operation >= Binary_Op_Equal
            && binop->operation <= Binary_Op_Greater_Equal) {
        binop->type = &basic_types[Basic_Kind_Bool];
    } else {
        binop->type = binop->left->type;
    }

    return 0;
}

static b32 check_expression(SemState* state, AstTyped* expr) {
    if (expr->kind > Ast_Kind_Type_Start && expr->kind < Ast_Kind_Type_End) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                (OnyxFilePos) { 0 },
                "type used as part of an expression");
        return 1;
    }

    if (expr->type == NULL) {
        expr->type = type_build_from_ast(state->node_allocator, expr->type_node);
    }

    i32 retval = 0;
    switch (expr->kind) {
        case Ast_Kind_Binary_Op: retval = check_binaryop(state, (AstBinaryOp *) expr); break;

        case Ast_Kind_Unary_Op:
            retval = check_expression(state, ((AstUnaryOp *) expr)->expr);

            if (((AstUnaryOp *) expr)->operation != Unary_Op_Cast) {
                expr->type = ((AstUnaryOp *) expr)->expr->type;
            }
            break;

        case Ast_Kind_Call:  retval = check_call(state, (AstCall *) expr); break;
        case Ast_Kind_Block: retval = check_block(state, (AstBlock *) expr); break;

        case Ast_Kind_Symbol:
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                    expr->token->pos,
                    expr->token->text, expr->token->length);
            retval = 1;
            break;

        case Ast_Kind_Local:
        case Ast_Kind_Param:
            if (expr->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "local variable with unknown type");
                retval = 1;
            }
            break;

        case Ast_Kind_Global:
            if (expr->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "global with unknown type");
                retval = 1;
            }
            break;

        case Ast_Kind_Argument:
            retval = check_expression(state, ((AstArgument *) expr)->value);
            expr->type = ((AstArgument *) expr)->value->type;
            break;

        case Ast_Kind_Literal:
            // NOTE: Literal types should have been decided
            // in the parser (for now).
            assert(expr->type != NULL);
            break;

        case Ast_Kind_Function: break;

        default:
            retval = 1;
            DEBUG_HERE;
            break;
    }

    return retval;
}

static b32 check_global(SemState* state, AstGlobal* global) {
    if (global->type == NULL)
        global->type = type_build_from_ast(state->allocator, global->type_node);

    if (global->type == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                global->token->pos,
                global->exported_name->text,
                global->exported_name->length);

        return 1;
    }

    return 0;
}

static b32 check_statement(SemState* state, AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Assignment: return check_assignment(state, (AstAssign *) stmt);
        case Ast_Kind_Return:     return check_return(state, (AstReturn *) stmt);
        case Ast_Kind_If:         return check_if(state, (AstIf *) stmt);
        case Ast_Kind_While:      return check_while(state, (AstWhile *) stmt);
        case Ast_Kind_Call:       return check_call(state, (AstCall *) stmt);
        case Ast_Kind_Block:      return check_block(state, (AstBlock *) stmt);

        default: return 0;
    }
}

static b32 check_statement_chain(SemState* state, AstNode* start) {
    while (start) {
        if (check_statement(state, start)) return 1;
        start = start->next;
    }

    return 0;
}

static b32 check_block(SemState* state, AstBlock* block) {
    if (check_statement_chain(state, block->body)) return 1;

    forll(AstLocal, local, block->locals->last_local, prev_local) {
        if (local->type == NULL) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                    local->token->pos,
                    local->token->text, local->token->length);
            return 1;
        }
    }

    return 0;
}

static b32 check_function(SemState* state, AstFunction* func) {
    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->next) {
        if (param->type == NULL) {
            param->type = type_build_from_ast(state->node_allocator, param->type_node);
        }

        if (param->type == NULL) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->token->pos,
                    "function parameter types must be known");
            return 1;
        }

        if (param->type->Basic.size == 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->token->pos,
                    "function parameters must have non-void types");
            return 1;
        }
    }

    if (func->type == NULL) {
        func->type = type_build_from_ast(state->node_allocator, func->type_node);
    }

    if ((func->flags & Ast_Flag_Exported) != 0) {
        if ((func->flags & Ast_Flag_Foreign) != 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    func->token->pos,
                    "exporting a foreign function");
            return 1;
        }

        if ((func->flags & Ast_Flag_Intrinsic) != 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    func->token->pos,
                    "exporting a intrinsic function");
            return 1;
        }

        if ((func->flags & Ast_Flag_Inline) != 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    func->token->pos,
                    "exporting a inlined function");
            return 1;
        }

        if (func->exported_name == NULL) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    func->token->pos,
                    "exporting function without a name");
            return 1;
        }
    }

    state->expected_return_type = func->type->Function.return_type;
    if (func->body) {
        return check_block(state, func->body);
    }

    return 0;
}

static b32 check_node(SemState* state, AstNode* node) {
    switch (node->kind) {
        case Ast_Kind_Function:     return check_function(state, (AstFunction *) node);
        case Ast_Kind_Block:        return check_block(state, (AstBlock *) node);
        case Ast_Kind_Assignment:   return check_assignment(state, (AstAssign *) node);
        case Ast_Kind_Return:       return check_return(state, (AstReturn *) node);
        case Ast_Kind_If:           return check_if(state, (AstIf *) node);
        case Ast_Kind_While:        return check_while(state, (AstWhile *) node);
        case Ast_Kind_Call:         return check_call(state, (AstCall *) node);
        case Ast_Kind_Binary_Op:    return check_binaryop(state, (AstBinaryOp *) node);
        default:                    return check_expression(state, (AstTyped *) node);
    }
}

void onyx_type_check(SemState* state, ParserOutput* program) {
    bh_arr_each(AstNode *, node, program->nodes_to_process) {
        check_node(state, *node);

        if ((*node)->kind == Ast_Kind_Function) {
            bh_arr_push(program->functions, (AstFunction *) *node);
        }

        if ((*node)->kind == Ast_Kind_Global) {
            bh_arr_push(program->globals, (AstGlobal *) *node);
        }
    }
}
