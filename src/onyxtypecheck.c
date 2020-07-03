#define BH_DEBUG
#include "onyxsempass.h"

static void typecheck_function(OnyxSemPassState* state, AstNodeFunction* func);
static void typecheck_block(OnyxSemPassState* state, AstNodeBlock* block);
static void typecheck_statement_chain(OnyxSemPassState* state, AstNode* start);
static void typecheck_statement(OnyxSemPassState* state, AstNode* stmt);
static void typecheck_assignment(OnyxSemPassState* state, AstNodeAssign* assign);
static void typecheck_return(OnyxSemPassState* state, AstNodeReturn* retnode);
static void typecheck_if(OnyxSemPassState* state, AstNodeIf* ifnode);
static void typecheck_while(OnyxSemPassState* state, AstNodeWhile* whilenode);
static void typecheck_call(OnyxSemPassState* state, AstNodeCall* call);
static void typecheck_expression(OnyxSemPassState* state, AstNodeTyped* expr);
static void typecheck_global(OnyxSemPassState* state, AstNodeGlobal* global);

static void typecheck_assignment(OnyxSemPassState* state, AstNodeAssign* assign) {
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

    typecheck_expression(state, assign->expr);

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

static void typecheck_return(OnyxSemPassState* state, AstNodeReturn* retnode) {
    if (retnode->expr) {
        typecheck_expression(state, retnode->expr);

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

static void typecheck_if(OnyxSemPassState* state, AstNodeIf* ifnode) {
    typecheck_expression(state, ifnode->cond);
    if (ifnode->cond->type != &builtin_types[TYPE_INFO_KIND_BOOL]) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                ifnode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    if (ifnode->true_block) typecheck_statement(state, ifnode->true_block);
    if (ifnode->false_block) typecheck_statement(state, ifnode->false_block);
}

static void typecheck_while(OnyxSemPassState* state, AstNodeWhile* whilenode) {
    typecheck_expression(state, whilenode->cond);
    if (whilenode->cond->type != &builtin_types[TYPE_INFO_KIND_BOOL]) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                whilenode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    typecheck_block(state, whilenode->body);
}

static void typecheck_call(OnyxSemPassState* state, AstNodeCall* call) {
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

    call->base.type = callee->base.type;

    AstNodeLocal* formal_param = callee->params;
    AstNodeArgument* actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        typecheck_expression(state, (AstNodeTyped *) actual_param);

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

static void typecheck_expression(OnyxSemPassState* state, AstNodeTyped* expr) {
    switch (expr->kind) {
        case AST_NODE_KIND_BIN_OP:
            expr->type = &builtin_types[TYPE_INFO_KIND_UNKNOWN];

            typecheck_expression(state, ((AstNodeBinOp *) expr)->left);
            typecheck_expression(state, ((AstNodeBinOp *) expr)->right);

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
                typecheck_expression(state, ((AstNodeUnaryOp *) expr)->expr);
                expr->type = ((AstNodeUnaryOp *) expr)->expr->type;
            }
            break;

        case AST_NODE_KIND_CALL:
            typecheck_call(state, (AstNodeCall *) expr);
            break;

        case AST_NODE_KIND_BLOCK:
            typecheck_block(state, (AstNodeBlock *) expr);
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
            typecheck_expression(state, ((AstNodeArgument *) expr)->value);
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

static void typecheck_global(OnyxSemPassState* state, AstNodeGlobal* global) {
    if (global->initial_value) {
        typecheck_expression(state, global->initial_value);

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

static void typecheck_statement(OnyxSemPassState* state, AstNode* stmt) {
    switch (stmt->kind) {
        case AST_NODE_KIND_ASSIGNMENT: typecheck_assignment(state, (AstNodeAssign *) stmt); break;
        case AST_NODE_KIND_RETURN:     typecheck_return(state, (AstNodeReturn *) stmt); break;
        case AST_NODE_KIND_IF:         typecheck_if(state, (AstNodeIf *) stmt); break;
        case AST_NODE_KIND_WHILE:      typecheck_while(state, (AstNodeWhile *) stmt); break;
        case AST_NODE_KIND_CALL:       typecheck_call(state, (AstNodeCall *) stmt); break;
        case AST_NODE_KIND_BLOCK:      typecheck_block(state, (AstNodeBlock *) stmt); break;

        default: break;
    }
}

static void typecheck_statement_chain(OnyxSemPassState* state, AstNode* start) {
    while (start) {
        typecheck_statement(state, start);
        start = start->next;
    }
}

static void typecheck_block(OnyxSemPassState* state, AstNodeBlock* block) {
    typecheck_statement_chain(state, block->body);

    forll(AstNodeLocal, local, block->scope->last_local, prev_local) {
        if (!local->base.type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                    local->base.token->pos,
                    local->base.token->token, local->base.token->length);
            return;
        }
    }
}

static void typecheck_function(OnyxSemPassState* state, AstNodeFunction* func) {
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
        typecheck_block(state, func->body);
    }
}

void onyx_type_check(OnyxSemPassState* state, OnyxProgram* program) {

    bh_arr_each(AstNodeGlobal *, global, program->globals)
        typecheck_global(state, *global);

    bh_arr_each(AstNodeFunction *, function, program->functions)
        typecheck_function(state, *function);
}
