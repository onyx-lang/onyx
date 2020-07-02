#define BH_DEBUG
#include "onyxsempass.h"

static void typecheck_function(OnyxSemPassState* state, OnyxAstNodeFunction* func);
static void typecheck_block(OnyxSemPassState* state, OnyxAstNodeBlock* block);
static void typecheck_statement_chain(OnyxSemPassState* state, OnyxAstNode* start);
static void typecheck_statement(OnyxSemPassState* state, OnyxAstNode* stmt);
static void typecheck_assignment(OnyxSemPassState* state, OnyxAstNode* assign);
static void typecheck_return(OnyxSemPassState* state, OnyxAstNode* retnode);
static void typecheck_if(OnyxSemPassState* state, OnyxAstNodeIf* ifnode);
static void typecheck_while(OnyxSemPassState* state, OnyxAstNodeWhile* whilenode);
static void typecheck_call(OnyxSemPassState* state, OnyxAstNodeCall* call);
static void typecheck_expression(OnyxSemPassState* state, OnyxAstNode* expr);
static void typecheck_global(OnyxSemPassState* state, OnyxAstNodeGlobal* global);

static void typecheck_assignment(OnyxSemPassState* state, OnyxAstNode* assign) {
    if (assign->left->kind == ONYX_AST_NODE_KIND_SYMBOL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                assign->left->token->pos,
                assign->left->token->token, assign->left->token->length);
        return;
    }

    if ((assign->left->flags & ONYX_AST_FLAG_CONST) != 0 && assign->left->type->is_known) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_ASSIGN_CONST,
                assign->token->pos,
                assign->left->token->token, assign->left->token->length);
        return;
    }

    if ((assign->left->flags & ONYX_AST_FLAG_LVAL) == 0) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_NOT_LVAL,
                assign->token->pos,
                assign->left->token->token, assign->left->token->length);
        return;
    }

    typecheck_expression(state, assign->right);

    if (!assign->left->type->is_known) {
        assign->left->type = assign->right->type;
    } else {
        if (assign->left->type != assign->right->type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_ASSIGNMENT_TYPE_MISMATCH,
                    assign->token->pos,
                    assign->left->type->name, assign->right->type->name);
            return;
        }
    }
}

static void typecheck_return(OnyxSemPassState* state, OnyxAstNode* retnode) {
    if (retnode->left) {
        typecheck_expression(state, retnode->left);

        if (retnode->left->type != state->expected_return_type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_RETURN_MISMATCH,
                    retnode->left->token->pos,
                    retnode->left->type->name, state->expected_return_type->name);
        }
    } else {
        if (state->expected_return_type->size > 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    retnode->token->pos,
                    "returning from non-void function without value");
        }
    }
}

static void typecheck_if(OnyxSemPassState* state, OnyxAstNodeIf* ifnode) {
    typecheck_expression(state, ifnode->cond);
    if (ifnode->cond->type != &builtin_types[ONYX_TYPE_INFO_KIND_BOOL]) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                ifnode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    if (ifnode->true_block) typecheck_statement(state, ifnode->true_block);
    if (ifnode->false_block) typecheck_statement(state, ifnode->false_block);
}

static void typecheck_while(OnyxSemPassState* state, OnyxAstNodeWhile* whilenode) {
    typecheck_expression(state, whilenode->cond);
    if (whilenode->cond->type != &builtin_types[ONYX_TYPE_INFO_KIND_BOOL]) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                whilenode->cond->token->pos,
                "expected boolean type for condition");
        return;
    }

    typecheck_block(state, whilenode->body);
}

static void typecheck_call(OnyxSemPassState* state, OnyxAstNodeCall* call) {
    OnyxAstNodeFunction* callee = (OnyxAstNodeFunction *) call->callee;

    if (callee->kind == ONYX_AST_NODE_KIND_SYMBOL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                callee->token->pos,
                callee->token->token, callee->token->length);
        return;
    }

    if (callee->kind != ONYX_AST_NODE_KIND_FUNCTION) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CALL_NON_FUNCTION,
                call->token->pos,
                callee->token->token, callee->token->length);
        return;
    }

    call->type = callee->return_type;

    OnyxAstNodeParam* formal_param = callee->params;
    OnyxAstNode* actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        typecheck_expression(state, actual_param);

        if (formal_param->type != actual_param->type) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_FUNCTION_PARAM_TYPE_MISMATCH,
                    call->token->pos,
                    callee->token->token, callee->token->length,
                    formal_param->type->name, arg_pos,
                    actual_param->type->name);
            return;
        }

        arg_pos++;
        formal_param = formal_param->next;
        actual_param = actual_param->next;
    }

    if (formal_param != NULL && actual_param == NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                call->token->pos,
                "too few arguments to function call");
        return;
    }

    if (formal_param == NULL && actual_param != NULL) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_LITERAL,
                call->token->pos,
                "too many arguments to function call");
        return;
    }
}

static void typecheck_expression(OnyxSemPassState* state, OnyxAstNode* expr) {
    switch (expr->kind) {
        case ONYX_AST_NODE_KIND_BIN_OP:
            expr->type = &builtin_types[ONYX_TYPE_INFO_KIND_UNKNOWN];

            typecheck_expression(state, expr->left);
            typecheck_expression(state, expr->right);

            if (expr->left->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                        expr->token->pos,
                        NULL, 0);
                return;
            }

            if (expr->right->type == NULL) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                        expr->token->pos,
                        NULL, 0);
                return;
            }

            if (expr->left->type != expr->right->type) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
                        expr->token->pos,
                        expr->left->type->name,
                        expr->right->type->name);
                return;
            }

            if (expr->as_binop.operation >= ONYX_BINARY_OP_EQUAL
                    && expr->as_binop.operation <= ONYX_BINARY_OP_GREATER_EQUAL) {
                expr->type = &builtin_types[ONYX_TYPE_INFO_KIND_BOOL];
            } else {
                expr->type = expr->left->type;
            }

            break;

        case ONYX_AST_NODE_KIND_UNARY_OP:
            if (expr->as_unaryop.operation != ONYX_UNARY_OP_CAST) {
                typecheck_expression(state, expr->left);
                expr->type = expr->left->type;
            }
            break;

        case ONYX_AST_NODE_KIND_CALL:
            typecheck_call(state, &expr->as_call);
            break;

        case ONYX_AST_NODE_KIND_BLOCK:
            typecheck_block(state, &expr->as_block);
            break;

        case ONYX_AST_NODE_KIND_SYMBOL:
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,
                    expr->token->pos,
                    expr->token->token, expr->token->length);
            break;

        case ONYX_AST_NODE_KIND_LOCAL:
        case ONYX_AST_NODE_KIND_PARAM:
            if (!expr->type->is_known) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "local variable with unknown type");
            }
            break;

        case ONYX_AST_NODE_KIND_GLOBAL:
            if (!expr->type->is_known) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_LITERAL,
                        expr->token->pos,
                        "global with unknown type");
            }
            break;

        case ONYX_AST_NODE_KIND_ARGUMENT:
            typecheck_expression(state, expr->left);
            expr->type = expr->left->type;
            break;

        case ONYX_AST_NODE_KIND_LITERAL:
            // NOTE: Literal types should have been decided
            // in the parser (for now).
            assert(expr->type->is_known);
            break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void typecheck_global(OnyxSemPassState* state, OnyxAstNodeGlobal* global) {
    if (global->initial_value) {
        typecheck_expression(state, global->initial_value);

        if (global->type->is_known) {
            if (global->type != global->initial_value->type) {
                onyx_message_add(state->msgs,
                        ONYX_MESSAGE_TYPE_GLOBAL_TYPE_MISMATCH,
                        global->token->pos,
                        global->token->token, global->token->length,
                        global->type->name, global->initial_value->type->name);
                return;
            }
        } else {
            if (global->initial_value->type)
                global->type = global->initial_value->type;
        }

    } else {
        if (!global->type || !global->type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    global->token->pos,
                    "global variable with unknown type");
        }
    }
}

static void typecheck_statement(OnyxSemPassState* state, OnyxAstNode* stmt) {
    switch (stmt->kind) {
        case ONYX_AST_NODE_KIND_ASSIGNMENT: typecheck_assignment(state, stmt); break;
        case ONYX_AST_NODE_KIND_RETURN:     typecheck_return(state, stmt); break;
        case ONYX_AST_NODE_KIND_IF:         typecheck_if(state, &stmt->as_if); break;
        case ONYX_AST_NODE_KIND_WHILE:      typecheck_while(state, &stmt->as_while); break;
        case ONYX_AST_NODE_KIND_CALL:       typecheck_call(state, &stmt->as_call); break;
        case ONYX_AST_NODE_KIND_BLOCK:      typecheck_block(state, &stmt->as_block); break;

        default: break;
    }
}

static void typecheck_statement_chain(OnyxSemPassState* state, OnyxAstNode* start) {
    while (start) {
        typecheck_statement(state, start);
        start = start->next;
    }
}

static void typecheck_block(OnyxSemPassState* state, OnyxAstNodeBlock* block) {
    typecheck_statement_chain(state, block->body);

    forll(OnyxAstNodeLocal, local, block->scope->last_local, prev_local) {
        if (!local->type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
                    local->token->pos,
                    local->token->token, local->token->length);
            return;
        }
    }
}

static void typecheck_function(OnyxSemPassState* state, OnyxAstNodeFunction* func) {
    forll(OnyxAstNodeParam, param, func->params, next) {
        if (!param->type->is_known) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->token->pos,
                    "function parameter types must be known");
            return;
        }

        if (param->type->size == 0) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_LITERAL,
                    param->token->pos,
                    "function parameters must have non-void types");
            return;
        }
    }

    state->expected_return_type = func->return_type;
    if (func->body) {
        typecheck_block(state, func->body);
    }
}

void onyx_type_check(OnyxSemPassState* state, OnyxProgram* program) {

    bh_arr_each(OnyxAstNodeGlobal *, global, program->globals)
        typecheck_global(state, *global);

    bh_arr_each(OnyxAstNodeFunction *, function, program->functions)
        typecheck_function(state, *function);
}
