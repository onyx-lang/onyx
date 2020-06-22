#define BH_DEBUG
#include "onyxsempass.h"

static void symbol_introduce(OnyxSemPassState* state, OnyxAstNode* symbol);
static void symbol_remove(OnyxSemPassState* state, OnyxAstNode* symbol);
static OnyxAstNode* symbol_resolve(OnyxSemPassState* state, OnyxAstNode* symbol);
static void scope_enter(OnyxSemPassState* state, OnyxAstNodeScope* scope);
static OnyxAstNodeScope* scope_leave(OnyxSemPassState* state);
static b32 define_function(OnyxSemPassState* state, OnyxAstNodeFuncDef* func);
static void symres_local(OnyxSemPassState* state, OnyxAstNodeLocal** local);
static void symres_call(OnyxSemPassState* state, OnyxAstNode* call);
static void symres_expression(OnyxSemPassState* state, OnyxAstNode** expr);
static void symres_assignment(OnyxSemPassState* state, OnyxAstNode* assign);
static void symres_return(OnyxSemPassState* state, OnyxAstNode* ret);
static void symres_if(OnyxSemPassState* state, OnyxAstNodeIf* ifnode);
static void symres_statement_chain(OnyxSemPassState* state, OnyxAstNode* walker, OnyxAstNode** trailer);
static b32 symres_statement(OnyxSemPassState* state, OnyxAstNode* stmt);
static void symres_block(OnyxSemPassState* state, OnyxAstNodeBlock* block);
static void symres_function_definition(OnyxSemPassState* state, OnyxAstNodeFuncDef* func);

static void symbol_introduce(OnyxSemPassState* state, OnyxAstNode* symbol) {
    onyx_token_null_toggle(*symbol->token);

    SemPassSymbol* sp_sym = (SemPassSymbol *) bh_alloc_item(state->allocator, SemPassSymbol);
    sp_sym->node = symbol;
    sp_sym->shadowed = NULL;

    if (bh_table_has(SemPassSymbol *, state->symbols, symbol->token->token)) {
        sp_sym->shadowed = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->token);
    }

    bh_table_put(SemPassSymbol *, state->symbols, symbol->token->token, sp_sym);

    if (symbol->kind == ONYX_AST_NODE_KIND_LOCAL) {
        OnyxAstNodeLocal* local = &symbol->as_local;
        local->prev_local = state->curr_scope->last_local;
        state->curr_scope->last_local = local;
    }

    onyx_token_null_toggle(*symbol->token);
}

static void symbol_remove(OnyxSemPassState* state, OnyxAstNode* symbol) {
    onyx_token_null_toggle(*symbol->token);

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->token);

    if (sp_sym->shadowed) {
        bh_table_put(SemPassSymbol *, state->symbols, symbol->token->token, sp_sym->shadowed);
    } else {
        bh_table_delete(SemPassSymbol *, state->symbols, symbol->token->token);
    }

    onyx_token_null_toggle(*symbol->token);
}

static OnyxAstNode* symbol_resolve(OnyxSemPassState* state, OnyxAstNode* symbol) {
    onyx_token_null_toggle(*symbol->token);

    if (!bh_table_has(SemPassSymbol *, state->symbols, symbol->token->token)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                symbol->token->pos,
                symbol->token->token);

        onyx_token_null_toggle(*symbol->token);
        return NULL;
    }

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->token);

    onyx_token_null_toggle(*symbol->token);
    return sp_sym->node;
}

static void scope_enter(OnyxSemPassState* state, OnyxAstNodeScope* scope) {
	scope->prev_scope = state->curr_scope;
	state->curr_scope = scope;
}

static OnyxAstNodeScope* scope_leave(OnyxSemPassState* state) {
	// NOTE: Can't leave a scope if there is no scope
	assert(state->curr_scope != NULL);

	for (OnyxAstNodeLocal *walker = state->curr_scope->last_local; walker != NULL; walker = walker->prev_local) {
		symbol_remove(state, (OnyxAstNode *) walker);
	}

	state->curr_scope = state->curr_scope->prev_scope;
	return state->curr_scope;
}

static b32 define_function(OnyxSemPassState* state, OnyxAstNodeFuncDef* func) {
    onyx_token_null_toggle(*func->token);

    // NOTE: If the function hasn't already been defined
    if (!bh_table_has(SemPassSymbol *, state->symbols, func->token->token)) {
        SemPassSymbol* sp_sym = bh_alloc_item(state->allocator, SemPassSymbol);
        sp_sym->node = (OnyxAstNode *) func;
        sp_sym->shadowed = NULL;
        bh_table_put(SemPassSymbol *, state->symbols, func->token->token, sp_sym);
    } else {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_FUNCTION_REDEFINITION,
                func->token->pos,
                func->token->token);

        // NOTE: I really wish C had defer...
        onyx_token_null_toggle(*func->token);
        return 0;
    }

    onyx_token_null_toggle(*func->token);
    return 1;
}

static void symres_local(OnyxSemPassState* state, OnyxAstNodeLocal** local) {
    symbol_introduce(state, (OnyxAstNode *) *local);
}

static void symres_call(OnyxSemPassState* state, OnyxAstNode* stmt) {
    OnyxAstNodeCall* call = &stmt->as_call;

    OnyxAstNode* callee = symbol_resolve(state, call->callee);
    if (callee) call->callee = callee;
    else DEBUG_HERE;

    symres_statement_chain(state, call->arguments, &call->arguments);
}

static void symres_expression(OnyxSemPassState* state, OnyxAstNode** expr) {
    switch ((*expr)->kind) {
        case ONYX_AST_NODE_KIND_ADD:
        case ONYX_AST_NODE_KIND_MINUS:
        case ONYX_AST_NODE_KIND_MULTIPLY:
        case ONYX_AST_NODE_KIND_DIVIDE:
        case ONYX_AST_NODE_KIND_MODULUS:
        case ONYX_AST_NODE_KIND_EQUAL:
        case ONYX_AST_NODE_KIND_NOT_EQUAL:
        case ONYX_AST_NODE_KIND_LESS:
        case ONYX_AST_NODE_KIND_LESS_EQUAL:
        case ONYX_AST_NODE_KIND_GREATER:
        case ONYX_AST_NODE_KIND_GREATER_EQUAL:
            symres_expression(state, &(*expr)->left);
            symres_expression(state, &(*expr)->right);
            break;

        case ONYX_AST_NODE_KIND_NEGATE:
            symres_expression(state, &(*expr)->left);
            break;

        case ONYX_AST_NODE_KIND_CAST:
            if ((*expr)->type == NULL) {
                DEBUG_HERE;
                return;
            }
            symres_expression(state, &(*expr)->left);
            break;

        case ONYX_AST_NODE_KIND_CALL: symres_call(state, *expr); break;

        case ONYX_AST_NODE_KIND_BLOCK: symres_block(state, &(*expr)->as_block);

        case ONYX_AST_NODE_KIND_SYMBOL:
            *expr = symbol_resolve(state, *expr);
            break;

        // NOTE: This is a good case, since it means the symbol is already resolved
        case ONYX_AST_NODE_KIND_LOCAL: break;

        case ONYX_AST_NODE_KIND_LITERAL: break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void symres_assignment(OnyxSemPassState* state, OnyxAstNode* assign) {
    OnyxAstNode* lval = symbol_resolve(state, assign->left);
    if (lval == NULL) return;
    assign->left = lval;

    symres_expression(state, &assign->right);
}

static void symres_return(OnyxSemPassState* state, OnyxAstNode* ret) {
    if (ret->left)
        symres_expression(state, &ret->left);
}

static void symres_if(OnyxSemPassState* state, OnyxAstNodeIf* ifnode) {
    symres_expression(state, &ifnode->cond);
    if (ifnode->true_block) {
        if (ifnode->true_block->kind == ONYX_AST_NODE_KIND_BLOCK)
            symres_block(state, &ifnode->true_block->as_block);

        else if (ifnode->true_block->kind == ONYX_AST_NODE_KIND_IF)
            symres_if(state, &ifnode->true_block->as_if);

        else DEBUG_HERE;
    }

    if (ifnode->false_block) {
        if (ifnode->false_block->kind == ONYX_AST_NODE_KIND_BLOCK)
            symres_block(state, &ifnode->false_block->as_block);

        else if (ifnode->false_block->kind == ONYX_AST_NODE_KIND_IF)
            symres_if(state, &ifnode->false_block->as_if);

        else DEBUG_HERE;
    }
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(OnyxSemPassState* state, OnyxAstNode* stmt) {
    switch (stmt->kind) {
        case ONYX_AST_NODE_KIND_LOCAL:      symres_local(state, (OnyxAstNodeLocal **) &stmt);       return 1;
        case ONYX_AST_NODE_KIND_ASSIGNMENT: symres_assignment(state, stmt);                         return 0;
		case ONYX_AST_NODE_KIND_RETURN:     symres_return(state, stmt);                             return 0;
        case ONYX_AST_NODE_KIND_IF:         symres_if(state, &stmt->as_if);                         return 0;
        case ONYX_AST_NODE_KIND_CALL:       symres_call(state, stmt);                               return 0;
        case ONYX_AST_NODE_KIND_ARGUMENT:   symres_expression(state, (OnyxAstNode **) &stmt->left); return 0;
        case ONYX_AST_NODE_KIND_BLOCK:      symres_block(state, &stmt->as_block);                   return 0;

        default: return 0;
    }
}

static void symres_statement_chain(OnyxSemPassState* state, OnyxAstNode* walker, OnyxAstNode** trailer) {
    while (walker) {
        if (symres_statement(state, walker)) {
            *trailer = walker->next;

            OnyxAstNode* tmp = walker->next;
            walker->next = NULL;
            walker = tmp;
        } else {
            trailer = &walker->next;
            walker = walker->next;
        }
    }
}

static void symres_block(OnyxSemPassState* state, OnyxAstNodeBlock* block) {
    scope_enter(state, block->scope);
    if (block->body)
        symres_statement_chain(state, block->body, &block->body);
    scope_leave(state);
}

static void symres_function_definition(OnyxSemPassState* state, OnyxAstNodeFuncDef* func) {
    forll(OnyxAstNodeParam, param, func->params, next) {
        symbol_introduce(state, (OnyxAstNode *) param);
    }

    symres_block(state, func->body);

    forll(OnyxAstNodeParam, param, func->params, next) {
        symbol_remove(state, (OnyxAstNode *) param);
    }
}

void onyx_resolve_symbols(OnyxSemPassState* state, OnyxAstNode* root_node) {
    OnyxAstNode* walker = root_node;
    while (walker) {
        switch (walker->kind) {
            case ONYX_AST_NODE_KIND_FUNCDEF:
                if (!define_function(state, &walker->as_funcdef)) return;
                break;

            case ONYX_AST_NODE_KIND_FOREIGN:
                if (walker->as_foreign.import->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
                    if (!define_function(state, &walker->as_foreign.import->as_funcdef)) return;
                }
                break;

            default: break;
        }

        walker = walker->next;
    }

    // NOTE: First, resolve all symbols
    walker = root_node;
    while (walker) {
        switch (walker->kind) {
            case ONYX_AST_NODE_KIND_FUNCDEF:
                symres_function_definition(state, &walker->as_funcdef);
                break;
            default: break;
        }

        walker = walker->next;
    }
}
