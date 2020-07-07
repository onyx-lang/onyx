#define BH_DEBUG
#include "onyxsempass.h"

static void symbol_introduce(OnyxSemPassState* state, AstNode* symbol);
static b32 symbol_unique_introduce(OnyxSemPassState* state, AstNode* symbol);
static void symbol_remove(OnyxSemPassState* state, AstNode* symbol);
static AstNode* symbol_resolve(OnyxSemPassState* state, AstNode* symbol);
static void scope_enter(OnyxSemPassState* state, AstNodeScope* scope);
static AstNodeScope* scope_leave(OnyxSemPassState* state);
static void symres_local(OnyxSemPassState* state, AstNodeLocal** local);
static void symres_call(OnyxSemPassState* state, AstNodeCall* call);
static void symres_expression(OnyxSemPassState* state, AstNode** expr);
static void symres_assignment(OnyxSemPassState* state, AstNodeAssign* assign);
static void symres_return(OnyxSemPassState* state, AstNodeReturn* ret);
static void symres_if(OnyxSemPassState* state, AstNodeIf* ifnode);
static void symres_while(OnyxSemPassState* state, AstNodeWhile* whilenode);
static void symres_statement_chain(OnyxSemPassState* state, AstNode* walker, AstNode** trailer);
static b32 symres_statement(OnyxSemPassState* state, AstNode* stmt);
static void symres_block(OnyxSemPassState* state, AstNodeBlock* block);
static void symres_function(OnyxSemPassState* state, AstNodeFunction* func);

static void symbol_introduce(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    SemPassSymbol* sp_sym = (SemPassSymbol *) bh_alloc_item(state->allocator, SemPassSymbol);
    sp_sym->node = symbol;
    sp_sym->shadowed = NULL;

    if (bh_table_has(SemPassSymbol *, state->symbols, symbol->token->token)) {
        sp_sym->shadowed = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->token);
    }

    bh_table_put(SemPassSymbol *, state->symbols, symbol->token->token, sp_sym);

    if (symbol->kind == AST_NODE_KIND_LOCAL) {
        AstNodeLocal* local = (AstNodeLocal *) symbol;
        local->prev_local = state->curr_scope->last_local;
        state->curr_scope->last_local = local;
    }

    onyx_token_null_toggle(symbol->token);
}

static void symbol_remove(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->token);

    if (sp_sym->shadowed) {
        bh_table_put(SemPassSymbol *, state->symbols, symbol->token->token, sp_sym->shadowed);
    } else {
        bh_table_delete(SemPassSymbol *, state->symbols, symbol->token->token);
    }

    onyx_token_null_toggle(symbol->token);
}

static AstNode* symbol_resolve(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    if (!bh_table_has(SemPassSymbol *, state->symbols, symbol->token->token)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                symbol->token->pos,
                symbol->token->token);

        onyx_token_null_toggle(symbol->token);
        return symbol;
    }

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->token);

    onyx_token_null_toggle(symbol->token);
    return sp_sym->node;
}

static void scope_enter(OnyxSemPassState* state, AstNodeScope* scope) {
    scope->prev_scope = state->curr_scope;
    state->curr_scope = scope;
}

static AstNodeScope* scope_leave(OnyxSemPassState* state) {
    // NOTE: Can't leave a scope if there is no scope
    assert(state->curr_scope != NULL);

    for (AstNodeLocal *walker = state->curr_scope->last_local; walker != NULL; walker = walker->prev_local) {
        symbol_remove(state, (AstNode *) walker);
    }

    state->curr_scope = state->curr_scope->prev_scope;
    return state->curr_scope;
}

static b32 symbol_unique_introduce(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    // NOTE: If the function hasn't already been defined
    if (!bh_table_has(SemPassSymbol *, state->symbols, symbol->token->token)) {
        SemPassSymbol* sp_sym = bh_alloc_item(state->allocator, SemPassSymbol);
        sp_sym->node = symbol;
        sp_sym->shadowed = NULL;
        bh_table_put(SemPassSymbol *, state->symbols, symbol->token->token, sp_sym);
    } else {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CONFLICTING_GLOBALS,
                symbol->token->pos,
                symbol->token->token);

        // NOTE: I really wish C had defer...
        onyx_token_null_toggle(symbol->token);
        return 0;
    }

    onyx_token_null_toggle(symbol->token);
    return 1;
}

static void symres_local(OnyxSemPassState* state, AstNodeLocal** local) {
    symbol_introduce(state, (AstNode *) *local);
}

static void symres_call(OnyxSemPassState* state, AstNodeCall* call) {
    AstNode* callee = symbol_resolve(state, call->callee);
    if (callee) call->callee = callee;
    else DEBUG_HERE;

    symres_statement_chain(state, (AstNode *) call->arguments, (AstNode **) &call->arguments);
}

static void symres_expression(OnyxSemPassState* state, AstNode** expr) {
    switch ((*expr)->kind) {
        case AST_NODE_KIND_BIN_OP:
            symres_expression(state, (AstNode **) &((AstNodeBinOp *)(*expr))->left);
            symres_expression(state, (AstNode **) &((AstNodeBinOp *)(*expr))->right);
            break;

        case AST_NODE_KIND_UNARY_OP:
            symres_expression(state, (AstNode **) &((AstNodeUnaryOp *)(*expr))->expr);
            break;

        case AST_NODE_KIND_CALL: symres_call(state, (AstNodeCall *) *expr); break;

        case AST_NODE_KIND_BLOCK: symres_block(state, (AstNodeBlock *) *expr); break;

        case AST_NODE_KIND_SYMBOL:
            *expr = symbol_resolve(state, *expr);
            break;

        // NOTE: This is a good case, since it means the symbol is already resolved
        case AST_NODE_KIND_LOCAL: break;

        case AST_NODE_KIND_LITERAL: break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void symres_assignment(OnyxSemPassState* state, AstNodeAssign* assign) {
    AstNodeTyped* lval = (AstNodeTyped *) symbol_resolve(state, (AstNode *) assign->lval);
    if (lval == NULL) return;
    assign->lval = lval;

    symres_expression(state, (AstNode **) &assign->expr);
}

static void symres_return(OnyxSemPassState* state, AstNodeReturn* ret) {
    if (ret->expr)
        symres_expression(state, (AstNode **) &ret->expr);
}

static void symres_if(OnyxSemPassState* state, AstNodeIf* ifnode) {
    symres_expression(state, (AstNode **) &ifnode->cond);
    if (ifnode->true_block.as_if != NULL) {
        if (ifnode->true_block.as_if->base.kind == AST_NODE_KIND_BLOCK)
            symres_block(state, ifnode->true_block.as_block);

        else if (ifnode->true_block.as_if->base.kind == AST_NODE_KIND_IF)
            symres_if(state, ifnode->true_block.as_if);

        else DEBUG_HERE;
    }

    if (ifnode->false_block.as_if != NULL) {
        if (ifnode->false_block.as_if->base.kind == AST_NODE_KIND_BLOCK)
            symres_block(state, ifnode->false_block.as_block);

        else if (ifnode->false_block.as_if->base.kind == AST_NODE_KIND_IF)
            symres_if(state, ifnode->false_block.as_if);

        else DEBUG_HERE;
    }
}

static void symres_while(OnyxSemPassState* state, AstNodeWhile* whilenode) {
    symres_expression(state, (AstNode **) &whilenode->cond);
    symres_block(state, whilenode->body);
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(OnyxSemPassState* state, AstNode* stmt) {
    switch (stmt->kind) {
        case AST_NODE_KIND_LOCAL:      symres_local(state, (AstNodeLocal **) &stmt);                return 1;
        case AST_NODE_KIND_ASSIGNMENT: symres_assignment(state, (AstNodeAssign *) stmt);            return 0;
        case AST_NODE_KIND_RETURN:     symres_return(state, (AstNodeReturn *) stmt);                return 0;
        case AST_NODE_KIND_IF:         symres_if(state, (AstNodeIf *) stmt);                        return 0;
        case AST_NODE_KIND_WHILE:      symres_while(state, (AstNodeWhile *) stmt);                 return 0;
        case AST_NODE_KIND_CALL:       symres_call(state, (AstNodeCall *) stmt);                    return 0;
        case AST_NODE_KIND_ARGUMENT:   symres_expression(state, (AstNode **) &((AstNodeArgument *)stmt)->value); return 0;
        case AST_NODE_KIND_BLOCK:      symres_block(state, (AstNodeBlock *) stmt);                  return 0;

        default: return 0;
    }
}

static void symres_statement_chain(OnyxSemPassState* state, AstNode* walker, AstNode** trailer) {
    while (walker) {
        if (symres_statement(state, walker)) {
            *trailer = walker->next;

            AstNode* tmp = walker->next;
            walker->next = NULL;
            walker = tmp;
        } else {
            trailer = &walker->next;
            walker = walker->next;
        }
    }
}

static void symres_block(OnyxSemPassState* state, AstNodeBlock* block) {
    scope_enter(state, block->scope);
    if (block->body)
        symres_statement_chain(state, block->body, &block->body);
    scope_leave(state);
}

static void symres_function(OnyxSemPassState* state, AstNodeFunction* func) {
    for (AstNodeLocal *param = func->params; param != NULL; param = (AstNodeLocal *) param->base.next) {
        symbol_introduce(state, (AstNode *) param);
    }

    symres_block(state, func->body);

    for (AstNodeLocal *param = func->params; param != NULL; param = (AstNodeLocal *) param->base.next) {
        symbol_remove(state, (AstNode *) param);
    }
}

void onyx_resolve_symbols(OnyxSemPassState* state, OnyxProgram* program) {

    // NOTE: First, introduce all global symbols
    bh_arr_each(AstNodeGlobal *, global, program->globals)
        if (!symbol_unique_introduce(state, (AstNode *) *global)) return;

    bh_arr_each(AstNodeFunction *, function, program->functions)
        if (!symbol_unique_introduce(state, (AstNode *) *function)) return;

    bh_arr_each(AstNodeForeign *, foreign, program->foreigns) {
        AstNodeKind import_kind = (*foreign)->import->kind;

        if (import_kind == AST_NODE_KIND_FUNCTION || import_kind == AST_NODE_KIND_GLOBAL)
            if (!symbol_unique_introduce(state, (*foreign)->import)) return;
    }


    // NOTE: Then, resolve all symbols in all functions
    bh_arr_each(AstNodeFunction *, function, program->functions)
        symres_function(state, *function);
}
