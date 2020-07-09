#define BH_DEBUG
#include "onyxsempass.h"

static void symbol_introduce(OnyxSemPassState* state, AstNode* symbol);
static b32 symbol_unique_introduce(OnyxSemPassState* state, AstNode* symbol);
static void symbol_remove(OnyxSemPassState* state, AstNode* symbol);
static AstNode* symbol_resolve(OnyxSemPassState* state, AstNode* symbol);
static void local_group_enter(OnyxSemPassState* state, AstLocalGroup* local_group);
static void local_group_leave(OnyxSemPassState* state);
static void symres_local(OnyxSemPassState* state, AstLocal** local);
static void symres_call(OnyxSemPassState* state, AstCall* call);
static void symres_expression(OnyxSemPassState* state, AstNode** expr);
static void symres_assignment(OnyxSemPassState* state, AstAssign* assign);
static void symres_return(OnyxSemPassState* state, AstReturn* ret);
static void symres_if(OnyxSemPassState* state, AstIf* ifnode);
static void symres_while(OnyxSemPassState* state, AstWhile* whilenode);
static void symres_statement_chain(OnyxSemPassState* state, AstNode* walker, AstNode** trailer);
static b32 symres_statement(OnyxSemPassState* state, AstNode* stmt);
static void symres_block(OnyxSemPassState* state, AstBlock* block);
static void symres_function(OnyxSemPassState* state, AstFunction* func);

static void symbol_introduce(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    SemPassSymbol* sp_sym = (SemPassSymbol *) bh_alloc_item(state->allocator, SemPassSymbol);
    sp_sym->node = symbol;
    sp_sym->shadowed = NULL;

    if (bh_table_has(SemPassSymbol *, state->symbols, symbol->token->text)) {
        sp_sym->shadowed = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->text);
    }

    bh_table_put(SemPassSymbol *, state->symbols, symbol->token->text, sp_sym);

    if (symbol->kind == Ast_Kind_Local) {
        AstLocal* local = (AstLocal *) symbol;
        local->prev_local = state->curr_local_group->last_local;
        state->curr_local_group->last_local = local;
    }

    onyx_token_null_toggle(symbol->token);
}

static void symbol_remove(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->text);

    if (sp_sym->shadowed) {
        bh_table_put(SemPassSymbol *, state->symbols, symbol->token->text, sp_sym->shadowed);
    } else {
        bh_table_delete(SemPassSymbol *, state->symbols, symbol->token->text);
    }

    onyx_token_null_toggle(symbol->token);
}

static AstNode* symbol_resolve(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    if (!bh_table_has(SemPassSymbol *, state->symbols, symbol->token->text)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                symbol->token->pos,
                symbol->token->text);

        onyx_token_null_toggle(symbol->token);
        return symbol;
    }

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, symbol->token->text);

    onyx_token_null_toggle(symbol->token);
    return sp_sym->node;
}

static void local_group_enter(OnyxSemPassState* state, AstLocalGroup* local_group) {
    local_group->prev_group = state->curr_local_group;
    state->curr_local_group = local_group;
}

static void local_group_leave(OnyxSemPassState* state) {
    assert(state->curr_local_group != NULL);

    for (AstLocal *walker = state->curr_local_group->last_local; walker != NULL; walker = walker->prev_local) {
        symbol_remove(state, (AstNode *) walker);
    }

    state->curr_local_group = state->curr_local_group->prev_group;
}

static b32 symbol_unique_introduce(OnyxSemPassState* state, AstNode* symbol) {
    onyx_token_null_toggle(symbol->token);

    // NOTE: If the function hasn't already been defined
    if (!bh_table_has(SemPassSymbol *, state->symbols, symbol->token->text)) {
        SemPassSymbol* sp_sym = bh_alloc_item(state->allocator, SemPassSymbol);
        sp_sym->node = symbol;
        sp_sym->shadowed = NULL;
        bh_table_put(SemPassSymbol *, state->symbols, symbol->token->text, sp_sym);
    } else {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CONFLICTING_GLOBALS,
                symbol->token->pos,
                symbol->token->text);

        // NOTE: I really wish C had defer...
        onyx_token_null_toggle(symbol->token);
        return 0;
    }

    onyx_token_null_toggle(symbol->token);
    return 1;
}

static void symres_local(OnyxSemPassState* state, AstLocal** local) {
    symbol_introduce(state, (AstNode *) *local);
}

static void symres_call(OnyxSemPassState* state, AstCall* call) {
    AstNode* callee = symbol_resolve(state, call->callee);
    if (callee) call->callee = callee;
    else DEBUG_HERE;

    symres_statement_chain(state, (AstNode *) call->arguments, (AstNode **) &call->arguments);
}

static void symres_expression(OnyxSemPassState* state, AstNode** expr) {
    switch ((*expr)->kind) {
        case Ast_Kind_Binary_Op:
            symres_expression(state, (AstNode **) &((AstBinaryOp *)(*expr))->left);
            symres_expression(state, (AstNode **) &((AstBinaryOp *)(*expr))->right);
            break;

        case Ast_Kind_Unary_Op:
            symres_expression(state, (AstNode **) &((AstUnaryOp *)(*expr))->expr);
            break;

        case Ast_Kind_Call: symres_call(state, (AstCall *) *expr); break;

        case Ast_Kind_Block: symres_block(state, (AstBlock *) *expr); break;

        case Ast_Kind_Symbol:
            *expr = symbol_resolve(state, *expr);
            break;

        // NOTE: This is a good case, since it means the symbol is already resolved
        case Ast_Kind_Local: break;

        case Ast_Kind_Literal: break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void symres_assignment(OnyxSemPassState* state, AstAssign* assign) {
    AstTyped* lval = (AstTyped *) symbol_resolve(state, (AstNode *) assign->lval);
    if (lval == NULL) return;
    assign->lval = lval;

    symres_expression(state, (AstNode **) &assign->expr);
}

static void symres_return(OnyxSemPassState* state, AstReturn* ret) {
    if (ret->expr)
        symres_expression(state, (AstNode **) &ret->expr);
}

static void symres_if(OnyxSemPassState* state, AstIf* ifnode) {
    symres_expression(state, (AstNode **) &ifnode->cond);
    if (ifnode->true_block.as_if != NULL) {
        if (ifnode->true_block.as_if->base.kind == Ast_Kind_Block)
            symres_block(state, ifnode->true_block.as_block);

        else if (ifnode->true_block.as_if->base.kind == Ast_Kind_If)
            symres_if(state, ifnode->true_block.as_if);

        else DEBUG_HERE;
    }

    if (ifnode->false_block.as_if != NULL) {
        if (ifnode->false_block.as_if->base.kind == Ast_Kind_Block)
            symres_block(state, ifnode->false_block.as_block);

        else if (ifnode->false_block.as_if->base.kind == Ast_Kind_If)
            symres_if(state, ifnode->false_block.as_if);

        else DEBUG_HERE;
    }
}

static void symres_while(OnyxSemPassState* state, AstWhile* whilenode) {
    symres_expression(state, (AstNode **) &whilenode->cond);
    symres_block(state, whilenode->body);
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(OnyxSemPassState* state, AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Local:      symres_local(state, (AstLocal **) &stmt);                return 1;
        case Ast_Kind_Assignment: symres_assignment(state, (AstAssign *) stmt);            return 0;
        case Ast_Kind_Return:     symres_return(state, (AstReturn *) stmt);                return 0;
        case Ast_Kind_If:         symres_if(state, (AstIf *) stmt);                        return 0;
        case Ast_Kind_While:      symres_while(state, (AstWhile *) stmt);                 return 0;
        case Ast_Kind_Call:       symres_call(state, (AstCall *) stmt);                    return 0;
        case Ast_Kind_Argument:   symres_expression(state, (AstNode **) &((AstArgument *)stmt)->value); return 0;
        case Ast_Kind_Block:      symres_block(state, (AstBlock *) stmt);                  return 0;

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

static void symres_block(OnyxSemPassState* state, AstBlock* block) {
    local_group_enter(state, block->locals);
    if (block->body)
        symres_statement_chain(state, block->body, &block->body);
    local_group_leave(state);
}

static void symres_function(OnyxSemPassState* state, AstFunction* func) {
    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->base.next) {
        symbol_introduce(state, (AstNode *) param);
    }

    symres_block(state, func->body);

    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->base.next) {
        symbol_remove(state, (AstNode *) param);
    }
}

void onyx_resolve_symbols(OnyxSemPassState* state, OnyxProgram* program) {

    // NOTE: First, introduce all global symbols
    bh_arr_each(AstGlobal *, global, program->globals)
        if (!symbol_unique_introduce(state, (AstNode *) *global)) return;

    bh_arr_each(AstFunction *, function, program->functions)
        if (!symbol_unique_introduce(state, (AstNode *) *function)) return;

    bh_arr_each(AstForeign *, foreign, program->foreigns) {
        AstKind import_kind = (*foreign)->import->kind;

        if (import_kind == Ast_Kind_Function || import_kind == Ast_Kind_Global)
            if (!symbol_unique_introduce(state, (*foreign)->import)) return;
    }


    // NOTE: Then, resolve all symbols in all functions
    bh_arr_each(AstFunction *, function, program->functions)
        symres_function(state, *function);
}
