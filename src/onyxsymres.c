#define BH_DEBUG
#include "onyxsempass.h"

static void symbol_introduce(OnyxSemPassState* state, OnyxToken* tkn, AstNode* symbol);
static void symbol_basic_type_introduce(OnyxSemPassState* state, AstBasicType* basic_type);
static b32 symbol_unique_introduce(OnyxSemPassState* state, OnyxToken* tkn, AstNode* symbol);
static void symbol_remove(OnyxSemPassState* state, OnyxToken* tkn);
static AstNode* symbol_resolve(OnyxSemPassState* state, OnyxToken* tkn);
static void local_group_enter(OnyxSemPassState* state, AstLocalGroup* local_group);
static void local_group_leave(OnyxSemPassState* state);

static void symres_local(OnyxSemPassState* state, AstLocal** local);
static void symres_call(OnyxSemPassState* state, AstCall* call);
static void symres_expression(OnyxSemPassState* state, AstTyped** expr);
static void symres_assignment(OnyxSemPassState* state, AstAssign* assign);
static void symres_return(OnyxSemPassState* state, AstReturn* ret);
static void symres_if(OnyxSemPassState* state, AstIf* ifnode);
static void symres_while(OnyxSemPassState* state, AstWhile* whilenode);
static void symres_statement_chain(OnyxSemPassState* state, AstNode* walker, AstNode** trailer);
static b32  symres_statement(OnyxSemPassState* state, AstNode* stmt);
static void symres_block(OnyxSemPassState* state, AstBlock* block);
static void symres_function(OnyxSemPassState* state, AstFunction* func);
static AstType* symres_type(OnyxSemPassState* state, AstType* type);

static void symbol_introduce(OnyxSemPassState* state, OnyxToken* tkn, AstNode* symbol) {
    onyx_token_null_toggle(tkn);

    SemPassSymbol* sp_sym = (SemPassSymbol *) bh_alloc_item(state->allocator, SemPassSymbol);
    sp_sym->node = symbol;
    sp_sym->shadowed = NULL;

    if (bh_table_has(SemPassSymbol *, state->symbols, tkn->text)) {
        sp_sym->shadowed = bh_table_get(SemPassSymbol *, state->symbols, tkn->text);
    }

    bh_table_put(SemPassSymbol *, state->symbols, tkn->text, sp_sym);

    if (symbol->kind == Ast_Kind_Local) {
        AstLocal* local = (AstLocal *) symbol;
        local->prev_local = state->curr_local_group->last_local;
        state->curr_local_group->last_local = local;
    }

    onyx_token_null_toggle(tkn);
}

static void symbol_remove(OnyxSemPassState* state, OnyxToken* tkn) {
    onyx_token_null_toggle(tkn);

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, tkn->text);

    if (sp_sym->shadowed) {
        bh_table_put(SemPassSymbol *, state->symbols, tkn->text, sp_sym->shadowed);
    } else {
        bh_table_delete(SemPassSymbol *, state->symbols, tkn->text);
    }

    onyx_token_null_toggle(tkn);
}

static AstNode* symbol_resolve(OnyxSemPassState* state, OnyxToken* tkn) {
    onyx_token_null_toggle(tkn);

    if (!bh_table_has(SemPassSymbol *, state->symbols, tkn->text)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                tkn->pos,
                tkn->text);

        onyx_token_null_toggle(tkn);
        return NULL;
    }

    SemPassSymbol* sp_sym = bh_table_get(SemPassSymbol *, state->symbols, tkn->text);

    onyx_token_null_toggle(tkn);
    return sp_sym->node;
}

static void local_group_enter(OnyxSemPassState* state, AstLocalGroup* local_group) {
    local_group->prev_group = state->curr_local_group;
    state->curr_local_group = local_group;
}

static void local_group_leave(OnyxSemPassState* state) {
    assert(state->curr_local_group != NULL);

    for (AstLocal *walker = state->curr_local_group->last_local; walker != NULL; walker = walker->prev_local) {
        symbol_remove(state, walker->base.token);
    }

    state->curr_local_group = state->curr_local_group->prev_group;
}

static void symbol_basic_type_introduce(OnyxSemPassState* state, AstBasicType* basic_type) {
    SemPassSymbol* sp_sym = bh_alloc_item(state->allocator, SemPassSymbol);
    sp_sym->node = (AstNode *) basic_type;
    sp_sym->shadowed = NULL;
    bh_table_put(SemPassSymbol *, state->symbols, basic_type->base.name, sp_sym);
}

static b32 symbol_unique_introduce(OnyxSemPassState* state, OnyxToken* tkn, AstNode* symbol) {
    onyx_token_null_toggle(tkn);

    // NOTE: If the function hasn't already been defined
    if (!bh_table_has(SemPassSymbol *, state->symbols, tkn->text)) {
        SemPassSymbol* sp_sym = bh_alloc_item(state->allocator, SemPassSymbol);
        sp_sym->node = symbol;
        sp_sym->shadowed = NULL;
        bh_table_put(SemPassSymbol *, state->symbols, tkn->text, sp_sym);
    } else {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CONFLICTING_GLOBALS,
                tkn->pos,
                tkn->text);

        // NOTE: I really wish C had defer...
        onyx_token_null_toggle(tkn);
        return 0;
    }

    onyx_token_null_toggle(tkn);
    return 1;
}

static AstType* symres_type(OnyxSemPassState* state, AstType* type) {
    if (type == NULL) return NULL;

    if (type->kind == Ast_Kind_Symbol) {
        return (AstType *) symbol_resolve(state, ((AstNode *) type)->token);
    }

    // NOTE: Already resolved
    if (type->kind == Ast_Kind_Basic_Type) return type;

    if (type->kind == Ast_Kind_Pointer_Type) {
        ((AstPointerType *) type)->elem = symres_type(state, ((AstPointerType *) type)->elem);
        return type;
    }

    if (type->kind == Ast_Kind_Function_Type) {
        AstFunctionType* ftype = (AstFunctionType *) type;

        ftype->return_type = symres_type(state, ftype->return_type);

        if (ftype->param_count > 0)
            fori (i, 0, ftype->param_count - 1) {
                ftype->params[i] = symres_type(state, ftype->params[i]);
            }

        return type;
    }

    assert(("Bad type node", 0));
    return NULL;
}

static void symres_local(OnyxSemPassState* state, AstLocal** local) {
    (*local)->base.type_node = symres_type(state, (*local)->base.type_node);
    symbol_introduce(state, (*local)->base.token, (AstNode *) *local);
}

static void symres_call(OnyxSemPassState* state, AstCall* call) {
    AstNode* callee = symbol_resolve(state, call->callee->token);
    if (callee)
        call->callee = callee;
    else {
        onyx_token_null_toggle(call->callee->token);
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                call->callee->token->pos,
                call->callee->token->text);
        onyx_token_null_toggle(call->callee->token);
        return;
    }

    symres_statement_chain(state, (AstNode *) call->arguments, (AstNode **) &call->arguments);
}

static void symres_unaryop(OnyxSemPassState* state, AstUnaryOp** unaryop) {
    if ((*unaryop)->operation == Unary_Op_Cast) {
        (*unaryop)->base.type_node = symres_type(state, (*unaryop)->base.type_node);
    }

    symres_expression(state, &(*unaryop)->expr);
}

static void symres_expression(OnyxSemPassState* state, AstTyped** expr) {
    switch ((*expr)->kind) {
        case Ast_Kind_Binary_Op:
            symres_expression(state, &((AstBinaryOp *)(*expr))->left);
            symres_expression(state, &((AstBinaryOp *)(*expr))->right);
            break;

        case Ast_Kind_Unary_Op:
            symres_unaryop(state, (AstUnaryOp **) expr);
            break;

        case Ast_Kind_Call: symres_call(state, (AstCall *) *expr); break;

        case Ast_Kind_Block: symres_block(state, (AstBlock *) *expr); break;

        case Ast_Kind_Symbol:
            *expr = (AstTyped *) symbol_resolve(state, ((AstNode *) *expr)->token);
            break;

        // NOTE: This is a good case, since it means the symbol is already resolved
        case Ast_Kind_Local: break;

        case Ast_Kind_Function:
        case Ast_Kind_Literal:
            (*expr)->type_node = symres_type(state, (*expr)->type_node);
            break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void symres_assignment(OnyxSemPassState* state, AstAssign* assign) {
    AstTyped* lval = (AstTyped *) symbol_resolve(state, assign->lval->token);
    if (lval == NULL) return;
    assign->lval = lval;

    symres_expression(state, &assign->expr);
}

static void symres_return(OnyxSemPassState* state, AstReturn* ret) {
    if (ret->expr)
        symres_expression(state, &ret->expr);
}

static void symres_if(OnyxSemPassState* state, AstIf* ifnode) {
    symres_expression(state, &ifnode->cond);
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
    symres_expression(state, &whilenode->cond);
    symres_block(state, whilenode->body);
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(OnyxSemPassState* state, AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Local:      symres_local(state, (AstLocal **) &stmt);                return 1;
        case Ast_Kind_Assignment: symres_assignment(state, (AstAssign *) stmt);            return 0;
        case Ast_Kind_Return:     symres_return(state, (AstReturn *) stmt);                return 0;
        case Ast_Kind_If:         symres_if(state, (AstIf *) stmt);                        return 0;
        case Ast_Kind_While:      symres_while(state, (AstWhile *) stmt);                  return 0;
        case Ast_Kind_Call:       symres_call(state, (AstCall *) stmt);                    return 0;
        case Ast_Kind_Argument:   symres_expression(state, (AstTyped **) &((AstArgument *)stmt)->value); return 0;
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
        param->base.type_node = symres_type(state, param->base.type_node);

        symbol_introduce(state, param->base.token, (AstNode *) param);
    }

    if (func->base.type_node != NULL) {
        func->base.type_node = symres_type(state, func->base.type_node);
    }

    symres_block(state, func->body);

    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->base.next) {
        symbol_remove(state, param->base.token);
    }
}

static void symres_top_node(OnyxSemPassState* state, AstNode** node) {
    switch ((*node)->kind) {
        case Ast_Kind_Call:
        case Ast_Kind_Unary_Op:
        case Ast_Kind_Binary_Op:
        case Ast_Kind_Literal:
        case Ast_Kind_Symbol:
             symres_expression(state, (AstTyped **) node);
             break;

        case Ast_Kind_Function:
             symres_function(state, (AstFunction *) *node);
             break;

        default:
             DEBUG_HERE;
             break;
    }
}

void onyx_resolve_symbols(OnyxSemPassState* state, ParserOutput* program) {

    // NOTE: Add types to global scope
    symbol_basic_type_introduce(state, &basic_type_void);
    symbol_basic_type_introduce(state, &basic_type_bool);
    symbol_basic_type_introduce(state, &basic_type_i8);
    symbol_basic_type_introduce(state, &basic_type_u8);
    symbol_basic_type_introduce(state, &basic_type_i16);
    symbol_basic_type_introduce(state, &basic_type_u16);
    symbol_basic_type_introduce(state, &basic_type_i32);
    symbol_basic_type_introduce(state, &basic_type_u32);
    symbol_basic_type_introduce(state, &basic_type_i64);
    symbol_basic_type_introduce(state, &basic_type_u64);
    symbol_basic_type_introduce(state, &basic_type_f32);
    symbol_basic_type_introduce(state, &basic_type_f64);
    symbol_basic_type_introduce(state, &basic_type_rawptr);

    bh_arr_each(AstBinding *, binding, program->top_level_bindings)
        if (!symbol_unique_introduce(state, (*binding)->base.token, (*binding)->node)) return;

    bh_arr_each(AstNode *, node, program->nodes_to_process)
        symres_top_node(state, node);
}
