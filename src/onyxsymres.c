#define BH_DEBUG
#include "onyxsempass.h"

static void symbol_introduce(SemState* state, OnyxToken* tkn, AstNode* symbol);
static void symbol_basic_type_introduce(SemState* state, AstBasicType* basic_type);
static b32 symbol_unique_introduce(SemState* state, OnyxToken* tkn, AstNode* symbol);
static void symbol_remove(SemState* state, OnyxToken* tkn);
static AstNode* symbol_resolve(SemState* state, OnyxToken* tkn);
static void local_group_enter(SemState* state, AstLocalGroup* local_group);
static void local_group_leave(SemState* state);

static AstType* symres_type(SemState* state, AstType* type);
static void symres_local(SemState* state, AstLocal** local);
static void symres_call(SemState* state, AstCall* call);
static void symres_expression(SemState* state, AstTyped** expr);
static void symres_assignment(SemState* state, AstAssign* assign);
static void symres_return(SemState* state, AstReturn* ret);
static void symres_if(SemState* state, AstIf* ifnode);
static void symres_while(SemState* state, AstWhile* whilenode);
static void symres_statement_chain(SemState* state, AstNode* walker, AstNode** trailer);
static b32  symres_statement(SemState* state, AstNode* stmt);
static void symres_block(SemState* state, AstBlock* block);
static void symres_function(SemState* state, AstFunction* func);
static void symres_global(SemState* state, AstGlobal* global);

static void symbol_introduce(SemState* state, OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    SemSymbol* sp_sym = (SemSymbol *) bh_alloc_item(state->allocator, SemSymbol);
    sp_sym->node = symbol;
    sp_sym->shadowed = NULL;

    if (bh_table_has(SemSymbol *, state->symbols, tkn->text)) {
        sp_sym->shadowed = bh_table_get(SemSymbol *, state->symbols, tkn->text);
    }

    bh_table_put(SemSymbol *, state->symbols, tkn->text, sp_sym);

    if (symbol->kind == Ast_Kind_Local) {
        AstLocal* local = (AstLocal *) symbol;
        local->prev_local = state->curr_local_group->last_local;
        state->curr_local_group->last_local = local;

        bh_arr_push(state->curr_function->locals, local);
    }

    token_toggle_end(tkn);
}

static void symbol_remove(SemState* state, OnyxToken* tkn) {
    token_toggle_end(tkn);

    SemSymbol* sp_sym = bh_table_get(SemSymbol *, state->symbols, tkn->text);

    if (sp_sym->shadowed) {
        bh_table_put(SemSymbol *, state->symbols, tkn->text, sp_sym->shadowed);
    } else {
        bh_table_delete(SemSymbol *, state->symbols, tkn->text);
    }

    token_toggle_end(tkn);
}

static AstNode* symbol_resolve(SemState* state, OnyxToken* tkn) {
    AstNode* res = NULL;

    while (res == NULL || res->kind == Ast_Kind_Symbol) {
        token_toggle_end(tkn);

        if (!bh_table_has(SemSymbol *, state->symbols, tkn->text)) {
            onyx_message_add(state->msgs,
                    ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                    tkn->pos,
                    tkn->text);

            token_toggle_end(tkn);
            return NULL;
        }

        res = bh_table_get(SemSymbol *, state->symbols, tkn->text)->node;
        token_toggle_end(tkn);

        tkn = res->token;
    }

    return res;
}

static void local_group_enter(SemState* state, AstLocalGroup* local_group) {
    local_group->prev_group = state->curr_local_group;
    state->curr_local_group = local_group;
}

static void local_group_leave(SemState* state) {
    assert(state->curr_local_group != NULL);

    for (AstLocal *walker = state->curr_local_group->last_local; walker != NULL; walker = walker->prev_local) {
        symbol_remove(state, walker->token);
    }

    state->curr_local_group = state->curr_local_group->prev_group;
}

static void symbol_basic_type_introduce(SemState* state, AstBasicType* basic_type) {
    SemSymbol* sp_sym = bh_alloc_item(state->allocator, SemSymbol);
    sp_sym->node = (AstNode *) basic_type;
    sp_sym->shadowed = NULL;
    bh_table_put(SemSymbol *, state->symbols, basic_type->name, sp_sym);
}

static b32 symbol_unique_introduce(SemState* state, OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    // NOTE: If the function hasn't already been defined
    if (!bh_table_has(SemSymbol *, state->symbols, tkn->text)) {
        SemSymbol* sp_sym = bh_alloc_item(state->allocator, SemSymbol);
        sp_sym->node = symbol;
        sp_sym->shadowed = NULL;
        bh_table_put(SemSymbol *, state->symbols, tkn->text, sp_sym);
    } else {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_CONFLICTING_GLOBALS,
                tkn->pos,
                tkn->text);

        // NOTE: I really wish C had defer...
        token_toggle_end(tkn);
        return 0;
    }

    token_toggle_end(tkn);
    return 1;
}

static AstType* symres_type(SemState* state, AstType* type) {
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

static void symres_local(SemState* state, AstLocal** local) {
    (*local)->type_node = symres_type(state, (*local)->type_node);

    symbol_introduce(state, (*local)->token, (AstNode *) *local);
}

static void symres_call(SemState* state, AstCall* call) {
    AstNode* callee = symbol_resolve(state, call->callee->token);
    if (callee)
        call->callee = callee;
    else {
        token_toggle_end(call->callee->token);
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                call->callee->token->pos,
                call->callee->token->text);
        token_toggle_end(call->callee->token);
        return;
    }

    symres_statement_chain(state, (AstNode *) call->arguments, (AstNode **) &call->arguments);
}

static void symres_unaryop(SemState* state, AstUnaryOp** unaryop) {
    if ((*unaryop)->operation == Unary_Op_Cast) {
        (*unaryop)->type_node = symres_type(state, (*unaryop)->type_node);
    }

    symres_expression(state, &(*unaryop)->expr);
}

static void symres_expression(SemState* state, AstTyped** expr) {
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

static void symres_assignment(SemState* state, AstAssign* assign) {
    AstTyped* lval = (AstTyped *) symbol_resolve(state, assign->lval->token);
    if (lval == NULL) return;
    assign->lval = lval;

    symres_expression(state, &assign->expr);
}

static void symres_return(SemState* state, AstReturn* ret) {
    if (ret->expr)
        symres_expression(state, &ret->expr);
}

static void symres_if(SemState* state, AstIf* ifnode) {
    symres_expression(state, &ifnode->cond);

    // BUG: This will not work for the following case:
    //  if cond foo := 10
    //  else foo := 20
    //
    // The declaration will cause a problem but semantically the above
    // doesn't make sense.
    if (ifnode->true_stmt != NULL)  symres_statement(state, ifnode->true_stmt);
    if (ifnode->false_stmt != NULL) symres_statement(state, ifnode->false_stmt);
}

static void symres_while(SemState* state, AstWhile* whilenode) {
    symres_expression(state, &whilenode->cond);
    symres_statement(state, whilenode->stmt);
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(SemState* state, AstNode* stmt) {
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

static void symres_statement_chain(SemState* state, AstNode* walker, AstNode** trailer) {
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

static void symres_block(SemState* state, AstBlock* block) {
    local_group_enter(state, block->locals);
    if (block->body)
        symres_statement_chain(state, block->body, &block->body);
    local_group_leave(state);
}

static void symres_function(SemState* state, AstFunction* func) {
    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->next) {
        param->type_node = symres_type(state, param->type_node);

        symbol_introduce(state, param->token, (AstNode *) param);
    }

    if (func->type_node != NULL) {
        func->type_node = symres_type(state, func->type_node);
    }

    state->curr_function = func;
    symres_block(state, func->body);

    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->next) {
        symbol_remove(state, param->token);
    }
}

static void symres_global(SemState* state, AstGlobal* global) {
    global->type_node = symres_type(state, global->type_node);
}

static void symres_top_node(SemState* state, AstNode** node) {
    switch ((*node)->kind) {
        case Ast_Kind_Call:
        case Ast_Kind_Unary_Op:
        case Ast_Kind_Binary_Op:
        case Ast_Kind_Literal:
        case Ast_Kind_Symbol:
             symres_expression(state, (AstTyped **) node);
             break;

        case Ast_Kind_Global:
             symres_global(state, (AstGlobal *) *node);
             break;

        case Ast_Kind_Function:
             symres_function(state, (AstFunction *) *node);
             break;

        default:
             DEBUG_HERE;
             break;
    }
}

void onyx_resolve_symbols(SemState* state, ParserOutput* program) {

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
        if (!symbol_unique_introduce(state, (*binding)->token, (*binding)->node)) return;

    bh_arr_each(AstNode *, node, program->nodes_to_process)
        symres_top_node(state, node);
}
