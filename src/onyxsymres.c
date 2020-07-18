#define BH_DEBUG
#include "onyxsempass.h"

static b32  symbol_introduce(SemState* state, OnyxToken* tkn, AstNode* symbol);
static AstNode* symbol_resolve(SemState* state, OnyxToken* tkn);
static void symbol_basic_type_introduce(SemState* state, AstBasicType* basic_type);
static void scope_enter(SemState* state, Scope* new_scope);
static void scope_leave(SemState* state);

static AstType* symres_type(SemState* state, AstType* type);
static void symres_local(SemState* state, AstLocal** local);
static void symres_call(SemState* state, AstCall* call);
static void symres_expression(SemState* state, AstTyped** expr);
static void symres_return(SemState* state, AstReturn* ret);
static void symres_if(SemState* state, AstIf* ifnode);
static void symres_while(SemState* state, AstWhile* whilenode);
static void symres_statement_chain(SemState* state, AstNode* walker, AstNode** trailer);
static b32  symres_statement(SemState* state, AstNode* stmt);
static void symres_block(SemState* state, AstBlock* block);
static void symres_function(SemState* state, AstFunction* func);
static void symres_global(SemState* state, AstGlobal* global);
static void symres_overloaded_function(SemState* state, AstOverloadedFunction* ofunc);

static b32 symbol_introduce(SemState* state, OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    if (bh_table_has(AstNode *, state->curr_scope->symbols, tkn->text)) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_REDECLARE_SYMBOL,
                tkn->pos,
                tkn->text);
        token_toggle_end(tkn);
        return 0;
    }

    bh_table_put(AstNode *, state->curr_scope->symbols, tkn->text, symbol);

    if (symbol->kind == Ast_Kind_Local)
        bh_arr_push(state->curr_function->locals, (AstLocal *) symbol);

    token_toggle_end(tkn);
    return 1;
}

static void symbol_basic_type_introduce(SemState* state, AstBasicType* basic_type) {
    bh_table_put(AstNode *, state->curr_scope->symbols, basic_type->name, (AstNode *) basic_type);
}

static AstNode* symbol_resolve(SemState* state, OnyxToken* tkn) {
    token_toggle_end(tkn);

    AstNode* res = NULL;
    Scope* scope = state->curr_scope;

    while (res == NULL && scope != NULL) {
        if (bh_table_has(AstNode *, scope->symbols, tkn->text)) {
            res = bh_table_get(AstNode *, scope->symbols, tkn->text);
        } else {
            scope = scope->parent;
        }
    }

    if (res == NULL ) {
        onyx_message_add(state->msgs,
                ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
                tkn->pos,
                tkn->text);

        token_toggle_end(tkn);
        return NULL;
    }

    if (res->kind == Ast_Kind_Symbol) {
        token_toggle_end(tkn);
        return symbol_resolve(state, res->token);
    }

    token_toggle_end(tkn);
    return res;
}

static void scope_enter(SemState* state, Scope* new_scope) {
    new_scope->parent = state->curr_scope;
    state->curr_scope = new_scope;
}

static void scope_leave(SemState* state) {
    state->curr_scope = state->curr_scope->parent;
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

        case Ast_Kind_Array_Access:
            symres_expression(state, &((AstArrayAccess *)(*expr))->addr);
            symres_expression(state, &((AstArrayAccess *)(*expr))->expr);
            break;

        default:
            DEBUG_HERE;
            break;
    }
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
        case Ast_Kind_Return:     symres_return(state, (AstReturn *) stmt);                return 0;
        case Ast_Kind_If:         symres_if(state, (AstIf *) stmt);                        return 0;
        case Ast_Kind_While:      symres_while(state, (AstWhile *) stmt);                  return 0;
        case Ast_Kind_Call:       symres_call(state, (AstCall *) stmt);                    return 0;
        case Ast_Kind_Argument:   symres_expression(state, (AstTyped **) &((AstArgument *)stmt)->value); return 0;
        case Ast_Kind_Block:      symres_block(state, (AstBlock *) stmt);                  return 0;

        case Ast_Kind_Break:      return 0;
        case Ast_Kind_Continue:   return 0;

        default:                  symres_expression(state, (AstTyped **) &stmt);           return 0;
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
    if (block->scope == NULL)
        block->scope = scope_create(state->node_allocator, state->curr_scope);

    scope_enter(state, block->scope);

    if (block->body)
        symres_statement_chain(state, block->body, &block->body);

    scope_leave(state);
}

static void symres_function(SemState* state, AstFunction* func) {
    if (func->scope == NULL)
        func->scope = scope_create(state->node_allocator, state->curr_scope);

    scope_enter(state, func->scope);

    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->next) {
        param->type_node = symres_type(state, param->type_node);

        symbol_introduce(state, param->token, (AstNode *) param);
    }

    if (func->type_node != NULL) {
        func->type_node = symres_type(state, func->type_node);
    }

    state->curr_function = func;
    symres_block(state, func->body);

    scope_leave(state);
}

static void symres_global(SemState* state, AstGlobal* global) {
    global->type_node = symres_type(state, global->type_node);
}

static void symres_overloaded_function(SemState* state, AstOverloadedFunction* ofunc) {
    bh_arr_each(AstTyped *, node, ofunc->overloads) {
        if ((*node)->kind == Ast_Kind_Symbol) {
            *node = (AstTyped *) symbol_resolve(state, (*node)->token);
        }
    }
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

        case Ast_Kind_Overloaded_Function:
             symres_overloaded_function(state, (AstOverloadedFunction *) *node);
             break;

        default:
             DEBUG_HERE;
             break;
    }
}

void onyx_resolve_symbols(SemState* state, ParserOutput* program) {

    state->global_scope = scope_create(state->node_allocator, NULL);
    scope_enter(state, state->global_scope);

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
        if (!symbol_introduce(state, (*binding)->token, (*binding)->node)) return;

    bh_arr_each(AstNode *, node, program->nodes_to_process)
        symres_top_node(state, node);
}
