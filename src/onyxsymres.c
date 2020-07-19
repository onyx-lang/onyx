#define BH_DEBUG
#include "onyxsempass.h"

static b32  symbol_introduce(OnyxToken* tkn, AstNode* symbol);
static AstNode* symbol_resolve(OnyxToken* tkn);
static void symbol_basic_type_introduce(AstBasicType* basic_type);
static void scope_enter(Scope* new_scope);
static void scope_leave();

static AstType* symres_type(AstType* type);
static void symres_local(AstLocal** local);
static void symres_call(AstCall* call);
static void symres_expression(AstTyped** expr);
static void symres_return(AstReturn* ret);
static void symres_if(AstIf* ifnode);
static void symres_while(AstWhile* whilenode);
static void symres_statement_chain(AstNode* walker, AstNode** trailer);
static b32  symres_statement(AstNode* stmt);
static void symres_block(AstBlock* block);
static void symres_function(AstFunction* func);
static void symres_global(AstGlobal* global);
static void symres_overloaded_function(AstOverloadedFunction* ofunc);

static b32 symbol_introduce(OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    if (bh_table_has(AstNode *, semstate.curr_scope->symbols, tkn->text)) {
        onyx_message_add(Msg_Type_Redeclare_Symbol,
                tkn->pos,
                tkn->text);
        token_toggle_end(tkn);
        return 0;
    }

    bh_table_put(AstNode *, semstate.curr_scope->symbols, tkn->text, symbol);

    if (symbol->kind == Ast_Kind_Local)
        bh_arr_push(semstate.curr_function->locals, (AstLocal *) symbol);

    token_toggle_end(tkn);
    return 1;
}

static void symbol_basic_type_introduce(AstBasicType* basic_type) {
    bh_table_put(AstNode *, semstate.curr_scope->symbols, basic_type->name, (AstNode *) basic_type);
}

static AstNode* symbol_resolve(OnyxToken* tkn) {
    token_toggle_end(tkn);

    AstNode* res = NULL;
    Scope* scope = semstate.curr_scope;

    while (res == NULL && scope != NULL) {
        if (bh_table_has(AstNode *, scope->symbols, tkn->text)) {
            res = bh_table_get(AstNode *, scope->symbols, tkn->text);
        } else {
            scope = scope->parent;
        }
    }

    if (res == NULL ) {
        onyx_message_add(Msg_Type_Unknown_Symbol,
                tkn->pos,
                tkn->text);

        token_toggle_end(tkn);
        return NULL;
    }

    if (res->kind == Ast_Kind_Symbol) {
        token_toggle_end(tkn);
        return symbol_resolve(res->token);
    }

    token_toggle_end(tkn);
    return res;
}

static void scope_enter(Scope* new_scope) {
    new_scope->parent = semstate.curr_scope;
    semstate.curr_scope = new_scope;
}

static void scope_leave() {
    semstate.curr_scope = semstate.curr_scope->parent;
}

static AstType* symres_type(AstType* type) {
    if (type == NULL) return NULL;

    if (type->kind == Ast_Kind_Symbol) {
        return (AstType *) symbol_resolve(((AstNode *) type)->token);
    }

    // NOTE: Already resolved
    if (type->kind == Ast_Kind_Basic_Type) return type;

    if (type->kind == Ast_Kind_Pointer_Type) {
        ((AstPointerType *) type)->elem = symres_type(((AstPointerType *) type)->elem);
        return type;
    }

    if (type->kind == Ast_Kind_Function_Type) {
        AstFunctionType* ftype = (AstFunctionType *) type;

        ftype->return_type = symres_type(ftype->return_type);

        if (ftype->param_count > 0)
            fori (i, 0, ftype->param_count - 1) {
                ftype->params[i] = symres_type(ftype->params[i]);
            }

        return type;
    }

    assert(("Bad type node", 0));
    return NULL;
}

static void symres_local(AstLocal** local) {
    (*local)->type_node = symres_type((*local)->type_node);

    symbol_introduce((*local)->token, (AstNode *) *local);
}

static void symres_call(AstCall* call) {
    AstNode* callee = symbol_resolve(call->callee->token);
    if (callee)
        call->callee = callee;
    else {
        token_toggle_end(call->callee->token);
        onyx_message_add(Msg_Type_Unknown_Symbol,
                call->callee->token->pos,
                call->callee->token->text);
        token_toggle_end(call->callee->token);
        return;
    }

    symres_statement_chain((AstNode *) call->arguments, (AstNode **) &call->arguments);
}

static void symres_unaryop(AstUnaryOp** unaryop) {
    if ((*unaryop)->operation == Unary_Op_Cast) {
        (*unaryop)->type_node = symres_type((*unaryop)->type_node);
    }

    symres_expression(&(*unaryop)->expr);
}

static void symres_expression(AstTyped** expr) {
    switch ((*expr)->kind) {
        case Ast_Kind_Binary_Op:
            symres_expression(&((AstBinaryOp *)(*expr))->left);
            symres_expression(&((AstBinaryOp *)(*expr))->right);
            break;

        case Ast_Kind_Unary_Op: symres_unaryop((AstUnaryOp **) expr); break;
        case Ast_Kind_Call: symres_call((AstCall *) *expr); break;
        case Ast_Kind_Block: symres_block((AstBlock *) *expr); break;

        case Ast_Kind_Symbol:
            *expr = (AstTyped *) symbol_resolve(((AstNode *) *expr)->token);
            break;

        // NOTE: This is a good case, since it means the symbol is already resolved
        case Ast_Kind_Local: break;

        case Ast_Kind_Function:
        case Ast_Kind_NumLit:
        case Ast_Kind_StrLit:
            (*expr)->type_node = symres_type((*expr)->type_node);
            break;

        case Ast_Kind_Address_Of:
            symres_expression(&((AstAddressOf *)(*expr))->expr);
            break;

        case Ast_Kind_Dereference:
            symres_expression(&((AstDereference *)(*expr))->expr);
            break;

        case Ast_Kind_Array_Access:
            symres_expression(&((AstArrayAccess *)(*expr))->addr);
            symres_expression(&((AstArrayAccess *)(*expr))->expr);
            break;

        default:
            DEBUG_HERE;
            break;
    }
}

static void symres_return(AstReturn* ret) {
    if (ret->expr)
        symres_expression(&ret->expr);
}

static void symres_if(AstIf* ifnode) {
    symres_expression(&ifnode->cond);

    // BUG: This will not work for the following case:
    //  if cond foo := 10
    //  else foo := 20
    //
    // The declaration will cause a problem but semantically the above
    // doesn't make sense.
    if (ifnode->true_stmt != NULL)  symres_statement(ifnode->true_stmt);
    if (ifnode->false_stmt != NULL) symres_statement(ifnode->false_stmt);
}

static void symres_while(AstWhile* whilenode) {
    symres_expression(&whilenode->cond);
    symres_statement(whilenode->stmt);
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Local:      symres_local((AstLocal **) &stmt);                return 1;
        case Ast_Kind_Return:     symres_return((AstReturn *) stmt);                return 0;
        case Ast_Kind_If:         symres_if((AstIf *) stmt);                        return 0;
        case Ast_Kind_While:      symres_while((AstWhile *) stmt);                  return 0;
        case Ast_Kind_Call:       symres_call((AstCall *) stmt);                    return 0;
        case Ast_Kind_Argument:   symres_expression((AstTyped **) &((AstArgument *)stmt)->value); return 0;
        case Ast_Kind_Block:      symres_block((AstBlock *) stmt);                  return 0;

        case Ast_Kind_Break:      return 0;
        case Ast_Kind_Continue:   return 0;

        default:                  symres_expression((AstTyped **) &stmt);           return 0;
    }
}

static void symres_statement_chain(AstNode* walker, AstNode** trailer) {
    while (walker) {
        if (symres_statement(walker)) {
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

static void symres_block(AstBlock* block) {
    if (block->scope == NULL)
        block->scope = scope_create(semstate.node_allocator, semstate.curr_scope);

    scope_enter(block->scope);

    if (block->body)
        symres_statement_chain(block->body, &block->body);

    scope_leave();
}

static void symres_function(AstFunction* func) {
    if (func->scope == NULL)
        func->scope = scope_create(semstate.node_allocator, semstate.curr_scope);

    scope_enter(func->scope);

    for (AstLocal *param = func->params; param != NULL; param = (AstLocal *) param->next) {
        param->type_node = symres_type(param->type_node);

        symbol_introduce(param->token, (AstNode *) param);
    }

    if (func->type_node != NULL) {
        func->type_node = symres_type(func->type_node);
    }

    semstate.curr_function = func;
    symres_block(func->body);

    scope_leave();
}

static void symres_global(AstGlobal* global) {
    global->type_node = symres_type(global->type_node);
}

static void symres_overloaded_function(AstOverloadedFunction* ofunc) {
    bh_arr_each(AstTyped *, node, ofunc->overloads) {
        if ((*node)->kind == Ast_Kind_Symbol) {
            *node = (AstTyped *) symbol_resolve((*node)->token);
        }
    }
}

void onyx_resolve_symbols(ProgramInfo* program) {

    semstate.global_scope = scope_create(semstate.node_allocator, NULL);
    scope_enter(semstate.global_scope);

    // NOTE: Add types to global scope
    symbol_basic_type_introduce(&basic_type_void);
    symbol_basic_type_introduce(&basic_type_bool);
    symbol_basic_type_introduce(&basic_type_i8);
    symbol_basic_type_introduce(&basic_type_u8);
    symbol_basic_type_introduce(&basic_type_i16);
    symbol_basic_type_introduce(&basic_type_u16);
    symbol_basic_type_introduce(&basic_type_i32);
    symbol_basic_type_introduce(&basic_type_u32);
    symbol_basic_type_introduce(&basic_type_i64);
    symbol_basic_type_introduce(&basic_type_u64);
    symbol_basic_type_introduce(&basic_type_f32);
    symbol_basic_type_introduce(&basic_type_f64);
    symbol_basic_type_introduce(&basic_type_rawptr);

    bh_arr_each(AstBinding *, binding, program->bindings)
        if (!symbol_introduce((*binding)->token, (*binding)->node)) return;

    bh_arr_each(Entity, entity, program->entities) {
        switch (entity->type) {
            case Entity_Type_Function:            symres_function(entity->function); break;
            case Entity_Type_Overloaded_Function: symres_overloaded_function(entity->overloaded_function); break;
            case Entity_Type_Global:              symres_global(entity->global); break;
            case Entity_Type_Expression:          symres_expression(&entity->expr); break;

            default: break;
        }
    }
}
