#define BH_DEBUG
#include "onyxsempass.h"

AstBasicType basic_type_void   = { { Ast_Kind_Basic_Type, 0, NULL, "void"   }, &basic_types[Basic_Kind_Void]  };
AstBasicType basic_type_bool   = { { Ast_Kind_Basic_Type, 0, NULL, "bool"   }, &basic_types[Basic_Kind_Bool]  };
AstBasicType basic_type_i8     = { { Ast_Kind_Basic_Type, 0, NULL, "i8"     }, &basic_types[Basic_Kind_I8]    };
AstBasicType basic_type_u8     = { { Ast_Kind_Basic_Type, 0, NULL, "u8"     }, &basic_types[Basic_Kind_U8]    };
AstBasicType basic_type_i16    = { { Ast_Kind_Basic_Type, 0, NULL, "i16"    }, &basic_types[Basic_Kind_I16]   };
AstBasicType basic_type_u16    = { { Ast_Kind_Basic_Type, 0, NULL, "u16"    }, &basic_types[Basic_Kind_U16]   };
AstBasicType basic_type_i32    = { { Ast_Kind_Basic_Type, 0, NULL, "i32"    }, &basic_types[Basic_Kind_I32]   };
AstBasicType basic_type_u32    = { { Ast_Kind_Basic_Type, 0, NULL, "u32"    }, &basic_types[Basic_Kind_U32]   };
AstBasicType basic_type_i64    = { { Ast_Kind_Basic_Type, 0, NULL, "i64"    }, &basic_types[Basic_Kind_I64]   };
AstBasicType basic_type_u64    = { { Ast_Kind_Basic_Type, 0, NULL, "u64"    }, &basic_types[Basic_Kind_U64]   };
AstBasicType basic_type_f32    = { { Ast_Kind_Basic_Type, 0, NULL, "f32"    }, &basic_types[Basic_Kind_F32]   };
AstBasicType basic_type_f64    = { { Ast_Kind_Basic_Type, 0, NULL, "f64"    }, &basic_types[Basic_Kind_F64]   };
AstBasicType basic_type_rawptr = { { Ast_Kind_Basic_Type, 0, NULL, "rawptr" }, &basic_types[Basic_Kind_Rawptr] };

AstNumLit builtin_heap_start = { Ast_Kind_NumLit, Ast_Flag_Const, NULL, NULL, (AstType *) &basic_type_rawptr, NULL, 0 };

const BuiltinSymbol builtin_symbols[] = {
    { "void",       (AstNode *) &basic_type_void },
    { "bool",       (AstNode *) &basic_type_bool },
    { "i8",         (AstNode *) &basic_type_i8 },
    { "u8",         (AstNode *) &basic_type_u8 },
    { "i16",        (AstNode *) &basic_type_i16 },
    { "u16",        (AstNode *) &basic_type_u16 },
    { "i32",        (AstNode *) &basic_type_i32 },
    { "u32",        (AstNode *) &basic_type_u32 },
    { "i64",        (AstNode *) &basic_type_i64 },
    { "u64",        (AstNode *) &basic_type_u64 },
    { "f32",        (AstNode *) &basic_type_f32 },
    { "f64",        (AstNode *) &basic_type_f64 },
    { "rawptr",     (AstNode *) &basic_type_rawptr },

    { "__heap_start", (AstNode *) &builtin_heap_start },

    { NULL, NULL },
};

static b32  symbol_introduce(OnyxToken* tkn, AstNode* symbol);
static void symbol_builtin_introduce(char* sym, AstNode *node);
static AstNode* symbol_resolve(OnyxToken* tkn);
static void scope_enter(Scope* new_scope);
static void scope_leave();

static AstType* symres_type(AstType* type);
static void symres_local(AstLocal** local);
static void symres_call(AstCall* call);
static void symres_expression(AstTyped** expr);
static void symres_return(AstReturn* ret);
static void symres_if(AstIf* ifnode);
static void symres_while(AstWhile* whilenode);
static void symres_for(AstFor* fornode);
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

static void symbol_builtin_introduce(char* sym, AstNode *node) {
    bh_table_put(AstNode *, semstate.curr_scope->symbols, sym, node);
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

    if (type->kind == Ast_Kind_Struct_Type) {
        AstStructType* s_node = (AstStructType *) type;
        if (s_node->flags & Ast_Flag_Type_Is_Resolved) return type;

        s_node->flags |= Ast_Flag_Type_Is_Resolved;

        bh_arr_each(AstStructMember *, member, s_node->members) {
            (*member)->type_node = symres_type((*member)->type_node);
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

        case Ast_Kind_Field_Access:
            symres_expression(&((AstFieldAccess *)(*expr))->expr);
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

static void symres_for(AstFor* fornode) {
    fornode->scope = scope_create(semstate.node_allocator, semstate.curr_scope);
    scope_enter(fornode->scope);

    symbol_introduce(fornode->var->token, (AstNode *) fornode->var);

    symres_expression(&fornode->start);
    symres_expression(&fornode->end);
    if (fornode->step) symres_expression(&fornode->step);

    symres_statement(fornode->stmt);

    scope_leave();
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Local:      symres_local((AstLocal **) &stmt);                return 1;
        case Ast_Kind_Return:     symres_return((AstReturn *) stmt);                return 0;
        case Ast_Kind_If:         symres_if((AstIf *) stmt);                        return 0;
        case Ast_Kind_While:      symres_while((AstWhile *) stmt);                  return 0;
        case Ast_Kind_For:        symres_for((AstFor *) stmt);                      return 0;
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
    BuiltinSymbol* bsym = (BuiltinSymbol *) &builtin_symbols[0];
    while (bsym->sym != NULL) {
        symbol_builtin_introduce(bsym->sym, bsym->node);
        bsym++;
    }

    bh_arr_each(AstBinding *, binding, program->bindings)
        if (!symbol_introduce((*binding)->token, (*binding)->node)) return;

    bh_arr_each(Entity, entity, program->entities) {
        switch (entity->type) {
            case Entity_Type_Function:            symres_function(entity->function); break;
            case Entity_Type_Overloaded_Function: symres_overloaded_function(entity->overloaded_function); break;
            case Entity_Type_Global:              symres_global(entity->global); break;
            case Entity_Type_Expression:          symres_expression(&entity->expr); break;
            case Entity_Type_Struct:              symres_type((AstType *) entity->struct_type); break;

            default: break;
        }
    }
}
