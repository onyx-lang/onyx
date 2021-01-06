#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxparser.h"
#include "onyxutils.h"
#include "onyxastnodes.h"
#include "onyxerrors.h"

static void scope_enter(Scope* new_scope);
static void scope_leave();

AstType* symres_type(AstType* type);
static void symres_local(AstLocal** local, b32 add_to_block_locals);
static void symres_call(AstCall* call);
static void symres_size_of(AstSizeOf* so);
static void symres_align_of(AstAlignOf* so);
static void symres_field_access(AstFieldAccess** fa);
static void symres_expression(AstTyped** expr);
static void symres_return(AstReturn* ret);
static void symres_if(AstIfWhile* ifnode);
static void symres_while(AstIfWhile* whilenode);
static void symres_for(AstFor* fornode);
static void symres_switch(AstSwitch* switchnode);
static void symres_use(AstUse* use);
static void symres_directive_solidify(AstDirectiveSolidify** psolid);
static void symres_statement_chain(AstNode** walker);
static b32  symres_statement(AstNode** stmt);
static void symres_block(AstBlock* block);
void symres_function_header(AstFunction* func);
void symres_function(AstFunction* func);
static void symres_global(AstGlobal* global);
static void symres_overloaded_function(AstOverloadedFunction* ofunc);
static void symres_use_package(AstUsePackage* package);
static void symres_enum(AstEnumType* enum_node);
static void symres_memres_type(AstMemRes** memres);
static void symres_memres(AstMemRes** memres);
static void symres_struct_defaults(AstType* st);

static AstFieldAccess* make_field_access(AstTyped* node, char* field) {
    AstFieldAccess* fa = onyx_ast_node_new(semstate.node_allocator, sizeof(AstFieldAccess), Ast_Kind_Field_Access);
    fa->field = field;
    fa->expr = node;

    return fa;
}

static void scope_enter(Scope* new_scope) {
    semstate.curr_scope = new_scope;
}

static void scope_leave() {
    semstate.curr_scope = semstate.curr_scope->parent;
}

AstType* symres_type(AstType* type) {
    if (type == NULL) return NULL;

    if (type->kind == Ast_Kind_Type_Alias) {
        ((AstTypeAlias *) type)->to = symres_type(((AstTypeAlias *) type)->to);
        return type;
    }

    if (type->kind == Ast_Kind_Symbol) {
        return (AstType *) symbol_resolve(semstate.curr_scope, ((AstNode *) type)->token);
    }

    if (type->kind == Ast_Kind_Field_Access) {
        AstFieldAccess* field = (AstFieldAccess *) type;
        symres_field_access(&field);

        if (!node_is_type((AstNode *) field))
            onyx_report_error(type->token->pos, "field access did not result in a type");

        return (AstType *) field;
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
            fori (i, 0, (i64) ftype->param_count) {
                ftype->params[i] = symres_type(ftype->params[i]);
            }

        return type;
    }

    if (type->kind == Ast_Kind_Struct_Type) {
        AstStructType* s_node = (AstStructType *) type;
        if (s_node->flags & Ast_Flag_Type_Is_Resolved) return type;

        s_node->flags |= Ast_Flag_Type_Is_Resolved;
        
        {
            bh_table(i32) mem_set;
            bh_table_init(global_heap_allocator, mem_set, bh_arr_length(s_node->members));

            bh_arr_each(AstStructMember *, member, s_node->members) {
                token_toggle_end((*member)->token);

                if (bh_table_has(i32, mem_set, (*member)->token->text)) {
                    onyx_report_error((*member)->token->pos,
                            "Duplicate struct member '%s'.",
                            (*member)->token->text);

                    token_toggle_end((*member)->token);
                    return type;
                }

                bh_table_put(i32, mem_set, (*member)->token->text, 1);
                token_toggle_end((*member)->token);
            }

            bh_table_free(mem_set);
        }

        fori (i, 0, bh_arr_length(s_node->members)) {
            AstStructMember *member = s_node->members[i];
            member->type_node = symres_type(member->type_node);

            if (!node_is_type((AstNode *) member->type_node)) {
                onyx_report_error(member->token->pos, "Member type is not a type.");
                return type;
            }

            if (member->flags & Ast_Flag_Struct_Mem_Used) {
                AstStructType *used = (AstStructType *) member->type_node;

                while (used->kind == Ast_Kind_Type_Alias) {
                    // NOTE: Maybe not a struct type.
                    used = (AstStructType *) ((AstTypeAlias *) used)->to;
                }

                if (used->kind != Ast_Kind_Struct_Type) {
                    onyx_report_error(member->token->pos,
                            "Can only 'use' members of struct type, got '%s'.",
                            onyx_ast_node_kind_string(used->kind));

                    return type;
                }
            }
        }

        return type;
    }

    if (type->kind == Ast_Kind_Array_Type) {
        AstArrayType* a_node = (AstArrayType *) type;

        if (a_node->count_expr) symres_expression(&a_node->count_expr);
        a_node->elem = symres_type(a_node->elem);

        return type;
    }

    if (type->kind == Ast_Kind_Enum_Type) {
       // symres_enum((AstEnumType *) type);
       return type;
    }

    if (type->kind == Ast_Kind_Slice_Type) {
        AstSliceType* s_node = (AstSliceType *) type;
        s_node->elem = symres_type(s_node->elem);

        return type;
    }

    if (type->kind == Ast_Kind_DynArr_Type) {
        AstDynArrType* da_node = (AstDynArrType *) type;
        da_node->elem = symres_type(da_node->elem);

        return type;
    }

    if (type->kind == Ast_Kind_VarArg_Type) {
        AstVarArgType* va_node = (AstVarArgType *) type;
        va_node->elem = symres_type(va_node->elem);

        return type;
    }

    if (type->kind == Ast_Kind_Poly_Struct_Type) {
        AstPolyStructType* pst_node = (AstPolyStructType *) type;
        pst_node->scope = scope_create(semstate.node_allocator, semstate.curr_scope, pst_node->token->pos);

        return type;
    }

    if (type->kind == Ast_Kind_Poly_Call_Type) {
        AstPolyCallType* pc_node = (AstPolyCallType *) type;

        pc_node->callee = symres_type(pc_node->callee);

        bh_arr_each(AstType *, param, pc_node->params) {
            *param = symres_type(*param);
        }

        return type;
    }

    return type;
}

static void symres_local(AstLocal** local, b32 add_to_block_locals) {
    (*local)->type_node = symres_type((*local)->type_node);

    // NOTE: This is a little gross, but it is allows for finer control
    // over when locals are in scope in a block, which reduces the number
    // of unique WASM locals and stack space needed.
    //                                            - brendanfh 2020/12/16
    if (add_to_block_locals)
        bh_arr_push(bh_arr_last(semstate.block_stack)->allocate_exprs, (AstTyped *) *local);

    bh_arr_push(semstate.curr_function->allocate_exprs, (AstTyped *) *local);

    if ((*local)->token != NULL)
        symbol_introduce(semstate.curr_scope, (*local)->token, (AstNode *) *local);
}

static void symres_call(AstCall* call) {
    symres_expression((AstTyped **) &call->callee);
    if (call->callee == NULL) return;

    symres_statement_chain((AstNode **) &call->arguments);
}

static void symres_size_of(AstSizeOf* so) {
    so->type_node = symres_type(so->type_node);
    so->so_ast_type = symres_type(so->so_ast_type);
}

static void symres_align_of(AstAlignOf* ao) {
    ao->type_node = symres_type(ao->type_node);
    ao->ao_ast_type = symres_type(ao->ao_ast_type);
}

static void symres_field_access(AstFieldAccess** fa) {
    if ((*fa)->expr == NULL) return;
    symres_expression(&(*fa)->expr);
    if ((*fa)->expr == NULL) return;

    if ((*fa)->expr->kind == Ast_Kind_Package) {
        AstPackage* package = (AstPackage *) (*fa)->expr;
        AstNode* n = symbol_resolve(package->package->scope, (*fa)->token);
        if (n) {
            // NOTE: not field access
            *fa = (AstFieldAccess *) n;
            return;
        }
    }

    if ((*fa)->expr->kind == Ast_Kind_Enum_Type) {
        AstEnumType* etype = (AstEnumType *) (*fa)->expr;
        AstNode* n = symbol_resolve(etype->scope, (*fa)->token);
        if (n) {
            // NOTE: not field access
            *fa = (AstFieldAccess *) n;
            return;
        }
    }
}

static void symres_pipe(AstBinaryOp** pipe) {
    AstCall* call_node = (AstCall *) (*pipe)->right;
    symres_expression((AstTyped **) &call_node);
    symres_expression(&(*pipe)->left);

    if (call_node->kind != Ast_Kind_Call) {
        onyx_report_error((*pipe)->token->pos, "Pipe operator expected call on right side.");
        return;
    }

    if ((*pipe)->left == NULL) return;

    AstArgument* implicit_arg = onyx_ast_node_new(semstate.node_allocator,
            sizeof(AstArgument),
            Ast_Kind_Argument);
    implicit_arg->token = (*pipe)->left->token;
    implicit_arg->value = (*pipe)->left;
    implicit_arg->next = (AstNode *) call_node->arguments;

    call_node->arguments = implicit_arg;
    call_node->arg_count++;
    call_node->next = (*pipe)->next;

    // NOTE: Not a BinaryOp node
    *pipe = (AstBinaryOp *) call_node;
}

static void symres_unaryop(AstUnaryOp** unaryop) {
    if ((*unaryop)->operation == Unary_Op_Cast) {
        (*unaryop)->type_node = symres_type((*unaryop)->type_node);
    }

    symres_expression(&(*unaryop)->expr);
}

static void symres_struct_literal(AstStructLiteral* sl) {
    if (sl->stnode != NULL) symres_expression(&sl->stnode);
    sl->stnode = (AstTyped *) symres_type((AstType *) sl->stnode);
    if (sl->stnode == NULL || sl->stnode->kind == Ast_Kind_Error || sl->stnode->kind == Ast_Kind_Symbol) return;

    sl->type_node = (AstType *) sl->stnode;
    while (sl->type_node->kind == Ast_Kind_Type_Alias)
        sl->type_node = ((AstTypeAlias *) sl->type_node)->to;

    if (sl->values != NULL) {
        bh_arr_each(AstTyped *, expr, sl->values) {
            if (*expr == NULL) onyx_report_error(sl->token->pos, "Some kind of error occured with this struct literal.");
            else               symres_expression(expr);
        }
    }

    if (sl->named_values != NULL) {
        bh_arr_each(AstStructMember *, smem, sl->named_values) {
            if ((*smem)->initial_value == NULL) onyx_report_error(sl->token->pos, "Some kind of error occured with this struct literal.");
            else                                symres_expression(&(*smem)->initial_value);
        }
    }
}

static void symres_array_literal(AstArrayLiteral* al) {
    if (al->atnode != NULL) symres_expression(&al->atnode);

    al->atnode = (AstTyped *) symres_type((AstType *) al->atnode);
    if (al->atnode == NULL || al->atnode->kind == Ast_Kind_Error || al->atnode->kind == Ast_Kind_Symbol) return;

    al->type_node = (AstType *) al->atnode;
    while (al->type_node->kind == Ast_Kind_Type_Alias)
        al->type_node = ((AstTypeAlias *) al->type_node)->to;

    bh_arr_each(AstTyped *, expr, al->values)
        symres_expression(expr);

    if (bh_arr_length(semstate.block_stack) > 0) {
        bh_arr_push(bh_arr_last(semstate.block_stack)->allocate_exprs, (AstTyped *) al);
        bh_arr_push(semstate.curr_function->allocate_exprs, (AstTyped *) al);
    }
}

static void symres_expression(AstTyped** expr) {
    switch ((*expr)->kind) {
        case Ast_Kind_Symbol:
            *expr = (AstTyped *) symbol_resolve(semstate.curr_scope, ((AstNode *) *expr)->token);
            break;

        case Ast_Kind_Binary_Op:
            symres_expression(&((AstBinaryOp *)(*expr))->left);
            symres_expression(&((AstBinaryOp *)(*expr))->right);

            if (((AstBinaryOp *) (*expr))->left)
                (*expr)->type_node = ((AstBinaryOp *)(*expr))->left->type_node;
            break;

        case Ast_Kind_Unary_Op:     symres_unaryop((AstUnaryOp **) expr); break;
        case Ast_Kind_Call:         symres_call((AstCall *) *expr); break;
        case Ast_Kind_Block:        symres_block((AstBlock *) *expr); break;
        case Ast_Kind_Address_Of:   symres_expression(&((AstAddressOf *)(*expr))->expr); break;
        case Ast_Kind_Dereference:  symres_expression(&((AstDereference *)(*expr))->expr); break;
        case Ast_Kind_Field_Access: symres_field_access((AstFieldAccess **) expr); break;
        case Ast_Kind_Pipe:         symres_pipe((AstBinaryOp **) expr); break;
        case Ast_Kind_Size_Of:      symres_size_of((AstSizeOf *)*expr); break;
        case Ast_Kind_Align_Of:     symres_align_of((AstAlignOf *)*expr); break;

        case Ast_Kind_Range_Literal:
            symres_expression(&((AstRangeLiteral *)(*expr))->low);
            symres_expression(&((AstRangeLiteral *)(*expr))->high);

            (*expr)->type_node = symres_type(builtin_range_type);

            // NOTE: This is a weird place to put this so maybe put it somewhere else eventually
            //                                                  - brendanfh   2020/09/04
            builtin_range_type_type = type_build_from_ast(semstate.node_allocator, builtin_range_type);
            break;

        case Ast_Kind_Function:
        case Ast_Kind_NumLit:
            (*expr)->type_node = symres_type((*expr)->type_node);
            break;

        case Ast_Kind_StrLit:
            (*expr)->type_node = symres_type(builtin_string_type);
            break;

        case Ast_Kind_Slice:
        case Ast_Kind_Array_Access:
            symres_expression(&((AstArrayAccess *)(*expr))->addr);
            symres_expression(&((AstArrayAccess *)(*expr))->expr);
            break;

        case Ast_Kind_Struct_Literal:
            symres_struct_literal((AstStructLiteral *)(*expr));
            break;

        case Ast_Kind_Array_Literal:
            symres_array_literal((AstArrayLiteral *)(*expr));
            break;

        case Ast_Kind_Directive_Solidify:
            symres_directive_solidify((AstDirectiveSolidify **) expr);
            break;

        default: break;
    }
}

static void symres_return(AstReturn* ret) {
    if (ret->expr)
        symres_expression(&ret->expr);
}

static void symres_if(AstIfWhile* ifnode) {
    if (ifnode->assignment != NULL) {
        ifnode->scope = scope_create(semstate.node_allocator, semstate.curr_scope, ifnode->token->pos);
        scope_enter(ifnode->scope);

        symres_local(&ifnode->local, 0);

        symres_statement((AstNode **) &ifnode->assignment);
    }

    symres_expression(&ifnode->cond);

    if (ifnode->true_stmt != NULL)  symres_statement((AstNode **) &ifnode->true_stmt);
    if (ifnode->false_stmt != NULL) symres_statement((AstNode **) &ifnode->false_stmt);

    if (ifnode->assignment != NULL) scope_leave();
}

static void symres_while(AstIfWhile* whilenode) {
    if (whilenode->assignment != NULL) {
        whilenode->scope = scope_create(semstate.node_allocator, semstate.curr_scope, whilenode->token->pos);
        scope_enter(whilenode->scope);

        symres_local(&whilenode->local, 0);

        symres_statement((AstNode **) &whilenode->assignment);
    }

    symres_expression(&whilenode->cond);

    if (whilenode->true_stmt)  symres_block(whilenode->true_stmt);
    if (whilenode->false_stmt) symres_block(whilenode->false_stmt);

    if (whilenode->assignment != NULL) scope_leave();
}

static void symres_for(AstFor* fornode) {
    fornode->scope = scope_create(semstate.node_allocator, semstate.curr_scope, fornode->token->pos);
    scope_enter(fornode->scope);

    symres_expression(&fornode->iter);

    symres_local(&fornode->var, 0);

    symres_block(fornode->stmt);

    scope_leave();
}

static void symres_switch(AstSwitch* switchnode) {
    if (switchnode->assignment != NULL) {
        switchnode->scope = scope_create(semstate.node_allocator, semstate.curr_scope, switchnode->token->pos);
        scope_enter(switchnode->scope);

        symbol_introduce(semstate.curr_scope, switchnode->local->token, (AstNode *) switchnode->local);

        symres_statement((AstNode **) &switchnode->assignment);
    }

    symres_expression(&switchnode->expr);

    bh_arr_each(AstSwitchCase, sc, switchnode->cases) {
        bh_arr_each(AstTyped *, value, sc->values)
            symres_expression(value);
            
        symres_block(sc->block);
    }

    if (switchnode->default_case)
        symres_block(switchnode->default_case);

    if (switchnode->assignment != NULL) scope_leave();
}

static void symres_use(AstUse* use) {
    symres_expression(&use->expr);
    if (use->expr == NULL) return;

    if (use->expr->kind == Ast_Kind_Enum_Type) {
        AstEnumType* et = (AstEnumType *) use->expr;

        bh_arr_each(AstEnumValue *, ev, et->values)
            symbol_introduce(semstate.curr_scope, (*ev)->token, (AstNode *) *ev);

        return;
    }

    if (use->expr->type_node == NULL && use->expr->type == NULL) goto cannot_use;

    AstType* effective_type = use->expr->type_node;
    if (effective_type->kind == Ast_Kind_Pointer_Type)
        effective_type = ((AstPointerType *) effective_type)->elem;

    if (effective_type->kind == Ast_Kind_Struct_Type ||
            effective_type->kind == Ast_Kind_Poly_Call_Type) {

        if (use->expr->type == NULL)
            use->expr->type = type_build_from_ast(semstate.node_allocator, use->expr->type_node);
        if (use->expr->type == NULL) goto cannot_use;

        Type* st = use->expr->type;
        if (st->kind == Type_Kind_Pointer)
            st = st->Pointer.elem;

        bh_table_each_start(StructMember, st->Struct.members);
            AstFieldAccess* fa = make_field_access(use->expr, value.name);
            symbol_raw_introduce(semstate.curr_scope, value.name, use->token->pos, (AstNode *) fa);
        bh_table_each_end;

        return;
    }

cannot_use:
    onyx_report_error(use->token->pos, "Cannot use this because its type is unknown.");
}

static void symres_directive_solidify(AstDirectiveSolidify** psolid) {
    AstDirectiveSolidify* solid = *psolid;
    if (solid->resolved_proc != NULL)
        *psolid = (AstDirectiveSolidify *) solid->resolved_proc;

    symres_expression((AstTyped **) &solid->poly_proc);
    if (solid->poly_proc && solid->poly_proc->kind == Ast_Kind_Directive_Solidify) {
        solid->poly_proc = (AstPolyProc *) ((AstDirectiveSolidify *) solid->poly_proc)->resolved_proc;
    }
    
    if (!solid->poly_proc || solid->poly_proc->kind != Ast_Kind_Polymorphic_Proc) {
        onyx_report_error(solid->token->pos, "Expected polymorphic procedure in #solidify directive.");
        return;
    }

    bh_arr_each(AstPolySolution, sln, solid->known_polyvars) {
        sln->ast_type = symres_type(sln->ast_type);
        sln->type = type_build_from_ast(semstate.node_allocator, sln->ast_type);
        if (onyx_has_errors()) return;
    }

    solid->resolved_proc = polymorphic_proc_try_solidify(solid->poly_proc, solid->known_polyvars, solid->token->pos);

    // NOTE: Not a DirectiveSolidify.
    *psolid = (AstDirectiveSolidify *) solid->resolved_proc;
    return;
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(AstNode** stmt) {
    switch ((*stmt)->kind) {
        case Ast_Kind_Local:       symres_local((AstLocal **) stmt, 1);               return 1;
        case Ast_Kind_Return:      symres_return((AstReturn *) *stmt);                return 0;
        case Ast_Kind_If:          symres_if((AstIfWhile *) *stmt);                   return 0;
        case Ast_Kind_While:       symres_while((AstIfWhile *) *stmt);                return 0;
        case Ast_Kind_For:         symres_for((AstFor *) *stmt);                      return 0;
        case Ast_Kind_Switch:      symres_switch((AstSwitch *) *stmt);                return 0;
        case Ast_Kind_Call:        symres_call((AstCall *) *stmt);                    return 0;
        case Ast_Kind_Argument:    symres_expression((AstTyped **) &((AstArgument *) *stmt)->value); return 0;
        case Ast_Kind_Block:       symres_block((AstBlock *) *stmt);                  return 0;
        case Ast_Kind_Defer:       symres_statement(&((AstDefer *) *stmt)->stmt);     return 0;
        case Ast_Kind_Use:         symres_use((AstUse *) *stmt);                      return 1;
        case Ast_Kind_Use_Package: symres_use_package((AstUsePackage *) *stmt);       return 1;

        case Ast_Kind_Jump:      return 0;

        default:                  symres_expression((AstTyped **) stmt);           return 0;
    }
}

static void symres_statement_chain(AstNode** walker) {
    while (*walker) {
        if (symres_statement(walker)) {
            AstNode* tmp = (*walker)->next;
            (*walker)->next = NULL;
            (*walker) = tmp;
        } else {
            walker = &(*walker)->next;
        }
    }
}

static void symres_block(AstBlock* block) {
    if (block->scope == NULL)
        block->scope = scope_create(semstate.node_allocator, semstate.curr_scope, block->token->pos);

    scope_enter(block->scope);
    bh_arr_push(semstate.block_stack, block);

    if (block->binding_scope != NULL)
        scope_include(block->scope, block->binding_scope, block->token->pos);

    if (block->body)
        symres_statement_chain(&block->body);

    bh_arr_pop(semstate.block_stack);
    scope_leave();
}

void symres_function_header(AstFunction* func) {
    if (func->scope == NULL)
        func->scope = scope_create(semstate.node_allocator, semstate.curr_scope, func->token->pos);

    func->flags |= Ast_Flag_Comptime;

    bh_arr_each(AstParam, param, func->params) {
        if (param->default_value != NULL) {
            symres_expression(&param->default_value);
            if (onyx_has_errors()) return;

            // HACK: It shouldn't be necessary to do this twice, but right now
            // if `null` is the default parameter and it hasn't been used anywhere in
            // code yet, it doesn't resolve properly. So for now I am just checking symbols twice.
            //                                                      -brendanfh 2020/12/24
            symres_expression(&param->default_value);
            if (onyx_has_errors()) return;
        }
    }

    if (func->overloaded_function != NULL) {
        symres_expression((AstTyped **) &func->overloaded_function);
        if (func->overloaded_function == NULL) return; // NOTE: Error message will already be generated

        if (func->overloaded_function->kind != Ast_Kind_Overloaded_Function) {
            onyx_report_error(func->token->pos, "#add_overload directive did not resolve to an overloaded function.");
            return;

        } else {
            AstOverloadedFunction* ofunc = (AstOverloadedFunction *) func->overloaded_function;
            bh_arr_push(ofunc->overloads, (AstTyped *) func);
        }
    }

    func->return_type = symres_type(func->return_type);
    if (!node_is_type((AstNode *) func->return_type)) {
        onyx_report_error(func->token->pos, "Return type is not a type.");
    }

    scope_enter(func->scope);

    bh_arr_each(AstParam, param, func->params) {
        if (param->local->type_node != NULL) {
            param->local->type_node = symres_type(param->local->type_node);
        }

        symbol_introduce(semstate.curr_scope, param->local->token, (AstNode *) param->local);

        if (param->local->flags & Ast_Flag_Param_Use) {
            if (param->local->type_node != NULL && param->local->type == NULL) {
                param->local->type = type_build_from_ast(semstate.allocator, param->local->type_node);
            }

            if (type_is_struct(param->local->type)) {
                Type* st;
                if (param->local->type->kind == Type_Kind_Struct) {
                    st = param->local->type;
                } else {
                    st = param->local->type->Pointer.elem;
                }

                bh_table_each_start(StructMember, st->Struct.members);
                    AstFieldAccess* fa = make_field_access((AstTyped *) param->local, value.name);
                    symbol_raw_introduce(semstate.curr_scope, value.name, param->local->token->pos, (AstNode *) fa);
                bh_table_each_end;

            } else if (param->local->type != NULL) {
                onyx_report_error(param->local->token->pos, "Can only 'use' structures or pointers to structures.");
            } else {
                onyx_report_error(param->local->token->pos, "Cannot deduce type of parameter '%b'; Try adding it explicitly.",
                    param->local->token->text,
                    param->local->token->length);
            }
        }
    }

    scope_leave();
}

void symres_function(AstFunction* func) {
    scope_enter(func->scope);

    semstate.curr_function = func;
    symres_block(func->body);

    scope_leave();
}

static void symres_global(AstGlobal* global) {
    global->type_node = symres_type(global->type_node);
}

static void symres_overloaded_function(AstOverloadedFunction* ofunc) {
    bh_arr_each(AstTyped *, node, ofunc->overloads) {
        symres_expression(node);
    }
}

static void symres_use_package(AstUsePackage* package) {
    token_toggle_end(package->package->token);
    Package* p = program_info_package_lookup(semstate.program, package->package->token->text);
    token_toggle_end(package->package->token);

    if (p == NULL) {
        onyx_report_error(package->token->pos, "package not found in included source files");
        return;
    }

    if (p->scope == semstate.curr_scope) return;

    if (package->alias != NULL) {
        AstPackage *pac_node = onyx_ast_node_new(semstate.node_allocator, sizeof(AstPackage), Ast_Kind_Package);
        pac_node->package = p;
        pac_node->token = package->alias;

        symbol_introduce(semstate.curr_scope, package->alias, (AstNode *) pac_node);
    }

    if (package->only != NULL) {
        bh_arr_each(AstAlias *, alias, package->only) {

            AstNode* thing = symbol_resolve(p->scope, (*alias)->token);
            if (thing == NULL) {
                onyx_report_error((*alias)->token->pos, "not found in package");
                return;
            }

            symbol_introduce(semstate.curr_scope, (*alias)->alias, thing);
        }
    }

    if (package->alias == NULL && package->only == NULL) {
        OnyxFilePos pos = { 0 };
        if (package->token != NULL)
            pos = package->token->pos;

        scope_include(semstate.curr_scope, p->scope, pos);
    }
}

static void symres_enum(AstEnumType* enum_node) {
    if (enum_node->backing->kind == Ast_Kind_Symbol) {
        enum_node->backing = (AstType *) symbol_resolve(semstate.curr_scope, enum_node->backing->token);
    }
    if (enum_node->backing == NULL) return;

    enum_node->backing_type = type_build_from_ast(semstate.allocator, enum_node->backing);
    enum_node->scope = scope_create(semstate.node_allocator, NULL, enum_node->token->pos);

    type_build_from_ast(semstate.node_allocator, (AstType *) enum_node);

    u64 next_assign_value = (enum_node->flags & Ast_Flag_Enum_Is_Flags) ? 1 : 0;
    bh_arr_each(AstEnumValue *, value, enum_node->values) {
        symbol_introduce(enum_node->scope, (*value)->token, (AstNode *) *value);
        (*value)->type = enum_node->etcache;

        if ((*value)->value != NULL) {
            // HACK
            resolve_expression_type((AstTyped *) (*value)->value);
            if (type_is_small_integer((*value)->value->type)) {
                next_assign_value = (*value)->value->value.i;
            } else if (type_is_integer((*value)->value->type)) {
                next_assign_value = (*value)->value->value.l;
            } else {
                onyx_report_error((*value)->token->pos, "expected numeric integer literal for enum initialization");
                return;
            }

            (*value)->value->type = enum_node->etcache;

        } else {
            AstNumLit* num = onyx_ast_node_new(semstate.node_allocator, sizeof(AstNumLit), Ast_Kind_NumLit);
            num->value.l = next_assign_value;
            num->flags |= Ast_Flag_Comptime;
            num->type = enum_node->etcache;

            (*value)->value = num;
        }

        (*value)->flags |= Ast_Flag_Comptime;

        if (enum_node->flags & Ast_Flag_Enum_Is_Flags) {
            next_assign_value <<= 1;
        } else {
            next_assign_value++;
        }
    }
}

static void symres_memres_type(AstMemRes** memres) {
    (*memres)->type_node = symres_type((*memres)->type_node);
}

static void symres_memres(AstMemRes** memres) {
    if ((*memres)->initial_value != NULL) {
        symres_expression(&(*memres)->initial_value);
    }
}

static void symres_struct_defaults(AstType* t) {
    switch (t->kind) {
        case Ast_Kind_Struct_Type: {
            AstStructType* st = (AstStructType *) t;
            bh_arr_each(AstStructMember *, smem, st->members) {
                if ((*smem)->initial_value != NULL) {
                    symres_expression(&(*smem)->initial_value);
                }
            }
            break;
        }

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* st = (AstPolyStructType *) t;
            bh_arr_each(AstStructMember *, smem, st->base_struct->members) {
                if ((*smem)->initial_value != NULL) {
                    symres_expression(&(*smem)->initial_value);
                }
            }
            break;
        }

        default: break;
    }
}

static void symres_polyproc(AstPolyProc* pp) {
    pp->poly_scope = semstate.curr_scope;
}

void symres_entity(Entity* ent) {
    if (ent->package) semstate.curr_package = ent->package;

    Scope* old_scope = NULL;
    if (ent->scope) {
        old_scope = semstate.curr_scope;
        scope_enter(ent->scope);
    }

    EntityState next_state = Entity_State_Check_Types;

    switch (ent->type) {
        case Entity_Type_Foreign_Function_Header:
        case Entity_Type_Function_Header:         symres_function_header(ent->function); break;
        case Entity_Type_Function:                symres_function(ent->function);        break;

        case Entity_Type_Foreign_Global_Header:
        case Entity_Type_Global_Header:       symres_global(ent->global); break;

        case Entity_Type_Use_Package:         symres_use_package(ent->use_package); break;
        case Entity_Type_Use:                 symres_use(ent->use);
                                              next_state = Entity_State_Finalized;
                                              break;

        case Entity_Type_Overloaded_Function:     symres_overloaded_function(ent->overloaded_function); break;
        case Entity_Type_Expression:              symres_expression(&ent->expr); break;
        case Entity_Type_Type_Alias:              ent->type_alias = symres_type(ent->type_alias); break;
        case Entity_Type_Enum:                    symres_enum(ent->enum_type); break;
        case Entity_Type_Memory_Reservation_Type: symres_memres_type(&ent->mem_res); break;
        case Entity_Type_Memory_Reservation:      symres_memres(&ent->mem_res); break;
        case Entity_Type_Polymorphic_Proc:        symres_polyproc(ent->poly_proc); break;
        case Entity_Type_String_Literal:          symres_expression(&ent->expr); break;
        case Entity_Type_Struct_Member_Default:   symres_struct_defaults((AstType *) ent->type_alias); break;

        default: break;
    }

    ent->state = next_state;

    if (ent->scope) {
        semstate.curr_scope = old_scope;
    }
}
