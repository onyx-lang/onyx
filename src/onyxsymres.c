#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxparser.h"
#include "onyxutils.h"



static void scope_enter(Scope* new_scope);
static void scope_leave();

static AstType* symres_type(AstType* type);
static void symres_local(AstLocal** local);
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
static void symres_statement_chain(AstNode** walker);
static b32  symres_statement(AstNode** stmt);
static void symres_block(AstBlock* block);
void symres_function(AstFunction* func);
static void symres_global(AstGlobal* global);
static void symres_overloaded_function(AstOverloadedFunction* ofunc);
static void symres_use_package(AstUsePackage* package);
static void symres_enum(AstEnumType* enum_node);
static void symres_memres(AstMemRes** memres);

static void scope_enter(Scope* new_scope) {
    if (new_scope->parent == NULL)
        new_scope->parent = semstate.curr_scope;
    semstate.curr_scope = new_scope;
}

static void scope_leave() {
    semstate.curr_scope = semstate.curr_scope->parent;
}

static AstType* symres_type(AstType* type) {
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
            fori (i, 0, ftype->param_count) {
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
            if ((*member)->initial_value != NULL) {
                symres_expression(&(*member)->initial_value);
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

    return type;
}

static void symres_local(AstLocal** local) {
    (*local)->type_node = symres_type((*local)->type_node);

    bh_arr_push(bh_arr_last(semstate.block_stack)->locals, *local);
    bh_arr_push(semstate.curr_function->locals, *local);

    symbol_introduce(semstate.curr_scope, (*local)->token, (AstNode *) *local);
}

static void symres_call(AstCall* call) {
    symres_expression((AstTyped **) &call->callee);
    if (call->callee == NULL) return;

    symres_statement_chain((AstNode **) &call->arguments);
}

static void symres_size_of(AstSizeOf* so) {
    so->type_node = symres_type(so->type_node);
    so->so_type = symres_type(so->so_type);
}

static void symres_align_of(AstAlignOf* ao) {
    ao->type_node = symres_type(ao->type_node);
    ao->ao_type = symres_type(ao->ao_type);
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
        onyx_report_error((*pipe)->token->pos, "universal function call expected call on right side");
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

    if ((*unaryop)->type_node == NULL)
        (*unaryop)->type_node = ((AstUnaryOp *)(*unaryop))->expr->type_node;
}

static void symres_struct_literal(AstStructLiteral* sl) {
    if (sl->stnode != NULL) symres_expression(&sl->stnode);
    if (sl->stnode == NULL || sl->stnode->kind == Ast_Kind_Error) return;

    sl->type_node = (AstType *) sl->stnode;
    sl->type = type_build_from_ast(semstate.allocator, sl->type_node);

    if (!type_is_structlike_strict(sl->type)) {
        onyx_report_error(sl->token->pos, "Type is not a constructable using a struct literal.");
        return;
    }

    if (bh_arr_length(sl->values) == 0) {
        bh_arr_set_length(sl->values, type_structlike_mem_count(sl->type));
        bh_arr_zero(sl->values);

        StructMember s;
        bh_arr_each(AstStructMember *, smem, sl->named_values) {
            token_toggle_end((*smem)->token);
            if (!type_lookup_member(sl->type, (*smem)->token->text, &s)) {
                onyx_report_error((*smem)->token->pos,
                    "The field '%s' does not exist on type '%s'.", (*smem)->token->text, type_get_name(sl->type));
                token_toggle_end((*smem)->token);
                return;
            }
            token_toggle_end((*smem)->token);

            if (sl->values[s.idx] != NULL) {
                onyx_report_error((*smem)->token->pos, "Multiple values given for '%b'.", (*smem)->token->text, (*smem)->token->length);
                return;
            }

            sl->values[s.idx] = (*smem)->initial_value;
        }

        if (sl->type->kind == Type_Kind_Struct) {
            AstStructType* st = (AstStructType *) sl->type_node;
            bh_arr_each(StructMember*, smem, sl->type->Struct.memarr) {
                u32 idx = (*smem)->idx;

                if (sl->values[idx] == NULL) {
                    if (st->members[idx]->initial_value == NULL) {
                        onyx_report_error(sl->token->pos, "No value was given for the field '%b'.",
                                st->members[idx]->token->text,
                                st->members[idx]->token->length);
                        return;
                    }

                    sl->values[idx] = st->members[idx]->initial_value;
                }
            }
        }
    }

    bh_arr_each(AstTyped *, expr, sl->values) symres_expression(expr);
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

        case Ast_Kind_Range:
            symres_expression(&((AstBinaryOp *)(*expr))->left);
            symres_expression(&((AstBinaryOp *)(*expr))->right);

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

        default: break;
    }
}

static void symres_return(AstReturn* ret) {
    if (ret->expr)
        symres_expression(&ret->expr);
}

static void symres_if(AstIfWhile* ifnode) {
    if (ifnode->assignment != NULL) {
        ifnode->scope = scope_create(semstate.node_allocator, semstate.curr_scope);
        scope_enter(ifnode->scope);

        symbol_introduce(semstate.curr_scope, ifnode->local->token, (AstNode *) ifnode->local);

        symres_statement((AstNode **) &ifnode->assignment);
    }

    symres_expression(&ifnode->cond);

    if (ifnode->true_stmt != NULL)  symres_statement((AstNode **) &ifnode->true_stmt);
    if (ifnode->false_stmt != NULL) symres_statement((AstNode **) &ifnode->false_stmt);

    if (ifnode->assignment != NULL) scope_leave();
}

static void symres_while(AstIfWhile* whilenode) {
    if (whilenode->assignment != NULL) {
        whilenode->scope = scope_create(semstate.node_allocator, semstate.curr_scope);
        scope_enter(whilenode->scope);

        symbol_introduce(semstate.curr_scope, whilenode->local->token, (AstNode *) whilenode->local);

        symres_statement((AstNode **) &whilenode->assignment);
    }

    symres_expression(&whilenode->cond);

    if (whilenode->true_stmt)  symres_block(whilenode->true_stmt);
    if (whilenode->false_stmt) symres_block(whilenode->false_stmt);

    if (whilenode->assignment != NULL) scope_leave();
}

static void symres_for(AstFor* fornode) {
    fornode->scope = scope_create(semstate.node_allocator, semstate.curr_scope);
    scope_enter(fornode->scope);

    symres_expression(&fornode->iter);

    symbol_introduce(semstate.curr_scope, fornode->var->token, (AstNode *) fornode->var);

    symres_block(fornode->stmt);

    scope_leave();
}

static void symres_switch(AstSwitch* switchnode) {
    if (switchnode->assignment != NULL) {
        switchnode->scope = scope_create(semstate.node_allocator, semstate.curr_scope);
        scope_enter(switchnode->scope);

        symbol_introduce(semstate.curr_scope, switchnode->local->token, (AstNode *) switchnode->local);

        symres_statement((AstNode **) &switchnode->assignment);
    }

    symres_expression(&switchnode->expr);

    bh_arr_each(AstSwitchCase, sc, switchnode->cases) {
        symres_expression(&sc->value);
        symres_block(sc->block);
    }

    if (switchnode->default_case)
        symres_block(switchnode->default_case);

    if (switchnode->assignment != NULL) scope_leave();
}

// NOTE: Returns 1 if the statment should be removed
static b32 symres_statement(AstNode** stmt) {
    switch ((*stmt)->kind) {
        case Ast_Kind_Local:      symres_local((AstLocal **) stmt);                  return 1;
        case Ast_Kind_Return:     symres_return((AstReturn *) *stmt);                return 0;
        case Ast_Kind_If:         symres_if((AstIfWhile *) *stmt);                   return 0;
        case Ast_Kind_While:      symres_while((AstIfWhile *) *stmt);                return 0;
        case Ast_Kind_For:        symres_for((AstFor *) *stmt);                      return 0;
        case Ast_Kind_Switch:     symres_switch((AstSwitch *) *stmt);                return 0;
        case Ast_Kind_Call:       symres_call((AstCall *) *stmt);                    return 0;
        case Ast_Kind_Argument:   symres_expression((AstTyped **) &((AstArgument *) *stmt)->value); return 0;
        case Ast_Kind_Block:      symres_block((AstBlock *) *stmt);                  return 0;
        case Ast_Kind_Defer:      symres_statement(&((AstDefer *) *stmt)->stmt);     return 0;

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
        block->scope = scope_create(semstate.node_allocator, semstate.curr_scope);

    scope_enter(block->scope);
    bh_arr_push(semstate.block_stack, block);

    if (block->body)
        symres_statement_chain(&block->body);

    bh_arr_pop(semstate.block_stack);
    scope_leave();
}

void symres_function(AstFunction* func) {
    if (func->scope == NULL)
        func->scope = scope_create(semstate.node_allocator, semstate.curr_scope);

    bh_arr_each(AstParam, param, func->params) {
        if (param->default_value != NULL) {
            symres_expression(&param->default_value);
            if (onyx_has_errors()) return;

            if (check_expression(&param->default_value)) return;
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

    scope_enter(func->scope);

    bh_arr_each(AstParam, param, func->params) {
        if (param->local->type_node != NULL) {
            param->local->type_node = symres_type(param->local->type_node);
            param->local->type = type_build_from_ast(semstate.allocator, param->local->type_node);

            if (param->default_value != NULL) {
                if (!types_are_compatible(param->local->type, param->default_value->type)) {
                    onyx_report_error(param->local->token->pos,
                            "Expected default value of type '%s', was of type '%s'.",
                            type_get_name(param->local->type),
                            type_get_name(param->default_value->type));
                    return;
                }
            }
        } else {
            param->local->type = param->default_value->type;
        }

        if (param->local->type == NULL) {
            onyx_report_error(param->local->token->pos,
                    "Unable to resolve type for parameter, '%b'.\n",
                    param->local->token->text,
                    param->local->token->length);
            return;
        }

        symbol_introduce(semstate.curr_scope, param->local->token, (AstNode *) param->local);

        if (param->local->flags & Ast_Flag_Param_Use) {
            if (type_is_struct(param->local->type)) {
                AstStructType* st;
                if (param->local->type->kind == Type_Kind_Struct) {
                    st = (AstStructType *) param->local->type_node;
                } else {
                    st = (AstStructType *) ((AstPointerType *) param->local->type_node)->elem;
                }

                bh_arr_each(AstStructMember *, mem, st->members) {
                    AstFieldAccess* fa = onyx_ast_node_new(semstate.node_allocator, sizeof(AstFieldAccess), Ast_Kind_Field_Access);
                    fa->token = (*mem)->token;
                    fa->type_node = (*mem)->type_node;
                    fa->expr = (AstTyped *) param->local;

                    token_toggle_end((*mem)->token);
                    symbol_raw_introduce(semstate.curr_scope,
                            (*mem)->token->text,
                            param->local->token->pos,
                            (AstNode *) fa);
                    token_toggle_end((*mem)->token);
                }

            } else {
                onyx_report_error(param->local->token->pos, "can only 'use' structures or pointers to structures.");
            }
        }
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
            *node = (AstTyped *) symbol_resolve(semstate.curr_scope, (*node)->token);
        }
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

        symbol_introduce(semstate.curr_package->include_scope, package->alias, (AstNode *) pac_node);
    }

    if (package->only != NULL) {
        bh_arr_each(AstAlias *, alias, package->only) {

            AstNode* thing = symbol_resolve(p->scope, (*alias)->token);
            if (thing == NULL) {
                onyx_report_error((*alias)->token->pos, "not found in package");
                return;
            }

            symbol_introduce(semstate.curr_package->include_scope, (*alias)->alias, thing);
        }
    }

    if (package->alias == NULL && package->only == NULL) {
        b32 already_included = 0;
        bh_arr_each(Package *, included_package, semstate.curr_package->unqualified_uses) {
            if (*included_package == p) {
                already_included = 1;
                break;
            }
        }

        if (already_included) return;

        scope_include(semstate.curr_package->include_scope, p->scope);

        bh_arr_push(semstate.curr_package->unqualified_uses, p);
    }
}

static void symres_enum(AstEnumType* enum_node) {
    if (enum_node->backing->kind == Ast_Kind_Symbol) {
        enum_node->backing = (AstType *) symbol_resolve(semstate.curr_scope, enum_node->backing->token);
    }
    if (enum_node->backing == NULL) return;

    enum_node->backing_type = type_build_from_ast(semstate.allocator, enum_node->backing);
    enum_node->scope = scope_create(semstate.node_allocator, NULL);

    u64 next_assign_value = (enum_node->flags & Ast_Flag_Enum_Is_Flags) ? 1 : 0;
    bh_arr_each(AstEnumValue *, value, enum_node->values) {
        symbol_introduce(enum_node->scope, (*value)->token, (AstNode *) *value);

        if ((*value)->value != NULL) {
            // HACK
            if ((*value)->value->type_node == (AstType *) &basic_type_i32) {
                next_assign_value = (*value)->value->value.i;
            } else if ((*value)->value->type_node == (AstType *) &basic_type_i64) {
                next_assign_value = (*value)->value->value.l;
            } else {
                onyx_report_error((*value)->token->pos, "expected numeric integer literal for enum initialization");
                return;
            }

        } else {
            AstNumLit* num = onyx_ast_node_new(semstate.node_allocator, sizeof(AstNumLit), Ast_Kind_NumLit);
            num->value.l = next_assign_value;
            num->type_node = enum_node->backing;
            num->type = enum_node->backing_type;
            (*value)->value = num;
        }

        if (enum_node->flags & Ast_Flag_Enum_Is_Flags) {
            next_assign_value <<= 1;
        } else {
            next_assign_value++;
        }
    }
}

static void symres_memres(AstMemRes** memres) {
    (*memres)->type_node = symres_type((*memres)->type_node);
    if ((*memres)->initial_value != NULL) {
        symres_expression(&(*memres)->initial_value);

        if ((*memres)->type_node == NULL)
            (*memres)->type_node = (*memres)->initial_value->type_node;

    } else {
        if ((*memres)->type_node == NULL) return;
    }
}

static void symres_polyproc(AstPolyProc* pp) {
    pp->poly_scope = scope_create(semstate.node_allocator, semstate.curr_scope);
}

void onyx_resolve_symbols() {

    semstate.curr_scope = semstate.program->global_scope;

    bh_arr_each(Entity, entity, semstate.program->entities) {
        if (entity->package) {
            scope_enter(entity->package->private_scope);
            semstate.curr_package = entity->package;
        }

        switch (entity->type) {
            case Entity_Type_Use_Package:         symres_use_package(entity->use_package); break;
            case Entity_Type_Function:            symres_function(entity->function); break;
            case Entity_Type_Overloaded_Function: symres_overloaded_function(entity->overloaded_function); break;
            case Entity_Type_Global:              symres_global(entity->global); break;
            case Entity_Type_Expression:          symres_expression(&entity->expr); break;
            case Entity_Type_Type_Alias:          entity->type_alias = symres_type(entity->type_alias); break;
            case Entity_Type_Enum:                symres_enum(entity->enum_type); break;
            case Entity_Type_Memory_Reservation:  symres_memres(&entity->mem_res); break;
            case Entity_Type_Polymorphic_Proc:    symres_polyproc(entity->poly_proc); break;

            default: break;
        }

        if (entity->package) scope_leave();
    }
}
