#define BH_DEBUG
#include "onyxparser.h"
#include "onyxutils.h"
#include "onyxastnodes.h"
#include "onyxerrors.h"

// Variables used during the symbol resolution phase.
static Scope*       curr_scope    = NULL;
static Package*     curr_package  = NULL;
static AstFunction* curr_function = NULL;
bh_arr(AstBlock *)  block_stack   = NULL;
static b32 report_unresolved_symbols = 1;

#define SYMRES(kind, ...) do { \
    SymresStatus ss = symres_ ## kind (__VA_ARGS__); \
    if (ss > Symres_Errors_Start) return ss;         \
    } while (0)

typedef enum SymresStatus {
    Symres_Success,
    Symres_Complete,

    Symres_Errors_Start,
    Symres_Yield,
    Symres_Error,
} SymresStatus;

static SymresStatus symres_type(AstType** type);
static SymresStatus symres_local(AstLocal** local, b32 add_to_block_locals);
static SymresStatus symres_call(AstCall* call);
static SymresStatus symres_size_of(AstSizeOf* so);
static SymresStatus symres_align_of(AstAlignOf* so);
static SymresStatus symres_field_access(AstFieldAccess** fa);
static SymresStatus symres_compound(AstCompound* compound);
static SymresStatus symres_expression(AstTyped** expr);
static SymresStatus symres_return(AstReturn* ret);
static SymresStatus symres_if(AstIfWhile* ifnode);
static SymresStatus symres_while(AstIfWhile* whilenode);
static SymresStatus symres_for(AstFor* fornode);
static SymresStatus symres_switch(AstSwitch* switchnode);
static SymresStatus symres_use(AstUse* use);
static SymresStatus symres_directive_solidify(AstDirectiveSolidify** psolid);
static SymresStatus symres_statement_chain(AstNode** walker);
static SymresStatus symres_statement(AstNode** stmt, b32 *remove);
static SymresStatus symres_block(AstBlock* block);
static SymresStatus symres_function_header(AstFunction* func);
static SymresStatus symres_function(AstFunction* func);
static SymresStatus symres_global(AstGlobal* global);
static SymresStatus symres_overloaded_function(AstOverloadedFunction* ofunc);
static SymresStatus symres_use_package(AstUsePackage* package);
static SymresStatus symres_enum(AstEnumType* enum_node);
static SymresStatus symres_memres_type(AstMemRes** memres);
static SymresStatus symres_memres(AstMemRes** memres);
static SymresStatus symres_struct_defaults(AstType* st);
static SymresStatus symres_static_if(AstStaticIf* static_if);

static void scope_enter(Scope* new_scope) {
    curr_scope = new_scope;
}

static void scope_leave() {
    curr_scope = curr_scope->parent;
}

static SymresStatus symres_symbol(AstNode** symbol_node) {
    OnyxToken* token = (*symbol_node)->token;
    AstNode* res = symbol_resolve(curr_scope, token);    

    if (!res) { // :SymresStall
        if (report_unresolved_symbols) {
            onyx_report_error(token->pos,
                "Unable to resolve symbol '%b'",
                token->text,
                token->length);

            return Symres_Error;
        } else {
            return Symres_Yield;
        }

    } else {
        *symbol_node = res;
    }

    return Symres_Success;
}

static SymresStatus symres_struct_type(AstStructType* s_node) {
    if (s_node->flags & Ast_Flag_Type_Is_Resolved) return Symres_Success;

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
                return Symres_Error;
            }

            bh_table_put(i32, mem_set, (*member)->token->text, 1);
            token_toggle_end((*member)->token);
        }

        bh_table_free(mem_set);
    }

    if (s_node->scope) {
        // FIX: This is probably wrong for the long term.
        s_node->scope->parent = curr_scope;

        scope_enter(s_node->scope);
    }

    fori (i, 0, bh_arr_length(s_node->members)) {
        AstStructMember *member = s_node->members[i];

        if (member->type_node) {
            SYMRES(type, &member->type_node);

            if (!node_is_type((AstNode *) member->type_node)) {
                onyx_report_error(member->token->pos, "Member type is not a type.");
                goto struct_symres_done;
            }

            if (member->flags & Ast_Flag_Struct_Mem_Used) {
                AstType *used = (AstType *) member->type_node;

                while (used->kind == Ast_Kind_Type_Alias) {
                    used = ((AstTypeAlias *) used)->to;
                }

                b32 use_works = (used->kind == Ast_Kind_Struct_Type || used->kind == Ast_Kind_Poly_Call_Type);

                if (used->kind == Ast_Kind_Type_Raw_Alias) {
                    AstTypeRawAlias* alias = (AstTypeRawAlias *) used;
                    use_works = (alias->to->kind == Type_Kind_Struct);
                }

                if (!use_works) {
                    onyx_report_error(member->token->pos,
                            "Can only 'use' members of struct type, got '%s'.",
                            onyx_ast_node_kind_string(used->kind));
                    goto struct_symres_done;
                }
            }
        }
    }

struct_symres_done:
    if (s_node->scope) scope_leave();
    return Symres_Success;
}

static SymresStatus symres_type(AstType** type) {
    // Don't make this kill all symbol resolution if the type is null.
    if (!type || !*type) return Symres_Success;

    switch ((*type)->kind) {
        case Ast_Kind_Symbol:     SYMRES(symbol, (AstNode **) type); break;
        case Ast_Kind_Basic_Type: break;
        case Ast_Kind_Type_Alias: SYMRES(type, &((AstTypeAlias *) *type)->to); break;
        case Ast_Kind_Field_Access: {
            SYMRES(field_access, (AstFieldAccess **) type);

            if (!node_is_type((AstNode *) *type))
                onyx_report_error((*type)->token->pos, "Field access did not result in a type.");
            break;
        }

        case Ast_Kind_Pointer_Type: SYMRES(type, &((AstPointerType *) *type)->elem); break;
        case Ast_Kind_Slice_Type:   SYMRES(type, &((AstSliceType *) *type)->elem); break;
        case Ast_Kind_DynArr_Type:  SYMRES(type, &((AstDynArrType *) *type)->elem); break;
        case Ast_Kind_VarArg_Type:  SYMRES(type, &((AstVarArgType *) *type)->elem); break;

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype = (AstFunctionType *) *type;

            SYMRES(type, &ftype->return_type);

            if (ftype->param_count > 0) {
                fori (i, 0, (i64) ftype->param_count) {
                    SYMRES(type, &ftype->params[i]);
                }
            }
            break;
        }

        case Ast_Kind_Struct_Type: SYMRES(struct_type, (AstStructType *) *type); break;
        case Ast_Kind_Array_Type: {
            AstArrayType* a_node = (AstArrayType *) *type;

            if (a_node->count_expr) SYMRES(expression, &a_node->count_expr);
            SYMRES(type, &a_node->elem);
            break;
        }

        case Ast_Kind_Enum_Type: break;

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* pst_node = (AstPolyStructType *) *type;
            pst_node->scope = scope_create(context.ast_alloc, curr_scope, pst_node->token->pos);

            bh_arr_each(AstPolyStructParam, param, pst_node->poly_params) {
                SYMRES(type, &param->type_node);
                param->type = type_build_from_ast(context.ast_alloc, param->type_node);
            }
            break;
        }

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pc_node = (AstPolyCallType *) *type;

            SYMRES(type, &pc_node->callee);

            bh_arr_each(AstNode *, param, pc_node->params) {
                if (node_is_type(*param)) {
                    SYMRES(type, (AstType **) param);
                } else {
                    SYMRES(expression, (AstTyped **) param);
                }
            }
            break;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* ctype = (AstCompoundType *) *type;

            bh_arr_each(AstType *, type, ctype->types) SYMRES(type, type);
        }
    }

    return Symres_Success;
}

static SymresStatus symres_local(AstLocal** local, b32 add_to_block_locals) {
    SYMRES(type, &(*local)->type_node);

    // NOTE: This is a little gross, but it is allows for finer control
    // over when locals are in scope in a block, which reduces the number
    // of unique WASM locals and stack space needed.
    //                                            - brendanfh 2020/12/16
    if (add_to_block_locals)
        bh_arr_push(bh_arr_last(block_stack)->allocate_exprs, (AstTyped *) *local);

    bh_arr_push(curr_function->allocate_exprs, (AstTyped *) *local);

    if ((*local)->token != NULL)
        symbol_introduce(curr_scope, (*local)->token, (AstNode *) *local);

    return Symres_Success;
}

static SymresStatus symres_arguments(Arguments* args) {
    bh_arr_each(AstTyped *, arg, args->values)
        SYMRES(expression, arg);

    bh_arr_each(AstNamedValue *, named_arg, args->named_values)
        SYMRES(expression, &(*named_arg)->value);

    return Symres_Success;
}

static SymresStatus symres_call(AstCall* call) {
    SYMRES(expression, (AstTyped **) &call->callee);
    SYMRES(arguments, &call->args);

    return Symres_Success;
}

static SymresStatus symres_size_of(AstSizeOf* so) {
    SYMRES(type, &so->type_node);
    SYMRES(type, &so->so_ast_type);

    return Symres_Success;
}

static SymresStatus symres_align_of(AstAlignOf* ao) {
    SYMRES(type, &ao->type_node);
    SYMRES(type, &ao->ao_ast_type);

    return Symres_Success;
}

static SymresStatus symres_field_access(AstFieldAccess** fa) {
    if ((*fa)->expr == NULL) return Symres_Error;
    SYMRES(expression, &(*fa)->expr);
    if ((*fa)->expr == NULL) return Symres_Error;

    AstNode* resolution = try_symbol_resolve_from_node((AstNode *) (*fa)->expr, (*fa)->token);
    if (resolution) *((AstNode **) fa) = resolution;

    return Symres_Success;
}

static SymresStatus symres_compound(AstCompound* compound) {
    bh_arr_each(AstTyped *, expr, compound->exprs) {
        SYMRES(expression, expr);
    }

    return Symres_Success;
}

static SymresStatus symres_pipe(AstBinaryOp** pipe) {
    AstCall* call_node = (AstCall *) (*pipe)->right;
    SYMRES(expression, (AstTyped **) &call_node);
    SYMRES(expression, &(*pipe)->left);

    if (call_node->kind != Ast_Kind_Call) {
        onyx_report_error((*pipe)->token->pos, "Pipe operator expected call on right side.");
        return Symres_Error;
    }

    if ((*pipe)->left == NULL) return Symres_Error;

    bh_arr_insertn(call_node->args.values, 0, 1);
    call_node->args.values[0] = (AstTyped *) make_argument(context.ast_alloc, (*pipe)->left);
    call_node->next = (*pipe)->next;

    // NOTE: Not a BinaryOp node
    *pipe = (AstBinaryOp *) call_node;

    return Symres_Success;
}

// CLEANUP: This is an experimental feature and might be removed in the future.
// I noticed a common pattern when writing in Onyx is something that looks like this:
//
//     foo.member_function(^foo, ...)
//
// I decided it would be worth adding a bit of syntactic sugar for such as call. I
// decided to use the '->' operator for this purpose. The snippet below is the exact
// same as the snippet above (after the nodes have been processed by the function below)
//
//     foo->member_function(...)
static SymresStatus symres_method_call(AstBinaryOp** mcall) {
    AstCall* call_node = (AstCall *) (*mcall)->right;
    if (call_node->kind != Ast_Kind_Call) {
        onyx_report_error((*mcall)->token->pos, "'->' expected procedure call on right side.");
        return Symres_Error;
    }

    SYMRES(expression, &(*mcall)->left);
    if ((*mcall)->left == NULL) return Symres_Error;

    AstFieldAccess* implicit_field_access = make_field_access(context.ast_alloc, (*mcall)->left, NULL);
    implicit_field_access->token = call_node->callee->token;
    call_node->callee = (AstTyped *) implicit_field_access;
    SYMRES(expression, (AstTyped **) &call_node);

    return Symres_Success;
}

static SymresStatus symres_unaryop(AstUnaryOp** unaryop) {
    if ((*unaryop)->operation == Unary_Op_Cast) {
        SYMRES(type, &(*unaryop)->type_node);
    }

    SYMRES(expression, &(*unaryop)->expr);

    return Symres_Success;
}

static SymresStatus symres_struct_literal(AstStructLiteral* sl) {
    if (sl->stnode != NULL) SYMRES(expression, &sl->stnode);
    SYMRES(type, (AstType **) &sl->stnode);

    sl->type_node = (AstType *) sl->stnode;
    while (sl->type_node->kind == Ast_Kind_Type_Alias)
        sl->type_node = ((AstTypeAlias *) sl->type_node)->to;

    SYMRES(arguments, &sl->args);

    return Symres_Success;
}

static SymresStatus symres_array_literal(AstArrayLiteral* al) {
    if (al->atnode != NULL) SYMRES(expression, &al->atnode);
    SYMRES(type, (AstType **) &al->atnode);

    al->type_node = (AstType *) al->atnode;
    while (al->type_node->kind == Ast_Kind_Type_Alias)
        al->type_node = ((AstTypeAlias *) al->type_node)->to;

    bh_arr_each(AstTyped *, expr, al->values)
        SYMRES(expression, expr);

    if (bh_arr_length(block_stack) > 0) {
        bh_arr_push(bh_arr_last(block_stack)->allocate_exprs, (AstTyped *) al);
        bh_arr_push(curr_function->allocate_exprs, (AstTyped *) al);
    }

    return Symres_Success;
}

static SymresStatus symres_expression(AstTyped** expr) {
    if (node_is_type((AstNode *) *expr)) {
        SYMRES(type, (AstType **) expr);
        return Symres_Success;
    }

    switch ((*expr)->kind) {
        case Ast_Kind_Symbol: SYMRES(symbol, (AstNode **) expr); break;

        case Ast_Kind_Binary_Op:
            SYMRES(expression, &((AstBinaryOp *)(*expr))->left);
            SYMRES(expression, &((AstBinaryOp *)(*expr))->right);
            break;

        case Ast_Kind_Unary_Op:     SYMRES(unaryop, (AstUnaryOp **) expr); break;
        case Ast_Kind_Call:         SYMRES(call, (AstCall *) *expr); break;
        case Ast_Kind_Argument:     SYMRES(expression, &((AstArgument *) *expr)->value); break;
        case Ast_Kind_Block:        SYMRES(block, (AstBlock *) *expr); break;
        case Ast_Kind_Address_Of:   SYMRES(expression, &((AstAddressOf *)(*expr))->expr); break;
        case Ast_Kind_Dereference:  SYMRES(expression, &((AstDereference *)(*expr))->expr); break;
        case Ast_Kind_Field_Access: SYMRES(field_access, (AstFieldAccess **) expr); break;
        case Ast_Kind_Pipe:         SYMRES(pipe, (AstBinaryOp **) expr); break;
        case Ast_Kind_Method_Call:  SYMRES(method_call, (AstBinaryOp **) expr); break;
        case Ast_Kind_Size_Of:      SYMRES(size_of, (AstSizeOf *)*expr); break;
        case Ast_Kind_Align_Of:     SYMRES(align_of, (AstAlignOf *)*expr); break;

        case Ast_Kind_Range_Literal:
            SYMRES(expression, &((AstRangeLiteral *)(*expr))->low);
            SYMRES(expression, &((AstRangeLiteral *)(*expr))->high);

            SYMRES(type, &builtin_range_type);
            (*expr)->type_node = builtin_range_type;

            // NOTE: This is a weird place to put this so maybe put it somewhere else eventually
            //                                                  - brendanfh   2020/09/04
            builtin_range_type_type = type_build_from_ast(context.ast_alloc, builtin_range_type);
            break;

        case Ast_Kind_Function:
        case Ast_Kind_NumLit:
            SYMRES(type, &(*expr)->type_node);
            break;

        case Ast_Kind_StrLit:
            SYMRES(type, &builtin_string_type);
            (*expr)->type_node = builtin_string_type;
            break;

        case Ast_Kind_Slice:
        case Ast_Kind_Array_Access:
            SYMRES(expression, &((AstArrayAccess *)(*expr))->addr);
            SYMRES(expression, &((AstArrayAccess *)(*expr))->expr);
            break;

        case Ast_Kind_Struct_Literal:
            SYMRES(struct_literal, (AstStructLiteral *)(*expr));
            break;

        case Ast_Kind_Array_Literal:
            SYMRES(array_literal, (AstArrayLiteral *)(*expr));
            break;

        case Ast_Kind_Directive_Solidify:
            SYMRES(directive_solidify, (AstDirectiveSolidify **) expr);
            break;

        case Ast_Kind_Compound:
            SYMRES(compound, (AstCompound *) *expr);
            break;

        default: break;
    }

    return Symres_Success;
}

static SymresStatus symres_return(AstReturn* ret) {
    if (ret->expr)
        SYMRES(expression, &ret->expr);

    return Symres_Success;
}

static SymresStatus symres_if(AstIfWhile* ifnode) {
    if (ifnode->assignment != NULL) {
        ifnode->scope = scope_create(context.ast_alloc, curr_scope, ifnode->token->pos);
        scope_enter(ifnode->scope);

        SYMRES(local, &ifnode->local, 0);

        SYMRES(statement, (AstNode **) &ifnode->assignment, NULL);
    }

    SYMRES(expression, &ifnode->cond);

    if (ifnode->true_stmt != NULL)  SYMRES(statement, (AstNode **) &ifnode->true_stmt, NULL);
    if (ifnode->false_stmt != NULL) SYMRES(statement, (AstNode **) &ifnode->false_stmt, NULL);

    if (ifnode->assignment != NULL) scope_leave();

    return Symres_Success;
}

static SymresStatus symres_while(AstIfWhile* whilenode) {
    if (whilenode->assignment != NULL) {
        whilenode->scope = scope_create(context.ast_alloc, curr_scope, whilenode->token->pos);
        scope_enter(whilenode->scope);

        SYMRES(local, &whilenode->local, 0);

        SYMRES(statement, (AstNode **) &whilenode->assignment, NULL);
    }

    SYMRES(expression, &whilenode->cond);

    if (whilenode->true_stmt)  SYMRES(block, whilenode->true_stmt);
    if (whilenode->false_stmt) SYMRES(block, whilenode->false_stmt);

    if (whilenode->assignment != NULL) scope_leave();

    return Symres_Success;
}

static SymresStatus symres_for(AstFor* fornode) {
    fornode->scope = scope_create(context.ast_alloc, curr_scope, fornode->token->pos);
    scope_enter(fornode->scope);
    SYMRES(expression, &fornode->iter);
    SYMRES(local, &fornode->var, 0);
    SYMRES(block, fornode->stmt);
    scope_leave();

    return Symres_Success;
}

static SymresStatus symres_switch(AstSwitch* switchnode) {
    if (switchnode->assignment != NULL) {
        switchnode->scope = scope_create(context.ast_alloc, curr_scope, switchnode->token->pos);
        scope_enter(switchnode->scope);

        symbol_introduce(curr_scope, switchnode->local->token, (AstNode *) switchnode->local);

        SYMRES(statement, (AstNode **) &switchnode->assignment, NULL);
    }

    SYMRES(expression, &switchnode->expr);

    bh_arr_each(AstSwitchCase, sc, switchnode->cases) {
        bh_arr_each(AstTyped *, value, sc->values)
            SYMRES(expression, value);
            
        SYMRES(block, sc->block);
    }

    if (switchnode->default_case)
        SYMRES(block, switchnode->default_case);

    if (switchnode->assignment != NULL) scope_leave();
    return Symres_Success;
}

static SymresStatus symres_use(AstUse* use) {
    SYMRES(expression, &use->expr);

    if (use->expr->kind == Ast_Kind_Enum_Type) {
        AstEnumType* et = (AstEnumType *) use->expr;

        bh_arr_each(AstEnumValue *, ev, et->values)
            symbol_introduce(curr_scope, (*ev)->token, (AstNode *) *ev);

        return Symres_Success;
    }

    if (use->expr->kind == Ast_Kind_Struct_Type) {
        AstStructType* st = (AstStructType *) use->expr;

        if (st->scope)
            scope_include(curr_scope, st->scope, use->token->pos);

        return Symres_Success;
    }

    if (use->expr->type_node == NULL && use->expr->type == NULL) goto cannot_use;

    AstType* effective_type = use->expr->type_node;
    if (effective_type->kind == Ast_Kind_Pointer_Type)
        effective_type = ((AstPointerType *) effective_type)->elem;

    if (effective_type->kind == Ast_Kind_Struct_Type ||
            effective_type->kind == Ast_Kind_Poly_Call_Type) {

        if (use->expr->type == NULL)
            use->expr->type = type_build_from_ast(context.ast_alloc, use->expr->type_node);
        if (use->expr->type == NULL) goto cannot_use;

        Type* st = use->expr->type;
        if (st->kind == Type_Kind_Pointer)
            st = st->Pointer.elem;

        bh_table_each_start(StructMember, st->Struct.members);
            AstFieldAccess* fa = make_field_access(context.ast_alloc, use->expr, value.name);
            symbol_raw_introduce(curr_scope, value.name, use->token->pos, (AstNode *) fa);
        bh_table_each_end;

        return Symres_Success;
    }

cannot_use:
    onyx_report_error(use->token->pos, "Cannot use this because its type is unknown.");
    return Symres_Error;
}

static SymresStatus symres_directive_solidify(AstDirectiveSolidify** psolid) {
    AstDirectiveSolidify* solid = *psolid;
    if (solid->resolved_proc != NULL)
        *psolid = (AstDirectiveSolidify *) solid->resolved_proc;

    SYMRES(expression, (AstTyped **) &solid->poly_proc);
    if (solid->poly_proc && solid->poly_proc->kind == Ast_Kind_Directive_Solidify) {
        solid->poly_proc = (AstPolyProc *) ((AstDirectiveSolidify *) solid->poly_proc)->resolved_proc;
    }
    
    if (!solid->poly_proc || solid->poly_proc->kind != Ast_Kind_Polymorphic_Proc) {
        onyx_report_error(solid->token->pos, "Expected polymorphic procedure in #solidify directive.");
        return Symres_Error;
    }

    bh_arr_each(AstPolySolution, sln, solid->known_polyvars) {
        // HACK: This assumes that 'ast_type' and 'value' are at the same offset.
        SYMRES(expression, &sln->value);
        if (onyx_has_errors()) return Symres_Error;

        if (node_is_type((AstNode *) sln->value)) {
            sln->type = type_build_from_ast(context.ast_alloc, sln->ast_type);
            sln->kind = PSK_Type;
        } else {
            sln->kind = PSK_Value;
        }

        if (onyx_has_errors()) return Symres_Error;
    }

    solid->resolved_proc = polymorphic_proc_try_solidify(solid->poly_proc, solid->known_polyvars, solid->token);

    // NOTE: Not a DirectiveSolidify.
    *psolid = (AstDirectiveSolidify *) solid->resolved_proc;
    return Symres_Success;
}

static SymresStatus symres_statement(AstNode** stmt, b32 *remove) {
    if (remove) *remove = 0;

    switch ((*stmt)->kind) {
        case Ast_Kind_Return:      SYMRES(return, (AstReturn *) *stmt);                  break;
        case Ast_Kind_If:          SYMRES(if, (AstIfWhile *) *stmt);                     break;
        case Ast_Kind_While:       SYMRES(while, (AstIfWhile *) *stmt);                  break;
        case Ast_Kind_For:         SYMRES(for, (AstFor *) *stmt);                        break;
        case Ast_Kind_Switch:      SYMRES(switch, (AstSwitch *) *stmt);                  break;
        case Ast_Kind_Call:        SYMRES(call, (AstCall *) *stmt);                      break;
        case Ast_Kind_Argument:    SYMRES(expression, (AstTyped **) &((AstArgument *) *stmt)->value); break;
        case Ast_Kind_Block:       SYMRES(block, (AstBlock *) *stmt);                    break;
        case Ast_Kind_Defer:       SYMRES(statement, &((AstDefer *) *stmt)->stmt, NULL); break;
        case Ast_Kind_Jump:        break;

        case Ast_Kind_Local:
            if (remove) *remove = 1;
            SYMRES(local, (AstLocal **) stmt, 1);
            break;

        case Ast_Kind_Use:
            if (remove) *remove = 1;
            SYMRES(use, (AstUse *) *stmt);
            break;

        case Ast_Kind_Use_Package:
            if (remove) *remove = 1;
            SYMRES(use_package, (AstUsePackage *) *stmt);
            break;

        default: SYMRES(expression, (AstTyped **) stmt); break;
    }

    return Symres_Success;
}

static SymresStatus symres_statement_chain(AstNode** walker) {
    b32 remove = 0;

    while (*walker) {
        SYMRES(statement, walker, &remove);
        if (remove) {
            remove = 0;
            AstNode* tmp = (*walker)->next;
            (*walker)->next = NULL;
            (*walker) = tmp;

        } else {
            walker = &(*walker)->next;
        }
    }
    return Symres_Success;
}

static SymresStatus symres_block(AstBlock* block) {
    if (block->scope == NULL)
        block->scope = scope_create(context.ast_alloc, curr_scope, block->token->pos);

    scope_enter(block->scope);
    bh_arr_push(block_stack, block);

    if (block->binding_scope != NULL)
        scope_include(block->scope, block->binding_scope, block->token->pos);

    if (block->body)
        SYMRES(statement_chain, &block->body);

    bh_arr_pop(block_stack);
    scope_leave();
    return Symres_Success;
}

SymresStatus symres_function_header(AstFunction* func) {
    func->flags |= Ast_Flag_Comptime;

    bh_arr_each(AstParam, param, func->params) {
        if (param->default_value != NULL) {
            SYMRES(expression, &param->default_value);
            if (onyx_has_errors()) return Symres_Error;

            // HACK: It shouldn't be necessary to do this twice, but right now
            // if `null` is the default parameter and it hasn't been used anywhere in
            // code yet, it doesn't resolve properly. So for now I am just checking symbols twice.
            //                                                      -brendanfh 2020/12/24
            SYMRES(expression, &param->default_value);
            if (onyx_has_errors()) return Symres_Error;
        }
    }
    
    if ((func->flags & Ast_Flag_From_Polymorphism) == 0) {
        if (func->overloaded_function != NULL) {
            SYMRES(expression, (AstTyped **) &func->overloaded_function);
            if (func->overloaded_function == NULL) return Symres_Error; // NOTE: Error message will already be generated

            if (func->overloaded_function->kind != Ast_Kind_Overloaded_Function) {
                onyx_report_error(func->token->pos, "#add_overload directive did not resolve to an overloaded function.");

            } else {
                AstOverloadedFunction* ofunc = (AstOverloadedFunction *) func->overloaded_function;
                bh_arr_push(ofunc->overloads, (AstTyped *) func);
            }
        }

        if (func->operator_overload != (BinaryOp) -1) {
            if (bh_arr_length(func->params) != 2) {
                onyx_report_error(func->token->pos, "Expected 2 exactly arguments for binary operator overload.");
            }

            if (binop_is_assignment(func->operator_overload)) {
                onyx_report_error(func->token->pos, "'%s' is not currently overloadable.", binaryop_string[func->operator_overload]);
            }

            bh_arr_push(operator_overloads[func->operator_overload], (AstTyped *) func);
        }
    }

    SYMRES(type, &func->return_type);
    if (!node_is_type((AstNode *) func->return_type)) {
        onyx_report_error(func->token->pos, "Return type is not a type.");
    }

    bh_arr_each(AstParam, param, func->params) {
        if (param->local->type_node != NULL) {
            SYMRES(type, &param->local->type_node);
        }
    }
    return Symres_Success;
}

SymresStatus symres_function(AstFunction* func) {
    if (func->scope == NULL)
        func->scope = scope_create(context.ast_alloc, curr_scope, func->token->pos);

    scope_enter(func->scope);

    bh_arr_each(AstParam, param, func->params) {
        symbol_introduce(curr_scope, param->local->token, (AstNode *) param->local);

        // CLEANUP: Currently, in order to 'use' parameters, the type must be completely
        // resolved and built. This is excessive because all that should need to be known
        // is the names of the members, since all that happens is implicit field accesses
        // are placed in the scope. So instead, there should be a way to just query all the
        // member names in the structure, without needing to know their type. This would be
        // easy if it were not for 'use' statements in structs. It is made even more complicated
        // by this situtation:
        //
        //     Foo :: struct (T: type_expr) {
        //         use t : T;
        // 
        //         something_else := 5 + 6 * 8;
        //     }
        //
        // The 'use t : T' member requires completely knowing the type of T, to know which
        // members should be brought in. At the moment, that requires completely building the
        // type of Foo($T).
        if (param->local->flags & Ast_Flag_Param_Use) {
            if (param->local->type_node != NULL && param->local->type == NULL) {
                param->local->type = type_build_from_ast(context.ast_alloc, param->local->type_node);
            }

            if (type_is_struct(param->local->type)) {
                Type* st;
                if (param->local->type->kind == Type_Kind_Struct) {
                    st = param->local->type;
                } else {
                    st = param->local->type->Pointer.elem;
                }

                bh_table_each_start(StructMember, st->Struct.members);
                    AstFieldAccess* fa = make_field_access(context.ast_alloc, (AstTyped *) param->local, value.name);
                    symbol_raw_introduce(curr_scope, value.name, param->local->token->pos, (AstNode *) fa);
                bh_table_each_end;

            } else if (param->local->type != NULL) {
                onyx_report_error(param->local->token->pos, "Can only 'use' structures or pointers to structures.");

            } else {
                // :ExplicitTyping
                onyx_report_error(param->local->token->pos, "Cannot deduce type of parameter '%b'; Try adding it explicitly.",
                    param->local->token->text,
                    param->local->token->length);
            }
        }
    }

    curr_function = func;
    SYMRES(block, func->body);

    scope_leave();
    return Symres_Success;
}

static SymresStatus symres_global(AstGlobal* global) {
    SYMRES(type, &global->type_node);
    return Symres_Success;
}

static SymresStatus symres_overloaded_function(AstOverloadedFunction* ofunc) {
    bh_arr_each(AstTyped *, node, ofunc->overloads) {
        SYMRES(expression, node);
    }
    return Symres_Success;
}

static SymresStatus symres_use_package(AstUsePackage* package) {
    token_toggle_end(package->package->token);
    Package* p = package_lookup(package->package->token->text);
    token_toggle_end(package->package->token);

    if (p == NULL) { // :SymresStall
        if (report_unresolved_symbols) {
            onyx_report_error(package->token->pos, "package not found in included source files");
            return Symres_Error;
        } else {
            return Symres_Yield;
        }
    }

    if (p->scope == curr_scope) return Symres_Success;

    if (package->alias != NULL) {
        AstPackage *pac_node = onyx_ast_node_new(context.ast_alloc, sizeof(AstPackage), Ast_Kind_Package);
        pac_node->package = p;
        pac_node->token = package->alias;

        symbol_introduce(curr_scope, package->alias, (AstNode *) pac_node);
    }

    if (package->only != NULL) {
        bh_arr_each(AstAlias *, alias, package->only) {

            AstNode* thing = symbol_resolve(p->scope, (*alias)->token);
            if (thing == NULL) { // :SymresStall
                if (report_unresolved_symbols) {
                    onyx_report_error((*alias)->token->pos, "This symbol was not found in this package.");
                    return Symres_Error;
                } else {
                    return Symres_Yield;
                }
            }

            symbol_introduce(curr_scope, (*alias)->alias, thing);
        }
    }

    if (package->alias == NULL && package->only == NULL) {
        OnyxFilePos pos = { 0 };
        if (package->token != NULL)
            pos = package->token->pos;

        scope_include(curr_scope, p->scope, pos);
    }
    return Symres_Success;
}

static SymresStatus symres_enum(AstEnumType* enum_node) {
    if (enum_node->backing->kind == Ast_Kind_Symbol) SYMRES(symbol, (AstNode **) &enum_node->backing);
    if (enum_node->backing == NULL) return Symres_Error;

    enum_node->backing_type = type_build_from_ast(context.ast_alloc, enum_node->backing);
    enum_node->scope = scope_create(context.ast_alloc, NULL, enum_node->token->pos);

    type_build_from_ast(context.ast_alloc, (AstType *) enum_node);

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
                return Symres_Error;
            }

            (*value)->value->type = enum_node->etcache;

        } else {
            AstNumLit* num = make_int_literal(context.ast_alloc, next_assign_value);
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
    return Symres_Success;
}

static SymresStatus symres_memres_type(AstMemRes** memres) {
    SYMRES(type, &(*memres)->type_node);
    return Symres_Success;
}

static SymresStatus symres_memres(AstMemRes** memres) {
    if ((*memres)->initial_value != NULL) {
        SYMRES(expression, &(*memres)->initial_value);
    }
    return Symres_Success;
}

static SymresStatus symres_struct_defaults(AstType* t) {
    if (t->kind != Ast_Kind_Struct_Type) return Symres_Error;
    
    AstStructType* st = (AstStructType *) t;
    if (st->scope) scope_enter(st->scope);
    
    bh_arr_each(AstStructMember *, smem, st->members) {
        if ((*smem)->initial_value != NULL) {
            SYMRES(expression, &(*smem)->initial_value);
            
            if ((*smem)->type_node == NULL && (*smem)->initial_value->type_node != NULL) {
                (*smem)->type_node = (*smem)->initial_value->type_node;
            }
        }
    }
    
    if (st->scope) scope_leave();
    return Symres_Success;
}

static SymresStatus symres_polyproc(AstPolyProc* pp) {
    pp->poly_scope = curr_scope;

    bh_arr_each(AstPolyParam, param, pp->poly_params) {
        if (param->kind != PPK_Baked_Value) continue;

        SYMRES(type, &param->type_expr);
    }

    // CLEANUP: This was copied from symres_function_header.
    if (pp->base_func->operator_overload != (BinaryOp) -1) {
        if (bh_arr_length(pp->base_func->params) != 2) {
            onyx_report_error(pp->base_func->token->pos, "Expected 2 exactly arguments for binary operator overload.");
        }

        if (binop_is_assignment(pp->base_func->operator_overload)) {
            onyx_report_error(pp->base_func->token->pos, "'%s' is not currently overloadable.", binaryop_string[pp->base_func->operator_overload]);
        }

        bh_arr_push(operator_overloads[pp->base_func->operator_overload], (AstTyped *) pp);
    }
    return Symres_Success;
}

static SymresStatus symres_static_if(AstStaticIf* static_if) {
    SYMRES(expression, &static_if->cond);
    return Symres_Success;
}

void symres_entity(Entity* ent) {
    if (block_stack == NULL) bh_arr_new(global_heap_allocator, block_stack, 16);

    if (ent->package) curr_package = ent->package;

    Scope* old_scope = NULL;
    if (ent->scope) {
        old_scope = curr_scope;
        scope_enter(ent->scope);
    }

    report_unresolved_symbols = (context.entities.type_count[Entity_Type_Static_If] == 0 &&
                                 context.entities.type_count[Entity_Type_Use_Package] == 0)
                                || context.cycle_detected;

    SymresStatus ss = Symres_Success;
    EntityState next_state = Entity_State_Check_Types;

    switch (ent->type) {
        case Entity_Type_Binding: {
            symbol_introduce(curr_scope, ent->binding->token, ent->binding->node);
            next_state = Entity_State_Finalized;
            break;
        }

        case Entity_Type_Static_If: {
            ss = symres_static_if(ent->static_if);
            next_state = Entity_State_Comptime_Check_Types;
            break;
        }
        
        case Entity_Type_Foreign_Function_Header:
        case Entity_Type_Function_Header:         ss = symres_function_header(ent->function); break;
        case Entity_Type_Function:                ss = symres_function(ent->function);        break;

        case Entity_Type_Foreign_Global_Header:
        case Entity_Type_Global_Header:       ss = symres_global(ent->global); break;

        case Entity_Type_Use_Package:         ss = symres_use_package(ent->use_package);
                                              next_state = Entity_State_Finalized;
                                              break;

        case Entity_Type_Use:                 ss = symres_use(ent->use);
                                              next_state = Entity_State_Finalized;
                                              break;

        case Entity_Type_Overloaded_Function:     ss = symres_overloaded_function(ent->overloaded_function); break;
        case Entity_Type_Expression:              ss = symres_expression(&ent->expr); break;
        case Entity_Type_Type_Alias:              ss = symres_type(&ent->type_alias); break;
        case Entity_Type_Enum:                    ss = symres_enum(ent->enum_type); break;
        case Entity_Type_Memory_Reservation_Type: ss = symres_memres_type(&ent->mem_res); break;
        case Entity_Type_Memory_Reservation:      ss = symres_memres(&ent->mem_res); break;
        case Entity_Type_Polymorphic_Proc:        ss = symres_polyproc(ent->poly_proc); break;
        case Entity_Type_String_Literal:          ss = symres_expression(&ent->expr); break;
        case Entity_Type_Struct_Member_Default:   ss = symres_struct_defaults((AstType *) ent->type_alias); break;

        default: break;
    }

    if (ss == Symres_Success) ent->state = next_state;
    if (ss == Symres_Yield)   ent->attempts++;

    if (ent->scope) curr_scope = old_scope;
}
