#define BH_DEBUG
#include "parser.h"
#include "utils.h"
#include "astnodes.h"
#include "errors.h"
#include "doc.h"

// :EliminatingSymres - notes the places where too much work is being done in symbol resolution

// Variables used during the symbol resolution phase.
static Scope*       current_scope    = NULL;
static b32 report_unresolved_symbols = 1;
static b32 resolved_a_symbol         = 0;

static Entity* current_entity = NULL;

#define SYMRES(kind, ...) do { \
    SymresStatus ss = symres_ ## kind (__VA_ARGS__); \
    if (ss > Symres_Errors_Start) return ss;         \
    } while (0)

#define SYMRES_INVISIBLE(kind, node, ...) do { \
    (node)->flags |= Ast_Flag_Symbol_Invisible; \
    SymresStatus ss = symres_ ## kind (__VA_ARGS__); \
    (node)->flags &= ~Ast_Flag_Symbol_Invisible; \
    if (ss > Symres_Errors_Start) return ss;         \
    } while (0)

typedef enum SymresStatus {
    Symres_Success,
    Symres_Complete,
    Symres_Goto_Parse,

    Symres_Errors_Start,
    Symres_Yield_Macro,
    Symres_Yield_Micro,
    Symres_Error,
} SymresStatus;

static SymresStatus symres_type(AstType** type);
static SymresStatus symres_local(AstLocal** local);
static SymresStatus symres_call(AstCall** pcall);
static SymresStatus symres_size_of(AstSizeOf* so);
static SymresStatus symres_align_of(AstAlignOf* so);
static SymresStatus symres_field_access(AstFieldAccess** fa);
static SymresStatus symres_compound(AstCompound* compound);
static SymresStatus symres_expression(AstTyped** expr);
static SymresStatus symres_return(AstReturn* ret);
static SymresStatus symres_if(AstIfWhile* ifnode);
static SymresStatus symres_while(AstIfWhile* whilenode);
static SymresStatus symres_for(AstFor* fornode);
static SymresStatus symres_case(AstSwitchCase *casenode);
static SymresStatus symres_switch(AstSwitch* switchnode);
static SymresStatus symres_directive_solidify(AstDirectiveSolidify** psolid);
static SymresStatus symres_directive_defined(AstDirectiveDefined** pdefined);
static SymresStatus symres_directive_insert(AstDirectiveInsert* insert);
static SymresStatus symres_statement_chain(AstNode** walker);
static SymresStatus symres_statement(AstNode** stmt, b32 *remove);
static SymresStatus symres_block(AstBlock* block);
static SymresStatus symres_function_header(AstFunction* func);
static SymresStatus symres_function(AstFunction* func);
static SymresStatus symres_global(AstGlobal* global);
static SymresStatus symres_overloaded_function(AstOverloadedFunction* ofunc);
static SymresStatus symres_package(AstPackage* package);
static SymresStatus symres_enum(AstEnumType* enum_node);
static SymresStatus symres_memres_type(AstMemRes** memres);
static SymresStatus symres_memres(AstMemRes** memres);
static SymresStatus symres_struct_defaults(AstType* st);
static SymresStatus symres_static_if(AstIf* static_if);
static SymresStatus symres_macro(AstMacro* macro);
static SymresStatus symres_constraint(AstConstraint* constraint);
static SymresStatus symres_polyquery(AstPolyQuery *query);

static void scope_enter(Scope* new_scope) {
    current_scope = new_scope;
}

static void scope_leave() {
    current_scope = current_scope->parent;
}

static SymresStatus symres_symbol(AstNode** symbol_node) {
    OnyxToken* token = (*symbol_node)->token;
    AstNode* res = symbol_resolve(current_scope, token);

    if (!res) { // :SymresStall
        if (report_unresolved_symbols) {
            token_toggle_end(token);
            char *closest = find_closest_symbol_in_scope_and_parents(current_scope, token->text);
            token_toggle_end(token);

            if (closest) onyx_report_error(token->pos, Error_Critical, "Unable to resolve symbol '%b'. Did you mean '%s'?", token->text, token->length, closest);
            else         onyx_report_error(token->pos, Error_Critical, "Unable to resolve symbol '%b'.", token->text, token->length);

            return Symres_Error;
        } else {
            return Symres_Yield_Macro;
        }

    } else {
        track_resolution_for_symbol_info(*symbol_node, res);
        *symbol_node = res;
        resolved_a_symbol = 1;
    }

    return Symres_Success;
}

static SymresStatus symres_struct_type(AstStructType* s_node) {
    if (s_node->flags & Ast_Flag_Type_Is_Resolved) return Symres_Success;

    s_node->flags |= Ast_Flag_Type_Is_Resolved;
    s_node->flags |= Ast_Flag_Comptime;

    assert(s_node->scope);
    scope_enter(s_node->scope);
    
    if (s_node->min_size_)      SYMRES(expression, &s_node->min_size_);
    if (s_node->min_alignment_) SYMRES(expression, &s_node->min_alignment_);

    if (s_node->polymorphic_argument_types) {
        assert(s_node->polymorphic_arguments);

        SymresStatus ss = Symres_Success, result;
        fori (i, 0, (i64) bh_arr_length(s_node->polymorphic_argument_types)) {
            result = symres_type(&s_node->polymorphic_argument_types[i]);
            if (result > ss) ss = result;

            if (s_node->polymorphic_arguments[i].value) {
                result = symres_expression(&s_node->polymorphic_arguments[i].value);
                if (result > ss) ss = result;
            }
        }
    }

    if (s_node->constraints.constraints) {
        bh_arr_each(AstConstraint *, constraint, s_node->constraints.constraints) {
            SYMRES(constraint, *constraint);
        }
    }

    fori (i, 0, bh_arr_length(s_node->members)) {
        AstStructMember *member = s_node->members[i];
        track_declaration_for_symbol_info(member->token->pos, (AstNode *) member);

        if (member->type_node) {
            SymresStatus ss = symres_type(&member->type_node);
            if (ss != Symres_Success) {
                s_node->flags &= ~Ast_Flag_Type_Is_Resolved;
                scope_leave();
                return ss;
            }
        }
    }

    scope_leave();
    return Symres_Success;
}

static SymresStatus symres_union_type(AstUnionType* u_node) {
    if (u_node->flags & Ast_Flag_Type_Is_Resolved) return Symres_Success;
    u_node->flags |= Ast_Flag_Comptime;

    SYMRES(type, &u_node->tag_backing_type);

    if (u_node->meta_tags) {
        bh_arr_each(AstTyped *, meta, u_node->meta_tags) {
            SYMRES(expression, meta);
        }
    }

    u_node->flags |= Ast_Flag_Type_Is_Resolved;

    assert(u_node->scope);
    scope_enter(u_node->scope);
    
    if (u_node->polymorphic_argument_types) {
        assert(u_node->polymorphic_arguments);

        SymresStatus ss = Symres_Success, result;
        fori (i, 0, (i64) bh_arr_length(u_node->polymorphic_argument_types)) {
            result = symres_type(&u_node->polymorphic_argument_types[i]);
            if (result > ss) ss = result;

            if (u_node->polymorphic_arguments[i].value) {
                result = symres_expression(&u_node->polymorphic_arguments[i].value);
                if (result > ss) ss = result;
            }
        }
    }

    if (u_node->constraints.constraints) {
        bh_arr_each(AstConstraint *, constraint, u_node->constraints.constraints) {
            SYMRES(constraint, *constraint);
        }
    }

    fori (i, 0, bh_arr_length(u_node->variants)) {
        AstUnionVariant *variant = u_node->variants[i];
        track_declaration_for_symbol_info(variant->token->pos, (AstNode *) variant);

        assert(variant->type_node);
        SymresStatus ss = symres_type(&variant->type_node);
        if (ss != Symres_Success) {
            u_node->flags &= ~Ast_Flag_Type_Is_Resolved;
            scope_leave();
            return ss;
        }
    }

    scope_leave();
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
            break;
        }

        case Ast_Kind_Pointer_Type: SYMRES(type, &((AstPointerType *) *type)->elem); break;
        case Ast_Kind_Slice_Type:   SYMRES(type, &((AstSliceType *) *type)->elem); break;
        case Ast_Kind_DynArr_Type:  SYMRES(type, &((AstDynArrType *) *type)->elem); break;
        case Ast_Kind_VarArg_Type:  SYMRES(type, &((AstVarArgType *) *type)->elem); break;
        case Ast_Kind_Multi_Pointer_Type: SYMRES(type, &((AstMultiPointerType *) *type)->elem); break;

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype = (AstFunctionType *) *type;

            if (ftype->param_count > 0) {
                fori (i, 0, (i64) ftype->param_count) {
                    SYMRES(type, &ftype->params[i]);
                }
            }

            SYMRES(type, &ftype->return_type);
            break;
        }

        case Ast_Kind_Struct_Type: SYMRES(struct_type, (AstStructType *) *type); break;
        case Ast_Kind_Union_Type:  SYMRES(union_type, (AstUnionType *) *type); break;
        case Ast_Kind_Array_Type: {
            AstArrayType* a_node = (AstArrayType *) *type;

            if (a_node->count_expr) SYMRES(expression, &a_node->count_expr);
            SYMRES(type, &a_node->elem);
            break;
        }

        case Ast_Kind_Enum_Type: break;

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* pst_node = (AstPolyStructType *) *type;
            assert(pst_node->scope);
            break;
        }

        case Ast_Kind_Poly_Union_Type: {
            AstPolyUnionType* put_node = (AstPolyUnionType *) *type;
            assert(put_node->scope);
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
            break;
        }

        case Ast_Kind_Alias: {
            AstAlias* alias = (AstAlias *) *type;
            SYMRES_INVISIBLE(type, alias, (AstType **) &alias->alias);

            break;
        }

        case Ast_Kind_Typeof: {
            AstTypeOf* type_of = (AstTypeOf *) *type;
            SYMRES(expression, &type_of->expr);
            break;
        }

        case Ast_Kind_Distinct_Type: {
            AstDistinctType *distinct = (AstDistinctType *) *type;
            SYMRES(type, &distinct->base_type);
            break;
        }
    }

    return Symres_Success;
}

static SymresStatus symres_local(AstLocal** local) {
    SYMRES(type, &(*local)->type_node);

    if ((*local)->token != NULL)
        symbol_introduce(current_scope, (*local)->token, (AstNode *) *local);

    return Symres_Success;
}

static SymresStatus symres_arguments(Arguments* args) {
    bh_arr_each(AstTyped *, arg, args->values)
        SYMRES(expression, arg);

    bh_arr_each(AstNamedValue *, named_arg, args->named_values)
        SYMRES(expression, &(*named_arg)->value);

    return Symres_Success;
}

static SymresStatus symres_call(AstCall** pcall) {
    AstCall *call = *pcall;
    SYMRES(expression, (AstTyped **) &call->callee);
    SYMRES(arguments, &call->args);

    AstNode* callee = strip_aliases((AstNode *) call->callee);
    if (callee->kind == Ast_Kind_Poly_Struct_Type ||
        callee->kind == Ast_Kind_Poly_Union_Type) {
        *pcall = (AstCall *) convert_call_to_polycall(call);
        SYMRES(type, (AstType **) pcall);
        return Symres_Success;
    }

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

    AstTyped* expr = (AstTyped *) strip_aliases((AstNode *) (*fa)->expr);

    b32 force_a_lookup = 0;

    if (expr->kind == Ast_Kind_Struct_Type ||
        expr->kind == Ast_Kind_Poly_Struct_Type ||
        expr->kind == Ast_Kind_Enum_Type ||
        expr->kind == Ast_Kind_Type_Raw_Alias ||
        expr->kind == Ast_Kind_Union_Type ||
        expr->kind == Ast_Kind_Poly_Union_Type ||
        expr->kind == Ast_Kind_Interface) {
        force_a_lookup = 1;
    }

    //
    // If we are trying to access a field on an alias, we have to make sure
    // the alias is "ready" to have a symbol looked up inside of it. This means
    // the alias should have passed symbol resolution. If not, force a lookup
    // and yield if the alias was not ready.
    if ((*fa)->expr->kind == Ast_Kind_Alias) {
        if ((*fa)->expr->entity && (*fa)->expr->entity->state < Entity_State_Check_Types) {
            force_a_lookup = 1;
        }
    }

    AstNode* resolution = try_symbol_resolve_from_node((AstNode *) expr, (*fa)->token);
    if (resolution) {
        track_resolution_for_symbol_info((AstNode *) *fa, resolution);
        *((AstNode **) fa) = resolution;

    } else if (expr->kind == Ast_Kind_Package) {
        if (report_unresolved_symbols) {
            token_toggle_end((*fa)->token);
            char *closest = find_closest_symbol_in_node((AstNode *) expr, (*fa)->token->text);
            token_toggle_end((*fa)->token);

            AstPackage *package = (AstPackage *) strip_aliases((AstNode *) (*fa)->expr);
            char *package_name = "unknown (compiler bug)";
            if (package && package->package) {
                package_name = package->package->name;
            }

            if (closest) {
                onyx_report_error((*fa)->token->pos, Error_Critical, "'%b' was not found in package '%s'. Did you mean '%s'?",
                    (*fa)->token->text,
                    (*fa)->token->length,
                    package_name,
                    closest);
            } else {
                onyx_report_error((*fa)->token->pos, Error_Critical, "'%b' was not found in package '%s'. Perhaps it is defined in a file that wasn't loaded?",
                    (*fa)->token->text,
                    (*fa)->token->length,
                    package_name);
            }
            return Symres_Error;

        } else {
            return Symres_Yield_Macro;
        }

    } else if (force_a_lookup) {
        if (context.cycle_detected || context.cycle_almost_detected >= 2) {
            onyx_report_error((*fa)->token->pos, Error_Critical, "'%b' does not exist here. This is a bad error message.",
                (*fa)->token->text,
                (*fa)->token->length);
            return Symres_Error;
        }

        return Symres_Yield_Macro;
    }

    return Symres_Success;
}

static SymresStatus symres_compound(AstCompound* compound) {
    bh_arr_each(AstTyped *, expr, compound->exprs) {
        SYMRES(expression, expr);
    }

    return Symres_Success;
}

static SymresStatus symres_if_expression(AstIfExpression* if_expr) {
    SYMRES(expression, &if_expr->cond);
    SYMRES(expression, &if_expr->true_expr);
    SYMRES(expression, &if_expr->false_expr);
    return Symres_Success;
}

static SymresStatus symres_pipe(AstBinaryOp** pipe) {
    AstCall* call_node = (AstCall *) (*pipe)->right;
    SYMRES(expression, (AstTyped **) &call_node);
    SYMRES(expression, &(*pipe)->left);

    if (call_node->kind != Ast_Kind_Call) {
        onyx_report_error((*pipe)->token->pos, Error_Critical, "Pipe operator expected call on right side.");
        return Symres_Error;
    }

    if ((*pipe)->left == NULL) return Symres_Error;

    // :EliminatingSymres
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
    // :EliminatingSymres
    
    // We have to check this no matter what, because if we return to symbol resolution
    // the left hand side could be something different. In particular this was a problem
    // when expanding `some_map["value"]->unwrap()`, as the left hand side expands to a
    // macro.
    SYMRES(expression, &(*mcall)->left);

    if (((*mcall)->flags & Ast_Flag_Has_Been_Symres) == 0) {
        if ((*mcall)->left == NULL) return Symres_Error;

        if ((*mcall)->right->kind != Ast_Kind_Call) {
            onyx_report_error((*mcall)->token->pos, Error_Critical, "'->' expected procedure call on right side.");
            return Symres_Error;
        }

        //
        // This is a small hack that makes chaining method calls
        // work. Because check_method_call replaces the method call
        // and marks it as completed, if there are multiple references
        // to the same method call node, one of them will be left dangling.
        // To remedy this, an alias node an be placed around the method call
        // so that when check_method_call replaces it, it is replaced
        // within the alias, and all references are updated.
        if ((*mcall)->left->kind == Ast_Kind_Method_Call) {
            AstAlias *left_alias = onyx_ast_node_new(context.ast_alloc, sizeof(AstAlias), Ast_Kind_Alias);
            left_alias->token = (*mcall)->left->token;
            left_alias->alias = (*mcall)->left;

            (*mcall)->left = (AstTyped *) left_alias;
        }

        AstFieldAccess* implicit_field_access = make_field_access(context.ast_alloc, (*mcall)->left, NULL);
        implicit_field_access->token = ((AstCall *) (*mcall)->right)->callee->token;
        ((AstCall *) (*mcall)->right)->callee = (AstTyped *) implicit_field_access;
        (*mcall)->flags |= Ast_Flag_Has_Been_Symres;
    }

    SYMRES(expression, (AstTyped **) &(*mcall)->right);

    // TODO: This doesn't look right? Does this ever happen? Check this...
    if ((*mcall)->right->kind != Ast_Kind_Call) {
        *mcall = (AstBinaryOp *) (*mcall)->right;
    }

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

    // :EliminatingSymres
    sl->type_node = (AstType *) sl->stnode;
    while (sl->type_node && sl->type_node->kind == Ast_Kind_Type_Alias)
        sl->type_node = ((AstTypeAlias *) sl->type_node)->to;

    SYMRES(arguments, &sl->args);

    return Symres_Success;
}

static SymresStatus symres_array_literal(AstArrayLiteral* al) {
    if (al->atnode != NULL) SYMRES(expression, &al->atnode);

    // :EliminatingSymres
    al->type_node = (AstType *) al->atnode;
    while (al->type_node && al->type_node->kind == Ast_Kind_Type_Alias)
        al->type_node = ((AstTypeAlias *) al->type_node)->to;

    bh_arr_each(AstTyped *, expr, al->values)
        SYMRES(expression, expr);

    return Symres_Success;
}

static SymresStatus symres_address_of(AstAddressOf** paof) {
    AstAddressOf *aof = (AstAddressOf *) *paof;
    SYMRES(expression, &aof->expr);

    AstTyped *expr = (AstTyped *) strip_aliases((AstNode *) aof->expr);
    if (node_is_type((AstNode *) expr)) {
        AstPointerType *pt = onyx_ast_node_new(context.ast_alloc, sizeof(AstPointerType), Ast_Kind_Pointer_Type);
        pt->token     = aof->token;
        pt->elem      = (AstType *) expr;
        pt->__unused  = aof->next;
        *paof         = (AstAddressOf *) pt;
        SYMRES(type, (AstType **) &pt);
        return Symres_Success;
    }

    return Symres_Success;
}

static SymresStatus symres_expression(AstTyped** expr) {
    if (node_is_type((AstNode *) *expr)) {
        SYMRES(type, (AstType **) expr);
        return Symres_Success;
    }

    switch ((*expr)->kind) {
        case Ast_Kind_Symbol:
            SYMRES(symbol, (AstNode **) expr);

            // HACK?
            // I don't know how I never ran into this problem before,
            // but when a symbol is resolved, there is never a "double
            // check" that its type node is symbol resolved as well.
            // This only proved to be an issue when using constraint
            // sentinels, so I only added that case here. This should
            // maybe be considered in the future because I think this
            // lack of double checking could be causing other bugs.
            if ((*expr)->kind == Ast_Kind_Constraint_Sentinel) {
                SYMRES(type, &(*expr)->type_node);
            }
            break;

        case Ast_Kind_Binary_Op:
            SYMRES(expression, &((AstBinaryOp *)(*expr))->left);
            SYMRES(expression, &((AstBinaryOp *)(*expr))->right);
            break;

        case Ast_Kind_Unary_Op:     SYMRES(unaryop, (AstUnaryOp **) expr); break;
        case Ast_Kind_Call:         SYMRES(call, (AstCall **) expr); break;
        case Ast_Kind_Argument:     SYMRES(expression, &((AstArgument *) *expr)->value); break;
        case Ast_Kind_Block:        SYMRES(block, (AstBlock *) *expr); break;
        case Ast_Kind_Dereference:  SYMRES(expression, &((AstDereference *)(*expr))->expr); break;
        case Ast_Kind_Field_Access: SYMRES(field_access, (AstFieldAccess **) expr); break;
        case Ast_Kind_Pipe:         SYMRES(pipe, (AstBinaryOp **) expr); break;
        case Ast_Kind_Method_Call:  SYMRES(method_call, (AstBinaryOp **) expr); break;
        case Ast_Kind_Size_Of:      SYMRES(size_of, (AstSizeOf *)*expr); break;
        case Ast_Kind_Align_Of:     SYMRES(align_of, (AstAlignOf *)*expr); break;
        case Ast_Kind_Address_Of:   SYMRES(address_of, (AstAddressOf **) expr); break;
        case Ast_Kind_Alias: {
            AstAlias *alias = (AstAlias *) *expr;
            SYMRES_INVISIBLE(expression, alias, &alias->alias);
            break;
        }

        case Ast_Kind_Range_Literal:
            SYMRES(expression, &((AstRangeLiteral *)(*expr))->low);
            SYMRES(expression, &((AstRangeLiteral *)(*expr))->high);

            // :EliminatingSymres
            SYMRES(type, &builtin_range_type);
            (*expr)->type_node = builtin_range_type;
            break;

        case Ast_Kind_Polymorphic_Proc:
            if (((AstFunction *) *expr)->captures) {
                ((AstFunction *) *expr)->scope_to_lookup_captured_values = current_scope;
            }
            break;

        case Ast_Kind_Function:
            if (((AstFunction *) *expr)->captures) {
                ((AstFunction *) *expr)->scope_to_lookup_captured_values = current_scope;
            }

            SYMRES(type, &(*expr)->type_node);
            break;

        case Ast_Kind_NumLit:
            SYMRES(type, &(*expr)->type_node);
            break;

        case Ast_Kind_StrLit: {
            AstStrLit* str = (AstStrLit *) *expr;
            if (str->is_cstr) {
                SYMRES(type, &builtin_cstring_type);
                str->type_node = builtin_cstring_type;

            } else {
                SYMRES(type, &builtin_string_type);
                str->type_node = builtin_string_type;
            }
            break;
        }

        case Ast_Kind_Slice:
        case Ast_Kind_Subscript:
            SYMRES(expression, &((AstSubscript *)(*expr))->addr);
            SYMRES(expression, &((AstSubscript *)(*expr))->expr);
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

        case Ast_Kind_Directive_Defined:
            SYMRES(directive_defined, (AstDirectiveDefined **) expr);
            break;

        case Ast_Kind_Compound:
            SYMRES(compound, (AstCompound *) *expr);
            break;

        case Ast_Kind_Package:
            SYMRES(package, (AstPackage *) *expr);
            break;

        case Ast_Kind_If_Expression:
            SYMRES(if_expression, (AstIfExpression *) *expr);
            break;

        case Ast_Kind_Directive_Insert:
            SYMRES(directive_insert, (AstDirectiveInsert *) *expr);
            break;

        case Ast_Kind_Do_Block: {
            Scope* old_current_scope = current_scope;
            SYMRES(type, &(*expr)->type_node);
            SYMRES(block, ((AstDoBlock *) *expr)->block);
            current_scope = old_current_scope;
            break;
        }

        case Ast_Kind_Param:
            if ((*expr)->flags & Ast_Flag_Param_Symbol_Dirty) {
                assert((*expr)->token->type == Token_Type_Symbol);
                *expr = (AstTyped *) make_symbol(context.ast_alloc, (*expr)->token);
                SYMRES(expression, expr);
            }
            break;

        case Ast_Kind_Constraint_Sentinel: {
            AstTyped *sentinel = (AstTyped *) *expr;
            SYMRES(type, &sentinel->type_node);
            break;
        }

        case Ast_Kind_Directive_Export_Name: {
            AstDirectiveExportName *ename = (AstDirectiveExportName *) *expr;
            SYMRES(expression, (AstTyped **) &ename->func);
            break;
        }

        case Ast_Kind_Switch: {
            SYMRES(switch, (AstSwitch *) *expr);
            break;
        }

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
    if (ifnode->kind == Ast_Kind_Static_If) {
        if ((ifnode->flags & Ast_Flag_Static_If_Resolved) == 0) {
            if (context.cycle_detected) {
                onyx_report_error(ifnode->token->pos, Error_Waiting_On, "Waiting on static if resolution.");
                return Symres_Error;
            } else {
                return Symres_Yield_Macro;
            }
        }

        if (static_if_resolution(ifnode)) {
            if (ifnode->true_stmt != NULL)  SYMRES(statement, (AstNode **) &ifnode->true_stmt, NULL);

        } else {
            if (ifnode->false_stmt != NULL) SYMRES(statement, (AstNode **) &ifnode->false_stmt, NULL);
        }

    } else {
        if (ifnode->initialization != NULL) {
            ifnode->scope = scope_create(context.ast_alloc, current_scope, ifnode->token->pos);
            scope_enter(ifnode->scope);

            SYMRES(statement_chain, &ifnode->initialization);
        }

        SYMRES(expression, &ifnode->cond);

        // NOTE: These are statements because "elseif" means the `false_stmt` has an if node.
        if (ifnode->true_stmt != NULL)  SYMRES(statement, (AstNode **) &ifnode->true_stmt, NULL);
        if (ifnode->false_stmt != NULL) SYMRES(statement, (AstNode **) &ifnode->false_stmt, NULL);

        if (ifnode->initialization != NULL) scope_leave();
    }

    return Symres_Success;
}

static SymresStatus symres_while(AstIfWhile* whilenode) {
    if (whilenode->initialization != NULL) {
        whilenode->scope = scope_create(context.ast_alloc, current_scope, whilenode->token->pos);
        scope_enter(whilenode->scope);

        SYMRES(statement_chain, &whilenode->initialization);
    }

    SYMRES(expression, &whilenode->cond);

    if (whilenode->true_stmt)  SYMRES(block, whilenode->true_stmt);
    if (whilenode->false_stmt) SYMRES(block, whilenode->false_stmt);

    if (whilenode->initialization != NULL) scope_leave();

    return Symres_Success;
}

static SymresStatus symres_for(AstFor* fornode) {
    fornode->scope = scope_create(context.ast_alloc, current_scope, fornode->token->pos);
    scope_enter(fornode->scope);
    SYMRES(expression, &fornode->iter);
    SYMRES(local, &fornode->var);
    SYMRES(block, fornode->stmt);
    scope_leave();

    return Symres_Success;
}

static SymresStatus symres_case(AstSwitchCase *casenode) {
    if (!casenode->is_default) {
        bh_arr_each(AstTyped *, expr, casenode->values) {
            SYMRES(expression, expr);
        }
    }

    if (casenode->capture) {
        if (casenode->scope == NULL) {
            casenode->scope = scope_create(context.ast_alloc, current_scope, casenode->token->pos);
            symbol_introduce(casenode->scope, casenode->capture->token, (AstNode *) casenode->capture);
        }

        scope_enter(casenode->scope);
    }

    if (casenode->body_is_expr) {
        SYMRES(expression, &casenode->expr);
    } else {
        SYMRES(block, casenode->block);
    }

    if (casenode->capture) {
        scope_leave();
    }

    return Symres_Success;
}

static SymresStatus symres_switch(AstSwitch* switchnode) {
    if (switchnode->initialization != NULL) {
        switchnode->scope = scope_create(context.ast_alloc, current_scope, switchnode->token->pos);
        scope_enter(switchnode->scope);

        SYMRES(statement_chain, &switchnode->initialization);
    }

    SYMRES(expression, &switchnode->expr);

    if (switchnode->cases == NULL) {
        SYMRES(block, switchnode->case_block);
    } else {
        bh_arr_each(AstSwitchCase *, pcase, switchnode->cases) {
            SYMRES(case, *pcase);
        }

        if (switchnode->default_case) {
            if (switchnode->is_expr) {
                SYMRES(expression, (AstTyped **) &switchnode->default_case);
            } else {
                SYMRES(block, switchnode->default_case);
            }
        }
    }

    if (switchnode->switch_kind == Switch_Kind_Use_Equals && switchnode->case_exprs) {
        bh_arr_each(CaseToBlock, ctb, switchnode->case_exprs) {
            SYMRES(expression, (AstTyped **) &ctb->comparison);
        }
    }

    if (switchnode->initialization != NULL) scope_leave();

    return Symres_Success;
}

static SymresStatus symres_directive_solidify(AstDirectiveSolidify** psolid) {
    AstDirectiveSolidify* solid = *psolid;

    SYMRES(expression, (AstTyped **) &solid->poly_proc);
    if (solid->poly_proc && solid->poly_proc->kind == Ast_Kind_Directive_Solidify) {
        AstFunction* potentially_resolved_proc = (AstFunction *) ((AstDirectiveSolidify *) solid->poly_proc)->resolved_proc;
        if (!potentially_resolved_proc) return Symres_Yield_Micro;

        solid->poly_proc = potentially_resolved_proc;
    }

    if (!solid->poly_proc || solid->poly_proc->kind != Ast_Kind_Polymorphic_Proc) {
        onyx_report_error(solid->token->pos, Error_Critical, "Expected polymorphic procedure in #solidify directive.");
        return Symres_Error;
    }
    
    bh_arr_each(AstPolySolution, sln, solid->known_polyvars) {
        // HACK: This assumes that 'ast_type' and 'value' are at the same offset.
        SYMRES(expression, &sln->value);
    }

    return Symres_Success;
}

static SymresStatus symres_directive_defined(AstDirectiveDefined** pdefined) {
    AstDirectiveDefined* defined = *pdefined;

    b32 has_to_be_resolved = context.cycle_almost_detected >= 1;

    onyx_errors_disable();
    resolved_a_symbol = 0;
    SymresStatus ss = symres_expression(&defined->expr);
    if (has_to_be_resolved && ss != Symres_Success && !resolved_a_symbol) {
        // The symbol definitely was not found and there is no chance that it could be found.
        defined->is_defined = 0;

        onyx_errors_enable();
        return Symres_Success;
    }

    if (ss == Symres_Success) {
        defined->is_defined = 1;

        onyx_errors_enable();
        return Symres_Success;
    }

    onyx_errors_enable();
    return Symres_Yield_Macro;
}

static SymresStatus symres_directive_insert(AstDirectiveInsert* insert) {
    SYMRES(expression, &insert->code_expr);
    bh_arr_each(AstTyped *, pexpr, insert->binding_exprs) {
        SYMRES(expression, pexpr);
    }
    return Symres_Success;
}

static SymresStatus symres_capture_block(AstCaptureBlock *block, Scope *captured_scope) {
    bh_arr_each(AstCaptureLocal *, capture, block->captures) {
        OnyxToken *token = (*capture)->token;
        AstTyped *resolved = (AstTyped *) symbol_resolve(captured_scope, token);

        if (!resolved) {
            // Should this do a yield? In there any case that that would make sense?
            onyx_report_error(token->pos, Error_Critical, "'%b' is not found in the enclosing scope.",
                    token->text, token->length);
            return Symres_Error;
        }

        (*capture)->captured_value = resolved;
    }

    bh_arr_each(AstCaptureLocal *, capture, block->captures) {
        symbol_introduce(current_scope, (*capture)->token, (AstNode *) *capture);
    }

    return Symres_Success;
}

static SymresStatus symres_statement(AstNode** stmt, b32 *remove) {
    if (remove) *remove = 0;

    switch ((*stmt)->kind) {
        case Ast_Kind_Return:      SYMRES(return, (AstReturn *) *stmt);                  break;
        case Ast_Kind_If:          SYMRES(if, (AstIfWhile *) *stmt);                     break;
        case Ast_Kind_Static_If:   SYMRES(if, (AstIfWhile *) *stmt);                     break;
        case Ast_Kind_While:       SYMRES(while, (AstIfWhile *) *stmt);                  break;
        case Ast_Kind_For:         SYMRES(for, (AstFor *) *stmt);                        break;
        case Ast_Kind_Switch:      SYMRES(switch, (AstSwitch *) *stmt);                  break;
        case Ast_Kind_Call:        SYMRES(call, (AstCall **) stmt);                      break;
        case Ast_Kind_Argument:    SYMRES(expression, (AstTyped **) &((AstArgument *) *stmt)->value); break;
        case Ast_Kind_Block:       SYMRES(block, (AstBlock *) *stmt);                    break;
        case Ast_Kind_Defer:       SYMRES(statement, &((AstDefer *) *stmt)->stmt, NULL); break;
        case Ast_Kind_Switch_Case: SYMRES(case, (AstSwitchCase *) *stmt);                break;
        case Ast_Kind_Jump:        break;
        case Ast_Kind_Directive_Remove: break;

        case Ast_Kind_Local:
            // if (remove) *remove = 1;
            SYMRES(local, (AstLocal **) stmt);
            break;

        case Ast_Kind_Import:
            if (remove) *remove = 1;
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
    if (block->rules & Block_Rule_New_Scope) {
        if (block->scope == NULL)
            block->scope = scope_create(context.ast_alloc, current_scope, block->token->pos);

        scope_enter(block->scope);
    }

    if (block->binding_scope != NULL)
        scope_include(current_scope, block->binding_scope, block->token->pos);

    if (block->quoted_block_capture_scope != NULL)
        scope_include(current_scope, block->quoted_block_capture_scope, block->token->pos);

    if (block->body) {
        AstNode** start = &block->body;
        fori (i, 0, block->statement_idx) {
            start = &(*start)->next;
        }

        b32 remove = 0;

        while (*start) {
            SymresStatus cs = symres_statement(start, &remove);

            if (remove) {
                remove = 0;
                AstNode* tmp = (*start)->next;
                (*start)->next = NULL;
                (*start) = tmp;

            } else {
                switch (cs) {
                    case Symres_Success:
                        start = &(*start)->next;
                        block->statement_idx++;
                        break;

                    default:
                        return cs;
                }
            }
        }

        block->statement_idx = 0;
    }

    if (block->rules & Block_Rule_New_Scope)
        scope_leave();

    return Symres_Success;
}

SymresStatus symres_function_header(AstFunction* func) {
    func->flags |= Ast_Flag_Comptime;

    if (!(func->flags & Ast_Flag_Function_Is_Lambda) && func->captures) {
        onyx_report_error(func->captures->token->pos, Error_Critical, "This procedure cannot capture values as it is not defined in an expression.");
        return Symres_Error;
    }

    if (func->captures && !func->scope_to_lookup_captured_values) {
        if (func->flags & Ast_Flag_Function_Is_Lambda_Inside_PolyProc) return Symres_Complete;

        return Symres_Yield_Macro;
    }

    if (func->scope == NULL)
        func->scope = scope_create(context.ast_alloc, current_scope, func->token->pos);

    if (func->constraints.constraints != NULL && func->constraints.constraints_met == 0) {
        bh_arr_each(AstConstraint *, constraint, func->constraints.constraints) {
            SYMRES(constraint, *constraint);
        }

        // Return early here to finish checking constraints in the checker.
        // Will resume here after constraints have been met.
        return Symres_Success;
    }

    scope_enter(func->scope);

    bh_arr_each(AstParam, param, func->params) {
        if (param->default_value != NULL) {
            SYMRES(expression, &param->default_value);
            if (onyx_has_errors()) return Symres_Error;
        }
    }

    bh_arr_each(AstParam, param, func->params) {
        symbol_introduce(current_scope, param->local->token, (AstNode *) param->local);
    }

    bh_arr_each(AstParam, param, func->params) {
        if (param->local->type_node != NULL) {
            SYMRES_INVISIBLE(type, param->local, &param->local->type_node);
        }
    }

    if (potentially_convert_function_to_polyproc(func)) {
        return Symres_Complete;
    }

    if (func->nodes_that_need_entities_after_clone && bh_arr_length(func->nodes_that_need_entities_after_clone) > 0 && func->entity) {
        bh_arr_each(AstNode *, node, func->nodes_that_need_entities_after_clone) {
            // This makes a lot of assumptions about how these nodes are being processed,
            // and I don't want to start using this with other nodes without considering
            // what the ramifications of that is.
            assert((*node)->kind == Ast_Kind_Static_If || (*node)->kind == Ast_Kind_File_Contents
                    || (*node)->kind == Ast_Kind_Function || (*node)->kind == Ast_Kind_Polymorphic_Proc);

            // Need to use current_scope->parent because current_scope is the function body scope.
            Scope *scope = current_scope->parent;

            if ((*node)->kind == Ast_Kind_Static_If) {
                AstIf *static_if = (AstIf *) *node;
                assert(static_if->defined_in_scope);
                scope = static_if->defined_in_scope;

                if (func->poly_scope) {
                    scope = scope_create(context.ast_alloc, scope, static_if->token->pos);
                    scope_include(scope, func->poly_scope, static_if->token->pos);
                }
            }

            add_entities_for_node(NULL, *node, scope, func->entity->package);
        }

        bh_arr_set_length(func->nodes_that_need_entities_after_clone, 0);
    }

    if (func->deprecated_warning) {
        SYMRES(expression, (AstTyped **) &func->deprecated_warning);
    }
    
    bh_arr_each(AstTyped *, pexpr, func->tags) {
        SYMRES(expression, pexpr);
    }

    if (func->foreign.import_name) {
        SYMRES(expression, &func->foreign.module_name);
        SYMRES(expression, &func->foreign.import_name);
    }

    if (func->captures) {
        SYMRES(capture_block, func->captures, func->scope_to_lookup_captured_values);
    }

    SYMRES(type, &func->return_type);

    if (context.options->stack_trace_enabled) {
        OnyxToken *stack_trace_token = bh_alloc_item(context.ast_alloc, OnyxToken);
        stack_trace_token->type = Token_Type_Symbol;
        stack_trace_token->length = 13;
        stack_trace_token->text = bh_strdup(context.ast_alloc, "__stack_trace ");
        stack_trace_token->pos = func->token->pos;

        if (!func->stack_trace_local) {
            assert(builtin_stack_trace_type);
            func->stack_trace_local = make_local(context.ast_alloc, stack_trace_token, builtin_stack_trace_type);
            func->stack_trace_local->flags |= Ast_Flag_Decl_Followed_By_Init;
        }

        SYMRES(local, &func->stack_trace_local);
    }

    scope_leave();

    return Symres_Success;
}

SymresStatus symres_function(AstFunction* func) {
    if (func->entity_header && func->entity_header->state < Entity_State_Check_Types) return Symres_Yield_Macro;
    if (func->kind == Ast_Kind_Polymorphic_Proc) return Symres_Complete;
    if (func->flags & Ast_Flag_Function_Is_Lambda_Inside_PolyProc) return Symres_Complete;
    assert(func->scope);

    scope_enter(func->scope);

    if ((func->flags & Ast_Flag_Has_Been_Symres) == 0) {
        // :EliminatingSymres
        bh_arr_each(AstParam, param, func->params) {
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
            if (param->is_used && !param->use_processed) {
                if (param->local->type_node != NULL && param->local->type == NULL) {
                    param->local->type = type_build_from_ast(context.ast_alloc, param->local->type_node);

                    if (param->local->type == NULL) return Symres_Yield_Macro;
                }

                if (type_is_struct(param->local->type)) {
                    Type* st;
                    if (param->local->type->kind == Type_Kind_Struct) {
                        st = param->local->type;
                    } else {
                        st = param->local->type->Pointer.elem;
                    }

                    if (st->Struct.status != SPS_Uses_Done) return Symres_Yield_Macro;

                    fori (i, 0, shlen(st->Struct.members)) {
                        StructMember* value = st->Struct.members[i].value;
                        AstFieldAccess* fa = make_field_access(context.ast_alloc, (AstTyped *) param->local, value->name);
                        symbol_raw_introduce(current_scope, value->name, param->local->token->pos, (AstNode *) fa);
                    }

                    param->use_processed = 1;

                } else if (param->local->type != NULL) {
                    onyx_report_error(param->local->token->pos, Error_Critical, "Can only 'use' structures or pointers to structures.");

                } else {
                    // :ExplicitTyping
                    onyx_report_error(param->local->token->pos, Error_Critical, "Cannot deduce type of parameter '%b'; Try adding it explicitly.",
                        param->local->token->text,
                        param->local->token->length);
                }
            }
        }

        func->flags |= Ast_Flag_Has_Been_Symres;
    }

    SYMRES(block, func->body);

    scope_leave();
    return Symres_Success;
}

static SymresStatus symres_global(AstGlobal* global) {
    SYMRES(type, &global->type_node);
    return Symres_Success;
}

static SymresStatus symres_overloaded_function(AstOverloadedFunction* ofunc) {
    bh_arr_each(OverloadOption, overload, ofunc->overloads) {
        SYMRES(expression, &overload->option);
    }

    if (ofunc->expected_return_node) {
        SYMRES(type, &ofunc->expected_return_node);
    }

    return Symres_Success;
}

static SymresStatus symres_package(AstPackage* package) {
    if (package->package == NULL) {
        if (!package->package_name) return Symres_Error;

        package->package = package_lookup(package->package_name);
    }

    if (package->package) {
        package_mark_as_used(package->package);
        return Symres_Success;
    } else {
        if (report_unresolved_symbols) {
            onyx_report_error(package->token->pos, Error_Critical,
                    "Package '%s' not found in included source files.",
                    package->package_name);
            return Symres_Error;
        } else {
            return Symres_Yield_Macro;
        }
    }
}

static SymresStatus symres_enum(AstEnumType* enum_node) {
    if (!enum_node->backing_type) {
        if (enum_node->backing == NULL) return Symres_Error;
        if (enum_node->backing->kind == Ast_Kind_Symbol) SYMRES(symbol, (AstNode **) &enum_node->backing);

        enum_node->backing_type = type_build_from_ast(context.ast_alloc, enum_node->backing);
    }

    if (enum_node->scope == NULL) {
        enum_node->scope = scope_create(context.ast_alloc, current_scope, enum_node->token->pos);

        type_build_from_ast(context.ast_alloc, (AstType *) enum_node);
    }

    scope_enter(enum_node->scope);

    // :EliminatingSymres
    u64 next_assign_value = enum_node->is_flags ? 1 : 0;
    bh_arr_each(AstEnumValue *, value, enum_node->values) {
        if ((*value)->flags & Ast_Flag_Has_Been_Checked) continue;

        (*value)->type = enum_node->etcache;
        (*value)->flags |= Ast_Flag_Comptime;

        if ((*value)->value != NULL) {
            SYMRES(expression, &(*value)->value);

            if ((*value)->value->kind == Ast_Kind_Enum_Value) {
                (*value)->value = ((AstEnumValue *) (*value)->value)->value;
                (*value)->value->type = enum_node->etcache;
            }

            if ((*value)->value->kind == Ast_Kind_NumLit) {
                AstNumLit *n_value = (AstNumLit *) (*value)->value;
                resolve_expression_type((AstTyped *) n_value);

                if (type_is_small_integer(n_value->type)) {
                    next_assign_value = n_value->value.i;
                } else if (type_is_integer(n_value->type)) {
                    next_assign_value = n_value->value.l;
                } else {
                    onyx_report_error((*value)->token->pos, Error_Critical, "expected numeric integer literal for enum initialization, got '%s'", type_get_name(n_value->type));
                    return Symres_Error;
                }

                n_value->type = enum_node->etcache;

            } else {
                if ((*value)->entity == NULL) {
                    add_entities_for_node(NULL, (AstNode *) (*value), enum_node->scope, NULL);
                }

                if (context.cycle_detected) {
                    onyx_report_error((*value)->token->pos, Error_Critical, "Expected compile time known value for enum initialization.");
                    return Symres_Error;
                }

                return Symres_Yield_Macro;
            }

        } else {
            AstNumLit* num = make_int_literal(context.ast_alloc, next_assign_value);
            num->type = enum_node->etcache;

            (*value)->value = (AstTyped *) num;
        }

        symbol_introduce(enum_node->scope, (*value)->token, (AstNode *) (*value));

        (*value)->flags |= Ast_Flag_Comptime | Ast_Flag_Has_Been_Checked;

        if (enum_node->is_flags) {
            next_assign_value <<= 1;
        } else {
            next_assign_value++;
        }
    }

    scope_leave();

    // HACK this ensure that you can only lookup symbols in an Enum that are actually defined in the enum.
    // However, during the symbol resolution of the values in an enum, they need to be able to see the
    // enclosing scope.
    enum_node->scope->parent = NULL;

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

    bh_arr_each(AstTyped *, ptag, (*memres)->tags) {
        SYMRES(expression, ptag);
    }

    return Symres_Success;
}

static SymresStatus symres_struct_defaults(AstType* t) {
    if (t->kind != Ast_Kind_Struct_Type) return Symres_Error;

    AstStructType* st = (AstStructType *) t;
    if (st->scope) scope_enter(st->scope);

    if (st->meta_tags) {
        bh_arr_each(AstTyped *, meta, st->meta_tags) {
            SYMRES(expression, meta);
        }
    }

    bh_arr_each(AstStructMember *, smem, st->members) {
        if ((*smem)->initial_value != NULL) {
            SYMRES(expression, &(*smem)->initial_value);
        }

        if ((*smem)->meta_tags != NULL) {
            bh_arr_each(AstTyped *, meta, (*smem)->meta_tags) {
                SYMRES(expression, meta);
            }
        }
    }

    if (st->scope) scope_leave();
    return Symres_Success;
}

static SymresStatus symres_polyproc(AstFunction* pp) {
    pp->flags |= Ast_Flag_Comptime;
    pp->parent_scope_of_poly_proc = current_scope;

    bh_arr_each(AstPolyParam, p, pp->poly_params) {
        if (p->kind != PSK_Value) continue;

        AstParam *param = &pp->params[p->idx];
        if (param->default_value != NULL) {
            SYMRES(expression, &param->default_value);
            if (onyx_has_errors()) return Symres_Error;
        }
    }

    return Symres_Success;
}

static SymresStatus symres_static_if(AstIf* static_if) {
    if (static_if->flags & Ast_Flag_Dead) return Symres_Complete;

    SYMRES(expression, &static_if->cond);
    return Symres_Success;
}

static SymresStatus symres_process_directive(AstNode* directive) {
    // :EliminatingSymres
    switch (directive->kind) {
        case Ast_Kind_Directive_Add_Overload: {
            AstDirectiveAddOverload *add_overload = (AstDirectiveAddOverload *) directive;

            SYMRES(expression, (AstTyped **) &add_overload->overloaded_function);
            if (add_overload->overloaded_function == NULL) return Symres_Error; // NOTE: Error message will already be generated

            AstOverloadedFunction *ofunc = (AstOverloadedFunction *) strip_aliases((AstNode *) add_overload->overloaded_function);
            if (ofunc->kind == Ast_Kind_Symbol) {
                if (context.cycle_detected) {
                    onyx_report_error(add_overload->token->pos, Error_Waiting_On, "Waiting for matched procedure to be known.");
                    return Symres_Error;
                }

                return Symres_Yield_Macro;
            }

            if (ofunc->kind != Ast_Kind_Overloaded_Function) {
                onyx_report_error(add_overload->token->pos, Error_Critical, "#match directive expects a matched procedure, got %s.",
                            onyx_ast_node_kind_string(ofunc->kind));
                return Symres_Error;
            }

            if (ofunc->locked) {
                onyx_report_error(add_overload->token->pos, Error_Critical, "Cannot add match option here as the original #match was declared as #locked.");
                onyx_report_error(ofunc->token->pos, Error_Critical, "Here is the original #match.");
                return Symres_Error;
            }

            if (ofunc->only_local_functions) {
                if (!token_same_file(add_overload->token, ofunc->token)) {
                    onyx_report_error(add_overload->token->pos, Error_Critical, "Cannot add match option here as this option is not within the same file as the original #match declared with #local.");
                    onyx_report_error(ofunc->token->pos, Error_Critical, "Here is the original #match.");
                    return Symres_Error;
                }
            }

            SYMRES(expression, (AstTyped **) &add_overload->overload);
            add_overload->overload->flags &= ~Ast_Flag_Function_Is_Lambda;

            add_overload_option(&ofunc->overloads, add_overload->order, add_overload->overload);
            break;
        }

        case Ast_Kind_Directive_Operator: {
            AstDirectiveOperator *operator = (AstDirectiveOperator *) directive;
            SYMRES(expression, &operator->overload);
            if (!operator->overload) return Symres_Error;

            AstFunction* overload = get_function_from_node((AstNode *) operator->overload);
            if (overload == NULL) {
                onyx_report_error(operator->token->pos, Error_Critical, "This cannot be used as an operator overload.");
                return Symres_Error;
            }

            overload->flags &= ~Ast_Flag_Function_Is_Lambda;
            
            // First try unary operator overloading
            // CLEANUP This is not written well at all...
            if (operator->operator == Binary_Op_Count) {
                if (bh_arr_length(overload->params) != 1) {
                    onyx_report_error(operator->token->pos, Error_Critical, "Expected exactly 1 argument for unary operator overload.");
                    return Symres_Error;
                }

                UnaryOp unop = Unary_Op_Count;
                switch (operator->operator_token->type) {
                    case '?': unop = Unary_Op_Try; break;
                }

                if (unop == Unary_Op_Count) {
                    onyx_report_error(operator->token->pos, Error_Critical, "Unknown operator.");
                    return Symres_Error;
                }

                add_overload_option(&unary_operator_overloads[unop], operator->order, operator->overload);
                return Symres_Success;
            }

            if (operator->operator != Binary_Op_Subscript_Equals && bh_arr_length(overload->params) != 2) {
                onyx_report_error(operator->token->pos, Error_Critical, "Expected exactly 2 arguments for binary operator overload.");
                return Symres_Error;
            }

            add_overload_option(&operator_overloads[operator->operator], operator->order, operator->overload);
            break;
        }

        case Ast_Kind_Directive_Export: {
            AstDirectiveExport *export = (AstDirectiveExport *) directive;
            SYMRES(expression, &export->export);
            SYMRES(expression, &export->export_name_expr);

            if (export->export->kind == Ast_Kind_Polymorphic_Proc) {
                onyx_report_error(export->token->pos, Error_Critical, "Cannot export a polymorphic function.");
                return Symres_Error;
            }

            if (export->export->kind == Ast_Kind_Function) {
                AstFunction *func = (AstFunction *) export->export;
                func->is_exported = 1;

                if (func->is_foreign) {
                    onyx_report_error(export->token->pos, Error_Critical, "Cannot export a foreign function.");
                    return Symres_Error;
                }

                if (func->is_intrinsic) {
                    onyx_report_error(export->token->pos, Error_Critical, "Cannot export an intrinsic function.");
                    return Symres_Error;
                }
            }

            break;
        }

        case Ast_Kind_Directive_Init: {
            AstDirectiveInit *init = (AstDirectiveInit *) directive;
            SYMRES(expression, &init->init_proc);

            if (init->dependencies) {
                bh_arr_each(AstDirectiveInit *, dependency, init->dependencies) {
                    SYMRES(expression, (AstTyped **) dependency);
                }
            }

            break;
        }

        case Ast_Kind_Directive_Library: {
            AstDirectiveLibrary *library = (AstDirectiveLibrary *) directive;
            SYMRES(expression, &library->library_symbol);
            break;
        }

        case Ast_Kind_Injection: {
            AstInjection *inject = (AstInjection *) directive;

            if (inject->dest == NULL) {
                if (inject->full_loc == NULL) return Symres_Error;

                AstTyped *full_loc = (AstTyped *) strip_aliases((AstNode *) inject->full_loc);

                if (full_loc->kind != Ast_Kind_Field_Access) {
                    onyx_report_error(inject->token->pos, Error_Critical, "#inject expects a dot (a.b) expression for the injection point.");
                    return Symres_Error;
                }

                AstFieldAccess *acc = (AstFieldAccess *) full_loc;
                inject->dest = acc->expr;
                inject->symbol = acc->token;
            }

            SYMRES(expression, &inject->dest);
            SYMRES(expression, &inject->to_inject);
            break;
        }

        case Ast_Kind_Directive_This_Package: {
            AstPackage *package = (AstPackage *) directive;
            package->kind = Ast_Kind_Package;
            package->package = current_entity->package;
            return Symres_Complete;
        }
    }

    return Symres_Success;
}

static SymresStatus symres_macro(AstMacro* macro) {
    macro->flags |= Ast_Flag_Comptime;

    if (macro->body->kind == Ast_Kind_Function) {
        SYMRES(function_header, (AstFunction *) macro->body);
    }
    else if (macro->body->kind == Ast_Kind_Polymorphic_Proc) {
        SYMRES(polyproc, (AstFunction *) macro->body);
    }

    return Symres_Success;
}

static SymresStatus symres_constraint(AstConstraint* constraint) {
    switch (constraint->phase) {
        case Constraint_Phase_Cloning_Expressions:
        case Constraint_Phase_Waiting_To_Be_Queued: {
            SYMRES(expression, (AstTyped **) &constraint->interface);

            bh_arr_each(AstType *, type_arg, constraint->type_args) {
                SYMRES(type, type_arg);
            }

            return Symres_Success;
        }

        case Constraint_Phase_Checking_Expressions: {
            SymresStatus ss;
            onyx_errors_disable();

            fori (i, constraint->expr_idx, bh_arr_length(constraint->exprs)) {
                InterfaceConstraint* ic = &constraint->exprs[i];

                // Most of this logic was directly copied from the
                // check_constraint code. There might be a better
                // way to factor this?
                ss = symres_expression(&ic->expr);
                if (ss == Symres_Yield_Macro) {
                    onyx_errors_enable();
                    return ss;
                }

                if (ss == Symres_Error && !ic->invert_condition) {
                    goto constraint_error;
                }

                if (ss == Symres_Success && ic->invert_condition) {
                    goto constraint_error;
                }

                if (ic->expected_type_expr) {
                    ss = symres_type(&ic->expected_type_expr);
                    if (ss == Symres_Yield_Macro) {
                        onyx_errors_enable();
                        return ss;
                    }
                }

                continue;

              constraint_error:
                onyx_errors_enable();
                *constraint->report_status = Constraint_Check_Status_Failed;
                return Symres_Error;
            }

            onyx_errors_enable();
            return Symres_Success;
        }
    }

    return Symres_Success;
}

static SymresStatus symres_polyquery(AstPolyQuery *query) {
    // :EliminatingSymres
    query->successful_symres = 0;

    if (query->function_header->scope == NULL)
        query->function_header->scope = scope_create(context.ast_alloc, query->proc->parent_scope_of_poly_proc, query->token->pos);

    scope_enter(query->function_header->scope);

    u32 idx = 0;
    bh_arr_each(AstParam, param, query->function_header->params) {
        bh_arr_each(AstPolyParam, pp, query->proc->poly_params) {
            if (pp->kind == PPK_Baked_Value && pp->idx == idx) goto skip_introducing_symbol;
        }

        symbol_introduce(current_scope, param->local->token, (AstNode *) param->local);

    skip_introducing_symbol:
        idx++;
    }

    bh_arr_each(AstParam, param, query->function_header->params) {
        if (param->local->type_node != NULL) {
            resolved_a_symbol = 0;

            onyx_errors_disable();
            param->local->flags |= Ast_Flag_Symbol_Invisible;
            symres_type(&param->local->type_node);
            param->local->flags &= ~Ast_Flag_Symbol_Invisible;
            onyx_errors_enable();
            
            if (resolved_a_symbol) query->successful_symres = 1;
        }
    }

    scope_leave();
    return Symres_Success;
}

static SymresStatus symres_foreign_block(AstForeignBlock *fb) {
    if (fb->scope == NULL)
        fb->scope = scope_create(context.ast_alloc, current_scope, fb->token->pos);

    SYMRES(expression, &fb->module_name);

    if (fb->module_name->kind != Ast_Kind_StrLit) {
        onyx_report_error(fb->token->pos, Error_Critical, "Expected module name to be a compile-time string literal.");
        return Symres_Error;
    }

    bh_arr_each(Entity *, pent, fb->captured_entities) {
        Entity *ent = *pent;
        if (ent->type == Entity_Type_Function_Header) {
            if (ent->function->body->next != NULL) {
                onyx_report_error(ent->function->token->pos, Error_Critical, "Procedures declared in a #foreign block should not have bodies.");
                return Symres_Error;
            }

            ent->function->foreign.import_name = (AstTyped *) make_string_literal(context.ast_alloc, ent->function->intrinsic_name);
            ent->function->foreign.module_name = fb->module_name;
            ent->function->is_foreign = 1;
            ent->function->is_foreign_dyncall = fb->uses_dyncall;
            ent->function->entity = NULL;
            ent->function->entity_header = NULL;
            ent->function->entity_body = NULL;

            add_entities_for_node(NULL, (AstNode *) ent->function, ent->scope, ent->package);
            continue;
        }

        if (ent->type == Entity_Type_Binding) {
            AstBinding* new_binding = onyx_ast_node_new(context.ast_alloc, sizeof(AstBinding), Ast_Kind_Binding);
            new_binding->token = ent->binding->token;
            new_binding->node = ent->binding->node;

            Entity e;
            memset(&e, 0, sizeof(e));
            e.type = Entity_Type_Binding;
            e.state = Entity_State_Introduce_Symbols;
            e.binding = new_binding;
            e.scope = fb->scope;
            e.package = ent->package;

            entity_heap_insert(&context.entities, e);
        }

        if (ent->type != Entity_Type_Function) {
            entity_heap_insert_existing(&context.entities, ent);
        }
    }

    return Symres_Complete;
}

static SymresStatus symres_include(AstInclude* include) {
    if (include->name != NULL) return Symres_Goto_Parse;

    SYMRES(expression, &include->name_node);

    if (include->name_node->kind != Ast_Kind_StrLit) {
        onyx_report_error(include->token->pos, Error_Critical, "Expected compile-time known string literal here. Got '%s'.", onyx_ast_node_kind_string(include->name_node->kind));
        return Symres_Error;
    }

    OnyxToken* str_token = include->name_node->token;
    if (str_token != NULL) {
        token_toggle_end(str_token);
        include->name = bh_strdup(context.ast_alloc, str_token->text);
        string_process_escape_seqs(include->name, include->name, strlen(include->name));
        token_toggle_end(str_token);
    }

    return Symres_Goto_Parse;
}

static SymresStatus symres_file_contents(AstFileContents* fc) {
    SYMRES(expression, &fc->filename_expr);

    if (fc->filename_expr->kind != Ast_Kind_StrLit) {
        onyx_report_error(fc->token->pos, Error_Critical, "Expected given expression to be a compile-time stirng literal.");
        return Symres_Error;
    }

    return Symres_Success;
}

static SymresStatus symres_import(AstImport* import) {
    AstPackage* package = import->imported_package;
    SYMRES(package, package);

    if (import->import_package_itself) {
        OnyxToken *name = bh_arr_last(package->path);
        name = import->qualified_package_name ? import->qualified_package_name : name;

        symbol_introduce(
                current_entity->scope,
                name,
                (AstNode *) package);
    }

    if (import->specified_imports) {
        package_track_use_package(package->package, import->entity);

        Scope *import_scope = package->package->scope;
        if (import_scope == current_scope) return Symres_Complete;

        // use X { * }
        if (import->only == NULL) {
            OnyxFilePos pos = import->token->pos;
            scope_include(current_scope, import_scope, pos);
            return Symres_Complete;
        }


        // use X { a, b, c }
        bh_arr_each(QualifiedImport, qi, import->only) {
            AstNode* imported = symbol_resolve(import_scope, qi->symbol_name);
            if (imported == NULL) { // :SymresStall
                if (report_unresolved_symbols) {
                    // TODO: Change package->name to package->qualified_name when
                    // merged with the documentation generation branch.
                    onyx_report_error(qi->symbol_name->pos, Error_Critical, 
                            "The symbol '%b' was not found the package '%s'.",
                            qi->symbol_name->text, qi->symbol_name->length, package->package->name);

                    return Symres_Error;
                } else {
                    return Symres_Yield_Macro;
                }
            }

            symbol_introduce(current_scope, qi->as_name, imported);
        }
    }

    return Symres_Complete;
}

void symres_entity(Entity* ent) {
    current_entity = ent;
    if (ent->scope) scope_enter(ent->scope);

    report_unresolved_symbols = context.cycle_detected;

    SymresStatus ss = Symres_Success;
    EntityState next_state = Entity_State_Check_Types;

    switch (ent->type) {
        case Entity_Type_Binding: {
            symbol_introduce(current_scope, ent->binding->token, ent->binding->node);
            track_documentation_for_symbol_info(ent->binding->node, ent->binding->documentation);
            track_declaration_for_tags((AstNode *) ent->binding);

            if (context.doc_info) {
                onyx_docs_submit(context.doc_info, ent->binding);
            }

            package_reinsert_use_packages(ent->package);
            next_state = Entity_State_Finalized;
            break;
        }

        case Entity_Type_Static_If:               ss = symres_static_if(ent->static_if); break;

        case Entity_Type_Load_Path:
        case Entity_Type_Load_File:               ss = symres_include(ent->include); break;
        case Entity_Type_File_Contents:           ss = symres_file_contents(ent->file_contents); break;

        case Entity_Type_Foreign_Function_Header:
        case Entity_Type_Temp_Function_Header:
        case Entity_Type_Function_Header:         ss = symres_function_header(ent->function); break;
        case Entity_Type_Function:                ss = symres_function(ent->function);        break;

        case Entity_Type_Global_Header:           ss = symres_global(ent->global); break;

        case Entity_Type_Import:                  ss = symres_import(ent->import); break;


        case Entity_Type_Polymorphic_Proc:        ss = symres_polyproc(ent->poly_proc);
                                                  next_state = Entity_State_Finalized;
                                                  break;

        case Entity_Type_Overloaded_Function:     ss = symres_overloaded_function(ent->overloaded_function); break;
        case Entity_Type_Expression:              ss = symres_expression(&ent->expr); break;
        case Entity_Type_Type_Alias:              ss = symres_type(&ent->type_alias); break;
        case Entity_Type_Enum:                    ss = symres_enum(ent->enum_type); break;
        case Entity_Type_Memory_Reservation_Type: ss = symres_memres_type(&ent->mem_res); break;
        case Entity_Type_Memory_Reservation:      ss = symres_memres(&ent->mem_res); break;
        case Entity_Type_String_Literal:          ss = symres_expression(&ent->expr); break;
        case Entity_Type_Struct_Member_Default:   ss = symres_struct_defaults((AstType *) ent->type_alias); break;
        case Entity_Type_Process_Directive:       ss = symres_process_directive((AstNode *) ent->expr); break;
        case Entity_Type_Macro:                   ss = symres_macro(ent->macro); break;
        case Entity_Type_Constraint_Check:        ss = symres_constraint(ent->constraint); break;
        case Entity_Type_Polymorph_Query:         ss = symres_polyquery(ent->poly_query); break;
        case Entity_Type_Foreign_Block:           ss = symres_foreign_block(ent->foreign_block);
                                                  if (context.options->generate_foreign_info) {
                                                      next_state = Entity_State_Check_Types;
                                                      ss = Symres_Success;
                                                  }
                                                  break;

        default: break;
    }

    if (ss == Symres_Yield_Macro) ent->macro_attempts++;
    if (ss == Symres_Yield_Micro) ent->micro_attempts++;
    if (ss == Symres_Complete)    ent->state = Entity_State_Finalized;
    if (ss == Symres_Goto_Parse)  ent->state = Entity_State_Parse;
    if (ss == Symres_Error)       ent->state = Entity_State_Failed;
    if (ss == Symres_Success) {
        ent->macro_attempts = 0;
        ent->micro_attempts = 0;
        ent->state = next_state;
    }

    current_scope = NULL;
    current_entity = NULL;
}
