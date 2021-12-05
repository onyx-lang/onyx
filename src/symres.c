#define BH_DEBUG
#include "parser.h"
#include "utils.h"
#include "astnodes.h"
#include "errors.h"

// Variables used during the symbol resolution phase.
static Scope*       curr_scope    = NULL;
static b32 report_unresolved_symbols = 1;
static b32 resolved_a_symbol         = 0;

// Everything related to waiting on is imcomplete at the moment.
static Entity* waiting_on         = NULL;

#define SYMRES(kind, ...) do { \
    SymresStatus ss = symres_ ## kind (__VA_ARGS__); \
    if (ss > Symres_Errors_Start) return ss;         \
    } while (0)

typedef enum SymresStatus {
    Symres_Success,
    Symres_Complete,

    Symres_Errors_Start,
    Symres_Yield_Macro,
    Symres_Yield_Micro,
    Symres_Error,
} SymresStatus;

static SymresStatus symres_type(AstType** type);
static SymresStatus symres_local(AstLocal** local);
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
            return Symres_Yield_Macro;
        }

    } else {
        *symbol_node = res;
        resolved_a_symbol = 1;
    }

    return Symres_Success;
}

static SymresStatus symres_struct_type(AstStructType* s_node) {
    if (s_node->flags & Ast_Flag_Type_Is_Resolved) return Symres_Success;

    s_node->flags |= Ast_Flag_Type_Is_Resolved;
    s_node->flags |= Ast_Flag_Comptime;

    if (s_node->scope) {
        // FIX: This is probably wrong for the long term.
        s_node->scope->parent = curr_scope;

        scope_enter(s_node->scope);
    }

    if (s_node->constraints.constraints) {
        bh_arr_each(AstConstraint *, constraint, s_node->constraints.constraints) {
            SYMRES(constraint, *constraint);
        }
    }

    fori (i, 0, bh_arr_length(s_node->members)) {
        AstStructMember *member = s_node->members[i];

        if (member->type_node) {
            SymresStatus ss = symres_type(&member->type_node);
            if (ss != Symres_Success) {
                s_node->flags &= ~Ast_Flag_Type_Is_Resolved;
                if (s_node->scope) scope_leave();
                return ss;
            }
        }
    }

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
                onyx_report_error((*type)->token->pos, "Field access did not result in a type. (%s)", onyx_ast_node_kind_string((*type)->kind));
            break;
        }

        case Ast_Kind_Pointer_Type: SYMRES(type, &((AstPointerType *) *type)->elem); break;
        case Ast_Kind_Slice_Type:   SYMRES(type, &((AstSliceType *) *type)->elem); break;
        case Ast_Kind_DynArr_Type:  SYMRES(type, &((AstDynArrType *) *type)->elem); break;
        case Ast_Kind_VarArg_Type:  SYMRES(type, &((AstVarArgType *) *type)->elem); break;

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
        case Ast_Kind_Array_Type: {
            AstArrayType* a_node = (AstArrayType *) *type;

            if (a_node->count_expr) SYMRES(expression, &a_node->count_expr);
            SYMRES(type, &a_node->elem);
            break;
        }

        case Ast_Kind_Enum_Type: break;

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* pst_node = (AstPolyStructType *) *type;
            pst_node->scope = scope_create(context.ast_alloc, pst_node->entity->scope, pst_node->token->pos);

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
            break;
        }

        case Ast_Kind_Alias: {
            AstAlias* alias = (AstAlias *) *type;
            alias->flags |= Ast_Flag_Symbol_Invisible;
            SYMRES(type, (AstType **) &alias->alias);
            alias->flags &= ~Ast_Flag_Symbol_Invisible;
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

    AstTyped* expr = (AstTyped *) strip_aliases((AstNode *) (*fa)->expr);

    b32 force_a_lookup = 0;
    if (expr->kind == Ast_Kind_Enum_Type) {
        force_a_lookup = 1;
    }

    AstNode* resolution = try_symbol_resolve_from_node((AstNode *) expr, (*fa)->token);
    if (resolution) *((AstNode **) fa) = resolution;
    else if (expr->kind == Ast_Kind_Package) {
        if (report_unresolved_symbols) {
            token_toggle_end((*fa)->token);
            char *closest = find_closest_symbol_in_node((AstNode *) expr, (*fa)->token->text);
            token_toggle_end((*fa)->token);

            if (closest) {
                onyx_report_error((*fa)->token->pos, "'%b' was not found in package '%s'. Did you mean '%s'?",
                    (*fa)->token->text,
                    (*fa)->token->length,
                    ((AstPackage *) (*fa)->expr)->package->name,
                    closest);
            } else {
                onyx_report_error((*fa)->token->pos, "'%b' was not found in package '%s'. Perhaps it is defined in a file that wasn't loaded?",
                    (*fa)->token->text,
                    (*fa)->token->length,
                    ((AstPackage *) (*fa)->expr)->package->name);
            }
            return Symres_Error;

        } else {
            return Symres_Yield_Macro;
        }
    }
    else if (force_a_lookup) {
        if (context.cycle_detected) {
            onyx_report_error((*fa)->token->pos, "'%b' does not exist here. This is a bad error message.",
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

    if (((*mcall)->flags & Ast_Flag_Has_Been_Symres) == 0) {
        AstFieldAccess* implicit_field_access = make_field_access(context.ast_alloc, (*mcall)->left, NULL);
        implicit_field_access->token = call_node->callee->token;
        call_node->callee = (AstTyped *) implicit_field_access;
    }

    SYMRES(expression, (AstTyped **) &call_node);
    (*mcall)->flags |= Ast_Flag_Has_Been_Symres;

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

    sl->type_node = (AstType *) sl->stnode;
    while (sl->type_node && sl->type_node->kind == Ast_Kind_Type_Alias)
        sl->type_node = ((AstTypeAlias *) sl->type_node)->to;

    SYMRES(arguments, &sl->args);

    return Symres_Success;
}

static SymresStatus symres_array_literal(AstArrayLiteral* al) {
    if (al->atnode != NULL) SYMRES(expression, &al->atnode);

    al->type_node = (AstType *) al->atnode;
    while (al->type_node && al->type_node->kind == Ast_Kind_Type_Alias)
        al->type_node = ((AstTypeAlias *) al->type_node)->to;

    bh_arr_each(AstTyped *, expr, al->values)
        SYMRES(expression, expr);

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
        case Ast_Kind_Alias: {
            (*expr)->flags |= Ast_Flag_Symbol_Invisible;
            SYMRES(expression, &((AstAlias *) *expr)->alias);
            (*expr)->flags &= ~Ast_Flag_Symbol_Invisible;
            break;
        }

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

        case Ast_Kind_Do_Block:
            SYMRES(block, ((AstDoBlock *) *expr)->block);
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
    if (ifnode->kind == Ast_Kind_Static_If) {
        if ((ifnode->flags & Ast_Flag_Static_If_Resolved) == 0) {
            return Symres_Yield_Macro;
        }

        if (static_if_resolution(ifnode)) {
            if (ifnode->true_stmt != NULL)  SYMRES(statement, (AstNode **) &ifnode->true_stmt, NULL);

        } else {
            if (ifnode->false_stmt != NULL) SYMRES(statement, (AstNode **) &ifnode->false_stmt, NULL);
        }

    } else {
        if (ifnode->initialization != NULL) {
            ifnode->scope = scope_create(context.ast_alloc, curr_scope, ifnode->token->pos);
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
        whilenode->scope = scope_create(context.ast_alloc, curr_scope, whilenode->token->pos);
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
    fornode->scope = scope_create(context.ast_alloc, curr_scope, fornode->token->pos);
    scope_enter(fornode->scope);
    SYMRES(expression, &fornode->iter);
    SYMRES(local, &fornode->var);
    SYMRES(block, fornode->stmt);
    scope_leave();

    return Symres_Success;
}

static SymresStatus symres_switch(AstSwitch* switchnode) {
    if (switchnode->initialization != NULL) {
        switchnode->scope = scope_create(context.ast_alloc, curr_scope, switchnode->token->pos);
        scope_enter(switchnode->scope);

        SYMRES(statement_chain, &switchnode->initialization);
    }

    SYMRES(expression, &switchnode->expr);

    bh_arr_each(AstSwitchCase, sc, switchnode->cases) {
        bh_arr_each(AstTyped *, value, sc->values)
            SYMRES(expression, value);

        SYMRES(block, sc->block);
    }

    if (switchnode->default_case)
        SYMRES(block, switchnode->default_case);

    if (switchnode->switch_kind == Switch_Kind_Use_Equals && switchnode->case_exprs) {
        bh_arr_each(CaseToBlock, ctb, switchnode->case_exprs) {
            SYMRES(expression, (AstTyped **) &ctb->comparison);
        }
    }

    if (switchnode->initialization != NULL) scope_leave();

    return Symres_Success;
}

// CLEANUP: A lot of duplication going on in this function. A proper
// "namespace" concept would be useful to remove a lot of the fluff
// code here. There already is try_resolve_symbol_from_node which
// may be able to do what is needed here?
static SymresStatus symres_use(AstUse* use) {
    SYMRES(expression, &use->expr);

    if (use->expr->kind == Ast_Kind_Package) {
        AstPackage* package = (AstPackage *) use->expr;
        SYMRES(package, package);

        if (package->package->scope == curr_scope) return Symres_Success;

        if (use->only == NULL) {
            OnyxFilePos pos = { 0 };
            if (use->token != NULL)
                pos = use->token->pos;

            scope_include(curr_scope, package->package->scope, pos);

        } else {
            bh_arr_each(QualifiedUse, qu, use->only) {
                AstNode* thing = symbol_resolve(package->package->scope, qu->symbol_name);
                if (thing == NULL) { // :SymresStall
                    if (report_unresolved_symbols) {
                        onyx_report_error(qu->symbol_name->pos,
                                "The symbol '%b' was not found in this package.",
                                qu->symbol_name->text, qu->symbol_name->length);
                        return Symres_Error;
                    } else {
                        return Symres_Yield_Macro;
                    }
                }

                symbol_introduce(curr_scope, qu->as_name, thing);
            }
        }

        package_track_use_package(package->package, use->entity);

        return Symres_Success;
    }

    if (use->expr->kind == Ast_Kind_Enum_Type) {
        AstEnumType* et = (AstEnumType *) use->expr;

        bh_arr_each(AstEnumValue *, ev, et->values)
            symbol_introduce(curr_scope, (*ev)->token, (AstNode *) *ev);

        return Symres_Success;
    }

    if (use->expr->kind == Ast_Kind_Struct_Type) {
        AstStructType* st = (AstStructType *) use->expr;
        if (!st->scope) return Symres_Success;

        if (use->only == NULL) {
            scope_include(curr_scope, st->scope, use->token->pos);

        } else {
            bh_arr_each(QualifiedUse, qu, use->only) {
                AstNode* thing = symbol_resolve(st->scope, qu->symbol_name);
                if (thing == NULL) {
                    onyx_report_error(qu->symbol_name->pos,
                            "The symbol '%b' was not found in this scope.",
                            qu->symbol_name->text, qu->symbol_name->length);
                    return Symres_Error;
                }

                symbol_introduce(curr_scope, qu->as_name, thing);
            }
        }

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

        fori (i, 0, shlen(st->Struct.members)) {
            StructMember value = st->Struct.members[i].value;
            AstFieldAccess* fa = make_field_access(context.ast_alloc, use->expr, value.name);
            symbol_raw_introduce(curr_scope, value.name, use->token->pos, (AstNode *) fa);
        }

        return Symres_Success;
    }

cannot_use:
    onyx_report_error(use->token->pos, "Cannot use this because its type is unknown.");
    return Symres_Error;
}

static SymresStatus symres_directive_solidify(AstDirectiveSolidify** psolid) {
    // @Cleanup: A lot of this code should move to the checker?
    
    AstDirectiveSolidify* solid = *psolid;
    if (solid->resolved_proc != NULL) {
        *psolid = (AstDirectiveSolidify *) solid->resolved_proc;
        return Symres_Success;
    }

    SYMRES(expression, (AstTyped **) &solid->poly_proc);
    if (solid->poly_proc && solid->poly_proc->kind == Ast_Kind_Directive_Solidify) {
        AstPolyProc* potentially_resolved_proc = (AstPolyProc *) ((AstDirectiveSolidify *) solid->poly_proc)->resolved_proc;
        if (!potentially_resolved_proc) return Symres_Yield_Micro;

        solid->poly_proc = potentially_resolved_proc;
    }

    if (!solid->poly_proc || solid->poly_proc->kind != Ast_Kind_Polymorphic_Proc) {
        onyx_report_error(solid->token->pos, "Expected polymorphic procedure in #solidify directive.");
        return Symres_Error;
    }

    bh_arr_each(AstPolySolution, sln, solid->known_polyvars) {
        // HACK: This assumes that 'ast_type' and 'value' are at the same offset.
        SYMRES(expression, &sln->value);

        if (node_is_type((AstNode *) sln->value)) {
            sln->type = type_build_from_ast(context.ast_alloc, sln->ast_type);
            sln->kind = PSK_Type;
        } else {
            sln->kind = PSK_Value;
        }
    }

    solid->resolved_proc = polymorphic_proc_try_solidify(solid->poly_proc, solid->known_polyvars, solid->token);
    if (solid->resolved_proc == (AstNode *) &node_that_signals_a_yield) {
        solid->resolved_proc = NULL;
        return Symres_Yield_Macro;
    }

    // NOTE: Not a DirectiveSolidify.
    *psolid = (AstDirectiveSolidify *) solid->resolved_proc;
    return Symres_Success;
}

static SymresStatus symres_directive_defined(AstDirectiveDefined** pdefined) {
    AstDirectiveDefined* defined = *pdefined;

    b32 use_package_count = (context.entities.type_count[Entity_Type_Use_Package] == 0);

    SymresStatus ss = symres_expression(&defined->expr);
    if (use_package_count && ss != Symres_Success) {
        // The symbol definitely was not found and there is no chance that it could be found.
        defined->is_defined = 0;
        return Symres_Success;
    }

    if (ss == Symres_Success) {
        defined->is_defined = 1;
        return Symres_Success;
    }

    return Symres_Yield_Macro;
}

static SymresStatus symres_directive_insert(AstDirectiveInsert* insert) {
    SYMRES(expression, &insert->code_expr);
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
        case Ast_Kind_Call:        SYMRES(call, (AstCall *) *stmt);                      break;
        case Ast_Kind_Argument:    SYMRES(expression, (AstTyped **) &((AstArgument *) *stmt)->value); break;
        case Ast_Kind_Block:       SYMRES(block, (AstBlock *) *stmt);                    break;
        case Ast_Kind_Defer:       SYMRES(statement, &((AstDefer *) *stmt)->stmt, NULL); break;
        case Ast_Kind_Jump:        break;

        case Ast_Kind_Local:
            // if (remove) *remove = 1;
            SYMRES(local, (AstLocal **) stmt);
            break;

        case Ast_Kind_Use:
            if (remove) *remove = 1;
            SYMRES(use, (AstUse *) *stmt);
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
            block->scope = scope_create(context.ast_alloc, curr_scope, block->token->pos);

        scope_enter(block->scope);
    }

    if (block->binding_scope != NULL)
        scope_include(curr_scope, block->binding_scope, block->token->pos);

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

    if (func->scope == NULL)
        func->scope = scope_create(context.ast_alloc, curr_scope, func->token->pos);

    scope_enter(func->scope);

    bh_arr_each(AstParam, param, func->params) {
        if (param->default_value != NULL) {
            SYMRES(expression, &param->default_value);
            if (onyx_has_errors()) return Symres_Error;
        }
    }

    bh_arr_each(AstParam, param, func->params) {
        symbol_introduce(curr_scope, param->local->token, (AstNode *) param->local);

        if (param->local->type_node != NULL) {
            SYMRES(type, &param->local->type_node);
        }
    }

    SYMRES(type, &func->return_type);
    if (!node_is_type((AstNode *) func->return_type)) {
        AstType* return_type = (AstType *) strip_aliases((AstNode *) func->return_type);
        if (return_type->kind == Ast_Kind_Symbol) return Symres_Yield_Macro;

        onyx_report_error(func->token->pos, "Return type is not a type.");
    }

    if (func->constraints.constraints != NULL) {
        bh_arr_each(AstConstraint *, constraint, func->constraints.constraints) {
            SYMRES(constraint, *constraint);
        }
    }

    scope_leave();

    return Symres_Success;
}

SymresStatus symres_function(AstFunction* func) {
    if (func->entity_header && func->entity_header->state < Entity_State_Check_Types) return Symres_Yield_Macro;
    assert(func->scope);

    scope_enter(func->scope);

    if ((func->flags & Ast_Flag_Has_Been_Symres) == 0) {
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

                    fori (i, 0, shlen(st->Struct.members)) {
                        StructMember value = st->Struct.members[i].value;
                        AstFieldAccess* fa = make_field_access(context.ast_alloc, (AstTyped *) param->local, value.name);
                        symbol_raw_introduce(curr_scope, value.name, param->local->token->pos, (AstNode *) fa);
                    }

                    param->use_processed = 1;

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
    return Symres_Success;
}

static SymresStatus symres_package(AstPackage* package) {
    if (package->package == NULL) {
        if (!package->package_name) return Symres_Error;

        package->package = package_lookup(package->package_name);
    }

    if (package->package) {
        return Symres_Success;
    } else {
        if (report_unresolved_symbols) {
            onyx_report_error(package->token->pos,
                    "Package '%s' not found in included source files.",
                    package->package_name);
            return Symres_Error;
        } else {
            return Symres_Yield_Macro;
        }
    }
}

static SymresStatus symres_enum(AstEnumType* enum_node) {
    if (enum_node->backing->kind == Ast_Kind_Symbol) SYMRES(symbol, (AstNode **) &enum_node->backing);
    if (enum_node->backing == NULL) return Symres_Error;

    if (enum_node->scope == NULL) {
        enum_node->backing_type = type_build_from_ast(context.ast_alloc, enum_node->backing);
        enum_node->scope = scope_create(context.ast_alloc, curr_scope, enum_node->token->pos);

        type_build_from_ast(context.ast_alloc, (AstType *) enum_node);
    }

    scope_enter(enum_node->scope);

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
                    onyx_report_error((*value)->token->pos, "expected numeric integer literal for enum initialization, got '%s'", type_get_name(n_value->type));
                    return Symres_Error;
                }

                n_value->type = enum_node->etcache;

            } else {
                if ((*value)->entity == NULL) {
                    add_entities_for_node(NULL, (AstNode *) (*value), enum_node->scope, NULL);
                }

                if (context.cycle_detected) {
                    onyx_report_error((*value)->token->pos, "Expected compile time known value for enum initialization.");
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

static SymresStatus symres_polyproc(AstPolyProc* pp) {
    pp->flags |= Ast_Flag_Comptime;
    pp->poly_scope = curr_scope;
    return Symres_Success;
}

static SymresStatus symres_static_if(AstIf* static_if) {
    SYMRES(expression, &static_if->cond);
    return Symres_Success;
}

static SymresStatus symres_process_directive(AstNode* directive) {
    switch (directive->kind) {
        case Ast_Kind_Directive_Add_Overload: {
            AstDirectiveAddOverload *add_overload = (AstDirectiveAddOverload *) directive;

            SYMRES(expression, (AstTyped **) &add_overload->overloaded_function);
            if (add_overload->overloaded_function == NULL) return Symres_Error; // NOTE: Error message will already be generated

            if (add_overload->overloaded_function->kind != Ast_Kind_Overloaded_Function) {
                onyx_report_error(add_overload->token->pos, "#add_overload directive did not resolve to an overloaded function.");

            } else {
                AstOverloadedFunction* ofunc = (AstOverloadedFunction *) add_overload->overloaded_function;
                SYMRES(expression, (AstTyped **) &add_overload->overload);
                add_overload_option(&ofunc->overloads, add_overload->precedence, add_overload->overload);
            }

            break;
        }

        case Ast_Kind_Directive_Operator: {
            AstDirectiveOperator *operator = (AstDirectiveOperator *) directive;
            SYMRES(expression, &operator->overload);
            if (!operator->overload) return Symres_Error;

            AstFunction* overload = get_function_from_node((AstNode *) operator->overload);
            if (overload == NULL) {
                onyx_report_error(operator->token->pos, "This cannot be used as an operator overload.");
                return Symres_Error;
            }

            if (operator->operator != Binary_Op_Subscript_Equals && bh_arr_length(overload->params) != 2) {
                onyx_report_error(operator->token->pos, "Expected exactly 2 arguments for binary operator overload.");
                return Symres_Error;
            }

            add_overload_option(&operator_overloads[operator->operator], 0, operator->overload);
            break;
        }

        case Ast_Kind_Directive_Export: {
            AstDirectiveExport *export = (AstDirectiveExport *) directive;
            SYMRES(expression, &export->export);

            if (export->export->kind == Ast_Kind_Polymorphic_Proc) {
                onyx_report_error(export->token->pos, "Cannot export a polymorphic function.");
                return Symres_Error;
            }

            if (export->export->kind == Ast_Kind_Function) {
                AstFunction *func = (AstFunction *) export->export;
                func->exported_name = export->export_name;
                func->is_exported = 1;

                if (func->is_exported) {
                    if (func->is_foreign) {
                        onyx_report_error(export->token->pos, "Cannot export a foreign function.");
                        return Symres_Error;
                    }

                    if (func->is_intrinsic) {
                        onyx_report_error(export->token->pos, "Cannot export an intrinsic function.");
                        return Symres_Error;
                    }
                }
            }

            break;
        }

        case Ast_Kind_Directive_Tag: {
            AstDirectiveTag *tag = (AstDirectiveTag *) directive;
            SYMRES(expression, &tag->tag);
            SYMRES(expression, &tag->expr);
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
    }

    return Symres_Success;
}

static SymresStatus symres_macro(AstMacro* macro) {
    if (macro->body->kind == Ast_Kind_Function) {
        SYMRES(function_header, (AstFunction *) macro->body);
    }
    else if (macro->body->kind == Ast_Kind_Polymorphic_Proc) {
        SYMRES(polyproc, (AstPolyProc *) macro->body);
    }

    macro->flags |= Ast_Flag_Comptime;

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
            fori (i, constraint->expr_idx, bh_arr_length(constraint->exprs)) {
                SYMRES(expression, &constraint->exprs[i]);
            }

            return Symres_Success;
        }
    }

    return Symres_Success;
}

static SymresStatus symres_polyquery(AstPolyQuery *query) {
    query->successful_symres = 0;

    if (query->function_header->scope == NULL)
        query->function_header->scope = scope_create(context.ast_alloc, query->proc->poly_scope, query->token->pos);

    scope_enter(query->function_header->scope);

    u32 idx = 0;
    bh_arr_each(AstParam, param, query->function_header->params) {
        bh_arr_each(AstPolyParam, pp, query->proc->poly_params) {
            if (pp->kind == PPK_Baked_Value && pp->idx == idx) goto skip_introducing_symbol;
        }
        symbol_introduce(curr_scope, param->local->token, (AstNode *) param->local);

    skip_introducing_symbol:
        if (param->local->type_node != NULL) {
            resolved_a_symbol = 0;
            symres_type(&param->local->type_node);
            onyx_clear_errors();

            if (resolved_a_symbol) query->successful_symres = 1;
        }

        idx++;
    }

    scope_leave();
    return Symres_Success;
}

static SymresStatus symres_foreign_block(AstForeignBlock *fb) {
    bh_arr_each(Entity *, pent, fb->captured_entities) {
        Entity *ent = *pent;
        if (ent->type == Entity_Type_Function_Header) {
            if (ent->function->body->next != NULL) {
                onyx_report_error(ent->function->token->pos, "Procedures declared in a #foreign block should not have bodies.");
                return Symres_Error;
            }

            ent->function->foreign_name = ent->function->intrinsic_name; // Hmm... This might not be right?
            ent->function->foreign_module = fb->module_name;
            ent->function->is_foreign = 1;
            ent->function->entity = NULL;
            ent->function->entity_header = NULL;
            ent->function->entity_body = NULL;

            add_entities_for_node(NULL, (AstNode *) ent->function, ent->scope, ent->package);
            continue;
        }

        if (ent->type != Entity_Type_Function) {
            entity_heap_insert_existing(&context.entities, ent);
        }
    }

    return Symres_Complete;
}

void symres_entity(Entity* ent) {
    if (ent->scope) scope_enter(ent->scope);

    report_unresolved_symbols = context.cycle_detected;

    SymresStatus ss = Symres_Success;
    EntityState next_state = Entity_State_Check_Types;

    switch (ent->type) {
        case Entity_Type_Binding: {
            symbol_introduce(curr_scope, ent->binding->token, ent->binding->node);
            package_reinsert_use_packages(ent->package);
            next_state = Entity_State_Finalized;
            break;
        }

        case Entity_Type_Static_If:               ss = symres_static_if(ent->static_if); break;

        case Entity_Type_Foreign_Function_Header:
        case Entity_Type_Temp_Function_Header:
        case Entity_Type_Function_Header:         ss = symres_function_header(ent->function); break;
        case Entity_Type_Function:                ss = symres_function(ent->function);        break;

        case Entity_Type_Global_Header:           ss = symres_global(ent->global); break;

        case Entity_Type_Use_Package:
        case Entity_Type_Use:                     ss = symres_use(ent->use);
                                                  next_state = Entity_State_Finalized;
                                                  break;

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
        case Entity_Type_Foreign_Block:           ss = symres_foreign_block(ent->foreign_block); break;

        default: break;
    }

    if (ss == Symres_Yield_Macro) ent->macro_attempts++;
    if (ss == Symres_Yield_Micro) ent->micro_attempts++;
    if (ss == Symres_Complete)    ent->state = Entity_State_Finalized;
    if (ss == Symres_Success) {
        ent->macro_attempts = 0;
        ent->micro_attempts = 0;
        ent->state = next_state;
    }

    curr_scope = NULL;
}
