
//
// Polymorphic Procedures
//

static void ensure_polyproc_cache_is_created(Context *context, AstFunction* pp) {
    if (pp->concrete_funcs == NULL)        sh_new_arena(pp->concrete_funcs);
    if (pp->active_queries.hashes == NULL) bh_imap_init(&pp->active_queries, context->gp_alloc, 31);
}

void insert_poly_sln_into_scope(Context *context, Scope* scope, AstPolySolution *sln) {
    AstNode *node = NULL;

    switch (sln->kind) {
        case PSK_Type:
            node = onyx_ast_node_new(context->ast_alloc, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
            ((AstTypeRawAlias *) node)->token = sln->poly_sym->token;
            ((AstTypeRawAlias *) node)->to = sln->type;
            ((AstTypeRawAlias *) node)->type = context->types.basic[Basic_Kind_Type_Index];
            ((AstTypeRawAlias *) node)->type_id = sln->type->id;
            break;

        case PSK_Value:
            // CLEANUP: Maybe clone this?
            // assert(sln->value->flags & Ast_Flag_Comptime);
            if ((sln->value->flags & Ast_Flag_Comptime) == 0) {
                ONYX_ERROR(sln->value->token->pos, Error_Critical, "Expected value to be compile time known.");
                return;
            }

            node = (AstNode *) sln->value;
            break;

        case PSK_Undefined: assert("Unexpected PSK_Undefined" && 0); break;
    }

    symbol_introduce(context, scope, sln->poly_sym->token, node);
}

static void insert_poly_slns_into_scope(Context *context, Scope* scope, bh_arr(AstPolySolution) slns) {
    bh_arr_each(AstPolySolution, sln, slns) {
        insert_poly_sln_into_scope(context, scope, sln);
    }
}

// NOTE: This might return a volatile string. Do not store it without copying it.
static char* build_poly_solution_key(Context *context, AstPolySolution* sln) {
    if (sln->kind == PSK_Type) {
        return bh_aprintf(context->ast_alloc, "@%d", sln->type->id);

    } else if (sln->kind == PSK_Value) {
        static char buffer[256];

        fori (i, 0, 256) buffer[i] = 0;

        if (sln->value->kind == Ast_Kind_NumLit) {
            strncat(buffer, "NUMLIT:", 127);
            strncat(buffer, bh_bprintf("%l", ((AstNumLit *) sln->value)->value.l), 127);

        } else {
            // HACK: For now, the value pointer is just used. This means that
            // sometimes, even through the solution is the same, it won't be
            // stored the same.
            bh_snprintf(buffer, 128, "%p", sln->value);
        }

        return buffer;
    }

    return NULL;
}

// NOTE: This returns a volatile string. Do not store it without copying it.
static char* build_poly_slns_unique_key(Context *context, bh_arr(AstPolySolution) slns) {
    static char key_buf[1024];
    fori (i, 0, 1024) key_buf[i] = 0;

    bh_arr_each(AstPolySolution, sln, slns) {
        if (sln != slns) strncat(key_buf, "$", 1023);

        token_toggle_end(sln->poly_sym->token);

        strncat(key_buf, sln->poly_sym->token->text, 1023);
        strncat(key_buf, "=", 1023);
        strncat(key_buf, build_poly_solution_key(context, sln), 1023);

        token_toggle_end(sln->poly_sym->token);
    }

    return key_buf;
}

// NOTE: This function adds a solidified function to the entity heap for it to be processed
// later. It optionally can start the function header entity at the code generation state if
// the header has already been processed.
static b32 add_solidified_function_entities(Context *context, AstSolidifiedFunction *solidified_func) {
    solidified_func->func->flags |= Ast_Flag_From_Polymorphism;

    Entity func_header_entity = {
        .state = Entity_State_Check_Types,
        .type = Entity_Type_Function_Header,
        .function = solidified_func->func,
        .package = NULL,
        .scope = solidified_func->func->poly_scope,
    };

    Entity func_entity = {
        .state = Entity_State_Check_Types,
        .type = Entity_Type_Function,
        .function = solidified_func->func,
        .package = NULL,
        .scope = solidified_func->func->poly_scope,
    };

    Entity* entity_header = entity_heap_insert(&context->entities, func_header_entity);
    Entity* entity_body   = entity_heap_insert(&context->entities, func_entity);

    solidified_func->func_header_entity  = entity_header;
    solidified_func->func->entity_header = entity_header;
    solidified_func->func->entity_body   = entity_body;
    solidified_func->func->entity        = entity_body;
    return 1;
}

// NOTE: This function is responsible for taking all of the information about generating
// a new polymorphic variant, and producing a solidified function. It optionally can only
// generate the header of the function, which is useful for cases such as checking if a
// set of arguments works for a polymorphic overload option.
static AstSolidifiedFunction generate_solidified_function(
    Context *context, 
    AstFunction* pp,
    bh_arr(AstPolySolution) slns,
    OnyxToken* tkn,
    b32 header_only) {

    AstSolidifiedFunction solidified_func;
    solidified_func.func_header_entity = NULL;

    // NOTE: Use the position of token if one was provided, otherwise just use NULL.
    OnyxFilePos poly_scope_pos = { 0 };
    if (tkn) poly_scope_pos = tkn->pos;

    if (header_only) {
        solidified_func.func = (AstFunction *) clone_function_header(context, pp);
        solidified_func.func->flags |= Ast_Flag_Incomplete_Body;

    } else {
        solidified_func.func = (AstFunction *) ast_clone(context, pp);
    }

    assert(pp->parent_scope_of_poly_proc);
    solidified_func.func->poly_scope = scope_create(context, pp->parent_scope_of_poly_proc, poly_scope_pos);
    insert_poly_slns_into_scope(context, solidified_func.func->poly_scope, slns);

    solidified_func.func->flags |= Ast_Flag_From_Polymorphism;
    solidified_func.func->generated_from = tkn;

    // HACK: Remove the baked parameters from the function defintion so they can be
    // resolved in the poly scope above the function. This does feel kinda of gross
    // and I would love an alternative to tell it to just "skip" the parameter, but
    // that is liable to breaking because it is one more thing to remember.
    //                                             - brendanfh 2021/01/18
    u32 removed_params = 0;
    bh_arr_each(AstPolyParam, param, pp->poly_params) {
        if (param->implicit_interface) {
            AstConstraint *constraint = onyx_ast_node_new(context->ast_alloc, sizeof(AstConstraint), Ast_Kind_Constraint);
            constraint->interface = (AstInterface *) param->implicit_interface;
            constraint->token = constraint->interface->token;

            bh_arr_new(context->gp_alloc, constraint->args, 1);
            bh_arr_push(constraint->args, (AstTyped *) ast_clone(context, param->poly_sym));

            //
            // Sometimes this array is uninitialized, and that would cause a memory leak
            // because the memory wouldn't be tracked in the gp_alloc.
            if (!solidified_func.func->constraints.constraints) {
                bh_arr_new(context->gp_alloc, solidified_func.func->constraints.constraints, 1);
            }

            bh_arr_push(solidified_func.func->constraints.constraints, constraint);
        }

        if (param->kind != PPK_Baked_Value) continue;

        bh_arr_deleten(solidified_func.func->params, param->idx - removed_params, 1);
        removed_params++;
    }

    return solidified_func;
}

static void ensure_solidified_function_has_body(Context *context, AstFunction* pp, AstSolidifiedFunction *solidified_func) {
    if (solidified_func->func->flags & Ast_Flag_Incomplete_Body) {
        clone_function_body(context, solidified_func->func, pp);

        // HACK: I'm asserting that this function should return without an error, because
        // the only case where it can return an error is if there was a problem with the
        // header. This should never be the case in this situation, since the header would
        // have to have successfully passed type checking before it would become a solidified
        // procedure.
        assert(add_solidified_function_entities(context, solidified_func));

        solidified_func->func->flags &= ~Ast_Flag_Incomplete_Body;
    }
}

// NOTE: These are temporary data structures used to represent the pattern matching system
// of polymorphic type resolution.
typedef struct PolySolveResult {
    PolySolutionKind kind;
    union {
        AstTyped* value;
        Type*     actual;
    };
} PolySolveResult;

typedef struct PolySolveElem {
    AstType* type_expr;

    PolySolutionKind kind;
    union {
        AstTyped* value;
        Type*     actual;
    };
} PolySolveElem;

// NOTE: The job of this function is to solve for the type/value that belongs in a
// polymorphic variable. This function takes in three arguments:
//  * The symbol node of the polymorphic parameter being searched for
//  * The type expression that should contain the symbol node it is some where
//  * The actual type to pattern match against
//
// This function utilizes a basic breadth-first search of the type_expr and actual type
// trees, always moving along them in parallel, so when the target is reached (if it is
// ever reached), the "actual" is the matched type/value.
static PolySolveResult solve_poly_type(Context *context, AstNode* target, AstType* type_expr, Type* actual) {
    bh_arr(PolySolveElem) elem_queue = NULL;
    bh_arr_new(context->gp_alloc, elem_queue, 4);

    PolySolveResult result = { PSK_Undefined, { NULL } };

    bh_arr_push(elem_queue, ((PolySolveElem) {
        .type_expr = type_expr,
        .kind = PSK_Type,
        .actual    = actual
    }));

    while (!bh_arr_is_empty(elem_queue)) {
        PolySolveElem elem = elem_queue[0];
        bh_arr_deleten(elem_queue, 0, 1);

        // This check does not strictly need the `type_auto_return` check,
        // but it does prevent bugs if the auto return type placeholder is
        // accidentally inserted into the real type.
        if (elem.type_expr == (AstType *) target && elem.actual != context->types.auto_return) {
            result.kind = elem.kind;

            assert(elem.kind != PSK_Undefined);
            if (result.kind == PSK_Type)  result.actual = elem.actual;
            if (result.kind == PSK_Value) result.value = elem.value;
            break;
        }

        if (elem.kind != PSK_Type) continue;

        switch (elem.type_expr->kind) {
            case Ast_Kind_Pointer_Type: {
                if (elem.actual->kind != Type_Kind_Pointer && elem.actual->kind != Type_Kind_MultiPointer) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstPointerType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->Pointer.elem,
                }));
                break;
            }

            case Ast_Kind_Multi_Pointer_Type: {
                if (elem.actual->kind != Type_Kind_MultiPointer) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstMultiPointerType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->MultiPointer.elem,
                }));
                break;
            }

            case Ast_Kind_Address_Of: {
                if (elem.actual->kind != Type_Kind_Pointer) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = (AstType *) ((AstAddressOf *) elem.type_expr)->expr,
                    .kind = PSK_Type,
                    .actual = elem.actual->Pointer.elem,
                }));
                break;
            }

            case Ast_Kind_Array_Type: {
                if (elem.actual->kind != Type_Kind_Array) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = (AstType*) ((AstArrayType *) elem.type_expr)->count_expr,
                    .kind = PSK_Value,

                    // CLEANUP: Making an integer literal every time is very very very gross. This should
                    // at least be cached or something.
                    .value = (AstTyped *) make_int_literal(context, elem.actual->Array.count)
                }));

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstArrayType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->Array.elem,
                }));
                break;
            }

            case Ast_Kind_Slice_Type: {
                if (elem.actual->kind != Type_Kind_Slice && elem.actual->kind != Type_Kind_DynArray
                        && elem.actual->kind != Type_Kind_VarArgs && elem.actual->kind != Type_Kind_Array) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstSliceType *) elem.type_expr)->elem,
                    .kind = PSK_Type,

                    // HACK: This makes the assumption that arrays, slices, dynamic arrays and varargs have the same element type at the same location.
                    .actual = elem.actual->Slice.elem,
                }));
                break;
            }

            case Ast_Kind_DynArr_Type: {
                if (elem.actual->kind != Type_Kind_DynArray) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstDynArrType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->DynArray.elem,
                }));
                break;
            }

            case Ast_Kind_VarArg_Type:
                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstVarArgType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = actual,
                }));
                break;

            case Ast_Kind_Function_Type: {
                if (elem.actual->kind != Type_Kind_Function) break;

                AstFunctionType* ft = (AstFunctionType *) elem.type_expr;

                fori (i, 0, (i64) ft->param_count) {
                    bh_arr_push(elem_queue, ((PolySolveElem) {
                        .type_expr = ft->params[i],
                        .kind = PSK_Type,
                        .actual = elem.actual->Function.params[i],
                    }));
                }

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ft->return_type,
                    .kind = PSK_Type,
                    .actual = elem.actual->Function.return_type,
                }));

                break;
            }

            case Ast_Kind_Call: {
                AstPolyCallType *pct = convert_call_to_polycall(context, (AstCall *) elem.type_expr);
                elem.type_expr = (AstType *) pct;

                // fallthrough
            }

            case Ast_Kind_Poly_Call_Type: {
                if (elem.actual->kind != Type_Kind_Struct && elem.actual->kind != Type_Kind_Union) break;

                bh_arr(AstPolySolution) slns;
                if (elem.actual->kind == Type_Kind_Struct) slns = elem.actual->Struct.poly_sln;
                if (elem.actual->kind == Type_Kind_Union)  slns = elem.actual->Union.poly_sln;

                if (bh_arr_length(slns) != bh_arr_length(((AstPolyCallType *) elem.type_expr)->params)) break;

                AstPolyCallType* pt = (AstPolyCallType *) elem.type_expr;

                fori (i, 0, bh_arr_length(pt->params)) {
                    PolySolutionKind kind = slns[i].kind;
                    if (kind == PSK_Type) {
                        bh_arr_push(elem_queue, ((PolySolveElem) {
                            .kind = kind,
                            .type_expr = (AstType *) pt->params[i],
                            .actual = slns[i].type,
                        }));
                    } else {
                        bh_arr_push(elem_queue, ((PolySolveElem) {
                            .kind = kind,
                            .type_expr = (AstType *) pt->params[i],
                            .value = slns[i].value,
                        }));
                    }
                }

                break;
            }

            case Ast_Kind_Type_Compound: {
                if (elem.actual->kind != Type_Kind_Compound) break;
                if (elem.actual->Compound.count != (u32) bh_arr_length(((AstCompoundType *) elem.type_expr)->types)) break;

                AstCompoundType* ct = (AstCompoundType *) elem.type_expr;

                fori (i, 0, bh_arr_length(ct->types)) {
                    bh_arr_push(elem_queue, ((PolySolveElem) {
                        .kind = PSK_Type,
                        .type_expr = ct->types[i],
                        .actual = elem.actual->Compound.types[i],
                    }));
                }

                break;
            }

            default: break;
        }
    }

    bh_arr_free(elem_queue);

    return result;
}

// NOTE: The job of this function is to take a polymorphic parameter and a set of arguments
// and solve for the argument that matches the parameter. This is needed because polymorphic
// procedure resolution has to happen before the named arguments are placed in their correct
// positions.
static AstTyped* lookup_param_in_arguments(AstFunction* func, AstPolyParam* param, Arguments* args, OnyxError* err_msg) {
    bh_arr(AstTyped *) arg_arr = args->values;
    bh_arr(AstNamedValue *) named_values = args->named_values;

    if ((i32) param->idx < 0)
        return NULL;

    // NOTE: This check is safe because currently the arguments given without a name
    // always map to the beginning indicies of the argument array.
    if (param->idx >= (u64) bh_arr_length(arg_arr)) {
        OnyxToken* param_name = func->params[param->idx].local->token;

        bh_arr_each(AstNamedValue *, named_value, named_values) {
            if (token_equals(param_name, (*named_value)->token)) {
                return (AstTyped *) (*named_value)->value;
            }
        }

        // This enables the following case:
        //
        //    f :: (x: i32, $T: type_expr = i32) -> T { ... }
        //    f(10);
        //
        if (param->idx < (u64) bh_arr_length(func->params)) {
            if (func->params[param->idx].default_value) {
                return (AstTyped *) func->params[param->idx].default_value;
            }
        }

        // CLEANUP
        if (err_msg) {
            err_msg->text = "Not enough arguments to polymorphic procedure. This error message may not be entirely right.";
        }

    } else {
        return (AstTyped *) arg_arr[param->idx];
    }

    return NULL;
}

static AstTyped* try_lookup_based_on_partial_function_type(Context *context, AstFunction *pp, AstFunctionType *ft) {
    if (ft->partial_function_type == NULL) {
        AstType *old_return_type = ft->return_type;
        ft->return_type = (AstType *) &context->basic_types.type_void;
        ft->partial_function_type = type_build_from_ast(context, (AstType *) ft);
        ft->return_type = old_return_type;
        if (!ft->partial_function_type) {
            context->polymorph.doing_nested_polymorph_lookup = 1;
            return NULL;
        }

        assert(ft->partial_function_type);
    }

    AstTyped *result = (AstTyped *) polymorphic_proc_lookup(context, pp, PPLM_By_Function_Type, ft->partial_function_type, pp->token);

    // If the result is not ready (NULL, yield flag, no type, or `type_auto_return` as return type), wait.
    if (result && (
            result->type == NULL
        || (result->type->kind == Type_Kind_Function && result->type->Function.return_type == context->types.auto_return)))
    {
        context->polymorph.doing_nested_polymorph_lookup = 1;
        result = NULL;
    }
    if (result == &context->node_that_signals_a_yield) {
        context->polymorph.doing_nested_polymorph_lookup = 1;
        result = NULL;
    }

    return result;
}

// NOTE: The job of this function is to solve for type of AstPolySolution using the provided
// information. It is asssumed that the "param" is of kind PPK_Poly_Type. This function uses
// either the arguments provided, or a function type to compare against to pattern match for
// the type that the parameter but be.
static void solve_for_polymorphic_param_type(Context *context, PolySolveResult* resolved, AstFunction* func, AstPolyParam* param, PolyProcLookupMethod pp_lookup, ptr actual, OnyxError* err_msg) {
    Type* actual_type = NULL;
    b32 can_strip_pointer = 0;

    switch (pp_lookup) {
        case PPLM_By_Arguments: {
            Arguments* args = (Arguments *) actual;

            AstTyped* typed_param = lookup_param_in_arguments(func, param, args, err_msg);
            if (typed_param == NULL) return;

            if (typed_param->kind != Ast_Kind_Argument) goto skip_nested_polymorph_case;

            AstTyped* potential = ((AstArgument *) typed_param)->value;

            if (potential->kind == Ast_Kind_Address_Of) {
                AstAddressOf *aof = (AstAddressOf *) potential;
                if (aof->can_be_removed) {
                    can_strip_pointer = 1;
                }
            }

            if (potential->kind != Ast_Kind_Polymorphic_Proc) goto skip_nested_polymorph_case;
            if (param->idx >= (u32) bh_arr_length(func->params)) goto skip_nested_polymorph_case;

            AstType *param_type = func->params[param->idx].local->type_node;
            if (param_type->kind != Ast_Kind_Function_Type) goto skip_nested_polymorph_case;

            AstFunctionType *ft = (AstFunctionType *) param_type;
            b32 all_types = 1;
            fori (i, 0, (i32) ft->param_count) {
                if (!node_is_type((AstNode *) ft->params[i])) {
                    all_types = 0;
                    break;
                }
            }

            if (all_types)
                typed_param = try_lookup_based_on_partial_function_type(context, (AstFunction *) potential, ft);

          skip_nested_polymorph_case:

            actual_type = query_expression_type(context, typed_param);
            if (actual_type == NULL) {
                if (typed_param) err_msg->pos = typed_param->token->pos;

                err_msg->text = "Unable to resolve type of this expression, needed to solve for polymorphic variable.";
                return;
            }

            break;
        }

        case PPLM_By_Function_Type: {
            Type* ft = (Type *) actual;
            if (param->idx >= ft->Function.param_count) {
                if (err_msg) {
                    err_msg->text = "Incompatible polymorphic argument to function parameter.";
                }
                return;
            }

            actual_type = ft->Function.params[param->idx];
            break;
        }

        default: return;
    }

    PolySolveResult res = solve_poly_type(context, param->poly_sym, param->type_expr, actual_type);

    // If we are trying to match against an "address of" node that was
    // placed because of a method call, there's a small chance that the
    // address of is unnecessary and can be removed. This happens in
    // unify_node_and_type, but because that relies on knowning the type,
    // it cannot happen until after polymorphic arguments are determined.
    // This case simply tries matching everything again, but without the
    // outer most pointer node. If everything succeeds, the pointer node
    // will be removed when the actual value is checked later.
    if (res.kind == PSK_Undefined && can_strip_pointer) {
        res = solve_poly_type(context, param->poly_sym, param->type_expr, actual_type->Pointer.elem);
    }

    if (res.kind == PSK_Undefined) {
        err_msg->pos = param->poly_sym->token->pos;
        err_msg->text = bh_aprintf(context->scratch_alloc,
            "Unable to solve for polymorphic variable '%b', given the type '%s'.",
            param->poly_sym->token->text,
            param->poly_sym->token->length,
            type_get_name(context, actual_type));
    }

    *resolved = res;
}


// NOTE: The job of this function is to look through the arguments provided and find a matching
// value that is to be baked into the polymorphic procedures poly-scope. It expected that param
// will be of kind PPK_Baked_Value. In other words, this handles the ($Baked: type) case.
// CLEANUP: This function is kind of gross at the moment, because it handles different cases for
// the argument kind. When type expressions (type_expr) become first-class types in the type
// system, this code should be able to be a lot cleaner.
static void solve_for_polymorphic_param_value(Context *context, PolySolveResult* resolved, AstFunction* func, AstPolyParam* param, PolyProcLookupMethod pp_lookup, ptr actual, OnyxError* err_msg) {
    if (pp_lookup != PPLM_By_Arguments) {
        err_msg->text = "Function type cannot be used to solved for baked parameter value.";
        return;
    }

    Arguments* args = (Arguments *) actual;
    AstTyped* value = lookup_param_in_arguments(func, param, args, err_msg);
    if (value == NULL) return;

    // HACK: Storing the original value because if this was an AstArgument, we need to flag
    // it as baked if it is determined that the argument is of the correct kind and type.
    AstTyped* orig_value = value;
    if (value->kind == Ast_Kind_Argument) {
        ((AstArgument *) orig_value)->is_baked = 0;
        value = ((AstArgument *) value)->value;
    }

    Type*    param_type = NULL;
    AstType *param_type_expr = func->params[param->idx].local->type_node;
    if (param_type_expr == (AstType *) &context->basic_types.type_type_expr) {
        if (!node_is_type((AstNode *) value)) {
            if (err_msg) {
                err_msg->pos = value->token->pos;
                err_msg->text = bh_aprintf(context->scratch_alloc,
                    "Expected type expression here, got a '%s'.",
                    type_get_name(context, value->type));
            }
            return;
        }

        Type* resolved_type = type_build_from_ast(context, (AstType *) value);
        if (resolved_type == NULL) context->polymorph.flag_to_yield = 1;

        *resolved = ((PolySolveResult) { PSK_Type, .actual = resolved_type });

    } else {
        resolve_expression_type(context, value);

        param_type = type_build_from_ast(context, param_type_expr);
        if (param_type == NULL) {
            context->polymorph.flag_to_yield = 1;
            err_msg->text = "Waiting to know type for polymorphic value.";
            return;
        }

        AstTyped* value_to_use = value;
        if (value->kind == Ast_Kind_Macro) {
            value_to_use = (AstTyped *) get_function_from_node((AstNode *) value);
        }

        TypeMatch tm = unify_node_and_type(context, &value_to_use, param_type);
        if (tm == TYPE_MATCH_FAILED) {
            if (err_msg) {
                err_msg->pos = param->poly_sym->token->pos;
                err_msg->text = bh_aprintf(context->scratch_alloc,
                    "The procedure '%s' expects a value of type '%s' for %d%s baked parameter '%b', got '%s'.",
                    get_function_name(context, func),
                    type_get_name(context, param_type),
                    param->idx + 1,
                    bh_num_suffix(param->idx + 1),
                    param->poly_sym->token->text,
                    param->poly_sym->token->length,
                    node_get_type_name(context, value_to_use));
            }
            return;
        }

        if (tm == TYPE_MATCH_YIELD) context->polymorph.flag_to_yield = 1;

        if ((value_to_use->flags & Ast_Flag_Comptime) == 0) {
            if (err_msg) {
                err_msg->pos = value_to_use->token->pos;
                err_msg->text = "Expected compile-time known argument here.";
            }
            return;
        }

        *resolved = ((PolySolveResult) { PSK_Value, value_to_use });
    }

    if (orig_value->kind == Ast_Kind_Argument) {
        ((AstArgument *) orig_value)->is_baked = 1;
    }
}

TypeMatch find_polymorphic_sln(Context *context, AstPolySolution *out, AstPolyParam *param, AstFunction *func, PolyProcLookupMethod pp_lookup, ptr actual, OnyxError* err_msg) {
    if (err_msg) {
        err_msg->pos = func->token->pos;
        err_msg->rank = Error_Critical;
    }

    // NOTE: Solve for the polymorphic parameter's value
    PolySolveResult resolved = { PSK_Undefined };
    switch (param->kind) {
        case PPK_Poly_Type:   solve_for_polymorphic_param_type (context, &resolved, func, param, pp_lookup, actual, err_msg); break;
        case PPK_Baked_Value: solve_for_polymorphic_param_value(context, &resolved, func, param, pp_lookup, actual, err_msg); break;

        default: if (err_msg) err_msg->text = "Invalid polymorphic parameter kind. This is a compiler bug.";
    }

    if (context->polymorph.doing_nested_polymorph_lookup) {
        context->polymorph.doing_nested_polymorph_lookup = 0;
        return TYPE_MATCH_SPECIAL;
    }

    if (context->polymorph.flag_to_yield) {
        context->polymorph.flag_to_yield = 0;
        return TYPE_MATCH_YIELD;
    }

    switch (resolved.kind) {
        case PSK_Type:
            out->kind = PSK_Type;
            out->poly_sym = param->poly_sym;
            out->type = resolved.actual;
            return TYPE_MATCH_SUCCESS;

        case PSK_Value:
            out->kind = PSK_Value;
            out->poly_sym = param->poly_sym;
            out->value = resolved.value;
            return TYPE_MATCH_SUCCESS;

        case PSK_Undefined:
        default:
            // NOTE: If no error message has been assigned to why this polymorphic parameter
            // resolution was unsuccessful, provide a basic dummy one.
            if (err_msg && err_msg->text == NULL)
                err_msg->text = bh_aprintf(context->scratch_alloc,
                    "Unable to solve for polymorphic variable '%b'.",
                    param->poly_sym->token->text,
                    param->poly_sym->token->length);

            out->kind = PSK_Undefined;
            return TYPE_MATCH_FAILED;
    }
}

// NOTE: The job of this function is to take a polymorphic procedure, as well as a method of
// solving for the polymorphic variables, in order to return an array of the solutions for all
// of the polymorphic variables.
static bh_arr(AstPolySolution) find_polymorphic_slns(Context *context, AstFunction* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxToken *tkn, b32 necessary) {
    ensure_polyproc_cache_is_created(context, pp);
    if (bh_imap_has(&pp->active_queries, (u64) actual)) {
        AstPolyQuery *query = (AstPolyQuery *) bh_imap_get(&pp->active_queries, (u64) actual);
        assert(query->kind == Ast_Kind_Polymorph_Query);
        assert(query->entity);

        if (query->entity->state == Entity_State_Finalized) return query->slns;
        if (query->entity->state == Entity_State_Failed)    return NULL;

        context->polymorph.flag_to_yield = 1;
        return NULL;
    }

    bh_arr(AstPolySolution) slns = NULL;
    bh_arr_new(context->gp_alloc, slns, bh_arr_length(pp->poly_params));

    // NOTE: "known solutions" are given through a '#solidify' directive. If this polymorphic
    // procedure is the result of a partially applied solidification, this array will be non-
    // empty and these solutions will be used.
    bh_arr_each(AstPolySolution, known_sln, pp->known_slns) bh_arr_push(slns, *known_sln);

    AstPolyQuery *query = onyx_ast_node_new(context->ast_alloc, sizeof(AstPolyQuery), Ast_Kind_Polymorph_Query);
    query->token = pp->token;
    query->proc = pp;
    query->pp_lookup = pp_lookup;
    query->given = actual;
    query->error_loc = tkn;
    query->slns = slns;
    query->function_header = clone_function_header(context, pp);
    query->function_header->flags |= Ast_Flag_Header_Check_No_Error;
    query->function_header->scope = NULL;
    query->error_on_fail = necessary;

    bh_imap_put(&pp->active_queries, (u64) actual, (u64) query);
    add_entities_for_node(&context->entities, NULL, (AstNode *) query, NULL, NULL);

    context->polymorph.flag_to_yield = 1;
    return NULL;
}

// NOTE: The job of this function is to be a wrapper to other functions, providing an error
// message if a solution could not be found. This can't be merged with polymorphic_proc_solidify
// because polymorphic_proc_try_solidify uses the aforementioned function.
AstFunction* polymorphic_proc_lookup(Context *context, AstFunction* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxToken* tkn) {

    // Ensure the polymorphic procedure is ready to be solved for.
    assert(pp->entity);
    if (pp->entity->state < Entity_State_Check_Types) return (AstFunction *) &context->node_that_signals_a_yield;

    ensure_polyproc_cache_is_created(context, pp);

    bh_arr(AstPolySolution) slns = find_polymorphic_slns(context, pp, pp_lookup, actual, tkn, 1);
    if (slns == NULL) {
        if (context->polymorph.flag_to_yield) {
            context->polymorph.flag_to_yield = 0;
            return (AstFunction *) &context->node_that_signals_a_yield;
        }

        return NULL;
    }

    AstFunction* result = polymorphic_proc_solidify(context, pp, slns, tkn);
    return result;
}

AstFunction* polymorphic_proc_solidify(Context *context, AstFunction* pp, bh_arr(AstPolySolution) slns, OnyxToken* tkn) {
    ensure_polyproc_cache_is_created(context, pp);

    // NOTE: Check if a version of this polyproc has already been created.
    char* unique_key = build_poly_slns_unique_key(context, slns);
    i32 index = shgeti(pp->concrete_funcs, unique_key);
    if (index != -1) {
        AstSolidifiedFunction solidified_func = pp->concrete_funcs[index].value;

        // NOTE: If this solution was originally created from a "build_only_header" call, then the body
        // will not have been or type checked, or anything. This ensures that the body is copied, the
        // entities are created and entered into the pipeline.
        ensure_solidified_function_has_body(context, pp, &solidified_func);

        // NOTE: Again, if this came from a "build_only_header" call, then there was no known token and
        // the "generated_from" member will be null. It is best to set it here so errors reported in that
        // function can report where the polymorphic instantiation occurred.
        if (solidified_func.func->generated_from == NULL)
            solidified_func.func->generated_from = tkn;

        return solidified_func.func;
    }

    AstSolidifiedFunction solidified_func = generate_solidified_function(context, pp, slns, tkn, 0);
    add_solidified_function_entities(context, &solidified_func);

    // NOTE: Cache the function for later use, reducing duplicate functions.
    shput(pp->concrete_funcs, unique_key, solidified_func);

    if (solidified_func.func->name) {
        solidified_func.func->assembly_name = bh_aprintf(
            context->gp_alloc,
            "%s$%s",
            solidified_func.func->name,
            unique_key
        );
    }

    return (AstFunction *) &context->node_that_signals_a_yield;
}

// NOTE: This can return either a AstFunction or an AstFunction, depending if enough parameters were
// supplied to remove all the polymorphic variables from the function.
AstNode* polymorphic_proc_try_solidify(Context *context, AstFunction* pp, bh_arr(AstPolySolution) slns, OnyxToken* tkn) {
    i32 valid_argument_count = 0;

    bh_arr_each(AstPolySolution, sln, slns) {
        b32 found_match = 0;

        bh_arr_each(AstPolyParam, param, pp->poly_params) {
            if (token_equals(sln->poly_sym->token, param->poly_sym->token)) {
                found_match = 1;
                break;
            }
        }

        if (found_match) {
            valid_argument_count++;
        } else {
            if (pp->name) {
                ONYX_ERROR(tkn->pos, Error_Critical, "'%b' is not a type variable of '%s'.",
                    sln->poly_sym->token->text, sln->poly_sym->token->length, pp->name);
            } else {
                ONYX_ERROR(tkn->pos, Error_Critical, "'%b' is not a type variable of '%b'.",
                    sln->poly_sym->token->text, sln->poly_sym->token->length,
                    pp->token->text, pp->token->length);
            }
            return (AstNode *) pp;
        }
    }

    if (valid_argument_count == bh_arr_length(pp->poly_params)) {
        return (AstNode *) polymorphic_proc_solidify(context, pp, slns, tkn);

    } else {
        // HACK: Some of these initializations assume that the entity for this polyproc has
        // made it through the symbol resolution phase.
        //                                                    - brendanfh 2020/12/25
        AstFunction* new_pp = onyx_ast_node_new(context->ast_alloc, sizeof(AstFunction), Ast_Kind_Polymorphic_Proc);
        memcpy(new_pp, pp, sizeof(AstFunction));
        new_pp->token = tkn;
        new_pp->poly_params = bh_arr_copy(context->ast_alloc, pp->poly_params);

        ensure_polyproc_cache_is_created(context, pp);
        new_pp->concrete_funcs = pp->concrete_funcs;

        new_pp->known_slns = NULL;
        bh_arr_new(context->gp_alloc, new_pp->known_slns, bh_arr_length(pp->known_slns) + bh_arr_length(slns));

        bh_arr_each(AstPolySolution, sln, pp->known_slns) bh_arr_push(new_pp->known_slns, *sln);
        bh_arr_each(AstPolySolution, sln, slns)           bh_arr_push(new_pp->known_slns, *sln);

        return (AstNode *) new_pp;
    }
}

AstFunction* polymorphic_proc_build_only_header(Context *context, AstFunction* pp, PolyProcLookupMethod pp_lookup, ptr actual) {
    ensure_polyproc_cache_is_created(context, pp);
    bh_arr(AstPolySolution) slns = find_polymorphic_slns(context, pp, pp_lookup, actual, NULL, 0);
    if (context->polymorph.flag_to_yield) {
        context->polymorph.flag_to_yield = 0;
        return (AstFunction *) &context->node_that_signals_a_yield;
    }
    if (slns == NULL) return NULL;

    ensure_polyproc_cache_is_created(context, pp);

    return polymorphic_proc_build_only_header_with_slns(context, pp, slns, 0);
}

AstFunction* polymorphic_proc_build_only_header_with_slns(Context *context, AstFunction* pp, bh_arr(AstPolySolution) slns, b32 error_if_failed) {
    AstSolidifiedFunction solidified_func;

    char* unique_key = build_poly_slns_unique_key(context, slns);
    i32 index = shgeti(pp->concrete_funcs, unique_key);
    if (index != -1) {
        solidified_func = pp->concrete_funcs[index].value;

    } else {
        // NOTE: This function is only going to have the header of it correctly created.
        // Nothing should happen to this function's body or else the original will be corrupted.
        //                                                      - brendanfh 2021/01/10
        solidified_func = generate_solidified_function(context, pp, slns, NULL, 1);
    }

    if (solidified_func.func_header_entity) {
        if (solidified_func.func_header_entity->state == Entity_State_Finalized) return solidified_func.func;
        if (solidified_func.func_header_entity->state == Entity_State_Failed)    return NULL;

        return (AstFunction *) &context->node_that_signals_a_yield;
    }

    BH_MASK_SET(solidified_func.func->flags, !error_if_failed, Ast_Flag_Header_Check_No_Error);

    Entity func_header_entity = {
        .state = Entity_State_Check_Types,
        .type = Entity_Type_Temp_Function_Header,
        .function = solidified_func.func,
        .package = NULL,
        .scope = solidified_func.func->poly_scope,
    };

    Entity* func_header_entity_ptr = entity_heap_insert(&context->entities, func_header_entity);
    solidified_func.func_header_entity = func_header_entity_ptr;

    // NOTE: Cache the function for later use.
    shput(pp->concrete_funcs, unique_key, solidified_func);

    return (AstFunction *) &context->node_that_signals_a_yield;
}

typedef struct AutoPolymorphVariable {
    u32 idx;
    u32 variable_count;
    AstType *base_type;
    AstType **replace;
} AutoPolymorphVariable;

// This should be called after all the parameter types have been had symbols resolved, but before anything
// happens to the body.
b32 potentially_convert_function_to_polyproc(Context *context, AstFunction *func) {
    bh_arr(AutoPolymorphVariable) auto_vars = NULL;
    bh_arr_new(context->gp_alloc, auto_vars, 2);

    u32 param_idx = 0;
    bh_arr_each(AstParam, param, func->params) {
        AstType **to_replace = &param->local->type_node;
        AstType *param_type = param->local->type_node;

        b32 done = 0;
        while (!done && param_type) {
            switch (param_type->kind) {
                case Ast_Kind_Pointer_Type:       to_replace = &((AstPointerType *)      *to_replace)->elem;  param_type = ((AstPointerType *) param_type)->elem;  break;
                case Ast_Kind_Multi_Pointer_Type: to_replace = &((AstMultiPointerType *) *to_replace)->elem;  param_type = ((AstMultiPointerType *) param_type)->elem;  break;
                case Ast_Kind_Array_Type:         to_replace = &((AstArrayType *)        *to_replace)->elem;  param_type = ((AstArrayType *)   param_type)->elem;  break;
                case Ast_Kind_Slice_Type:         to_replace = &((AstSliceType *)        *to_replace)->elem;  param_type = ((AstSliceType *)   param_type)->elem;  break;
                case Ast_Kind_DynArr_Type:        to_replace = &((AstDynArrType *)       *to_replace)->elem;  param_type = ((AstDynArrType *)  param_type)->elem;  break;
                case Ast_Kind_Alias:                                                                          param_type = (AstType *) ((AstAlias *) param_type)->alias; break;
                case Ast_Kind_Type_Alias:                                                                     param_type = ((AstTypeAlias *)   param_type)->to;    break;
                case Ast_Kind_Poly_Struct_Type: {
                    AutoPolymorphVariable apv;
                    apv.idx = param_idx;
                    apv.base_type = param->local->type_node;
                    apv.variable_count = bh_arr_length(((AstPolyStructType *) param_type)->poly_params);
                    apv.replace = to_replace;

                    bh_arr_push(auto_vars, apv);
                    done = 1;
                    break;
                }

                case Ast_Kind_Poly_Union_Type: {
                    AutoPolymorphVariable apv;
                    apv.idx = param_idx;
                    apv.base_type = param->local->type_node;
                    apv.variable_count = bh_arr_length(((AstPolyUnionType *) param_type)->poly_params);
                    apv.replace = to_replace;

                    bh_arr_push(auto_vars, apv);
                    done = 1;
                    break;
                }

                default: done = 1; break;
            }
        }

        param_idx++;
    }

    if (bh_arr_length(auto_vars) == 0) return 0;

    bh_arr_new(context->gp_alloc, func->poly_params, bh_arr_length(auto_vars));

    param_idx = 0;
    bh_arr_each(AutoPolymorphVariable, apv, auto_vars) {
        AstPolyParam pp;
        pp.idx = apv->idx;
        pp.kind = PPK_Poly_Type;
        pp.implicit_interface = NULL;

        AstPolyCallType* pcall = onyx_ast_node_new(context->ast_alloc, sizeof(AstPolyCallType), Ast_Kind_Poly_Call_Type);
        pcall->callee = *apv->replace;
        pcall->token = pcall->callee->token;
        pcall->flags |= Ast_Flag_Poly_Call_From_Auto;
        bh_arr_new(context->gp_alloc, pcall->params, apv->variable_count);

        AstType *dealiased_base_type = (AstType *) strip_aliases((AstNode *) apv->base_type);

        if (dealiased_base_type->kind == Ast_Kind_Poly_Struct_Type
            || dealiased_base_type->kind == Ast_Kind_Poly_Union_Type) {
            pp.type_expr = (AstType *) pcall;
        } else {
            pp.type_expr = apv->base_type;
        }
        *apv->replace = (AstType *) pcall;

        fori (i, 0, apv->variable_count) {
            OnyxToken* name_token = bh_alloc_item(context->ast_alloc, OnyxToken);
            name_token->text = bh_aprintf(context->ast_alloc, "__autopoly_var_%d\0", param_idx);
            name_token->length = strlen(name_token->text);
            name_token->type = Token_Type_Symbol;
            name_token->pos  = pcall->token->pos;

            pp.poly_sym = make_symbol(context, name_token);
            pp.poly_sym->flags |= Ast_Flag_Symbol_Is_PolyVar;
            bh_arr_push(pcall->params, pp.poly_sym);
            bh_arr_push(func->poly_params, pp);
            param_idx ++;
        }
    }

    convert_function_to_polyproc(context, func);

    bh_arr_each(AstParam, param, func->params) {
        param->local->flags |= Ast_Flag_Param_Symbol_Dirty;
    }

    return 1;
}

//
// Polymorphic Structures
//
//
// Currently, I am not very happy about how polymorphic structure generation works. My biggest problem
// with it is that it is very different from the polymorhic procedure generation. Also, it needs to
// completely generate and check the structure right away, which means there is a lot of up-front work
// done here that could probably be done elsewhere. This really relates to a large problem in the compiler
// that types need to be known completely by the time symbol resolution is done, even though that
// information shouldn't need to be known until right before the types are checked.
//
// The above documentation is very incorrect but I don't want to fix it right now. Basically, polymorphic
// structures now have a delay instantiation phase and are not forced to be completed immediately.

char* build_poly_struct_name(Context *context, char *name, Type* type) {
    char name_buf[256];
    fori (i, 0, 256) name_buf[i] = 0;


    // Special case for `? T`
    if (type->kind == Type_Kind_Union
        && type->Union.constructed_from == context->builtins.optional_type) {
        strncat(name_buf, "? ", 255);
        strncat(name_buf, type_get_name(context, type->Union.poly_sln[0].type), 255);

        return bh_aprintf(context->gp_alloc, "%s", name_buf);
    }

    bh_arr(AstPolySolution) slns = NULL;
    if (type->kind == Type_Kind_Struct) slns = type->Struct.poly_sln;
    if (type->kind == Type_Kind_Union)  slns = type->Union.poly_sln;


    strncat(name_buf, name, 255);
    strncat(name_buf, "(", 255);
    bh_arr_each(AstPolySolution, ptype, slns) {
        if (ptype != slns)
            strncat(name_buf, ", ", 255);

        // This logic will have to be other places as well.

        switch (ptype->kind) {
            case PSK_Undefined: assert(0); break;
            case PSK_Type:      strncat(name_buf, type_get_name(context, ptype->type), 255); break;
            case PSK_Value: {
                // FIX
                AstNode* value = strip_aliases((AstNode *) ptype->value);

                if (value->kind == Ast_Kind_NumLit) {
                    AstNumLit* nl = (AstNumLit *) value;
                    if (type_is_integer(nl->type)) {
                        strncat(name_buf, bh_bprintf("%l", nl->value.l), 127);
                    } else {
                        strncat(name_buf, "numlit (FIX ME)", 127);
                    }
                } else if (value->kind == Ast_Kind_Code_Block) {
                    AstCodeBlock* code = (AstCodeBlock *) value;
                    OnyxFilePos code_loc = code->token->pos;
                    strncat(name_buf, bh_bprintf("code at %s:%d,%d", code_loc.filename, code_loc.line, code_loc.column), 127);
                } else {
                    strncat(name_buf, "<expr>", 127);
                }

                break;
            }
        }
    }
    strncat(name_buf, ")", 255);

    return bh_aprintf(context->gp_alloc, "%s", name_buf);
}

Type* polymorphic_struct_lookup(Context *context, AstPolyStructType* ps_type, bh_arr(AstPolySolution) slns, OnyxFilePos pos, b32 error_if_failed) {
    if (ps_type->scope == NULL) {
        return NULL;
    }

    assert(!ps_type->base_struct->scope);

    if (ps_type->concrete_structs == NULL) {
        sh_new_arena(ps_type->concrete_structs);
    }

    if (bh_arr_length(slns) != bh_arr_length(ps_type->poly_params)) {
        ONYX_ERROR(pos, Error_Critical, "Wrong number of arguments for '%s'. Expected %d, got %d.",
            ps_type->name,
            bh_arr_length(ps_type->poly_params),
            bh_arr_length(slns));

        return NULL;
    }

    i32 i = 0;
    bh_arr_each(AstPolySolution, sln, slns) {
        sln->poly_sym = (AstNode *) &ps_type->poly_params[i];
        i++;
    }

    char* unique_key = build_poly_slns_unique_key(context, slns);
    i32 index = shgeti(ps_type->concrete_structs, unique_key);
    if (index != -1) {
        AstStructType* concrete_struct = ps_type->concrete_structs[index].value;

        if (concrete_struct->entity_type->state < Entity_State_Check_Types) {
            return NULL;
        }

        if (concrete_struct->entity_type->state == Entity_State_Failed) {
            return (Type *) &context->node_that_signals_failure;
        }

        Type* cs_type = type_build_from_ast(context, (AstType *) concrete_struct);
        if (!cs_type) return NULL;

        cs_type->Struct.constructed_from = (AstType *) ps_type;
        
        if (cs_type->Struct.poly_sln == NULL) cs_type->Struct.poly_sln = bh_arr_copy(context->gp_alloc, slns);
        if (cs_type->Struct.name == NULL)     cs_type->Struct.name = build_poly_struct_name(context, ps_type->name, cs_type);

        return cs_type;
    }

    Scope* sln_scope = scope_create(context, ps_type->scope, ps_type->token->pos);
    insert_poly_slns_into_scope(context, sln_scope, slns);

    AstStructType* concrete_struct = (AstStructType *) ast_clone(context, ps_type->base_struct);
    concrete_struct->scope = scope_create(context, sln_scope, ps_type->token->pos);
    concrete_struct->polymorphic_error_loc = pos;
    BH_MASK_SET(concrete_struct->flags, !error_if_failed, Ast_Flag_Header_Check_No_Error);


    i64 arg_count = bh_arr_length(ps_type->poly_params);
    bh_arr_new(context->gp_alloc, concrete_struct->polymorphic_argument_types, arg_count);
    bh_arr_set_length(concrete_struct->polymorphic_argument_types, arg_count);
    concrete_struct->polymorphic_arguments = bh_arr_copy(context->gp_alloc, slns);

    fori (i, 0, (i64) bh_arr_length(ps_type->poly_params)) {
        concrete_struct->polymorphic_argument_types[i] = (AstType *) ast_clone(context, ps_type->poly_params[i].type_node);
    }

    shput(ps_type->concrete_structs, unique_key, concrete_struct);
    add_entities_for_node(&context->entities, NULL, (AstNode *) concrete_struct, sln_scope, NULL);
    return NULL;
}

Type* polymorphic_union_lookup(Context *context, AstPolyUnionType* pu_type, bh_arr(AstPolySolution) slns, OnyxFilePos pos, b32 error_if_failed) {
    if (pu_type->scope == NULL) {
        return NULL;
    }

    assert(!pu_type->base_union->scope);

    if (pu_type->concrete_unions == NULL) {
        sh_new_arena(pu_type->concrete_unions);
    }

    if (bh_arr_length(slns) != bh_arr_length(pu_type->poly_params)) {
        ONYX_ERROR(pos, Error_Critical, "Wrong number of arguments for '%s'. Expected %d, got %d.",
            pu_type->name,
            bh_arr_length(pu_type->poly_params),
            bh_arr_length(slns));

        return NULL;
    }

    i32 i = 0;
    bh_arr_each(AstPolySolution, sln, slns) {
        sln->poly_sym = (AstNode *) &pu_type->poly_params[i];
        i++;
    }

    char* unique_key = build_poly_slns_unique_key(context, slns);
    i32 index = shgeti(pu_type->concrete_unions, unique_key);
    if (index != -1) {
        AstUnionType* concrete_union = pu_type->concrete_unions[index].value;

        if (concrete_union->entity->state < Entity_State_Check_Types) {
            return NULL;
        }

        if (concrete_union->entity->state == Entity_State_Failed) {
            return (Type *) &context->node_that_signals_failure;
        }

        Type* cu_type = type_build_from_ast(context, (AstType *) concrete_union);
        if (!cu_type) return NULL;

        cu_type->Union.constructed_from = (AstType *) pu_type;
        
        if (cu_type->Union.poly_sln == NULL) cu_type->Union.poly_sln = bh_arr_copy(context->gp_alloc, slns);
        cu_type->Union.name = build_poly_struct_name(context, pu_type->name, cu_type);

        return cu_type;
    }

    Scope* sln_scope = scope_create(context, pu_type->scope, pu_type->token->pos);
    insert_poly_slns_into_scope(context, sln_scope, slns);

    AstUnionType* concrete_union = (AstUnionType *) ast_clone(context, pu_type->base_union);
    concrete_union->scope = scope_create(context, sln_scope, pu_type->token->pos);
    concrete_union->polymorphic_error_loc = pos;
    BH_MASK_SET(concrete_union->flags, !error_if_failed, Ast_Flag_Header_Check_No_Error);

    i64 arg_count = bh_arr_length(pu_type->poly_params);
    bh_arr_new(context->gp_alloc, concrete_union->polymorphic_argument_types, arg_count);
    bh_arr_set_length(concrete_union->polymorphic_argument_types, arg_count);
    concrete_union->polymorphic_arguments = bh_arr_copy(context->gp_alloc, slns);
    concrete_union->name = pu_type->name;

    fori (i, 0, (i64) bh_arr_length(pu_type->poly_params)) {
        concrete_union->polymorphic_argument_types[i] = (AstType *) ast_clone(context, pu_type->poly_params[i].type_node);
    }

    shput(pu_type->concrete_unions, unique_key, concrete_union);
    add_entities_for_node(&context->entities, NULL, (AstNode *) concrete_union, sln_scope, NULL);
    return NULL;
}
