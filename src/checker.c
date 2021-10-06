#define BH_DEBUG
#include "parser.h"
#include "utils.h"

// All of the `check` functions return a boolean that signals if an issue
// was reached while processing the node. These error booleans propagate
// up the call stack until they reach `check_entity`.

#define CHECK(kind, ...) do { \
    CheckStatus cs = check_ ## kind (__VA_ARGS__); \
    if (cs > Check_Errors_Start) return cs; \
    } while (0)

#define YIELD(loc, msg) do { \
    if (context.cycle_detected) { \
        onyx_report_error(loc, msg); \
        return Check_Error; \
    } else { \
        return Check_Yield_Macro; \
    } \
    } while (0)

#define YIELD_(loc, msg, ...) do { \
    if (context.cycle_detected) { \
        onyx_report_error(loc, msg, __VA_ARGS__); \
        return Check_Error; \
    } else { \
        return Check_Yield_Macro; \
    } \
    } while (0)

#define ERROR(loc, msg) do { \
    onyx_report_error(loc, msg); \
    return Check_Error; \
    } while (0)

#define ERROR_(loc, msg, ...) do { \
    onyx_report_error(loc, msg, __VA_ARGS__); \
    return Check_Error; \
    } while (0)

typedef enum CheckStatus {
    Check_Success,  // The node was successfully checked with out errors
    Check_Complete, // The node is done processing

    Check_Errors_Start,
    Check_Return_To_Symres, // Return this node for further symres processing
    Check_Yield_Macro,
    Check_Error,    // There was an error when checking the node
} CheckStatus;

CheckStatus check_block(AstBlock* block);
CheckStatus check_statement_chain(AstNode** start);
CheckStatus check_statement(AstNode** pstmt);
CheckStatus check_return(AstReturn* retnode);
CheckStatus check_if(AstIfWhile* ifnode);
CheckStatus check_while(AstIfWhile* whilenode);
CheckStatus check_for(AstFor* fornode);
CheckStatus check_switch(AstSwitch* switchnode);
CheckStatus check_call(AstCall** pcall);
CheckStatus check_binaryop(AstBinaryOp** pbinop);
CheckStatus check_unaryop(AstUnaryOp** punop);
CheckStatus check_struct_literal(AstStructLiteral* sl);
CheckStatus check_array_literal(AstArrayLiteral* al);
CheckStatus check_range_literal(AstRangeLiteral** range);
CheckStatus check_compound(AstCompound* compound);
CheckStatus check_if_expression(AstIfExpression* if_expr);
CheckStatus check_expression(AstTyped** expr);
CheckStatus check_address_of(AstAddressOf* aof);
CheckStatus check_dereference(AstDereference* deref);
CheckStatus check_subscript(AstSubscript** paa);
CheckStatus check_field_access(AstFieldAccess** pfield);
CheckStatus check_method_call(AstBinaryOp** mcall);
CheckStatus check_size_of(AstSizeOf* so);
CheckStatus check_align_of(AstAlignOf* ao);
CheckStatus check_global(AstGlobal* global);
CheckStatus check_function(AstFunction* func);
CheckStatus check_overloaded_function(AstOverloadedFunction* func);
CheckStatus check_struct(AstStructType* s_node);
CheckStatus check_function_header(AstFunction* func);
CheckStatus check_memres_type(AstMemRes* memres);
CheckStatus check_memres(AstMemRes* memres);
CheckStatus check_type(AstType* type);
CheckStatus check_insert_directive(AstDirectiveInsert** pinsert);
CheckStatus check_do_block(AstDoBlock** pdoblock);

// HACK HACK HACK
b32 expression_types_must_be_known = 0;

#define STATEMENT_LEVEL 1
#define EXPRESSION_LEVEL 2
u32 current_checking_level=0;

static inline void fill_in_type(AstTyped* node) {
    if (node->type == NULL) {
        if (check_type(node->type_node) > Check_Errors_Start) return;

        node->type = type_build_from_ast(context.ast_alloc, node->type_node);
    }
}

// HACK: This should be baked into a structure, not a global variable.
static Type** expected_return_type = NULL;

CheckStatus check_return(AstReturn* retnode) {
    if (retnode->expr) {
        CHECK(expression, &retnode->expr);

        if (*expected_return_type == &type_auto_return) {
            resolve_expression_type(retnode->expr);
            if (retnode->expr->type == NULL)
                YIELD(retnode->token->pos, "Trying to determine automatic return type.");

            *expected_return_type = retnode->expr->type;
            return Check_Success;
        }

        if (!unify_node_and_type(&retnode->expr, *expected_return_type)) {
            ERROR_(retnode->token->pos,
                    "Expected to return a value of type '%s', returning value of type '%s'.",
                    type_get_name(*expected_return_type),
                    node_get_type_name(retnode->expr));
        }

    } else {
        if (*expected_return_type == &type_auto_return) {
            *expected_return_type = &basic_types[Basic_Kind_Void];
            return Check_Success;
        }

        if ((*expected_return_type)->Basic.size > 0) {
            ERROR_(retnode->token->pos,
                "Returning from non-void function without a value. Expected a value of type '%s'.",
                type_get_name(*expected_return_type));
        }
    }

    return Check_Success;
}

CheckStatus check_if(AstIfWhile* ifnode) {
    if (ifnode->initialization != NULL) CHECK(statement_chain, &ifnode->initialization);

    if (ifnode->kind == Ast_Kind_Static_If) {
        if ((ifnode->flags & Ast_Flag_Static_If_Resolved) == 0) {
            YIELD(ifnode->token->pos, "Waiting for static if to be resolved.");
        }

        if (static_if_resolution(ifnode)) {
            if (ifnode->true_stmt != NULL) CHECK(statement, (AstNode **) &ifnode->true_stmt);

        } else {
            if (ifnode->false_stmt != NULL) CHECK(statement, (AstNode **) &ifnode->false_stmt);
        }

    } else {
        CHECK(expression, &ifnode->cond);

        if (!type_is_bool(ifnode->cond->type)) {
            ERROR_(ifnode->cond->token->pos, "Expected expression of type 'bool' for condition, got '%s'", type_get_name(ifnode->cond->type));
        }

        if (ifnode->true_stmt)  CHECK(statement, (AstNode **) &ifnode->true_stmt);
        if (ifnode->false_stmt) CHECK(statement, (AstNode **) &ifnode->false_stmt);
    }

    return Check_Success;
}

CheckStatus check_while(AstIfWhile* whilenode) {
    if (whilenode->initialization != NULL) CHECK(statement_chain, &whilenode->initialization);

    CHECK(expression, &whilenode->cond);

    if (!type_is_bool(whilenode->cond->type)) {
        ERROR_(whilenode->cond->token->pos, "Expected expression of type 'bool' for condition, got '%s'", type_get_name(whilenode->cond->type));
    }

    if (whilenode->true_stmt)  CHECK(statement, (AstNode **) &whilenode->true_stmt);
    if (whilenode->false_stmt) CHECK(statement, (AstNode **) &whilenode->false_stmt);

    return Check_Success;
}

CheckStatus check_for(AstFor* fornode) {
    CHECK(expression, &fornode->iter);
    resolve_expression_type(fornode->iter);

    Type* iter_type = fornode->iter->type;
    if (iter_type == NULL) YIELD(fornode->token->pos, "Waiting for iteration expression type to be known.");

    fornode->loop_type = For_Loop_Invalid;
    if (types_are_compatible(iter_type, &basic_types[Basic_Kind_I32])) {
        if (fornode->by_pointer) {
            ERROR(fornode->var->token->pos, "Cannot iterate by pointer over a range.");
        }

        AstNumLit* low_0    = make_int_literal(context.ast_alloc, 0);
        AstRangeLiteral* rl = make_range_literal(context.ast_alloc, (AstTyped *) low_0, fornode->iter);
        CHECK(range_literal, &rl);
        fornode->iter = (AstTyped *) rl;

        fornode->var->type = builtin_range_type_type->Struct.memarr[0]->type;
        fornode->var->flags |= Ast_Flag_Cannot_Take_Addr;
        fornode->loop_type = For_Loop_Range;
    }
    else if (types_are_compatible(iter_type, builtin_range_type_type)) {
        if (fornode->by_pointer) {
            ERROR(fornode->var->token->pos, "Cannot iterate by pointer over a range.");
        }

        // NOTE: Blindly copy the first range member's type which will
        // be the low value.                - brendanfh 2020/09/04
        fornode->var->type = builtin_range_type_type->Struct.memarr[0]->type;
        fornode->var->flags |= Ast_Flag_Cannot_Take_Addr;
        fornode->loop_type = For_Loop_Range;

    }
    else if (iter_type->kind == Type_Kind_Array) {
        if (fornode->by_pointer) fornode->var->type = type_make_pointer(context.ast_alloc, iter_type->Array.elem);
        else                     fornode->var->type = iter_type->Array.elem;

        fornode->loop_type = For_Loop_Array;
    }
    else if (iter_type->kind == Type_Kind_Slice) {
        if (fornode->by_pointer) fornode->var->type = type_make_pointer(context.ast_alloc, iter_type->Slice.elem);
        else                     fornode->var->type = iter_type->Slice.elem;

        fornode->loop_type = For_Loop_Slice;

    }
    else if (iter_type->kind == Type_Kind_VarArgs) {
        if (fornode->by_pointer) {
            ERROR_(fornode->var->token->pos, "Cannot iterate by pointer over '%s'.", type_get_name(iter_type));
        }

        fornode->var->type = iter_type->VarArgs.elem;

        // NOTE: Slices are VarArgs are being treated the same here.
        fornode->loop_type = For_Loop_Slice;
    }
    else if (iter_type->kind == Type_Kind_DynArray) {
        if (fornode->by_pointer) fornode->var->type = type_make_pointer(context.ast_alloc, iter_type->DynArray.elem);
        else                     fornode->var->type = iter_type->DynArray.elem;

        fornode->loop_type = For_Loop_DynArr;
    }
    else if (type_struct_constructed_from_poly_struct(iter_type, builtin_iterator_type)) {
        if (fornode->by_pointer) {
            ERROR(fornode->var->token->pos, "Cannot iterate by pointer over an iterator.");
        }

        // HACK: This assumes the Iterator type only has a single type argument.
        fornode->var->type = iter_type->Struct.poly_sln[0].type;
        fornode->loop_type = For_Loop_Iterator;
    }

    if (fornode->by_pointer)
        fornode->var->flags |= Ast_Flag_Cannot_Take_Addr;

    if (fornode->loop_type == For_Loop_Invalid) {
        ERROR_(fornode->iter->token->pos,
                "Cannot iterate over a '%s'.",
                type_get_name(iter_type));
    }

    CHECK(block, fornode->stmt);

    return Check_Success;
}

static b32 add_case_to_switch_statement(AstSwitch* switchnode, u64 case_value, AstBlock* block, OnyxFilePos pos) {
    switchnode->min_case = bh_min(switchnode->min_case, case_value);
    switchnode->max_case = bh_max(switchnode->max_case, case_value);

    if (bh_imap_has(&switchnode->case_map, case_value)) {
        onyx_report_error(pos, "Multiple cases for values '%d'.", case_value);
        return 1;
    }

    bh_imap_put(&switchnode->case_map, case_value, (u64) block);
    return 0;
}

CheckStatus check_switch(AstSwitch* switchnode) {
    if (switchnode->initialization != NULL) CHECK(statement_chain, &switchnode->initialization);

    CHECK(expression, &switchnode->expr);
    Type* resolved_expr_type = resolve_expression_type(switchnode->expr);
    if (!type_is_integer(switchnode->expr->type) && switchnode->expr->type->kind != Type_Kind_Enum) {
        ERROR(switchnode->expr->token->pos, "expected integer or enum type for switch expression");
    }

    // LEAK if this has to be yielded
    bh_imap_init(&switchnode->case_map, global_heap_allocator, bh_arr_length(switchnode->cases) * 2);

    switchnode->min_case = 0xffffffffffffffff;

    // Umm, this doesn't check the type of the case expression to the type of the expression
    bh_arr_each(AstSwitchCase, sc, switchnode->cases) {
        CHECK(block, sc->block);

        bh_arr_each(AstTyped *, value, sc->values) {
            CHECK(expression, value);

            if ((*value)->kind == Ast_Kind_Range_Literal) {
                AstRangeLiteral* rl = (AstRangeLiteral *) (*value);
                resolve_expression_type(rl->low);
                resolve_expression_type(rl->high);

                if (rl->low->kind != Ast_Kind_NumLit || rl->high->kind != Ast_Kind_NumLit) {
                    ERROR(rl->token->pos, "case statement expected compile time known range.");
                }

                promote_numlit_to_larger((AstNumLit *) rl->low);
                promote_numlit_to_larger((AstNumLit *) rl->high);

                i64 lower = ((AstNumLit *) rl->low)->value.l;
                i64 upper = ((AstNumLit *) rl->high)->value.l;

                // NOTE: This is inclusive!!!!
                fori (case_value, lower, upper + 1) {
                    if (add_case_to_switch_statement(switchnode, case_value, sc->block, rl->token->pos))
                        return Check_Error;
                }

                continue;
            }

            if (!unify_node_and_type(value, resolved_expr_type)) {
                OnyxToken* tkn = sc->block->token;
                if ((*value)->token) tkn = (*value)->token;

                ERROR_(tkn->pos, "Mismatched types in switch-case. Expected '%s', got '%s'.",
                    type_get_name(resolved_expr_type), type_get_name((*value)->type));
            }

            if (node_is_type((AstNode*) (*value))) {
                Type* type = type_build_from_ast(context.ast_alloc, (AstType*) (*value));

                if (add_case_to_switch_statement(switchnode, type->id, sc->block, sc->block->token->pos))
                    return Check_Error;

                continue;
            }

            if ((*value)->kind == Ast_Kind_Enum_Value) {
                (*value) = (AstTyped *) ((AstEnumValue *) (*value))->value;
            }

            if ((*value)->kind != Ast_Kind_NumLit) {
                ERROR((*value)->token->pos, "case statement expected compile time known integer");
            }

            resolve_expression_type((*value));
            // promote_numlit_to_larger((AstNumLit *) (*value));

            if (add_case_to_switch_statement(switchnode, ((AstNumLit *) (*value))->value.l, sc->block, sc->block->token->pos))
                return Check_Error;
        }
    }

    if (switchnode->default_case)
        CHECK(block, switchnode->default_case);

    return 0;
}

CheckStatus check_arguments(Arguments* args) {
    bh_arr_each(AstTyped *, actual, args->values)
        CHECK(expression, actual);

    bh_arr_each(AstNamedValue *, named_value, args->named_values)
        CHECK(expression, &(*named_value)->value);

    return Check_Success;
}

CheckStatus check_argument(AstArgument** parg) {
    CHECK(expression, &(*parg)->value);
    (*parg)->type = (*parg)->value->type;

    return Check_Success;
}

static CheckStatus check_resolve_callee(AstCall* call, AstTyped** effective_callee) {
    if (call->kind == Ast_Kind_Intrinsic_Call) return Check_Success;

    call->callee = (AstTyped *) strip_aliases((AstNode *) call->callee);

    AstTyped* callee = call->callee;
    b32 calling_a_macro = 0;

    if (callee->kind == Ast_Kind_Overloaded_Function) {
        b32 should_yield = 0;
        AstTyped* new_callee = find_matching_overload_by_arguments(
            ((AstOverloadedFunction *) callee)->overloads,
            &call->args,
            &should_yield);

        if (new_callee == NULL) {
            if (callee->entity->state > Entity_State_Check_Types && !should_yield) {
                report_unable_to_match_overload(call);
                return Check_Error;

            } else {
                YIELD(call->token->pos, "Waiting for overloaded function option to pass type-checking.");
            }
        }

        callee = new_callee;
    }

    if (callee->kind == Ast_Kind_Macro) {
        calling_a_macro = 1;
        call->callee = callee;

        AstTyped* new_callee = (AstTyped *) macro_resolve_header((AstMacro *) callee, &call->args, call->token);
        if (new_callee == NULL) return Check_Error;
        if (new_callee == (AstTyped *) &node_that_signals_a_yield) {
            YIELD(call->token->pos, "Waiting for macro header to pass type-checking.");
        }

        arguments_remove_baked(&call->args);
        callee = new_callee;

    } else if (callee->kind == Ast_Kind_Polymorphic_Proc) {
        AstTyped* new_callee = (AstTyped *) polymorphic_proc_lookup((AstPolyProc *) callee, PPLM_By_Arguments, &call->args, call->token);
        if (new_callee == NULL) return Check_Error;
        if (new_callee == (AstTyped *) &node_that_signals_a_yield) {
            YIELD(call->token->pos, "Waiting for polymorphic procedure header to pass type-checking.");
        }

        arguments_remove_baked(&call->args);
        callee = new_callee;
    }

    if (!calling_a_macro) call->callee = callee;

    // NOTE: Build callee's type
    fill_in_type((AstTyped *) callee);
    if (callee->type == NULL) {
        YIELD(call->token->pos, "Trying to resolve function type for callee.");
    }

    if (callee->type->kind != Type_Kind_Function) {
        ERROR_(call->token->pos,
                "Attempting to call something that is not a function, '%b'.",
                callee->token->text, callee->token->length);
    }

    *effective_callee = callee;
    return Check_Success;
}

CheckStatus check_call(AstCall** pcall) {
    // All the things that need to be done when checking a call node.
    //      1. Ensure the callee is not a symbol
    //      2. Check the callee expression (since it could be a variable or a field access, etc)
    //      3. Check all arguments
    //          * Cannot pass overloaded functions (ROBUSTNESS)
    //      4. If callee is an overloaded function, use the argument types to determine which overload is used.
    //      5. If callee is polymorphic, use the arguments type to generate a polymorphic function.
    //      7. Fill in arguments
    //      8. If callee is an intrinsic, turn call into an Intrinsic_Call node
    //      9. Check types of formal and actual params against each other, handling varargs
    AstCall* call = *pcall;
    
    if (call->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    u32 current_checking_level_store = current_checking_level;
    CHECK(expression, &call->callee);
    CHECK(arguments, &call->args);
    current_checking_level = current_checking_level_store;

    // SPEED CLEANUP: Keeping an original copy for basically no reason except that sometimes you
    // need to know the baked argument values in code generation.
    // This should only be done once, but right now it is being done everytime this is checked,
    // which can be multiple if we have to yield on a callee's type.
    arguments_clone(&call->original_args, &call->args);

    AstFunction* callee=NULL;
    CHECK(resolve_callee, call, (AstTyped **) &callee);

    i32 arg_count = get_argument_buffer_size(&callee->type->Function, &call->args);
    arguments_ensure_length(&call->args, arg_count);

    char* err_msg = NULL;
    fill_in_arguments(&call->args, (AstNode *) callee, &err_msg);
    if (err_msg != NULL) ERROR(call->token->pos, err_msg);

    bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values;
    bh_arr_each(AstArgument *, arg, arg_arr) {
        if (*arg != NULL) continue;

        ERROR(call->token->pos, "Not all arguments were given a value.");
    }

    // HACK HACK HACK
    // :CallSiteIsGross
    bh_arr_each(AstArgument *, arg, arg_arr) {
        AstTyped** arg_value = &(*arg)->value;

        if ((*arg_value)->kind == Ast_Kind_Call_Site) {
            AstCallSite* callsite = (AstCallSite *) ast_clone(context.ast_alloc, *arg_value);
            callsite->callsite_token = call->token;

            // HACK CLEANUP
            OnyxToken* str_token = bh_alloc(context.ast_alloc, sizeof(OnyxToken));
            str_token->text  = bh_strdup(global_heap_allocator, (char *) call->token->pos.filename);
            str_token->length = strlen(call->token->pos.filename);
            str_token->pos = call->token->pos;
            str_token->type = Token_Type_Literal_String;

            AstStrLit* filename = bh_alloc_item(context.ast_alloc, AstStrLit);
            memset(filename, 0, sizeof(AstStrLit));
            filename->kind  = Ast_Kind_StrLit;
            filename->token = str_token;
            filename->addr  = 0;

            add_entities_for_node(NULL, (AstNode *) filename, NULL, NULL);
            callsite->filename = filename;

            callsite->line   = make_int_literal(context.ast_alloc, call->token->pos.line);
            callsite->column = make_int_literal(context.ast_alloc, call->token->pos.column);

            convert_numlit_to_type(callsite->line,   &basic_types[Basic_Kind_U32]);
            convert_numlit_to_type(callsite->column, &basic_types[Basic_Kind_U32]);

            *arg_value = (AstTyped *) callsite;
        }
    }

    // NOTE: If we are calling an intrinsic function, translate the
    // call into an intrinsic call node.
    if (callee->flags & Ast_Flag_Intrinsic) {
        call->kind = Ast_Kind_Intrinsic_Call;
        call->callee = NULL;

        token_toggle_end(callee->intrinsic_name);
        char* intr_name = callee->intrinsic_name->text;

        if (bh_table_has(OnyxIntrinsic, intrinsic_table, intr_name)) {
            call->intrinsic = bh_table_get(OnyxIntrinsic, intrinsic_table, intr_name);

        } else {
            onyx_report_error(callee->token->pos, "Intrinsic not supported, '%s'.", intr_name);
            token_toggle_end(callee->intrinsic_name);
            return Check_Error;
        }

        token_toggle_end(callee->intrinsic_name);
    }

    call->va_kind = VA_Kind_Not_VA;
    call->type = callee->type->Function.return_type;
    if (call->type == &type_auto_return && call->callee->kind != Ast_Kind_Macro) {
        YIELD(call->token->pos, "Waiting for auto-return type to be solved.");
    }

    OnyxError error;
    if (!check_arguments_against_type(&call->args, &callee->type->Function, &call->va_kind,
            call->token, get_function_name(callee), &error)) {
        onyx_submit_error(error);
        return Check_Error;
    }

    call->flags   |= Ast_Flag_Has_Been_Checked;
    callee->flags |= Ast_Flag_Function_Used;

    if (call->kind == Ast_Kind_Call && call->callee->kind == Ast_Kind_Macro) {
        expand_macro(pcall, callee);
        return Check_Return_To_Symres;
    }

    return Check_Success;
}

static void report_bad_binaryop(AstBinaryOp* binop) {
    onyx_report_error(binop->token->pos, "Binary operator '%s' not understood for arguments of type '%s' and '%s'.",
            binaryop_string[binop->operation],
            node_get_type_name(binop->left),
            node_get_type_name(binop->right));
}

static AstCall* binaryop_try_operator_overload(AstBinaryOp* binop) {
    if (bh_arr_length(operator_overloads[binop->operation]) == 0) return NULL;

    Arguments args = ((Arguments) { NULL, NULL });
    bh_arr_new(global_heap_allocator, args.values, 2);
    bh_arr_push(args.values, (AstTyped *) make_argument(context.ast_alloc, binop->left));
    bh_arr_push(args.values, (AstTyped *) make_argument(context.ast_alloc, binop->right));

    u32 current_checking_level_store = current_checking_level;
    check_argument((AstArgument **) &args.values[0]);
    check_argument((AstArgument **) &args.values[1]);
    current_checking_level = current_checking_level_store;

    if (binop_is_assignment(binop->operation)) {
        args.values[0] = (AstTyped *) make_address_of(context.ast_alloc, binop->left);

        u32 current_checking_level_store = current_checking_level;
        CheckStatus cs = check_address_of((AstAddressOf *) args.values[0]);
        current_checking_level = current_checking_level_store;

        if (cs == Check_Yield_Macro) return (AstCall *) &node_that_signals_a_yield;
        if (cs == Check_Error) {
            return NULL;
        }

        args.values[0] = (AstTyped *) make_argument(context.ast_alloc, args.values[0]);
        current_checking_level_store = current_checking_level;
        check_argument((AstArgument **) &args.values[0]);
        current_checking_level = current_checking_level_store;
    }

    b32 should_yield = 0;
    AstTyped* overload = find_matching_overload_by_arguments(operator_overloads[binop->operation], &args, &should_yield);
    if (should_yield) {
        bh_arr_free(args.values);
        return (AstCall *) &node_that_signals_a_yield;
    }

    if (overload == NULL) {
        bh_arr_free(args.values);
        return NULL;
    }

    AstCall* implicit_call = onyx_ast_node_new(context.ast_alloc, sizeof(AstCall), Ast_Kind_Call);
    implicit_call->token = binop->token;
    implicit_call->callee = overload;
    implicit_call->va_kind = VA_Kind_Not_VA;

    implicit_call->args = args;
    return implicit_call;
}


CheckStatus check_binaryop_assignment(AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;
    if (current_checking_level == EXPRESSION_LEVEL)
        ERROR(binop->token->pos, "Assignment not valid in expression.");

    if (!is_lval((AstNode *) binop->left))
        ERROR_(binop->left->token->pos,
                "Cannot assign to '%b'.",
                binop->left->token->text, binop->left->token->length);

    if ((binop->left->flags & Ast_Flag_Const) != 0 && binop->left->type != NULL)
        ERROR_(binop->token->pos,
                "Cannot assign to constant '%b.'.",
                binop->left->token->text, binop->left->token->length);

    if (binop->operation == Binary_Op_Assign) {
        // NOTE: Raw assignment

        // NOTE: This is the 'type inference' system. Very stupid, but very easy.
        // If a left operand has an unknown type, fill it in with the type of
        // the right hand side.
        if (binop->left->type == NULL) {
            resolve_expression_type(binop->right);

            Type* right_type = get_expression_type(binop->right);
            if (right_type == NULL) {
                if (binop->right->entity == NULL || binop->right->entity->state > Entity_State_Check_Types) {
                    ERROR(binop->token->pos, "Could not resolve type of right hand side to infer.");

                } else {
                    YIELD(binop->token->pos, "Trying to resolve try of right hand side.");
                }
            }

            if (right_type->kind == Type_Kind_Compound) {
                AstCompound* lhs = (AstCompound *) binop->left;
                if (lhs->kind != Ast_Kind_Compound) {
                    ERROR_(binop->token->pos,
                            "Expected left hand side to have %d expressions.",
                            binop->right->type->Compound.count);
                }

                i32 expr_count = right_type->Compound.count;
                if (bh_arr_length(lhs->exprs) != expr_count) {
                    ERROR_(binop->token->pos,
                            "Expected left hand side to have %d expressions.",
                            binop->right->type->Compound.count);
                }

                fori (i, 0, expr_count) {
                    lhs->exprs[i]->type = right_type->Compound.types[i];
                }

                lhs->type = type_build_compound_type(context.ast_alloc, lhs);

            } else {
                binop->left->type = right_type;
            }
        }

    } else {
        // NOTE: +=, -=, ...

        BinaryOp operation = -1;
        if      (binop->operation == Binary_Op_Assign_Add)      operation = Binary_Op_Add;
        else if (binop->operation == Binary_Op_Assign_Minus)    operation = Binary_Op_Minus;
        else if (binop->operation == Binary_Op_Assign_Multiply) operation = Binary_Op_Multiply;
        else if (binop->operation == Binary_Op_Assign_Divide)   operation = Binary_Op_Divide;
        else if (binop->operation == Binary_Op_Assign_Modulus)  operation = Binary_Op_Modulus;
        else if (binop->operation == Binary_Op_Assign_And)      operation = Binary_Op_And;
        else if (binop->operation == Binary_Op_Assign_Or)       operation = Binary_Op_Or;
        else if (binop->operation == Binary_Op_Assign_Xor)      operation = Binary_Op_Xor;
        else if (binop->operation == Binary_Op_Assign_Shl)      operation = Binary_Op_Shl;
        else if (binop->operation == Binary_Op_Assign_Shr)      operation = Binary_Op_Shr;
        else if (binop->operation == Binary_Op_Assign_Sar)      operation = Binary_Op_Sar;

        AstBinaryOp* new_right = make_binary_op(context.ast_alloc, operation, binop->left, binop->right);
        binop->right = (AstTyped *) new_right;
        new_right->token = binop->token;
        binop->operation = Binary_Op_Assign;

        CHECK(binaryop, (AstBinaryOp **) &binop->right);
    }

    if (binop->right->type == NULL) {
        if (binop->right->entity != NULL && binop->right->entity->state <= Entity_State_Check_Types) {
            YIELD(binop->token->pos, "Trying to resolve type of right hand side.");
        }
    }

    if (!unify_node_and_type(&binop->right, binop->left->type)) {
        ERROR_(binop->token->pos,
                "Cannot assign value of type '%s' to a '%s'.",
                node_get_type_name(binop->right),
                node_get_type_name(binop->left));
    }

    binop->type = &basic_types[Basic_Kind_Void];

    return Check_Success;
}

static b32 binary_op_is_allowed(BinaryOp operation, Type* type) {
    static const u8 binop_allowed[Binary_Op_Count] = {
        /* Add */             Basic_Flag_Numeric | Basic_Flag_Pointer,
        /* Minus */           Basic_Flag_Numeric | Basic_Flag_Pointer,
        /* Multiply */        Basic_Flag_Numeric,
        /* Divide */          Basic_Flag_Numeric,
        /* Modulus */         Basic_Flag_Integer,

        /* Equal */           Basic_Flag_Equality,
        /* Not_Equal */       Basic_Flag_Equality,
        /* Less */            Basic_Flag_Ordered,
        /* Less_Equal */      Basic_Flag_Ordered,
        /* Greater */         Basic_Flag_Ordered,
        /* Greater_Equal */   Basic_Flag_Ordered,

        /* And */             Basic_Flag_Integer,
        /* Or */              Basic_Flag_Integer,
        /* Xor */             Basic_Flag_Integer,
        /* Shl */             Basic_Flag_Integer,
        /* Shr */             Basic_Flag_Integer,
        /* Sar */             Basic_Flag_Integer,

        /* Bool_And */        Basic_Flag_Boolean,
        /* Bool_Or */         Basic_Flag_Boolean,

        /* Assign_Start */    0,
        /* Assign */          0,
        /* Assign_Add */      0,
        /* Assign_Minus */    0,
        /* Assign_Multiply */ 0,
        /* Assign_Divide */   0,
        /* Assign_Modulus */  0,
        /* Assign_And */      0,
        /* Assign_Or */       0,
        /* Assign_Xor */      0,
        /* Assign_Shl */      0,
        /* Assign_Shr */      0,
        /* Assign_Sar */      0,
        /* Assign_End */      0,

        /* Pipe */            0,
        /* Range */           0,
    };

    enum BasicFlag effective_flags = 0;
    switch (type->kind) {
        case Type_Kind_Basic:    effective_flags = type->Basic.flags;  break;
        case Type_Kind_Pointer:  effective_flags = Basic_Flag_Pointer; break;
        case Type_Kind_Enum:     effective_flags = Basic_Flag_Integer; break;
        case Type_Kind_Function: effective_flags = Basic_Flag_Equality; break;
    }

    return (binop_allowed[operation] & effective_flags) != 0;
}

CheckStatus check_binaryop_compare(AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    // HACK: Since ^... to rawptr is a one way conversion, strip any pointers
    // away so they can be compared as expected
    Type* ltype = binop->left->type;
    Type* rtype = binop->right->type;

    if (ltype->kind == Type_Kind_Pointer) ltype = &basic_types[Basic_Kind_Rawptr];
    if (rtype->kind == Type_Kind_Pointer) rtype = &basic_types[Basic_Kind_Rawptr];

    if (!types_are_compatible(ltype, rtype)) {
        b32 left_ac  = node_is_auto_cast((AstNode *) binop->left);
        b32 right_ac = node_is_auto_cast((AstNode *) binop->right);

        if (left_ac && right_ac) ERROR(binop->token->pos, "Cannot have auto cast on both sides of binary operator.");
        else if (unify_node_and_type(&binop->left, rtype));
        else if (unify_node_and_type(&binop->right, ltype));
        else {
            ERROR_(binop->token->pos,
                    "Cannot compare '%s' to '%s'.",
                    type_get_name(ltype),
                    type_get_name(rtype));
        }
    }

    if (!binary_op_is_allowed(binop->operation, binop->left->type)) {
        report_bad_binaryop(binop);
        return Check_Error;
    }

    binop->type = &basic_types[Basic_Kind_Bool];
    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(context.ast_alloc, (AstTyped *) binop);
    }

    return Check_Success;
}

CheckStatus check_binaryop_bool(AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (!type_is_bool(binop->left->type) || !type_is_bool(binop->right->type)) {
        report_bad_binaryop(binop);
        return Check_Error;
    }

    binop->type = &basic_types[Basic_Kind_Bool];

    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(context.ast_alloc, (AstTyped *) binop);
    }
    return Check_Success;
}

CheckStatus check_binaryop(AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (binop->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    u32 current_checking_level_store = current_checking_level;
    CHECK(expression, &binop->left);
    CHECK(expression, &binop->right);
    current_checking_level = current_checking_level_store;

    if ((binop->left->flags & Ast_Flag_Comptime) && (binop->right->flags & Ast_Flag_Comptime)) {
        binop->flags |= Ast_Flag_Comptime;
    }

    if (expression_types_must_be_known) {
        if (binop->left->type == NULL || binop->right->type == NULL) {
            ERROR(binop->token->pos, "Internal compiler error: one of the operands types is unknown here.");
        }
    }

    // :UnaryFieldAccessIsGross
    if (binop->left->kind == Ast_Kind_Unary_Field_Access || binop->right->kind == Ast_Kind_Unary_Field_Access) {
        if      (unify_node_and_type(&binop->left, binop->right->type));
        else if (unify_node_and_type(&binop->right, binop->left->type));
        else {
            report_bad_binaryop(binop);
            return Check_Error;
        }
    }

    // NOTE: Try operator overloading before checking everything else.
    if ((binop->left->type != NULL && binop->left->type->kind != Type_Kind_Basic)
        || (binop->right->type != NULL && binop->right->type->kind != Type_Kind_Basic)) {
        AstCall *implicit_call = binaryop_try_operator_overload(binop);

        if (implicit_call == (AstCall *) &node_that_signals_a_yield)
            YIELD(binop->token->pos, "Trying to resolve operator overload.");

        if (implicit_call != NULL) {
            // NOTE: Not a binary op
            implicit_call->next = binop->next;
            *pbinop = (AstBinaryOp *) implicit_call;

            CHECK(call, (AstCall **) pbinop);
            return Check_Success;
        }
    }

    if (binop_is_assignment(binop->operation)) return check_binaryop_assignment(pbinop);

    // NOTE: Comparision operators and boolean operators are handled separately.
    if (binop_is_compare(binop->operation)) 
        return check_binaryop_compare(pbinop);
    if (binop->operation == Binary_Op_Bool_And || binop->operation == Binary_Op_Bool_Or)
        return check_binaryop_bool(pbinop);

    // NOTE: The left side cannot be compound.
    //       The right side always is numeric.
    //       The left side cannot be rawptr.
    if (type_is_compound(binop->left->type))  goto bad_binaryop;
    if (!type_is_numeric(binop->right->type)) goto bad_binaryop;
    if (type_is_rawptr(binop->left->type)) {
        ERROR(binop->token->pos, "Cannot operate on a 'rawptr'. Cast it to a another pointer type first.");
    }

    // NOTE: Handle basic pointer math.
    if (type_is_pointer(binop->left->type)) {
        if (binop->operation != Binary_Op_Add && binop->operation != Binary_Op_Minus) goto bad_binaryop;

        resolve_expression_type(binop->right);
        if (!type_is_integer(binop->right->type)) goto bad_binaryop;

        AstNumLit* numlit = make_int_literal(context.ast_alloc, type_size_of(binop->left->type->Pointer.elem));
        numlit->token = binop->right->token;
        numlit->type = binop->right->type;

        AstBinaryOp* binop_node = make_binary_op(context.ast_alloc, Binary_Op_Multiply, binop->right, (AstTyped *) numlit);
        binop_node->token = binop->token;
        CHECK(binaryop, &binop_node);

        binop->right = (AstTyped *) binop_node;
        binop->type = binop->left->type;
        binop->right->type = binop->left->type;
    }

    if (!types_are_compatible(binop->left->type, binop->right->type)) {
        b32 left_ac  = node_is_auto_cast((AstNode *) binop->left);
        b32 right_ac = node_is_auto_cast((AstNode *) binop->right);

        if (left_ac && right_ac) {
            ERROR(binop->token->pos, "Cannot have auto cast on both sides of binary operator.");
        }
        else if (unify_node_and_type(&binop->left, binop->right->type));
        else if (unify_node_and_type(&binop->right, binop->left->type));
        else {
            ERROR_(binop->token->pos,
                    "Mismatched types for binary operation '%s'. left: '%s', right: '%s'.",
                    binaryop_string[binop->operation],
                    node_get_type_name(binop->left),
                    node_get_type_name(binop->right));
        }
    }

    binop->type = binop->left->type;
    if (!binary_op_is_allowed(binop->operation, binop->type)) goto bad_binaryop;

    // NOTE: Enum flags with '&' result in a boolean value
    if (binop->type->kind == Type_Kind_Enum && binop->type->Enum.is_flags && binop->operation == Binary_Op_And) {
         binop->type = &basic_types[Basic_Kind_Bool];
    }

    binop->flags |= Ast_Flag_Has_Been_Checked;

    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(context.ast_alloc, (AstTyped *) binop);
    }
    return Check_Success;

bad_binaryop:
    report_bad_binaryop(binop);

    return Check_Error;
}

CheckStatus check_unaryop(AstUnaryOp** punop) {
    AstUnaryOp* unaryop = *punop;

    CHECK(expression, &unaryop->expr);

    if (unaryop->operation != Unary_Op_Negate) {
        resolve_expression_type(unaryop->expr);
    }

    if (unaryop->operation == Unary_Op_Cast) {
        char* err;
        if (unaryop->type == NULL)
            YIELD(unaryop->token->pos, "Trying to resolve destination type for cast.");

        if (!cast_is_legal(unaryop->expr->type, unaryop->type, &err)) {
            ERROR_(unaryop->token->pos, "Cast Error: %s", err);
        }

    } else {
        unaryop->type = unaryop->expr->type;
    }

    if (unaryop->operation == Unary_Op_Not) {
        if (!type_is_bool(unaryop->expr->type)) {
            ERROR_(unaryop->token->pos,
                    "Bool negation operator expected bool type, got '%s'.",
                    node_get_type_name(unaryop->expr));
        }
    }

    if (unaryop->operation == Unary_Op_Bitwise_Not) {
        if (!type_is_integer(unaryop->expr->type)) {
            ERROR_(unaryop->token->pos,
                    "Bitwise operator expected integer type, got '%s'.",
                    node_get_type_name(unaryop->expr));
        }
    }

    if (unaryop->expr->flags & Ast_Flag_Comptime) {
        unaryop->flags |= Ast_Flag_Comptime;
        // NOTE: Not a unary op
        *punop = (AstUnaryOp *) ast_reduce(context.ast_alloc, (AstTyped *) unaryop);
    }

    return Check_Success;
}

CheckStatus check_struct_literal(AstStructLiteral* sl) {
    if (sl->type == NULL) {
        // NOTE: This is used for automatically typed struct literals. If there is no provided
        // type for the struct literal, assume that it is passes successfully. When it is used
        // elsewhere, it will be added as an expression entity that will be processed once the
        // stnode is filled out.
        if (sl->stnode == NULL) return Check_Success;

        if (!node_is_type((AstNode *) sl->stnode)) {
            ERROR(sl->token->pos, "Type used for struct literal is not a type.");
        }

        fill_in_type((AstTyped *) sl);
        if (sl->type == NULL)
            YIELD(sl->token->pos, "Trying to resolve type of struct literal.");
    }

    if (!type_is_structlike_strict(sl->type)) {
        ERROR_(sl->token->pos,
                "'%s' is not constructable using a struct literal.",
                type_get_name(sl->type));
    }

    i32 mem_count = type_structlike_mem_count(sl->type);
    arguments_ensure_length(&sl->args, mem_count);

    // :Idempotency
    if ((sl->flags & Ast_Flag_Has_Been_Checked) == 0) {
        char* err_msg = NULL;
        if (!fill_in_arguments(&sl->args, (AstNode *) sl, &err_msg)) {
            onyx_report_error(sl->token->pos, err_msg);

            bh_arr_each(AstTyped *, value, sl->args.values) {
                if (*value == NULL) {
                    i32 member_idx = value - sl->args.values; // Pointer subtraction hack
                    StructMember smem;
                    type_lookup_member_by_idx(sl->type, member_idx, &smem);

                    onyx_report_error(sl->token->pos,
                        "Value not given for %d%s member, '%s', for type '%s'.",
                        member_idx + 1, bh_num_suffix(member_idx + 1),
                        smem.name, type_get_name(sl->type));
                }
            }

            return Check_Error;
        }
    }
    sl->flags |= Ast_Flag_Has_Been_Checked;

    AstTyped** actual = sl->args.values;
    StructMember smem;

    // BUG: There are problems setting the comptime flag this late in the checking because
    // if the struct literal was type inferred, then the literal won't be correctly determined
    // to be comptime on the first pass, which is needed for top level expressions.
    sl->flags |= Ast_Flag_Comptime;

    fori (i, 0, mem_count) {
        // NOTE: Not checking the return on this function because
        // this for loop is bounded by the number of members in the
        // type.
        type_lookup_member_by_idx(sl->type, i, &smem);
        Type* formal = smem.type;

        CHECK(expression, actual);

        // HACK HACK HACK
        if ((*actual)->type == NULL &&
            (*actual)->entity != NULL &&
            (*actual)->entity->state <= Entity_State_Check_Types) {
            YIELD_((*actual)->token->pos, "Trying to resolve type of expression for member '%s'.", smem.name);
        }

        if (!unify_node_and_type(actual, formal)) {
            ERROR_(sl->token->pos,
                    "Mismatched types for %d%s member named '%s', expected '%s', got '%s'.",
                    i + 1, bh_num_suffix(i + 1),
                    smem.name,
                    type_get_name(formal),
                    node_get_type_name(*actual));
        }

        sl->flags &= ((*actual)->flags & Ast_Flag_Comptime) | (sl->flags &~ Ast_Flag_Comptime);
        actual++;
    }

    return Check_Success;
}

CheckStatus check_array_literal(AstArrayLiteral* al) {
    // :Idempotency
    if ((al->flags & Ast_Flag_Array_Literal_Typed) == 0) {
        if (al->atnode == NULL)
            YIELD(al->token->pos, "Waiting for array literal type to be known.");

        if (!node_is_type((AstNode *) al->atnode))
            ERROR(al->token->pos, "Array type is not a type.");

        fill_in_type((AstTyped *) al);
        if (al->type == NULL)
            YIELD(al->token->pos, "Trying to resolve type of array literal.");

        al->type = type_make_array(context.ast_alloc, al->type, bh_arr_length(al->values));
        if (al->type == NULL || al->type->kind != Type_Kind_Array)
            ERROR(al->token->pos, "Expected array type for array literal. This is a compiler bug.");

        al->flags |= Ast_Flag_Array_Literal_Typed;
    }

    if (al->type->Array.count != (u32) bh_arr_length(al->values)) {
        ERROR_(al->token->pos, "Wrong array size (%d) for number of values (%d).",
            al->type->Array.count, bh_arr_length(al->values));
    }

    al->flags |= Ast_Flag_Comptime;
    assert(al->type->kind == Type_Kind_Array);

    Type* elem_type = al->type->Array.elem;
    bh_arr_each(AstTyped *, expr, al->values) {
        CHECK(expression, expr);

        // HACK HACK HACK
        if ((*expr)->type == NULL &&
            (*expr)->entity != NULL &&
            (*expr)->entity->state <= Entity_State_Check_Types) {
            YIELD_(al->token->pos, "Trying to resolve type of %d%s element of array literal.", expr - al->values, bh_num_suffix(expr - al->values));
        }

        al->flags &= ((*expr)->flags & Ast_Flag_Comptime) | (al->flags &~ Ast_Flag_Comptime);

        if (!unify_node_and_type(expr, elem_type)) {
            ERROR_((*expr)->token->pos, "Mismatched types for value of in array, expected '%s', got '%s'.",
                type_get_name(elem_type),
                node_get_type_name(*expr));
        }
    }

    return Check_Success;
}

CheckStatus check_range_literal(AstRangeLiteral** prange) {
    AstRangeLiteral* range = *prange;
    CHECK(expression, &range->low);
    CHECK(expression, &range->high);

    Type* expected_range_type = builtin_range_type_type;
    StructMember smem;

    type_lookup_member(expected_range_type, "low", &smem);
    if (!unify_node_and_type(&range->low, smem.type)) {
        ERROR_(range->token->pos,
            "Expected left side of range to be a 32-bit integer, got '%s'.",
            node_get_type_name(range->low));
    }

    type_lookup_member(expected_range_type, "high", &smem);
    if (!unify_node_and_type(&range->high, smem.type)) {
        ERROR_(range->token->pos,
            "Expected right side of range to be a 32-bit integer, got '%s'.",
            node_get_type_name(range->high));
    }

    if (range->step == NULL) {
        type_lookup_member(expected_range_type, "step", &smem);
        assert(smem.initial_value != NULL);
        CHECK(expression, smem.initial_value);

        range->step = *smem.initial_value;
    }

    return Check_Success;
}

CheckStatus check_compound(AstCompound* compound) {
    bh_arr_each(AstTyped *, expr, compound->exprs) {
        CHECK(expression, expr);
    }

    compound->type = type_build_compound_type(context.ast_alloc, compound);
    return Check_Success;
}

CheckStatus check_if_expression(AstIfExpression* if_expr) {
    CHECK(expression, &if_expr->cond);
    CHECK(expression, &if_expr->true_expr);
    CHECK(expression, &if_expr->false_expr);

    if (!unify_node_and_type(&if_expr->cond, &basic_types[Basic_Kind_Bool])) {
        ERROR_(if_expr->token->pos, "If-expression expected boolean for condition, got '%s'.",
            type_get_name(if_expr->cond->type));
    }

    resolve_expression_type((AstTyped *) if_expr);

    if (!types_are_compatible(if_expr->true_expr->type, if_expr->false_expr->type)) {
        ERROR_(if_expr->token->pos, "Mismatched types for if-expression, left side is '%s', and right side is '%s'.",
            type_get_name(if_expr->true_expr->type), type_get_name(if_expr->false_expr->type));
    }

    return Check_Success;
}

CheckStatus check_do_block(AstDoBlock** pdoblock) {
    AstDoBlock* doblock = *pdoblock;
    if (doblock->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    fill_in_type((AstTyped *) doblock);

    Type** old_expected_return_type = expected_return_type;
    expected_return_type = &doblock->type;

    doblock->block->rules = Block_Rule_Do_Block;
    CHECK(block, doblock->block);

    if (doblock->type == &type_auto_return) {
        // ERROR(doblock->token->pos, "Unable to determine type of do-block expression.");
        doblock->type = &basic_types[Basic_Kind_Void];
    }

    expected_return_type = old_expected_return_type;
    doblock->flags |= Ast_Flag_Has_Been_Checked;
    return Check_Success;
}

CheckStatus check_address_of(AstAddressOf* aof) {
    CHECK(expression, &aof->expr);
    if (aof->expr->type == NULL) {
        YIELD(aof->token->pos, "Trying to resolve type of expression to take a reference.");
    }

    AstTyped* expr = (AstTyped *) strip_aliases((AstNode *) aof->expr);

    if ((expr->kind != Ast_Kind_Subscript
            && expr->kind != Ast_Kind_Dereference
            && expr->kind != Ast_Kind_Field_Access
            && expr->kind != Ast_Kind_Memres
            && expr->kind != Ast_Kind_Local)
            || (expr->flags & Ast_Flag_Cannot_Take_Addr) != 0) {
        ERROR(aof->token->pos, "Cannot take the address of something that is not an l-value.");
    }

    expr->flags |= Ast_Flag_Address_Taken;

    aof->type = type_make_pointer(context.ast_alloc, expr->type);

    return Check_Success;
}

CheckStatus check_dereference(AstDereference* deref) {
    CHECK(expression, &deref->expr);

    if (!type_is_pointer(deref->expr->type))
        ERROR(deref->token->pos, "Cannot dereference non-pointer value.");

    if (deref->expr->type == basic_type_rawptr.basic_type)
        ERROR(deref->token->pos, "Cannot dereference 'rawptr'. Cast to another pointer type first.");

    deref->type = deref->expr->type->Pointer.elem;

    return Check_Success;
}

CheckStatus check_subscript(AstSubscript** psub) {
    AstSubscript* sub = *psub;
    CHECK(expression, &sub->addr);
    CHECK(expression, &sub->expr);

    // NOTE: Try operator overloading before checking everything else.
    if ((sub->addr->type != NULL && sub->expr->type != NULL) &&
        (sub->addr->type->kind != Type_Kind_Basic || sub->expr->type->kind != Type_Kind_Basic)) {
        // AstSubscript is the same as AstBinaryOp for the first sizeof(AstBinaryOp) bytes
        AstBinaryOp* binop = (AstBinaryOp *) sub;
        AstCall *implicit_call = binaryop_try_operator_overload(binop);

        if (implicit_call == (AstCall *) &node_that_signals_a_yield)
            YIELD(sub->token->pos, "Trying to resolve operator overload.");

        if (implicit_call != NULL) {
            // NOTE: Not an array access
            implicit_call->next = sub->next;
            *psub = (AstSubscript *) implicit_call;

            CHECK(call, (AstCall **) psub);
            return Check_Success;
        }
    }

    if (!type_is_array_accessible(sub->addr->type)) {
        report_bad_binaryop((AstBinaryOp *) sub);
        return Check_Error;
    }

    if (types_are_compatible(sub->expr->type, builtin_range_type_type)) {
        Type *of = NULL;
        if (sub->addr->type->kind == Type_Kind_Pointer)
            of = sub->addr->type->Pointer.elem;
        else if (sub->addr->type->kind == Type_Kind_Array)
            of = sub->addr->type->Array.elem;
        else {
            // FIXME: Slice creation should be allowed for slice types and dynamic array types, like it
            // is below, but this code doesn't look at that.
            report_bad_binaryop((AstBinaryOp *) sub);
            ERROR(sub->token->pos, "Invalid type for left of slice creation.");
        }

        sub->kind = Ast_Kind_Slice;
        sub->type = type_make_slice(context.ast_alloc, of);
        sub->elem_size = type_size_of(of);

        return Check_Success;
    }

    resolve_expression_type(sub->expr);
    if (sub->expr->type->kind != Type_Kind_Basic
            || (sub->expr->type->Basic.kind != Basic_Kind_I32 && sub->expr->type->Basic.kind != Basic_Kind_U32)) {
        report_bad_binaryop((AstBinaryOp *) sub);
        ERROR_(sub->token->pos,
            "Expected type u32 or i32 for index, got '%s'.",
            node_get_type_name(sub->expr));
    }

    if (sub->addr->type->kind == Type_Kind_Pointer)
        sub->type = sub->addr->type->Pointer.elem;
    else if (sub->addr->type->kind == Type_Kind_Array)
        sub->type = sub->addr->type->Array.elem;
    else if (sub->addr->type->kind == Type_Kind_Slice
            || sub->addr->type->kind == Type_Kind_DynArray
            || sub->addr->type->kind == Type_Kind_VarArgs) {
        // If we are accessing on a slice or a dynamic array, implicitly add a field access for the data member
        StructMember smem;
        type_lookup_member(sub->addr->type, "data", &smem);

        AstFieldAccess* fa = make_field_access(context.ast_alloc, sub->addr, "data");
        fa->type   = smem.type;
        fa->offset = smem.offset;
        fa->idx    = smem.idx;

        sub->addr = (AstTyped *) fa;
        sub->type = sub->addr->type->Pointer.elem;
    }
    else {
        report_bad_binaryop((AstBinaryOp *) sub);
        ERROR(sub->token->pos, "Invalid type for left of array access.");
    }

    sub->elem_size = type_size_of(sub->type);

    return Check_Success;
}

CheckStatus check_field_access(AstFieldAccess** pfield) {
    AstFieldAccess* field = *pfield;
    CHECK(expression, &field->expr);
    if (field->expr->type == NULL) {
        // onyx_report_error(field->token->pos, "Unable to deduce type of expression for accessing field.");
        YIELD(field->token->pos, "Trying to resolve type of source expression.");
    }

    if (!type_is_structlike(field->expr->type)) {
        ERROR_(field->token->pos,
            "Cannot access field '%b' on '%s'. Type is not a struct.",
            field->token->text,
            field->token->length,
            node_get_type_name(field->expr));
    }

    // Optimization for (*foo).member.
    if (field->expr->kind == Ast_Kind_Dereference) {
        field->expr = ((AstDereference *) field->expr)->expr;
    }

    StructMember smem;
    if (field->token != NULL && field->field == NULL) {
        token_toggle_end(field->token);
        // CLEANUP: Duplicating the string here isn't the best for effiency,
        // but it fixes a lot of bugs, so here we are.
        //                                      - brendanfh  2020/12/08
        field->field = bh_strdup(context.ast_alloc, field->token->text);
        token_toggle_end(field->token);
    }

    if (!type_lookup_member(field->expr->type, field->field, &smem)) {
        if (field->expr->type->kind == Type_Kind_Array) {
            if (!strcmp(field->field, "count")) {
                *pfield = (AstFieldAccess *) make_int_literal(context.ast_alloc, field->expr->type->Array.count);
                return Check_Success;
            }
        }

        AstType* type_node = field->expr->type->ast_type;
        AstNode* n = try_symbol_raw_resolve_from_node((AstNode *) type_node, field->field);
        if (n) {
            *pfield = (AstFieldAccess *) n;
            return Check_Success;
        }

        ERROR_(field->token->pos,
            "Field '%s' does not exists on '%s'.",
            field->field,
            node_get_type_name(field->expr));
    }

    field->offset = smem.offset;
    field->idx = smem.idx;
    field->type = smem.type;

    return Check_Success;
}

CheckStatus check_method_call(AstBinaryOp** mcall) {
    CHECK(expression, &(*mcall)->left);
    if ((*mcall)->left->type == NULL) {
        YIELD((*mcall)->token->pos, "Trying to resolve type of left hand side.");
    }

    AstTyped* implicit_argument = (*mcall)->left;

    // Symbol resolution should have ensured that this is call node.
    AstCall* call_node = (AstCall *) (*mcall)->right;
    assert(call_node->kind == Ast_Kind_Call);

    // :Idempotency
    if (((*mcall)->flags & Ast_Flag_Has_Been_Checked) == 0) {
        // Implicitly take the address of the value if it is not already a pointer type.
        // This could be weird to think about semantically so some testing with real code
        // would be good.                                      - brendanfh 2020/02/05
        if (implicit_argument->type->kind != Type_Kind_Pointer)
            implicit_argument = (AstTyped *) make_address_of(context.ast_alloc, implicit_argument);

        implicit_argument = (AstTyped *) make_argument(context.ast_alloc, implicit_argument);

        bh_arr_insertn(call_node->args.values, 0, 1);
        call_node->args.values[0] = implicit_argument;
    }
    (*mcall)->flags |= Ast_Flag_Has_Been_Checked;

    CHECK(call, &call_node);
    call_node->next = (*mcall)->next;

    *mcall = (AstBinaryOp *) call_node;
    return Check_Success;
}

CheckStatus check_size_of(AstSizeOf* so) {
    CHECK(type, so->so_ast_type);

    so->so_type = type_build_from_ast(context.ast_alloc, so->so_ast_type);
    if (so->so_type == NULL)
        YIELD(so->token->pos, "Trying to resolve type to take the size of.");

    so->size = type_size_of(so->so_type);

    return Check_Success;
}

CheckStatus check_align_of(AstAlignOf* ao) {
    CHECK(type, ao->ao_ast_type);

    ao->ao_type = type_build_from_ast(context.ast_alloc, ao->ao_ast_type);
    if (ao->ao_type == NULL)
        YIELD(ao->token->pos, "Trying to resolve type to take the alignment of.");

    ao->alignment = type_alignment_of(ao->ao_type);

    return Check_Success;
}

CheckStatus check_expression(AstTyped** pexpr) {
    AstTyped* expr = *pexpr;
    if (expr->kind > Ast_Kind_Type_Start && expr->kind < Ast_Kind_Type_End) {
        // This is to ensure that the type will exist when compiling. For example, a poly-call type
        // would have to wait for the entity to pass through, which the code generation does not know
        // about.
        CHECK(type, (AstType *) expr);

        // Don't try to construct a polystruct ahead of time because you can't.
        if (expr->kind != Ast_Kind_Poly_Struct_Type) {
            if (type_build_from_ast(context.ast_alloc, (AstType*) expr) == NULL) {
                YIELD(expr->token->pos, "Trying to construct type.");
            }
        }

        expr->type = &basic_types[Basic_Kind_Type_Index];
        return Check_Success;
    }

    if (expr->kind == Ast_Kind_Polymorphic_Proc) {
        // polymorphic procedures do not need to be checked. Their concrete instantiations
        // will be checked when they are created.
        return Check_Success;
    }

    if (expr->kind == Ast_Kind_Macro) {
        return Check_Success;
    }

    fill_in_type(expr);
    current_checking_level = EXPRESSION_LEVEL;

    CheckStatus retval = Check_Success;
    switch (expr->kind) {
        case Ast_Kind_Binary_Op: retval = check_binaryop((AstBinaryOp **) pexpr); break;
        case Ast_Kind_Unary_Op:  retval = check_unaryop((AstUnaryOp **) pexpr); break;

        case Ast_Kind_Call:     retval = check_call((AstCall **) pexpr); break;
        case Ast_Kind_Argument: retval = check_argument((AstArgument **) pexpr); break;
        case Ast_Kind_Block:    retval = check_block((AstBlock *) expr); break;

        case Ast_Kind_Symbol:
            YIELD_(expr->token->pos, "Waiting to resolve symbol, '%b'.", expr->token->text, expr->token->length);
            break;

        case Ast_Kind_Param:
            if (expr->type == NULL) {
                onyx_report_error(expr->token->pos, "Parameter with bad type.");
                retval = Check_Error;
            }
            break;

        case Ast_Kind_Local: break;

        case Ast_Kind_Address_Of:    retval = check_address_of((AstAddressOf *) expr); break;
        case Ast_Kind_Dereference:   retval = check_dereference((AstDereference *) expr); break;
        case Ast_Kind_Slice:
        case Ast_Kind_Subscript:     retval = check_subscript((AstSubscript **) pexpr); break;
        case Ast_Kind_Field_Access:  retval = check_field_access((AstFieldAccess **) pexpr); break;
        case Ast_Kind_Method_Call:   retval = check_method_call((AstBinaryOp **) pexpr); break;
        case Ast_Kind_Size_Of:       retval = check_size_of((AstSizeOf *) expr); break;
        case Ast_Kind_Align_Of:      retval = check_align_of((AstAlignOf *) expr); break;
        case Ast_Kind_Range_Literal: retval = check_range_literal((AstRangeLiteral **) pexpr); break;

        case Ast_Kind_Global:
            if (expr->type == NULL) {
                onyx_report_error(expr->token->pos, "Global with unknown type.");
                retval = Check_Error;
            }
            break;

        case Ast_Kind_NumLit:
            assert(expr->type != NULL);
            break;

        case Ast_Kind_Struct_Literal:
            retval = check_struct_literal((AstStructLiteral *) expr);
            break;

        case Ast_Kind_Array_Literal:
            retval = check_array_literal((AstArrayLiteral *) expr);
            break;

        case Ast_Kind_Function:
            // NOTE: Will need something like this at some point
            // AstFunction* func = (AstFunction *) expr;
            // bh_arr_each(AstParam, param, func->params) {
            //     if (param->default_value != NULL) {
            //         onyx_message_add(Msg_Type_Literal,
            //                 func->token->pos,
            //                 "cannot use functions with default parameters in this way");
            //         retval = 1;
            //         break;
            //     }
            // }
            if (expr->type == NULL)
                YIELD(expr->token->pos, "Waiting for function type to be resolved.");

            expr->flags |= Ast_Flag_Function_Used;
            break;

        case Ast_Kind_Directive_Solidify:
            *pexpr = (AstTyped *) ((AstDirectiveSolidify *) expr)->resolved_proc;
            break;

        case Ast_Kind_Directive_Defined:
            *pexpr = (AstTyped *) make_bool_literal(context.ast_alloc, ((AstDirectiveDefined *) expr)->is_defined);
            fill_in_type(*pexpr);
            break;

        case Ast_Kind_Compound:
            CHECK(compound, (AstCompound *) expr);
            break;

        case Ast_Kind_Call_Site:
            // NOTE: This has to be set here because if it were to be set in the parser,
            // builtin_callsite_type wouldn't be known when parsing the builtin.onyx file.
            expr->type_node = builtin_callsite_type;
            break;

        case Ast_Kind_If_Expression:
            CHECK(if_expression, (AstIfExpression *) expr);
            break;

        case Ast_Kind_Alias:
            CHECK(expression, &((AstAlias *) expr)->alias);
            expr->flags |= (((AstAlias *) expr)->alias->flags & Ast_Flag_Comptime);
            expr->type = ((AstAlias *) expr)->alias->type;
            break;

        case Ast_Kind_Directive_Insert:
            retval = check_insert_directive((AstDirectiveInsert **) pexpr);
            break;

        case Ast_Kind_Code_Block:
            expr->flags |= Ast_Flag_Comptime;
            fill_in_type(expr);
            break;

        case Ast_Kind_Do_Block:
            retval = check_do_block((AstDoBlock **) pexpr);
            break;

        case Ast_Kind_StrLit: break;
        case Ast_Kind_File_Contents: break;
        case Ast_Kind_Overloaded_Function: break;
        case Ast_Kind_Enum_Value: break;
        case Ast_Kind_Memres: break;
        case Ast_Kind_Polymorphic_Proc: break;
        case Ast_Kind_Package: break;
        case Ast_Kind_Error: break;
        case Ast_Kind_Unary_Field_Access: break;

        // NOTE: The only way to have an Intrinsic_Call node is to have gone through the
        // checking of a call node at least once.
        case Ast_Kind_Intrinsic_Call: break;

        default:
            retval = Check_Error;
            onyx_report_error(expr->token->pos, "UNEXPECTED INTERNAL COMPILER ERROR");
            DEBUG_HERE;
            break;
    }

    return retval;
}

CheckStatus check_global(AstGlobal* global) {
    fill_in_type((AstTyped *) global);

    if (global->type == NULL) {
        YIELD(global->token->pos, "Trying to resolve type for global.");
    }

    return Check_Success;
}

CheckStatus check_insert_directive(AstDirectiveInsert** pinsert) {
    AstDirectiveInsert* insert = *pinsert;
    if (insert->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    CHECK(expression, &insert->code_expr);
    if (insert->code_expr->type == NULL) {
        if (insert->code_expr->entity && insert->code_expr->entity->state >= Entity_State_Code_Gen) {
            ERROR(insert->token->pos, "Expected expression of type 'Code'.");
        }

        // Bad wording for the message.
        YIELD(insert->token->pos, "Waiting for resolution to code expression type.");
    }

    Type* code_type = type_build_from_ast(context.ast_alloc, builtin_code_type);

    if (!unify_node_and_type(&insert->code_expr, code_type)) {
        ERROR_(insert->token->pos, "#insert expected a value of type 'Code', got '%s'.",
            type_get_name(insert->code_expr->type));
    }

    AstCodeBlock* code_block = (AstCodeBlock *) insert->code_expr;
    code_block = (AstCodeBlock *) strip_aliases((AstNode *) code_block);

    assert(code_block->kind == Ast_Kind_Code_Block);

    AstNode* cloned_block = ast_clone(context.ast_alloc, code_block->code);
    cloned_block->next = insert->next;
    *(AstNode **) pinsert = cloned_block;

    insert->flags |= Ast_Flag_Has_Been_Checked;

    return Check_Return_To_Symres;
}

CheckStatus check_statement(AstNode** pstmt) {
    AstNode* stmt = *pstmt;

    current_checking_level = STATEMENT_LEVEL;

    switch (stmt->kind) {
        case Ast_Kind_Jump:       return Check_Success;

        case Ast_Kind_Return:     return check_return((AstReturn *) stmt);
        case Ast_Kind_If:         return check_if((AstIfWhile *) stmt);
        case Ast_Kind_Static_If:  return check_if((AstIfWhile *) stmt);
        case Ast_Kind_While:      return check_while((AstIfWhile *) stmt);
        case Ast_Kind_For:        return check_for((AstFor *) stmt);
        case Ast_Kind_Switch:     return check_switch((AstSwitch *) stmt);
        case Ast_Kind_Block:      return check_block((AstBlock *) stmt);
        case Ast_Kind_Defer:      return check_statement(&((AstDefer *) stmt)->stmt);
        case Ast_Kind_Call: {
            CHECK(call, (AstCall **) pstmt);
            stmt->flags |= Ast_Flag_Expr_Ignored;
            return Check_Success;
        }

        case Ast_Kind_Binary_Op:
            CHECK(binaryop, (AstBinaryOp **) pstmt);
            (*pstmt)->flags |= Ast_Flag_Expr_Ignored;
            return Check_Success;

        // NOTE: Local variable declarations used to be removed after the symbol
        // resolution phase because long long ago, all locals needed to be known
        // in a block in order to efficiently allocate enough space and registers
        // for them all. Now with LocalAllocator, this is no longer necessary.
        // Therefore, locals stay in the tree and need to be passed along.
        case Ast_Kind_Local: {
            AstTyped* typed_stmt = (AstTyped *) stmt;
            fill_in_type(typed_stmt);
            if (typed_stmt->type_node != NULL && typed_stmt->type == NULL) {
                CHECK(type, typed_stmt->type_node);

                if (!node_is_type((AstNode *) typed_stmt->type_node)) {
                    ERROR(stmt->token->pos, "Local's type is not a type.");
                }

                YIELD(typed_stmt->token->pos, "Waiting for local variable's type.");
            }
            return Check_Success;
        }

        default:
            CHECK(expression, (AstTyped **) pstmt);
            (*pstmt)->flags |= Ast_Flag_Expr_Ignored;
            return Check_Success;
    }
}

CheckStatus check_statement_chain(AstNode** start) {
    while (*start) {
        CHECK(statement, start);
        start = &(*start)->next;
    }

    return Check_Success;
}

CheckStatus check_block(AstBlock* block) {
    CHECK(statement_chain, &block->body);

    // CLEANUP: There will need to be some other method of 
    // checking the following conditions.
    //
    // bh_arr_each(AstTyped *, value, block->allocate_exprs) {
    //     fill_in_type(*value);

    //     if ((*value)->kind == Ast_Kind_Local) {
    //         if ((*value)->type == NULL) {
    //             onyx_report_error((*value)->token->pos,
    //                     "Unable to resolve type for local '%b'.",
    //                     (*value)->token->text, (*value)->token->length);
    //             return Check_Error;
    //         }

    //         if ((*value)->type->kind == Type_Kind_Compound) {
    //             onyx_report_error((*value)->token->pos,
    //                     "Compound type not allowed as local variable type. Try splitting this into multiple variables.");
    //             return Check_Error;
    //         }
    //     }
    // }

    return Check_Success;
}

CheckStatus check_function(AstFunction* func) {
    if (func->flags & Ast_Flag_Already_Checked) return Check_Success;
    if (func->entity_header && func->entity_header->state < Entity_State_Code_Gen)
        YIELD(func->token->pos, "Waiting for procedure header to pass type-checking");
    
    expected_return_type = &func->type->Function.return_type;
    if (func->body) {
        CheckStatus status = check_block(func->body);
        if (status == Check_Error && func->generated_from && context.cycle_detected == 0)
            ERROR(func->generated_from->pos, "Error in polymorphic procedure generated from this location.");

        if (status != Check_Success) return status;
    }

    if (*expected_return_type == &type_auto_return) {
        *expected_return_type = &basic_types[Basic_Kind_Void];
    }

    func->flags |= Ast_Flag_Already_Checked;
    return Check_Success;
}

CheckStatus check_overloaded_function(AstOverloadedFunction* func) {
    b32 done = 1;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, global_heap_allocator, 4);
    build_all_overload_options(func->overloads, &all_overloads);

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) entry->key;
        if (node->kind == Ast_Kind_Overloaded_Function) continue;

        if (   node->kind != Ast_Kind_Function
            && node->kind != Ast_Kind_Polymorphic_Proc
            && node->kind != Ast_Kind_Macro) {
            onyx_report_error(node->token->pos, "Overload option not procedure or macro. Got '%s'",
                onyx_ast_node_kind_string(node->kind));

            bh_imap_free(&all_overloads);
            return Check_Error;
        }

        if (node->kind == Ast_Kind_Function) {
            AstFunction* func = (AstFunction *) node;

            if (func->entity_header && func->entity_header->state <= Entity_State_Check_Types) {
                done = 0;
            }
        }
    }

    bh_imap_free(&all_overloads);

    if (done) return Check_Success;
    else      YIELD(func->token->pos, "Waiting for all options to pass type-checking.");
}

CheckStatus check_struct(AstStructType* s_node) {
    if (s_node->entity_defaults && s_node->entity_defaults->state < Entity_State_Check_Types)
        YIELD(s_node->token->pos, "Waiting for struct member defaults to pass symbol resolution.");

    bh_arr_each(AstStructMember *, smem, s_node->members) {
        if ((*smem)->type_node != NULL) {
            CHECK(type, (*smem)->type_node);
        }

        if ((*smem)->type_node == NULL && (*smem)->initial_value != NULL) {
            CHECK(expression, &(*smem)->initial_value);

            fill_in_type((*smem)->initial_value);
            if ((*smem)->initial_value->type == NULL)
                YIELD((*smem)->initial_value->token->pos, "Trying to resolve type for initial value for member.");

            resolve_expression_type((*smem)->initial_value);
            if ((*smem)->type == NULL) (*smem)->type = (*smem)->initial_value->type;

            if ((*smem)->type == NULL) {
                ERROR((*smem)->initial_value->token->pos, "Unable to deduce type of initial value. This is probably a compiler bug.");
            }
        }
    }

    // NOTE: fills in the stcache
    type_build_from_ast(context.ast_alloc, (AstType *) s_node);
    if (s_node->stcache == NULL || !s_node->stcache_is_valid)
        YIELD(s_node->token->pos, "Waiting for type to be constructed.");

    bh_arr_each(StructMember *, smem, s_node->stcache->Struct.memarr) {
        if ((*smem)->type->kind == Type_Kind_Compound) {
            ERROR(s_node->token->pos, "Compound types are not allowed as struct member types.");
        }
    }

    return Check_Success;
}

CheckStatus check_struct_defaults(AstStructType* s_node) {
    if (s_node->entity_type && s_node->entity_type->state < Entity_State_Code_Gen)
        YIELD(s_node->token->pos, "Waiting for struct type to be constructed before checking defaulted members.");

    if (s_node->meta_tags) {
        bh_arr_each(AstTyped *, meta, s_node->meta_tags) {
            CHECK(expression, meta);
            resolve_expression_type(*meta);

            if (((*meta)->flags & Ast_Flag_Comptime) == 0) {
                onyx_report_error((*meta)->token->pos, "#tag expressions are expected to be compile-time known.");
                return Check_Error;
            }
        }
    }

    bh_arr_each(StructMember *, smem, s_node->stcache->Struct.memarr) {
        if ((*smem)->initial_value && *(*smem)->initial_value) {
            CHECK(expression, (*smem)->initial_value);

            if (!unify_node_and_type((*smem)->initial_value, (*smem)->type)) {
                ERROR_((*(*smem)->initial_value)->token->pos,
                        "Mismatched type for initial value, expected '%s', got '%s'.",
                        type_get_name((*smem)->type),
                        type_get_name((*(*smem)->initial_value)->type));
            }

            resolve_expression_type(*(*smem)->initial_value);
        }

        if ((*smem)->meta_tags) {
            bh_arr_each(AstTyped *, meta, (*smem)->meta_tags) {
                CHECK(expression, meta);
                resolve_expression_type(*meta);

                if (((*meta)->flags & Ast_Flag_Comptime) == 0) {
                    onyx_report_error((*meta)->token->pos, "#tag expressions are expected to be compile-time known.");
                    return Check_Error;
                }
            }
        }
    }

    return Check_Success;
}

CheckStatus check_function_header(AstFunction* func) {
    //if (func->entity_body && func->entity_body->state < Entity_State_Check_Types)
    //    YIELD(func->token->pos, "Waiting for function body to complete symbol resolution to check header.");

    b32 expect_default_param = 0;
    b32 has_had_varargs = 0;

    bh_arr_each(AstParam, param, func->params) {
        AstLocal* local = param->local;

        if (expect_default_param && param->default_value == NULL) {
            ERROR(local->token->pos,
                    "All parameters must have default values after the first default valued parameter.");
        }

        if (has_had_varargs && param->vararg_kind != VA_Kind_Not_VA) {
            ERROR(local->token->pos,
                    "Can only have one param that is of variable argument type.");
        }

        if (has_had_varargs && param->vararg_kind != VA_Kind_Not_VA) {
            ERROR(local->token->pos,
                    "Variable arguments must be last in parameter list");
        }

        if (param->vararg_kind == VA_Kind_Untyped) {
            // HACK
            if (builtin_vararg_type_type == NULL)
                builtin_vararg_type_type = type_build_from_ast(context.ast_alloc, builtin_vararg_type);

            local->type = builtin_vararg_type_type;
        }

        if (param->default_value != NULL) {
            if (param->vararg_kind != VA_Kind_Not_VA) {
                ERROR(local->token->pos, "Variadic arguments cannot have default values.");
            }

            CHECK(expression, &param->default_value);

            if (local->type_node == NULL && local->type == NULL) {
                local->type = resolve_expression_type(param->default_value);
            }

            expect_default_param = 1;
        }

        if (local->type_node != NULL) CHECK(type, local->type_node);
        if (local->type_node != NULL) {
            if (!node_is_type((AstNode *) local->type_node)) {
                ERROR(local->token->pos, "Parameter type is not a type.");
            }
        }

        fill_in_type((AstTyped *) local);
        if (local->type == NULL) {
            // onyx_report_error(param->local->token->pos,
            //         "Unable to resolve type for parameter, '%b'",
            //         local->token->text,
            //         local->token->length);
            YIELD(local->token->pos, "Waiting for parameter type to be known.");
        }

        if (local->type->kind == Type_Kind_Compound) {
            ERROR(param->local->token->pos, "Compound types are not allowed as parameter types. Try splitting this into multiple parameters.");
        }

        // NOTE: I decided to make parameter default values not type checked against
        // the actual parameter type. The actual type checking will happen in check_call
        // when the default value is used as an argument and then has to be checked against
        // the parameter type                                  - brendanfh 2021/01/06
        // if (param->default_value != NULL) {
        //     if (!unify_node_and_type(&param->default_value, param->local->type)) {
        //         onyx_report_error(param->local->token->pos,
        //                 "Expected default value of type '%s', was of type '%s'.",
        //                 type_get_name(param->local->type),
        //                 type_get_name(param->default_value->type));
        //         return Check_Error;
        //     }
        // }

        if (param->vararg_kind != VA_Kind_Not_VA) has_had_varargs = 1;

        if (local->type->kind != Type_Kind_Array && type_size_of(local->type) == 0) {
            ERROR(local->token->pos, "Function parameters cannot have zero-width types.");
        }
    }

    if (func->return_type != NULL) CHECK(type, func->return_type);

    func->type = type_build_function_type(context.ast_alloc, func);
    if (func->type == NULL) YIELD(func->token->pos, "Waiting for function type to be constructed");

    return Check_Success;
}

CheckStatus check_memres_type(AstMemRes* memres) {
    CHECK(type, memres->type_node);
    fill_in_type((AstTyped *) memres);
    if (memres->type_node && !memres->type) YIELD(memres->token->pos, "Waiting for global type to be constructed.");
    return Check_Success;
}

CheckStatus check_memres(AstMemRes* memres) {
    if (memres->initial_value != NULL) {
        CHECK(expression, &memres->initial_value);

        if ((memres->initial_value->flags & Ast_Flag_Comptime) == 0) {
            ERROR(memres->initial_value->token->pos, "Top level expressions must be compile time known.");
        }

        if (memres->type != NULL) {
            Type* memres_type = memres->type;
            if (!unify_node_and_type(&memres->initial_value, memres_type)) {
                ERROR_(memres->token->pos,
                        "Cannot assign value of type '%s' to a '%s'.",
                        node_get_type_name(memres->initial_value),
                        type_get_name(memres_type));
            }

        } else {
            resolve_expression_type(memres->initial_value);
            if (memres->initial_value->type == NULL && memres->initial_value->entity != NULL && memres->initial_value->entity->state <= Entity_State_Check_Types) {
                YIELD(memres->token->pos, "Waiting for global type to be constructed.");
            }
            memres->type = memres->initial_value->type;
        }
    }

    return Check_Success;
}

CheckStatus check_type(AstType* type) {
    if (type == NULL) return Check_Success;
    
    AstType* original_type = type;
    while (type->kind == Ast_Kind_Type_Alias)
        type = ((AstTypeAlias *) type)->to;

    if (type->flags & Ast_Flag_Already_Checked) return Check_Success;

    switch (type->kind) {
        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pc_node = (AstPolyCallType *) type;

            bh_arr_each(AstNode *, param, pc_node->params) {
                if (!node_is_type(*param)) {
                    CHECK(expression, (AstTyped **) param);
                    resolve_expression_type((AstTyped *) *param);
                    fill_in_type((AstTyped *) *param);
                }
            }

            break;
        }

        case Ast_Kind_Typeof: {
            AstTypeOf *type_of = (AstTypeOf *) type;
            CHECK(expression, (AstTyped **) &type_of->expr);
            resolve_expression_type(type_of->expr);

            if (type_of->expr->type == NULL) {
                YIELD(type_of->token->pos, "Trying to check type for type-of expression.");
            }

            type_of->resolved_type = type_of->expr->type;
            break;
        }

        case Ast_Kind_Pointer_Type: CHECK(type, ((AstPointerType *) type)->elem); break;
        case Ast_Kind_Slice_Type:   CHECK(type, ((AstSliceType *) type)->elem); break;
        case Ast_Kind_DynArr_Type:  CHECK(type, ((AstDynArrType *) type)->elem); break;
        case Ast_Kind_VarArg_Type:  CHECK(type, ((AstVarArgType *) type)->elem); break;

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype = (AstFunctionType *) type;

            CHECK(type, ftype->return_type);

            if (ftype->param_count > 0) {
                fori (i, 0, (i64) ftype->param_count) {
                    CHECK(type, ftype->params[i]);
                }
            }
            break;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* ctype = (AstCompoundType *) type;

            bh_arr_each(AstType *, type, ctype->types) CHECK(type, *type);
            break;
        }
        
        case Ast_Kind_Array_Type: {
            AstArrayType* atype = (AstArrayType *) type;
            if (atype->count_expr) {
                CHECK(expression, &atype->count_expr);
                resolve_expression_type(atype->count_expr);
            }

            break;
        }
    }

    type = original_type;
    while (type->kind == Ast_Kind_Type_Alias) {
        type->flags |= Ast_Flag_Comptime;
        type = ((AstTypeAlias *) type)->to;
    }

    type->flags |= Ast_Flag_Already_Checked;
    return Check_Success;
}

CheckStatus check_static_if(AstIf* static_if) {
    expression_types_must_be_known = 1;
    CheckStatus result = check_expression(&static_if->cond);
    if (result == Check_Yield_Macro) return Check_Yield_Macro;
    expression_types_must_be_known = 0;

    if (result > Check_Errors_Start || !(static_if->cond->flags & Ast_Flag_Comptime)) {
        ERROR(static_if->token->pos, "Expected this condition to be compile time known.");
    }

    if (!type_is_bool(static_if->cond->type)) {
        ERROR(static_if->token->pos, "Expected this condition to be a boolean value.");
    }

    static_if->flags |= Ast_Flag_Static_If_Resolved;

    b32 resolution = static_if_resolution(static_if);

    if (context.options->print_static_if_results)
        bh_printf("Static if statement at %s:%d:%d resulted in %s\n",
            static_if->token->pos.filename,
            static_if->token->pos.line,
            static_if->token->pos.column,
            resolution ? "true" : "false");

    if (resolution) {
        bh_arr_each(Entity *, ent, static_if->true_entities) {
            entity_heap_insert_existing(&context.entities, *ent);
        }

    } else {
        bh_arr_each(Entity *, ent, static_if->false_entities) {
            entity_heap_insert_existing(&context.entities, *ent);
        }
    }

    return Check_Complete;
}

CheckStatus check_process_directive(AstNode* directive) {
    if (directive->kind == Ast_Kind_Directive_Export) {
        AstTyped* export = ((AstDirectiveExport *) directive)->export;
        if (export->entity && export->entity->state <= Entity_State_Check_Types)
            YIELD(directive->token->pos, "Waiting for export type to be known.");
    }

    return Check_Success;
}

CheckStatus check_macro(AstMacro* macro) {
    if (macro->body->kind == Ast_Kind_Function) {
        CHECK(function_header, (AstFunction *) macro->body);
    }

    return Check_Success;
}

CheckStatus check_node(AstNode* node) {
    switch (node->kind) {
        case Ast_Kind_Function:             return check_function((AstFunction *) node);
        case Ast_Kind_Overloaded_Function:  return check_overloaded_function((AstOverloadedFunction *) node);
        case Ast_Kind_Block:                return check_block((AstBlock *) node);
        case Ast_Kind_Return:               return check_return((AstReturn *) node);
        case Ast_Kind_If:                   return check_if((AstIfWhile *) node);
        case Ast_Kind_Static_If:            return check_if((AstIfWhile *) node);
        case Ast_Kind_While:                return check_while((AstIfWhile *) node);
        case Ast_Kind_Call:                 return check_call((AstCall **) &node);
        case Ast_Kind_Binary_Op:            return check_binaryop((AstBinaryOp **) &node);
        default:                            return check_expression((AstTyped **) &node);
    }
}

void check_entity(Entity* ent) {
    CheckStatus cs = Check_Success;

    switch (ent->type) {
        case Entity_Type_Foreign_Function_Header:
        case Entity_Type_Function_Header:          cs = check_function_header(ent->function); break;
        case Entity_Type_Function:                 cs = check_function(ent->function); break;
        case Entity_Type_Overloaded_Function:      cs = check_overloaded_function(ent->overloaded_function); break;
        case Entity_Type_Global:                   cs = check_global(ent->global); break;
        case Entity_Type_Struct_Member_Default:    cs = check_struct_defaults((AstStructType *) ent->type_alias); break;
        case Entity_Type_Memory_Reservation_Type:  cs = check_memres_type(ent->mem_res); break;
        case Entity_Type_Memory_Reservation:       cs = check_memres(ent->mem_res); break;
        case Entity_Type_Static_If:                cs = check_static_if(ent->static_if); break;
        case Entity_Type_Macro:                    cs = check_macro(ent->macro);

        case Entity_Type_Expression:
            cs = check_expression(&ent->expr);
            resolve_expression_type(ent->expr);
            break;

        case Entity_Type_Type_Alias:
            if (ent->type_alias->kind == Ast_Kind_Struct_Type)
                cs = check_struct((AstStructType *) ent->type_alias);
            else
                cs = check_type(ent->type_alias);
            break;

        case Entity_Type_Process_Directive:        cs = check_process_directive((AstNode *) ent->expr); break;

        case Entity_Type_File_Contents: 
            if (context.options->no_file_contents) {
                onyx_report_error(ent->expr->token->pos, "#file_contents is disabled for this compilation.");
            }
            break;

        default: break;
    }

    if (cs == Check_Success)          ent->state = Entity_State_Code_Gen;
    if (cs == Check_Complete)         ent->state = Entity_State_Finalized;
    if (cs == Check_Return_To_Symres) ent->state = Entity_State_Resolve_Symbols;
    if (cs == Check_Yield_Macro)      ent->macro_attempts++;
    else {
        ent->macro_attempts = 0;
        ent->micro_attempts = 0;
    }
}
