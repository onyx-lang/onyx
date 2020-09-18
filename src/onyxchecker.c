#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxparser.h"
#include "onyxutils.h"

#define CHECK(kind, ...) b32 check_ ## kind (__VA_ARGS__)

CHECK(block, AstBlock* block);
CHECK(statement_chain, AstNode* start);
CHECK(statement, AstNode* stmt);
CHECK(return, AstReturn* retnode);
CHECK(if, AstIfWhile* ifnode);
CHECK(while, AstIfWhile* whilenode);
CHECK(for, AstFor* fornode);
CHECK(switch, AstSwitch* switchnode);
CHECK(call, AstCall* call);
CHECK(binaryop, AstBinaryOp** pbinop, b32 assignment_is_ok);
CHECK(unaryop, AstUnaryOp** punop);
CHECK(struct_literal, AstStructLiteral* sl);
CHECK(expression, AstTyped** expr);
CHECK(address_of, AstAddressOf* aof);
CHECK(dereference, AstDereference* deref);
CHECK(array_access, AstArrayAccess* expr);
CHECK(field_access, AstFieldAccess** pfield);
CHECK(range_literal, AstBinaryOp** range);
CHECK(size_of, AstSizeOf* so);
CHECK(align_of, AstAlignOf* ao);
CHECK(global, AstGlobal* global);
CHECK(function, AstFunction* func);
CHECK(overloaded_function, AstOverloadedFunction* func);
CHECK(struct, AstStructType* s_node);
CHECK(function_header, AstFunction* func);
CHECK(memres, AstMemRes* memres);

static inline void fill_in_type(AstTyped* node) {
    if (node->type_node && node->type_node->kind == Ast_Kind_Array_Type) {
        if (((AstArrayType *) node->type_node)->count_expr) check_expression(&((AstArrayType *) node->type_node)->count_expr);
    }

    if (node->type == NULL)
        node->type = type_build_from_ast(semstate.allocator, node->type_node);
}

b32 check_return(AstReturn* retnode) {
    if (retnode->expr) {
        if (check_expression(&retnode->expr)) return 1;

        if (!types_are_compatible(retnode->expr->type, semstate.expected_return_type)) {
            onyx_report_error(retnode->expr->token->pos,
                    "Expected to return a value of type '%s', returning value of type '%s'.",
                    type_get_name(semstate.expected_return_type),
                    type_get_name(retnode->expr->type));
            return 1;
        }
    } else {
        if (semstate.expected_return_type->Basic.size > 0) {
            onyx_report_error(retnode->token->pos,
                "Returning from non-void function without value. Expected a value of type '%s'.",
                type_get_name(semstate.expected_return_type));
            return 1;
        }
    }

    return 0;
}

b32 check_if(AstIfWhile* ifnode) {
    if (ifnode->assignment != NULL) check_statement((AstNode *) ifnode->assignment);

    if (check_expression(&ifnode->cond)) return 1;

    if (!type_is_bool(ifnode->cond->type)) {
        onyx_report_error(ifnode->cond->token->pos, "expected boolean type for condition");
        return 1;
    }

    if (ifnode->true_stmt)  if (check_statement((AstNode *) ifnode->true_stmt))  return 1;
    if (ifnode->false_stmt) if (check_statement((AstNode *) ifnode->false_stmt)) return 1;

    return 0;
}

b32 check_while(AstIfWhile* whilenode) {
    if (whilenode->assignment != NULL) check_statement((AstNode *) whilenode->assignment);

    if (check_expression(&whilenode->cond)) return 1;

    if (!type_is_bool(whilenode->cond->type)) {
        onyx_report_error(whilenode->cond->token->pos, "expected boolean type for condition");
        return 1;
    }

    if (whilenode->true_stmt)  if (check_block(whilenode->true_stmt))  return 1;
    if (whilenode->false_stmt) if (check_block(whilenode->false_stmt)) return 1;

    return 0;
}

b32 check_for(AstFor* fornode) {
    if (check_expression(&fornode->iter)) return 1;
    fornode->loop_type = For_Loop_Invalid;

    Type* iter_type = fornode->iter->type;
    b32 can_iterate = 0;
    if (types_are_compatible(iter_type, builtin_range_type_type)) {
        if (fornode->by_pointer) {
            onyx_report_error(fornode->var->token->pos, "Cannot iterate by pointer over a range.");
            return 1;
        }

        can_iterate = 1;

        // NOTE: Blindly copy the first range member's type which will
        // be the low value.                - brendanfh 2020/09/04
        fornode->var->type = builtin_range_type_type->Struct.memarr[0]->type;
        fornode->var->flags |= Ast_Flag_Cannot_Take_Addr;
        fornode->loop_type = For_Loop_Range;

    }
    else if (iter_type->kind == Type_Kind_Array) {
        can_iterate = 1;

        if (fornode->by_pointer) fornode->var->type = type_make_pointer(semstate.node_allocator, iter_type->Array.elem);
        else                     fornode->var->type = iter_type->Array.elem;

        fornode->loop_type = For_Loop_Array;
    }
    else if (iter_type->kind == Type_Kind_Slice) {
        can_iterate = 1;

        if (fornode->by_pointer) fornode->var->type = iter_type->Slice.ptr_to_data;
        else                     fornode->var->type = iter_type->Slice.ptr_to_data->Pointer.elem;

        fornode->loop_type = For_Loop_Slice;

    }
    else if (iter_type->kind == Type_Kind_VarArgs) {
        if (fornode->by_pointer) {
            onyx_report_error(fornode->var->token->pos, "Cannot iterate by pointer over '%s'.", type_get_name(iter_type));
            return 1;
        }

        can_iterate = 1;

        fornode->var->type = iter_type->VarArgs.ptr_to_data->Pointer.elem;

        // NOTE: Slices are VarArgs are being treated the same here.
        fornode->loop_type = For_Loop_Slice;
    }
    else if (iter_type->kind == Type_Kind_DynArray) {
        can_iterate = 1;

        if (fornode->by_pointer) fornode->var->type = iter_type->DynArray.ptr_to_data;
        else                     fornode->var->type = iter_type->DynArray.ptr_to_data->Pointer.elem;

        fornode->loop_type = For_Loop_DynArr;
    }

    if (fornode->by_pointer)
        fornode->var->flags |= Ast_Flag_Cannot_Take_Addr;

    if (!can_iterate) {
        onyx_report_error(fornode->iter->token->pos,
                "Cannot iterate over a '%s'.",
                type_get_name(iter_type));
        return 1;
    }

    if (check_block(fornode->stmt)) return 1;

    return 0;
}

b32 check_switch(AstSwitch* switchnode) {
    if (switchnode->assignment != NULL) check_statement((AstNode *) switchnode->assignment);

    if (check_expression(&switchnode->expr)) return 1;
    if (!type_is_integer(switchnode->expr->type) && switchnode->expr->type->kind != Type_Kind_Enum) {
        onyx_report_error(switchnode->expr->token->pos, "expected integer or enum type for switch expression");
        return 1;
    }

    bh_imap_init(&switchnode->case_map, global_heap_allocator, bh_arr_length(switchnode->cases) * 2);

    switchnode->min_case = 0xffffffffffffffff;

    bh_arr_each(AstSwitchCase, sc, switchnode->cases) {
        if (check_block(sc->block)) return 1;
        if (check_expression(&sc->value)) return 1;

        if (sc->value->kind == Ast_Kind_Enum_Value) {
            sc->value = (AstTyped *) ((AstEnumValue *) sc->value)->value;
        }

        if (sc->value->kind != Ast_Kind_NumLit) {
            onyx_report_error(sc->value->token->pos, "case statement expected compile time known integer");
            return 1;
        }

        promote_numlit_to_larger((AstNumLit *) sc->value);

        u64 value = ((AstNumLit *) sc->value)->value.l;
        switchnode->min_case = bh_min(switchnode->min_case, value);
        switchnode->max_case = bh_max(switchnode->max_case, value);

        if (bh_imap_has(&switchnode->case_map, value)) {
            onyx_report_error(sc->value->token->pos, "Multiple cases for values '%d'.", value);
            return 1;
        }

        bh_imap_put(&switchnode->case_map, value, (u64) sc->block);
    }

    if (switchnode->default_case)
        check_block(switchnode->default_case);

    return 0;
}

static AstTyped* match_overloaded_function(AstCall* call, AstOverloadedFunction* ofunc) {
    bh_arr_each(AstTyped *, node, ofunc->overloads) {
        AstFunction* overload = (AstFunction *) *node;

        fill_in_type((AstTyped *) overload);

        TypeFunction* ol_type = &overload->type->Function;

        if (call->arg_count < ol_type->needed_param_count) continue;

        AstArgument* arg = call->arguments;
        Type** param_type = ol_type->params;
        while (arg != NULL) {
            fill_in_type((AstTyped *) arg);

            if ((*param_type)->kind == Type_Kind_VarArgs) {
                if (!types_are_compatible((*param_type)->VarArgs.ptr_to_data->Pointer.elem, arg->type))
                    goto no_match;
            }
            else if (!types_are_compatible(*param_type, arg->type)) goto no_match;

            param_type++;
            arg = (AstArgument *) arg->next;
        }

        return (AstTyped *) overload;

no_match:
        continue;
    }

    char* arg_str = bh_alloc(global_scratch_allocator, 1024);

    AstArgument* arg = call->arguments;
    while (arg != NULL) {
        strncat(arg_str, type_get_name(arg->type), 1023);

        if (arg->next != NULL)
            strncat(arg_str, ", ", 1023);

        arg = (AstArgument *) arg->next;
    }

    onyx_report_error(call->token->pos, "unable to match overloaded function with provided argument types: (%s)", arg_str);

    bh_free(global_scratch_allocator, arg_str);
    return NULL;
}

b32 check_call(AstCall* call) {
    // All the things that need to be done when checking a call node.
    //      1. Ensure the callee is not a symbol
    //      2. Check the callee expression (since it could be a variable or a field access, etc)
    //      3. Check all arguments
    //          * Cannot pass overloaded functions
    //          * Cannot pass a non-simple struct
    //      4. If callee is an overloaded function, use the argument types to determine which overload is used.
    //      5. If callee is polymorphic, use the arguments type to generate a polymorphic function.
    //      6. If an argument is polymorphic, generate the correct polymorphic function.
    //      7. Fill in default arguments
    //      8. If callee is an intrinsic, turn call into an Intrinsic_Call node
    //      9. Check types of formal and actual params against each other



    AstFunction* callee = (AstFunction *) call->callee;

    if (callee->kind == Ast_Kind_Symbol) {
        onyx_report_error(callee->token->pos, "Unresolved symbol '%b'.", callee->token->text, callee->token->length);
        return 1;
    }

    if (check_expression(&call->callee)) return 1;

    b32 has_polymorphic_args = 0;

    // NOTE: Check arguments
    AstArgument* actual = call->arguments;
    while (actual != NULL) {
        if (check_expression((AstTyped **) &actual)) return 1;

        if (actual->value->kind == Ast_Kind_Overloaded_Function) {
            onyx_report_error(actual->token->pos, "Cannot pass overloaded functions as parameters.");
            return 1;
        }

        if (type_is_structlike_strict(actual->value->type)) {
            if (!type_structlike_is_simple(actual->value->type)) {
                onyx_report_error(actual->token->pos,
                    "Can only pass simple structs as parameters (no nested structures). passing by pointer is the only way for now.");
                return 1;
            }
        }

        if (actual->value->kind == Ast_Kind_Polymorphic_Proc)
            has_polymorphic_args = 1;

        actual = (AstArgument *) actual->next;
    }

    if (callee->kind == Ast_Kind_Overloaded_Function) {
        call->callee = match_overloaded_function(call, (AstOverloadedFunction *) callee);
        callee = (AstFunction *) call->callee;

        if (callee == NULL) return 1;
    }

    if (callee->kind == Ast_Kind_Polymorphic_Proc) {
        call->callee = (AstTyped *) polymorphic_proc_lookup(
                (AstPolyProc *) call->callee,
                PPLM_By_Call,
                call,
                call->token->pos);

        if (call->callee == NULL) return 1;

        callee = (AstFunction *) call->callee;
    }

    // NOTE: Build callee's type
    fill_in_type((AstTyped *) callee);

    if (callee->type->kind != Type_Kind_Function) {
        onyx_report_error(call->token->pos,
                "Attempting to call something that is not a function, '%b'.",
                callee->token->text, callee->token->length);
        return 1;
    }

    if (has_polymorphic_args) {
        actual = call->arguments;
        u32 arg_idx = 0;

        while (actual != NULL) {
            if (actual->value->kind == Ast_Kind_Polymorphic_Proc) {
                actual->value = (AstTyped *) polymorphic_proc_lookup(
                    (AstPolyProc *) actual->value,
                    PPLM_By_Function_Type,
                    callee->type->Function.params[arg_idx],
                    actual->token->pos);

                if (actual->value == NULL) return 1;

                actual->type = actual->value->type;
                actual->value->flags |= Ast_Flag_Function_Used;
            }

            actual = (AstArgument *) actual->next;
            arg_idx++;
        }
    }

    if (callee->kind == Ast_Kind_Function) {
        if (call->arg_count < bh_arr_length(callee->params)) {
            AstArgument** last_arg = &call->arguments;
            while (*last_arg != NULL)
                last_arg = (AstArgument **) &(*last_arg)->next;

            while (call->arg_count < bh_arr_length(callee->params)
                && callee->params[call->arg_count].default_value != NULL) {
                AstTyped* dv = callee->params[call->arg_count].default_value;

                AstArgument* new_arg = onyx_ast_node_new(semstate.node_allocator, sizeof(AstArgument), Ast_Kind_Argument);
                new_arg->token = dv->token;
                new_arg->value = dv;
                new_arg->type = dv->type;
                new_arg->next = NULL;

                *last_arg = new_arg;
                last_arg = (AstArgument **) &(*last_arg)->next;

                call->arg_count++;
            }
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
            ((AstIntrinsicCall *)call)->intrinsic = bh_table_get(OnyxIntrinsic, intrinsic_table, intr_name);

        } else {
            onyx_report_error(callee->token->pos, "Intrinsic not supported, '%s'.", intr_name);
            token_toggle_end(callee->intrinsic_name);
            return 1;
        }

        token_toggle_end(callee->intrinsic_name);
    }

    call->type = callee->type->Function.return_type;

    Type **formal_params = callee->type->Function.params;
    actual = call->arguments;

    Type* variadic_type = NULL;
    AstParam* variadic_param = NULL;

    i32 arg_pos = 0;
    while (1) {
        if (actual == NULL) break;
        if (arg_pos >= callee->type->Function.param_count && variadic_type == NULL) break;

        if (variadic_type == NULL && formal_params[arg_pos]->kind == Type_Kind_VarArgs) {
            variadic_type = formal_params[arg_pos]->VarArgs.ptr_to_data->Pointer.elem;
            variadic_param = &callee->params[arg_pos];
        }

        if (variadic_type != NULL) {
            if (!types_are_compatible(variadic_type, actual->type)) {
                onyx_report_error(actual->token->pos,
                        "The function '%b' expects a value of type '%s' for the variadic parameter, '%b', got '%s'.",
                        callee->token->text, callee->token->length,
                        type_get_name(variadic_type),
                        variadic_param->local->token->text,
                        variadic_param->local->token->length,
                        type_get_name(actual->type));
                return 1;
            }

            actual->flags |= Ast_Flag_Arg_Is_VarArg;

        } else if (!types_are_compatible(formal_params[arg_pos], actual->type)) {
            onyx_report_error(actual->token->pos,
                    "The function '%b' expects a value of type '%s' for %d%s parameter, got '%s'.",
                    callee->token->text, callee->token->length,
                    type_get_name(formal_params[arg_pos]),
                    arg_pos + 1,
                    bh_num_suffix(arg_pos + 1),
                    type_get_name(actual->type));
            return 1;
        }

        arg_pos++;
        actual = (AstArgument *) actual->next;
    }

    if (arg_pos < callee->type->Function.param_count) {
        onyx_report_error(call->token->pos, "Too few arguments to function call.");
        return 1;
    }

    if (actual != NULL) {
        onyx_report_error(call->token->pos, "Too many arguments to function call.");
        return 1;
    }

    callee->flags |= Ast_Flag_Function_Used;

    return 0;
}

b32 check_binop_assignment(AstBinaryOp* binop, b32 assignment_is_ok) {
    if (!assignment_is_ok) {
        onyx_report_error(binop->token->pos, "Assignment not valid in expression.");
        return 1;
    }

    if (!is_lval((AstNode *) binop->left)) {
        onyx_report_error(binop->left->token->pos,
                "Cannot assign to '%b'.",
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if ((binop->left->flags & Ast_Flag_Const) != 0 && binop->left->type != NULL) {
        onyx_report_error(binop->token->pos,
                "Cannot assign to constant '%b.'.",
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (binop->operation == Binary_Op_Assign) {
        // NOTE: Raw assignment

        // NOTE: This is the 'type inference' system. Very stupid, but very easy.
        // If a left operand has an unknown type, fill it in with the type of
        // the right hand side.
        if (binop->left->type == NULL) binop->left->type = binop->right->type;

    } else {
        // NOTE: +=, -=, ...

        AstBinaryOp* binop_node = onyx_ast_node_new(
                semstate.node_allocator,
                sizeof(AstBinaryOp),
                Ast_Kind_Binary_Op);

        binop_node->token = binop->token;
        binop_node->left  = binop->left;
        binop_node->right = binop->right;
        binop_node->type  = binop->right->type;

        if      (binop->operation == Binary_Op_Assign_Add)      binop_node->operation = Binary_Op_Add;
        else if (binop->operation == Binary_Op_Assign_Minus)    binop_node->operation = Binary_Op_Minus;
        else if (binop->operation == Binary_Op_Assign_Multiply) binop_node->operation = Binary_Op_Multiply;
        else if (binop->operation == Binary_Op_Assign_Divide)   binop_node->operation = Binary_Op_Divide;
        else if (binop->operation == Binary_Op_Assign_Modulus)  binop_node->operation = Binary_Op_Modulus;
        else if (binop->operation == Binary_Op_Assign_And)      binop_node->operation = Binary_Op_And;
        else if (binop->operation == Binary_Op_Assign_Or)       binop_node->operation = Binary_Op_Or;
        else if (binop->operation == Binary_Op_Assign_Xor)      binop_node->operation = Binary_Op_Xor;
        else if (binop->operation == Binary_Op_Assign_Shl)      binop_node->operation = Binary_Op_Shl;
        else if (binop->operation == Binary_Op_Assign_Shr)      binop_node->operation = Binary_Op_Shr;
        else if (binop->operation == Binary_Op_Assign_Sar)      binop_node->operation = Binary_Op_Sar;

        binop->right = (AstTyped *) binop_node;
        binop->operation = Binary_Op_Assign;

        if (check_binaryop(&binop_node, 0)) return 1;
    }

    if (!types_are_compatible(binop->right->type, binop->left->type)) {
        onyx_report_error(binop->token->pos,
                "Cannot assign value of type '%s' to a '%s'.",
                type_get_name(binop->right->type),
                type_get_name(binop->left->type));
        return 1;
    }

    binop->type = &basic_types[Basic_Kind_Void];

    return 0;
}

b32 check_binaryop_compare(AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (binop->left->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (type_is_structlike_strict(binop->left->type)) {
        onyx_report_error(binop->token->pos, "Invalid type for left side of comparison operator.");
        return 1;
    }

    if (type_is_structlike_strict(binop->right->type)) {
        onyx_report_error(binop->token->pos, "Invalid type for right side of comparison operator.");
        return 1;
    }

    // HACK: Since ^... to rawptr is a one way conversion, strip any pointers
    // away so they can be compared as expected
    Type* ltype = binop->left->type;
    Type* rtype = binop->right->type;

    if (ltype->kind == Type_Kind_Pointer) ltype = &basic_types[Basic_Kind_Rawptr];
    if (rtype->kind == Type_Kind_Pointer) rtype = &basic_types[Basic_Kind_Rawptr];

    if (!types_are_compatible(ltype, rtype)) {
        onyx_report_error(binop->token->pos,
                "Cannot compare '%s' to '%s'.",
                type_get_name(ltype),
                type_get_name(rtype));
        return 1;
    }

    binop->type = &basic_types[Basic_Kind_Bool];
    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(semstate.node_allocator, (AstTyped *) binop);
    }

    return 0;
}

b32 check_binaryop_bool(AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (binop->left->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (!type_is_bool(binop->left->type) || !type_is_bool(binop->right->type)) {
        onyx_report_error(binop->token->pos, "Boolean operator expects boolean types for both operands.");
        return 1;
    }

    binop->type = &basic_types[Basic_Kind_Bool];

    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(semstate.node_allocator, (AstTyped *) binop);
    }
    return 0;
}

b32 check_binaryop(AstBinaryOp** pbinop, b32 assignment_is_ok) {
    AstBinaryOp* binop = *pbinop;

    if (check_expression(&binop->left)) return 1;
    if (check_expression(&binop->right)) return 1;

    if ((binop->left->flags & Ast_Flag_Comptime) && (binop->right->flags & Ast_Flag_Comptime)) {
        binop->flags |= Ast_Flag_Comptime;
    }

    if (binop_is_assignment(binop)) return check_binop_assignment(binop, assignment_is_ok);
    if (binop_is_compare(binop))    return check_binaryop_compare(pbinop);
    if (binop->operation == Binary_Op_Bool_And
        || binop->operation == Binary_Op_Bool_Or)
        return check_binaryop_bool(pbinop);

    if (binop->left->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_report_error(binop->token->pos,
                "Unable to resolve type for symbol '%b'.",
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (!type_is_numeric(binop->left->type) && !type_is_pointer(binop->left->type)) {
        onyx_report_error(binop->token->pos,
                "Expected numeric or pointer type for left side of binary operator, got '%s'.",
                type_get_name(binop->left->type));
        return 1;
    }

    if (!type_is_numeric(binop->right->type)) {
        onyx_report_error(binop->token->pos,
                "Expected numeric type for right side of binary operator, got '%s'.",
                type_get_name(binop->right->type));
        return 1;
    }

    if (type_is_pointer(binop->right->type)) {
        onyx_report_error(binop->token->pos, "Right side of binary operator is a pointer.");
        return 1;
    }

    if (binop->left->type->kind == Type_Kind_Basic
        && binop->left->type->Basic.kind == Basic_Kind_Rawptr
        && !binop_is_compare(binop)) {
        onyx_report_error(binop->token->pos, "Cannot operate on a 'rawptr'. Cast it to a another pointer type first.");
        return 1;
    }

    b32 lptr = type_is_pointer(binop->left->type);
    if (lptr && (binop->operation != Binary_Op_Add && binop->operation != Binary_Op_Minus)) {
        onyx_report_error(binop->token->pos, "This operator is not supported for these operands.");
        return 1;
    }

    if (lptr) {
        if (!type_is_integer(binop->right->type)) {
            onyx_report_error(binop->right->token->pos, "Expected integer type.");
            return 1;
        }

        AstNumLit* numlit = onyx_ast_node_new(
                semstate.node_allocator,
                sizeof(AstNumLit),
                Ast_Kind_NumLit);

        numlit->token = binop->right->token;
        numlit->type = binop->right->type;
        numlit->value.i = type_size_of(binop->left->type->Pointer.elem);

        AstBinaryOp* binop_node = onyx_ast_node_new(
                semstate.node_allocator,
                sizeof(AstBinaryOp),
                Ast_Kind_Binary_Op);

        binop_node->token = binop->token;
        binop_node->left  = binop->right;
        binop_node->right = (AstTyped *) numlit;
        binop_node->type  = binop->right->type;
        binop_node->operation = Binary_Op_Multiply;

        if (check_binaryop(&binop_node, 0)) return 1;

        binop->right = (AstTyped *) binop_node;
        binop->type = binop->left->type;
        binop->right->type = binop->left->type;
    }

    if (!types_are_compatible(binop->left->type, binop->right->type)) {
        onyx_report_error(binop->token->pos,
                "Mismatched types for binary operation. left: '%s', right: '%s'.",
                type_get_name(binop->left->type),
                type_get_name(binop->right->type));
        return 1;
    }

    binop->type = binop->left->type;

    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(semstate.node_allocator, (AstTyped *) binop);
    }
    return 0;
}

b32 check_unaryop(AstUnaryOp** punop) {
    AstUnaryOp* unaryop = *punop;

    if (check_expression(&unaryop->expr)) return 1;

    if (unaryop->operation != Unary_Op_Cast) {
        unaryop->type = unaryop->expr->type;
    }

    if (unaryop->expr->flags & Ast_Flag_Comptime) {
        unaryop->flags |= Ast_Flag_Comptime;
        // NOTE: Not a unary op
        *punop = (AstUnaryOp *) ast_reduce(semstate.node_allocator, (AstTyped *) unaryop);
    }

    return 0;
}

b32 check_struct_literal(AstStructLiteral* sl) {
    fill_in_type((AstTyped *) sl);

    u32 mem_count = type_structlike_mem_count(sl->type);

    if (mem_count != bh_arr_length(sl->values)) {
        onyx_report_error(sl->token->pos,
                "'%s' expects %d values, given %d.",
                type_get_name(sl->type),
                mem_count,
                bh_arr_length(sl->values));
        return 1;
    }

    AstTyped** actual = sl->values;
    StructMember smem;

    fori (i, 0, mem_count) {
        if (check_expression(actual)) return 1;

        // NOTE: Not checking the return on this function because
        // this for loop is bounded by the number of members in the
        // type.
        type_lookup_member_by_idx(sl->type, i, &smem);
        Type* formal = smem.type;

        // NOTE: We may want to report a different error if the struct member was named,
        // since those names may not be in the correct order. - brendanfh   2020/09/12
        if (!types_are_compatible(formal, (*actual)->type)) {
            onyx_report_error(sl->token->pos,
                    "Mismatched types for %d%s member named '%s', expected '%s', got '%s'.",
                    i + 1, bh_num_suffix(i + 1),
                    smem.name,
                    type_get_name(formal),
                    type_get_name((*actual)->type));
            return 1;
        }

        actual++;
    }

    return 0;
}

b32 check_address_of(AstAddressOf* aof) {
    if (check_expression(&aof->expr)) return 1;

    if ((aof->expr->kind != Ast_Kind_Array_Access
            && aof->expr->kind != Ast_Kind_Dereference
            && aof->expr->kind != Ast_Kind_Field_Access
            && aof->expr->kind != Ast_Kind_Memres
            && aof->expr->kind != Ast_Kind_Local)
            || (aof->expr->flags & Ast_Flag_Cannot_Take_Addr) != 0) {
        onyx_report_error(aof->token->pos, "Cannot take the address of value.");
        return 1;
    }

    aof->expr->flags |= Ast_Flag_Address_Taken;

    aof->type = type_make_pointer(semstate.allocator, aof->expr->type);

    return 0;
}

b32 check_dereference(AstDereference* deref) {
    if (check_expression(&deref->expr)) return 1;

    if (!type_is_pointer(deref->expr->type)) {
        onyx_report_error(deref->token->pos, "Cannot dereference non-pointer value.");
        return 1;
    }

    if (deref->expr->type == basic_type_rawptr.type) {
        onyx_report_error(deref->token->pos, "Cannot dereference 'rawptr'. Cast to another pointer type first.");
        return 1;
    }

    deref->type = deref->expr->type->Pointer.elem;

    return 0;
}

b32 check_array_access(AstArrayAccess* aa) {
    if (check_expression(&aa->addr)) return 1;
    if (check_expression(&aa->expr)) return 1;

    if (!type_is_array_accessible(aa->addr->type)) {
        onyx_report_error(aa->token->pos,
                "Expected pointer or array type for left of array access, got '%s'.",
                type_get_name(aa->addr->type));
        return 1;
    }

    if (types_are_compatible(aa->expr->type, builtin_range_type_type)) {
        Type *of = NULL;
        if (aa->addr->type->kind == Type_Kind_Pointer)
            of = aa->addr->type->Pointer.elem;
        else if (aa->addr->type->kind == Type_Kind_Array)
            of = aa->addr->type->Array.elem;
        else {
            onyx_report_error(aa->token->pos, "Invalid type for left of slice creation.");
            return 1;
        }

        aa->kind = Ast_Kind_Slice;
        aa->type = type_make_slice(semstate.node_allocator, of);
        aa->elem_size = type_size_of(of);

        return 0;
    }

    if (aa->expr->type->kind != Type_Kind_Basic
            || (aa->expr->type->Basic.kind != Basic_Kind_I32 && aa->expr->type->Basic.kind != Basic_Kind_U32)) {
        onyx_report_error(aa->token->pos, "Expected type u32 or i32 for index.");
        return 1;
    }

    if (aa->addr->type->kind == Type_Kind_Pointer)
        aa->type = aa->addr->type->Pointer.elem;
    else if (aa->addr->type->kind == Type_Kind_Array)
        aa->type = aa->addr->type->Array.elem;
    else if (aa->addr->type->kind == Type_Kind_Slice
            || aa->addr->type->kind == Type_Kind_DynArray
            || aa->addr->type->kind == Type_Kind_VarArgs) {
        // If we are accessing on a slice or a dynamic array, implicitly add a field access for the data member

        StructMember smem;
        type_lookup_member(aa->addr->type, "data", &smem);

        AstFieldAccess* fa = onyx_ast_node_new(semstate.node_allocator, sizeof(AstFieldAccess), Ast_Kind_Field_Access);
        fa->token = aa->addr->token;
        fa->type = smem.type;
        fa->offset = smem.offset;
        fa->idx = smem.idx;
        fa->expr = aa->addr;

        aa->addr = (AstTyped *) fa;
        aa->type = aa->addr->type->Pointer.elem;
    }
    else {
        onyx_report_error(aa->token->pos, "Invalid type for left of array access.");
        return 1;
    }

    aa->elem_size = type_size_of(aa->type);

    return 0;
}

b32 check_field_access(AstFieldAccess** pfield) {
    AstFieldAccess* field = *pfield;
    if (check_expression(&field->expr)) return 1;
    if (field->expr->type == NULL) return 1;

    if (!type_is_structlike(field->expr->type)) {
        onyx_report_error(field->token->pos,
            "Cannot access field '%b' on '%s'. Type is not a struct.",
            field->token->text,
            field->token->length,
            type_get_name(field->expr->type));
        return 1;
    }

    token_toggle_end(field->token);
    StructMember smem;
    if (!type_lookup_member(field->expr->type, field->token->text, &smem)) {
        onyx_report_error(field->token->pos,
            "Field '%s' does not exists on '%s'.",
            field->token->text,
            type_get_name(field->expr->type));
        token_toggle_end(field->token);
        return 1;
    }

    field->offset = smem.offset;
    field->idx = smem.idx;
    field->type = smem.type;

    token_toggle_end(field->token);
    return 0;
}

b32 check_range_literal(AstBinaryOp** prange) {
    AstBinaryOp* range = *prange;
    if (check_expression(&range->left))  return 1;
    if (check_expression(&range->right)) return 1;

    Type* expected_range_type = builtin_range_type_type;
    StructMember smem;

    type_lookup_member(expected_range_type, "low", &smem);
    if (!types_are_compatible(range->left->type, smem.type)) {
        onyx_report_error(range->token->pos, "Expected left side of range to be a 32-bit integer.");
        return 1;
    }

    type_lookup_member(expected_range_type, "high", &smem);
    if (!types_are_compatible(range->right->type, smem.type)) {
        onyx_report_error(range->token->pos, "Expected right side of range to be a 32-bit integer.");
        return 1;
    }

    // NOTE: Implicitly converting this to a struct literal because that makes the
    // WASM generation easier and more robust for return a range from a procedure
    // and the like. This could be improved as struct literals are made more throughout
    // the code base.
    //                                                          - brendanfh, 2020/09/03
    AstStructLiteral* rsl = onyx_ast_node_new(semstate.node_allocator, sizeof(AstStructLiteral), Ast_Kind_Struct_Literal);
    bh_arr_new(global_heap_allocator, rsl->values, 3);

    bh_arr_insert_end(rsl->values, 2);
    type_lookup_member(expected_range_type, "low", &smem);
    rsl->values[smem.idx] = range->left;
    type_lookup_member(expected_range_type, "high", &smem);
    rsl->values[smem.idx] = range->right;

    // HACK: This relies on the third member of the 'range' struct to exist, be the step,
    // and have an intial_value.
    AstStructMember* step_member = ((AstStructType *) builtin_range_type)->members[2];
    if (check_expression(&step_member->initial_value)) return 1;
    bh_arr_push(rsl->values, step_member->initial_value);

    rsl->token = range->token;
    rsl->type = expected_range_type;

    // NOTE: Not a binary op
    *prange = (AstBinaryOp *) rsl;

    return 0;
}

b32 check_size_of(AstSizeOf* so) {
    so->size = type_size_of(type_build_from_ast(semstate.allocator, so->so_type));

    return 0;
}

b32 check_align_of(AstAlignOf* ao) {
    ao->alignment = type_alignment_of(type_build_from_ast(semstate.allocator, ao->ao_type));

    return 0;
}

b32 check_expression(AstTyped** pexpr) {
    AstTyped* expr = *pexpr;
    if (expr->kind > Ast_Kind_Type_Start && expr->kind < Ast_Kind_Type_End) {
        onyx_report_error(expr->token->pos, "Type used as part of an expression.");
        return 1;
    }

    fill_in_type(expr);

    i32 retval = 0;
    switch (expr->kind) {
        case Ast_Kind_Binary_Op: retval = check_binaryop((AstBinaryOp **) pexpr, 0); break;
        case Ast_Kind_Unary_Op:  retval = check_unaryop((AstUnaryOp **) pexpr); break;

        case Ast_Kind_Call:  retval = check_call((AstCall *) expr); break;
        case Ast_Kind_Block: retval = check_block((AstBlock *) expr); break;

        case Ast_Kind_Symbol:
            onyx_report_error(expr->token->pos,
                    "Unable to resolve symbol '%b'.",
                    expr->token->text, expr->token->length);
            retval = 1;
            break;

        case Ast_Kind_Param:
            if (expr->type == NULL) {
                onyx_report_error(expr->token->pos, "Parameter with unknown type. You should hopefully never see this.");
                retval = 1;
            }
            break;

        case Ast_Kind_Local: break;

        case Ast_Kind_Address_Of:   retval = check_address_of((AstAddressOf *) expr); break;
        case Ast_Kind_Dereference:  retval = check_dereference((AstDereference *) expr); break;
        case Ast_Kind_Slice:
        case Ast_Kind_Array_Access: retval = check_array_access((AstArrayAccess *) expr); break;
        case Ast_Kind_Field_Access: retval = check_field_access((AstFieldAccess **) pexpr); break;
        case Ast_Kind_Size_Of:      retval = check_size_of((AstSizeOf *) expr); break;
        case Ast_Kind_Align_Of:     retval = check_align_of((AstAlignOf *) expr); break;
        case Ast_Kind_Range:        retval = check_range_literal((AstBinaryOp **) pexpr); break;

        case Ast_Kind_Global:
            if (expr->type == NULL) {
                onyx_report_error(expr->token->pos, "Global with unknown type.");
                retval = 1;
            }
            break;

        case Ast_Kind_Argument:
            retval = check_expression(&((AstArgument *) expr)->value);
            expr->type = ((AstArgument *) expr)->value->type;
            break;

        case Ast_Kind_NumLit:
            // NOTE: Literal types should have been decided
            // in the parser (for now).
            assert(expr->type != NULL);
            break;

        case Ast_Kind_Struct_Literal:
            retval = check_struct_literal((AstStructLiteral *) expr);
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

            expr->flags |= Ast_Flag_Function_Used;
            break;

        case Ast_Kind_StrLit: break;
        case Ast_Kind_File_Contents: break;
        case Ast_Kind_Overloaded_Function: break;
        case Ast_Kind_Enum_Value: break;

        case Ast_Kind_Memres: break;

        case Ast_Kind_Polymorphic_Proc: break;

        case Ast_Kind_Package: break;

        case Ast_Kind_Error: break;

        default:
            retval = 1;
            DEBUG_HERE;
            break;
    }

    return retval;
}

b32 check_global(AstGlobal* global) {
    fill_in_type((AstTyped *) global);

    if (global->type == NULL) {
        onyx_report_error(global->token->pos,
                "Unable to resolve type for global '%b'.",
                global->exported_name->text,
                global->exported_name->length);

        return 1;
    }

    return 0;
}

b32 check_statement(AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Jump:      return 0;

        case Ast_Kind_Return:     return check_return((AstReturn *) stmt);
        case Ast_Kind_If:         return check_if((AstIfWhile *) stmt);
        case Ast_Kind_While:      return check_while((AstIfWhile *) stmt);
        case Ast_Kind_For:        return check_for((AstFor *) stmt);
        case Ast_Kind_Switch:     return check_switch((AstSwitch *) stmt);
        case Ast_Kind_Block:      return check_block((AstBlock *) stmt);
        case Ast_Kind_Defer:      return check_statement(((AstDefer *) stmt)->stmt);

        case Ast_Kind_Binary_Op:
            stmt->flags |= Ast_Flag_Expr_Ignored;
            return check_binaryop((AstBinaryOp **) &stmt, 1);

        default:
            stmt->flags |= Ast_Flag_Expr_Ignored;
            return check_expression((AstTyped **) &stmt);
    }
}

b32 check_statement_chain(AstNode* start) {
    while (start) {
        if (check_statement(start)) return 1;
        start = start->next;
    }

    return 0;
}

b32 check_block(AstBlock* block) {
    if (check_statement_chain(block->body)) return 1;

    bh_table_each_start(AstTyped *, block->scope->symbols);
        fill_in_type(value);

        if (value->type == NULL) {
            onyx_report_error(value->token->pos,
                    "Unable to resolve type for local '%b'.",
                    value->token->text, value->token->length);
            return 1;
        }
    bh_table_each_end;

    return 0;
}

b32 check_function(AstFunction* func) {
    semstate.expected_return_type = func->type->Function.return_type;
    if (func->body) {
        return check_block(func->body);
    }

    return 0;
}

b32 check_overloaded_function(AstOverloadedFunction* func) {
    bh_arr_each(AstTyped *, node, func->overloads) {
        if ((*node)->kind == Ast_Kind_Overloaded_Function) {
            onyx_report_error((*node)->token->pos, "Overload option can not be another overloaded function.");

            return 1;
        }

        if ((*node)->kind != Ast_Kind_Function) {
            onyx_report_error((*node)->token->pos, "Overload option not function.");

            return 1;
        }
    }

    return 0;
}

b32 check_struct(AstStructType* s_node) {
    bh_table(i32) mem_set;
    bh_table_init(global_heap_allocator, mem_set, bh_arr_length(s_node->members));

    bh_arr_each(AstStructMember *, member, s_node->members) {
        token_toggle_end((*member)->token);

        if (bh_table_has(i32, mem_set, (*member)->token->text)) {
            onyx_report_error((*member)->token->pos,
                    "Duplicate struct member '%s'.",
                    (*member)->token->text);

            token_toggle_end((*member)->token);
            return 1;
        }

        bh_table_put(i32, mem_set, (*member)->token->text, 1);
        token_toggle_end((*member)->token);
    }

    bh_table_free(mem_set);

    // NOTE: fills in the stcache
    type_build_from_ast(semstate.allocator, (AstType *) s_node);

    return 0;
}

b32 check_function_header(AstFunction* func) {
    b32 expect_default_param = 0;
    b32 has_had_varargs = 0;

    bh_arr_each(AstParam, param, func->params) {
        AstLocal* local = param->local;

        if (expect_default_param && param->default_value == NULL) {
            onyx_report_error(local->token->pos,
                    "All parameters must have default values after the first default valued parameter.");
            return 1;
        }

        if (has_had_varargs && param->is_vararg) {
            onyx_report_error(local->token->pos,
                    "Can only have one param that is of variable argument type.");
            return 1;
        }

        if (has_had_varargs && !param->is_vararg) {
            onyx_report_error(local->token->pos,
                    "Variable arguments must be last in parameter list");
            return 1;
        }

        if (param->default_value != NULL) expect_default_param = 1;

        fill_in_type((AstTyped *) local);

        if (local->type == NULL) {
            onyx_report_error(local->token->pos, "Function parameter types must be known.");
            return 1;
        }

        if (param->is_vararg) has_had_varargs = 1;

        if (local->type->kind != Type_Kind_Array
            && type_size_of(local->type) == 0) {
            onyx_report_error(local->token->pos, "Function parameters cannot have zero-width types.");
            return 1;
        }
    }

    func->type = type_build_function_type(semstate.node_allocator, func);

    if ((func->flags & Ast_Flag_Exported) != 0) {
        if ((func->flags & Ast_Flag_Foreign) != 0) {
            onyx_report_error(func->token->pos, "exporting a foreign function");
            return 1;
        }

        if ((func->flags & Ast_Flag_Intrinsic) != 0) {
            onyx_report_error(func->token->pos, "exporting a intrinsic function");
            return 1;
        }

        if (func->exported_name == NULL) {
            onyx_report_error(func->token->pos, "exporting function without a name");
            return 1;
        }
    }

    return 0;
}

b32 check_memres(AstMemRes* memres) {
    fill_in_type((AstTyped *) memres);

    if (memres->initial_value != NULL) {
        fill_in_type(memres->initial_value);
        check_expression(&memres->initial_value);

        if ((memres->initial_value->flags & Ast_Flag_Comptime) == 0) {
            onyx_report_error(memres->initial_value->token->pos, "Top level expressions must be compile time known.");
            return 1;
        }

        Type* memres_type = memres->type;

        if (!types_are_compatible(memres_type, memres->initial_value->type)) {
            onyx_report_error(memres->token->pos,
                    "Cannot assign value of type '%s' to a '%s'.",
                    type_get_name(memres_type),
                    type_get_name(memres->initial_value->type));
            return 1;
        }
    }

    return 0;
}

b32 check_node(AstNode* node) {
    switch (node->kind) {
        case Ast_Kind_Function:             return check_function((AstFunction *) node);
        case Ast_Kind_Overloaded_Function:  return check_overloaded_function((AstOverloadedFunction *) node);
        case Ast_Kind_Block:                return check_block((AstBlock *) node);
        case Ast_Kind_Return:               return check_return((AstReturn *) node);
        case Ast_Kind_If:                   return check_if((AstIfWhile *) node);
        case Ast_Kind_While:                return check_while((AstIfWhile *) node);
        case Ast_Kind_Call:                 return check_call((AstCall *) node);
        case Ast_Kind_Binary_Op:            return check_binaryop((AstBinaryOp **) &node, 1);
        default:                            return check_expression((AstTyped **) &node);
    }
}

void onyx_type_check() {
    bh_arr_each(Entity, entity, semstate.program->entities) {
        switch (entity->type) {
            case Entity_Type_Foreign_Function_Header:
            case Entity_Type_Function_Header:
                if (check_function_header(entity->function)) return;
                break;

            case Entity_Type_Function:
                if (check_function(entity->function)) return;
                break;

            case Entity_Type_Overloaded_Function:
                if (check_overloaded_function(entity->overloaded_function)) return;
                break;

            case Entity_Type_Global:
                if (entity->global->flags & Ast_Flag_Foreign)
                    semstate.program->foreign_global_count++;

                if (check_global(entity->global)) return;
                break;

            case Entity_Type_Expression:
                if (check_expression(&entity->expr)) return;
                break;

            case Entity_Type_Type_Alias:
                if (entity->type_alias->kind == Ast_Kind_Struct_Type)
                    if (check_struct((AstStructType *) entity->type_alias)) return;
                break;

            case Entity_Type_Memory_Reservation:
                if (check_memres(entity->mem_res)) return;
                break;

            case Entity_Type_Enum: break;

            case Entity_Type_String_Literal: break;

            case Entity_Type_File_Contents: break;

            case Entity_Type_Global_Header: break;

            case Entity_Type_Use_Package: break;

            case Entity_Type_Polymorphic_Proc: break;

            default: DEBUG_HERE; break;
        }
    }
}
