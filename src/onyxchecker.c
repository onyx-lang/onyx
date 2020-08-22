#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxparser.h"
#include "onyxutils.h"

#define CHECK(kind, ...) static b32 check_ ## kind (__VA_ARGS__)

CHECK(block, AstBlock* block);
CHECK(statement_chain, AstNode* start);
CHECK(statement, AstNode* stmt);
CHECK(return, AstReturn* retnode);
CHECK(if, AstIf* ifnode);
CHECK(while, AstWhile* whilenode);
CHECK(for, AstFor* fornode);
CHECK(call, AstCall* call);
CHECK(binaryop, AstBinaryOp** pbinop, b32 assignment_is_ok);
CHECK(unaryop, AstUnaryOp** punop);
CHECK(struct_literal, AstStructLiteral* sl);
CHECK(expression, AstTyped** expr);
CHECK(address_of, AstAddressOf* aof);
CHECK(dereference, AstDereference* deref);
CHECK(array_access, AstArrayAccess* expr);
CHECK(field_access, AstFieldAccess** pfield);
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

CHECK(return, AstReturn* retnode) {
    if (retnode->expr) {
        if (check_expression(&retnode->expr)) return 1;

        if (!types_are_compatible(retnode->expr->type, semstate.expected_return_type)) {
            onyx_message_add(Msg_Type_Function_Return_Mismatch,
                    retnode->expr->token->pos,
                    type_get_name(retnode->expr->type),
                    type_get_name(semstate.expected_return_type));
            return 1;
        }
    } else {
        if (semstate.expected_return_type->Basic.size > 0) {
            onyx_message_add(Msg_Type_Literal,
                    retnode->token->pos,
                    "returning from non-void function without value");
            return 1;
        }
    }

    return 0;
}

CHECK(if, AstIf* ifnode) {
    if (check_expression(&ifnode->cond)) return 1;

    if (!type_is_bool(ifnode->cond->type)) {
        onyx_message_add(Msg_Type_Literal,
                ifnode->cond->token->pos,
                "expected boolean type for condition");
        return 1;
    }

    if (ifnode->true_stmt)  if (check_statement((AstNode *) ifnode->true_stmt))  return 1;
    if (ifnode->false_stmt) if (check_statement((AstNode *) ifnode->false_stmt)) return 1;

    return 0;
}

CHECK(while, AstWhile* whilenode) {
    if (check_expression(&whilenode->cond)) return 1;

    if (!type_is_bool(whilenode->cond->type)) {
        onyx_message_add(Msg_Type_Literal,
                whilenode->cond->token->pos,
                "expected boolean type for condition");
        return 1;
    }

    return check_block(whilenode->stmt);
}

CHECK(for, AstFor* fornode) {
    if (check_expression(&fornode->start)) return 1;
    if (check_expression(&fornode->end)) return 1;
    if (check_expression(&fornode->step)) return 1;

    if (fornode->var->type_node == NULL || fornode->var->type_node != fornode->start->type_node)
        fornode->var->type_node = fornode->start->type_node;
    fill_in_type((AstTyped *) fornode->var);

    if (!type_is_integer(fornode->start->type)) {
        onyx_message_add(Msg_Type_Literal,
                fornode->start->token->pos,
                "expected expression of integer type for start");
        return 1;
    }

    if (!type_is_integer(fornode->end->type)) {
        onyx_message_add(Msg_Type_Literal,
                fornode->end->token->pos,
                "expected expression of integer type for end");
        return 1;
    }

    if (!type_is_integer(fornode->step->type)) {
        onyx_message_add(Msg_Type_Literal,
                fornode->step->token->pos,
                "expected expression of integer type for step");
        return 1;
    }

    // NOTE: Auto promote implicit step to the type of start
    if (fornode->step->kind == Ast_Kind_NumLit) {
        fornode->step->type_node = fornode->start->type_node;
        fornode->step->type = fornode->start->type;
        promote_numlit_to_larger((AstNumLit *) fornode->step);
    }

    if (!types_are_compatible(fornode->end->type, fornode->start->type)) {
        onyx_message_add(Msg_Type_Literal,
                fornode->end->token->pos,
                "type of end does not match type of start");
        return 1;
    }

    if (!types_are_compatible(fornode->step->type, fornode->start->type)) {
        onyx_message_add(Msg_Type_Literal,
                fornode->start->token->pos,
                "type of step does not match type of start");
        return 1;
    }


    if (check_block(fornode->stmt)) return 1;

    return 0;
}

static AstTyped* match_overloaded_function(AstCall* call, AstOverloadedFunction* ofunc) {
    bh_arr_each(AstTyped *, node, ofunc->overloads) {
        AstFunction* overload = (AstFunction *) *node;

        fill_in_type((AstTyped *) overload);

        TypeFunction* ol_type = &overload->type->Function;

        if (ol_type->param_count != call->arg_count) continue;

        AstArgument* arg = call->arguments;
        Type** param_type = ol_type->params;
        while (arg != NULL) {
            fill_in_type((AstTyped *) arg);

            if (!types_are_compatible(*param_type, arg->type)) goto no_match;

            param_type++;
            arg = (AstArgument *) arg->next;
        }

        return (AstTyped *) overload;

no_match:
        continue;
    }

    onyx_message_add(Msg_Type_Literal,
            call->token->pos,
            "unable to match overloaded function");

    return NULL;
}

CHECK(call, AstCall* call) {
    AstFunction* callee = (AstFunction *) call->callee;

    if (callee->kind == Ast_Kind_Symbol) {
        onyx_message_add(Msg_Type_Unresolved_Symbol,
                callee->token->pos,
                callee->token->text, callee->token->length);
        return 1;
    }

    if (check_expression(&call->callee)) return 1;

    // NOTE: Check arguments
    AstNode** prev_param = (AstNode **) &call->arguments;
    AstArgument* actual_param = call->arguments;
    while (actual_param != NULL) {
        if (check_expression((AstTyped **) &actual_param)) return 1;

        if (actual_param->value->kind == Ast_Kind_Overloaded_Function) {
            onyx_message_add(Msg_Type_Literal,
                    actual_param->token->pos,
                    "cannot pass overloaded functions as parameters.");
            return 1;
        }

        prev_param = (AstNode **) &actual_param->next;
        actual_param = (AstArgument *) actual_param->next;
    }

    if (callee->kind == Ast_Kind_Overloaded_Function) {
        call->callee = match_overloaded_function(call, (AstOverloadedFunction *) callee);
        callee = (AstFunction *) call->callee;

        if (callee == NULL) return 1;
    }

    // NOTE: Build callee's type
    fill_in_type((AstTyped *) callee);

    if (callee->type->kind != Type_Kind_Function) {
        onyx_message_add(Msg_Type_Call_Non_Function,
                call->token->pos,
                callee->token->text, callee->token->length);
        return 1;
    }

    // NOTE: If we calling an intrinsic function, translate the
    // call into an intrinsic call node.
    if (callee->flags & Ast_Flag_Intrinsic) {
        call->kind = Ast_Kind_Intrinsic_Call;
        call->callee = NULL;

        token_toggle_end(callee->intrinsic_name);

        char* intr_name = callee->intrinsic_name->text;
        OnyxIntrinsic intrinsic = ONYX_INTRINSIC_UNDEFINED;

        if (!strcmp("memory_size", intr_name))       intrinsic = ONYX_INTRINSIC_MEMORY_SIZE;
        else if (!strcmp("memory_grow", intr_name))  intrinsic = ONYX_INTRINSIC_MEMORY_GROW;

        else if (!strcmp("clz_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_CLZ;
        else if (!strcmp("ctz_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_CTZ;
        else if (!strcmp("popcnt_i32", intr_name))   intrinsic = ONYX_INTRINSIC_I32_POPCNT;
        else if (!strcmp("and_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_AND;
        else if (!strcmp("or_i32", intr_name))       intrinsic = ONYX_INTRINSIC_I32_OR;
        else if (!strcmp("xor_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_XOR;
        else if (!strcmp("shl_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_SHL;
        else if (!strcmp("slr_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_SLR;
        else if (!strcmp("sar_i32", intr_name))      intrinsic = ONYX_INTRINSIC_I32_SAR;
        else if (!strcmp("rotl_i32", intr_name))     intrinsic = ONYX_INTRINSIC_I32_ROTL;
        else if (!strcmp("rotr_i32", intr_name))     intrinsic = ONYX_INTRINSIC_I32_ROTR;

        else if (!strcmp("clz_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_CLZ;
        else if (!strcmp("ctz_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_CTZ;
        else if (!strcmp("popcnt_i64", intr_name))   intrinsic = ONYX_INTRINSIC_I64_POPCNT;
        else if (!strcmp("and_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_AND;
        else if (!strcmp("or_i64", intr_name))       intrinsic = ONYX_INTRINSIC_I64_OR;
        else if (!strcmp("xor_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_XOR;
        else if (!strcmp("shl_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_SHL;
        else if (!strcmp("slr_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_SLR;
        else if (!strcmp("sar_i64", intr_name))      intrinsic = ONYX_INTRINSIC_I64_SAR;
        else if (!strcmp("rotl_i64", intr_name))     intrinsic = ONYX_INTRINSIC_I64_ROTL;
        else if (!strcmp("rotr_i64", intr_name))     intrinsic = ONYX_INTRINSIC_I64_ROTR;

        else if (!strcmp("abs_f32", intr_name))      intrinsic = ONYX_INTRINSIC_F32_ABS;
        else if (!strcmp("ceil_f32", intr_name))     intrinsic = ONYX_INTRINSIC_F32_CEIL;
        else if (!strcmp("floor_f32", intr_name))    intrinsic = ONYX_INTRINSIC_F32_FLOOR;
        else if (!strcmp("trunc_f32", intr_name))    intrinsic = ONYX_INTRINSIC_F32_TRUNC;
        else if (!strcmp("nearest_f32", intr_name))  intrinsic = ONYX_INTRINSIC_F32_NEAREST;
        else if (!strcmp("sqrt_f32", intr_name))     intrinsic = ONYX_INTRINSIC_F32_SQRT;
        else if (!strcmp("min_f32", intr_name))      intrinsic = ONYX_INTRINSIC_F32_MIN;
        else if (!strcmp("max_f32", intr_name))      intrinsic = ONYX_INTRINSIC_F32_MAX;
        else if (!strcmp("copysign_f32", intr_name)) intrinsic = ONYX_INTRINSIC_F32_COPYSIGN;

        else if (!strcmp("abs_f64", intr_name))      intrinsic = ONYX_INTRINSIC_F64_ABS;
        else if (!strcmp("ceil_f64", intr_name))     intrinsic = ONYX_INTRINSIC_F64_CEIL;
        else if (!strcmp("floor_f64", intr_name))    intrinsic = ONYX_INTRINSIC_F64_FLOOR;
        else if (!strcmp("trunc_f64", intr_name))    intrinsic = ONYX_INTRINSIC_F64_TRUNC;
        else if (!strcmp("nearest_f64", intr_name))  intrinsic = ONYX_INTRINSIC_F64_NEAREST;
        else if (!strcmp("sqrt_f64", intr_name))     intrinsic = ONYX_INTRINSIC_F64_SQRT;
        else if (!strcmp("min_f64", intr_name))      intrinsic = ONYX_INTRINSIC_F64_MIN;
        else if (!strcmp("max_f64", intr_name))      intrinsic = ONYX_INTRINSIC_F64_MAX;
        else if (!strcmp("copysign_f64", intr_name)) intrinsic = ONYX_INTRINSIC_F64_COPYSIGN;

        ((AstIntrinsicCall *)call)->intrinsic = intrinsic;

        token_toggle_end(callee->intrinsic_name);
    }

    call->type = callee->type->Function.return_type;

    Type** formal_param = &callee->type->Function.params[0];
    actual_param = call->arguments;

    i32 arg_pos = 0;
    while (formal_param != NULL && actual_param != NULL) {
        if (!types_are_compatible(*formal_param, actual_param->type)) {
            onyx_message_add(Msg_Type_Function_Param_Mismatch,
                    actual_param->token->pos,
                    callee->token->text, callee->token->length,
                    type_get_name(*formal_param),
                    arg_pos,
                    type_get_name(actual_param->type));
            return 1;
        }

        arg_pos++;
        formal_param++;
        actual_param = (AstArgument *) actual_param->next;
    }

    if (arg_pos < callee->type->Function.param_count) {
        onyx_message_add(Msg_Type_Literal,
                call->token->pos,
                "too few arguments to function call");
        return 1;
    }

    if (arg_pos > callee->type->Function.param_count) {
        onyx_message_add(Msg_Type_Literal,
                call->token->pos,
                "too many arguments to function call");
        return 1;
    }

    callee->flags |= Ast_Flag_Function_Used;

    return 0;
}

CHECK(binop_assignment, AstBinaryOp* binop, b32 assignment_is_ok) {
    if (!assignment_is_ok) {
        onyx_message_add(Msg_Type_Literal,
            binop->token->pos,
            "assignment not valid in expression");
        return 1;
    }

    if (!is_lval((AstNode *) binop->left)) {
        onyx_message_add(Msg_Type_Not_Lval,
                binop->left->token->pos,
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if ((binop->left->flags & Ast_Flag_Const) != 0 && binop->left->type != NULL) {
        onyx_message_add(Msg_Type_Assign_Const,
                binop->token->pos,
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
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

    if (!types_are_compatible(binop->left->type, binop->right->type)) {
        onyx_message_add(Msg_Type_Assignment_Mismatch,
                binop->token->pos,
                type_get_name(binop->left->type),
                type_get_name(binop->right->type));
        return 1;
    }

    binop->type = &basic_types[Basic_Kind_Void];

    return 0;
}

CHECK(binaryop_compare, AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (binop->left->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (binop->left->type->kind == Type_Kind_Struct) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "invalid type for left side of binary operator");
        return 1;
    }

    if (binop->right->type->kind == Type_Kind_Struct) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "invalid type for right side of binary operator");
        return 1;
    }

    if (!types_are_compatible(binop->left->type, binop->right->type)) {
        onyx_message_add(Msg_Type_Binop_Mismatch,
                binop->token->pos,
                type_get_name(binop->left->type),
                type_get_name(binop->right->type));
        return 1;
    }

    binop->type = &basic_types[Basic_Kind_Bool];
    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(semstate.node_allocator, (AstTyped *) binop);
    }
    
    return 0;
}

CHECK(binaryop_bool, AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (binop->left->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (!type_is_bool(binop->left->type) || !type_is_bool(binop->right->type)) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "boolean operator expects boolean types for both operands");
        return 1;
    }

    binop->type = &basic_types[Basic_Kind_Bool];

    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(semstate.node_allocator, (AstTyped *) binop);
    }
    return 0;
}

CHECK(binaryop, AstBinaryOp** pbinop, b32 assignment_is_ok) {
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
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
                binop->left->token->text, binop->left->token->length);
        return 1;
    }

    if (binop->right->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                binop->token->pos,
                binop->right->token->text, binop->right->token->length);
        return 1;
    }

    if (!type_is_numeric(binop->left->type) && !type_is_pointer(binop->left->type)) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "expected numeric or pointer type for left side of binary operator");
        return 1;
    }

    if (!type_is_numeric(binop->right->type)) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "expected numeric type for right side of binary operator");
        return 1;
    }

    if (type_is_pointer(binop->right->type)) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "right side of binary operator is a pointer");
        return 1;
    }

    if (binop->left->type->kind == Type_Kind_Basic
        && binop->left->type->Basic.kind == Basic_Kind_Rawptr
        && !binop_is_compare(binop)) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "cannot operate on a rawptr");
        return 1;
    }

    b32 lptr = type_is_pointer(binop->left->type);
    if (lptr && (binop->operation != Binary_Op_Add && binop->operation != Binary_Op_Minus)) {
        onyx_message_add(Msg_Type_Literal,
                binop->token->pos,
                "this operator is not supported for these operands");
        return 1;
    }

    if (lptr) {
        if (!type_is_integer(binop->right->type)) {
            onyx_message_add(Msg_Type_Literal,
                    binop->right->token->pos,
                    "expected integer type");
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
        onyx_message_add(Msg_Type_Binop_Mismatch,
                binop->token->pos,
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

CHECK(unaryop, AstUnaryOp** punop) {
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

CHECK(struct_literal, AstStructLiteral* sl) {
    fill_in_type((AstTyped *) sl);

    TypeStruct* st = &sl->type->Struct;
    if (st->mem_count != bh_arr_length(sl->values)) {
        onyx_message_add(Msg_Type_Literal,
                sl->token->pos,
                "incorrect number of initial values for this type");
        return 1;
    }

    AstTyped** actual = sl->values;
    StructMember** formal = st->memarr;

    fori (i, 0, st->mem_count) {
        if (check_expression(actual)) return 1;

        if (!types_are_compatible((*formal)->type, (*actual)->type)) {
            onyx_message_add(Msg_Type_Assignment_Mismatch,
                    sl->token->pos,
                    type_get_name((*formal)->type),
                    type_get_name((*actual)->type));
            return 1;
        }

        actual++, formal++;
    }

    return 0;
}

CHECK(address_of, AstAddressOf* aof) {
    if (check_expression(&aof->expr)) return 1;

    if (aof->expr->kind != Ast_Kind_Array_Access
            && aof->expr->kind != Ast_Kind_Dereference
            && aof->expr->kind != Ast_Kind_Field_Access
            && aof->expr->kind != Ast_Kind_Memres
            && aof->expr->kind != Ast_Kind_Local) {
        onyx_message_add(Msg_Type_Literal,
                aof->token->pos,
                "cannot take the address of this");
        return 1;
    }

    aof->expr->flags |= Ast_Flag_Address_Taken;

    aof->type = type_make_pointer(semstate.allocator, aof->expr->type);

    return 0;
}

CHECK(dereference, AstDereference* deref) {
    if (check_expression(&deref->expr)) return 1;

    if (!type_is_pointer(deref->expr->type)) {
        onyx_message_add(Msg_Type_Literal,
                deref->token->pos,
                "cannot dereference non-pointer");
        return 1;
    }

    if (deref->expr->type == basic_type_rawptr.type) {
        onyx_message_add(Msg_Type_Literal,
                deref->token->pos,
                "cannot dereference rawptr");
        return 1;
    }

    deref->type = deref->expr->type->Pointer.elem;

    return 0;
}

CHECK(array_access, AstArrayAccess* aa) {
    if (check_expression(&aa->addr)) return 1;
    if (check_expression(&aa->expr)) return 1;

    if (!type_is_pointer(aa->addr->type)) {
        onyx_message_add(Msg_Type_Literal,
                aa->token->pos,
                "expected pointer or array type for left of array access");
        return 1;
    }

    if (aa->expr->type->kind != Type_Kind_Basic
            || (aa->expr->type->Basic.flags & Basic_Flag_Integer) == 0) {
        onyx_message_add(Msg_Type_Literal,
                aa->token->pos,
                "expected integer type for index");
        return 1;
    }

    if (aa->addr->type->kind == Type_Kind_Pointer)
        aa->type = aa->addr->type->Pointer.elem;
    else if (aa->addr->type->kind == Type_Kind_Array)
        aa->type = aa->addr->type->Array.elem;
    else {
        onyx_message_add(Msg_Type_Literal,
                aa->token->pos,
                "invalid type for left of array access");
        return 1;
    }

    aa->elem_size = type_size_of(aa->type);

    return 0;
}

CHECK(field_access, AstFieldAccess** pfield) {
    AstFieldAccess* field = *pfield;
    if (check_expression(&field->expr)) return 1;

    if (!type_is_struct(field->expr->type)) {
        onyx_message_add(Msg_Type_Literal,
            field->token->pos,
            "expected expression of kind struct or pointer to struct");
        return 1;
    }

    token_toggle_end(field->token);
    StructMember smem;
    if (!type_struct_lookup_member(field->expr->type, field->token->text, &smem)) {
        onyx_message_add(Msg_Type_No_Field,
            field->token->pos,
            field->token->text,
            type_get_name(field->expr->type));
        token_toggle_end(field->token);
        return 1;
    }

    field->offset = smem.offset;
    field->type = smem.type;

    token_toggle_end(field->token);
    return 0;
}

CHECK(size_of, AstSizeOf* so) {
    so->size = type_size_of(type_build_from_ast(semstate.allocator, so->so_type));

    return 0;
}

CHECK(align_of, AstAlignOf* ao) {
    ao->alignment = type_alignment_of(type_build_from_ast(semstate.allocator, ao->ao_type));

    return 0;
}

CHECK(expression, AstTyped** pexpr) {
    AstTyped* expr = *pexpr;
    if (expr->kind > Ast_Kind_Type_Start && expr->kind < Ast_Kind_Type_End) {
        onyx_message_add(Msg_Type_Literal,
                expr->token->pos,
                "type used as part of an expression");
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
            onyx_message_add(Msg_Type_Unresolved_Symbol,
                    expr->token->pos,
                    expr->token->text, expr->token->length);
            retval = 1;
            break;

        case Ast_Kind_Param:
            if (expr->type == NULL) {
                onyx_message_add(Msg_Type_Literal,
                        expr->token->pos,
                        "local variable with unknown type");
                retval = 1;
            }
            break;

        case Ast_Kind_Local: break;

        case Ast_Kind_Address_Of:   retval = check_address_of((AstAddressOf *) expr); break;
        case Ast_Kind_Dereference:  retval = check_dereference((AstDereference *) expr); break;
        case Ast_Kind_Array_Access: retval = check_array_access((AstArrayAccess *) expr); break;
        case Ast_Kind_Field_Access: retval = check_field_access((AstFieldAccess **) pexpr); break;
        case Ast_Kind_Size_Of:      retval = check_size_of((AstSizeOf *) expr); break;
        case Ast_Kind_Align_Of:     retval = check_align_of((AstAlignOf *) expr); break;

        case Ast_Kind_Global:
            if (expr->type == NULL) {
                onyx_message_add(Msg_Type_Literal,
                        expr->token->pos,
                        "global with unknown type");
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
            expr->flags |= Ast_Flag_Function_Used;
            break;

        case Ast_Kind_StrLit: break;
        case Ast_Kind_File_Contents: break;
        case Ast_Kind_Overloaded_Function: break;
        case Ast_Kind_Enum_Value: break;

        case Ast_Kind_Memres: break;

        default:
            retval = 1;
            DEBUG_HERE;
            break;
    }

    return retval;
}

CHECK(global, AstGlobal* global) {
    fill_in_type((AstTyped *) global);

    if (global->type == NULL) {
        onyx_message_add(Msg_Type_Unresolved_Type,
                global->token->pos,
                global->exported_name->text,
                global->exported_name->length);

        return 1;
    }

    return 0;
}

CHECK(statement, AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Break:      return 0;
        case Ast_Kind_Continue:   return 0;

        case Ast_Kind_Return:     return check_return((AstReturn *) stmt);
        case Ast_Kind_If:         return check_if((AstIf *) stmt);
        case Ast_Kind_While:      return check_while((AstWhile *) stmt);
        case Ast_Kind_For:        return check_for((AstFor *) stmt);
        case Ast_Kind_Block:      return check_block((AstBlock *) stmt);
        case Ast_Kind_Defer: {
            if (!semstate.defer_allowed) {
                onyx_message_add(Msg_Type_Literal,
                    stmt->token->pos,
                    "deferred statement not allowed in deferred block");
                return 1;
            }

            semstate.defer_allowed = 0;
            b32 state = check_statement(((AstDefer *) stmt)->stmt);
            semstate.defer_allowed = 1;

            return state;
        }

        case Ast_Kind_Binary_Op:
            stmt->flags |= Ast_Flag_Expr_Ignored;
            return check_binaryop((AstBinaryOp **) &stmt, 1);

        default:
            stmt->flags |= Ast_Flag_Expr_Ignored;
            return check_expression((AstTyped **) &stmt);
    }
}

CHECK(statement_chain, AstNode* start) {
    while (start) {
        if (check_statement(start)) return 1;
        start = start->next;
    }

    return 0;
}

CHECK(block, AstBlock* block) {
    if (check_statement_chain(block->body)) return 1;

    bh_table_each_start(AstTyped *, block->scope->symbols);
        fill_in_type(value);

        if (value->type == NULL) {
            onyx_message_add(Msg_Type_Unresolved_Type,
                    value->token->pos,
                    value->token->text, value->token->length);
            return 1;
        }
    bh_table_each_end;

    return 0;
}

CHECK(function, AstFunction* func) {
    semstate.expected_return_type = func->type->Function.return_type;
    if (func->body) {
        return check_block(func->body);
    }

    return 0;
}

CHECK(overloaded_function, AstOverloadedFunction* func) {
    bh_arr_each(AstTyped *, node, func->overloads) {
        if ((*node)->kind == Ast_Kind_Overloaded_Function) {
            onyx_message_add(Msg_Type_Literal,
                    (*node)->token->pos,
                    "overload option can not be another overloaded function (yet)");

            return 1;
        }

        if ((*node)->kind != Ast_Kind_Function) {
            onyx_message_add(Msg_Type_Literal,
                    (*node)->token->pos,
                    "overload option not function");

            return 1;
        }
    }

    return 0;
}

CHECK(struct, AstStructType* s_node) {
    bh_table(i32) mem_set;
    bh_table_init(global_heap_allocator, mem_set, bh_arr_length(s_node->members));

    if (bh_arr_length(s_node->members) == 0) {
        onyx_message_add(Msg_Type_Literal,
                s_node->token->pos,
                "empty structure");
        return 1;
    }

    bh_arr_each(AstStructMember *, member, s_node->members) {
        token_toggle_end((*member)->token);

        if (bh_table_has(i32, mem_set, (*member)->token->text)) {
            onyx_message_add(Msg_Type_Duplicate_Member,
                    (*member)->token->pos,
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

CHECK(function_header, AstFunction* func) {
    AstLocal *param = func->params;
    while (param != NULL) {
        fill_in_type((AstTyped *) param);

        if (param->type == NULL) {
            onyx_message_add(Msg_Type_Literal,
                    param->token->pos,
                    "function parameter types must be known");
            return 1;
        }

        if (param->type->kind != Type_Kind_Array
            && type_size_of(param->type) == 0) {
            onyx_message_add(Msg_Type_Literal,
                    param->token->pos,
                    "function parameters must have non-void types");
            return 1;
        }

        param = (AstLocal *) param->next;
    }

    fill_in_type((AstTyped *) func);

    if ((func->flags & Ast_Flag_Exported) != 0) {
        if ((func->flags & Ast_Flag_Foreign) != 0) {
            onyx_message_add(Msg_Type_Literal,
                    func->token->pos,
                    "exporting a foreign function");
            return 1;
        }

        if ((func->flags & Ast_Flag_Intrinsic) != 0) {
            onyx_message_add(Msg_Type_Literal,
                    func->token->pos,
                    "exporting a intrinsic function");
            return 1;
        }

        if ((func->flags & Ast_Flag_Inline) != 0) {
            onyx_message_add(Msg_Type_Literal,
                    func->token->pos,
                    "exporting a inlined function");
            return 1;
        }

        if (func->exported_name == NULL) {
            onyx_message_add(Msg_Type_Literal,
                    func->token->pos,
                    "exporting function without a name");
            return 1;
        }
    }

    return 0;
}

CHECK(memres, AstMemRes* memres) {
    fill_in_type((AstTyped *) memres);

    if (memres->initial_value != NULL) {
        fill_in_type(memres->initial_value);
        check_expression(&memres->initial_value);

        if ((memres->initial_value->flags & Ast_Flag_Comptime) == 0) {
            onyx_message_add(Msg_Type_Literal,
                    memres->initial_value->token->pos,
                    "top level expressions must be compile time known");
            return 1;
        }

        Type* memres_type = memres->type;

        if (!types_are_compatible(memres_type, memres->initial_value->type)) {
            onyx_message_add(Msg_Type_Binop_Mismatch,
                    memres->token->pos,
                    type_get_name(memres_type),
                    type_get_name(memres->initial_value->type));
            return 1;
        }
    }
    
    return 0;
}

CHECK(node, AstNode* node) {
    switch (node->kind) {
        case Ast_Kind_Function:             return check_function((AstFunction *) node);
        case Ast_Kind_Overloaded_Function:  return check_overloaded_function((AstOverloadedFunction *) node);
        case Ast_Kind_Block:                return check_block((AstBlock *) node);
        case Ast_Kind_Return:               return check_return((AstReturn *) node);
        case Ast_Kind_If:                   return check_if((AstIf *) node);
        case Ast_Kind_While:                return check_while((AstWhile *) node);
        case Ast_Kind_Call:                 return check_call((AstCall *) node);
        case Ast_Kind_Binary_Op:            return check_binaryop((AstBinaryOp **) &node, 1);
        default:                            return check_expression((AstTyped **) &node);
    }
}

void onyx_type_check() {
    bh_arr_each(Entity, entity, semstate.program->entities) {
        switch (entity->type) {
            case Entity_Type_Function_Header:
                if (entity->function->flags & Ast_Flag_Foreign)
                    semstate.program->foreign_func_count++;

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

            default: DEBUG_HERE; break;
        }
    }
}
