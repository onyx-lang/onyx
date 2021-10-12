#include "astnodes.h"
#include "parser.h"
#include "utils.h"

static const char* ast_node_names[] = {
    "ERROR",
    "PACKAGE",
    "INCLUDE FILE",
    "INCLUDE FOLDER",
    "MEMORY RESERVATION",

    "BINDING",
    "ALIAS",
    "FUNCTION",
    "OVERLOADED_FUNCTION",
    "POLYMORPHIC PROC",
    "BLOCK",
    "LOCAL",
    "GLOBAL",
    "SYMBOL",

    "UN_OP",
    "BIN_OP",

    "COMPOUND",
    "NAMED_VALUE",

    "TYPE_START (BAD)",
    "TYPE",
    "BASIC_TYPE",
    "POINTER_TYPE",
    "FUNCTION_TYPE",
    "ARRAY TYPE",
    "SLICE TYPE",
    "DYNARR TYPE",
    "VARARG TYPE",
    "STRUCT TYPE",
    "POLYMORPHIC STRUCT TYPE",
    "POLYMORPHIC STRUCT CALL TYPE",
    "ENUM TYPE",
    "TYPE_ALIAS",
    "TYPE RAW ALIAS",
    "COMPOUND TYPE",
    "TYPE OF",
    "TYPE_END (BAD)",

    "STRUCT MEMBER",
    "ENUM VALUE",

    "NUMERIC LITERAL",
    "STRING LITERAL",
    "PARAM",
    "ARGUMENT",
    "CALL",
    "INTRINSIC CALL",
    "RETURN",
    "ADDRESS OF",
    "DEREFERENCE",
    "ARRAY ACCESS",
    "SLICE",
    "FIELD ACCESS",
    "UNARY FIELD ACCESS",
    "PIPE",
    "METHOD_CALL",
    "RANGE",
    "SIZE OF",
    "ALIGN OF",
    "FILE CONTENTS",
    "STRUCT LITERAL",
    "ARRAY LITERAL",
    "IF EXPRESSION",

    "IF",
    "FOR",
    "WHILE",
    "JUMP",
    "USE",
    "DEFER",
    "SWITCH",
    "SWITCH CASE",

    "SOLIDIFY",
    "STATIC IF",
    "STATIC ERROR",
    "ADD OVERLOAD",
    "OPERATOR OVERLOAD",
    "EXPORT",
    "DEFINED",
    "TAG",
    "CALL SITE",

    "CODE BLOCK",
    "DIRECTIVE INSERT",
    "MACRO",
    "DO BLOCK",

    "NOTE",

    "AST_NODE_KIND_COUNT",
};

const char* onyx_ast_node_kind_string(AstKind kind) {
    return ast_node_names[kind];
}

const char *binaryop_string[Binary_Op_Count] = {
    "+", "-", "*", "/", "%",
    "==", "!=", "<", "<=", ">", ">=",
    "&", "|", "^", "<<", ">>", ">>>",
    "&&", "||",

    "NONE",
    "=", "+=", "-=", "*=", "/=", "%=",
    "&=", "|=", "^=", "<<=", ">>=", ">>>=",
    "NONE",

    "|>", "..", "->",

    "[]",
};

const char* entity_state_strings[Entity_State_Count] = {
    "Error",
    "Parse Builtin",
    "Introduce Symbols",
    "Parse",
    "Resolve Symbols",
    "Check Types",
    "Code Gen",
    "Finalized",
};

const char* entity_type_strings[Entity_Type_Count] = {
    "Unknown",
    "Error",
    "Note",
    "Add to Load Path",
    "Load File",
    "Binding (Declaration)",
    "Use Package",
    "Static If",
    "String Literal",
    "File Contents",
    "Enum",
    "Type Alias",
    "Memory Reservation Type",
    "Use",
    "Polymorphic Proc",
    "Macro",
    "Foreign_Function Header",
    "Foreign_Global Header",
    "Function Header",
    "Global Header",
    "Process Directive",
    "Struct Member Default",
    "Memory Reservation",
    "Expression",
    "Global",
    "Overloaded_Function",
    "Function",
};

#define REDUCE_BINOP_ALL(op) \
    if (type_is_small_integer(res->type) || type_is_bool(res->type)) { \
        res->value.i = left->value.i op right->value.i; \
    } else if (type_is_integer(res->type) \
        || res->type->Basic.kind == Basic_Kind_Int_Unsized \
        || res->type->kind == Type_Kind_Enum) { \
        res->value.l = left->value.l op right->value.l; \
    } else if (res->type->Basic.kind == Basic_Kind_F32) { \
        res->value.f = left->value.f op right->value.f; \
    } else if (res->type->Basic.kind == Basic_Kind_F64 || res->type->Basic.kind == Basic_Kind_Float_Unsized) { \
        res->value.d = left->value.d op right->value.d; \
    } \
    break;

#define REDUCE_BINOP_INT(op) \
    if (type_is_small_integer(res->type) || type_is_bool(res->type)) { \
        res->value.i = left->value.i op right->value.i; \
    } else if (type_is_integer(res->type) \
        || res->type->Basic.kind == Basic_Kind_Int_Unsized \
        || res->type->kind == Type_Kind_Enum) { \
        res->value.l = left->value.l op right->value.l; \
    } \
    break;

#define REDUCE_BINOP_BOOL(op) \
    if (type_is_bool(res->type)) { \
        res->value.i = left->value.i op right->value.i; \
    } \
    break;

AstNumLit* ast_reduce_binop(bh_allocator a, AstBinaryOp* node) {
    AstNumLit* left =  (AstNumLit *) ast_reduce(a, node->left);
    AstNumLit* right = (AstNumLit *) ast_reduce(a, node->right);

    if (left->kind != Ast_Kind_NumLit || right->kind != Ast_Kind_NumLit) {
        node->left  = (AstTyped *) left;
        node->right = (AstTyped *) right;
        return (AstNumLit *) node;
    }

    AstNumLit* res = onyx_ast_node_new(a, sizeof(AstNumLit), Ast_Kind_NumLit);
    res->token = node->token;
    res->flags |= node->flags;
    res->flags |= Ast_Flag_Comptime;
    res->type_node = node->type_node;
    res->type = node->type;
    res->next = node->next;

    switch (node->operation) {
    case Binary_Op_Add:           REDUCE_BINOP_ALL(+);
    case Binary_Op_Minus:         REDUCE_BINOP_ALL(-);
    case Binary_Op_Multiply:      REDUCE_BINOP_ALL(*);
    case Binary_Op_Divide:        REDUCE_BINOP_ALL(/);
    case Binary_Op_Modulus:       REDUCE_BINOP_INT(%);

    case Binary_Op_Equal:         REDUCE_BINOP_ALL(==);
    case Binary_Op_Not_Equal:     REDUCE_BINOP_ALL(!=);
    case Binary_Op_Less:          REDUCE_BINOP_ALL(<);
    case Binary_Op_Less_Equal:    REDUCE_BINOP_ALL(<=);
    case Binary_Op_Greater:       REDUCE_BINOP_ALL(>);
    case Binary_Op_Greater_Equal: REDUCE_BINOP_ALL(>=);

    case Binary_Op_And:           REDUCE_BINOP_INT(&);
    case Binary_Op_Or:            REDUCE_BINOP_INT(|);
    case Binary_Op_Xor:           REDUCE_BINOP_INT(^);
    case Binary_Op_Shl:           REDUCE_BINOP_INT(<<);
    case Binary_Op_Shr:           REDUCE_BINOP_INT(>>);
    case Binary_Op_Sar:           REDUCE_BINOP_INT(>>);

    case Binary_Op_Bool_And:      REDUCE_BINOP_BOOL(&&);
    case Binary_Op_Bool_Or:       REDUCE_BINOP_BOOL(||);

    default: break;
    }

    return res;
}

#define REDUCE_UNOP(op) \
    if (type_is_small_integer(unop->type) || type_is_bool(unop->type)) { \
        res->value.i = op (operand)->value.i; \
    } else if (type_is_integer(unop->type) || unop->type->Basic.kind == Basic_Kind_Int_Unsized) { \
        res->value.l = op (operand)->value.l; \
    } else if (unop->type->Basic.kind == Basic_Kind_F32) { \
        res->value.f = op (operand)->value.f; \
    } else if (unop->type->Basic.kind == Basic_Kind_F64 || unop->type->Basic.kind == Basic_Kind_Float_Unsized) { \
        res->value.d = op (operand)->value.d; \
    } \
    break;

#define REDUCE_UNOP_INT(op) \
    if (type_is_small_integer(unop->type) || type_is_bool(unop->type)) { \
        res->value.i = op (operand)->value.i; \
    } else if (type_is_integer(unop->type) || res->type->Basic.kind == Basic_Kind_Int_Unsized) { \
        res->value.l = op (operand)->value.l; \
    }

AstTyped* ast_reduce_unaryop(bh_allocator a, AstUnaryOp* unop) {
    // GROSS
    AstNumLit* operand = (AstNumLit *) ast_reduce(a, unop->expr);
    unop->expr = (AstTyped *) operand;

    if (operand->kind != Ast_Kind_NumLit) {
        return (AstTyped *) unop;
    }

    AstNumLit* res = onyx_ast_node_new(a, sizeof(AstNumLit), Ast_Kind_NumLit);
    res->token = unop->token;
    res->flags |= Ast_Flag_Comptime;
    res->type_node = unop->type_node;
    res->type = unop->type;
    res->next = unop->next;

    switch (unop->operation) {
        case Unary_Op_Negate: REDUCE_UNOP(-);
        case Unary_Op_Not: {
            if (type_is_bool(res->type)) res->value.i = ! (operand)->value.i;
            break;
        }
        case Unary_Op_Bitwise_Not: REDUCE_UNOP_INT(~);

        case Unary_Op_Cast: {
            #if 0
            Type* from = operand->type;
            Type* to   = unop->type;

            if (from->kind == Type_Kind_Enum) from = from->Enum.backing;
            if (to->kind == Type_Kind_Enum) to = to->Enum.backing;

            if (from->kind != Type_Kind_Pointer && from->kind != Type_Kind_Basic) return (AstTyped *) unop;
            if (to->kind != Type_Kind_Pointer && to->kind != Type_Kind_Basic) return (AstTyped *) unop;

            b32 from_is_int=0, to_is_int=0;
            i32 from_size,     to_size;

            from_size   = type_size_of(from);
            to_size     = type_size_of(to);

            if (from->kind == Type_Kind_Basic) from_is_int = (from->flags & (Basic_Flag_Integer | Basic_Flag_Pointer)) != 0;
            if (to->kind   == Type_Kind_Basic) to_is_int   = (to->flags   & (Basic_Flag_Integer | Basic_Flag_Pointer)) != 0;

            if (from->kind == Type_Kind_Pointer) from_is_int = 1;
            if (to->kind   == Type_Kind_Pointer) to_is_int = 1;

            if (from_is_int == to_is_int && from_size == to_size) {
                // If they are already equal, nothing to do
                return (AstTyped *) res;
            }

            if (from_is_int && !to_is_int) {
                if (from_size == to_size) {
                    if (from_size == 4) {
                        res->value.f = (f32) operand->value.i;
                    }
                    else if (from_size == 8) {
                        res->value.d = (f64) operand->value.l;
                    }
                } else {
                    if (from_size == 4) {
                        res->value.d = (f64) operand->value.i;
                    }
                    else if (from_size == 8) {
                        res->value.f = (f32) operand->value.l;
                    }
                }
            } else {
                if (from_size == to_size) {
                    if (from_size == 4) {
                        res->value.i = (i32) operand->value.f;
                    }
                    else if (from_size == 8) {
                        res->value.l = (i64) operand->value.d;
                    }
                } else {
                    if (from_size == 4) {
                        res->value.l = (i64) operand->value.f;
                    }
                    else if (from_size == 8) {
                        res->value.i = (i32) operand->value.d;
                    }
                }
            }
            break;

            #endif
        }

        default: return (AstTyped *) unop;
    }

    return (AstTyped *) res;
}

AstTyped* ast_reduce(bh_allocator a, AstTyped* node) {
    assert(node->flags & Ast_Flag_Comptime);

    switch (node->kind) {
        case Ast_Kind_Binary_Op:  return (AstTyped *) ast_reduce_binop(a, (AstBinaryOp *) node);
        case Ast_Kind_Unary_Op:   return (AstTyped *) ast_reduce_unaryop(a, (AstUnaryOp *) node);
        case Ast_Kind_Enum_Value: return (AstTyped *) ((AstEnumValue *) node)->value;
        case Ast_Kind_Alias:      return (AstTyped *) ast_reduce(a, ((AstAlias *) node)->alias);
        default:                  return node;
    }
}

void promote_numlit_to_larger(AstNumLit* num) {
    assert(num->type != NULL);

    if (type_is_integer(num->type) && num->type->Basic.size <= 4) {
        // NOTE: Int32, Int16, Int8
        i64 val = (i64) num->value.i;
        num->value.l = val;
        num->type = &basic_types[Basic_Kind_I64];
    } else if (num->type->Basic.size <= 4) {
        // NOTE: Float32
        f64 val = (f64) num->value.f;
        num->value.d = val;
        num->type = &basic_types[Basic_Kind_F64];
    }
}

// NOTE: Returns 1 if the conversion was successful.
b32 convert_numlit_to_type(AstNumLit* num, Type* type) {
    if (num->type == NULL)
        num->type = type_build_from_ast(context.ast_alloc, num->type_node);
    assert(num->type);

    if (types_are_compatible(num->type, type)) return 1;
    if (!type_is_numeric(type)) return 0;

    if (num->type->Basic.kind == Basic_Kind_Int_Unsized) {

        //
        //  Integer literal auto cast rules:
        //      - Up in size always works
        //      - Down in size only works if value is in range of smaller type.
        //      - Cast to float only works if value is less than the maximum precise value for float size.
        //

        if (type->Basic.flags & Basic_Flag_Integer) {
            if (type->Basic.flags & Basic_Flag_Unsigned) {
                u64 value = (u64) num->value.l;
                if (type->Basic.size == 8) {
                    num->type = type;
                    return 1;
                }
                switch (type->Basic.size) {
                    case 1: if (value <= 255) {
                                num->type = type;
                                return 1;
                            }
                    case 2: if (value <= 65535) {
                                num->type = type;
                                return 1;
                            }
                    case 4: if (value <= 4294967295) {
                                num->type = type;
                                return 1;
                            }
                }
                
                onyx_report_error(num->token->pos, "Unsigned integer constant with value '%l' does not fit into %d-bits.",
                        num->value.l,
                        type->Basic.size * 8);

            } else {
                i64 value = (i64) num->value.l;
                switch (type->Basic.size) {
                    case 1: if (-128ll <= value && value <= 127ll) {
                                num->value.i = (i32) value;
                                num->type = type;
                                return 1;
                            } break;
                    case 2: if (-32768ll <= value && value <= 32767ll) {
                                num->value.i = (i32) value;
                                num->type = type;
                                return 1;
                            } break;
                    case 4: if (-2147483648ll <= value && value <= 2147483647ll) {
                                num->value.i = (i32) value;
                                num->type = type;
                                return 1;
                            } break;
                    case 8: {   num->type = type;
                                return 1;
                            } break;
                }

                onyx_report_error(num->token->pos, "Integer constant with value '%l' does not fit into %d-bits.",
                        num->value.l,
                        type->Basic.size * 8);
            }
        }

        else if (type->Basic.flags & Basic_Flag_Float) {
            if (type->Basic.size == 4) {
                // TODO(Brendan): Check these boundary conditions
                if (bh_abs(num->value.l) >= (1 << 23)) {
                    onyx_report_error(num->token->pos, "Integer '%l' does not fit in 32-bit float exactly.", num->value.l);
                    return 0;
                }

                num->type = type;
                num->value.f = (f32) num->value.l;
                return 1;
            }
            if (type->Basic.size == 8) {
                // TODO(Brendan): Check these boundary conditions
                if (bh_abs(num->value.l) >= (1ull << 52)) {
                    onyx_report_error(num->token->pos, "Integer '%l' does not fit in 64-bit float exactly.", num->value.l);
                    return 0;
                }

                num->type = type;
                num->value.d = (f64) num->value.l;
                return 1;
            }
        }
    }
    else if (num->type->Basic.kind == Basic_Kind_Float_Unsized) {
        // NOTE: Floats don't cast to integers implicitly.
        if ((type->Basic.flags & Basic_Flag_Float) == 0) return 0;

        if (type->Basic.kind == Basic_Kind_F32) {
            num->value.f = (f32) num->value.d;
        }

        num->type = type;
        return 1;
    }
    else if (num->type->Basic.kind == Basic_Kind_F32) {
        // NOTE: Floats don't cast to integers implicitly.
        if ((type->Basic.flags & Basic_Flag_Float) == 0) return 0;

        if (type->Basic.kind == Basic_Kind_F64) {
            num->value.d = (f64) num->value.f;
            num->type = type;
            return 1;
        }
    }

    return 0;
}

// NOTE: Returns 0 if it was not possible to make the types compatible.
// TODO: This function should be able return a "yield" condition. There
// are a couple cases that need to yield in order to be correct, like
// polymorphic functions with a typeof for the return type.
b32 unify_node_and_type_(AstTyped** pnode, Type* type, b32 permanent) {
    AstTyped* node = *pnode;
    if (type == NULL) return 0;
    if (node == NULL) return 0;

    if (node->kind == Ast_Kind_Struct_Literal && node->type_node == NULL) {
        if (node->entity != NULL) return 1;
        if (type->kind == Type_Kind_VarArgs) type = type->VarArgs.elem;
        if (!type_is_sl_constructable(type)) return 0;

        // If this shouldn't make permanent changes and submit entities,
        // just assume that it works and don't submit the entities.
        if (!permanent) return 1;

        node->type = type;

        add_entities_for_node(NULL, (AstNode *) node, NULL, NULL);
        return 1;
    }

    if (node->kind == Ast_Kind_Array_Literal && node->type_node == NULL) {
        if (node->entity != NULL) return 1;

        // If this shouldn't make permanent changes and submit entities,
        // just assume that it works and don't submit the entities.
        if (!permanent) return 1;

        Type* array_type=NULL;
        switch (type->kind) {
            case Type_Kind_Array: array_type = type; break;
            case Type_Kind_Slice: {
                Type* elem_type = type->Slice.elem;
                AstArrayLiteral* al = (AstArrayLiteral *) node;
                array_type = type_make_array(context.ast_alloc, elem_type, bh_arr_length(al->values));

                *pnode = (AstTyped *) make_cast(context.ast_alloc, node, type);

                break;
            }

            default: assert(0);
        }

        node->type = array_type;
        node->flags |= Ast_Flag_Array_Literal_Typed;

        add_entities_for_node(NULL, (AstNode *) node, NULL, NULL);
        return 1;
    }

    if (node->kind == Ast_Kind_Unary_Field_Access) {
        AstType* ast_type = type->ast_type;
        AstNode* resolved = try_symbol_resolve_from_node((AstNode *) ast_type, node->token);
        if (resolved == NULL) return 0;

        if (permanent) *pnode = (AstTyped *) resolved;
        return 1;
    }

    if (node->kind == Ast_Kind_Overloaded_Function) {
        AstTyped* func = find_matching_overload_by_type(((AstOverloadedFunction *) node)->overloads, type);
        if (func == NULL) return 0;

        // HACK: It feels like there should be a better place to flag that a procedure was definitely used.
        if (func->kind == Ast_Kind_Function)
            func->flags |= Ast_Flag_Function_Used;

        *pnode = func;
        node = *pnode;
    }

    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        AstFunction* func = polymorphic_proc_lookup((AstPolyProc *) node, PPLM_By_Function_Type, type, node->token);
        if (func == NULL) return 0;

        // FIXME: This is incorrect. It should actually yield and not return a failure.
        if (func == (AstFunction *) &node_that_signals_a_yield) return 0;

        *pnode = (AstTyped *) func;
        node = *pnode;
    }

    // HACK: NullProcHack
    // The null_proc matches any procedure, and because of that, will cause a runtime error if you
    // try to call it.
    if (type->kind == Type_Kind_Function && (node->flags & Ast_Flag_Proc_Is_Null) != 0) return 1;

    // The normal case where everything works perfectly.
    Type* node_type = get_expression_type(node);
    if (types_are_compatible(node_type, type)) return 1;

    i64 any_id = type_build_from_ast(context.ast_alloc, builtin_any_type)->id;
    if (node_type && node_type->id != any_id && type->id == any_id) return 1;

    // Here are some of the ways you can unify a node with a type if the type of the
    // node does not match the given type:
    // 
    // If the nodes type is a function type and that function has an automatic return
    // value placeholder, fill in that placeholder with the actual type.
    // :AutoReturnType
    if (node_type && node_type->kind == Type_Kind_Function
        && node_type->Function.return_type == &type_auto_return
        && type->kind == Type_Kind_Function) {

        node_type->Function.return_type = type->Function.return_type;
        return 1;
    }

    // If the node is an auto cast (~~) node, then check to see if the cast is legal
    // to the destination type, and if it is change the type to cast to.
    if (node_is_auto_cast((AstNode *) node)) {
        char* dummy;
        Type* from_type = get_expression_type(((AstUnaryOp *) node)->expr);
        if (!from_type || !cast_is_legal(from_type, type, &dummy)) {
            return 0;

        } else {
            if (permanent) ((AstUnaryOp *) node)->type = type;
            return 1;
        }
    }

    // If the destination type is a slice, then automatically convert arrays, dynamic
    // arrays, and var args, if they are the same type. This is big convenience feature
    // that makes working with arrays much easier.
    // [N] T  -> [] T
    // [..] T -> [] T
    // ..T    -> [] T
    else if (node_type && type->kind == Type_Kind_Slice) {
        if (node_type->kind == Type_Kind_Array || node_type->kind == Type_Kind_DynArray || node_type->kind == Type_Kind_VarArgs) {
            char* dummy;
            if (cast_is_legal(node_type, type, &dummy)) {
                *pnode = (AstTyped *) make_cast(context.ast_alloc, node, type);
                return 1;
            }
        }
    }

    // If the node is a numeric literal, try to convert it to the destination type.
    else if (node->kind == Ast_Kind_NumLit) {
        if (convert_numlit_to_type((AstNumLit *) node, type)) return 1;
    }

    // If the node is a compound expression, and it doesn't have a type created,
    // recursive call this function with the individual components of the compound
    // expression.
    else if (node->kind == Ast_Kind_Compound) {
        if (type->kind != Type_Kind_Compound) return 0;

        AstCompound* compound = (AstCompound *) node;

        u32 expr_count = bh_arr_length(compound->exprs);
        if (expr_count != type->Compound.count) return 0;

        fori (i, 0, (i64) expr_count) {
            if (!unify_node_and_type_(&compound->exprs[i], type->Compound.types[i], permanent)) return 0;
        }

        compound->type = type_build_compound_type(context.ast_alloc, compound);
        
        return 1;
    }

    else if (node->kind == Ast_Kind_If_Expression) {
        AstIfExpression* if_expr = (AstIfExpression *) node;

        b32 true_success  = unify_node_and_type_(&if_expr->true_expr,  type, permanent);
        b32 false_success = unify_node_and_type_(&if_expr->false_expr, type, permanent);

        if (true_success && false_success) {
            if (permanent) if_expr->type = type;
            return 1;

        } else {
            return 0;
        }
    }

    else if (node->kind == Ast_Kind_Alias) {
        AstAlias* alias = (AstAlias *) node;
        return unify_node_and_type_(&alias->alias, type, permanent);
    }

    return 0;
}

Type* resolve_expression_type(AstTyped* node) {
    if (node == NULL) return NULL;

    if (node->kind == Ast_Kind_Compound) {
        bh_arr_each(AstTyped *, expr, ((AstCompound *) node)->exprs) {
            resolve_expression_type(*expr);
        }
    }

    if (node->kind == Ast_Kind_Argument) {
        node->type = resolve_expression_type(((AstArgument *) node)->value);
    }

    if (node->kind == Ast_Kind_If_Expression) {
        AstIfExpression* if_expr = (AstIfExpression *) node;

        Type* ltype = resolve_expression_type(if_expr->true_expr);
        unify_node_and_type(&if_expr->false_expr, ltype);

        if_expr->type = ltype;
    }

    if (node->kind == Ast_Kind_Alias) {
        AstAlias* alias = (AstAlias *) node;
        alias->type = resolve_expression_type(alias->alias);
    }

    if (node_is_type((AstNode *) node)) {
        return &basic_types[Basic_Kind_Type_Index];
    }

    if (node->type == NULL)
        node->type = type_build_from_ast(context.ast_alloc, node->type_node);

    if (node->kind == Ast_Kind_NumLit && node->type->kind == Type_Kind_Basic) {
        if (node->type->Basic.kind == Basic_Kind_Int_Unsized) {
            if (bh_abs(((AstNumLit *) node)->value.l) >= (1ull << 32))
                convert_numlit_to_type((AstNumLit *) node, &basic_types[Basic_Kind_I64]);
            else
                convert_numlit_to_type((AstNumLit *) node, &basic_types[Basic_Kind_I32]);
        }
        else if (node->type->Basic.kind == Basic_Kind_Float_Unsized) {
            convert_numlit_to_type((AstNumLit *) node, &basic_types[Basic_Kind_F64]);
        }
    }

    return node->type;
}

i64 get_expression_integer_value(AstTyped* node) {
    resolve_expression_type(node);

    if (node->kind == Ast_Kind_NumLit && type_is_integer(node->type)) {
        return ((AstNumLit *) node)->value.l;
    }

    if (node->kind == Ast_Kind_NumLit && type_is_bool(node->type)) {
        return ((AstNumLit *) node)->value.i;
    }

    if (node->kind == Ast_Kind_Argument) {
        return get_expression_integer_value(((AstArgument *) node)->value);
    }

    if (node->kind == Ast_Kind_Size_Of) {
        return ((AstSizeOf *) node)->size;
    }

    if (node->kind == Ast_Kind_Align_Of) {
        return ((AstAlignOf *) node)->alignment;
    }

    if (node->kind == Ast_Kind_Alias) {
        return get_expression_integer_value(((AstAlias *) node)->alias);
    }

    if (node_is_type((AstNode*) node)) {
        Type* type = type_build_from_ast(context.ast_alloc, (AstType *) node);
        return type->id;
    }

    return 0;
}

static const b32 cast_legality[][12] = {
    /* I8  */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0 },
    /* U8  */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0 },
    /* I16 */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0 },
    /* U16 */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0 },
    /* I32 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    /* U32 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    /* I64 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 },
    /* U64 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0 },
    /* F32 */ { 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0 },
    /* F64 */ { 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0 },
    /* PTR */ { 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0 },
    /* TYP */ { 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1,}
};

b32 cast_is_legal(Type* from_, Type* to_, char** err_msg) {
    Type* from = from_;
    Type* to   = to_;

    if (from == NULL) {
        if (err_msg) *err_msg = "'from' is null. (Compiler Error)";
        return 0;
    }
    if (to == NULL) {
        if (err_msg) *err_msg = "'to' is null. (Compiler Error)";
        return 0;
    }

    if (from->kind == Type_Kind_Enum) from = from->Enum.backing;
    if (to->kind == Type_Kind_Enum) to = to->Enum.backing;

    if (from->kind == Type_Kind_Struct || to->kind == Type_Kind_Struct) {
        *err_msg = "Cannot cast to or from a struct.";
        return 0;
    }

    // CLEANUP: These error messages should be a lot better and actually
    // provide the types of the things in question.
    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_Array) {
        if (!types_are_compatible(to->Slice.elem, from->Array.elem)) {
            *err_msg = "Array to slice cast is not valid here because the types are different.";
            return 0;
        } else {
            return 1;
        }
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_DynArray) {
        if (!types_are_compatible(to->Slice.elem, from->DynArray.elem)) {
            *err_msg = "Dynmaic array to slice cast is not valid here because the types are different.";
            return 0;
        } else {
            return 1;
        }
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_VarArgs) {
        if (!types_are_compatible(to->Slice.elem, from->VarArgs.elem)) {
            *err_msg = "Variadic argument to slice cast is not valid here because the types are different.";
            return 0;
        } else {
            return 1;
        }
    }

    if (from->kind == Type_Kind_Slice || to->kind == Type_Kind_Slice) {
        *err_msg = "Cannot cast to or from a slice.";
        return 0;
    }

    if (from->kind == Type_Kind_DynArray || to->kind == Type_Kind_DynArray) {
        *err_msg = "Cannot cast to or from a dynamic array.";
        return 0;
    }

    if (to->kind == Type_Kind_Function) {
        *err_msg = "Cannot cast to a function.";
        return 0;
    }

    if (   (type_is_simd(to) && !type_is_simd(from))
        || (!type_is_simd(to) && type_is_simd(from))) {
        *err_msg = "Can only perform a SIMD cast between SIMD types.";
        return 0;
    }

    if (from->kind == Type_Kind_Basic && from->Basic.kind == Basic_Kind_Void) {
        *err_msg = "Cannot cast from void.";
        return 0;
    }
    i32 fromidx = -1, toidx = -1;
    if (from->Basic.flags & Basic_Flag_Pointer || from->kind == Type_Kind_Array) {
        fromidx = 10;
    }
    else if (from->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (from->Basic.flags & Basic_Flag_Unsigned) != 0;

        fromidx = log2_dumb(from->Basic.size) * 2 + unsign;
    }
    else if (from->Basic.flags & Basic_Flag_Float) {
        if      (from->Basic.size == 4) fromidx = 8;
        else if (from->Basic.size == 8) fromidx = 9;
    }
    else if (from->Basic.flags & Basic_Flag_Boolean) {
        fromidx = 0;
    }
    else if (from->Basic.flags & Basic_Flag_Type_Index) {
        fromidx = 11;
    }

    if (to->Basic.flags & Basic_Flag_Pointer || to->kind == Type_Kind_Array) {
        toidx = 10;
    }
    else if (to->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (to->Basic.flags & Basic_Flag_Unsigned) != 0;

        toidx = log2_dumb(to->Basic.size) * 2 + unsign;
    }
    else if (to->Basic.flags & Basic_Flag_Float) {
        if      (to->Basic.size == 4) toidx = 8;
        else if (to->Basic.size == 8) toidx = 9;
    }
    else if (to->Basic.flags & Basic_Flag_Boolean) {
        toidx = 0;
    }
    else if (to->Basic.flags & Basic_Flag_Type_Index) {
        toidx = 11;
    }

    if (fromidx != -1 && toidx != -1) {
        if (!cast_legality[fromidx][toidx]) {
            *err_msg = bh_aprintf(global_heap_allocator, "Cast from '%s' to '%s' is not allowed.", type_get_name(from_), type_get_name(to_));
            return 0;
        }
    } else {
        *err_msg = bh_aprintf(global_heap_allocator, "Cast from '%s' to '%s' is not allowed.", type_get_name(from_), type_get_name(to_));
        return 0;
    }

    *err_msg = NULL;
    return 1;
}

char* get_function_name(AstFunction* func) {
    if (func->kind != Ast_Kind_Function) return "<procedure>";

    if (func->name != NULL) return func->name;

    if (func->exported_name != NULL) {
        return bh_aprintf(global_scratch_allocator,
                "EXPORTED:%b",
                func->exported_name->text,
                func->exported_name->length);
    }

    return "<anonymous procedure>";
}

AstNode* strip_aliases(AstNode* n) {
    if (n == NULL) return n;

    while (n->kind == Ast_Kind_Alias) n = (AstNode *) ((AstAlias *) n)->alias;

    return n;
}

AstNumLit* make_bool_literal(bh_allocator a, b32 b) {
    AstNumLit* bl = onyx_ast_node_new(a, sizeof(AstNumLit), Ast_Kind_NumLit);
    bl->flags |= Ast_Flag_Comptime;
    bl->type_node = (AstType *) &basic_type_bool;

    bl->value.i = b ? 1 : 0;
    return bl;
}

AstNumLit* make_int_literal(bh_allocator a, i64 i) {
    AstNumLit* num = onyx_ast_node_new(a, sizeof(AstNumLit), Ast_Kind_NumLit);
    num->flags |= Ast_Flag_Comptime;

    if (bh_abs(i) >= ((u64) 1 << 32))
        num->type_node = (AstType *) &basic_type_i64;
    else
        num->type_node = (AstType *) &basic_type_i32;

    num->value.l = i;
    return num;
}

AstNumLit* make_float_literal(bh_allocator a, f64 d) {
    // NOTE: Use convert_numlit_to_type to make this a concrete float
    AstNumLit* num = onyx_ast_node_new(a, sizeof(AstNumLit), Ast_Kind_NumLit);
    num->flags |= Ast_Flag_Comptime;
    num->type_node = (AstType *) &basic_type_float_unsized;
    num->value.d = d;
    return num;
}

AstRangeLiteral* make_range_literal(bh_allocator a, AstTyped* low, AstTyped* high) {
    AstRangeLiteral* rl = onyx_ast_node_new(a, sizeof(AstRangeLiteral), Ast_Kind_Range_Literal);
    rl->type = builtin_range_type_type;
    rl->low = low;
    rl->high = high;
    return rl;
}

AstBinaryOp* make_binary_op(bh_allocator a, BinaryOp operation, AstTyped* left, AstTyped* right) {
    AstBinaryOp* binop_node = onyx_ast_node_new(a, sizeof(AstBinaryOp), Ast_Kind_Binary_Op);
    binop_node->left  = left;
    binop_node->right = right;
    binop_node->operation = operation;
    return binop_node;
}

AstArgument* make_argument(bh_allocator a, AstTyped* value) {
    AstArgument* arg = onyx_ast_node_new(a, sizeof(AstArgument), Ast_Kind_Argument);
    if (value->token) arg->token = value->token;
    arg->value = value;
    arg->type = value->type;
    arg->next = NULL;
    arg->va_kind = VA_Kind_Not_VA;
    return arg;
}

AstFieldAccess* make_field_access(bh_allocator a, AstTyped* node, char* field) {
    AstFieldAccess* fa = onyx_ast_node_new(a, sizeof(AstFieldAccess), Ast_Kind_Field_Access);
    if (node->token) fa->token = node->token;
    fa->field = field;
    fa->expr = node;

    return fa;
}

AstAddressOf* make_address_of(bh_allocator a, AstTyped* node) {
    AstAddressOf* ao = onyx_ast_node_new(a, sizeof(AstAddressOf), Ast_Kind_Address_Of);
    if (node->token) ao->token = node->token;
    ao->expr = node;

    return ao; 
}

AstLocal* make_local(bh_allocator a, OnyxToken* token, AstType* type_node) {
    AstLocal* local = onyx_ast_node_new(a, sizeof(AstLocal), Ast_Kind_Local);
    local->token = token;
    local->type_node = type_node;

    return local;
}

AstNode* make_symbol(bh_allocator a, OnyxToken* sym) {
    AstNode* symbol = onyx_ast_node_new(a, sizeof(AstNode), Ast_Kind_Symbol);
    symbol->token = sym;
    return symbol;
}

AstUnaryOp* make_cast(bh_allocator a, AstTyped* expr, Type* to) {
    AstUnaryOp* cast = onyx_ast_node_new(a, sizeof(AstUnaryOp), Ast_Kind_Unary_Op);
    cast->token = expr->token;
    cast->operation = Unary_Op_Cast;
    cast->expr = expr;
    cast->type = to;
    return cast;
}

void arguments_initialize(Arguments* args) {
    if (args->values == NULL)       bh_arr_new(global_heap_allocator, args->values, 2);
    if (args->named_values == NULL) bh_arr_new(global_heap_allocator, args->named_values, 2);

    // CLEANUP: I'm not sure if I need to initialize these to NULL values, but it doesn't hurt.
    fori (i, 0, 2) {
        args->values[i] = NULL;
        args->named_values[i] = NULL;
    }

    args->used_argument_count = -1;
}

void arguments_ensure_length(Arguments* args, u32 count) {
    // Make the array big enough
    bh_arr_grow(args->values, count);

    // NULL initialize the new elements
    fori (i, bh_arr_length(args->values), count) args->values[i] = NULL;

    // Set the actual length to the count, but never let it decrease in size
    bh_arr_set_length(args->values, bh_max(count, (u32) bh_arr_length(args->values)));
}

void arguments_copy(Arguments* dest, Arguments* src) {
    dest->used_argument_count = -1;
    dest->named_values = src->named_values;
    
    bh_arr_grow(dest->values, (u32) bh_arr_length(src->values));
    bh_arr_set_length(dest->values, (u32) bh_arr_length(src->values));
    bh_arr_each(AstTyped*, arg, dest->values) *arg = NULL;
    fori (i, 0, bh_arr_length(src->values)) dest->values[i] = src->values[i];
}

// In clone, the named_values are not copied. This is used in find_matching_overload_by_arguments since it doesn't need them to be copied.
void arguments_clone(Arguments* dest, Arguments* src) {
    dest->used_argument_count = -1;
    dest->named_values = src->named_values;
    dest->values = bh_arr_copy(global_heap_allocator, src->values);
}

void arguments_deep_clone(bh_allocator a, Arguments* dest, Arguments* src) {
    dest->used_argument_count = -1;
    dest->values = NULL;
    dest->named_values = NULL;

    bh_arr_new(global_heap_allocator, dest->named_values, bh_arr_length(src->named_values));
    bh_arr_new(global_heap_allocator, dest->values, bh_arr_length(src->values));

    bh_arr_each(AstNamedValue *, nv, src->named_values)
        bh_arr_push(dest->named_values, (AstNamedValue *) ast_clone(a, *nv));

    bh_arr_each(AstTyped *, val, src->values)
        bh_arr_push(dest->values, (AstTyped *) ast_clone(a, (AstNode *) *val));
}

void arguments_remove_baked(Arguments* args) {
    fori (i, 0, bh_arr_length(args->values)) {
        if (args->values[i]->kind != Ast_Kind_Argument) continue;
        if (!((AstArgument *) args->values[i])->is_baked) continue;

        bh_arr_deleten(args->values, i, 1);
        i--;
    }

    fori (i, 0, bh_arr_length(args->named_values)) {
        if (args->named_values[i]->value->kind != Ast_Kind_Argument) continue;
        if (!((AstArgument *) args->named_values[i]->value)->is_baked) continue;

        bh_arr_deleten(args->named_values, i, 1);
        i--;
    }
}

// GROSS: Using void* to avoid having to cast everything.
const char* node_get_type_name(void* node) {
    if (node_is_type((AstNode *) node)) return "type_expr";

    if (((AstNode *) node)->kind == Ast_Kind_Argument) {
        return node_get_type_name(((AstArgument *) node)->value);
    }

    if (((AstNode *) node)->kind == Ast_Kind_Polymorphic_Proc) {
        return "polymorphic procedure";
    }

    return type_get_name(((AstTyped *) node)->type);
}

b32 static_if_resolution(AstIf* static_if) {
    if (static_if->kind != Ast_Kind_Static_If) return 0;

    // assert(condition_value->kind == Ast_Kind_NumLit); // This should be right, right?
    i64 value = get_expression_integer_value(static_if->cond);

    return value != 0;
}
