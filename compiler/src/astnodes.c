#include "astnodes.h"
#include "parser.h"
#include "utils.h"

static const char* ast_node_names[] = {
    "ERROR",
    "PACKAGE",
    "INCLUDE FILE",
    "INCLUDE FOLDER",
    "INCLUDE ALL IN FOLDER",
    "INCLUDE LIBRARY PATH",
    "IMPORT",
    "MEMORY RESERVATION",

    "BINDING",
    "ALIAS",
    "INJECTION",
    "FUNCTION",
    "OVERLOADED_FUNCTION",
    "POLYMORPHIC PROC",
    "POLYMORPH QUERY",
    "INTERFACE",
    "CONSTRAINT",
    "CONSTRAINT SENTITEL",
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
    "MULTI POINTER TYPE",
    "FUNCTION_TYPE",
    "ARRAY TYPE",
    "SLICE TYPE",
    "DYNARR TYPE",
    "VARARG TYPE",
    "STRUCT TYPE",
    "POLYMORPHIC STRUCT TYPE",
    "POLYMORPHIC STRUCT CALL TYPE",
    "UNION TYPE",
    "POLYMORPHIC UNION TYPE",
    "ENUM TYPE",
    "TYPE_ALIAS",
    "TYPE RAW ALIAS",
    "COMPOUND TYPE",
    "TYPE OF",
    "DISTINCT TYPE",
    "TYPE_END (BAD)",

    "STRUCT MEMBER",
    "UNION VARIANT",
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
    "DEFER",
    "SWITCH",
    "CASE",

    "SOLIDIFY",
    "STATIC IF",
    "STATIC ERROR",
    "ADD OVERLOAD",
    "OPERATOR OVERLOAD",
    "EXPORT",
    "DEFINED",
    "INIT",
    "LIBRARY",
    "REMOVE",
    "FIRST",
    "EXPORT NAME",
    "THIS PACKAGE",
    "WASM SECTION",
    "CALL SITE",

    "CODE BLOCK",
    "DIRECTIVE INSERT",
    "MACRO",
    "DO BLOCK",

    "CAPTURE BLOCK",
    "CAPTURE LOCAL",

    "FOREIGN BLOCK",
    "ZERO VALUE",

    "JS CODE",

    "COMPILER EXTENSION",
    "PROCEDURAL MACRO",
    "PROCEDURAL EXPANSION",

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

    "|>", "..", "..=", "->",

    "[]", "[]=", "^[]",

    "??"
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
    "Failed",
};

const char* entity_type_strings[Entity_Type_Count] = {
    "Unknown",
    "Error",
    "Add to Load Path",
    "Load File",
    "Binding (Declaration)",
    "Use Package",
    "Import",
    "Static If",
    "String Literal",
    "File Contents",
    "CompilerExtension",
    "Procedural Expansion",
    "Enum",
    "Enum Value",
    "Type Alias",
    "Memory Reservation Type",
    "Use",
    "Interface",
    "Constraint Check",
    "Polymorphic Proc",
    "Polymorph Query",
    "Foreign Block",
    "Macro",
    "Foreign_Function Header",
    "Temporary Function Header",
    "Function Header",
    "Global Header",
    "Process Directive",
    "Struct Member Default",
    "Memory Reservation",
    "Expression",
    "Job",
    "Global",
    "Overloaded_Function",
    "Function",
    "JS",
};

AstNumLit* ast_reduce_type_compare(Context *context, AstBinaryOp* node) {
    AstType* left =  (AstType *) ast_reduce(context, node->left);
    AstType* right = (AstType *) ast_reduce(context, node->right);

    Type* left_type  = type_build_from_ast(context, left);
    Type* right_type = type_build_from_ast(context, right);

    AstNumLit* res = onyx_ast_node_new(context->ast_alloc, sizeof(AstNumLit), Ast_Kind_NumLit);
    res->token = node->token;
    res->flags |= node->flags;
    res->flags |= Ast_Flag_Comptime;
    res->type_node = (AstType *) &context->basic_types.type_bool;
    res->type = context->types.basic[Basic_Kind_Bool];
    res->next = node->next;

    switch (node->operation) {
        case Binary_Op_Equal:     res->value.l = left_type->id == right_type->id; break;
        case Binary_Op_Not_Equal: res->value.l = left_type->id != right_type->id; break;
        default: assert("Bad case in ast_reduce_type_compare" && 0);
    }

    return res;
}

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

AstNumLit* ast_reduce_binop(Context *context, AstBinaryOp* node) {
    AstNumLit* left =  (AstNumLit *) ast_reduce(context, node->left);
    AstNumLit* right = (AstNumLit *) ast_reduce(context, node->right);

    if (node_is_type((AstNode *) left) && node_is_type((AstNode *) right)) {
        if (node->operation == Binary_Op_Equal || node->operation == Binary_Op_Not_Equal) {
            return (AstNumLit *) ast_reduce_type_compare(context, node);
        }
    }

    if (left->kind != Ast_Kind_NumLit) {
        b32 valid = 0;
        AstNumLit *tmp = make_int_literal(context, get_expression_integer_value(context, (AstTyped *) left, &valid));
        if (valid) left = tmp;
    }

    if (right->kind != Ast_Kind_NumLit) {
        b32 valid = 0;
        AstNumLit *tmp = make_int_literal(context, get_expression_integer_value(context, (AstTyped *) right, &valid));
        if (valid) right = tmp;
    }

    if (left->kind != Ast_Kind_NumLit || right->kind != Ast_Kind_NumLit) {
        node->left  = (AstTyped *) left;
        node->right = (AstTyped *) right;
        return (AstNumLit *) node;
    }

    AstNumLit* res = onyx_ast_node_new(context->ast_alloc, sizeof(AstNumLit), Ast_Kind_NumLit);
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

AstTyped* ast_reduce_unaryop(Context *context, AstUnaryOp* unop) {
    // GROSS
    AstNumLit* operand = (AstNumLit *) ast_reduce(context, unop->expr);
    unop->expr = (AstTyped *) operand;

    if (operand->kind != Ast_Kind_NumLit) {
        return (AstTyped *) unop;
    }

    AstNumLit* res = onyx_ast_node_new(context->ast_alloc, sizeof(AstNumLit), Ast_Kind_NumLit);
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

AstTyped* ast_reduce(Context *context, AstTyped* node) {
    assert(node->flags & Ast_Flag_Comptime);

    switch (node->kind) {
        case Ast_Kind_Binary_Op:  return (AstTyped *) ast_reduce_binop(context, (AstBinaryOp *) node);
        case Ast_Kind_Unary_Op:   return (AstTyped *) ast_reduce_unaryop(context, (AstUnaryOp *) node);
        case Ast_Kind_Enum_Value: return (AstTyped *) ((AstEnumValue *) node)->value;
        case Ast_Kind_Alias:      return (AstTyped *) ast_reduce(context, ((AstAlias *) node)->alias);
        default:                  return node;
    }
}

void promote_numlit_to_larger(Context *context, AstNumLit* num) {
    assert(num->type != NULL);

    if (type_is_integer(num->type) && num->type->Basic.size <= 4) {
        // NOTE: Int32, Int16, Int8
        i64 val = (i64) num->value.i;
        num->value.l = val;
        num->type = context->types.basic[Basic_Kind_I64];
    } else if (num->type->Basic.size <= 4) {
        // NOTE: Float32
        f64 val = (f64) num->value.f;
        num->value.d = val;
        num->type = context->types.basic[Basic_Kind_F64];
    }
}

// NOTE: Returns 1 if the conversion was successful.
b32 convert_numlit_to_type(Context *context, AstNumLit* num, Type* to_type, b32 permanent) {
    if (num->type == NULL)
        num->type = type_build_from_ast(context, num->type_node);
    assert(num->type);

    if (types_are_compatible(context, num->type, to_type)) return 1;
    if (!type_is_numeric(to_type)) return 0;

    Type *type = to_type;
    if (type->kind == Type_Kind_Enum) type = type->Enum.backing;
    if (type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;

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
                    if (permanent) num->type = to_type;
                    return 1;
                }
                switch (type->Basic.size) {
                    case 1: if (value <= 255) {
                                if (permanent) num->type = to_type;
                                return 1;
                            }
                    case 2: if (value <= 65535) {
                                if (permanent) num->type = to_type;
                                return 1;
                            }
                    case 4: if (value <= 4294967295) {
                                if (permanent) num->type = to_type;
                                return 1;
                            }
                }

                if (permanent) ONYX_ERROR(num->token->pos, Error_Critical, "Unsigned integer constant with value '%l' does not fit into %d-bits.",
                        num->value.l,
                        type->Basic.size * 8);

            } else {
                i64 value = (i64) num->value.l;
                switch (type->Basic.size) {
                    case 1: if (-128ll <= value && value <= 127ll) {
                                if (permanent) num->value.i = (i32) value;
                                if (permanent) num->type = to_type;
                                return 1;
                            } break;
                    case 2: if (-32768ll <= value && value <= 32767ll) {
                                if (permanent) num->value.i = (i32) value;
                                if (permanent) num->type = to_type;
                                return 1;
                            } break;
                    case 4: if (-2147483648ll <= value && value <= 2147483647ll) {
                                if (permanent) num->value.i = (i32) value;
                                if (permanent) num->type = to_type;
                                return 1;
                            } break;
                    case 8: {   if (permanent) num->type = to_type;
                                return 1;
                            } break;
                }

                if (permanent) ONYX_ERROR(num->token->pos, Error_Critical, "Integer constant with value '%l' does not fit into %d-bits.",
                        num->value.l,
                        type->Basic.size * 8);
            }
        }

        else if (type->Basic.flags & Basic_Flag_Float) {
            if (type->Basic.size == 4) {
                if (bh_abs(num->value.l) >= (1 << 23)) {
                    if (permanent) ONYX_ERROR(num->token->pos, Error_Critical, "Integer '%l' does not fit in 32-bit float exactly.", num->value.l);
                    return 0;
                }

                if (permanent) num->type = to_type;
                if (permanent) num->value.f = (f32) num->value.l;
                return 1;
            }
            if (type->Basic.size == 8) {
                if (bh_abs(num->value.l) >= (1ll << 52)) {
                    if (permanent) ONYX_ERROR(num->token->pos, Error_Critical, "Integer '%l' does not fit in 64-bit float exactly.", num->value.l);
                    return 0;
                }

                if (permanent) num->type = to_type;
                if (permanent) num->value.d = (f64) num->value.l;
                return 1;
            }
        }
    }
    else if (num->type->Basic.kind == Basic_Kind_Float_Unsized) {
        // NOTE: Floats don't cast to integers implicitly.
        if ((type->Basic.flags & Basic_Flag_Float) == 0) return 0;

        if (type->Basic.kind == Basic_Kind_F32) {
            if (permanent) num->value.f = (f32) num->value.d;
        }

        if (permanent) num->type = to_type;
        return 1;
    }
    else if (num->type->Basic.kind == Basic_Kind_F32) {
        // NOTE: Floats don't cast to integers implicitly.
        if ((type->Basic.flags & Basic_Flag_Float) == 0) return 0;

        if (type->Basic.kind == Basic_Kind_F64) {
            if (permanent) num->value.d = (f64) num->value.f;
            if (permanent) num->type = to_type;
            return 1;
        }
    }

    return 0;
}

TypeMatch unify_node_and_type_(Context *context, AstTyped** pnode, Type* type, b32 permanent) {
    AstTyped* node = *pnode;
    if (type == NULL) return TYPE_MATCH_FAILED;
    if (node == NULL) return TYPE_MATCH_FAILED;

    if (node->kind == Ast_Kind_Struct_Literal && (node->type_node == NULL && node->type == NULL)) {
        if (node->entity != NULL) return TYPE_MATCH_SUCCESS;
        if (type->kind == Type_Kind_VarArgs) type = type->VarArgs.elem;

        //
        // If the structure literal has arguments, and the type is not constructable
        // using a struct literal, then they cannot be unified. However, if no arguments
        // are given, e.g. .{}, then any type should be matched, as that is the universal
        // zero-value.
        if (!type_is_sl_constructable(type)) {
            AstStructLiteral *sl = (AstStructLiteral *) node;
            if (bh_arr_length(sl->args.values) != 0 || bh_arr_length(sl->args.named_values) != 0) {
                return TYPE_MATCH_FAILED;
            }
        }

        // If this shouldn't make permanent changes and submit entities,
        // just assume that it works and don't submit the entities.
        if (!permanent) return TYPE_MATCH_SUCCESS;

        node->type = type;

        add_entities_for_node(&context->entities, NULL, (AstNode *) node, NULL, NULL);
        return TYPE_MATCH_SUCCESS;
    }

    if (node->kind == Ast_Kind_Array_Literal && node->type == NULL) {
        if (node->entity != NULL) return TYPE_MATCH_SUCCESS;

        // If this shouldn't make permanent changes and submit entities,
        // just assume that it works and don't submit the entities.
        if (!permanent) {
            //
            // This only works if the destination type is an array or slice,
            // otherwise there is no way the array literal would match.
            if (type->kind != Type_Kind_Array && type->kind != Type_Kind_Slice) {
                return TYPE_MATCH_FAILED;
            }

            return TYPE_MATCH_SUCCESS;
        }

        Type* array_type=NULL;
        switch (type->kind) {
            case Type_Kind_Array: array_type = type; break;
            case Type_Kind_Slice: {
                Type* elem_type = type->Slice.elem;
                AstArrayLiteral* al = (AstArrayLiteral *) node;
                array_type = type_make_array(context, elem_type, bh_arr_length(al->values));

                *pnode = (AstTyped *) make_cast(context, node, type);

                break;
            }

            // This is not the right thing to return here. In theory it should
            // try to extract the type of the first element of the array and then
            // use that as the `array_type`. Then only if there are no elements in the
            // array should it return TYPE_MATCH_FAILED;
            default: return TYPE_MATCH_FAILED;
        }

        node->type = array_type;
        node->flags |= Ast_Flag_Array_Literal_Typed;

        add_entities_for_node(&context->entities, NULL, (AstNode *) node, NULL, NULL);
        return TYPE_MATCH_SUCCESS;
    }

    if (node->kind == Ast_Kind_Unary_Field_Access) {
        if (type->kind == Type_Kind_Union) {
            token_toggle_end(node->token);
            int index = 0;
            if ((index = shgeti(type->Union.variants, node->token->text)) != -1) {
                UnionVariant *uv = type->Union.variants[index].value;
                if (uv->type != context->types.basic[Basic_Kind_Void]) {
                    if (permanent) {
                        ONYX_ERROR(node->token->pos, Error_Critical,
                            "Shorthand union literal syntax '.%s' is not all for this variant, because its type is not void; it is '%s'. Use the longer syntax, '.{ %s = value }'",
                            node->token->text,
                            type_get_name(context, uv->type),
                            node->token->text);
                    }
                    token_toggle_end(node->token);
                    return TYPE_MATCH_FAILED;
                }

                if (permanent) {
                    AstStructLiteral *sl = make_union_variant_of_void(context, type, node->token, uv);
                    *pnode = (AstTyped *) sl;
                }

                token_toggle_end(node->token);
                return TYPE_MATCH_SUCCESS;
            }
            token_toggle_end(node->token);
        }

        AstNode* resolved = try_symbol_resolve_from_type(context, type, node->token);
        if (resolved == NULL) {
            if (context->cycle_detected) {
                token_toggle_end(node->token);
                char *closest = find_closest_symbol_in_node(context, (AstNode *) type->ast_type, node->token->text);
                token_toggle_end(node->token);

                if (closest) {
                    ONYX_ERROR(node->token->pos, Error_Critical, "'%b' does not exist in '%s'. Did you mean '%s'?",
                        node->token->text, node->token->length,
                        type_get_name(context, type),
                        closest);
                } else {
                    ONYX_ERROR(node->token->pos, Error_Critical, "'%b' does not exist in '%s'.",
                        node->token->text, node->token->length, type_get_name(context, type));
                }

                return TYPE_MATCH_FAILED;
            }

            return TYPE_MATCH_YIELD;
        }

        if (permanent) {
            track_resolution_for_symbol_info(context, (AstNode *) node, resolved);
            *pnode = (AstTyped *) resolved;
        }

        return TYPE_MATCH_SUCCESS;
    }

    if (node->kind == Ast_Kind_Overloaded_Function) {
        AstTyped* func = find_matching_overload_by_type(context, ((AstOverloadedFunction *) node)->overloads, type);
        if (func == NULL) return TYPE_MATCH_FAILED;
        if (func == (AstTyped *) &context->node_that_signals_a_yield) return TYPE_MATCH_YIELD;

        if (permanent) {
            ensure_overload_returns_correct_type(context, func, (AstOverloadedFunction *) node);
            *pnode = func;
        }

        node = func;
    }

    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        AstFunction* func = polymorphic_proc_lookup(context, (AstFunction *) node, PPLM_By_Function_Type, type, node->token);
        if (func == NULL) return TYPE_MATCH_FAILED;
        if (func == (AstFunction *) &context->node_that_signals_a_yield) return TYPE_MATCH_YIELD;

        *pnode = (AstTyped *) func;
        node = *pnode;
    }

    // HACK: NullProcHack
    // The null_proc matches any procedure, and because of that, will cause a runtime error if you
    // try to call it.
    if (type->kind == Type_Kind_Function && (node->flags & Ast_Flag_Proc_Is_Null) != 0) return TYPE_MATCH_SUCCESS;

    // The normal case where everything works perfectly.
    Type* node_type = get_expression_type(context, node);
    if (types_are_compatible(context, node_type, type)) return TYPE_MATCH_SUCCESS;


    // 'any' matches any type. This is commented out because it was causing
    // many issues with incorrect implementations in the rest of the compiler
    // since this bypasses all normal type checking and forces the rest of the 
    // compiler to handle "mismatched" types. This should be properly fixed soon,
    // but will remain commented out for now.
    //
    // Type* any_type = type_build_from_ast(context, context->builtins.any_type);
    // if (any_type == NULL) return TYPE_MATCH_YIELD;
    // i64 any_id = any_type->id;
    // if (node_type && node_type->id != any_id && type->id == any_id) return TYPE_MATCH_SUCCESS;
    //

    // Here are some of the ways you can unify a node with a type if the type of the
    // node does not match the given type:
    //
    // If the nodes type is a function type and that function has an automatic return
    // value placeholder, wait for the return type to be solved by the function first.
    // :AutoReturnType
    if (node_type && node_type->kind == Type_Kind_Function
        && node_type->Function.return_type == context->types.auto_return
        && type->kind == Type_Kind_Function) {

        return TYPE_MATCH_YIELD;
    }

    // If the node is an auto cast (~~) node, then check to see if the cast is legal
    // to the destination type, and if it is change the type to cast to.
    if (node_is_auto_cast((AstNode *) node)) {
        char* dummy;
        Type* from_type = get_expression_type(context, ((AstUnaryOp *) node)->expr);
        if (!from_type || !cast_is_legal(context, from_type, type, &dummy)) {
            return TYPE_MATCH_FAILED;

        } else {
            if (permanent) ((AstUnaryOp *) node)->type = type;
            return TYPE_MATCH_SUCCESS;
        }
    }

    // String literals implicitly become c-strings for convience.
    if (node->kind == Ast_Kind_StrLit
        && type->kind == Type_Kind_MultiPointer
        && type->MultiPointer.elem == context->types.basic[Basic_Kind_U8]) {

        if (permanent) {
            AstStrLit *strlit = (AstStrLit *) node;
            strlit->is_cstr = 1;
            strlit->type = type;
        }

        return TYPE_MATCH_SUCCESS;
    }

    // If the destination type is a slice, then automatically convert arrays, dynamic
    // arrays, and var args, if they are the same type. This is big convenience feature
    // that makes working with arrays much easier.
    // [N] T  -> [] T
    // [..] T -> [] T
    // ..T    -> [] T
    if (node_type && type->kind == Type_Kind_Slice &&
        (node_type->kind == Type_Kind_Array || node_type->kind == Type_Kind_DynArray || node_type->kind == Type_Kind_VarArgs)) {

        char* dummy;
        b32 legal = cast_is_legal(context, node_type, type, &dummy);
        if (permanent && legal) {
            *pnode = (AstTyped *) make_cast(context, node, type);
        }

        return legal ? TYPE_MATCH_SUCCESS : TYPE_MATCH_FAILED;
    }

    if (node->kind == Ast_Kind_Zero_Value) {
        if (node_type == NULL) {
            node->type = type;
            return TYPE_MATCH_SUCCESS; // Shouldn't this be on the next line? And have node_type == node->type checked?
        }
    }

    if (node->kind == Ast_Kind_Switch) {
        AstSwitch *switchnode = (AstSwitch *) node;
        if (!switchnode->is_expr) return TYPE_MATCH_FAILED;

        if (switchnode->cases == NULL) return TYPE_MATCH_YIELD;

        bh_arr_each(AstSwitchCase *, pcasestmt, switchnode->cases) {
            AstSwitchCase *casestmt = *pcasestmt;
            if (!casestmt->body_is_expr) continue;

            switch (unify_node_and_type_(context, &casestmt->expr, type, permanent)) {
                case TYPE_MATCH_SUCCESS: break;
                case TYPE_MATCH_FAILED: return TYPE_MATCH_FAILED;
                case TYPE_MATCH_YIELD: return TYPE_MATCH_YIELD;
                default: break;
            }
        }

        if (switchnode->default_case) {
            switch (unify_node_and_type_(context, (AstTyped **) &switchnode->default_case, type, permanent)) {
                case TYPE_MATCH_SUCCESS: break;
                case TYPE_MATCH_FAILED: return TYPE_MATCH_FAILED;
                case TYPE_MATCH_YIELD: return TYPE_MATCH_YIELD;
                default: break;
            }
        }

        if (permanent) switchnode->type = type;
        return TYPE_MATCH_SUCCESS;
    }

    // If the destination type is an optional, and the node's type is a value of
    // the same underlying type, then we can construct an optional with a value
    // implicitly. This makes working with optionals barable.
    if (type_constructed_from_poly(type, context->builtins.optional_type)) {
        TypeMatch match = unify_node_and_type_(context, pnode, type->Union.poly_sln[0].type, permanent);
        if (match == TYPE_MATCH_SUCCESS) {
            if (permanent) {
                AstStructLiteral *opt_lit = make_optional_literal_some(context, node, type);

                *(AstStructLiteral **) pnode = opt_lit;
            }

            return TYPE_MATCH_SUCCESS;
        }

        if (match == TYPE_MATCH_YIELD) return TYPE_MATCH_YIELD;
    }


    // If the node is a numeric literal, try to convert it to the destination type.
    if (node->kind == Ast_Kind_NumLit) {
        if (convert_numlit_to_type(context, (AstNumLit *) node, type, permanent)) return TYPE_MATCH_SUCCESS;
        return TYPE_MATCH_FAILED;
    }

    // If the node is a compound expression, and it doesn't have a type created,
    // recursive call this function with the individual components of the compound
    // expression.
    if (node->kind == Ast_Kind_Compound) {
        if (type->kind != Type_Kind_Compound) return TYPE_MATCH_FAILED;

        AstCompound* compound = (AstCompound *) node;

        u32 expr_count = bh_arr_length(compound->exprs);
        if (expr_count != type->Compound.count) return TYPE_MATCH_FAILED;

        fori (i, 0, (i64) expr_count) {
            TypeMatch tm = unify_node_and_type_(context, &compound->exprs[i], type->Compound.types[i], permanent);
            if (tm != TYPE_MATCH_SUCCESS) {
                return tm;
            }
        }

        compound->type = type_build_compound_type(context, compound);

        return TYPE_MATCH_SUCCESS;
    }

    if (node->kind == Ast_Kind_If_Expression) {
        AstIfExpression* if_expr = (AstIfExpression *) node;

        TypeMatch true_success  = unify_node_and_type_(context, &if_expr->true_expr,  type, permanent);
        TypeMatch false_success = unify_node_and_type_(context, &if_expr->false_expr, type, permanent);

        if (true_success == TYPE_MATCH_SUCCESS && false_success == TYPE_MATCH_SUCCESS) {
            if (permanent) if_expr->type = type;
            return TYPE_MATCH_SUCCESS;

        } else if (true_success == TYPE_MATCH_FAILED || false_success == TYPE_MATCH_FAILED) {
            return TYPE_MATCH_FAILED;

        } else {
            return TYPE_MATCH_YIELD;
        }
    }

    if (node->kind == Ast_Kind_Alias) {
        AstAlias* alias = (AstAlias *) node;
        return unify_node_and_type_(context, &alias->alias, type, permanent);
    }

    if (node->kind == Ast_Kind_Address_Of) {
        AstAddressOf *address_of = (AstAddressOf *) node;
        if (address_of->can_be_removed) {
            if (!permanent) {
                return unify_node_and_type_(context, &address_of->expr, type, permanent);

            } else {
                *pnode = (AstTyped *) address_of->expr;
                return unify_node_and_type_(context, pnode, type, permanent);
            }
        }
    }

    //
    // This is a SUPER RARE case. It is used when checking to
    // see if the return value from a interface constraint is
    // an instance of a type generic polymorphic structure.
    // For example, converting to an iterator can be tested with:
    //
    //       IsIterable :: interface (T: type_expr) {
    //           t as T;
    //           { t->AsIterator() } -> Iterator;
    //       }
    //
    // Here, Iterator is a polymorphic type, and would normally
    // not be allowed in any other expression. However, check_constraint
    // has a special check to see if the above happens, and when
    // it does, this case allows the types to pass correctly.
    // Note, this should be the ONLY time this happens. All
    // other uses of checking polymorphic structures against
    // actual nodes strictly forbidden.
    if (type->kind == Type_Kind_PolyStruct) {
        if (node_type && node_type->kind == Type_Kind_Struct) {
            if (node_type->Struct.constructed_from->type_id == type->id) {
                return TYPE_MATCH_SUCCESS;
            }
        }
    }

    //
    // This case enables to ability to have less values on the
    // left hand side of an assignment than what the right hand
    // side call would be returning.
    if (node_type && node_type->kind == Type_Kind_Compound) {
        AstCall *call = get_call_expr_from_node((AstNode *) node);
        if (!call) return TYPE_MATCH_FAILED;

        i32 keep = 0;

        if (type->kind != Type_Kind_Compound) {
            if (!types_are_compatible(context, node_type->Compound.types[0], type)) {
                return TYPE_MATCH_FAILED;
            }

            keep += type_linear_member_count(type);

        } else {
            fori (i, 0, type->Compound.count) {
                if (!types_are_compatible(context, node_type->Compound.types[i], type->Compound.types[i])) {
                    return TYPE_MATCH_FAILED;
                }

                keep += type_linear_member_count(node_type->Compound.types[i]);
            }
        }

        call->ignored_return_value_count = type_linear_member_count(node_type) - keep;

        return TYPE_MATCH_SUCCESS;
    }

    return TYPE_MATCH_FAILED;
}

// TODO CLEANUP: Currently, query_expression_type and resolve_expression_type
// are almost the exact same function. Any logic that would be added to one
// will also HAVE TO BE ADDED TO THE OTHER. I would like to abstract the common
// code between them, but I think there enough minor differences that that
// might not be possible.

Type* query_expression_type(Context *context, AstTyped *node) {
    if (node == NULL) return NULL;

    if (node->kind == Ast_Kind_Argument) {
        return query_expression_type(context, ((AstArgument *) node)->value);
    }

    if (node->kind == Ast_Kind_If_Expression) {
        AstIfExpression* if_expr = (AstIfExpression *) node;
        return query_expression_type(context, if_expr->true_expr);
    }

    if (node->kind == Ast_Kind_Alias) {
        AstAlias* alias = (AstAlias *) node;
        return query_expression_type(context, alias->alias);
    }

    if (node_is_type((AstNode *) node)) {
        return context->types.basic[Basic_Kind_Type_Index];
    }

    if (node->kind == Ast_Kind_Array_Literal && node->type == NULL) {
        AstArrayLiteral* al = (AstArrayLiteral *) node;
        Type* elem_type = context->types.basic[Basic_Kind_Void];
        if (bh_arr_length(al->values) > 0) {
            elem_type = query_expression_type(context, al->values[0]);
        }

        if (elem_type) {
            return type_make_array(context, elem_type, bh_arr_length(al->values));
        }
    }

    if (node->kind == Ast_Kind_Struct_Literal && node->type == NULL) {
        AstStructLiteral* sl = (AstStructLiteral *) node;
        if (sl->stnode || sl->type_node) return NULL;

        // If values without names are given to a struct literal without
        // a type, then we cannot implicitly build the type of the struct
        // literal, as the name of every member cannot be known. Maybe we
        // could implicitly do something like _1, _2, ... for the members
        // that we not given names?
        if (bh_arr_length(sl->args.values) > 0) {
            return NULL;
        }

        return type_build_implicit_type_of_struct_literal(context, sl, 1);
    }

    // If polymorphic procedures HAVE to have a type, most likely
    // because they are part of a `typeof` expression, they are
    // assigned a void type. This is cleared before the procedure
    // is solidified.
    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        return context->types.basic[Basic_Kind_Void];
    }

    if (node->kind == Ast_Kind_Macro) {
        return query_expression_type(context, (AstTyped *) ((AstMacro *) node)->body);
    }

    if (node->kind == Ast_Kind_Package) {
        return type_build_from_ast(context, node->type_node);
    }

    if (node->type == NULL)
        return type_build_from_ast(context, node->type_node);

    if (node->kind == Ast_Kind_NumLit && node->type->kind == Type_Kind_Basic) {
        if (node->type->Basic.kind == Basic_Kind_Int_Unsized) {
            b32 big    = bh_abs(((AstNumLit *) node)->value.l) >= (1ll << 32);
            b32 unsign = ((AstNumLit *) node)->was_hex_literal;

            if (((AstNumLit *) node)->was_char_literal) return context->types.basic[Basic_Kind_U8];
            else if ( big && !unsign) return context->types.basic[Basic_Kind_I64];
            else if ( big &&  unsign) return context->types.basic[Basic_Kind_U64];
            else if (!big && !unsign) return context->types.basic[Basic_Kind_I32];
            else if (!big &&  unsign) return context->types.basic[Basic_Kind_U32];
        }
        else if (node->type->Basic.kind == Basic_Kind_Float_Unsized) {
            return context->types.basic[Basic_Kind_F64];
        }
    }

    return node->type;
}

// See note above about query_expresion_type.
Type* resolve_expression_type(Context *context, AstTyped* node) {
    if (node == NULL) return NULL;

    if (node->kind == Ast_Kind_Compound) {
        bh_arr_each(AstTyped *, expr, ((AstCompound *) node)->exprs) {
            resolve_expression_type(context, *expr);
        }

        node->type = type_build_compound_type(context, (AstCompound *) node);
        return node->type;
    }

    if (node->kind == Ast_Kind_Argument) {
        node->type = resolve_expression_type(context, ((AstArgument *) node)->value);
    }

    if (node->kind == Ast_Kind_If_Expression) {
        AstIfExpression* if_expr = (AstIfExpression *) node;

        Type* ltype = resolve_expression_type(context, if_expr->true_expr);
        unify_node_and_type(context, &if_expr->false_expr, ltype);

        if_expr->type = ltype;
    }

    if (node->kind == Ast_Kind_Alias) {
        AstAlias* alias = (AstAlias *) node;
        alias->type = resolve_expression_type(context, alias->alias);
    }

    if (node_is_type((AstNode *) node)) {
        return context->types.basic[Basic_Kind_Type_Index];
    }

    if (node->kind == Ast_Kind_Array_Literal && node->type == NULL) {
        AstArrayLiteral* al = (AstArrayLiteral *) node;
        Type* elem_type = context->types.basic[Basic_Kind_Void];
        if (bh_arr_length(al->values) > 0) {
            elem_type = resolve_expression_type(context, al->values[0]);
        }

        if (elem_type) {
            node->type = type_make_array(context, elem_type, bh_arr_length(al->values));
            node->flags |= Ast_Flag_Array_Literal_Typed;

            if (node->entity == NULL) {
                add_entities_for_node(&context->entities, NULL, (AstNode *) node, NULL, NULL);
            }
        }
    }

    if (node->kind == Ast_Kind_Struct_Literal && node->type == NULL) {
        AstStructLiteral* sl = (AstStructLiteral *) node;
        if (sl->stnode || sl->type_node) return NULL;

        // If values without names are given to a struct literal without
        // a type, then we cannot implicitly build the type of the struct
        // literal, as the name of every member cannot be known. Maybe we
        // could implicitly do something like _1, _2, ... for the members
        // that we not given names?
        if (bh_arr_length(sl->args.values) > 0) {
            return NULL;
        }

        sl->type = type_build_implicit_type_of_struct_literal(context, sl, 0);
        if (sl->type) {
            add_entities_for_node(&context->entities, NULL, (AstNode *) sl, NULL, NULL);
        }
    }

    // If polymorphic procedures HAVE to have a type, most likely
    // because they are part of a `typeof` expression, they are
    // assigned a void type. This is cleared before the procedure
    // is solidified.
    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        node->type = context->types.basic[Basic_Kind_Void];
    }

    if (node->kind == Ast_Kind_Macro) {
        return resolve_expression_type(context, (AstTyped *) ((AstMacro *) node)->body);
    }

    if (node->kind == Ast_Kind_Package) {
        node->type_node = context->builtins.package_id_type;
        node->type = type_build_from_ast(context, node->type_node);
    }

    if (node->type == NULL)
        node->type = type_build_from_ast(context, node->type_node);

    if (node->kind == Ast_Kind_NumLit && node->type->kind == Type_Kind_Basic) {
        if (node->type->Basic.kind == Basic_Kind_Int_Unsized) {
            b32 big    = bh_abs(((AstNumLit *) node)->value.l) >= (1ll << 32);
            b32 unsign = ((AstNumLit *) node)->was_hex_literal;

            if (((AstNumLit *) node)->was_char_literal) convert_numlit_to_type(context, (AstNumLit *) node, context->types.basic[Basic_Kind_U8], 1);
            else if ( big && !unsign) convert_numlit_to_type(context, (AstNumLit *) node, context->types.basic[Basic_Kind_I64], 1);
            else if ( big &&  unsign) convert_numlit_to_type(context, (AstNumLit *) node, context->types.basic[Basic_Kind_U64], 1);
            else if (!big && !unsign) convert_numlit_to_type(context, (AstNumLit *) node, context->types.basic[Basic_Kind_I32], 1);
            else if (!big &&  unsign) convert_numlit_to_type(context, (AstNumLit *) node, context->types.basic[Basic_Kind_U32], 1);
        }
        else if (node->type->Basic.kind == Basic_Kind_Float_Unsized) {
            convert_numlit_to_type(context, (AstNumLit *) node, context->types.basic[Basic_Kind_F64], 1);
        }
    }

    return node->type;
}

i64 get_expression_integer_value(Context *context, AstTyped* node, b32 *is_valid) {
    if (!node) return 0;

    resolve_expression_type(context, node);

    if (is_valid) *is_valid = 1;

    if (node->kind == Ast_Kind_NumLit && type_is_integer(node->type)) {
        return ((AstNumLit *) node)->value.l;
    }

    if (node->kind == Ast_Kind_NumLit && type_is_bool(node->type)) {
        return ((AstNumLit *) node)->value.i;
    }

    if (node->kind == Ast_Kind_Argument) {
        return get_expression_integer_value(context, ((AstArgument *) node)->value, is_valid);
    }

    if (node->kind == Ast_Kind_Size_Of) {
        return ((AstSizeOf *) node)->size;
    }

    if (node->kind == Ast_Kind_Align_Of) {
        return ((AstAlignOf *) node)->alignment;
    }

    if (node->kind == Ast_Kind_Alias) {
        return get_expression_integer_value(context, ((AstAlias *) node)->alias, is_valid);
    }

    if (node->kind == Ast_Kind_Enum_Value) {
        return get_expression_integer_value(context, ((AstEnumValue *) node)->value, is_valid);
    }

    if (node->kind == Ast_Kind_Unary_Op && type_is_integer(node->type)) {
        return get_expression_integer_value(context, ((AstUnaryOp *) node)->expr, is_valid);
    }

    if (node_is_type((AstNode*) node)) {
        Type* type = type_build_from_ast(context, (AstType *) node);
        if (type) return type->id;
    }

    if (is_valid) *is_valid = 0;
    return 0;
}

char *get_expression_string_value(Context *context, AstTyped* node, b32 *out_is_valid) {
    resolve_expression_type(context, node);

    if (out_is_valid) *out_is_valid = 1;

    if (node->kind == Ast_Kind_StrLit) {
        AstStrLit *str = (AstStrLit *) node;

        // CLEANUP: Maybe this should allocate on the heap?
        // I guess if in all cases the memory is allocated on the heap,
        // then the caller can free the memory.
        char* strdata = bh_alloc_array(context->gp_alloc, char, str->token->length + 1);
        i32 length  = string_process_escape_seqs(strdata, str->token->text, str->token->length);
        strdata[length] = '\0';

        return strdata;
    }

    if (node->kind == Ast_Kind_Alias) {
        return get_expression_string_value(context, ((AstAlias *) node)->alias, out_is_valid);
    }

    if (out_is_valid) *out_is_valid = 0;
    return NULL;
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

b32 cast_is_legal(Context *context, Type* from_, Type* to_, char** err_msg) {
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

    if (from_->id == to_->id) return 1;

    if (to->kind == Type_Kind_Distinct) {
        if (types_are_compatible(context, to->Distinct.base_type, from)) {
            return 1;
        }

        if (from->kind == Type_Kind_Distinct && types_are_compatible(context, to, from->Distinct.base_type)) {
            return 1;
        }

        *err_msg = "Cannot convert from a distinct type to the wrong destination type.";
        return 0;
    }

    if (from->kind == Type_Kind_Distinct) {
        if (types_are_compatible(context, from->Distinct.base_type, to)) {
            return 1;
        }

        if (to->kind == Type_Kind_Distinct && types_are_compatible(context, from, to->Distinct.base_type)) {
            return 1;
        }

        *err_msg = "Cannot convert to a distinct type from the wrong destination type.";
        return 0;
    }

    if (from->kind == Type_Kind_Union && to->kind == Type_Kind_Enum) {
        if (from->Union.tag_type->id == to->id) {
            return 1;
        }
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
        if (!types_are_compatible(context, to->Slice.elem, from->Array.elem)) {
            *err_msg = "Array to slice cast is not valid here because the types are different.";
            return 0;
        } else {
            return 1;
        }
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_DynArray) {
        //if (!types_are_compatible(context, to->Slice.elem, from->DynArray.elem)) {
        if (type_size_of(to->Slice.elem) != type_size_of(from->DynArray.elem)) {
            *err_msg = "Dynmaic array to slice cast is not valid here because the types are different sizes.";
            return 0;
        } else {
            return 1;
        }
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_VarArgs) {
        if (!types_are_compatible(context, to->Slice.elem, from->VarArgs.elem)) {
            *err_msg = "Variadic argument to slice cast is not valid here because the types are different.";
            return 0;
        } else {
            return 1;
        }
    }

    if (from->kind == Type_Kind_Slice || to->kind == Type_Kind_Slice) {
        if ((from->kind != Type_Kind_Slice || to->kind != Type_Kind_Slice)
            || to->Slice.elem->kind != Type_Kind_Pointer || from->Slice.elem->kind != Type_Kind_Pointer
            || !types_are_compatible(context, from->Slice.elem, to->Slice.elem)) {
            *err_msg = "Cannot only cast between slice types when both are a slice of compatible pointers.";
            return 0;
        } else {
            return 1;
        }
    }

    if (from->kind == Type_Kind_DynArray || to->kind == Type_Kind_DynArray) {
        *err_msg = "Cannot cast to or from a dynamic array.";
        return 0;
    }

    if (to->kind == Type_Kind_Function) {
        *err_msg = "Cannot cast to a function.";
        return 0;
    }

    if (from->kind == Type_Kind_Function) {
        *err_msg = "Can only cast a function to a 'u32'.";
        return to == context->types.basic[Basic_Kind_U32];
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
            *err_msg = bh_aprintf(context->gp_alloc, "Cast from '%s' to '%s' is not allowed.", type_get_name(context, from_), type_get_name(context, to_));
            return 0;
        }
    } else {
        *err_msg = bh_aprintf(context->gp_alloc, "Cast from '%s' to '%s' is not allowed.", type_get_name(context, from_), type_get_name(context, to_));
        return 0;
    }

    *err_msg = NULL;
    return 1;
}



TypeMatch implicit_cast_to_bool(Context *context, AstTyped **pnode) {
    AstTyped *node = *pnode;

    if (!node->type) return TYPE_MATCH_YIELD;

    if ((node->type->kind == Type_Kind_Basic && node->type->Basic.kind == Basic_Kind_Rawptr)
        || (node->type->kind == Type_Kind_Pointer)
        || (node->type->kind == Type_Kind_MultiPointer)) {
        AstNumLit *zero = make_int_literal(context, 0);
        zero->type = context->types.basic[Basic_Kind_Rawptr];

        AstBinaryOp* cmp = make_binary_op(context, Binary_Op_Not_Equal, node, (AstTyped *) zero);
        cmp->token = node->token;
        cmp->type = context->types.basic[Basic_Kind_Bool];

        *pnode = (AstTyped *) cmp;
        return TYPE_MATCH_SUCCESS;
    }

    if (node->type->kind == Type_Kind_Slice ||
        node->type->kind == Type_Kind_DynArray ||
        node->type->kind == Type_Kind_VarArgs) {
        StructMember smem;
        assert(type_lookup_member(context, node->type, "count", &smem));

        // These fields are filled out here in order to prevent
        // going through the type checker one more time.
        AstFieldAccess *field = make_field_access(context, node, "count");
        field->offset = smem.offset;
        field->idx = smem.idx;
        field->type = smem.type;
        field->flags |= Ast_Flag_Has_Been_Checked;

        AstNumLit *zero = make_int_literal(context, 0);
        zero->type = smem.type;

        AstBinaryOp* cmp = make_binary_op(context, Binary_Op_Not_Equal, (AstTyped *) field, (AstTyped *) zero);
        cmp->type = context->types.basic[Basic_Kind_Bool];

        *pnode = (AstTyped *) cmp;
        return TYPE_MATCH_SUCCESS;
    }

    if (context->caches.implicit_cast_to_bool_cache.entries == NULL) {
        bh_imap_init(&context->caches.implicit_cast_to_bool_cache, context->gp_alloc, 8);
    }

    if (!bh_imap_has(&context->caches.implicit_cast_to_bool_cache, (u64) node)) {
        AstArgument *implicit_arg = make_argument(context, node);

        Arguments *args = bh_alloc_item(context->ast_alloc, Arguments);
        bh_arr_new(context->ast_alloc, args->values, 1);
        bh_arr_push(args->values, (AstTyped *) implicit_arg);

        bh_imap_put(&context->caches.implicit_cast_to_bool_cache, (u64) node, (u64) args);
    }

    Arguments *args = (Arguments *) bh_imap_get(&context->caches.implicit_cast_to_bool_cache, (u64) node);
    AstFunction *overload = (AstFunction *) find_matching_overload_by_arguments(context, context->builtins.implicit_bool_cast->overloads, args);

    if (overload == NULL)                                       return TYPE_MATCH_FAILED;
    if (overload == (AstFunction *) &context->node_that_signals_a_yield) return TYPE_MATCH_YIELD;

    AstCall *implicit_call = onyx_ast_node_new(context->ast_alloc, sizeof(AstCall), Ast_Kind_Call);
    implicit_call->token = node->token;
    implicit_call->callee = (AstTyped *) overload;
    implicit_call->va_kind = VA_Kind_Not_VA;
    implicit_call->args.values = args->values;

    *(AstCall **) pnode = implicit_call;
    bh_imap_delete(&context->caches.implicit_cast_to_bool_cache, (u64) node);

    return TYPE_MATCH_YIELD;
}

static char *sanitize_name(bh_allocator a, char *name) {
    if (!name) return name;

    char *sanitized = bh_strdup(a, name); 
    char *c = sanitized;
    while (*c) {
        if (!char_is_alphanum(*c)) {
            *c = '_';
        }
        c++;
    }
    return sanitized;
}

char* get_function_name(Context *context, AstFunction* func) {
    if (func->kind != Ast_Kind_Function) return "unnamed_proc";

    if (func->name != NULL) return func->name;

    if (func->exported_name != NULL) {
        return bh_aprintf(context->scratch_alloc,
                "%b",
                func->exported_name->text,
                func->exported_name->length);
    }

    return "unnamed_proc";
}

char* get_function_assembly_name(Context *context, AstFunction* func) {
    if (func->kind == Ast_Kind_Function) {
        if (func->assembly_name != NULL) return func->assembly_name;

        if (func->exported_name != NULL) {
            return bh_aprintf(context->scratch_alloc,
                    "%b",
                    func->exported_name->text,
                    func->exported_name->length);
        }
    }

    if (func->token) {
        return bh_aprintf(context->ast_alloc,
            "unnamed_at_%s_%d",
            sanitize_name(context->scratch_alloc, (char *) func->token->pos.filename),
            func->token->pos.line);
    }

    return "unnamed";
}

char* generate_name_within_scope(Context *context, Scope *scope, OnyxToken* symbol) {
    char name[512];
    memset(name, 0, 512);

    bh_arr(char *) names=NULL;
    bh_arr_new(context->gp_alloc, names, 4);

    while (scope != NULL) {
        bh_arr_push(names, scope->name);
        scope = scope->parent;
    }

    bh_arr_each(char *, n, names) {
        if (*n == NULL) continue;

        strncat(name, *n, 511);
        strncat(name, ".", 511);
    }
    bh_arr_free(names);

    return bh_aprintf(context->gp_alloc, "%s%b", name, symbol->text, symbol->length);
}

AstNode* strip_aliases(AstNode* n) {
    if (n == NULL) return n;

    while (n->kind == Ast_Kind_Alias) n = (AstNode *) ((AstAlias *) n)->alias;

    return n;
}

AstNumLit* make_bool_literal(Context *context, b32 b) {
    AstNumLit* bl = onyx_ast_node_new(context->ast_alloc, sizeof(AstNumLit), Ast_Kind_NumLit);
    bl->flags |= Ast_Flag_Comptime;
    bl->type_node = (AstType *) &context->basic_types.type_bool;
    bl->type = context->types.basic[Basic_Kind_Bool];

    bl->value.i = b ? 1 : 0;
    return bl;
}

AstNumLit* make_int_literal(Context *context, i64 i) {
    AstNumLit* num = onyx_ast_node_new(context->ast_alloc, sizeof(AstNumLit), Ast_Kind_NumLit);
    num->flags |= Ast_Flag_Comptime;

    if (bh_abs(i) >= (1ll << 32))
        num->type_node = (AstType *) &context->basic_types.type_i64;
    else
        num->type_node = (AstType *) &context->basic_types.type_i32;

    num->value.l = i;
    return num;
}

AstNumLit* make_float_literal(Context *context, f64 d) {
    // NOTE: Use convert_numlit_to_type to make this a concrete float
    AstNumLit* num = onyx_ast_node_new(context->ast_alloc, sizeof(AstNumLit), Ast_Kind_NumLit);
    num->flags |= Ast_Flag_Comptime;
    num->type_node = (AstType *) &context->basic_types.type_float_unsized;
    num->value.d = d;
    return num;
}

AstRangeLiteral* make_range_literal(Context *context, AstTyped* low, AstTyped* high) {
    AstRangeLiteral* rl = onyx_ast_node_new(context->ast_alloc, sizeof(AstRangeLiteral), Ast_Kind_Range_Literal);
    rl->low = low;
    rl->high = high;
    return rl;
}

AstStrLit* make_string_literal(Context *context, OnyxToken *token) {
    AstStrLit *str = onyx_ast_node_new(context->ast_alloc, sizeof(AstStrLit), Ast_Kind_StrLit);
    str->flags |= Ast_Flag_Comptime;
    str->type_node = context->builtins.string_type;
    str->token = token;
    return str;
}

AstBinaryOp* make_binary_op(Context *context, BinaryOp operation, AstTyped* left, AstTyped* right) {
    AstBinaryOp* binop_node = onyx_ast_node_new(context->ast_alloc, sizeof(AstBinaryOp), Ast_Kind_Binary_Op);
    binop_node->left  = left;
    binop_node->right = right;
    binop_node->operation = operation;
    return binop_node;
}

AstArgument* make_argument(Context *context, AstTyped* value) {
    AstArgument* arg = onyx_ast_node_new(context->ast_alloc, sizeof(AstArgument), Ast_Kind_Argument);
    if (value->token) arg->token = value->token;
    arg->value = value;
    arg->type = value->type;
    arg->next = NULL;
    arg->va_kind = VA_Kind_Not_VA;
    return arg;
}

AstFieldAccess* make_field_access(Context *context, AstTyped* node, char* field) {
    AstFieldAccess* fa = onyx_ast_node_new(context->ast_alloc, sizeof(AstFieldAccess), Ast_Kind_Field_Access);
    if (node->token) fa->token = node->token;
    fa->field = field;
    fa->expr = node;

    return fa;
}

AstAddressOf* make_address_of(Context *context, AstTyped* node) {
    AstAddressOf* ao = onyx_ast_node_new(context->ast_alloc, sizeof(AstAddressOf), Ast_Kind_Address_Of);
    if (node->token) ao->token = node->token;
    ao->expr = node;

    return ao;
}

AstLocal* make_local(Context *context, OnyxToken* token, AstType* type_node) {
    AstLocal* local = onyx_ast_node_new(context->ast_alloc, sizeof(AstLocal), Ast_Kind_Local);
    local->token = token;
    local->type_node = type_node;

    return local;
}

AstLocal* make_local_with_type(Context *context, OnyxToken* token, Type* type) {
    AstLocal* local = onyx_ast_node_new(context->ast_alloc, sizeof(AstLocal), Ast_Kind_Local);
    local->token = token;
    local->type = type;

    return local;
}

AstNode* make_symbol(Context *context, OnyxToken* sym) {
    AstNode* symbol = onyx_ast_node_new(context->ast_alloc, sizeof(AstTyped), Ast_Kind_Symbol);
    symbol->token = sym;
    return symbol;
}

AstUnaryOp* make_cast(Context *context, AstTyped* expr, Type* to) {
    AstUnaryOp* cast = onyx_ast_node_new(context->ast_alloc, sizeof(AstUnaryOp), Ast_Kind_Unary_Op);
    cast->token = expr->token;
    cast->operation = Unary_Op_Cast;
    cast->expr = expr;
    cast->type = to;
    return cast;
}

AstZeroValue* make_zero_value(Context *context, OnyxToken* token, Type* type) {
    AstZeroValue* zero_value = onyx_ast_node_new(context->ast_alloc, sizeof(AstZeroValue), Ast_Kind_Zero_Value);
    zero_value->token = token;
    zero_value->flags |= Ast_Flag_Comptime;
    zero_value->type = type;
    return zero_value;
}

AstStructLiteral* make_optional_literal_some(Context *context, AstTyped *expr, Type *opt_type) {
    AstStructLiteral *opt_lit = onyx_ast_node_new(context->ast_alloc, sizeof(AstStructLiteral), Ast_Kind_Struct_Literal);
    opt_lit->token = expr->token;

    bh_arr_new(context->ast_alloc, opt_lit->values_to_initialize, 2);
    bh_arr_push(opt_lit->values_to_initialize, ((ValueWithOffset) { (AstTyped *) make_int_literal(context, 1), 0 })); // 1 is Some
    bh_arr_push(opt_lit->values_to_initialize, ((ValueWithOffset) { expr, opt_type->Union.alignment }));

    opt_lit->type = opt_type;
    opt_lit->values_to_initialize[0].value->type = opt_type->Union.tag_type;

    opt_lit->flags |= Ast_Flag_Has_Been_Checked;
    return opt_lit;
}

AstStructLiteral* make_union_variant_of_void(Context *context, Type* union_type, OnyxToken* token, UnionVariant* variant) {
    AstStructLiteral *lit = onyx_ast_node_new(context->ast_alloc, sizeof(AstStructLiteral), Ast_Kind_Struct_Literal);
    lit->token = token;

    assert(variant->type == context->types.basic[Basic_Kind_Void]);

    bh_arr_new(context->ast_alloc, lit->values_to_initialize, 2);
    bh_arr_push(lit->values_to_initialize, ((ValueWithOffset) { (AstTyped *) make_int_literal(context, variant->tag_value), 0 }));
    bh_arr_push(lit->values_to_initialize, ((ValueWithOffset) { (AstTyped *) make_zero_value(context, token, variant->type), union_type->Union.alignment }));

    lit->type = union_type;
    lit->values_to_initialize[0].value->type = union_type->Union.tag_type;

    lit->flags |= Ast_Flag_Has_Been_Checked;
    return lit;
}


void arguments_initialize(Context *context, Arguments* args) {
    if (args->values == NULL)       bh_arr_new(context->gp_alloc, args->values, 2);
    if (args->named_values == NULL) bh_arr_new(context->gp_alloc, args->named_values, 2);

    // CLEANUP: I'm not sure if I need to initialize these to NULL values, but it doesn't hurt.
    fori (i, 0, 2) {
        args->values[i] = NULL;
        args->named_values[i] = NULL;
    }

    args->used_argument_count = -1;
}

void arguments_ensure_length(Context *context, Arguments* args, u32 count) {
    // Make the array big enough
    bh_arr_grow(args->values, count);

    // NULL initialize the new elements
    fori (i, bh_arr_length(args->values), count) args->values[i] = NULL;

    // Set the actual length to the count, but never let it decrease in size
    bh_arr_set_length(args->values, bh_max(count, (u32) bh_arr_length(args->values)));
}

void arguments_copy(Context *context, Arguments* dest, Arguments* src) {
    dest->used_argument_count = -1;
    dest->named_values = src->named_values;

    bh_arr_grow(dest->values, (u32) bh_arr_length(src->values));
    bh_arr_set_length(dest->values, (u32) bh_arr_length(src->values));
    bh_arr_each(AstTyped*, arg, dest->values) *arg = NULL;
    fori (i, 0, bh_arr_length(src->values)) dest->values[i] = src->values[i];
}

// In clone, the named_values are not copied. This is used in find_matching_overload_by_arguments since it doesn't need them to be copied.
void arguments_clone(Context *context, Arguments* dest, Arguments* src) {
    dest->used_argument_count = -1;
    dest->named_values = src->named_values;
    dest->values = bh_arr_copy(context->gp_alloc, src->values);
}

void arguments_deep_clone(Context *context, Arguments* dest, Arguments* src) {
    dest->used_argument_count = -1;
    dest->values = NULL;
    dest->named_values = NULL;

    bh_arr_new(context->gp_alloc, dest->named_values, bh_arr_length(src->named_values));
    bh_arr_new(context->gp_alloc, dest->values, bh_arr_length(src->values));

    bh_arr_each(AstNamedValue *, nv, src->named_values)
        bh_arr_push(dest->named_values, (AstNamedValue *) ast_clone(context, *nv));

    bh_arr_each(AstTyped *, val, src->values)
        bh_arr_push(dest->values, (AstTyped *) ast_clone(context, (AstNode *) *val));
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

void arguments_clear_baked_flags(Arguments* args) {
    bh_arr_each(AstTyped *, arg, args->values) {
        if ((*arg)->kind == Ast_Kind_Argument) {
            ((AstArgument *) *arg)->is_baked = 0;
        }
    }

    bh_arr_each(AstNamedValue *, arg, args->named_values) {
        if ((*arg)->value->kind == Ast_Kind_Argument) {
            ((AstArgument *) (*arg)->value)->is_baked = 0;
        }
    }
}

// GROSS: Using void* to avoid having to cast everything.
const char* node_get_type_name(Context *context, void* node) {
    if (((AstNode *) node)->kind == Ast_Kind_Argument) {
        return node_get_type_name(context, ((AstArgument *) node)->value);
    }

    if (((AstNode *) node)->kind == Ast_Kind_Polymorphic_Proc) {
        return "polymorphic procedure";
    }

    if (((AstNode *) node)->kind == Ast_Kind_Overloaded_Function) {
        return "overloaded procedure";
    }

    if (((AstNode *) node)->kind == Ast_Kind_Alias) {
        return node_get_type_name(context, ((AstAlias *) node)->alias);
    }

    return type_get_name(context, ((AstTyped *) node)->type);
}

b32 static_if_resolution(Context *context, AstIf* static_if) {
    if (static_if->kind != Ast_Kind_Static_If) return 0;

    // assert(condition_value->kind == Ast_Kind_NumLit); // This should be right, right?
    i64 value = get_expression_integer_value(context, static_if->cond, NULL);

    return value != 0;
}

AstPolyCallType* convert_call_to_polycall(Context *context, AstCall* call) {
    // HACK HACK HACK
    AstPolyCallType *pct = onyx_ast_node_new(context->ast_alloc, sizeof(AstPolyCallType), Ast_Kind_Poly_Call_Type);
    pct->token  = call->token;
    pct->next   = call->next;
    pct->callee = (AstType *) call->callee;
    pct->params = (AstNode **) bh_arr_copy(context->gp_alloc, call->args.values);
    bh_arr_each(AstNode *, pp, pct->params) {
        if ((*pp)->kind == Ast_Kind_Argument) {
            *pp = (AstNode *) (*(AstArgument **) pp)->value;
        }
    }

    return pct;
}

void insert_auto_dispose_call(Context *context, AstLocal *local) {
    AstAddressOf *aof = make_address_of(context, (AstTyped *) local);
    aof->token = local->token;
    aof->can_be_removed = 1;

    AstCall *dispose_call = onyx_ast_node_new(context->ast_alloc, sizeof(AstCall), Ast_Kind_Call);
    dispose_call->token = local->token;
    dispose_call->callee = (AstTyped *) context->builtins.dispose_used_local;

    arguments_initialize(context, &dispose_call->args);
    bh_arr_push(dispose_call->args.values, (AstTyped *) make_argument(context, (AstTyped *) aof));

    AstDefer *defered = onyx_ast_node_new(context->ast_alloc, sizeof(AstDefer), Ast_Kind_Defer);
    defered->token = local->token;
    defered->stmt = (AstNode *) dispose_call;

    AstNode **insertion_point = &local->next;
    if (local->type_node == NULL && (*insertion_point)->kind == Ast_Kind_Binary_Op) {
        // Handle `use x := ...`.
        insertion_point = &(*insertion_point)->next;
    }

    defered->next = *insertion_point;
    *insertion_point = (AstNode *) defered;
}


AstCall * create_implicit_for_expansion_call(Context *context, AstFor *fornode) {
    AstCall *call = onyx_ast_node_new(context->ast_alloc, sizeof(AstCall), Ast_Kind_Call);
    call->token = fornode->token;
    call->callee = (AstTyped *) context->builtins.for_expansion;
    call->next = fornode->next;

    arguments_initialize(context, &call->args);

    //
    // Create the code block that will represent the body of the for-loop.
    // This code block is given up to 2 inputs, depending on if the index variable
    // was set in the for-loop. The implementer of a __for_expansion overload must
    // always provide 2 values when `#unquote`-ing, as they cannot currently know if
    // the index variable was asked for.
    //
    // Maybe we could pass `void` as the `index_type` if the index variable is not necessary?
    // I would like to keep the implementations of __for_expansion simple and easy
    // to read, but sometimes there are extra complications you cannot avoid...
    //
    AstCodeBlock *body_code_block = onyx_ast_node_new(context->ast_alloc, sizeof(AstCodeBlock), Ast_Kind_Code_Block);
    body_code_block->token = fornode->token;
    body_code_block->type_node = context->builtins.code_type;
    body_code_block->code = (AstNode *) fornode->stmt;
    ((AstBlock *) body_code_block->code)->rules = Block_Rule_Code_Block;

    bh_arr_new(context->ast_alloc, body_code_block->binding_symbols, bh_arr_length(fornode->indexing_variables));
    bh_arr_each(AstLocal *, indexing_variable, fornode->indexing_variables) {
        CodeBlockBindingSymbol sym;
        sym.symbol    = (*indexing_variable)->token;
        sym.type_node = (*indexing_variable)->type_node;
        bh_arr_push(body_code_block->binding_symbols, sym);
    }

    i32 flags = 0;
    if (fornode->by_pointer) flags |= 1; // BY_POINTER
    if (fornode->no_close)   flags |= 2; // NO_CLOSE
    
    AstNumLit *flag_node = make_int_literal(context, flags);
    flag_node->type_node = context->builtins.for_expansion_flag_type;
    // flag_node->type = type_build_from_ast(context, context->builtins.for_expansion_flag_type);
    // assert(flag_node->type);

    // Arguments are: 
    //    Iterator
    //    Code block with 2 inputs (value, index)
    //    Flags
    bh_arr_push(call->args.values, (AstTyped *) make_argument(context, (AstTyped *) fornode->iter));
    bh_arr_push(call->args.values, (AstTyped *) make_argument(context, (AstTyped *) flag_node));
    bh_arr_push(call->args.values, (AstTyped *) make_argument(context, (AstTyped *) body_code_block));

    return call;
}



b32 resolve_intrinsic_interface_constraint(Context *context, AstConstraint *constraint) {
    AstInterface *interface = constraint->interface;
    Type* type = type_build_from_ast(context, (AstType *) constraint->args[0]);
    if (!type) return 0;

    if (!strcmp(interface->name, "type_is_bool"))     return type_is_bool(type);
    if (!strcmp(interface->name, "type_is_int"))      return type_is_integer(type);
    if (!strcmp(interface->name, "type_is_float"))    return type->kind == Type_Kind_Basic && (type->Basic.flags & Basic_Flag_Float);
    if (!strcmp(interface->name, "type_is_number"))   return type->kind == Type_Kind_Basic && (type->Basic.flags & Basic_Flag_Numeric);
    if (!strcmp(interface->name, "type_is_simd"))     return type->kind == Type_Kind_Basic && (type->Basic.flags & Basic_Flag_SIMD);
    if (!strcmp(interface->name, "type_is_pointer"))  return type_is_pointer(type) || type_is_rawptr(type);
    if (!strcmp(interface->name, "type_is_enum"))     return type->kind == Type_Kind_Enum;
    if (!strcmp(interface->name, "type_is_simple"))   return type->kind == Type_Kind_Basic
                                                          || type->kind == Type_Kind_Enum
                                                          || type->kind == Type_Kind_Pointer;
    if (!strcmp(interface->name, "type_is_array"))    return type->kind == Type_Kind_Array;
    if (!strcmp(interface->name, "type_is_slice"))    return type->kind == Type_Kind_Slice;
    if (!strcmp(interface->name, "type_is_struct"))   return type->kind == Type_Kind_Struct;
    if (!strcmp(interface->name, "type_is_compound")) return type->kind == Type_Kind_Array
                                                          || type->kind == Type_Kind_Slice
                                                          || type->kind == Type_Kind_DynArray
                                                          || type->kind == Type_Kind_Struct;
    if (!strcmp(interface->name, "type_is_function")) return type->kind == Type_Kind_Function;
    return 0;
}
