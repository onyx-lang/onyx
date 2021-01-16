#include "onyxastnodes.h"
#include "onyxsempass.h"
#include "onyxparser.h"
#include "onyxutils.h"

AstNode empty_node = { Ast_Kind_Error, 0, NULL, NULL };

static const char* ast_node_names[] = {
    "ERROR",
    "PROGRAM",
    "PACKAGE",
    "INCLUDE FILE",
    "INCLUDE FOLDER",
    "USE PACKAGE",
    "ALIAS",
    "MEMORY RESERVATION",

    "BINDING",
    "FUNCTION",
    "OVERLOADED_FUNCTION",
    "POLYMORPHIC PROC",
    "BLOCK",
    "LOCAL GROUP",
    "LOCAL",
    "GLOBAL",
    "SYMBOL",

    "UN_OP",
    "BIN_OP",

    "COMPOUND",
    "NAMED_VALUE"

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
    "ARRAY_ACCESS",
    "SLICE",
    "FIELD_ACCESS",
    "PIPE",
    "RANGE",
    "SIZE OF",
    "ALIGN OF",
    "FILE CONTENTS",
    "STRUCT LITERAL",
    "ARRAY LITERAL",

    "IF",
    "FOR",
    "WHILE",
    "JUMP",
    "USE",
    "DEFER",
    "SWITCH",
    "SWITCH CASE",

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

    "|>", "..",
};

const char* entity_state_strings[Entity_State_Count] = {
    "Error",
    "Parse Builtin",
    "Parse",
    "Resolve_Symbols",
    "Check_Types",
    "Code_Gen",
    "Finalized",
};

const char* entity_type_strings[Entity_Type_Count] = {
    "Unknown",
    "Add to Load Path",
    "Load File",
    "Use Package",
    "String Literal",
    "File Contents",
    "Enum",
    "Type Alias",
    "Memory Reservation Type",
    "Use",
    "Polymorphic Proc",
    "Foreign_Function Header",
    "Foreign_Global Header",
    "Function Header",
    "Global Header",
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
    res->flags |= Ast_Flag_Comptime;
    res->type_node = node->type_node;
    res->type = node->type;

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
        res->value.i = op ((AstNumLit *) unop->expr)->value.i; \
    } else if (type_is_integer(unop->type) || unop->type->Basic.kind == Basic_Kind_Int_Unsized) { \
        res->value.l = op ((AstNumLit *) unop->expr)->value.l; \
    } else if (unop->type->Basic.kind == Basic_Kind_F32) { \
        res->value.f = op ((AstNumLit *) unop->expr)->value.f; \
    } else if (unop->type->Basic.kind == Basic_Kind_F64 || unop->type->Basic.kind == Basic_Kind_Float_Unsized) { \
        res->value.d = op ((AstNumLit *) unop->expr)->value.d; \
    } \
    break;

#define REDUCE_UNOP_INT(op) \
    if (type_is_small_integer(unop->type) || type_is_bool(unop->type)) { \
        res->value.i = op ((AstNumLit *) unop->expr)->value.i; \
    } else if (type_is_integer(unop->type)) { \
        res->value.l = op ((AstNumLit *) unop->expr)->value.l; \
    }

AstTyped* ast_reduce_unaryop(bh_allocator a, AstUnaryOp* unop) {
    unop->expr = ast_reduce(a, unop->expr);

    if (unop->expr->kind != Ast_Kind_NumLit) {
        return (AstTyped *) unop;
    }

    AstNumLit* res = onyx_ast_node_new(a, sizeof(AstNumLit), Ast_Kind_NumLit);
    res->token = unop->token;
    res->flags |= Ast_Flag_Comptime;
    res->type_node = unop->type_node;
    res->type = unop->type;

    switch (unop->operation) {
        case Unary_Op_Negate: REDUCE_UNOP(-);
        case Unary_Op_Not: {
            if (type_is_bool(res->type)) res->value.i = ! ((AstNumLit *) unop->expr)->value.i;
            break;
        }
        case Unary_Op_Bitwise_Not: REDUCE_UNOP_INT(~);

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
        num->type = type_build_from_ast(semstate.allocator, num->type_node);
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
b32 type_check_or_auto_cast(AstTyped** pnode, Type* type) {
    AstTyped* node = *pnode;
    assert(type != NULL);
    assert(node != NULL);

    if (node_is_type((AstNode *) node)) return 0;

    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        AstFunction* func = polymorphic_proc_lookup((AstPolyProc *) node, PPLM_By_Function_Type, type, node->token);
        if (func == NULL) return 0;

        *pnode = (AstTyped *) func;
        node = *pnode;
    }

    // HACK: NullProcHack
    if (type->kind == Type_Kind_Function && (node->flags & Ast_Flag_Proc_Is_Null) != 0) return 1;

    if (types_are_compatible(node->type, type)) return 1;
    if (node_is_auto_cast((AstNode *) node)) {
        char* dummy;
        if (!cast_is_legal(((AstUnaryOp *) node)->expr->type, type, &dummy)) {
            return 0;

        } else {
            ((AstUnaryOp *) node)->type = type;
            return 1;
        }
    }
    else if (node->kind == Ast_Kind_NumLit) {
        if (convert_numlit_to_type((AstNumLit *) node, type)) return 1;
    }
    else if (node->kind == Ast_Kind_Compound) {
        if (type->kind != Type_Kind_Compound) return 0;

        AstCompound* compound = (AstCompound *) node;

        u32 expr_count = bh_arr_length(compound->exprs);
        if (expr_count != type->Compound.count) return 0;

        fori (i, 0, (i64) expr_count) {
            if (!type_check_or_auto_cast(&compound->exprs[i], type->Compound.types[i])) return 0;
        }

        compound->type = type_build_compound_type(semstate.node_allocator, compound);
        
        return 1;
    }

    return 0;
}

Type* resolve_expression_type(AstTyped* node) {
    if (node->kind == Ast_Kind_Compound) {
        bh_arr_each(AstTyped *, expr, ((AstCompound *) node)->exprs) {
            resolve_expression_type(*expr);
        }
    }

    if (node->kind == Ast_Kind_Argument) {
        node->type = resolve_expression_type(((AstArgument *) node)->value);
    }

    if (node_is_type((AstNode *) node)) {
        return NULL;
    }

    if (node->type == NULL)
        node->type = type_build_from_ast(semstate.allocator, node->type_node);

    if (node->kind == Ast_Kind_NumLit && node->type->kind == Type_Kind_Basic) {
        if (node->type->Basic.kind == Basic_Kind_Int_Unsized) {
            if ((((u64) ((AstNumLit *) node)->value.l) >> 32) > 0)
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

static const b32 cast_legality[][11] = {
    /* I8  */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0 },
    /* U8  */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0 },
    /* I16 */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0 },
    /* U16 */ { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0 },
    /* I32 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    /* U32 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    /* I64 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    /* U64 */ { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    /* F32 */ { 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0 },
    /* F64 */ { 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0 },
    /* PTR */ { 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1 },
};

b32 cast_is_legal(Type* from_, Type* to_, char** err_msg) {
    Type* from = from_;
    Type* to   = to_;

    if (from->kind == Type_Kind_Enum) from = from->Enum.backing;
    if (to->kind == Type_Kind_Enum) to = to->Enum.backing;

    if (from->kind == Type_Kind_Struct || to->kind == Type_Kind_Struct) {
        *err_msg = "Cannot cast to or from a struct.";
        return 0;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_Array) {
        if (!types_are_compatible(to->Slice.ptr_to_data->Pointer.elem, from->Array.elem)) {
            *err_msg = "Array to slice cast is not valid here because the types are different.";
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

    if (fromidx != -1 && toidx != -1) {
        if (!cast_legality[fromidx][toidx]) {
            *err_msg = bh_aprintf(global_heap_allocator, "Cast from '%s' to '%s' is not allowed.", type_get_name(from_), type_get_name(to_));
            return 0;
        }
    }

    *err_msg = NULL;
    return 1;
}

char* get_function_name(AstFunction* func) {
    if (func->name != NULL) {
        return bh_aprintf(global_scratch_allocator, "%b", func->name->text, func->name->length);
    }

    if (func->exported_name != NULL) {
        return bh_aprintf(global_scratch_allocator,
                "EXPORTED:%b",
                func->exported_name->text,
                func->exported_name->length);
    }

    return "<anonymous procedure>";
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

void arguments_initialize(Arguments* args) {
    if (args->values == NULL)       bh_arr_new(global_heap_allocator, args->values, 2);
    if (args->named_values == NULL) bh_arr_new(global_heap_allocator, args->named_values, 2);

    // CLEANUP: I'm not sure if I need to initialize these to NULL values, but it doesn't hurt.
    fori (i, 0, 2) {
        args->values[i] = NULL;
        args->named_values[i] = NULL;
    }
}

void arguments_ensure_length(Arguments* args, u32 count) {
    // Make the array big enough
    bh_arr_grow(args->values, count);

    // NULL initialize the new elements
    fori (i, bh_arr_length(args->values), count) args->values[i] = NULL;

    // Set the actual length to the count, but never let it decrease in size
    bh_arr_set_length(args->values, bh_max(count, (u32) bh_arr_length(args->values)));
}

// In clone, the named_values are not copied. This is used in match_overloaded_function since it doesn't need them to be copied.
void arguments_clone(Arguments* dest, Arguments* src) {
    dest->named_values = src->named_values;
    dest->values = bh_arr_copy(global_heap_allocator, src->values);
}

void arguments_deep_clone(bh_allocator a, Arguments* dest, Arguments* src) {
    dest->values = NULL;
    dest->named_values = NULL;

    bh_arr_new(global_heap_allocator, dest->named_values, bh_arr_length(src->named_values));
    bh_arr_new(global_heap_allocator, dest->values, bh_arr_length(src->values));

    bh_arr_each(AstNamedValue *, nv, src->named_values)
        bh_arr_push(dest->named_values, (AstNamedValue *) ast_clone(a, *nv));

    bh_arr_each(AstTyped *, val, src->values)
        bh_arr_push(dest->values, (AstTyped *) ast_clone(a, (AstNode *) *val));
}

void arguments_removed_baked(Arguments* args) {
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

    return type_get_name(((AstTyped *) node)->type);
}
