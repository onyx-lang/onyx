#define BH_DEBUG

#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxerrors.h"
#include "onyxparser.h"
#include "onyxastnodes.h"
#include "onyxsempass.h"


bh_scratch global_scratch;
bh_allocator global_scratch_allocator;

bh_managed_heap global_heap;
bh_allocator global_heap_allocator;

static AstNode empty_node = { Ast_Kind_Error, 0, NULL, NULL };

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


void program_info_init(ProgramInfo* prog, bh_allocator alloc) {
    prog->global_scope = scope_create(alloc, NULL, (OnyxFilePos) { 0 });

    bh_table_init(alloc, prog->packages, 16);

    // NOTE: This will be initialized upon the first call to entity_heap_insert.
    prog->entities.entities = NULL;
}

Package* program_info_package_lookup(ProgramInfo* prog, char* package_name) {
    if (bh_table_has(Package *, prog->packages, package_name)) {
        return bh_table_get(Package *, prog->packages, package_name);
    } else {
        return NULL;
    }
}

Package* program_info_package_lookup_or_create(ProgramInfo* prog, char* package_name, Scope* parent_scope, bh_allocator alloc) {
    if (bh_table_has(Package *, prog->packages, package_name)) {
        return bh_table_get(Package *, prog->packages, package_name);

    } else {
        Package* package = bh_alloc_item(alloc, Package);

        char* pac_name = bh_alloc_array(alloc, char, strlen(package_name) + 1);
        memcpy(pac_name, package_name, strlen(package_name) + 1);

        package->name = pac_name;
        package->scope = scope_create(alloc, parent_scope, (OnyxFilePos) { 0 });
        package->private_scope = scope_create(alloc, package->scope, (OnyxFilePos) { 0 });

        bh_table_put(Package *, prog->packages, pac_name, package);

        return package;
    }
}

Scope* scope_create(bh_allocator a, Scope* parent, OnyxFilePos created_at) {
    Scope* scope = bh_alloc_item(a, Scope);
    scope->parent = parent;
    scope->created_at = created_at;
    scope->symbols = NULL;

    bh_table_init(global_heap_allocator, scope->symbols, 64);

    return scope;
}

void scope_include(Scope* target, Scope* source, OnyxFilePos pos) {
    bh_table_each_start(AstNode *, source->symbols);
        symbol_raw_introduce(target, (char *) key, pos, value);
    bh_table_each_end;
}

b32 symbol_introduce(Scope* scope, OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    b32 ret = symbol_raw_introduce(scope, tkn->text, tkn->pos, symbol);

    token_toggle_end(tkn);
    return ret;
}

b32 symbol_raw_introduce(Scope* scope, char* name, OnyxFilePos pos, AstNode* symbol) {
    if (bh_table_has(AstNode *, scope->symbols, name)) {
        if (bh_table_get(AstNode *, scope->symbols, name) != symbol) {
            onyx_report_error(pos, "Redeclaration of symbol '%s'.", name);
            return 0;
        }

        return 1;
    }

    bh_table_put(AstNode *, scope->symbols, name, symbol);
    return 1;
}

void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node) {
    bh_table_put(AstNode *, scope->symbols, sym, node);
}

void symbol_subpackage_introduce(Scope* scope, OnyxToken* sym, AstPackage* package) {
    token_toggle_end(sym);

    if (bh_table_has(AstNode *, scope->symbols, sym->text)) {
        AstNode* maybe_package = bh_table_get(AstNode *, scope->symbols, sym->text);
        assert(maybe_package->kind == Ast_Kind_Package);
    } else {
        bh_table_put(AstNode *, scope->symbols, sym->text, (AstNode *) package);
    }

    token_toggle_end(sym);
}

AstNode* symbol_raw_resolve(Scope* start_scope, char* sym) {
    AstNode* res = NULL;
    Scope* scope = start_scope;

    while (res == NULL && scope != NULL) {
        if (bh_table_has(AstNode *, scope->symbols, sym)) {
            res = bh_table_get(AstNode *, scope->symbols, sym);
        } else {
            scope = scope->parent;
        }
    }

    if (res == NULL) return NULL;

    if (res->kind == Ast_Kind_Symbol) {
        return symbol_resolve(start_scope, res->token);
    }

    return res;
}

AstNode* symbol_resolve(Scope* start_scope, OnyxToken* tkn) {
    token_toggle_end(tkn);
    AstNode* res = symbol_raw_resolve(start_scope, tkn->text);

    if (res == NULL) {
        onyx_report_error(tkn->pos, "Unable to resolve symbol '%s'.", tkn->text);
        token_toggle_end(tkn);
        return &empty_node;
    }

    token_toggle_end(tkn);
    return res;
}

void scope_clear(Scope* scope) {
    bh_table_clear(scope->symbols);
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
        case Ast_Kind_NumLit:     return node;
        case Ast_Kind_Enum_Value: return (AstTyped *) ((AstEnumValue *) node)->value;
        default:                  return NULL;
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

static void insert_poly_slns_into_scope(Scope* scope, bh_arr(AstPolySolution) slns) {
    bh_arr_each(AstPolySolution, sln, slns) {
        AstNode *node = NULL;

        switch (sln->kind) {
            case PSK_Type:
                node = onyx_ast_node_new(semstate.node_allocator, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
                ((AstTypeRawAlias *) node)->token = sln->poly_sym->token;
                ((AstTypeRawAlias *) node)->to = sln->type;
                break;

            case PSK_Value:
                // CLEANUP: Maybe clone this?
                node = (AstNode *) sln->value;
                break;
        }

        symbol_introduce(scope, sln->poly_sym->token, node);
    }
}

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

static PolySolveResult solve_poly_type(AstNode* target, AstType* type_expr, Type* actual) {
    bh_arr(PolySolveElem) elem_queue = NULL;
    bh_arr_new(global_heap_allocator, elem_queue, 4);

    PolySolveResult result = { PSK_Undefined, { NULL } };

    bh_arr_push(elem_queue, ((PolySolveElem) {
        .type_expr = type_expr,
        .kind = PSK_Type,
        .actual    = actual
    }));

    while (!bh_arr_is_empty(elem_queue)) {
        PolySolveElem elem = elem_queue[0];
        bh_arr_deleten(elem_queue, 0, 1);

        if (elem.type_expr == (AstType *) target) {
            result.kind = elem.kind;

            assert(elem.kind != PSK_Undefined);
            if (result.kind == PSK_Type)  result.actual = elem.actual;
            if (result.kind == PSK_Value) result.value = elem.value;
            break;
        }

        if (elem.kind != PSK_Type) continue;

        switch (elem.type_expr->kind) {
            case Ast_Kind_Pointer_Type: {
                if (elem.actual->kind != Type_Kind_Pointer) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstPointerType *) elem.type_expr)->elem,
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
                    .value = (AstTyped *) make_int_literal(semstate.node_allocator, elem.actual->Array.count)
                }));

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstArrayType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->Array.elem,
                }));
                break;
            }

            case Ast_Kind_Slice_Type: {
                if (elem.actual->kind != Type_Kind_Slice) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstSliceType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->Slice.ptr_to_data->Pointer.elem,
                }));
                break;
            }

            case Ast_Kind_DynArr_Type: {
                if (elem.actual->kind != Type_Kind_DynArray) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstDynArrType *) elem.type_expr)->elem,
                    .kind = PSK_Type,
                    .actual = elem.actual->DynArray.ptr_to_data->Pointer.elem,
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

            case Ast_Kind_Poly_Call_Type: {
                if (elem.actual->kind != Type_Kind_Struct) break;
                if (bh_arr_length(elem.actual->Struct.poly_sln) != bh_arr_length(((AstPolyCallType *) elem.type_expr)->params)) break;

                AstPolyCallType* pt = (AstPolyCallType *) elem.type_expr;

                fori (i, 0, bh_arr_length(pt->params)) {
                    PolySolutionKind kind = elem.actual->Struct.poly_sln[i].kind;
                    if (kind == PSK_Type) {
                        bh_arr_push(elem_queue, ((PolySolveElem) {
                            .kind = kind,
                            .type_expr = (AstType *) pt->params[i],
                            .actual = elem.actual->Struct.poly_sln[i].type,
                        }));
                    } else {
                        bh_arr_push(elem_queue, ((PolySolveElem) {
                            .kind = kind,
                            .type_expr = (AstType *) pt->params[i],
                            .value = elem.actual->Struct.poly_sln[i].value,
                        }));
                    }
                }

                break;
            }

            default: break;
        }
    }

    bh_arr_free(elem_queue);

    return result;
}

AstFunction* polymorphic_proc_lookup(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxFilePos pos) {
    if (pp->concrete_funcs == NULL) {
        bh_table_init(global_heap_allocator, pp->concrete_funcs, 8);
    }

    bh_arr(AstPolySolution) slns = NULL;
    bh_arr_new(global_heap_allocator, slns, bh_arr_length(pp->poly_params));

    bh_arr_each(AstPolySolution, known_sln, pp->known_slns) bh_arr_push(slns, *known_sln);

    bh_arr_each(AstPolyParam, param, pp->poly_params) {
        b32 already_solved = 0;
        bh_arr_each(AstPolySolution, known_sln, pp->known_slns) {
            if (token_equals(param->poly_sym->token, known_sln->poly_sym->token)) {
                already_solved = 1;
                break;
            }
        }
        if (already_solved) continue;

        Type* actual_type;

        if (pp_lookup == PPLM_By_Call) {
            if (param->idx >= ((AstCall *) actual)->arg_count) {
                onyx_report_error(pos, "Not enough arguments to polymorphic procedure.");
                return NULL;
            }

            bh_arr(AstArgument *) arg_arr = ((AstCall *) actual)->arg_arr;
            actual_type = resolve_expression_type(arg_arr[param->idx]->value);
        }

        else if (pp_lookup == PPLM_By_Function_Type) {
            Type* ft = (Type*) actual;
            if (param->idx >= ft->Function.param_count) {
                onyx_report_error(pos, "Incompatible polymorphic argument to function paramter.");
                return NULL;
            }

            actual_type = ft->Function.params[param->idx];
        }

        else {
            onyx_report_error(pos, "Cannot resolve polymorphic function type.");
            return NULL;
        }

        PolySolveResult resolved = solve_poly_type(param->poly_sym, param->type_expr, actual_type);

        switch (resolved.kind) {
            case PSK_Undefined:
                onyx_report_error(pos,
                    "Unable to solve for polymoprhic variable '%b', using the type '%s'.",
                    param->poly_sym->token->text,
                    param->poly_sym->token->length,
                    type_get_name(actual_type));
                return NULL;

            case PSK_Type:
                bh_arr_push(slns, ((AstPolySolution) {
                    .kind     = PSK_Type,
                    .poly_sym = param->poly_sym,
                    .type     = resolved.actual,
                }));
                break;

            case PSK_Value:
                bh_arr_push(slns, ((AstPolySolution) {
                    .kind     = PSK_Value,
                    .poly_sym = param->poly_sym,
                    .value    = resolved.value,
                }));
                break;
        }
    }

    AstFunction* result = polymorphic_proc_solidify(pp, slns, pos);
    
    bh_arr_free(slns);
    return result;
}

// NOTE: This might return a volatile string. Do not store it without copying it.
static char* build_poly_solution_key(AstPolySolution* sln) {
    static char buffer[128];

    if (sln->kind == PSK_Type) {
        return (char *) type_get_unique_name(sln->type);
    }

    else if (sln->kind == PSK_Value) {
        fori (i, 0, 128) buffer[i] = 0;

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
static char* build_poly_slns_unique_key(bh_arr(AstPolySolution) slns) {
    static char key_buf[1024];
    fori (i, 0, 1024) key_buf[i] = 0;

    bh_arr_each(AstPolySolution, sln, slns) {
        token_toggle_end(sln->poly_sym->token);

        strncat(key_buf, sln->poly_sym->token->text, 1023);
        strncat(key_buf, "=", 1023);
        strncat(key_buf, build_poly_solution_key(sln), 1023);
        strncat(key_buf, ";", 1023);

        token_toggle_end(sln->poly_sym->token);
    }

    return key_buf;
}

AstFunction* polymorphic_proc_solidify(AstPolyProc* pp, bh_arr(AstPolySolution) slns, OnyxFilePos pos) {
    if (pp->concrete_funcs == NULL) {
        bh_table_init(global_heap_allocator, pp->concrete_funcs, 8);
    }

    // NOTE: Check if a version of this polyproc has already been created.
    char* unique_key = build_poly_slns_unique_key(slns);
    if (bh_table_has(AstFunction *, pp->concrete_funcs, unique_key)) {
        return bh_table_get(AstFunction *, pp->concrete_funcs, unique_key);
    }

    Scope* poly_scope = scope_create(semstate.node_allocator, pp->poly_scope, pos);
    insert_poly_slns_into_scope(poly_scope, slns);

    AstFunction* func = (AstFunction *) ast_clone(semstate.node_allocator, pp->base_func);
    bh_table_put(AstFunction *, pp->concrete_funcs, unique_key, func);

    func->flags |= Ast_Flag_Function_Used;

    Entity func_header_entity = {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Function_Header,
        .function = func,
        .package = NULL,
        .scope = poly_scope,
    };

    entity_bring_to_state(&func_header_entity, Entity_State_Code_Gen);
    if (onyx_has_errors()) {
        onyx_report_error(pos, "Error in polymorphic procedure header generated from this call site.");
        return NULL;
    }

    Entity func_entity = {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Function,
        .function = func,
        .package = NULL,
        .scope = poly_scope,
    };

    entity_heap_insert(&semstate.program->entities, func_header_entity);
    entity_heap_insert(&semstate.program->entities, func_entity);
    return func;
}

// NOTE: This can return either a AstFunction or an AstPolyProc, depending if enough parameters were
// supplied to remove all the polymorphic variables from the function.
AstNode* polymorphic_proc_try_solidify(AstPolyProc* pp, bh_arr(AstPolySolution) slns, OnyxFilePos pos) {
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
            onyx_report_error(pos, "'%b' is not a type variable of '%b'.",
                sln->poly_sym->token->text, sln->poly_sym->token->length,
                pp->token->text, pp->token->length);
            return (AstNode *) pp;
        }
    }

    if (valid_argument_count == bh_arr_length(pp->poly_params)) {
        return (AstNode *) polymorphic_proc_solidify(pp, slns, pos);

    } else {
        // HACK: Some of these initializations assume that the entity for this polyproc has
        // made it through the symbol resolution phase.
        //                                                    - brendanfh 2020/12/25
        AstPolyProc* new_pp = onyx_ast_node_new(semstate.node_allocator, sizeof(AstPolyProc), Ast_Kind_Polymorphic_Proc);
        new_pp->token = pp->token;                            // TODO: Change this to be the solidify->token
        new_pp->base_func = pp->base_func;
        new_pp->poly_scope = new_pp->poly_scope;
        new_pp->flags = pp->flags;
        new_pp->poly_params = pp->poly_params;

        // POTENTIAL BUG: Copying this doesn't feel right...
        if (pp->concrete_funcs == NULL) {
            bh_table_init(global_heap_allocator, pp->concrete_funcs, 8);
        }
        new_pp->concrete_funcs = pp->concrete_funcs;

        new_pp->known_slns = NULL;
        bh_arr_new(global_heap_allocator, new_pp->known_slns, bh_arr_length(pp->known_slns) + bh_arr_length(slns));

        bh_arr_each(AstPolySolution, sln, pp->known_slns) bh_arr_push(new_pp->known_slns, *sln);
        bh_arr_each(AstPolySolution, sln, slns)           bh_arr_push(new_pp->known_slns, *sln);

        return (AstNode *) new_pp;
    }
}

char* build_poly_struct_name(AstPolyStructType* ps_type, Type* cs_type) {
    char name_buf[256];
    fori (i, 0, 256) name_buf[i] = 0;

    strncat(name_buf, ps_type->name, 255);
    strncat(name_buf, "(", 255);
    bh_arr_each(AstPolySolution, ptype, cs_type->Struct.poly_sln) {
        if (ptype != cs_type->Struct.poly_sln)
            strncat(name_buf, ", ", 255);

        // This logic will have to be other places as well.

        switch (ptype->kind) {
            case PSK_Undefined: assert(0); break;
            case PSK_Type:      strncat(name_buf, type_get_name(ptype->type), 255); break;
            case PSK_Value: {
                // FIX
                if (ptype->value->kind == Ast_Kind_NumLit) {
                    AstNumLit* nl = (AstNumLit *) ptype->value;
                    if (type_is_integer(nl->type)) {
                        strncat(name_buf, bh_bprintf("%l", nl->value.l), 127);
                    } else {
                        strncat(name_buf, "numlit (FIX ME)", 127);
                    }
                } else {
                    strncat(name_buf, "<expr>", 127);
                }

                break;
            }
        }
    }
    strncat(name_buf, ")", 255);

    return bh_aprintf(global_heap_allocator, "%s", name_buf);
}

AstStructType* polymorphic_struct_lookup(AstPolyStructType* ps_type, bh_arr(AstPolySolution) slns, OnyxFilePos pos) {
    // @Cleanup
    assert(ps_type->scope != NULL);

    if (ps_type->concrete_structs == NULL) {
        bh_table_init(global_heap_allocator, ps_type->concrete_structs, 16);
    }

    if (bh_arr_length(slns) != bh_arr_length(ps_type->poly_params)) {
        onyx_report_error(pos, "Wrong number of arguments for '%s'. Expected %d, got %d",
            ps_type->name,
            bh_arr_length(ps_type->poly_params),
            bh_arr_length(slns));

        return NULL;
    }

    i32 i = 0;
    bh_arr_each(AstPolySolution, sln, slns) {
        sln->poly_sym = (AstNode *) &ps_type->poly_params[i];
        
        PolySolutionKind expected_kind = PSK_Undefined;
        if ((AstNode *) ps_type->poly_params[i].type_node == &type_expr_symbol) {
            expected_kind = PSK_Type;
        } else {
            expected_kind = PSK_Value;
        }

        if (sln->kind != expected_kind) {
            if (expected_kind == PSK_Type) 
                onyx_report_error(pos, "Expected type expression for %d%s argument.", i + 1, bh_num_suffix(i + 1));

            if (expected_kind == PSK_Value)
                onyx_report_error(pos, "Expected value expression of type '%s' for %d%s argument.",
                    type_get_name(ps_type->poly_params[i].type),
                    i + 1, bh_num_suffix(i + 1));

            return NULL;
        }

        if (sln->kind == PSK_Value) {
            if ((sln->value->flags & Ast_Flag_Comptime) == 0) {
                onyx_report_error(pos,
                    "Expected compile-time known argument for '%b'.",
                    sln->poly_sym->token->text,
                    sln->poly_sym->token->length);
                return NULL;
            }

            if (!types_are_compatible(sln->value->type, ps_type->poly_params[i].type)) {
                onyx_report_error(pos, "Expected compile-time argument of type '%s', got '%s'.",
                    type_get_name(ps_type->poly_params[i].type),
                    type_get_name(sln->value->type));
                return NULL;
            }
        }

        i++;
    }

    char* unique_key = build_poly_slns_unique_key(slns);
    if (bh_table_has(AstStructType *, ps_type->concrete_structs, unique_key)) {
        return bh_table_get(AstStructType *, ps_type->concrete_structs, unique_key);
    }

    scope_clear(ps_type->scope);
    insert_poly_slns_into_scope(ps_type->scope, slns);

    AstStructType* concrete_struct = (AstStructType *) ast_clone(semstate.node_allocator, ps_type->base_struct);
    bh_table_put(AstStructType *, ps_type->concrete_structs, unique_key, concrete_struct);

    Entity struct_entity = {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Type_Alias,
        .type_alias = (AstType *) concrete_struct,
        .package = NULL,
        .scope = ps_type->scope,
    };
    Entity struct_default_entity = {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Struct_Member_Default,
        .type_alias = (AstType *) concrete_struct,
        .package = NULL,
        .scope = ps_type->scope,
    };

    entity_bring_to_state(&struct_entity, Entity_State_Code_Gen);
    entity_bring_to_state(&struct_default_entity, Entity_State_Code_Gen);
 
    if (onyx_has_errors()) {
        onyx_report_error(pos, "Error in creating polymoprhic struct instantiation here.");
        return NULL;
    }

    Type* cs_type = type_build_from_ast(semstate.node_allocator, (AstType *) concrete_struct);
    cs_type->Struct.poly_sln = NULL;
    bh_arr_new(global_heap_allocator, cs_type->Struct.poly_sln, bh_arr_length(slns));

    fori (i, 0, bh_arr_length(slns)) bh_arr_push(cs_type->Struct.poly_sln, slns[i]);

    cs_type->Struct.name = build_poly_struct_name(ps_type, cs_type);
    return concrete_struct;
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

    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        AstFunction* func = polymorphic_proc_lookup((AstPolyProc *) node, PPLM_By_Function_Type, type, node->token->pos);
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

    return 0;
}

Type* resolve_expression_type(AstTyped* node) {
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

void entity_bring_to_state(Entity* ent, EntityState state) {
    while (ent->state != state) {
        switch (ent->state) {
            case Entity_State_Resolve_Symbols: symres_entity(ent); break;
            case Entity_State_Check_Types:     check_entity(ent);  break;
            case Entity_State_Code_Gen:        emit_entity(ent);   break;

            default: return;
        }

        if (onyx_has_errors()) return;
    }
}
