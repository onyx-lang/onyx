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
    "Include Folder",
    "Include File",
    "Use Package",
    "String Literal",
    "File Contents",
    "Enum",
    "Type Alias",
    "Memory Reservation",
    "Use",
    "Polymorphic Proc",
    "Foreign_Function Header",
    "Foreign_Global Header",
    "Function Header",
    "Global Header",
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
        bh_table_put(AstNode *, scope->symbols, sym->text, package);
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
    } else if (type_is_integer(res->type) || res->type->Basic.kind == Basic_Kind_Int_Unsized) { \
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
    } else if (type_is_integer(res->type) || res->type->Basic.kind == Basic_Kind_Int_Unsized) { \
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
        case Ast_Kind_Enum_Value: return (AstTyped *) ast_reduce(a, (AstTyped *) ((AstEnumValue *) node)->value);
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

typedef struct PolySolveElem {
    AstType* type_expr;
    Type*    actual;
} PolySolveElem;

static Type* solve_poly_type(AstNode* target, AstType* type_expr, Type* actual) {
    bh_arr(PolySolveElem) elem_queue = NULL;
    bh_arr_new(global_heap_allocator, elem_queue, 4);

    Type* result = NULL;

    bh_arr_push(elem_queue, ((PolySolveElem) {
        .type_expr = type_expr,
        .actual    = actual
    }));

    while (!bh_arr_is_empty(elem_queue)) {
        PolySolveElem elem = elem_queue[0];
        bh_arr_deleten(elem_queue, 0, 1);

        if (elem.type_expr == (AstType *) target) {
            result = elem.actual;
            break;
        }

        switch (elem.type_expr->kind) {
            case Ast_Kind_Pointer_Type: {
                if (elem.actual->kind != Type_Kind_Pointer) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstPointerType *) elem.type_expr)->elem,
                    .actual = elem.actual->Pointer.elem,
                }));
                break;
            }

            case Ast_Kind_Array_Type: {
                // TODO: Add check for same size array
                if (elem.actual->kind != Type_Kind_Array) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstArrayType *) elem.type_expr)->elem,
                    .actual = elem.actual->Array.elem,
                }));
                break;
            }

            case Ast_Kind_Slice_Type: {
                if (elem.actual->kind != Type_Kind_Slice) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstSliceType *) elem.type_expr)->elem,
                    .actual = elem.actual->Slice.ptr_to_data->Pointer.elem,
                }));
                break;
            }

            case Ast_Kind_DynArr_Type: {
                if (elem.actual->kind != Type_Kind_DynArray) break;

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstDynArrType *) elem.type_expr)->elem,
                    .actual = elem.actual->DynArray.ptr_to_data->Pointer.elem,
                }));
                break;
            }

            case Ast_Kind_VarArg_Type:
                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ((AstVarArgType *) elem.type_expr)->elem,
                    .actual = actual,
                }));
                break;

            case Ast_Kind_Function_Type: {
                if (elem.actual->kind != Type_Kind_Function) break;

                AstFunctionType* ft = (AstFunctionType *) elem.type_expr;

                fori (i, 0, ft->param_count) {
                    bh_arr_push(elem_queue, ((PolySolveElem) {
                        .type_expr = ft->params[i],
                        .actual = elem.actual->Function.params[i],
                    }));
                }

                bh_arr_push(elem_queue, ((PolySolveElem) {
                    .type_expr = ft->return_type,
                    .actual = elem.actual->Function.return_type,
                }));

                break;
            }

            case Ast_Kind_Poly_Call_Type: {
                if (elem.actual->kind != Type_Kind_Struct) break;
                if (bh_arr_length(elem.actual->Struct.poly_args) != bh_arr_length(((AstPolyCallType *) elem.type_expr)->params)) break;

                AstPolyCallType* pt = (AstPolyCallType *) elem.type_expr;

                fori (i, 0, bh_arr_length(pt->params)) {
                    bh_arr_push(elem_queue, ((PolySolveElem) {
                        .type_expr = pt->params[i],
                        .actual = elem.actual->Struct.poly_args[i],
                    }));
                }

                break;
            }

            default: break;
        }
    }

solving_done:
    bh_arr_free(elem_queue);

    return result;
}

AstFunction* polymorphic_proc_lookup(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxFilePos pos) {
    if (pp->concrete_funcs == NULL) {
        bh_table_init(global_heap_allocator, pp->concrete_funcs, 8);
    }

    bh_arr(AstPolySolution) slns = NULL;
    bh_arr_new(global_heap_allocator, slns, bh_arr_length(pp->poly_params));

    bh_arr_each(AstPolyParam, param, pp->poly_params) {
        Type* actual_type;

        if (pp_lookup == PPLM_By_Call) {
            AstArgument* arg = ((AstCall *) actual)->arguments;
            if (param->idx >= ((AstCall *) actual)->arg_count) {
                onyx_report_error(pos, "Not enough arguments to polymorphic procedure.");
                return NULL;
            }

            fori (i, 0, param->idx) arg = (AstArgument *) arg->next;
            actual_type = resolve_expression_type(arg->value);
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

        Type* resolved_type = solve_poly_type(param->poly_sym, param->type_expr, actual_type);

        if (resolved_type == NULL) {
            onyx_report_error(pos, "Unable to match polymorphic procedure type with actual type, '%s'.", type_get_name(actual_type));
            return NULL;
        }

        bh_arr_push(slns, ((AstPolySolution) {
            .poly_sym = param->poly_sym,
            .type     = resolved_type
        }));
    }

    AstFunction* result = polymorphic_proc_solidify(pp, slns, pos);
    
    bh_arr_free(slns);
    return result;
}

// NOTE: This returns a volatile string. Do not store it without copying it.
static char* build_polyproc_unique_key(bh_arr(AstPolySolution) slns) {
    static char key_buf[1024];
    fori (i, 0, 1024) key_buf[i] = 0;

    bh_arr_each(AstPolySolution, sln, slns) {
        token_toggle_end(sln->poly_sym->token);

        strncat(key_buf, sln->poly_sym->token->text, 1023);
        strncat(key_buf, "=", 1023);
        strncat(key_buf, type_get_unique_name(sln->type), 1023);
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
    char* unique_key = build_polyproc_unique_key(slns);
    if (bh_table_has(AstFunction *, pp->concrete_funcs, unique_key)) {
        return bh_table_get(AstFunction *, pp->concrete_funcs, unique_key);
    }

    Scope* poly_scope = scope_create(semstate.node_allocator, pp->poly_scope, pos);
    bh_arr_each(AstPolySolution, sln, slns) {
        AstTypeRawAlias* raw = onyx_ast_node_new(semstate.node_allocator, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
        raw->to = sln->type;

        symbol_introduce(poly_scope, sln->poly_sym->token, (AstNode *) raw);
    }

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


AstStructType* polymorphic_struct_lookup(AstPolyStructType* ps_type, bh_arr(Type *) params, OnyxFilePos pos) {
    // @Cleanup
    assert(bh_arr_length(ps_type->poly_params) == bh_arr_length(params));
    assert(ps_type->scope != NULL);

    if (ps_type->concrete_structs == NULL) {
        bh_table_init(global_heap_allocator, ps_type->concrete_structs, 16);
    }

    scope_clear(ps_type->scope);

    fori (i, 0, bh_arr_length(ps_type->poly_params)) {
        if (params[i] == NULL) {
            onyx_report_error((OnyxFilePos) { 0 }, "Type parameter is not a type.");
            return NULL;
        }

        AstTypeRawAlias* raw = onyx_ast_node_new(semstate.node_allocator, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
        raw->to = params[i];

        symbol_introduce(ps_type->scope, ps_type->poly_params[i], (AstNode *) raw);
    }

    char key_buf[1024];
    fori (i, 0, 1024) key_buf[i] = 0;
    bh_table_each_start(AstNode *, ps_type->scope->symbols);
        strncat(key_buf, key, 1023);
        strncat(key_buf, "=", 1023);
        strncat(key_buf, type_get_unique_name(((AstTypeRawAlias *) value)->to), 1023);
        strncat(key_buf, ";", 1023);
    bh_table_each_end;

    if (bh_table_has(AstStructType *, ps_type->concrete_structs, key_buf)) {
        return bh_table_get(AstStructType *, ps_type->concrete_structs, key_buf);
    }

    AstStructType* concrete_struct = (AstStructType *) ast_clone(semstate.node_allocator, ps_type->base_struct);

    Scope* old_scope = semstate.curr_scope;
    semstate.curr_scope = ps_type->scope;
    concrete_struct = (AstStructType *) symres_type((AstType *) concrete_struct);
    semstate.curr_scope = old_scope;

    if (onyx_has_errors()) goto has_error;
    goto no_errors;

has_error:
    // onyx_report_error(pos, "Error in polymorphic struct generated from this call site.");
    return NULL;

no_errors:
    bh_table_put(AstStructType *, ps_type->concrete_structs, key_buf, concrete_struct);

    Type* cs_type = type_build_from_ast(semstate.node_allocator, (AstType *) concrete_struct);

    cs_type->Struct.poly_args = NULL;
    bh_arr_new(global_heap_allocator, cs_type->Struct.poly_args, bh_arr_length(params));

    fori (i, 0, bh_arr_length(params)) bh_arr_push(cs_type->Struct.poly_args, params[i]);

    char name_buf[256];
    fori (i, 0, 256) name_buf[i] = 0;

    strncat(name_buf, ps_type->name, 255);
    strncat(name_buf, "(", 255);
    bh_arr_each(Type *, ptype, cs_type->Struct.poly_args) {
        if (ptype != cs_type->Struct.poly_args)
            strncat(name_buf, ", ", 255);

        strncat(name_buf, type_get_name(*ptype), 255);
    }
    strncat(name_buf, ")", 255);
    cs_type->Struct.name = bh_aprintf(semstate.node_allocator, "%s", name_buf);

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
                if (value <= ((u64) 1 << (type->Basic.size * 8)) - 1) {
                    num->type = type;
                    return 1;
                }
                
                onyx_report_error(num->token->pos, "Integer constant with value '%l' does not fit into %d-bits.",
                        num->value.l,
                        type->Basic.size * 8);

            } else {
                i64 value = (i64) num->value.l;
                switch (type->Basic.size) {
                    case 1: if (-128 <= value && value <= 127) {
                                num->value.i = (i32) value;
                                num->type = type;
                                return 1;
                            } break;
                    case 2: if (-32768 <= value && value <= 32767) {
                                num->value.i = (i32) value;
                                num->type = type;
                                return 1;
                            } break;
                    case 4: if (-2147483648 <= value && value <= 2147483647) {
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
                    onyx_report_error(num->token->pos, "Integer '%l' does not fit in 32-bit float exactly.", num->value.l);
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

    if (types_are_compatible(node->type, type)) return 1;
    if (node_is_auto_cast((AstNode *) node)) {
        // If the node is an auto cast, we convert it to a cast node which will reports errors if
        // the cast is illegal in the code generation.
        ((AstUnaryOp *) node)->type = type;
        ((AstUnaryOp *) node)->operation = Unary_Op_Cast;
        return 1;
    }
    else if (node->kind == Ast_Kind_NumLit) {
        if (convert_numlit_to_type((AstNumLit *) node, type)) return 1;
    }

    return 0;
}

Type* resolve_expression_type(AstTyped* node) {
    if (node->type == NULL)
        node->type = type_build_from_ast(semstate.allocator, node->type_node);

    if (node->kind == Ast_Kind_NumLit) {
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
