#define BH_DEBUG

#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxmsgs.h"
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
    "STRUCT TYPE",
    "ENUM TYPE",
    "TYPE_ALIAS",
    "TYPE RAW ALIAS"
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
    "UFC",
    "SIZE OF",
    "ALIGN OF",
    "FILE CONTENTS",
    "STRUCT LITERAL",

    "IF",
    "FOR",
    "WHILE",
    "JUMP",
    "DEFER",
    "SWITCH",
    "SWITCH CASE"

    "AST_NODE_KIND_COUNT",
};

const char* onyx_ast_node_kind_string(AstKind kind) {
    return ast_node_names[kind];
}


void program_info_init(ProgramInfo* prog, bh_allocator alloc) {
    prog->global_scope = scope_create(alloc, NULL);

    bh_table_init(alloc, prog->packages, 16);

    prog->entities = NULL;
    bh_arr_new(alloc, prog->entities, 4);
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
        package->include_scope = scope_create(alloc, parent_scope);
        package->scope = scope_create(alloc, package->include_scope);
        package->private_scope = scope_create(alloc, package->scope);

        bh_table_put(Package *, prog->packages, pac_name, package);

        return package;
    }
}

Scope* scope_create(bh_allocator a, Scope* parent) {
    Scope* scope = bh_alloc_item(a, Scope);
    scope->parent = parent;
    scope->symbols = NULL;

    bh_table_init(global_heap_allocator, scope->symbols, 64);

    return scope;
}

void scope_include(Scope* target, Scope* source) {
    bh_table_each_start(AstNode *, source->symbols);
        symbol_raw_introduce(target, (char *) key, value->token->pos, value);
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
        onyx_message_add(Msg_Type_Redeclare_Symbol, pos, name);
        return 0;
    }

    bh_table_put(AstNode *, scope->symbols, name, symbol);
    return 1;
}

void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node) {
    bh_table_put(AstNode *, scope->symbols, sym, node);
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
        onyx_message_add(Msg_Type_Unknown_Symbol, tkn->pos, tkn->text);
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
    } else if (type_is_integer(res->type)) { \
        res->value.l = left->value.l op right->value.l; \
    } else if (res->type->Basic.kind == Basic_Kind_F32) { \
        res->value.f = left->value.f op right->value.f; \
    } else if (res->type->Basic.kind == Basic_Kind_F64) { \
        res->value.d = left->value.d op right->value.d; \
    } \
    break;

#define REDUCE_BINOP_INT(op) \
    if (type_is_small_integer(res->type) || type_is_bool(res->type)) { \
        res->value.i = left->value.i op right->value.i; \
    } else if (type_is_integer(res->type)) { \
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
    } else if (type_is_integer(unop->type)) { \
        res->value.l = op ((AstNumLit *) unop->expr)->value.l; \
    } else if (unop->type->Basic.kind == Basic_Kind_F32) { \
        res->value.f = op ((AstNumLit *) unop->expr)->value.f; \
    } else if (unop->type->Basic.kind == Basic_Kind_F64) { \
        res->value.d = op ((AstNumLit *) unop->expr)->value.d; \
    } \
    break;

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

        default: return (AstTyped *) unop;
    }

    return (AstTyped *) res;
}

AstTyped* ast_reduce(bh_allocator a, AstTyped* node) {
    assert(node->flags & Ast_Flag_Comptime);

    switch (node->kind) {
        case Ast_Kind_Binary_Op: return (AstTyped *) ast_reduce_binop(a, (AstBinaryOp *) node);
        case Ast_Kind_Unary_Op:  return (AstTyped *) ast_reduce_unaryop(a, (AstUnaryOp *) node);
        case Ast_Kind_NumLit:    return node;
        default:                 return NULL; 
    }
}

void promote_numlit_to_larger(AstNumLit* num) {
    assert(num->type != NULL);

    if (type_is_integer(num->type) && num->type->Basic.size <= 4) {
        // NOTE: Int32, Int16, Int8
        i64 val = (i64) num->value.i;
        num->value.l = val;
    } else if (num->type->Basic.size <= 4) {
        // NOTE: Float32
        f64 val = (f64) num->value.f;
        num->value.d = val;
    }
}

static Type* solve_poly_type(AstNode* target, AstType* type_expr, Type* actual) {
    while (1) {
        if (type_expr == (AstType *) target) return actual;

        switch (type_expr->kind) {
            case Ast_Kind_Pointer_Type: {
                if (actual->kind != Type_Kind_Pointer) return NULL;

                type_expr = ((AstPointerType *) type_expr)->elem;
                actual = actual->Pointer.elem;
                break;
            }

            case Ast_Kind_Slice_Type: {
                if (actual->kind != Type_Kind_Slice) return NULL;

                type_expr = ((AstSliceType *) type_expr)->elem;
                actual = actual->Slice.ptr_to_data->Pointer.elem;
                break;
            }

            case Ast_Kind_DynArr_Type: {
                if (actual->kind != Type_Kind_DynArray) return NULL;

                type_expr = ((AstDynArrType *) type_expr)->elem;
                actual = actual->DynArray.ptr_to_data->Pointer.elem;
                break;
            }

            default:
                return NULL;
        }
    }

    return NULL;
}

AstFunction* polymorphic_proc_lookup(AstPolyProc* pp, AstCall* call) {
    if (pp->concrete_funcs == NULL) {
        bh_table_init(global_heap_allocator, pp->concrete_funcs, 8);
    }

    scope_clear(pp->poly_scope);

    bh_arr_each(AstPolyParam, param, pp->poly_params) {
        AstArgument* arg = call->arguments;
        if (param->idx >= call->arg_count) {
            onyx_message_add(Msg_Type_Literal,
                    call->token->pos,
                    "not enough arguments to polymorphic procedure.");
            return NULL;
        }

        fori (i, 0, param->idx) arg = (AstArgument *) arg->next;
        Type* arg_type = arg->type;

        Type* resolved_type = solve_poly_type(param->poly_sym, param->type_expr, arg_type);

        if (resolved_type == NULL) {
            onyx_message_add(Msg_Type_Literal,
                    call->token->pos,
                    "unable to match polymorphic procedure type.");
            return NULL;
        }

        AstTypeRawAlias* raw = onyx_ast_node_new(semstate.node_allocator, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
        raw->to = resolved_type;

        symbol_introduce(pp->poly_scope, param->poly_sym->token, (AstNode *) raw);
    }

    static char key_buf[1024];
    fori (i, 0, 1024) key_buf[i] = 0;
    bh_table_each_start(AstNode *, pp->poly_scope->symbols);
        strncat(key_buf, bh_bprintf("%s=", key), 1023);
        strncat(key_buf, type_get_name(((AstTypeRawAlias *) value)->to), 1023);
        strncat(key_buf, ";", 1023);
    bh_table_each_end;

    if (bh_table_has(AstFunction *, pp->concrete_funcs, key_buf)) {
        return bh_table_get(AstFunction *, pp->concrete_funcs, key_buf);
    }

    semstate.curr_scope = pp->poly_scope;

    AstFunction* func = (AstFunction *) ast_clone(semstate.node_allocator, pp->base_func);
    bh_table_put(AstFunction *, pp->concrete_funcs, key_buf, func);

    symres_function(func);
    if (check_function_header(func)) return NULL;
    if (check_function(func)) return NULL;

    bh_arr_push(semstate.other_entities, ((Entity) {
        .type = Entity_Type_Function_Header,
        .function = func,
        .package = NULL,
    }));
    bh_arr_push(semstate.other_entities, ((Entity) {
        .type = Entity_Type_Function,
        .function = func,
        .package = NULL,
    }));

    return func;
}

i32 sort_entities(const void* e1, const void* e2) {
    return ((Entity *)e1)->type - ((Entity *)e2)->type;
}
