#define BH_DEBUG

#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxerrors.h"
#include "onyxparser.h"
#include "onyxastnodes.h"

bh_scratch global_scratch;
bh_allocator global_scratch_allocator;

bh_managed_heap global_heap;
bh_allocator global_heap_allocator;

//
// Program info and packages
//
Package* package_lookup(char* package_name) {
    if (bh_table_has(Package *, context.packages, package_name)) {
        return bh_table_get(Package *, context.packages, package_name);
    } else {
        return NULL;
    }
}

Package* package_lookup_or_create(char* package_name, Scope* parent_scope, bh_allocator alloc) {
    if (bh_table_has(Package *, context.packages, package_name)) {
        return bh_table_get(Package *, context.packages, package_name);

    } else {
        Package* package = bh_alloc_item(alloc, Package);

        char* pac_name = bh_alloc_array(alloc, char, strlen(package_name) + 1);
        memcpy(pac_name, package_name, strlen(package_name) + 1);
        pac_name[strlen(package_name)] = '\0';

        package->name = pac_name;
        package->scope = scope_create(alloc, parent_scope, (OnyxFilePos) { 0 });
        package->private_scope = scope_create(alloc, package->scope, (OnyxFilePos) { 0 });
        package->use_package_entities = NULL;

        bh_table_put(Package *, context.packages, pac_name, package);

        return package;
    }
}

void package_track_use_package(Package* package, Entity* entity) {
    if (package->use_package_entities == NULL) {
        bh_arr_new(global_heap_allocator, package->use_package_entities, 4);
    }

    bh_arr_push(package->use_package_entities, entity);
}

void package_reinsert_use_packages(Package* package) {
    if (!package->use_package_entities) return;

    bh_arr_each(Entity *, use_package, package->use_package_entities) {
        (*use_package)->state = Entity_State_Resolve_Symbols;
        (*use_package)->macro_attempts = 0;
        entity_heap_insert_existing(&context.entities, *use_package);
    } 

    bh_arr_set_length(package->use_package_entities, 0);
}


//
// Scoping
//
static u64 next_scope_id = 1;

Scope* scope_create(bh_allocator a, Scope* parent, OnyxFilePos created_at) {
    Scope* scope = bh_alloc_item(a, Scope);
    scope->id = next_scope_id++;
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
    if (strcmp(name, "_")) {
        if (bh_table_has(AstNode *, scope->symbols, name)) {
            if (bh_table_get(AstNode *, scope->symbols, name) != symbol) {
                onyx_report_error(pos, "Redeclaration of symbol '%s'.", name);
                return 0;
            }
            return 1;
        }
    }

    bh_table_put(AstNode *, scope->symbols, name, symbol);
    return 1;
}

void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node) {
    bh_table_put(AstNode *, scope->symbols, sym, node);
}

void symbol_subpackage_introduce(Scope* scope, char* sym, AstPackage* package) {
    if (bh_table_has(AstNode *, scope->symbols, sym)) {
        AstNode* maybe_package = bh_table_get(AstNode *, scope->symbols, sym);
        
        // CLEANUP: Make this assertion an actual error message.
        assert(maybe_package->kind == Ast_Kind_Package);
    } else {
        bh_table_put(AstNode *, scope->symbols, sym, (AstNode *) package);
    }
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

    return res;
}

AstNode* symbol_resolve(Scope* start_scope, OnyxToken* tkn) {
    token_toggle_end(tkn);
    AstNode* res = symbol_raw_resolve(start_scope, tkn->text);
    token_toggle_end(tkn);

    return res;
}

AstNode* try_symbol_raw_resolve_from_node(AstNode* node, char* symbol) {
    b32 used_pointer = 0;

    while (1) {
        if (!node) return NULL;

        switch (node->kind) {
            case Ast_Kind_Type_Raw_Alias: node = (AstNode *) ((AstTypeRawAlias *) node)->to->ast_type; break;
            case Ast_Kind_Type_Alias:     node = (AstNode *) ((AstTypeAlias *) node)->to; break;
            case Ast_Kind_Alias:          node = (AstNode *) ((AstAlias *) node)->alias; break;
            case Ast_Kind_Pointer_Type: {
                if (used_pointer) goto all_types_peeled_off;
                used_pointer = 1;

                node = (AstNode *) ((AstPointerType *) node)->elem;
                break;
            }

            default: goto all_types_peeled_off;
        }
    }

all_types_peeled_off:
    if (!node) return NULL;

    switch (node->kind) {
        case Ast_Kind_Package: {
            AstPackage* package = (AstPackage *) node;

            // CLEANUP
            if (package->package == NULL) {
                package->package = package_lookup(package->package_name);
            }

            if (package->package == NULL) {
                return NULL;
            }

            return symbol_raw_resolve(package->package->scope, symbol);
        } 

        case Ast_Kind_Enum_Type: {
            AstEnumType* etype = (AstEnumType *) node;
            return symbol_raw_resolve(etype->scope, symbol);
        }

        case Ast_Kind_Struct_Type: {
            AstStructType* stype = (AstStructType *) node;
            AstNode* result = symbol_raw_resolve(stype->scope, symbol);

            if (result == NULL && stype->stcache != NULL) {
                Type* struct_type = stype->stcache;
                assert(struct_type->kind == Type_Kind_Struct);

                bh_arr_each(AstPolySolution, sln, struct_type->Struct.poly_sln) {
                    if (token_text_equals(sln->poly_sym->token, symbol)) {
                        if (sln->kind == PSK_Type) {
                            result = (AstNode *) sln->type->ast_type;
                        } else {
                            result = (AstNode *) sln->value;
                        }
                    }
                }
            }

            return result;
        }

        case Ast_Kind_Poly_Struct_Type: {
            AstStructType* stype = ((AstPolyStructType *) node)->base_struct;
            return symbol_raw_resolve(stype->scope, symbol);
        }
    }

    return NULL;
}

AstNode* try_symbol_resolve_from_node(AstNode* node, OnyxToken* token) {
    token_toggle_end(token);
    AstNode* result = try_symbol_raw_resolve_from_node(node, token->text);
    token_toggle_end(token);

    return result;
}

void scope_clear(Scope* scope) {
    bh_table_clear(scope->symbols);
}


//
// Polymorphic Procedures
//

AstNode node_that_signals_a_yield = { 0 };

static void ensure_polyproc_cache_is_created(AstPolyProc* pp) {
    if (pp->concrete_funcs == NULL) {
        bh_table_init(global_heap_allocator, pp->concrete_funcs, 16);
    }
}

static void insert_poly_slns_into_scope(Scope* scope, bh_arr(AstPolySolution) slns) {
    bh_arr_each(AstPolySolution, sln, slns) {
        AstNode *node = NULL;

        switch (sln->kind) {
            case PSK_Type:
                node = onyx_ast_node_new(context.ast_alloc, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
                ((AstTypeRawAlias *) node)->token = sln->poly_sym->token;
                ((AstTypeRawAlias *) node)->to = sln->type;
                break;

            case PSK_Value:
                // CLEANUP: Maybe clone this?
                assert(sln->value->flags & Ast_Flag_Comptime);
                node = (AstNode *) sln->value;
                break;
        }

        symbol_introduce(scope, sln->poly_sym->token, node);
    }
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

// NOTE: This function adds a solidified function to the entity heap for it to be processed
// later. It optionally can start the function header entity at the code generation state if
// the header has already been processed.
static b32 add_solidified_function_entities(AstSolidifiedFunction solidified_func, b32 header_already_processed) {
    solidified_func.func->flags |= Ast_Flag_Function_Used;
    solidified_func.func->flags |= Ast_Flag_From_Polymorphism;

    EntityState header_start_state = Entity_State_Resolve_Symbols;
    if (header_already_processed) header_start_state = Entity_State_Code_Gen;

    Entity func_header_entity = {
        .state = header_start_state,
        .type = Entity_Type_Function_Header,
        .function = solidified_func.func,
        .package = NULL,
        .scope = solidified_func.poly_scope,
    };

    entity_bring_to_state(&func_header_entity, Entity_State_Code_Gen);
    if (onyx_has_errors()) return 0;

    Entity func_entity = {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Function,
        .function = solidified_func.func,
        .package = NULL,
        .scope = solidified_func.poly_scope,
    };

    Entity* entity_header = entity_heap_insert(&context.entities, func_header_entity);
    Entity* entity_body   = entity_heap_insert(&context.entities, func_entity);

    solidified_func.func->entity_header = entity_header;
    solidified_func.func->entity_body   = entity_body;

    return 1;
}

// NOTE: This function is responsible for taking all of the information about generating
// a new polymorphic variant, and producing a solidified function. It optionally can only
// generate the header of the function, which is useful for cases such as checking if a
// set of arguments works for a polymorphic overload option.
static AstSolidifiedFunction generate_solidified_function(
    AstPolyProc* pp,
    bh_arr(AstPolySolution) slns,
    OnyxToken* tkn,
    b32 header_only) {

    AstSolidifiedFunction solidified_func;

    // NOTE: Use the position of token if one was provided, otherwise just use NULL.
    OnyxFilePos poly_scope_pos = { 0 };
    if (tkn) poly_scope_pos = tkn->pos;

    solidified_func.poly_scope = scope_create(context.ast_alloc, pp->poly_scope, poly_scope_pos);
    insert_poly_slns_into_scope(solidified_func.poly_scope, slns);

    if (header_only) {
        solidified_func.func = (AstFunction *) clone_function_header(context.ast_alloc, pp->base_func);
        solidified_func.func->flags |= Ast_Flag_Incomplete_Body;

    } else {
        solidified_func.func = (AstFunction *) ast_clone(context.ast_alloc, pp->base_func);
    }

    solidified_func.func->flags |= Ast_Flag_From_Polymorphism;
    solidified_func.func->generated_from = tkn;

    // HACK: Remove the baked parameters from the function defintion so they can be
    // resolved in the poly scope above the function. This does feel kinda of gross
    // and I would love an alternative to tell it to just "skip" the parameter, but
    // that is liable to breaking because it is one more thing to remember.
    //                                             - brendanfh 2021/01/18
    u32 removed_params = 0;
    bh_arr_each(AstPolyParam, param, pp->poly_params) {
        if (param->kind != PPK_Baked_Value) continue;

        bh_arr_deleten(solidified_func.func->params, param->idx - removed_params, 1);
        removed_params++;
    }

    return solidified_func;
}

static void ensure_solidified_function_has_body(AstPolyProc* pp, AstSolidifiedFunction solidified_func) {
    if (solidified_func.func->flags & Ast_Flag_Incomplete_Body) {
        clone_function_body(context.ast_alloc, solidified_func.func, pp->base_func);

        // HACK: I'm asserting that this function should return without an error, because
        // the only case where it can return an error is if there was a problem with the
        // header. This should never be the case in this situation, since the header would
        // have to have successfully passed type checking before it would become a solidified
        // procedure.
        assert(add_solidified_function_entities(solidified_func, 1));

        solidified_func.func->flags &= ~Ast_Flag_Incomplete_Body;
    }
}

// NOTE: These are temporary data structures used to represent the pattern matching system
// of polymoprhic type resolution.
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
// polymoprhic variable. This function takes in three arguments:
//  * The symbol node of the polymorphic parameter being searched for
//  * The type expression that should contain the symbol node it is some where
//  * The actual type to pattern match against
//
// This function utilizes a basic breadth-first search of the type_expr and actual type
// trees, always moving along them in parallel, so when the target is reached (if it is
// ever reached), the "actual" is the matched type/value.
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

                    // CLEANUP: Making an integer literal every time is very very very gross. This should
                    // at least be cached or something.
                    .value = (AstTyped *) make_int_literal(context.ast_alloc, elem.actual->Array.count)
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
static AstTyped* lookup_param_in_arguments(AstPolyProc* pp, AstPolyParam* param, Arguments* args, char** err_msg) {
    bh_arr(AstTyped *) arg_arr = args->values;
    bh_arr(AstNamedValue *) named_values = args->named_values;

    // NOTE: This check is safe because currently the arguments given without a name
    // always map to the beginning indidies of the argument array.
    if (param->idx >= (u64) bh_arr_length(arg_arr)) {
        OnyxToken* param_name = pp->base_func->params[param->idx].local->token;

        bh_arr_each(AstNamedValue *, named_value, named_values) {
            if (token_equals(param_name, (*named_value)->token)) {
                return (AstTyped *) (*named_value)->value;
            }
        }

        // CLEANUP
        if (err_msg) *err_msg = "Not enough arguments to polymorphic procedure. This error message may not be entirely right.";

    } else {
        return (AstTyped *) arg_arr[param->idx];
    }

    return NULL;
}

// NOTE: The job of this function is to solve for type of AstPolySolution using the provided
// information. It is asssumed that the "param" is of kind PPK_Poly_Type. This function uses
// either the arguments provided, or a function type to compare against to pattern match for
// the type that the parameter but be.
static void solve_for_polymorphic_param_type(PolySolveResult* resolved, AstPolyProc* pp, AstPolyParam* param, PolyProcLookupMethod pp_lookup, ptr actual, char** err_msg) {
    Type* actual_type = NULL;

    switch (pp_lookup) {
        case PPLM_By_Arguments: {
            Arguments* args = (Arguments *) actual;

            AstTyped* typed_param = lookup_param_in_arguments(pp, param, args, err_msg);
            if (typed_param == NULL) return;

            actual_type = resolve_expression_type(typed_param);
            if (actual_type == NULL) return;

            break;
        }

        case PPLM_By_Function_Type: {
            Type* ft = (Type *) actual;
            if (param->idx >= ft->Function.param_count) {
                if (err_msg) *err_msg = "Incompatible polymorphic argument to function parameter.";
                return;
            }

            actual_type = ft->Function.params[param->idx];
            break;
        }

        default: return;
    }

    *resolved = solve_poly_type(param->poly_sym, param->type_expr, actual_type);
}

// HACK HACK HACK nocheckin
static b32 flag_to_yield = 0;

// NOTE: The job of this function is to look through the arguments provided and find a matching
// value that is to be baked into the polymorphic procedures poly-scope. It expected that param
// will be of kind PPK_Baked_Value.
// CLEANUP: This function is kind of gross at the moment, because it handles different cases for
// the argument kind. When type expressions (type_expr) become first-class types in the type
// system, this code should be able to be a lot cleaner.
static void solve_for_polymorphic_param_value(PolySolveResult* resolved, AstPolyProc* pp, AstPolyParam* param, PolyProcLookupMethod pp_lookup, ptr actual, char** err_msg) {
    if (pp_lookup != PPLM_By_Arguments) {
        *err_msg = "Function type cannot be used to solved for baked parameter value.";
        return;
    }

    Arguments* args = (Arguments *) actual;
    AstTyped* value = lookup_param_in_arguments(pp, param, args, err_msg);
    if (value == NULL) return;

    // HACK: Storing the original value because if this was an AstArgument, we need to flag
    // it as baked if it is determined that the argument is of the correct kind and type.
    AstTyped* orig_value = value;
    if (value->kind == Ast_Kind_Argument) {
        ((AstArgument *) orig_value)->is_baked = 0;
        value = ((AstArgument *) value)->value;
    }

    if (param->type_expr == (AstType *) &basic_type_type_expr) {
        if (!node_is_type((AstNode *) value)) {
            if (err_msg) *err_msg = "Expected type expression.";
            return;
        }

        Type* resolved_type = type_build_from_ast(context.ast_alloc, (AstType *) value);
        if (resolved_type == NULL) flag_to_yield = 1;

        *resolved = ((PolySolveResult) { PSK_Type, .actual = resolved_type });

    } else {
        resolve_expression_type(value);

        if ((value->flags & Ast_Flag_Comptime) == 0) {
            if (err_msg) *err_msg = "Expected compile-time known argument.";
            return;
        }

        if (param->type == NULL)
            param->type = type_build_from_ast(context.ast_alloc, param->type_expr);

        if (!type_check_or_auto_cast(&value, param->type)) {
            if (err_msg) *err_msg = bh_aprintf(global_scratch_allocator,
                    "The procedure '%s' expects a value of type '%s' for %d%s parameter, got '%s'.",
                    get_function_name(pp->base_func),
                    type_get_name(param->type),
                    param->idx + 1,
                    bh_num_suffix(param->idx + 1),
                    node_get_type_name(value));
            return;
        }

        *resolved = ((PolySolveResult) { PSK_Value, value });
    }

    if (orig_value->kind == Ast_Kind_Argument) {
        ((AstArgument *) orig_value)->is_baked = 1;
    }
}

// NOTE: The job of this function is to take a polymorphic procedure, as well as a method of
// solving for the polymorphic variables, in order to return an array of the solutions for all
// of the polymorphic variables.
static bh_arr(AstPolySolution) find_polymorphic_slns(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual, char** err_msg) {
    bh_arr(AstPolySolution) slns = NULL;
    bh_arr_new(global_heap_allocator, slns, bh_arr_length(pp->poly_params));

    // NOTE: "known solutions" are given through a '#solidify' directive. If this polymorphic
    // procedure is the result of a partially applied solidification, this array will be non-
    // empty and these solutions will be used.
    bh_arr_each(AstPolySolution, known_sln, pp->known_slns) bh_arr_push(slns, *known_sln);

    bh_arr_each(AstPolyParam, param, pp->poly_params) {

        // NOTE: First check to see if this polymorphic variable was already specified in the
        // known solutions.
        b32 already_solved = 0;
        bh_arr_each(AstPolySolution, known_sln, pp->known_slns) {
            if (token_equals(param->poly_sym->token, known_sln->poly_sym->token)) {
                already_solved = 1;
                break;
            }
        }
        if (already_solved) continue;

        // NOTE: Solve for the polymoprhic parameter's value
        PolySolveResult resolved = { PSK_Undefined };
        switch (param->kind) {
            case PPK_Poly_Type:   solve_for_polymorphic_param_type (&resolved, pp, param, pp_lookup, actual, err_msg); break;
            case PPK_Baked_Value: solve_for_polymorphic_param_value(&resolved, pp, param, pp_lookup, actual, err_msg); break;

            default: if (err_msg) *err_msg = "Invalid polymorphic parameter kind. This is a compiler bug.";
        }

        if (flag_to_yield) {
            bh_arr_free(slns);
            return NULL;
        }
        
        switch (resolved.kind) {
            case PSK_Undefined:
                // NOTE: If no error message has been assigned to why this polymorphic parameter
                // resolution was unsuccessful, provide a basic dummy one.
                if (err_msg && *err_msg == NULL)
                    *err_msg = bh_aprintf(global_scratch_allocator,
                        "Unable to solve for polymoprhic variable '%b'.",
                        param->poly_sym->token->text,
                        param->poly_sym->token->length);

                goto sln_not_found;

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

    return slns;

    sln_not_found:
    bh_arr_free(slns);
    return NULL;
}

// NOTE: The job of this function is to be a wrapper to other functions, providing an error
// message if a solution could not be found. This can't be merged with polymoprhic_proc_solidify
// because polymoprhic_proc_try_solidify uses the aforementioned function.
AstFunction* polymorphic_proc_lookup(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxToken* tkn) {
    ensure_polyproc_cache_is_created(pp);

    char *err_msg = NULL;
    bh_arr(AstPolySolution) slns = find_polymorphic_slns(pp, pp_lookup, actual, &err_msg);
    if (slns == NULL) {
        if (flag_to_yield) {
            flag_to_yield = 0;
            return (AstFunction *) &node_that_signals_a_yield;
        }

        if (err_msg != NULL) onyx_report_error(tkn->pos, err_msg);
        else                 onyx_report_error(tkn->pos, "Some kind of error occured when generating a polymorphic procedure. You hopefully will not see this");

        return NULL;
    }

    AstFunction* result = polymorphic_proc_solidify(pp, slns, tkn);
    
    bh_arr_free(slns);
    return result;
}

AstFunction* polymorphic_proc_solidify(AstPolyProc* pp, bh_arr(AstPolySolution) slns, OnyxToken* tkn) {
    ensure_polyproc_cache_is_created(pp);

    // NOTE: Check if a version of this polyproc has already been created.
    char* unique_key = build_poly_slns_unique_key(slns);
    if (bh_table_has(AstSolidifiedFunction, pp->concrete_funcs, unique_key)) {
        AstSolidifiedFunction solidified_func = bh_table_get(AstSolidifiedFunction, pp->concrete_funcs, unique_key);

        // NOTE: If this solution was originally created from a "build_only_header" call, then the body
        // will not have been or type checked, or anything. This ensures that the body is copied, the
        // entities are created and entered into the pipeline.
        ensure_solidified_function_has_body(pp, solidified_func);

        // NOTE: Again, if this came from a "build_only_header" call, then there was no known token and
        // the "generated_from" member will be null. It is best to set it here so errors reported in that
        // function can report where the polymorphic instantiation occurred.
        if (solidified_func.func->generated_from == NULL)
            solidified_func.func->generated_from = tkn;

        return solidified_func.func;
    }

    AstSolidifiedFunction solidified_func = generate_solidified_function(pp, slns, tkn, 0);

    // NOTE: Cache the function for later use, reducing duplicate functions.
    bh_table_put(AstSolidifiedFunction, pp->concrete_funcs, unique_key, solidified_func);

    if (!add_solidified_function_entities(solidified_func, 0)) {
        onyx_report_error(tkn->pos, "Error in polymorphic procedure header generated from this call site.");
        return NULL;
    }

    return solidified_func.func;
}

// NOTE: This can return either a AstFunction or an AstPolyProc, depending if enough parameters were
// supplied to remove all the polymorphic variables from the function.
AstNode* polymorphic_proc_try_solidify(AstPolyProc* pp, bh_arr(AstPolySolution) slns, OnyxToken* tkn) {
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
            onyx_report_error(tkn->pos, "'%b' is not a type variable of '%b'.",
                sln->poly_sym->token->text, sln->poly_sym->token->length,
                pp->token->text, pp->token->length);
            return (AstNode *) pp;
        }
    }

    if (valid_argument_count == bh_arr_length(pp->poly_params)) {
        return (AstNode *) polymorphic_proc_solidify(pp, slns, tkn);

    } else {
        // HACK: Some of these initializations assume that the entity for this polyproc has
        // made it through the symbol resolution phase.
        //                                                    - brendanfh 2020/12/25
        AstPolyProc* new_pp = onyx_ast_node_new(context.ast_alloc, sizeof(AstPolyProc), Ast_Kind_Polymorphic_Proc);
        new_pp->token = pp->token;                            // TODO: Change this to be the solidify->token
        new_pp->base_func = pp->base_func;
        new_pp->poly_scope = new_pp->poly_scope; // CLEANUP: This doesn't seem correct??
        new_pp->flags = pp->flags;
        new_pp->poly_params = pp->poly_params;

        ensure_polyproc_cache_is_created(pp);
        new_pp->concrete_funcs = pp->concrete_funcs;

        new_pp->known_slns = NULL;
        bh_arr_new(global_heap_allocator, new_pp->known_slns, bh_arr_length(pp->known_slns) + bh_arr_length(slns));

        bh_arr_each(AstPolySolution, sln, pp->known_slns) bh_arr_push(new_pp->known_slns, *sln);
        bh_arr_each(AstPolySolution, sln, slns)           bh_arr_push(new_pp->known_slns, *sln);

        return (AstNode *) new_pp;
    }
}

AstFunction* polymorphic_proc_build_only_header(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual) {
    bh_arr(AstPolySolution) slns = find_polymorphic_slns(pp, pp_lookup, actual, NULL);
    if (slns == NULL) return NULL;

    ensure_polyproc_cache_is_created(pp);

    char* unique_key = build_poly_slns_unique_key(slns);
    if (bh_table_has(AstSolidifiedFunction, pp->concrete_funcs, unique_key)) {
        AstSolidifiedFunction solidified_func = bh_table_get(AstSolidifiedFunction, pp->concrete_funcs, unique_key);
        return solidified_func.func;
    }

    // NOTE: This function is only going to have the header of it correctly created.
    // Nothing should happen to this function's body or else the original will be corrupted.
    //                                                      - brendanfh 2021/01/10
    AstSolidifiedFunction solidified_func = generate_solidified_function(pp, slns, NULL, 1);

    Entity func_header_entity = {
        .state = Entity_State_Resolve_Symbols,
        .type = Entity_Type_Function_Header,
        .function = solidified_func.func,
        .package = NULL,
        .scope = solidified_func.poly_scope,
    };

    b32 successful = entity_bring_to_state(&func_header_entity, Entity_State_Code_Gen);
    if (onyx_has_errors()) {
        onyx_clear_errors();
        return NULL;
    }

    // NOTE: Cache the function for later use, only if it didn't have errors in its header.
    bh_table_put(AstSolidifiedFunction, pp->concrete_funcs, unique_key, solidified_func);
    
    return solidified_func.func;
}

//
// Overloaded Procedures
//

//
// @Cleanup: Everything having to do with overload resolving!
// Things that need to be available:
//  * A copy of the arguments list that can be mutated
//      - The named_values do not need to be copied, because they are not modified when fill_in_arguments happens.
//      - Only values needs to have a copy available
//      - This copy needs to be reset after checking every option
//
// Steps needed to check if an overload option is "the one":
//  1. Figure out what the overload is
//      a. If polymorphic, generate the header for the procedure only
//  2. Place the arguments in the copy, according to the overload option
//  3. Ensure the option has a type filled out
//  4. For each argument
//      a. Ensure it has a place to go (not too many arguments)
//      b. Ensure it has a type
//      c. Ensure the types match (currently there could be a problem if an option is attempted and doesn't work all the way that polymorphic procedures as arguments could still be solidified)
//
// Additional features that this needs to account for:
//  * Resolving an overload from a list of parameter types
//  * Resolving an overload from a TypeFunction (so an overloaded procedure can be passed as a parameter)
//

void add_overload_option(bh_arr(OverloadOption)* poverloads, u64 precedence, AstTyped* overload) {
    bh_arr(OverloadOption) overloads = *poverloads;

    i32 index = -1;
    fori (i, 0, bh_arr_length(overloads)) {
        if (overloads[i].precedence > precedence) {
            index = i;
            break;
        }
    }

    if (index < 0) {
        bh_arr_push(overloads, ((OverloadOption) {
            .precedence = precedence,
            .option     = overload,
        }));

    } else {
        bh_arr_insertn(overloads, index, 1);
        overloads[index].precedence = precedence;
        overloads[index].option     = overload;
    }

    *poverloads = overloads;
}

// NOTE: The job of this function is to take a set of overloads, and traverse it to add all possible
// overloads that are reachable. This is slightly more difficult than it may seem. In this language,
// overloaded procedures have a strict ordering to their overloads, which determines how the correct
// match will be found. This was not very complicated until overloaded procedures could be used as
// overload options. This means that you could create an "infinite loop" of overloads like so:
// 
//  o1 :: {                 o2 :: {
//      (...) { ... },          (...) { ... },
//      o2                      o1
//  }                       }
//
// Obviously, this is not really an infinite loop. It just means that all options are available if
// o1 or o2 are called. The difference between o1 and o2 is the order that the overloads will be
// searched. To build the the list of overloads, a hashmap is used to prevent the problem from being
// O(n^2), even though n would (probably) be small. bh_imap has the useful property that it maintains
// an "entries" array that, so long as nothing is ever removed from it, will maintain the order in
// which entries were put into the map. This is useful because a simple recursive algorithm can
// collect all the overloads into the map, and also use the map to provide a base case.
void build_all_overload_options(bh_arr(OverloadOption) overloads, bh_imap* all_overloads) {
    bh_arr_each(OverloadOption, overload, overloads) {
        if (bh_imap_has(all_overloads, (u64) overload->option)) continue;

        bh_imap_put(all_overloads, (u64) overload->option, 1);

        if (overload->option->kind == Ast_Kind_Overloaded_Function) {
            AstOverloadedFunction* sub_overload = (AstOverloadedFunction *) overload->option;
            build_all_overload_options(sub_overload->overloads, all_overloads);
        }
    }
}

AstTyped* find_matching_overload_by_arguments(bh_arr(OverloadOption) overloads, Arguments* param_args) {
    Arguments args;
    arguments_clone(&args, param_args);
    arguments_ensure_length(&args, bh_arr_length(args.values) + bh_arr_length(args.named_values));

    // CLEANUP SPEED: This currently rebuilds the complete set of overloads every time one is looked up.
    // This should be cached in the AstOverloadedFunction or somewhere like that.
    bh_imap all_overloads;
    bh_imap_init(&all_overloads, global_heap_allocator, bh_arr_length(overloads) * 2);
    build_all_overload_options(overloads, &all_overloads);

    AstTyped *matched_overload = NULL;

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) entry->key;
        arguments_copy(&args, param_args);

        AstFunction* overload = NULL;
        switch (node->kind) {
            case Ast_Kind_Function:         overload = (AstFunction *) node; break;
            case Ast_Kind_Polymorphic_Proc: overload = polymorphic_proc_build_only_header((AstPolyProc *) node, PPLM_By_Arguments, param_args); break;
        }

        // NOTE: Overload is not something that is known to be overloadable.
        if (overload == NULL) continue;
        if (overload->kind != Ast_Kind_Function) continue;
        // if (overload->type == NULL) continue;
        // assert(overload->type->kind == Type_Kind_Function);

        // NOTE: If the arguments cannot be placed successfully in the parameters list
        if (!fill_in_arguments(&args, (AstNode *) overload, NULL)) continue;

        TypeFunction* ol_type = &overload->type->Function;
        if (bh_arr_length(args.values) < (i32) ol_type->needed_param_count) continue;

        b32 all_arguments_work = 1;
        fori (i, 0, bh_arr_length(args.values)) {
            if (i >= ol_type->param_count) {
                all_arguments_work = 0;
                break;
            }

            Type* type_to_match = ol_type->params[i];
            AstTyped** value    = &args.values[i];

            if (type_to_match->kind == Type_Kind_VarArgs) type_to_match = type_to_match->VarArgs.ptr_to_data->Pointer.elem;
            if ((*value)->kind == Ast_Kind_Argument) {
                // :ArgumentResolvingIsComplicated
                if (((AstArgument *) (*value))->is_baked) continue;
                value = &((AstArgument *) *value)->value;
            }

            if (!type_check_or_auto_cast(value, type_to_match)) {
                all_arguments_work = 0;
                break;
            }
        }

        if (all_arguments_work) {
            matched_overload = node;
            break;
        }
    }

    bh_imap_free(&all_overloads);
    bh_arr_free(args.values);
    return matched_overload;
}

AstTyped* find_matching_overload_by_type(bh_arr(OverloadOption) overloads, Type* type) {
    if (type->kind != Type_Kind_Function) return NULL;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, global_heap_allocator, bh_arr_length(overloads) * 2);
    build_all_overload_options(overloads, &all_overloads);

    AstTyped *matched_overload = NULL;

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) entry->key;
        if (node->kind == Ast_Kind_Overloaded_Function) continue;

        if (type_check_or_auto_cast(&node, type)) {
            matched_overload = node;
            break;
        }
    }
    
    bh_imap_free(&all_overloads);
    return matched_overload;
}

void report_unable_to_match_overload(AstCall* call) {
    char* arg_str = bh_alloc(global_scratch_allocator, 1024);
    arg_str[0] = '\0';

    bh_arr_each(AstTyped *, arg, call->args.values) {
        strncat(arg_str, node_get_type_name(*arg), 1023);

        if (arg != &bh_arr_last(call->args.values))
            strncat(arg_str, ", ", 1023);
    }

    if (bh_arr_length(call->args.named_values) > 0) {
        if (bh_arr_length(call->args.values) > 0) {
            strncat(arg_str, ", ", 1023);
        }

        bh_arr_each(AstNamedValue *, named_value, call->args.named_values) { 
            token_toggle_end((*named_value)->token);
            strncat(arg_str, (*named_value)->token->text, 1023);
            token_toggle_end((*named_value)->token);

            strncat(arg_str, "=", 1023);
            strncat(arg_str, node_get_type_name((*named_value)->value), 1023); // CHECK: this might say 'unknown'.

            if (named_value != &bh_arr_last(call->args.named_values))
                strncat(arg_str, ", ", 1023);
        }
    }

    onyx_report_error(call->token->pos, "Unable to match overloaded function with provided argument types: (%s)", arg_str);

    bh_free(global_scratch_allocator, arg_str);
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
        if ((AstNode *) ps_type->poly_params[i].type_node == (AstNode *) &basic_type_type_expr) {
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
            resolve_expression_type(sln->value);

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
        AstStructType* concrete_struct = bh_table_get(AstStructType *, ps_type->concrete_structs, unique_key);

        if (concrete_struct->entity_type->state < Entity_State_Check_Types) {
            return NULL;
        }

        Type* cs_type = type_build_from_ast(context.ast_alloc, (AstType *) concrete_struct);
        if (!cs_type) return NULL;

        if (cs_type->Struct.poly_sln == NULL) cs_type->Struct.poly_sln = bh_arr_copy(global_heap_allocator, slns);
        if (cs_type->Struct.name == NULL)     cs_type->Struct.name = build_poly_struct_name(ps_type, cs_type);

        return concrete_struct;
    }

    Scope* sln_scope = scope_create(context.ast_alloc, ps_type->scope, ps_type->token->pos);
    insert_poly_slns_into_scope(sln_scope, slns);

    AstStructType* concrete_struct = (AstStructType *) ast_clone(context.ast_alloc, ps_type->base_struct);
    bh_table_put(AstStructType *, ps_type->concrete_structs, unique_key, concrete_struct);

    add_entities_for_node(NULL, (AstNode *) concrete_struct, sln_scope, NULL);
    return NULL;
}

b32 entity_bring_to_state(Entity* ent, EntityState state) {
    EntityState last_state = ent->state;

    while (ent->state != state) {
        switch (ent->state) {
            case Entity_State_Resolve_Symbols: symres_entity(ent); break;
            case Entity_State_Check_Types:     check_entity(ent);  break;
            case Entity_State_Code_Gen:        emit_entity(ent);   break;

            default: return 0;
        }

        if (ent->state == last_state) return 0;
        last_state = ent->state;

        if (onyx_has_errors()) return 0;
    }

    return 1;
}


//
// Arguments resolving
//
static i32 lookup_idx_by_name(AstNode* provider, char* name) {
    switch (provider->kind) {
        case Ast_Kind_Struct_Literal: {
            AstStructLiteral* sl = (AstStructLiteral *) provider;
            assert(sl->type);

            StructMember s;
            if (!type_lookup_member(sl->type, name, &s)) return -1;
            if (s.included_through_use) return -1;

            return s.idx;
        }

        case Ast_Kind_Function: {
            AstFunction* func = (AstFunction *) provider;

            i32 param_idx = -1;
            i32 idx = 0;
            bh_arr_each(AstParam, param, func->params) {
                if (token_text_equals(param->local->token, name)) {
                    param_idx = idx;
                    break;
                }

                idx++;
            }

            return param_idx;
        }

        default: return -1;
    }
}

static AstNode* lookup_default_value_by_idx(AstNode* provider, i32 idx) {
    switch (provider->kind) {
        case Ast_Kind_Struct_Literal: {
            AstStructLiteral* sl = (AstStructLiteral *) provider;
            assert(sl->type);

            if (sl->type->kind == Type_Kind_Struct) {
                bh_arr(StructMember *) memarr = sl->type->Struct.memarr;
                if (idx >= bh_arr_length(memarr)) return NULL;

                return (AstNode *) *memarr[idx]->initial_value;
            }

            return NULL;
        }

        case Ast_Kind_Function: {
            AstFunction* func = (AstFunction *) provider;

            AstTyped* default_value = func->params[idx].default_value;
            if (default_value == NULL) return NULL;

            AstArgument* arg = make_argument(context.ast_alloc, default_value);
            return (AstNode *) arg;
        }

        default: return NULL;
    }
}

static i32 maximum_argument_count(AstNode* provider) {
    switch (provider->kind) {
        case Ast_Kind_Struct_Literal: {
            AstStructLiteral* sl = (AstStructLiteral *) provider;
            assert(sl->type);

            return type_structlike_mem_count(sl->type);
        }
    }

    // NOTE: This returns int_max for anything other than struct literals because the
    // bounds checking on the arguments will be done elsewhere.
    return 0x7fffffff;
}

// NOTE: The values array can be partially filled out, and is the resulting array.
// Returns if all the values were filled in.
b32 fill_in_arguments(Arguments* args, AstNode* provider, char** err_msg) {

    { // Delete baked arguments
        // :ArgumentResolvingIsComplicated
        i32 max = bh_arr_length(args->values);
        fori (i, 0, max) {
            AstTyped* value = args->values[i];
            if (value == NULL) continue;

            if (value->kind == Ast_Kind_Argument) {
                if (((AstArgument *) value)->is_baked) {
                    i--;
                    max--;
                    bh_arr_deleten(args->values, i, 1);
                }
            }
        }
    }

    if (args->named_values != NULL) {
        bh_arr_each(AstNamedValue *, p_named_value, args->named_values) {
            AstNamedValue* named_value = *p_named_value;

            if (named_value->value->kind == Ast_Kind_Argument) {
                if (((AstArgument *) named_value->value)->is_baked) {
                    // :ArgumentResolvingIsComplicated
                    bh_arr_set_length(args->values, bh_arr_length(args->values) - 1);
                    continue;
                }
            }

            token_toggle_end(named_value->token);
            i32 idx = lookup_idx_by_name(provider, named_value->token->text);
            if (idx == -1) {
                if (err_msg) *err_msg = bh_aprintf(global_scratch_allocator, "'%s' is not a valid named parameter here.", named_value->token->text);
                token_toggle_end(named_value->token);
                return 0;
            }

            // assert(idx < bh_arr_length(args->values));
            if (idx >= bh_arr_length(args->values)) {
                token_toggle_end(named_value->token);
                return 0;
            }

            if (args->values[idx] != NULL && args->values[idx] != named_value->value) {
                if (err_msg) *err_msg = bh_aprintf(global_scratch_allocator, "Multiple values given for parameter named '%s'.", named_value->token->text);
                token_toggle_end(named_value->token);
                return 0;
            }

            args->values[idx] = named_value->value;
            token_toggle_end(named_value->token);
        }
    }

    b32 success = 1;
    fori (idx, 0, bh_arr_length(args->values)) {
        if (args->values[idx] == NULL) args->values[idx] = (AstTyped *) lookup_default_value_by_idx(provider, idx);
        if (args->values[idx] == NULL) {
            *err_msg = bh_aprintf(global_scratch_allocator, "No value given for %d%s argument.", idx + 1, bh_num_suffix(idx + 1));
            success = 0;
        }
    }

    i32 maximum_arguments = maximum_argument_count(provider);
    if (bh_arr_length(args->values) > maximum_arguments) {
        *err_msg = bh_aprintf(global_scratch_allocator, "Too many values provided. Expected at most %d.", maximum_arguments);
        success = 0;
    }

    return success;
}

//
// String parsing helpers
//
u32 char_to_base16_value(char x) {
    if (x >= '0' && x <= '9') return (u32) (x - '0');
    if (x >= 'A' && x <= 'F') return (u32) (x - 'A' + 10);
    if (x >= 'a' && x <= 'f') return (u32) (x - 'a' + 10);
    return 0xffffffff;
}

i32 string_process_escape_seqs(char* dest, char* src, i32 len) {
    i32 total_len = 0;
    for (i32 i = 0; i < len; i++) {
        if (src[i] == '\\') {
            i++;
            switch (src[i]) {
            case '0':  *dest++ = '\0'; total_len++; break;
            case 'a':  *dest++ = '\a'; total_len++; break;
            case 'b':  *dest++ = '\b'; total_len++; break;
            case 'f':  *dest++ = '\f'; total_len++; break;
            case 'n':  *dest++ = '\n'; total_len++; break;
            case 't':  *dest++ = '\t'; total_len++; break;
            case 'r':  *dest++ = '\r'; total_len++; break;
            case 'v':  *dest++ = '\v'; total_len++; break;
            case 'e':  *dest++ = '\e'; total_len++; break;
            case '"':  *dest++ = '"';  total_len++; break;
            case '\\': *dest++ = '\\'; total_len++; break;
            case 'x': {
                u8 ch1 = src[i + 1];
                u8 ch2 = src[i + 2];
                *dest++ = (i8) (char_to_base16_value(ch1) << 4 | char_to_base16_value(ch2));
                total_len++;
                i += 2;
                break;
            }
            default:  *dest++ = '\\';
                      *dest++ = src[i];
                      total_len += 2;
            }
        } else {
            *dest++ = src[i];
            total_len += 1;
        }
    }

    // NOTE: Gotta null terminate
    *dest = 0;

    return total_len;
}

char* lookup_included_file(char* filename, char* relative_to, b32 add_onyx_suffix, b32 search_included_folders) {
    assert(relative_to != NULL);

    static char path[256];
    fori (i, 0, 256) path[i] = 0;

    static char fn[128];
    fori (i, 0, 128) fn[i] = 0;

    if (!bh_str_ends_with(filename, ".onyx") && add_onyx_suffix) {
        bh_snprintf(fn, 128, "%s.onyx", filename);
    } else {
        bh_snprintf(fn, 128, "%s", filename);
    }

#if defined(_BH_LINUX)
    #define DIR_SEPARATOR '/'
#elif defined(_BH_WINDOWS)
    #define DIR_SEPARATOR '\\'
#endif

    fori (i, 0, 128) if (fn[i] == '/') fn[i] = DIR_SEPARATOR;

    if (bh_str_starts_with(filename, "./")) {
        if (relative_to[strlen(relative_to) - 1] != DIR_SEPARATOR)
            bh_snprintf(path, 256, "%s%c%s", relative_to, DIR_SEPARATOR, fn + 2);
        else
            bh_snprintf(path, 256, "%s%s", relative_to, fn + 2);

        if (bh_file_exists(path)) return bh_path_get_full_name(path, global_scratch_allocator);

        return fn;
    }

    if (search_included_folders) {
        bh_arr_each(const char *, folder, context.options->included_folders) {
            if ((*folder)[strlen(*folder) - 1] != DIR_SEPARATOR)
                bh_snprintf(path, 256, "%s%c%s", *folder, DIR_SEPARATOR, fn);
            else
                bh_snprintf(path, 256, "%s%s", *folder, fn);

            if (bh_file_exists(path)) return bh_path_get_full_name(path, global_scratch_allocator);
        }
    }

    return fn;

#undef DIR_SEPARATOR
}

