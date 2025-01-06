#include "utils.h"
#include "lex.h"
#include "astnodes.h"
#include "errors.h"
#include "parser.h"
#include "astnodes.h"
#include "errors.h"
#include "doc.h"

//
// Program info and packages
//

Package* package_lookup(Context *context, char* package_name) {
    i32 index = shgeti(context->packages, package_name);
    if (index != -1) {
        return context->packages[index].value;
    } else {
        return NULL;
    }
}

Package* package_lookup_or_create(Context *context, char* package_name, Scope* parent_scope, OnyxFilePos pos) {
    i32 index = shgeti(context->packages, package_name);
    if (index != -1) {
        return context->packages[index].value;

    } else {
        Package* package = bh_alloc_item(context->ast_alloc, Package);

        char* pac_name = bh_alloc_array(context->ast_alloc, char, strlen(package_name) + 1);
        memcpy(pac_name, package_name, strlen(package_name) + 1);
        pac_name[strlen(package_name)] = '\0';

        package->name = pac_name;
        package->unqualified_name = pac_name + bh_str_last_index_of(pac_name, '.');
        package->use_package_entities = NULL;
        package->id = ++context->next_package_id;
        package->parent_id = -1;
        bh_arr_new(context->gp_alloc, package->sub_packages, 4);

        if (!strcmp(pac_name, "builtin")) {
            package->private_scope = scope_create(context, context->global_scope, pos);
            package->scope = context->global_scope;
        } else {
            package->scope = scope_create(context, parent_scope, pos);
            package->private_scope = scope_create(context, package->scope, pos);
        }

        shput(context->packages, pac_name, package);

        // The builtin package is special. The 'builtin' symbol will be
        // accessible even if you do not `use builtin`.
        if (!strcmp(pac_name, "builtin")) {
            AstPackage* package_node = onyx_ast_node_new(context->ast_alloc, sizeof(AstPackage), Ast_Kind_Package);
            package_node->package_name = package->name;
            package_node->package = package;
            package_node->type_node = context->builtins.package_id_type;
            package_node->flags |= Ast_Flag_Comptime;

            symbol_raw_introduce(context, context->global_scope, pac_name, pos, (AstNode *) package_node);
        }

        return package;
    }
}

void package_track_use_package(Context *context, Package* package, Entity* entity) {
    assert(entity);

    if (package->use_package_entities == NULL) {
        bh_arr_new(context->gp_alloc, package->use_package_entities, 4);
    }

    bh_arr_push(package->use_package_entities, entity);
}

void package_reinsert_use_packages(Context *context, Package* package) {
    if (!package) return;
    if (!package->use_package_entities) return;

    bh_arr_each(Entity *, use_package, package->use_package_entities) {
        (*use_package)->state = Entity_State_Check_Types;
        (*use_package)->macro_attempts = 0;
        entity_heap_insert_existing(&context->entities, *use_package);
    } 

    bh_arr_set_length(package->use_package_entities, 0);
}

void package_mark_as_used(Context *context, Package* package) {
    if (!package) return;
    if (package->is_included_somewhere) return;
    package->is_included_somewhere = 1;
    
    bh_arr_each(Entity *, pent, package->buffered_entities) {
        entity_heap_insert_existing(&context->entities, *pent);
    }

    bh_arr_clear(package->buffered_entities);
}



//
// Scoping
//

Scope* scope_create(Context *context, Scope* parent, OnyxFilePos created_at) {
    Scope* scope = bh_alloc_item(context->ast_alloc, Scope);
    bh_arr_push(context->scopes, scope);

    scope->id = ++context->next_scope_id;
    scope->parent = parent;
    scope->created_at = created_at;
    scope->name = NULL;

    // This will be set on the first symbol insertion.
    scope->symbols = NULL;

    return scope;
}

void scope_include(Context *context, Scope* target, Scope* source, OnyxFilePos pos) {
    fori (i, 0, shlen(source->symbols)) {
        symbol_raw_introduce(context, target, source->symbols[i].key, pos, source->symbols[i].value);
    }
}

b32 symbol_introduce(Context *context, Scope* scope, OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    b32 ret = symbol_raw_introduce(context, scope, tkn->text, tkn->pos, symbol);

    token_toggle_end(tkn);
    return ret;
}

b32 symbol_raw_introduce(Context *context, Scope* scope, char* name, OnyxFilePos pos, AstNode* symbol) {
    if (!scope->symbols) {
        sh_new_arena(scope->symbols);
    }

    if (strcmp(name, "_")) {
        i32 index = shgeti(scope->symbols, name);
        if (index != -1) {
            AstNode *node = scope->symbols[index].value;
            if (node != symbol) {
                ONYX_ERROR(pos, Error_Critical, "Redeclaration of symbol '%s'.", name);

                if (node->token) {
                    ONYX_ERROR(node->token->pos, Error_Critical, "Previous declaration was here.");
                }

                return 0;
            }
            return 1;
        }
    }

    shput(scope->symbols, name, symbol);
    track_declaration_for_symbol_info(context, pos, symbol);
    return 1;
}

void symbol_builtin_introduce(Context *context, Scope* scope, char* sym, AstNode *node) {
    if (!scope->symbols) sh_new_arena(scope->symbols);

    shput(scope->symbols, sym, node);
}

void symbol_subpackage_introduce(Context *context, Package* parent, char* sym, AstPackage* subpackage) {
    Scope *scope = parent->scope;
    if (!scope->symbols) sh_new_arena(scope->symbols);

    i32 index = shgeti(scope->symbols, sym);
    if (index != -1) {
        AstNode* maybe_package = scope->symbols[index].value;
        
        // CLEANUP: Make this assertion an actual error message.
        assert(maybe_package->kind == Ast_Kind_Package);

    } else {
        shput(scope->symbols, sym, (AstNode *) subpackage);

        // Parent: parent->id
        // Child:  subpackage->package->id
        bh_arr_push(parent->sub_packages, subpackage->package->id);
    }
}

AstNode* symbol_raw_resolve_no_ascend(Context *context, Scope* scope, char* sym) {
    if (!scope || !scope->symbols) return NULL;

    i32 index = shgeti(scope->symbols, sym);
    if (index != -1) {
        AstNode* res = scope->symbols[index].value;

        if ((res->flags & Ast_Flag_Symbol_Invisible) == 0) {
            return res;
        }
    }

    return NULL;
}

AstNode* symbol_raw_resolve_limited(Context *context, Scope* start_scope, char* sym, i32 limit) {
    Scope* scope = start_scope;
    AstNode *res = NULL;

    while (scope != NULL && limit-- > 0) {
        res = symbol_raw_resolve_no_ascend(context, scope, sym);
        if (res) {
            return res;
        }

        scope = scope->parent;
    }

    return NULL;
}

AstNode* symbol_raw_resolve(Context *context, Scope* start_scope, char* sym) {
    Scope* scope = start_scope;
    AstNode *res = NULL;

    while (scope != NULL) {
        res = symbol_raw_resolve_no_ascend(context, scope, sym);
        if (res) {
            return res;
        }

        scope = scope->parent;
    }

    return NULL;
}

AstNode* symbol_resolve(Context *context, Scope* start_scope, OnyxToken* tkn) {
    token_toggle_end(tkn);
    AstNode* res = symbol_raw_resolve(context, start_scope, tkn->text);
    token_toggle_end(tkn);

    return res;
}

AstNode* try_symbol_raw_resolve_from_node(Context *context, AstNode* node, char* symbol) {
    // CLEANUP: I think this has a lot of duplication from get_scope_from_node.
    // There are some additional cases handled here, but I think the majority
    // of this code could be rewritten in terms of get_scope_from_node.

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
                package->package = package_lookup(context, package->package_name);
            }

            if (package->package == NULL) {
                return NULL;
            }

            return symbol_raw_resolve_no_ascend(context, package->package->scope, symbol);
        } 

        case Ast_Kind_Foreign_Block:
        case Ast_Kind_Basic_Type:
        case Ast_Kind_Enum_Type:
        case Ast_Kind_Poly_Union_Type:
        case Ast_Kind_Distinct_Type:
        case Ast_Kind_Interface: {
            Scope* scope = get_scope_from_node(context, node);
            return symbol_raw_resolve_no_ascend(context, scope, symbol);
        }

        case Ast_Kind_Slice_Type:
        case Ast_Kind_DynArr_Type: {
            Scope* scope = get_scope_from_node(context, node);

            if (!scope)
                return NULL;

            return symbol_raw_resolve(context, scope, symbol);
        }

        case Ast_Kind_Struct_Type: {
            AstStructType* stype = (AstStructType *) node;

            // Temporarily disable the parent scope so that you can't access things
            // "above" the structures scope. This leads to unintended behavior, as when
            // you are accessing a static element on a structure, you don't expect to
            // bleed to the top level scope.            AstNode *result = NULL;
            AstNode *result = NULL;
            if (stype->stcache != NULL) {
                result = try_symbol_raw_resolve_from_type(context, stype->stcache, symbol);
            }

            if (result == NULL && stype->scope) {
                result = symbol_raw_resolve_no_ascend(context, stype->scope, symbol);
            }

            return result;
        }

        case Ast_Kind_Union_Type: {
            AstUnionType* utype = (AstUnionType *) node;

            AstNode *result = NULL;
            if (utype->utcache != NULL) {
                result = try_symbol_raw_resolve_from_type(context, utype->utcache, symbol);
            }

            if (result == NULL && utype->scope) {
                result = symbol_raw_resolve_no_ascend(context, utype->scope, symbol);
            }

            return result;
        }

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* stype = ((AstPolyStructType *) node);
            if ((AstType *) node == context->builtins.array_type) {
                // We have to ascend on the builtin Array type because it
                // "extends" the Slice type. This is the only structure
                // that works this way. It might be worth considering
                // forcing the use the Slice functions, but then it can
                // get confusing about where every function lives, ya know.
                // Is "get" in Array or Slice.
                return symbol_raw_resolve_limited(context, stype->scope, symbol, 2);

            } else {
                return symbol_raw_resolve_no_ascend(context, stype->scope, symbol);
            }
        }

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pctype = (AstPolyCallType *) node;
            if (pctype->resolved_type) {
                return try_symbol_raw_resolve_from_type(context, pctype->resolved_type, symbol);
            }
            return NULL;
        }

        case Ast_Kind_Compiler_Extension: {
            AstCompilerExtension *ext = (AstCompilerExtension *) node;

            bh_arr_each(AstProceduralMacro *, pmac, ext->proc_macros) {
                if (token_text_equals((*pmac)->token, symbol)) {
                    return (AstNode *) *pmac;
                }
            }

            return NULL;
        }

        case Ast_Kind_Code_Block: {
            AstCodeBlock *block = (AstCodeBlock *) node;

            if (!strcmp(symbol, "capture_count")) {
                return (AstNode *) make_int_literal(context, bh_arr_length(block->binding_symbols));
            }

            if (bh_str_starts_with(symbol, "capture_type_")) {
                char *num = symbol + 13; // go past the "capture_type_"
                int idx = atoi(num); // TODO: Remove use of atoi here...
                if (idx != 0 && idx <= bh_arr_length(block->binding_symbols)) {
                    return (AstNode *) block->binding_symbols[idx - 1].type_node;
                }
            }

            return NULL;
        }

        default: break;
    }

    return NULL;
}

AstNode* try_symbol_resolve_from_node(Context *context, AstNode* node, OnyxToken* token) {
    token_toggle_end(token);
    AstNode* result = try_symbol_raw_resolve_from_node(context, node, token->text);
    token_toggle_end(token);

    return result;
}

static AstNode* try_symbol_raw_resolve_from_poly_sln(Context *context, bh_arr(AstPolySolution) slns, char *symbol) {
    if (slns == NULL) return NULL;

    bh_arr_each(AstPolySolution, sln, slns) {
        if (token_text_equals(sln->poly_sym->token, symbol)) {
            if (sln->kind == PSK_Type) {
                AstTypeRawAlias* alias = onyx_ast_node_new(context->ast_alloc, sizeof(AstTypeRawAlias), Ast_Kind_Type_Raw_Alias);
                alias->type = context->types.basic[Basic_Kind_Type_Index];
                alias->to = sln->type;
                return (AstNode *) alias;

            } else {
                return (AstNode *) sln->value;
            }
        }
    }

    return NULL;
}

AstNode* try_symbol_raw_resolve_from_type(Context *context, Type *type, char* symbol) {
    while (type->kind == Type_Kind_Pointer) {
        type = type->Pointer.elem; 
    }

    switch (type->kind) {
        case Type_Kind_Basic: {
            return symbol_raw_resolve_no_ascend(context, ((AstBasicType *) type->ast_type)->scope, symbol);
        }

        case Type_Kind_Enum: {
            return symbol_raw_resolve_no_ascend(context, ((AstEnumType *) type->ast_type)->scope, symbol);
        }

        case Type_Kind_Slice: {
            return symbol_raw_resolve(context, type->Slice.scope, symbol);
        }

        case Type_Kind_DynArray: {
            return symbol_raw_resolve(context, type->DynArray.scope, symbol);
        }

        case Type_Kind_Struct: {
            AstNode *poly_sln_res = try_symbol_raw_resolve_from_poly_sln(context, type->Struct.poly_sln, symbol);
            if (poly_sln_res) return poly_sln_res;

            i32 limit = 1;
            if (type->Struct.constructed_from) {
                // Structs scope -> Poly Solution Scope -> Poly Struct Scope -> Enclosing Scope
                limit = 3;
            }

            return symbol_raw_resolve_limited(context, type->Struct.scope, symbol, limit);
        }

        case Type_Kind_Union: {
            AstNode *poly_sln_res = try_symbol_raw_resolve_from_poly_sln(context, type->Union.poly_sln, symbol);
            if (poly_sln_res) return poly_sln_res;

            if (!strcmp(symbol, "tag_enum")) {
                return (AstNode *) type->Union.tag_type->ast_type;
            }

            i32 limit = 1;
            if (type->Union.constructed_from) {
                // Structs scope -> Poly Solution Scope -> Poly Struct Scope -> Enclosing Scope
                limit = 3;
            }

            return symbol_raw_resolve_limited(context, type->Union.scope, symbol, limit);
        }

        case Type_Kind_PolyStruct: {
            return symbol_raw_resolve_no_ascend(context, type->PolyStruct.scope, symbol);
        }

        case Type_Kind_PolyUnion: {
            return symbol_raw_resolve_no_ascend(context, type->PolyUnion.scope, symbol);
        }

        case Type_Kind_Distinct: {
            return symbol_raw_resolve(context, type->Distinct.scope, symbol);
        }

        default: return NULL;
    }

    return NULL;
}

AstNode* try_symbol_resolve_from_type(Context *context, Type *type, OnyxToken *token) {
    token_toggle_end(token);
    AstNode* result = try_symbol_raw_resolve_from_type(context, type, token->text);
    token_toggle_end(token);

    return result;
}

void scope_clear(Scope* scope) {
    sh_new_arena(scope->symbols);
}

// Polymorphic procedures are in their own file to clean up this file.
#include "polymorph.h"

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

void add_overload_option(bh_arr(OverloadOption)* poverloads, u64 order, AstTyped* overload) {
    bh_arr(OverloadOption) overloads = *poverloads;

    i32 index = -1;
    fori (i, 0, bh_arr_length(overloads)) {
        if (overloads[i].order > order) {
            index = i;
            break;
        }
    }

    if (index < 0) {
        bh_arr_push(overloads, ((OverloadOption) {
            .order  = order,
            .option = overload,
        }));

    } else {
        bh_arr_insertn(overloads, index, 1);
        overloads[index].order  = order;
        overloads[index].option = overload;
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

AstTyped* find_matching_overload_by_arguments(Context *context, bh_arr(OverloadOption) overloads, Arguments* param_args) {
    Arguments args;
    arguments_clone(context, &args, param_args);
    arguments_ensure_length(context, &args, bh_arr_length(args.values) + bh_arr_length(args.named_values));

    // CLEANUP SPEED: This currently rebuilds the complete set of overloads every time one is looked up.
    // This should be cached in the AstOverloadedFunction or somewhere like that.
    bh_imap all_overloads;
    bh_imap_init(&all_overloads, context->gp_alloc, bh_arr_length(overloads) * 2);
    build_all_overload_options(overloads, &all_overloads);

    AstTyped *matched_overload = NULL;

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) strip_aliases((AstNode *) entry->key);
        arguments_copy(context, &args, param_args);

        AstFunction* overload = NULL;
        switch (node->kind) {
            case Ast_Kind_Macro:            overload = macro_resolve_header(context, (AstMacro *) node, param_args, NULL, 0); break;
            case Ast_Kind_Polymorphic_Proc: overload = polymorphic_proc_build_only_header(context, (AstFunction *) node, PPLM_By_Arguments, param_args); break;
            case Ast_Kind_Function:
                overload = (AstFunction *) node;
                arguments_clear_baked_flags(&args);
                break;
            default: break;
        }

        // NOTE: Overload is not something that is known to be overloadable.
        if (overload == NULL) continue;
        if (overload->kind != Ast_Kind_Function) continue;
        if (overload == (AstFunction *) &context->node_that_signals_a_yield || overload->type == NULL) {
            // If it was not possible to create the type for this procedure, tell the
            // caller that this should yield and try again later.

            // return and not continue because if the overload that didn't have a type will
            // work in the future, then it has to take precedence over the other options available.
            bh_imap_free(&all_overloads);
            bh_arr_free(args.values);
            return (AstTyped *) &context->node_that_signals_a_yield;
        }
        assert(overload->type->kind == Type_Kind_Function);

        arguments_remove_baked(&args);
        arguments_ensure_length(context, &args, get_argument_buffer_size(context, &overload->type->Function, &args));

        // NOTE: If the arguments cannot be placed successfully in the parameters list
        if (!fill_in_arguments(context, &args, (AstNode *) overload, NULL, 0)) continue;
        
        VarArgKind va_kind;
        TypeMatch tm = check_arguments_against_type(context, &args, &overload->type->Function, &va_kind, NULL, NULL, NULL);
        if (tm == TYPE_MATCH_SUCCESS) {
            matched_overload = node;
            break;
        }

        if (tm == TYPE_MATCH_YIELD) {
            bh_imap_free(&all_overloads);
            bh_arr_free(args.values);
            return (AstTyped *) &context->node_that_signals_a_yield;
        }
    }

    bh_imap_free(&all_overloads);
    bh_arr_free(args.values);
    return matched_overload;
}

AstTyped* find_matching_overload_by_type(Context *context, bh_arr(OverloadOption) overloads, Type* type) {
    if (type->kind != Type_Kind_Function) return NULL;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, context->gp_alloc, bh_arr_length(overloads) * 2);
    build_all_overload_options(overloads, &all_overloads);

    AstTyped *matched_overload = NULL;

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) entry->key;
        if (node->kind == Ast_Kind_Overloaded_Function) continue;

        TypeMatch tm = unify_node_and_type(context, &node, type);
        if (tm == TYPE_MATCH_SUCCESS) {
            matched_overload = node;
            break;
        }

        if (tm == TYPE_MATCH_YIELD) {
            return (AstTyped *) &context->node_that_signals_a_yield;
        }
    }
    
    bh_imap_free(&all_overloads);
    return matched_overload;
}

void report_unable_to_match_overload(Context *context, AstCall* call, bh_arr(OverloadOption) overloads) {
    char* arg_str = bh_alloc(context->scratch_alloc, 1024);
    arg_str[0] = '\0';

    bh_arr_each(AstTyped *, arg, call->args.values) {
        strncat(arg_str, node_get_type_name(context, *arg), 1023);

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
            strncat(arg_str, node_get_type_name(context, (*named_value)->value), 1023); // CHECK: this might say 'unknown'.

            if (named_value != &bh_arr_last(call->args.named_values))
                strncat(arg_str, ", ", 1023);
        }
    }

    ONYX_ERROR(call->token->pos, Error_Critical, "Unable to match overloaded function with provided argument types: (%s)", arg_str);

    bh_free(context->scratch_alloc, arg_str);

    // CLEANUP SPEED: This currently rebuilds the complete set of overloads every time one is looked up.
    // This should be cached in the AstOverloadedFunction or somewhere like that.
    bh_imap all_overloads;
    bh_imap_init(&all_overloads, context->gp_alloc, bh_arr_length(overloads) * 2);
    build_all_overload_options(overloads, &all_overloads);

    i32 i = 1;
    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) strip_aliases((AstNode *) entry->key);
        ONYX_ERROR(node->token->pos, Error_Critical, "Here is one of the overloads. %d/%d", i++, bh_arr_length(all_overloads.entries));
    }

    bh_imap_free(&all_overloads);
}

void report_incorrect_overload_expected_type(Context *context, Type *given, Type *expected, OnyxToken *overload, OnyxToken *group) {
    ONYX_ERROR(overload->pos, Error_Critical,
            "Expected this overload option to return '%s', but instead it returns '%s'.",
            type_get_name(context, expected), type_get_name(context, given));

    ONYX_ERROR(group->pos, Error_Critical, "Here is where the overloaded function was defined.");
}

static TypeMatch ensure_overload_returns_correct_type_job(Context *context, void *raw_data) {
    OverloadReturnTypeCheck *data = raw_data;
    Type *expected_type = data->expected_type;
    AstTyped *node      = data->node;

    assert(expected_type && node);
    
    // If the entity on the node has been completed and unused,
    // skip checking this because the function is likely not used.
    if (node->entity && node->entity->state >= Entity_State_Finalized) {
        return TYPE_MATCH_SUCCESS;
    }

    // HACK: This case should go away, but there were issues with some overloads
    // not ever completing there auto return type resolution, likely because they
    // were not actually used. This creates a problem here because this code
    // will still wait for them. As a cheap solution, if there is a cycle detected,
    // return success, even if the types may not match.
    if (context->cycle_almost_detected > 0) {
        return TYPE_MATCH_SUCCESS;
    }

    AstFunction *func = (AstFunction *) node;
    if (func->kind == Ast_Kind_Macro) {
        func = (AstFunction *) ((AstMacro *) func)->body;
    }

    if (!func->type) return TYPE_MATCH_YIELD;
    if (!func->type->Function.return_type) return TYPE_MATCH_YIELD;

    Type *return_type = func->type->Function.return_type;
    if (return_type == context->types.auto_return) return TYPE_MATCH_YIELD;

    // See the note about using Polymorphic Structures as expected return types,
    // in check_overloaded_function().
    if (expected_type->kind == Type_Kind_PolyStruct) {
        if (   return_type->kind == Type_Kind_Struct
            && return_type->Struct.constructed_from
            && return_type->Struct.constructed_from->type_id == expected_type->id) {
            return TYPE_MATCH_SUCCESS;
        }

        report_incorrect_overload_expected_type(context, return_type, expected_type, func->token, data->group);
        return TYPE_MATCH_FAILED;
    }

    if (!types_are_compatible(context, return_type, expected_type)) {
        report_incorrect_overload_expected_type(context, return_type, expected_type, func->token, data->group);
        return TYPE_MATCH_FAILED;
    }

    return TYPE_MATCH_SUCCESS;
}

void ensure_overload_returns_correct_type(Context *context, AstTyped *overload, AstOverloadedFunction *group) {
    // This might not be entirely right as the type might not have been constructed yet, I think?
    //
    // Also, as a HACK, this does not check for the correct return type when errors are disabled.
    // Errors are only disabled when doing something non-permantent, like checking an interface
    // constraint, so this is a cheap way to tell if that is where we are coming from.
    //
    if (group->expected_return_type && onyx_errors_are_enabled(context)) {
        OverloadReturnTypeCheck *data = bh_alloc_item(context->ast_alloc, OverloadReturnTypeCheck);
        data->expected_type = group->expected_return_type;
        data->node = overload;
        data->group = group->token;

        entity_heap_add_job(&context->entities, ensure_overload_returns_correct_type_job, data);
    }
}



//
// Macros
//
//
// TODO: Write this documentation
void expand_macro(Context *context, AstCall** pcall, AstFunction* template) {
    AstCall* call = *pcall;
    AstMacro* macro = (AstMacro *) call->callee;
    assert(macro->kind == Ast_Kind_Macro);

    assert(template->kind == Ast_Kind_Function);
    assert(template->type != NULL);
    assert(template->type->kind == Type_Kind_Function);

    bh_arr(AstNode *) nodes_that_need_entities=NULL;
    bh_arr_new(context->gp_alloc, nodes_that_need_entities, 4);

    AstBlock* expansion = (AstBlock *) ast_clone_with_captured_entities(context, template->body, &nodes_that_need_entities);
    expansion->rules = Block_Rule_Macro;
    expansion->scope = NULL;
    expansion->next = call->next;
    expansion->macro_generated_from = call->token;

    AstNode* subst = (AstNode *) expansion;

    if (template->type->Function.return_type != context->types.basic[Basic_Kind_Void]) {
        expansion->rules = Block_Rule_Do_Block;

        AstDoBlock* doblock = (AstDoBlock *) onyx_ast_node_new(context->ast_alloc, sizeof(AstDoBlock), Ast_Kind_Do_Block);
        doblock->token = expansion->token;
        doblock->block = expansion;
        doblock->type = template->type->Function.return_type;
        doblock->next = expansion->next;
        doblock->named_return_locals = NULL;
        expansion->next = NULL;

        if (template->named_return_locals) {
            bh_arr_new(context->ast_alloc, doblock->named_return_locals, bh_arr_length(template->named_return_locals));

            bh_arr_each(AstLocal *, named_return, template->named_return_locals) {
                AstLocal *cloned = (AstLocal *) ast_clone(context, *named_return);
                bh_arr_push(doblock->named_return_locals, cloned);

                cloned->next = doblock->block->body;
                doblock->block->body = (AstNode *) cloned;
            }
        }

        subst = (AstNode *) doblock;
    }

    Scope* argument_scope = scope_create(context, NULL, call->token->pos);
    if (expansion->binding_scope != NULL)
        scope_include(context, argument_scope, expansion->binding_scope, call->token->pos);
    expansion->binding_scope = argument_scope;

    // HACK HACK HACK This is probably very wrong. I don't know what guarentees that
    // the paramters and arguments are going to be in the same order exactly.
    Type *any_type = type_build_from_ast(context, context->builtins.any_type);
    fori (i, 0, bh_arr_length(call->args.values)) {
        AstNode *value = (AstNode *) ((AstArgument *) call->args.values[i])->value;
        assert(template->params[i].local->type);

        Type *param_type = template->params[i].local->type;
        if (param_type == any_type
            || (param_type->kind == Type_Kind_VarArgs && param_type->VarArgs.elem == any_type)) {
            ONYX_ERROR(macro->token->pos, Error_Critical, "Currently, macros do not support arguments of type 'any' or '..any'.");
        }

        symbol_introduce(context, argument_scope, template->params[i].local->token, value);
    }

    if (template->poly_scope != NULL)
        scope_include(context, argument_scope, template->poly_scope, call->token->pos);

    if (bh_arr_length(nodes_that_need_entities) > 0) {
        // :CopyPaste from check_function
        bh_arr_each(AstNode *, node, nodes_that_need_entities) {
            // This makes a lot of assumptions about how these nodes are being processed,
            // and I don't want to start using this with other nodes without considering
            // what the ramifications of that is.
            assert((*node)->kind == Ast_Kind_Static_If || (*node)->kind == Ast_Kind_File_Contents);

            Scope *scope = argument_scope;

            if ((*node)->kind == Ast_Kind_Static_If) {
                AstIf *static_if = (AstIf *) *node;
                assert(static_if->defined_in_scope);
                scope = static_if->defined_in_scope;

                if (template->poly_scope) {
                    scope = scope_create(context, scope, static_if->token->pos);
                    scope_include(context, scope, template->poly_scope, static_if->token->pos);
                }
            }

            add_entities_for_node(&context->entities, NULL, *node, scope, macro->entity->package);
        }
    }

    *(AstNode **) pcall = subst;

    bh_arr_free(nodes_that_need_entities);
    return;
}

AstFunction* macro_resolve_header(Context *context, AstMacro* macro, Arguments* args, OnyxToken* callsite, b32 error_if_failed) {
    switch (macro->body->kind) {
        case Ast_Kind_Function: return (AstFunction *) macro->body;

        case Ast_Kind_Polymorphic_Proc: {
            AstFunction* pp = (AstFunction *) macro->body;
            ensure_polyproc_cache_is_created(context, pp);

            bh_arr(AstPolySolution) slns = find_polymorphic_slns(context, pp, PPLM_By_Arguments, args, callsite, error_if_failed);

            if (slns == NULL) {
                if (context->polymorph.flag_to_yield) {
                    context->polymorph.flag_to_yield = 0;
                    return (AstFunction *) &context->node_that_signals_a_yield;
                }

                return NULL;
            }

            return polymorphic_proc_build_only_header_with_slns(context, pp, slns, error_if_failed);
        }

        default: assert("Bad macro body type." && 0);
    }

    return NULL;
}


//
// Arguments resolving
//
static i32 lookup_idx_by_name(Context *context, AstNode* provider, char* name) {
    switch (provider->kind) {
        case Ast_Kind_Struct_Literal: {
            AstStructLiteral* sl = (AstStructLiteral *) provider;
            assert(sl->type);

            StructMember s;
            if (!type_lookup_member(context, sl->type, name, &s)) return -1;
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

static AstNode* lookup_default_value_by_idx(Context *context, AstNode* provider, i32 idx) {
    switch (provider->kind) {
        case Ast_Kind_Struct_Literal: {
            AstStructLiteral* sl = (AstStructLiteral *) provider;
            assert(sl->type);

            if (sl->type->kind == Type_Kind_Struct) {
                bh_arr(StructMember *) memarr = sl->type->Struct.memarr;
                if (idx >= bh_arr_length(memarr)) return NULL;

                if (memarr[idx]->initial_value)
                    return (AstNode *) *memarr[idx]->initial_value;

                return NULL;
            }

            return NULL;
        }

        case Ast_Kind_Function: {
            AstFunction* func = (AstFunction *) provider;

            AstTyped* default_value = func->params[idx].default_value;
            if (default_value == NULL) return NULL;

            AstArgument* arg = make_argument(context, default_value);
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

        default: break;
    }

    // NOTE: This returns int_max for anything other than struct literals because the
    // bounds checking on the arguments will be done elsewhere.
    return 0x7fffffff;
}

static i32 non_baked_argument_count(Arguments* args) {
    if (args->used_argument_count >= 0) return args->used_argument_count;

    i32 count = 0;

    bh_arr_each(AstTyped *, actual, args->values) {
        if ((*actual)->kind != Ast_Kind_Argument) count++;
        else if (!((AstArgument *) (*actual))->is_baked) count++;
    }

    bh_arr_each(AstNamedValue *, named_value, args->named_values) {
        if ((*named_value)->value->kind != Ast_Kind_Argument) count++;
        else if (!((AstArgument *) (*named_value)->value)->is_baked) count++;
    }

    args->used_argument_count = count;
    return count;
}

i32 get_argument_buffer_size(Context *context, TypeFunction* type, Arguments* args) {
    i32 non_vararg_param_count = (i32) type->param_count;
    if (non_vararg_param_count > 0) {
        if (type->params[type->param_count - 1] == context->builtins.vararg_type_type) non_vararg_param_count--;
        if (type->params[type->param_count - 1]->kind == Type_Kind_VarArgs)  non_vararg_param_count--;
    }

    return bh_max(non_vararg_param_count, non_baked_argument_count(args));
}

// NOTE: The values array can be partially filled out, and is the resulting array.
// Returns if all the values were filled in.
b32 fill_in_arguments(Context *context, Arguments* args, AstNode* provider, char** err_msg, b32 insert_zero_values) {

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
            i32 idx = lookup_idx_by_name(context, provider, named_value->token->text);
            if (idx == -1) {
                if (err_msg) *err_msg = bh_aprintf(context->scratch_alloc, "'%s' is not a valid named parameter here.", named_value->token->text);
                token_toggle_end(named_value->token);
                return 0;
            }

            // assert(idx < bh_arr_length(args->values));
            if (idx >= bh_arr_length(args->values)) {
                if (err_msg) *err_msg = bh_aprintf(context->scratch_alloc, "Error placing value with name '%s' at index '%d'.", named_value->token->text, idx);
                token_toggle_end(named_value->token);
                return 0;
            }

            if (args->values[idx] != NULL && args->values[idx] != named_value->value) {
                if (err_msg) *err_msg = bh_aprintf(context->scratch_alloc, "Multiple values given for parameter named '%s'.", named_value->token->text);
                token_toggle_end(named_value->token);
                return 0;
            }

            args->values[idx] = named_value->value;
            token_toggle_end(named_value->token);
        }
    }

    b32 success = 1;
    fori (idx, 0, bh_arr_length(args->values)) {
        if (args->values[idx] == NULL) {
            args->values[idx] = (AstTyped *) lookup_default_value_by_idx(context, provider, idx);
        }
        if (args->values[idx] == NULL) {
            if (insert_zero_values) {
                assert(provider->token);
                args->values[idx] = (AstTyped *) make_zero_value(context, provider->token, NULL);
            } else {
                if (err_msg) *err_msg = bh_aprintf(context->scratch_alloc, "No value given for %d%s argument.", idx + 1, bh_num_suffix(idx + 1));
                success = 0;
                break;
            }
        }
    }

    i32 maximum_arguments = maximum_argument_count(provider);
    if (bh_arr_length(args->values) > maximum_arguments) {
        if (err_msg) *err_msg = bh_aprintf(context->scratch_alloc, "Too many values provided. Expected at most %d.", maximum_arguments);
        success = 0;
    }

    return success;
}


//
// Argument checking
//

typedef enum ArgState {
    AS_Expecting_Exact,
    AS_Expecting_Typed_VA,
    AS_Expecting_Untyped_VA,
} ArgState;

TypeMatch check_arguments_against_type(Context *context, Arguments* args, TypeFunction* func_type, VarArgKind* va_kind,
                                       OnyxToken* location, char* func_name, OnyxError* error) {
    // In this function, if error is not NULL, then it is assumed that permanent changes can
    // be made. Otherwise, permanent changes should be avoided; only detecting issues should be done.

    b32 permanent = location != NULL;
    if (func_name == NULL) func_name = "UNKNOWN FUNCTION";

    if (error) error->rank = Error_Critical;

    bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) args->values;
    i32 arg_count = get_argument_buffer_size(context, func_type, args);

    Type **formal_params = func_type->params;
    Type* variadic_type = NULL;
    i64 any_type_id = type_build_from_ast(context, context->builtins.any_type)->id;

    ArgState arg_state = AS_Expecting_Exact;
    u32 arg_pos = 0;
    while (1) {
        switch (arg_state) {
            case AS_Expecting_Exact: {
                if (arg_pos >= func_type->param_count) goto type_checking_done;

                if (formal_params[arg_pos]->kind == Type_Kind_VarArgs) {
                    variadic_type = formal_params[arg_pos]->VarArgs.elem;
                    arg_state = AS_Expecting_Typed_VA;
                    continue;
                }

                if ((i16) arg_pos == func_type->vararg_arg_pos) {
                    arg_state = AS_Expecting_Untyped_VA;
                    continue;
                }

                if (arg_pos >= (u32) bh_arr_length(arg_arr)) goto type_checking_done;

                assert(arg_arr[arg_pos]->kind == Ast_Kind_Argument);

                TypeMatch tm = unify_node_and_type_(context, &arg_arr[arg_pos]->value, formal_params[arg_pos], permanent);
                if (tm == TYPE_MATCH_YIELD) return tm;
                if (tm == TYPE_MATCH_SPECIAL) return tm;
                if (tm == TYPE_MATCH_FAILED) {
                    // Handle the weird case of `x: any` as an argument.
                    if (formal_params[arg_pos]->id == any_type_id) {
                        resolve_expression_type(context, arg_arr[arg_pos]->value);
                        if (error != NULL) {
                            arg_arr[arg_pos]->pass_as_any = 1;
                        }

                        arg_arr[arg_pos]->va_kind = VA_Kind_Not_VA;
                        break;
                    }

                    if (error != NULL) {
                        AstArgument *the_arg = (void *) arg_arr[arg_pos];
                        if (the_arg->used_as_lval_of_method_call) {
                            if (formal_params[arg_pos]->kind == Type_Kind_Pointer &&
                                formal_params[arg_pos]->Pointer.elem == arg_arr[arg_pos]->type) {
                                // We didn't match the type, and this arg was from a method call,
                                // and its because it wanted a &T, but got a T. This is likely
                                // due to the fact that the method call argument is not an lval.
                                error->pos = arg_arr[arg_pos]->token->pos;
                                error->text = bh_aprintf(context->gp_alloc,
                                        "This method expects a pointer to the first argument, which normally `->` would do automatically, but in this case, the left-hand side is not an l-value, so its address cannot be taken. Try storing it in a temporary variable first, then calling the method."
                                );
                                return tm;
                            }
                        }

                        if (arg_arr[arg_pos]->token) error->pos = arg_arr[arg_pos]->token->pos;

                        error->text = bh_aprintf(context->gp_alloc,
                                "The procedure '%s' expects a value of type '%s' for %d%s parameter, got '%s'.",
                                func_name,
                                type_get_name(context, formal_params[arg_pos]),
                                arg_pos + 1,
                                bh_num_suffix(arg_pos + 1),
                                node_get_type_name(context, arg_arr[arg_pos]->value));
                    }
                    return tm;
                }

                arg_arr[arg_pos]->va_kind = VA_Kind_Not_VA;
                break;
            }

            case AS_Expecting_Typed_VA: {
                if (variadic_type->id == any_type_id) *va_kind = VA_Kind_Any;

                if (arg_pos >= (u32) bh_arr_length(arg_arr)) goto type_checking_done;

                if (variadic_type->id == any_type_id) {
                    resolve_expression_type(context, arg_arr[arg_pos]->value);
                    if (arg_arr[arg_pos]->value->type == NULL) {
                        if (error != NULL) {
                            error->pos = arg_arr[arg_pos]->token->pos;
                            error->text = "Unable to resolve type of argument.";
                        }
                        return TYPE_MATCH_FAILED;
                    }

                    arg_arr[arg_pos]->va_kind = VA_Kind_Any; 
                    break;
                }

                *va_kind = VA_Kind_Typed;

                assert(arg_arr[arg_pos]->kind == Ast_Kind_Argument);
                TypeMatch tm = unify_node_and_type_(context, &arg_arr[arg_pos]->value, variadic_type, permanent);
                if (tm == TYPE_MATCH_YIELD) return tm;
                if (tm == TYPE_MATCH_FAILED) {
                    if (error != NULL) {
                        error->pos = arg_arr[arg_pos]->token->pos,
                        error->text = bh_aprintf(context->gp_alloc,
                            "The procedure '%s' expects a value of type '%s' for the variadic parameter, got '%s'.",
                            func_name,
                            type_get_name(context, variadic_type),
                            node_get_type_name(context, arg_arr[arg_pos]->value));
                    }
                    return tm;
                }

                arg_arr[arg_pos]->va_kind = VA_Kind_Typed;
                break;
            }

            case AS_Expecting_Untyped_VA: {
                *va_kind = VA_Kind_Untyped;

                if (arg_pos >= (u32) bh_arr_length(arg_arr)) goto type_checking_done;

                assert(arg_arr[arg_pos]->kind == Ast_Kind_Argument);
                resolve_expression_type(context, arg_arr[arg_pos]->value);
                if (arg_arr[arg_pos]->value->type == NULL) {
                    if (error != NULL) {
                        error->pos = arg_arr[arg_pos]->token->pos;
                        error->text = "Unable to resolve type for argument.";
                    }
                    return TYPE_MATCH_FAILED;
                }

                arg_arr[arg_pos]->va_kind = VA_Kind_Untyped;
                break;
            }
        }

        arg_pos++;
    }

type_checking_done:
    if (arg_pos < func_type->needed_param_count) {
        if (error != NULL) {
            if (location) error->pos = location->pos;
            error->text = bh_aprintf(context->gp_alloc,
                    "Too few arguments to function call. Expected at least %d argument%s, but only got %d.",
                    func_type->needed_param_count, bh_num_plural(func_type->needed_param_count), arg_pos);
        }
        return TYPE_MATCH_FAILED;
    }

    if (arg_pos < (u32) arg_count) {
        if (error != NULL) {
            if (location) error->pos = location->pos;
            error->text = bh_aprintf(context->gp_alloc,
                    "Too many arguments to function call. Expected at most %d argument%s, but got %d.",
                    arg_pos, bh_num_plural(arg_pos), arg_count);
        }
        return TYPE_MATCH_FAILED;
    }

    return TYPE_MATCH_SUCCESS;
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

static i32 encode_utf8_char(char ** d, u32 r) {
    char *dest = *d;
    int len = 0;

    if (r <= 0x7F) {
        *dest++ = r;
        len = 1;
    }

    else if (r <= 0x7FF) {
        *dest++ = (0xC0 | ((r >> 6) & 0x1F));
        *dest++ = (0x80 | (r & 0x3F));
        len = 2;
    }

    else if (r >= 0xD800 && r <= 0xDFFF) {
    }

    else if (r <= 0xFFFF) {
        *dest++ = (0xE0 | ((r >> 12) & 0x0F));
        *dest++ = (0x80 | ((r >> 6) & 0x3F));
        *dest++ = (0x80 | (r & 0x3F));
        len = 3;
    }

    else if (r <= 0x10FFFF) {
        *dest++ = (0xF0 | ((r >> 18) & 0x07));
        *dest++ = (0x80 | ((r >> 12) & 0x3F));
        *dest++ = (0x80 | ((r >> 6) & 0x3F));
        *dest++ = (0x80 | (r & 0x3F));
        len = 4;
    }

    *d = dest;
    return len;
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
            case '\'': *dest++ = '\''; total_len++; break;
            case '\\': *dest++ = '\\'; total_len++; break;
            case 'x': {
                u8 ch1 = src[i + 1];
                u8 ch2 = src[i + 2];
                *dest++ = (i8) (char_to_base16_value(ch1) << 4 | char_to_base16_value(ch2));
                total_len++;
                i += 2;
                break;
            }
            case 'u': {
                if (len - i < 5) break;
                u32 c =
                      (char_to_base16_value(src[i + 1]) << 12)
                    | (char_to_base16_value(src[i + 2]) << 8)
                    | (char_to_base16_value(src[i + 3]) << 4)
                    | (char_to_base16_value(src[i + 4]));
                total_len += encode_utf8_char(&dest, c);
                i += 5;
                break;
            }
            case 'U': {
                if (len - i < 7) break;
                u32 c =
                      (char_to_base16_value(src[i + 1]) << 20)
                    | (char_to_base16_value(src[i + 2]) << 16)
                    | (char_to_base16_value(src[i + 3]) << 12)
                    | (char_to_base16_value(src[i + 4]) << 8)
                    | (char_to_base16_value(src[i + 5]) << 4)
                    | (char_to_base16_value(src[i + 6]));
                total_len += encode_utf8_char(&dest, c);
                i += 7;
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


static Scope **get_scope_from_node_helper(Context *context, AstNode *node) {
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
            if (package->package == NULL) return NULL;

            return &package->package->scope;
        } 

        case Ast_Kind_Foreign_Block: {
            AstForeignBlock* fb = (AstForeignBlock *) node;
            return &fb->scope;
        }

        case Ast_Kind_Basic_Type: {
            AstBasicType* btype = (AstBasicType *) node;
            return &btype->scope;
        }

        case Ast_Kind_Enum_Type: {
            AstEnumType* etype = (AstEnumType *) node;
            return &etype->scope;
        }

        case Ast_Kind_Slice_Type: {
            Type *t = type_build_from_ast(context, (AstType *) node);
            if (t) return &t->Slice.scope;
            return NULL;
        }
        
        case Ast_Kind_DynArr_Type: {
            Type *t = type_build_from_ast(context, (AstType *) node);
            if (t) return &t->DynArray.scope;
            return NULL;
        }

        case Ast_Kind_Struct_Type: {
            AstStructType* stype = (AstStructType *) node;
            return &stype->scope;
        }

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* pstype = (AstPolyStructType *) node;
            return &pstype->scope;
        }

        case Ast_Kind_Union_Type: {
            AstUnionType* utype = (AstUnionType *) node;
            return &utype->scope;
        }

        case Ast_Kind_Poly_Union_Type: {
            AstPolyUnionType* putype = (AstPolyUnionType *) node;
            return &putype->scope;
        }

        case Ast_Kind_Poly_Call_Type: {
            Type *t = type_build_from_ast(context, (AstType *) node);
            if (t) {
                return &t->Struct.scope;
            }
            return NULL;
        }

        case Ast_Kind_Distinct_Type: {
            AstDistinctType* dtype = (AstDistinctType *) node;
            return &dtype->scope;
        }

        case Ast_Kind_Interface: {
            AstInterface* inter = (AstInterface *) node;
            return &inter->scope;
        }

        default: break;
    }

    return NULL;
}

Scope *get_scope_from_node(Context *context, AstNode *node) {
    if (!node) return NULL;

    Scope **pscope = get_scope_from_node_helper(context, node);
    if (!pscope) return NULL;
    return *pscope;
}

Scope *get_scope_from_node_or_create(Context *context, AstNode *node) {
    if (!node) return NULL;

    Scope **pscope = get_scope_from_node_helper(context, node);
    if (!pscope) return NULL;

    // Create the scope if it does not exist.
    // This uses a NULL parent, which I think is what 
    // is used in other parts of the compiler for struct/enum
    // scopes?
    if (!*pscope) {
        OnyxFilePos pos = {0};
        if (node->token) pos = node->token->pos;

        *pscope = scope_create(context, NULL, pos);
    }

    return *pscope;
}

u32 levenshtein_distance(Context *context, const char *str1, const char *str2) {
    i32 m = strlen(str1) + 1;
    i32 n = strlen(str2) + 1;

    i32 *d = bh_alloc_array(context->scratch_alloc, i32, m * n);
    fori (i, 0, m * n) d[i] = 0;

    fori (i, 0, m) d[i * n + 0] = i;
    fori (j, 0, n) d[0 * n + j] = j;

    fori (j, 1, n) {
        fori (i, 1, m) {
            i32 subst_cost = str1[i - 1] == str2[j - 1] ? 0 : 1;

            i32 a = d[(i - 1) * n + j] + 1;
            i32 b = d[i * n + (j - 1)] + 1;
            i32 c = d[(i - 1) * n + (j - 1)] + subst_cost;

            d[i * n + j] = bh_min(bh_min(a, b), c);
        }
    }

    return d[m * n - 1];
}

char *find_closest_symbol_in_scope(Context *context, Scope *scope, char *sym, u32 *out_distance) {
    *out_distance = 0x7fffffff;

    if (scope == NULL) return NULL;

    char* closest = NULL;
    fori (i, 0, shlen(scope->symbols)) {
        if (scope->symbols[i].value && scope->symbols[i].value->flags & Ast_Flag_Symbol_Invisible) continue;

        char *key = scope->symbols[i].key;
        u32 d = levenshtein_distance(context, key, sym); 
        if (d < *out_distance) {
            *out_distance = d;
            closest = (char *) key;
        }
    }

    return closest;
}

char *find_closest_symbol_in_scope_and_parents(Context *context, Scope *scope, char *sym) {
    u32 min_dist = 0x7fffffff;
    u32 tmp_dist; 

    char *closest = NULL;
    while (scope != NULL) {
        char *tmp_closest = find_closest_symbol_in_scope(context, scope, sym, &tmp_dist);
        if (tmp_dist < min_dist) {
            min_dist = tmp_dist;
            closest = tmp_closest;
        }

        scope = scope->parent;
    }

    return closest;
}
        
char *find_closest_symbol_in_node(Context *context, AstNode* node, char *sym) {
    Scope *scope = get_scope_from_node(context, node);
    if (!scope) {
        if (node && node->kind == Ast_Kind_Poly_Call_Type) {
            AstPolyCallType* pcall = (AstPolyCallType *) node;
            return find_closest_symbol_in_node(context, (AstNode *) pcall->callee, sym);
        }

        return NULL;
    }

    u32 dist;
    return find_closest_symbol_in_scope(context, scope, sym, &dist);
}



static u32 symbol_info_get_file_id(SymbolInfoTable *syminfo, const char *filename) {
    u32 file_id;
    if (shgeti(syminfo->files, filename) == -1) {
        file_id = syminfo->next_file_id++;
        shput(syminfo->files, filename, file_id);

    } else {
        file_id = shget(syminfo->files, filename);
    }

    return file_id;
}

void track_declaration_for_symbol_info(Context *context, OnyxFilePos pos, AstNode *node) {
    if (!context->options->generate_symbol_info_file) return;
    if (pos.filename == NULL) return;

    SymbolInfoTable *syminfo = context->symbol_info;
    assert(syminfo);

    if (bh_imap_has(&syminfo->node_to_id, (u64) node)) return;

    u32 symbol_id = syminfo->next_symbol_id++;
    u32 file_id = symbol_info_get_file_id(syminfo, pos.filename);

    SymbolInfo symbol;
    symbol.id = symbol_id;
    symbol.file_id = file_id;
    symbol.line = pos.line;
    symbol.column = pos.column;
    symbol.documentation = NULL;
    symbol.documentation_length = 0;
    bh_arr_push(syminfo->symbols, symbol);

    bh_imap_put(&syminfo->node_to_id, (u64) node, (u64) symbol_id);
}

void track_documentation_for_symbol_info(Context *context, AstNode *node, AstBinding *binding) {
    if (!context->options->generate_lsp_info_file) return;
    if (!context->options->generate_symbol_info_file) return;

    SymbolInfoTable *syminfo = context->symbol_info;
    assert(syminfo);

    if (!bh_imap_has(&syminfo->node_to_id, (u64) node)) return;

    u64 symbol_id = bh_imap_get(&syminfo->node_to_id, (u64) node);
    if (binding->documentation_token_old) {
        syminfo->symbols[symbol_id].documentation        = binding->documentation_token_old->text;
        syminfo->symbols[symbol_id].documentation_length = binding->documentation_token_old->length;
    } else if (binding->documentation_string) {
        syminfo->symbols[symbol_id].documentation = binding->documentation_string;
        syminfo->symbols[symbol_id].documentation_length = strlen(binding->documentation_string);
    } else {
        syminfo->symbols[symbol_id].documentation = "";
        syminfo->symbols[symbol_id].documentation_length = 0;
    }
}

void track_resolution_for_symbol_info(Context *context, AstNode *original, AstNode *resolved) {
    if (!context->options->generate_symbol_info_file) return;
    if (!resolved) return;

    SymbolInfoTable *syminfo = context->symbol_info;
    assert(syminfo);

    if (!bh_imap_has(&syminfo->node_to_id, (u64) resolved)) return;

    u32 symbol_id = (u32) bh_imap_get(&syminfo->node_to_id, (u64) resolved);

    u32 file_id = symbol_info_get_file_id(syminfo, original->token->pos.filename);

    SymbolResolution res;
    res.symbol_id = symbol_id;
    res.file_id = file_id;
    res.line = original->token->pos.line;
    res.column = original->token->pos.column;
    res.length = original->token->length;

    bh_arr_push(syminfo->symbols_resolutions, res);
}



//
// Compiler Events
//

void compiler_events_init(Context *context) {
    bh_arena_init(&context->events.event_arena, context->gp_alloc, 1024 * 1024);
    context->events.event_alloc = bh_arena_allocator(&context->events.event_arena);

    // All other fields should be already set to 0/NULL.
}

void compiler_events_clear(Context *context) {
    bh_arena_clear(&context->events.event_arena);
    context->events.first = NULL;
    context->events.last  = NULL;
    context->events.event_count = 0;
}

CompilerEvent *compiler_event_add(Context *context, u32 event_type) {
    CompilerEvent *new_event = bh_alloc_item(context->events.event_alloc, CompilerEvent);
    new_event->type = event_type;
    new_event->first_field = NULL;

    new_event->next = context->events.last;
    if (context->events.last) context->events.last->next = new_event;
    context->events.last = new_event;
    if (!context->events.first) context->events.first = new_event;

    context->events.event_count++;

    return new_event;
}

void compiler_event_add_field_str(Context *context, CompilerEvent *event, char *field, char *value) {
    if (!value) return;

    CompilerEventField *new_field = bh_alloc_item(context->events.event_alloc, CompilerEventField);
    new_field->type = 0; // 0 for string
    new_field->field = bh_strdup(context->events.event_alloc, field);
    new_field->s = bh_strdup(context->events.event_alloc, value);

    new_field->next = event->first_field;
    event->first_field = new_field;
}

void compiler_event_add_field_int(Context *context, CompilerEvent *event, char *field, i32 value) {
    CompilerEventField *new_field = bh_alloc_item(context->events.event_alloc, CompilerEventField);
    new_field->type = 1; // 1 for int
    new_field->field = bh_strdup(context->events.event_alloc, field);
    new_field->i = value;

    new_field->next = event->first_field;
    event->first_field = new_field;
}

