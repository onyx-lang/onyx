#define BH_DEBUG

#include "utils.h"
#include "lex.h"
#include "astnodes.h"
#include "errors.h"
#include "parser.h"
#include "astnodes.h"
#include "errors.h"

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
    scope->name = NULL;

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
    Scope* scope = start_scope;

    while (scope != NULL) {
        if (bh_table_has(AstNode *, scope->symbols, sym)) {
            AstNode* res = bh_table_get(AstNode *, scope->symbols, sym);

            if ((res->flags & Ast_Flag_Symbol_Invisible) == 0) {
                return res;
            }
        }

        scope = scope->parent;
    }

    return NULL;
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

        case Ast_Kind_Poly_Call_Type: {
            AstNode* callee = (AstNode *) ((AstPolyCallType *) node)->callee;
            return try_symbol_raw_resolve_from_node(callee, symbol);
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

// Polymorphic procedures are in their own file to clean up this file.
#include "polymorph.c"

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

AstTyped* find_matching_overload_by_arguments(bh_arr(OverloadOption) overloads, Arguments* param_args, b32* should_yield) {
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
            case Ast_Kind_Macro:            overload = macro_resolve_header((AstMacro *) node, param_args, NULL); break;
            case Ast_Kind_Polymorphic_Proc: overload = polymorphic_proc_build_only_header((AstPolyProc *) node, PPLM_By_Arguments, param_args); break;
        }

        // NOTE: Overload is not something that is known to be overloadable.
        if (overload == NULL) continue;
        if (overload == (AstFunction *) &node_that_signals_a_yield) {
            // If it was not possible to create the type for this procedure, tell the
            // caller that this should yield and try again later.
            if (should_yield) *should_yield = 1;

            return NULL;
        }
        if (overload->kind != Ast_Kind_Function) continue;
        if (overload->type == NULL) {
            // If it was not possible to create the type for this procedure, tell the
            // caller that this should yield and try again later.
            if (should_yield) *should_yield = 1;

            // return and not continue because if the overload that didn't have a type will
            // work in the future, then it has to take precedence over the other options available.
            return NULL;
        }
        assert(overload->type->kind == Type_Kind_Function);

        arguments_remove_baked(&args);
        arguments_ensure_length(&args, get_argument_buffer_size(&overload->type->Function, &args));

        // NOTE: If the arguments cannot be placed successfully in the parameters list
        if (!fill_in_arguments(&args, (AstNode *) overload, NULL)) continue;
        
        VarArgKind va_kind;
        TypeMatch tm = check_arguments_against_type(&args, &overload->type->Function, &va_kind, NULL, NULL, NULL);
        if (tm == TYPE_MATCH_SUCCESS) {
            matched_overload = node;
            break;
        }

        if (tm == TYPE_MATCH_YIELD) {
            if (should_yield) *should_yield = 1;
            return NULL;
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

        TypeMatch tm = unify_node_and_type(&node, type);
        if (tm == TYPE_MATCH_SUCCESS) {
            matched_overload = node;
            break;
        }

        if (tm == TYPE_MATCH_YIELD) {
            return (AstTyped *) &node_that_signals_a_yield;
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
// Macros
//
//
// TODO: Write this documentation
void expand_macro(AstCall** pcall, AstFunction* template) {
    AstCall* call = *pcall;
    AstMacro* macro = (AstMacro *) call->callee;
    assert(macro->kind == Ast_Kind_Macro);

    assert(template->kind == Ast_Kind_Function);
    assert(template->type != NULL);
    assert(template->type->kind == Type_Kind_Function);

    AstBlock* expansion = (AstBlock *) ast_clone(context.ast_alloc, template->body);
    expansion->rules = Block_Rule_Macro;
    expansion->scope = NULL;
    expansion->next = call->next;

    AstNode* subst = (AstNode *) expansion;

    if (template->type->Function.return_type != &basic_types[Basic_Kind_Void]) {
        AstDoBlock* doblock = (AstDoBlock *) onyx_ast_node_new(context.ast_alloc, sizeof(AstDoBlock), Ast_Kind_Do_Block);
        doblock->token = expansion->token;
        doblock->block = expansion;
        doblock->type = template->type->Function.return_type;
        doblock->next = expansion->next;
        expansion->next = NULL;

        subst = (AstNode *) doblock;
    }

    Scope* argument_scope = scope_create(context.ast_alloc, NULL, call->token->pos);
    if (expansion->binding_scope != NULL)
        scope_include(argument_scope, expansion->binding_scope, call->token->pos);
    expansion->binding_scope = argument_scope;

    // HACK HACK HACK This is probably very wrong. I don't know what guarentees that
    // the paramters and arguments are going to be in the same order exactly.
    fori (i, 0, bh_arr_length(call->args.values)) {
        symbol_introduce(argument_scope,
            template->params[i].local->token,
            (AstNode *) ((AstArgument *) call->args.values[i])->value);
    }

    if (template->poly_scope != NULL)
        scope_include(argument_scope, template->poly_scope, call->token->pos);

    *(AstNode **) pcall = subst;
    return;
}

AstFunction* macro_resolve_header(AstMacro* macro, Arguments* args, OnyxToken* callsite) {
    switch (macro->body->kind) {
        case Ast_Kind_Function: return (AstFunction *) macro->body;

        case Ast_Kind_Polymorphic_Proc: {
            AstPolyProc* pp = (AstPolyProc *) macro->body;
            ensure_polyproc_cache_is_created(pp);

            char* err_msg=NULL;
            bh_arr(AstPolySolution) slns = find_polymorphic_slns(pp, PPLM_By_Arguments, args, &err_msg);

            if (slns == NULL) {
                if (flag_to_yield) {
                    flag_to_yield = 0;
                    return (AstFunction *) &node_that_signals_a_yield;
                }

                if (callsite) onyx_report_error(callsite->pos, err_msg);

                return NULL;
            }

            // CLEANUP Copy'n'pasted from polymorphic_proc_build_only_header
            return polymorphic_proc_build_only_header_with_slns(pp, slns);
        }

        default: assert(("Bad macro body type.", 0));
    }

    return NULL;
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

i32 get_argument_buffer_size(TypeFunction* type, Arguments* args) {
    i32 non_vararg_param_count = (i32) type->param_count;
    if (non_vararg_param_count > 0) {
        if (type->params[type->param_count - 1] == builtin_vararg_type_type) non_vararg_param_count--;
        if (type->params[type->param_count - 1]->kind == Type_Kind_VarArgs)  non_vararg_param_count--;
    }

    return bh_max(non_vararg_param_count, non_baked_argument_count(args));
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
                if (err_msg) *err_msg = bh_aprintf(global_scratch_allocator, "Error placing value with name '%s' at index '%d'.", named_value->token->text, idx);
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
            if (err_msg) *err_msg = bh_aprintf(global_scratch_allocator, "No value given for %d%s argument.", idx + 1, bh_num_suffix(idx + 1));
            success = 0;
        }
    }

    i32 maximum_arguments = maximum_argument_count(provider);
    if (bh_arr_length(args->values) > maximum_arguments) {
        if (err_msg) *err_msg = bh_aprintf(global_scratch_allocator, "Too many values provided. Expected at most %d.", maximum_arguments);
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

TypeMatch check_arguments_against_type(Arguments* args, TypeFunction* func_type, VarArgKind* va_kind,
                                       OnyxToken* location, char* func_name, OnyxError* error) {
    b32 permanent = location != NULL;
    if (func_name == NULL) func_name = "UNKNOWN FUNCTION";

    bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) args->values;
    i32 arg_count = get_argument_buffer_size(func_type, args);

    Type **formal_params = func_type->params;
    Type* variadic_type = NULL;
    i64 any_type_id = type_build_from_ast(context.ast_alloc, builtin_any_type)->id;

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
                TypeMatch tm = unify_node_and_type_(&arg_arr[arg_pos]->value, formal_params[arg_pos], permanent);
                if (tm == TYPE_MATCH_YIELD) return tm;
                if (tm == TYPE_MATCH_FAILED) {
                    if (error != NULL) {
                        error->pos = arg_arr[arg_pos]->token->pos,
                        error->text = bh_aprintf(global_heap_allocator,
                                "The procedure '%s' expects a value of type '%s' for %d%s parameter, got '%s'.",
                                func_name,
                                type_get_name(formal_params[arg_pos]),
                                arg_pos + 1,
                                bh_num_suffix(arg_pos + 1),
                                node_get_type_name(arg_arr[arg_pos]->value));
                    }
                    return tm;
                }

                if (arg_arr[arg_pos]->value->type && arg_arr[arg_pos]->value->type->id != any_type_id && formal_params[arg_pos]->id == any_type_id) {
                    resolve_expression_type(arg_arr[arg_pos]->value);
                    arg_arr[arg_pos]->pass_as_any = 1;
                }

                arg_arr[arg_pos]->va_kind = VA_Kind_Not_VA;
                break;
            }

            case AS_Expecting_Typed_VA: {
                if (variadic_type->id == any_type_id) *va_kind = VA_Kind_Any;

                if (arg_pos >= (u32) bh_arr_length(arg_arr)) goto type_checking_done;

                if (variadic_type->id == any_type_id) {
                    resolve_expression_type(arg_arr[arg_pos]->value);
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
                TypeMatch tm = unify_node_and_type_(&arg_arr[arg_pos]->value, variadic_type, permanent);
                if (tm == TYPE_MATCH_YIELD) return tm;
                if (tm == TYPE_MATCH_FAILED) {
                    if (error != NULL) {
                        error->pos = arg_arr[arg_pos]->token->pos,
                        error->text = bh_aprintf(global_heap_allocator,
                            "The procedure '%s' expects a value of type '%s' for the variadic parameter, got '%s'.",
                            func_name,
                            type_get_name(variadic_type),
                            node_get_type_name(arg_arr[arg_pos]->value));
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
                resolve_expression_type(arg_arr[arg_pos]->value);
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
            error->pos = location->pos;
            error->text = "Too few arguments to function call.";
        }
        return TYPE_MATCH_FAILED;
    }

    if (arg_pos < (u32) arg_count) {
        if (error != NULL) {
            error->pos = location->pos;
            error->text = bh_aprintf(global_heap_allocator, "Too many arguments to function call. %d %d", arg_pos, arg_count);
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

