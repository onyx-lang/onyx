#ifndef BH_INTERNAL_ALLOCATOR
    #define BH_INTERNAL_ALLOCATOR (context->gp_alloc)
#endif

#include "astnodes.h"
#include "types.h"
#include "parser.h"
#include "utils.h"
#include "doc.h"

// All of the `check` functions return a boolean that signals if an issue
// was reached while processing the node. These error booleans propagate
// up the call stack until they reach `check_entity`.

#define CHECK(kind, ...) do { \
    CheckStatus cs = check_ ## kind (context, __VA_ARGS__); \
    if (cs > Check_Errors_Start) return cs; \
    } while (0)

#define CHECK_INVISIBLE(kind, node, ...) do { \
    (node)->flags |= Ast_Flag_Symbol_Invisible; \
    CheckStatus cs = check_ ## kind (context, __VA_ARGS__); \
    (node)->flags &= ~Ast_Flag_Symbol_Invisible; \
    if (cs > Check_Errors_Start) return cs;      \
    } while (0)

#define YIELD(loc, msg) do { \
    if (context->cycle_detected) { \
        ONYX_ERROR(loc, Error_Waiting_On, msg); \
        return Check_Error; \
    } else { \
        return Check_Yield; \
    } \
    } while (0)

#define YIELD_(loc, msg, ...) do { \
    if (context->cycle_detected) { \
        ONYX_ERROR(loc, Error_Waiting_On, msg, __VA_ARGS__); \
        return Check_Error; \
    } else { \
        return Check_Yield; \
    } \
    } while (0)

#define YIELD_ERROR(loc, msg) do { \
    if (context->cycle_detected) { \
        ONYX_ERROR(loc, Error_Critical, msg); \
        return Check_Error; \
    } else { \
        return Check_Yield; \
    } \
    } while (0)

#define YIELD_ERROR_(loc, msg, ...) do { \
    if (context->cycle_detected) { \
        ONYX_ERROR(loc, Error_Critical, msg, __VA_ARGS__); \
        return Check_Error; \
    } else { \
        return Check_Yield; \
    } \
    } while (0)

#define ERROR(loc, msg) do { \
    ONYX_ERROR(loc, Error_Critical, msg); \
    return Check_Error; \
    } while (0)

#define ERROR_(loc, msg, ...) do { \
    ONYX_ERROR(loc, Error_Critical, msg, __VA_ARGS__); \
    return Check_Error; \
    } while (0)

#define TYPE_CHECK_(expr, type, type_name)                                                      \
    TypeMatch type_name;                                                                        \
    type_name = unify_node_and_type(context, expr, type);                                       \
    if (type_name == TYPE_MATCH_YIELD || type_name == TYPE_MATCH_SPECIAL) YIELD((*expr)->token->pos, "Waiting on type checking."); \
    if (type_name == TYPE_MATCH_FAILED)

#define TYPE_QUERY_(expr, type, type_name)                                                      \
    TypeMatch type_name;                                                                        \
    type_name = unify_node_and_type_(context, expr, type, 0);                                   \
    if (type_name == TYPE_MATCH_YIELD || type_name == TYPE_MATCH_SPECIAL) YIELD((*expr)->token->pos, "Waiting on type checking."); \
    if (type_name == TYPE_MATCH_SUCCESS)

#define CONCAT(a, b) a##_##b
#define DEFER_LINE(a, line) CONCAT(a, line)
#define TYPE_CHECK(expr, type) TYPE_CHECK_(expr, type, DEFER_LINE(tc, __LINE__))
#define TYPE_QUERY(expr, type) TYPE_QUERY_(expr, type, DEFER_LINE(tc, __LINE__))

typedef enum CheckStatus {
    Check_Success,  // The node was successfully checked with out errors
    Check_Complete, // The node is done processing

    Check_Errors_Start,
    Check_Goto_Parse,
    Check_Yield,
    Check_Failed,           // The node is done processing and should be put in the state of Failed.
    Check_Error,    // There was an error when checking the node
} CheckStatus;

#define CHECK_FUNC(name, ...) CheckStatus check_##name(Context *context, __VA_ARGS__)

CHECK_FUNC(block, AstBlock* block);
CHECK_FUNC(statement_chain, AstNode** start);
CHECK_FUNC(statement, AstNode** pstmt);
CHECK_FUNC(return, AstReturn* retnode);
CHECK_FUNC(if, AstIfWhile* ifnode);
CHECK_FUNC(while, AstIfWhile* whilenode);
CHECK_FUNC(for, AstFor** pfornode);
CHECK_FUNC(case, AstSwitchCase *casenode);
CHECK_FUNC(switch, AstSwitch* switchnode);
CHECK_FUNC(call, AstCall** pcall);
CHECK_FUNC(binaryop, AstBinaryOp** pbinop);
CHECK_FUNC(unaryop, AstUnaryOp** punop);
CHECK_FUNC(struct_literal, AstStructLiteral* sl);
CHECK_FUNC(array_literal, AstArrayLiteral* al);
CHECK_FUNC(range_literal, AstRangeLiteral** range);
CHECK_FUNC(compound, AstCompound* compound);
CHECK_FUNC(if_expression, AstIfExpression* if_expr);
CHECK_FUNC(expression, AstTyped** expr);
CHECK_FUNC(address_of, AstAddressOf** paof);
CHECK_FUNC(dereference, AstDereference* deref);
CHECK_FUNC(subscript, AstSubscript** paa);
CHECK_FUNC(field_access, AstFieldAccess** pfield);
CHECK_FUNC(method_call, AstBinaryOp** mcall);
CHECK_FUNC(size_of, AstSizeOf* so);
CHECK_FUNC(align_of, AstAlignOf* ao);
CHECK_FUNC(global, AstGlobal* global);
CHECK_FUNC(function, AstFunction* func);
CHECK_FUNC(overloaded_function, AstOverloadedFunction* func);
CHECK_FUNC(struct, AstStructType* s_node);
CHECK_FUNC(temp_function_header, AstFunction* func);
CHECK_FUNC(function_header, AstFunction* func);
CHECK_FUNC(memres_type, AstMemRes* memres);
CHECK_FUNC(memres, AstMemRes* memres);
CHECK_FUNC(type, AstType** ptype);
CHECK_FUNC(insert_directive, AstDirectiveInsert** pinsert, b32 expected_expression);
CHECK_FUNC(directive_solidify, AstDirectiveSolidify** psolid);
CHECK_FUNC(directive_defined, AstDirectiveDefined** pdefined);
CHECK_FUNC(do_block, AstDoBlock** pdoblock);
CHECK_FUNC(constraint, AstConstraint *constraint);
CHECK_FUNC(constraint_context, ConstraintContext *cc, Scope *scope, OnyxFilePos pos);
CHECK_FUNC(polyquery, AstPolyQuery *query);
CHECK_FUNC(directive_first, AstDirectiveFirst *first);
CHECK_FUNC(directive_export_name, AstDirectiveExportName *ename);
CHECK_FUNC(proc_expansion, AstProceduralExpansion **pexp, ProceduralMacroExpansionKind kind);
CHECK_FUNC(package, AstPackage* package);


#define STATEMENT_LEVEL 1
#define EXPRESSION_LEVEL 2

static void scope_enter(Context *context, Scope* new_scope) {
    assert(new_scope);
    context->checker.current_scope = new_scope;
    bh_arr_push(context->checker.scope_stack, new_scope);
}

static void scope_leave(Context *context) {
    assert(bh_arr_length(context->checker.scope_stack) > 0);
    bh_arr_pop(context->checker.scope_stack);
    context->checker.current_scope = bh_arr_last(context->checker.scope_stack);
}

static void clear_modes(Context *context) {
    context->checker.mode = 0;
}

static void enable_mode(Context *context, CheckerMode mode) {
    context->checker.mode |= mode;
}

static void disable_mode(Context *context, CheckerMode mode) {
    context->checker.mode &= ~mode;
}

static b32 mode_enabled(Context *context, CheckerMode mode) {
    return (context->checker.mode & mode) != 0;
}

static inline void fill_in_type(Context *context, AstTyped* node) {
    if (node->type == NULL) {
        if (check_type(context, &node->type_node) > Check_Errors_Start) return;

        node->type = type_build_from_ast(context, node->type_node);
    }
}

static void reset_statement_idx_on_all_blocks(Context *context, AstBlock *block) {
    block->statement_idx = 0;

    AstNode *walker = block->body;
    while (walker) {
        if (walker->kind == Ast_Kind_Block) {
            reset_statement_idx_on_all_blocks(context, (AstBlock *) walker);
        }

        walker = walker->next;
    }
}

CHECK_FUNC(symbol, AstNode** symbol_node) {
    if (mode_enabled(context, CM_Dont_Resolve_Symbols)) return Check_Yield;

    OnyxToken* token = (*symbol_node)->token;
    AstNode* res = symbol_resolve(context, context->checker.current_scope, token);

    if (!res) {
        if (context->cycle_detected) {
            token_toggle_end(token);
            char *closest = find_closest_symbol_in_scope_and_parents(context, context->checker.current_scope, token->text);
            token_toggle_end(token);

            if (closest) ERROR_(token->pos, "Unable to resolve symbol '%b'. Did you mean '%s'?", token->text, token->length, closest);
            else         ERROR_(token->pos, "Unable to resolve symbol '%b'.", token->text, token->length);

            return Check_Error;
        } else {
            return Check_Yield;
        }

    } else {
        track_resolution_for_symbol_info(context, *symbol_node, res);
        *symbol_node = res;
        context->checker.resolved_a_symbol = 1;
    }

    return Check_Success;
}

CHECK_FUNC(local, AstLocal** local) {
    CHECK(type, &(*local)->type_node);

    if ((*local)->token != NULL)
        symbol_introduce(context, context->checker.current_scope, (*local)->token, (AstNode *) *local);

    if ((*local)->auto_dispose) {
        insert_auto_dispose_call(context, *local);
        (*local)->auto_dispose = 0;
    }

    return Check_Success;
}

CHECK_FUNC(return, AstReturn* retnode) {
    Type ** expected_return_type;
    bh_arr(AstLocal *) named_return_values;

    if (retnode->count >= (u32) bh_arr_length(context->checker.expected_return_type_stack)) {
        ERROR_(retnode->token->pos, "Too many repeated 'return's here. Expected a maximum of %d.",
                bh_arr_length(context->checker.expected_return_type_stack));
    }

    if (retnode->from_proc) {
        expected_return_type = context->checker.expected_return_type_stack[0];
        named_return_values  = context->checker.named_return_values_stack[0];
    } else {
        i32 idx = bh_arr_length(context->checker.expected_return_type_stack) - retnode->count - 1;
        expected_return_type = context->checker.expected_return_type_stack[idx];
        named_return_values  = context->checker.named_return_values_stack[idx];
    }


retry_return_expr_check:

    if (retnode->expr) {
        CHECK(expression, &retnode->expr);

        if (*expected_return_type == context->types.auto_return) {
            resolve_expression_type(context, retnode->expr);
            if (retnode->expr->type == NULL)
                YIELD_ERROR(retnode->token->pos, "Unable to determine the automatic return type here.");

            *expected_return_type = retnode->expr->type;
            return Check_Success;
        }

        TYPE_CHECK(&retnode->expr, *expected_return_type) {
            ERROR_(retnode->token->pos,
                    "Expected to return a value of type '%s', returning value of type '%s'.",
                    type_get_name(context, *expected_return_type),
                    node_get_type_name(context, retnode->expr));
        }

        //
        // Catch the obvious case of return '^.{ ... }', as that will never
        // be legal.
        if (retnode->expr->kind == Ast_Kind_Address_Of) {
            AstAddressOf *aof = (AstAddressOf *) retnode->expr;
            if (node_is_addressable_literal((AstNode *) aof->expr)) {
                ERROR(retnode->token->pos, "Cannot return a pointer to a literal, as the space reserved for the literal will be freed upon returning.");
            }
        }

    } else {
        if (*expected_return_type == context->types.auto_return) {
            *expected_return_type = context->types.basic[Basic_Kind_Void];
            return Check_Success;
        }

        if ((*expected_return_type) != context->types.basic[Basic_Kind_Void]) {
            if (!named_return_values) {
                ERROR_(retnode->token->pos,
                    "Returning from non-void function without a value. Expected a value of type '%s'.",
                    type_get_name(context, *expected_return_type));

            } else {
                if (bh_arr_length(named_return_values) == 1) {
                    retnode->expr = (AstTyped *) named_return_values[0];

                } else {
                    AstCompound *implicit_compound = onyx_ast_node_new(context->ast_alloc, sizeof(AstCompound), Ast_Kind_Compound);
                    implicit_compound->token = retnode->token;

                    bh_arr_new(context->ast_alloc, implicit_compound->exprs, bh_arr_length(named_return_values));
                    bh_arr_each(AstLocal *, named_return, named_return_values) {
                        bh_arr_push(implicit_compound->exprs, (AstTyped *) *named_return);
                    }

                    retnode->expr = (AstTyped *) implicit_compound;
                }

                goto retry_return_expr_check;
            }
        }
    }

    return Check_Success;
}

CHECK_FUNC(if, AstIfWhile* ifnode) {
    if (ifnode->kind == Ast_Kind_Static_If) {
        if ((ifnode->flags & Ast_Flag_Static_If_Resolved) == 0) {
            YIELD(ifnode->token->pos, "Waiting for static if to be resolved.");
        }

        if (static_if_resolution(context, ifnode)) {
            if (ifnode->true_stmt != NULL) {
                ifnode->true_stmt->rules = Block_Rule_Macro & ~Block_Rule_New_Scope;
                CHECK(statement, (AstNode **) &ifnode->true_stmt);
                ifnode->flags |= ifnode->true_stmt->flags & Ast_Flag_Block_Returns;
            }

        } else {
            if (ifnode->false_stmt != NULL) {
                ifnode->false_stmt->rules = Block_Rule_Macro & ~Block_Rule_New_Scope;
                CHECK(statement, (AstNode **) &ifnode->false_stmt);
                ifnode->flags |= ifnode->false_stmt->flags & Ast_Flag_Block_Returns;
            }
        }

    } else {
        if (ifnode->initialization != NULL) {
            if (!ifnode->scope) {
                ifnode->scope = scope_create(context, context->checker.current_scope, ifnode->token->pos);
            }

            scope_enter(context, ifnode->scope);
            CHECK(statement_chain, &ifnode->initialization);
        }

        CHECK(expression, &ifnode->cond);

        if (!type_is_bool(ifnode->cond->type)) {
            TypeMatch implicit_cast = implicit_cast_to_bool(context, &ifnode->cond);
            if (implicit_cast == TYPE_MATCH_YIELD) YIELD(ifnode->token->pos, "Waiting for implicit cast to bool to check.");
            if (implicit_cast == TYPE_MATCH_FAILED) {
                ERROR_(ifnode->token->pos, "Expected expression of type 'bool' for condition, got '%s'", type_get_name(context, ifnode->cond->type));
            }
        }

        if (ifnode->true_stmt)  CHECK(statement, (AstNode **) &ifnode->true_stmt);
        if (ifnode->false_stmt) CHECK(statement, (AstNode **) &ifnode->false_stmt);

        if (ifnode->true_stmt && ifnode->false_stmt) {
            if ((ifnode->true_stmt->flags & Ast_Flag_Block_Returns) && (ifnode->false_stmt->flags & Ast_Flag_Block_Returns))
                ifnode->flags |= Ast_Flag_Block_Returns;
        }

        if (ifnode->initialization != NULL) {
            scope_leave(context);
        }
    }

    return Check_Success;
}

CHECK_FUNC(while, AstIfWhile* whilenode) {
    if (whilenode->initialization != NULL) {
        if (!whilenode->scope) {
            whilenode->scope = scope_create(context, context->checker.current_scope, whilenode->token->pos);
        }

        scope_enter(context, whilenode->scope);
        CHECK(statement_chain, &whilenode->initialization);
    }

    CHECK(expression, &whilenode->cond);

    if (!type_is_bool(whilenode->cond->type)) {
        TypeMatch implicit_cast = implicit_cast_to_bool(context, &whilenode->cond);
        if (implicit_cast == TYPE_MATCH_YIELD) YIELD(whilenode->token->pos, "Waiting for implicit cast to bool to check.");
        if (implicit_cast == TYPE_MATCH_FAILED) {
            ERROR_(whilenode->token->pos, "Expected expression of type 'bool' for condition, got '%s'", type_get_name(context, whilenode->cond->type));
        }
    }

    bh_arr_push(context->checker.while_node_stack, whilenode);

    if (whilenode->true_stmt) {
        CHECK(statement, (AstNode **) &whilenode->true_stmt);
    }

    if (whilenode->false_stmt) {
        if (whilenode->bottom_test) {
            ERROR(whilenode->token->pos, "while-loops with an 'else' clause cannot be bottom tested.");
        }

        CHECK(statement, (AstNode **) &whilenode->false_stmt);
    }

    bh_arr_pop(context->checker.while_node_stack);

    if (whilenode->initialization != NULL) {
        scope_leave(context);
    }

    return Check_Success;
}

CHECK_FUNC(for, AstFor** pfornode) {
    AstFor *fornode = *pfornode;

    // HACK
    CHECK(expression, (AstTyped **) &context->builtins.for_expansion_flag_type);

    CHECK(expression, &fornode->iter);
    resolve_expression_type(context, fornode->iter);

    bh_arr_each(AstLocal *, index_variable, fornode->indexing_variables) {
        if ((*index_variable)->type_node) {
            CHECK(type, &(*index_variable)->type_node);
        }
    }

    if (!fornode->intermediate_macro_expansion) {
        fornode->intermediate_macro_expansion = create_implicit_for_expansion_call(context, fornode);
        assert(fornode->intermediate_macro_expansion);
    }
 
    CheckStatus cs = check_call(context, (AstCall **) &fornode->intermediate_macro_expansion);
    if (cs == Check_Yield) {
        if (fornode->intermediate_macro_expansion->kind == Ast_Kind_Call) {
            return Check_Yield;
        }
    }

    if (cs == Check_Error) {
        i32 vars = bh_arr_length(fornode->indexing_variables);

        ERROR_(fornode->token->pos, "Unable to loop over a '%s'%s with %d captured argument%s.",
               type_get_name(context, fornode->iter->type),
               fornode->by_pointer ? " by pointer," : "",
               vars,
               bh_num_plural(vars));
    }

    *pfornode = (AstFor *) fornode->intermediate_macro_expansion;
    CHECK(expression, (AstTyped **) pfornode);

    return Check_Success;
}

static b32 add_case_to_switch_statement(Context *context, AstSwitch* switchnode, u64 case_value, AstSwitchCase* casestmt, OnyxFilePos pos) {
    assert(switchnode->switch_kind == Switch_Kind_Integer || switchnode->switch_kind == Switch_Kind_Union);

    switchnode->min_case = bh_min(switchnode->min_case, case_value);
    switchnode->max_case = bh_max(switchnode->max_case, case_value);

    if (bh_imap_has(&switchnode->case_map, case_value)) {
        ONYX_ERROR(pos, Error_Critical, "Multiple cases for values '%d'.", case_value);
        return 1;
    }

    bh_imap_put(&switchnode->case_map, case_value, (u64) casestmt);
    return 0;
}

static CheckStatus collect_switch_case_blocks(Context *context, AstSwitch* switchnode, AstBlock* root) {
    AstNode *walker = root->body;
    while (walker != NULL) {
        switch (walker->kind) {
            case Ast_Kind_Block:
                collect_switch_case_blocks(context, switchnode, (AstBlock *) walker);
                break;

            case Ast_Kind_Switch_Case: {
                AstSwitchCase *case_node = (AstSwitchCase *) walker;
                if (case_node->is_default) {
                    if (switchnode->default_case != NULL && switchnode->default_case != case_node->block) {
                        ERROR(case_node->token->pos, "Multiple default cases given");
                        ERROR(switchnode->default_case->token->pos, "Multiple default cases given");
                        return Check_Error;
                    }

                    switchnode->default_case = case_node->block;
                } else {
                    bh_arr_push(switchnode->cases, case_node);
                }
                break;
            }

            case Ast_Kind_Static_If: {
                // At this point, the static if will be resolved. So, this
                // only needs to add the cases from one side or another.

                AstIf* static_if = (AstIf *) walker;
                assert(static_if->flags & Ast_Flag_Static_If_Resolved);

                if (static_if_resolution(context, static_if)) {
                    if (static_if->true_stmt) collect_switch_case_blocks(context, switchnode, static_if->true_stmt);
                } else {
                    if (static_if->false_stmt) collect_switch_case_blocks(context, switchnode, static_if->false_stmt);
                }

                break;
            }

            default:
                ERROR(walker->token->pos, "This statement is not allowed here.");
        }

        walker = walker->next;
    }

    return Check_Success;
}

CHECK_FUNC(case, AstSwitchCase *casenode) {
    if (!casenode->is_default) {
        bh_arr_each(AstTyped *, expr, casenode->values) {
            CHECK(expression, expr);
        }
    }

    if (mode_enabled(context, CM_Dont_Check_Case_Bodies)) return Check_Success;

    if (casenode->capture) {
        if (casenode->scope == NULL) {
            casenode->scope = scope_create(context, context->checker.current_scope, casenode->token->pos);
            symbol_introduce(context, casenode->scope, casenode->capture->token, (AstNode *) casenode->capture);
        }

        scope_enter(context, casenode->scope);
    }

    if (casenode->body_is_expr) {
        CHECK(expression, &casenode->expr);
    } else {
        CHECK(block, casenode->block);
    }

    if (casenode->capture) {
        scope_leave(context);
    }

    return Check_Success;
}

CHECK_FUNC(switch, AstSwitch* switchnode) {
    //
    // Checking switches is quite complicated and tricky because of the feature-set Onyx
    // supports. Switch bodies can contain arbitrary statements at parse-time, but must
    // be expanded to a tree of block-nodes with case-nodes as the leaves. This complicates
    // the checking, because case-bodies cannot be checked before they know the type of their
    // captured variables (if there are any), but the case values must be checked before the
    // switch node can do proper type checking.
    // 

    if (switchnode->initialization) {
        if (switchnode->scope == NULL) {
            switchnode->scope = scope_create(context, context->checker.current_scope, switchnode->token->pos);
        }

        scope_enter(context, switchnode->scope);

        CHECK(statement_chain, &switchnode->initialization);
    }

    CHECK(expression, &switchnode->expr);
    Type* resolved_expr_type = resolve_expression_type(context, switchnode->expr);

    if (!(switchnode->flags & Ast_Flag_Has_Been_Checked)) {
        if (resolved_expr_type == NULL) YIELD(switchnode->token->pos, "Waiting for expression type to be known.");

        switchnode->switch_kind = Switch_Kind_Integer;
        if (!type_is_integer(switchnode->expr->type) && switchnode->expr->type->kind != Type_Kind_Enum) {
            switchnode->switch_kind = Switch_Kind_Use_Equals;
        }

        if (type_union_get_variant_count(switchnode->expr->type) > 0) {
            switchnode->switch_kind = Switch_Kind_Union;
        }

        switch (switchnode->switch_kind) {
            case Switch_Kind_Integer:
                switchnode->min_case = 0xffffffffffffffff;
                bh_imap_init(&switchnode->case_map, context->gp_alloc, 4);
                break;

            case Switch_Kind_Use_Equals:
                bh_arr_new(context->gp_alloc, switchnode->case_exprs, 4);
                break;

            case Switch_Kind_Union:
                switchnode->min_case = 1;
                bh_imap_init(&switchnode->case_map, context->gp_alloc, 4);

                u32 variants = type_union_get_variant_count(switchnode->expr->type);
                switchnode->union_variants_handled = bh_alloc_array(context->ast_alloc, u8, variants);
                break;

            default: assert(0);
        }

        switchnode->flags |= Ast_Flag_Has_Been_Checked;
    }

    if (switchnode->cases == NULL) {
        //
        // Set CM_Dont_Check_Case_Bodies so the bodies of case nodes will be skipped.
        // This avoids weird type-checking and symbol resolution issues when a case
        // node comes from an #insert or macro expansion. They will be re-checked
        // fully in the next check_block below.
        //
        enable_mode(context, CM_Dont_Check_Case_Bodies);
        CHECK(block, switchnode->case_block);
        disable_mode(context, CM_Dont_Check_Case_Bodies);

        bh_arr_new(context->gp_alloc, switchnode->cases, 4);
        if (collect_switch_case_blocks(context, switchnode, switchnode->case_block) != Check_Success) {
            return Check_Error;
        }

        // This is important, otherwise if this block has to return to symbol resolution.
        reset_statement_idx_on_all_blocks(context, switchnode->case_block);
    }

    fori (i, switchnode->yield_return_index, bh_arr_length(switchnode->cases)) {
        AstSwitchCase *sc = switchnode->cases[i];

        if (sc->capture && bh_arr_length(sc->values) != 1) {
            ERROR(sc->token->pos, "Expected exactly one value in switch-case when using a capture, i.e. `case X as value { ... }`.");
        }

        if (sc->capture && switchnode->switch_kind != Switch_Kind_Union) {
            ERROR_(sc->capture->token->pos, "Captures in switch cases are only allowed when switching over a union type. Switching over '%s' here.",
                    type_get_name(context, switchnode->expr->type));
        }

        if (sc->flags & Ast_Flag_Has_Been_Checked) goto check_switch_case_block;

        bh_arr_each(AstTyped *, value, sc->values) {
            CHECK(expression, value);

            // Handle case 1 .. 10
            if (switchnode->switch_kind == Switch_Kind_Integer && (*value)->kind == Ast_Kind_Range_Literal) {
                AstRangeLiteral* rl = (AstRangeLiteral *) (*value);
                resolve_expression_type(context, rl->low);
                resolve_expression_type(context, rl->high);

                if (rl->low->kind != Ast_Kind_NumLit || rl->high->kind != Ast_Kind_NumLit) {
                    ERROR(rl->token->pos, "case statement expected compile time known range.");
                }

                promote_numlit_to_larger(context, (AstNumLit *) rl->low);
                promote_numlit_to_larger(context, (AstNumLit *) rl->high);

                i64 lower = ((AstNumLit *) rl->low)->value.l;
                i64 upper = ((AstNumLit *) rl->high)->value.l;

                fori (case_value, lower, upper) {
                    if (add_case_to_switch_statement(context, switchnode, case_value, sc, rl->token->pos))
                        return Check_Error;
                }

                continue;
            }

            if (switchnode->switch_kind == Switch_Kind_Union) {
                Type *union_expr_type = resolved_expr_type;
                if (union_expr_type->kind == Type_Kind_Pointer) {
                    union_expr_type = union_expr_type->Pointer.elem;
                }

                TYPE_CHECK(value, union_expr_type->Union.tag_type) {
                    OnyxToken* tkn = sc->block->token;
                    if ((*value)->token) tkn = (*value)->token;

                    ERROR_(tkn->pos, "'%b' is not a variant of '%s'.",
                        (*value)->token->text, (*value)->token->length, type_get_name(context, union_expr_type));
                }

                // We subtract one here because variant numbering starts at 1, instead of 0.
                // This is so a zeroed out block of memory does not have a valid variant.
                // This is going to change now...
                i32 variant_number = get_expression_integer_value(context, *value, NULL);
                switchnode->union_variants_handled[variant_number] = 1;

                UnionVariant *union_variant = union_expr_type->Union.variants_ordered[variant_number];
                if (sc->capture) {
                    if (sc->capture_is_by_pointer) {
                        sc->capture->type = type_make_pointer(context, union_variant->type);
                    } else {
                        sc->capture->type = union_variant->type;
                    }
                }

            } else {
                TYPE_CHECK(value, resolved_expr_type) {
                    OnyxToken* tkn = sc->block->token;
                    if ((*value)->token) tkn = (*value)->token;

                    ERROR_(tkn->pos, "Mismatched types in switch-case. Expected '%s', got '%s'.",
                        type_get_name(context, resolved_expr_type), type_get_name(context, (*value)->type));
                }
            }

            switch (switchnode->switch_kind) {
                case Switch_Kind_Integer:
                case Switch_Kind_Union: {
                    b32 is_valid;
                    i64 integer_value = get_expression_integer_value(context, *value, &is_valid);
                    if (!is_valid)
                        ERROR_((*value)->token->pos, "Case statement expected compile time known integer. Got '%s'.", onyx_ast_node_kind_string((*value)->kind));

                    if (add_case_to_switch_statement(context, switchnode, integer_value, sc, sc->block->token->pos))
                        return Check_Error;

                    break;
                }

                case Switch_Kind_Use_Equals: {
                    // Gross
                    b32 found = 0;
                    bh_arr_each(CaseToBlock, ctb, switchnode->case_exprs) {
                        if (ctb->original_value == *value) {
                            CHECK(expression, (AstTyped **) &ctb->comparison);
                            found = 1;
                            break;
                        }
                    }
                    if (found) break;

                    CaseToBlock ctb;
                    ctb.casestmt = sc;
                    ctb.original_value = *value;
                    ctb.comparison = make_binary_op(context, Binary_Op_Equal, switchnode->expr, *value);
                    ctb.comparison->token = (*value)->token;
                    bh_arr_push(switchnode->case_exprs, ctb);

                    CHECK(binaryop, &bh_arr_last(switchnode->case_exprs).comparison);
                    break;
                }
            }
        }

        sc->flags |= Ast_Flag_Has_Been_Checked;

      check_switch_case_block:
        if (switchnode->is_expr) {
            if (!sc->body_is_expr) {
                ONYX_ERROR(sc->token->pos, Error_Critical, "Inside a switch expression, all cases must return a value.");
                ERROR(sc->token->pos, "Change the case statement to look like 'case X => expr'.");
            }
        } else {
            if (sc->body_is_expr) {
                ERROR(sc->token->pos, "This kind of case statement is only allowed in switch expressions, not switch statements.");
            }
        }

        switchnode->yield_return_index += 1;
    }

    CHECK(block, switchnode->case_block);

    b32 all_cases_return = 1;
    bh_arr_each(AstSwitchCase *, pcase, switchnode->cases) {
        AstSwitchCase *sc = *pcase;

        if (sc->body_is_expr) {
            if (switchnode->type == NULL) {
                switchnode->type = resolve_expression_type(context, sc->expr);

            } else {
                TYPE_CHECK(&sc->expr, switchnode->type) {
                    ERROR_(sc->token->pos, "Expected case expression to be of type '%s', got '%s'.",
                        type_get_name(context, switchnode->type),
                        type_get_name(context, sc->expr->type));
                }
            }

        } else {
            all_cases_return = all_cases_return && (sc->block->flags & Ast_Flag_Block_Returns);
        }
    }

    if (switchnode->default_case) {
        if (switchnode->is_expr) {
            AstTyped **default_case = (AstTyped **) &switchnode->default_case;
            CHECK(expression, default_case);

            if (switchnode->type) {
                TYPE_CHECK(default_case, switchnode->type) {
                    ERROR_((*default_case)->token->pos, "Expected case expression to be of type '%s', got '%s'.",
                        type_get_name(context, switchnode->type),
                        type_get_name(context, (*default_case)->type));
                }
            }

        } else {
            CHECK(block, switchnode->default_case);

            all_cases_return = all_cases_return && (switchnode->default_case->flags & Ast_Flag_Block_Returns);
        }

    } else if (switchnode->switch_kind == Switch_Kind_Union) {
        // If there is no default case, and this is a union switch,
        // make sure all cases are handled.

        bh_arr(char *) missed_variants = NULL;

        i32 variant_count = type_union_get_variant_count(switchnode->expr->type);
        fori (i, 0, variant_count) {
            if (!switchnode->union_variants_handled[i]) {
                UnionVariant *uv = type_lookup_union_variant_by_idx(switchnode->expr->type, i);
                assert(uv && uv->name);
                bh_arr_push(missed_variants, uv->name);
            }
        }

        i32 missed_variant_count = bh_arr_length(missed_variants);
        if (missed_variant_count > 0) {
            char  buf[1024] = {0};
            fori (i, 0, bh_min(missed_variant_count, 2)) {
                if (i != 0) strncat(buf, ", ", 1023);
                strncat(buf, missed_variants[i], 1023);
            }

            if (missed_variant_count > 2) {
                strncat(buf, bh_bprintf(" and %d more", missed_variant_count - 2), 1023);
            }

            ERROR_(switchnode->token->pos, "Unhandled union variants: '%s'", buf);
        }
    }

    if (all_cases_return) {
        switchnode->flags |= Ast_Flag_Block_Returns;
    }

    if (switchnode->initialization) {
        scope_leave(context);
    }

    return Check_Success;
}

CHECK_FUNC(arguments, Arguments* args) {
    bh_arr_each(AstTyped *, actual, args->values)
        CHECK(expression, actual);

    bh_arr_each(AstNamedValue *, named_value, args->named_values)
        CHECK(expression, &(*named_value)->value);

    return Check_Success;
}

CHECK_FUNC(argument, AstArgument** parg) {
    CHECK(expression, &(*parg)->value);
    (*parg)->type = (*parg)->value->type;

    return Check_Success;
}

CHECK_FUNC(resolve_callee, AstCall* call, AstTyped** effective_callee) {
    if (call->kind == Ast_Kind_Intrinsic_Call) return Check_Success;

    AstTyped* callee = (AstTyped *) strip_aliases((AstNode *) call->callee);
    AstTyped* original_callee = callee;

    b32 calling_a_macro = 0;
    b32 need_to_check_overload_return_type = 0;

    if (callee->kind == Ast_Kind_Overloaded_Function) {
        AstTyped* new_callee = find_matching_overload_by_arguments(
            context,
            ((AstOverloadedFunction *) callee)->overloads,
            &call->args);

        if (new_callee == NULL) {
            if (context->cycle_almost_detected < 2) {
                YIELD(call->token->pos, "Waiting to know all options for overloaded function");
            }

            //
            // When reporting an error about a for-loop expansion overload, don't report all the overloads
            // as there are likely very many of them and the error message won't be very useful. Instead,
            // a better error message will be printed in check_for.
            if ((AstOverloadedFunction *) callee == context->builtins.for_expansion) {
                return Check_Error;
            }

            report_unable_to_match_overload(context, call, ((AstOverloadedFunction *) callee)->overloads);
            return Check_Error;
        }

        if (new_callee == (AstTyped *) &context->node_that_signals_a_yield) {
            YIELD(call->token->pos, "Waiting for overloaded function option to pass type-checking.");
        }

        need_to_check_overload_return_type = 1;

        callee = new_callee;
    }

    if (callee->kind == Ast_Kind_Macro) {
        calling_a_macro = 1;
        call->callee = callee;

        AstTyped* new_callee = (AstTyped *) macro_resolve_header(context, (AstMacro *) callee, &call->args, call->token, 1);
        if (new_callee == NULL) return Check_Error;
        if (new_callee == (AstTyped *) &context->node_that_signals_a_yield) {
            YIELD(call->token->pos, "Waiting for macro header to pass type-checking.");
        }

        arguments_remove_baked(&call->args);
        callee = new_callee;

    } else while (callee->kind == Ast_Kind_Polymorphic_Proc) {
        AstTyped* new_callee = (AstTyped *) polymorphic_proc_lookup(context, (AstFunction *) callee, PPLM_By_Arguments, &call->args, call->token);
        if (new_callee == NULL) return Check_Error;
        if (new_callee == (AstTyped *) &context->node_that_signals_a_yield) {
            YIELD(call->token->pos, "Waiting for polymorphic procedure header to pass type-checking.");
        }

        arguments_remove_baked(&call->args);
        callee = new_callee;
    }

    // NOTE: Build callee's type
    fill_in_type(context, (AstTyped *) callee);
    if (callee->type == NULL) {
        YIELD(call->token->pos, "Trying to resolve function type for callee.");
    }

    if (!calling_a_macro) call->callee = callee;

    if (callee->type->kind != Type_Kind_Function) {
        ERROR_(call->token->pos,
                "Attempting to call something that is not a function, '%b'.",
                callee->token->text, callee->token->length);
    }

    if (need_to_check_overload_return_type) {
        ensure_overload_returns_correct_type(context, callee, (AstOverloadedFunction *) original_callee);
    }

    *effective_callee = callee;
    return Check_Success;
}

CHECK_FUNC(call, AstCall** pcall) {
    // All the things that need to be done when checking a call node.
    //      1. Ensure the callee is not a symbol
    //      2. Check the callee expression (since it could be a variable or a field access, etc)
    //      3. Check all arguments
    //          * Cannot pass overloaded functions (ROBUSTNESS)
    //      4. If callee is an overloaded function, use the argument types to determine which overload is used.
    //      5. If callee is polymorphic, use the arguments type to generate a polymorphic function.
    //      7. Fill in arguments
    //      8. If callee is an intrinsic, turn call into an Intrinsic_Call node
    //      9. Check types of formal and actual params against each other, handling varargs
    AstCall* call = *pcall;

    if (call->placeholder_argument_position > 0) {
        ONYX_ERROR(call->token->pos, Error_Critical, "This call contains an argument placeholder '_', but it was not piped into.");
        return Check_Error;
    }

    if (call->kind == Ast_Kind_Call) {
        u32 current_checking_level_store = context->checker.current_checking_level;
        CHECK(expression, &call->callee);
        context->checker.current_checking_level = current_checking_level_store;

        AstNode* callee = strip_aliases((AstNode *) call->callee);
        if (callee->kind == Ast_Kind_Poly_Struct_Type ||
            callee->kind == Ast_Kind_Poly_Union_Type) {
            *pcall = (AstCall *) convert_call_to_polycall(context, call);
            CHECK(expression, (AstTyped **) pcall);
            return Check_Success;
        }
    }

    if (call->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    u32 current_checking_level_store = context->checker.current_checking_level;
    CHECK(arguments, &call->args);
    context->checker.current_checking_level = current_checking_level_store;

    AstFunction* callee=NULL;
    CHECK(resolve_callee, call, (AstTyped **) &callee);

    if (callee->kind == Ast_Kind_Function) {
        if (callee->constraints.constraints != NULL && callee->constraints.constraints_met == 0) {
            YIELD(call->token->pos, "Waiting for constraints to be checked on callee.");
        }
    }

    i32 arg_count = get_argument_buffer_size(context, &callee->type->Function, &call->args);
    arguments_ensure_length(context, &call->args, arg_count);

    char* err_msg = NULL;
    fill_in_arguments(context, &call->args, (AstNode *) callee, &err_msg, 0);
    if (err_msg != NULL) {
        ONYX_ERROR(callee->token->pos, Error_Critical, "Here is the function being called.");
        ERROR(call->token->pos, err_msg);
    }

    bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values;
    bh_arr_each(AstArgument *, arg, arg_arr) {
        if (*arg != NULL) continue;

        ERROR(call->token->pos, "Not all arguments were given a value.");
    }

    // HACK HACK HACK
    // :CallSiteIsGross
    bh_arr_each(AstArgument *, arg, arg_arr) {
        AstTyped** arg_value = &(*arg)->value;

        if ((*arg_value)->kind == Ast_Kind_Call_Site) {
            AstCallSite* callsite = (AstCallSite *) ast_clone(context, *arg_value);
            if (callsite->collapsed) continue;

            callsite->callsite_token = call->token;

            // HACK CLEANUP
            OnyxToken* str_token = bh_alloc(context->ast_alloc, sizeof(OnyxToken));
            str_token->text  = bh_strdup(context->gp_alloc, (char *) call->token->pos.filename);
            str_token->length = strlen(call->token->pos.filename);
            str_token->pos = call->token->pos;
            str_token->type = Token_Type_Literal_String;

            AstStrLit* filename = bh_alloc_item(context->ast_alloc, AstStrLit);
            memset(filename, 0, sizeof(AstStrLit));
            filename->kind  = Ast_Kind_StrLit;
            filename->token = str_token;
            filename->data_id = 0;
            filename->type_node = context->builtins.string_type;

            add_entities_for_node(&context->entities, NULL, (AstNode *) filename, NULL, NULL);
            callsite->filename = filename;

            callsite->line   = make_int_literal(context, call->token->pos.line);
            callsite->column = make_int_literal(context, call->token->pos.column);

            convert_numlit_to_type(context, callsite->line,   context->types.basic[Basic_Kind_U32], 1);
            convert_numlit_to_type(context, callsite->column, context->types.basic[Basic_Kind_U32], 1);

            callsite->collapsed = 1;
            *arg_value = (AstTyped *) callsite;
        }
    }

    // NOTE: If we are calling an intrinsic function, translate the
    // call into an intrinsic call node.
    if (callee->kind == Ast_Kind_Function && callee->is_intrinsic) {
        call->kind = Ast_Kind_Intrinsic_Call;
        call->callee = NULL;

        token_toggle_end(callee->intrinsic_name);
        char* intr_name = callee->intrinsic_name->text;

        OnyxIntrinsic intrinsic = 0xffffffff;
        const IntrinsicMap *im = &builtin_intrinsics[0];
        while (im->name) {
            if (!strcmp(im->name, intr_name)) {
                intrinsic = im->intrinsic;
                break;
            }
            im++;
        }

        if (intrinsic == 0xffffffff) {
            ONYX_ERROR(callee->token->pos, Error_Critical, "Intrinsic not supported, '%s'.", intr_name);
            token_toggle_end(callee->intrinsic_name);
            return Check_Error;
        }

        call->intrinsic = intrinsic;

        token_toggle_end(callee->intrinsic_name);
    }

    call->va_kind = VA_Kind_Not_VA;
    call->type = callee->type->Function.return_type;
    if (call->type == context->types.auto_return && call->callee->kind != Ast_Kind_Macro) {
        YIELD(call->token->pos, "Waiting for auto-return type to be solved.");
    }

    OnyxError error;
    TypeMatch tm = check_arguments_against_type(context, &call->args, &callee->type->Function, &call->va_kind,
                                                call->token, get_function_name(context, callee), &error);
    if (tm == TYPE_MATCH_FAILED) {
        onyx_submit_error(context, error);
        return Check_Error;
    }

    if (tm == TYPE_MATCH_YIELD || tm == TYPE_MATCH_SPECIAL) {
        YIELD(call->token->pos, "Waiting on argument type checking.");
    }

    if (tm == TYPE_MATCH_YIELD) YIELD(call->token->pos, "Waiting on argument type checking.");

    call->flags |= Ast_Flag_Has_Been_Checked;

    if (call->kind == Ast_Kind_Call && call->callee->kind == Ast_Kind_Macro) {
        expand_macro(context, pcall, callee);
        return Check_Yield;
    }

    if (callee->kind == Ast_Kind_Function && callee->deprecated_warning) {
        ONYX_WARNING(callee->token->pos, "Calling a deprecated function: %b",
            callee->deprecated_warning->token->text, callee->deprecated_warning->token->length);

        ONYX_WARNING(call->token->pos, "Here is where the deprecated function was called.");
    }

    return Check_Success;
}

static void report_bad_binaryop(Context *context, AstBinaryOp* binop) {
    ONYX_ERROR(binop->token->pos, Error_Critical, "Binary operator '%s' not understood for arguments of type '%s' and '%s'.",
            binaryop_string[binop->operation],
            node_get_type_name(context, binop->left),
            node_get_type_name(context, binop->right));
}

static AstCall* binaryop_try_operator_overload(Context *context, AstBinaryOp* binop, AstTyped* third_argument) {
    if (bh_arr_length(context->operator_overloads[binop->operation]) == 0) return &context->checker.__op_maybe_overloaded;

    if (binop->overload_args == NULL || binop->overload_args->values[1] == NULL) {
        if (binop->overload_args == NULL) {
            binop->overload_args = bh_alloc_item(context->ast_alloc, Arguments);
            bh_arr_new(context->ast_alloc, binop->overload_args->values, 3);
            bh_arr_set_length(binop->overload_args->values, third_argument ? 3 : 2);
        }

        if (binop_is_assignment(binop->operation)) {
            binop->overload_args->values[0] = (AstTyped *) make_address_of(context, binop->left);

            u32 current_all_checks_are_final = context->checker.all_checks_are_final;
            context->checker.all_checks_are_final = 0;
            u32 current_checking_level_store = context->checker.current_checking_level;
            CheckStatus cs = check_address_of(context, (AstAddressOf **) &binop->overload_args->values[0]);
            context->checker.current_checking_level = current_checking_level_store;
            context->checker.all_checks_are_final   = current_all_checks_are_final;

            if (cs == Check_Yield)      return (AstCall *) &context->node_that_signals_a_yield;
            if (cs == Check_Error)            return NULL;

            binop->overload_args->values[0] = (AstTyped *) make_argument(context, binop->overload_args->values[0]);

        } else {
            binop->overload_args->values[0] = (AstTyped *) make_argument(context, binop->left);
        }

        binop->overload_args->values[1] = (AstTyped *) make_argument(context, binop->right);
        if (third_argument != NULL) binop->overload_args->values[2] = (AstTyped *) make_argument(context, third_argument);
    }

    AstTyped* overload = find_matching_overload_by_arguments(context, context->operator_overloads[binop->operation], binop->overload_args);
    if (overload == NULL || overload == (AstTyped *) &context->node_that_signals_a_yield) return (AstCall *) overload;

    AstCall* implicit_call = onyx_ast_node_new(context->ast_alloc, sizeof(AstCall), Ast_Kind_Call);
    implicit_call->token = binop->token;
    implicit_call->callee = overload;
    implicit_call->va_kind = VA_Kind_Not_VA;

    arguments_clone(context, &implicit_call->args, binop->overload_args);
    return implicit_call;
}

static AstCall* unaryop_try_operator_overload(Context *context, AstUnaryOp* unop) {
    if (bh_arr_length(context->unary_operator_overloads[unop->operation]) == 0) return &context->checker.__op_maybe_overloaded;

    if (unop->overload_args == NULL || unop->overload_args->values[0] == NULL) {
        if (unop->overload_args == NULL) {
            unop->overload_args = bh_alloc_item(context->ast_alloc, Arguments);
            bh_arr_new(context->ast_alloc, unop->overload_args->values, 1);
            bh_arr_set_length(unop->overload_args->values, 1);
        }

        unop->overload_args->values[0] = (AstTyped *) make_argument(context, unop->expr);
    }

    AstTyped* overload = find_matching_overload_by_arguments(context, context->unary_operator_overloads[unop->operation], unop->overload_args);
    if (overload == NULL || overload == (AstTyped *) &context->node_that_signals_a_yield) return (AstCall *) overload;

    AstCall* implicit_call = onyx_ast_node_new(context->ast_alloc, sizeof(AstCall), Ast_Kind_Call);
    implicit_call->token = unop->token;
    implicit_call->callee = overload;
    implicit_call->va_kind = VA_Kind_Not_VA;

    arguments_clone(context, &implicit_call->args, unop->overload_args);
    return implicit_call;
}



static CheckStatus assign_type_or_check(Context *context, AstTyped **node, Type *type, OnyxToken *report_loc) {
    if (node && (*node)->type == NULL) {
        (*node)->type = type;

    } else {
        TYPE_CHECK(node, type) {
            ERROR_(report_loc->pos,
                    "Cannot assign value of type '%s' to a '%s'.",
                    type_get_name(context, type),
                    node_get_type_name(context, *node));
            return Check_Error;
        }
    }

    return Check_Success;
}

#define TRY_ASSIGN_TYPE_OR_FAIL(context, node, type, report) do { \
    CheckStatus stat = assign_type_or_check((context), (node), (type), (report)); \
    if (stat != Check_Success) return stat; \
    } while (0);

CHECK_FUNC(binaryop_assignment, AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;
    if (context->checker.current_checking_level == EXPRESSION_LEVEL)
        ERROR(binop->token->pos, "Assignment not valid in expression.");

    if (!is_lval((AstNode *) binop->left))
        ERROR_(binop->left->token->pos,
                "Cannot assign to '%b'.",
                binop->left->token->text, binop->left->token->length);

    if ((binop->left->flags & Ast_Flag_Const) != 0 && binop->left->type != NULL)
        ERROR_(binop->token->pos,
                "Cannot assign to constant '%b'.",
                binop->left->token->text, binop->left->token->length);

    if (binop->operation == Binary_Op_Assign) {
        // NOTE: Raw assignment

        // NOTE: This is the 'type inference' system. Very stupid, but very easy.
        // If a left operand has an unknown type, fill it in with the type of
        // the right hand side.
        if (binop->left->type == NULL) {
            if (binop->left->type_node != NULL && binop->left->entity && binop->left->entity->state <= Entity_State_Check_Types) {
                YIELD(binop->token->pos, "Waiting for type to be constructed on left hand side.");
            }

            // NOTE: There is a subtlety here. You cannot use the result of `resolve_expression_type` directly,
            // as in some cases (especially with macros and polyprocs), the result is not "correct". The result
            // makes them appears as though they are runtime-known values, which they are not. Using the following
            // pattern does prevent this issue.
            resolve_expression_type(context, binop->right);

            Type* right_type = get_expression_type(context, binop->right);
            if (right_type == NULL) {
                if (binop->right->entity == NULL || binop->right->entity->state > Entity_State_Check_Types) {
                    ERROR(binop->token->pos, "Could not resolve type of right hand side to infer.");

                } else {
                    YIELD(binop->token->pos, "Trying to resolve type of right hand side.");
                }
            }

            if (right_type->kind == Type_Kind_Compound) {
                AstCompound* lhs = (AstCompound *) binop->left;

                i32 given_expr_count = right_type->Compound.count;
                i32 store_expr_count = 1;
                if (lhs->kind == Ast_Kind_Compound) {
                    store_expr_count = bh_arr_length(lhs->exprs);
                }

                if (get_call_expr_from_node((AstNode *) binop->right)) {
                    if (store_expr_count > given_expr_count) {
                        ERROR_(binop->token->pos, "Left hand side can only have %d expressions here.", given_expr_count);
                    }

                } else if (store_expr_count != given_expr_count) {
                    ERROR_(binop->token->pos, "Expected left hand side to have %d expressions.", given_expr_count);
                }

                if (store_expr_count == 1 && lhs->kind != Ast_Kind_Compound) {
                    TRY_ASSIGN_TYPE_OR_FAIL(context, &binop->left, right_type->Compound.types[0], binop->token);

                } else {
                    fori (i, 0, store_expr_count) {
                        if (right_type->Compound.types[i] == context->types.basic[Basic_Kind_Void]) {
                            ERROR(lhs->exprs[i]->token->pos, "Due to inference, this variables type would be 'void', which is not allowed.");
                        }

                        TRY_ASSIGN_TYPE_OR_FAIL(context, &lhs->exprs[i], right_type->Compound.types[i], binop->token);
                    }

                    lhs->type = type_build_compound_type(context, lhs);
                }

            } else {
                if (right_type == context->types.basic[Basic_Kind_Void]) {
                    ERROR(binop->left->token->pos, "Due to inference, this variables type would be 'void', which is not allowed.");
                }

                binop->left->type = right_type;
            }
        }

    } else {
        // NOTE: +=, -=, ...
        // NOTE: At this point, it is assumed that operator overloads for +=, -=, etc have been tested.

        BinaryOp operation = -1;
        if      (binop->operation == Binary_Op_Assign_Add)      operation = Binary_Op_Add;
        else if (binop->operation == Binary_Op_Assign_Minus)    operation = Binary_Op_Minus;
        else if (binop->operation == Binary_Op_Assign_Multiply) operation = Binary_Op_Multiply;
        else if (binop->operation == Binary_Op_Assign_Divide)   operation = Binary_Op_Divide;
        else if (binop->operation == Binary_Op_Assign_Modulus)  operation = Binary_Op_Modulus;
        else if (binop->operation == Binary_Op_Assign_And)      operation = Binary_Op_And;
        else if (binop->operation == Binary_Op_Assign_Or)       operation = Binary_Op_Or;
        else if (binop->operation == Binary_Op_Assign_Xor)      operation = Binary_Op_Xor;
        else if (binop->operation == Binary_Op_Assign_Shl)      operation = Binary_Op_Shl;
        else if (binop->operation == Binary_Op_Assign_Shr)      operation = Binary_Op_Shr;
        else if (binop->operation == Binary_Op_Assign_Sar)      operation = Binary_Op_Sar;

        AstBinaryOp* new_right = make_binary_op(context, operation, binop->left, binop->right);
        binop->right = (AstTyped *) new_right;
        new_right->token = binop->token;
        binop->operation = Binary_Op_Assign;

        CHECK(binaryop, (AstBinaryOp **) &binop->right);
    }

    if (binop->right->type == NULL) {
        if (binop->right->entity != NULL && binop->right->entity->state <= Entity_State_Check_Types) {
            YIELD(binop->token->pos, "Trying to resolve type of right hand side.");
        }
    }

    TYPE_CHECK(&binop->right, binop->left->type) {
        ERROR_(binop->token->pos,
                "Cannot assign value of type '%s' to a '%s'.",
                node_get_type_name(context, binop->right),
                node_get_type_name(context, binop->left));
    }

    binop->type = context->types.basic[Basic_Kind_Void];

    return Check_Success;
}

static b32 binary_op_is_allowed(BinaryOp operation, Type* type) {
    static const u8 binop_allowed[Binary_Op_Count] = {
        /* Add */             Basic_Flag_Numeric | Basic_Flag_Multi_Pointer,
        /* Minus */           Basic_Flag_Numeric | Basic_Flag_Multi_Pointer,
        /* Multiply */        Basic_Flag_Numeric,
        /* Divide */          Basic_Flag_Numeric,
        /* Modulus */         Basic_Flag_Integer,

        /* Equal */           Basic_Flag_Equality,
        /* Not_Equal */       Basic_Flag_Equality,
        /* Less */            Basic_Flag_Ordered,
        /* Less_Equal */      Basic_Flag_Ordered,
        /* Greater */         Basic_Flag_Ordered,
        /* Greater_Equal */   Basic_Flag_Ordered,

        /* And */             Basic_Flag_Integer,
        /* Or */              Basic_Flag_Integer,
        /* Xor */             Basic_Flag_Integer,
        /* Shl */             Basic_Flag_Integer,
        /* Shr */             Basic_Flag_Integer,
        /* Sar */             Basic_Flag_Integer,

        /* Bool_And */        Basic_Flag_Boolean,
        /* Bool_Or */         Basic_Flag_Boolean,

        /* Assign_Start */    0,
        /* Assign */          0,
        /* Assign_Add */      0,
        /* Assign_Minus */    0,
        /* Assign_Multiply */ 0,
        /* Assign_Divide */   0,
        /* Assign_Modulus */  0,
        /* Assign_And */      0,
        /* Assign_Or */       0,
        /* Assign_Xor */      0,
        /* Assign_Shl */      0,
        /* Assign_Shr */      0,
        /* Assign_Sar */      0,
        /* Assign_End */      0,

        /* Pipe */            0,
        /* Range */           0,
    };

    enum BasicFlag effective_flags = 0;
    switch (type->kind) {
        case Type_Kind_Basic:        effective_flags = type->Basic.flags;  break;
        case Type_Kind_Pointer:      effective_flags = Basic_Flag_Pointer; break;
        case Type_Kind_MultiPointer: effective_flags = Basic_Flag_Multi_Pointer; break;
        case Type_Kind_Enum:         effective_flags = Basic_Flag_Integer; break;
        case Type_Kind_Function:     effective_flags = Basic_Flag_Equality; break;

        default: break;
    }

    return (binop_allowed[operation] & effective_flags) != 0;
}

CHECK_FUNC(binaryop_compare, AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    // HACK: Since ^... to rawptr is a one way conversion, strip any pointers
    // away so they can be compared as expected
    Type* ltype = binop->left->type;
    Type* rtype = binop->right->type;

    if (ltype == NULL) YIELD(binop->token->pos, "Waiting for left-type to be known.");
    if (rtype == NULL) YIELD(binop->token->pos, "Waiting for right-type to be known.");

    if (ltype->kind == Type_Kind_Pointer) ltype = context->types.basic[Basic_Kind_Rawptr];
    if (rtype->kind == Type_Kind_Pointer) rtype = context->types.basic[Basic_Kind_Rawptr];

    if (!types_are_compatible(context, ltype, rtype)) {
        b32 left_ac  = node_is_auto_cast((AstNode *) binop->left);
        b32 right_ac = node_is_auto_cast((AstNode *) binop->right);
        if (left_ac && right_ac) ERROR(binop->token->pos, "Cannot have auto cast on both sides of binary operator.");

        TYPE_CHECK(&binop->left, rtype) {
            TYPE_CHECK(&binop->right, ltype) {
                ERROR_(binop->token->pos,
                        "Cannot compare '%s' to '%s'.",
                        type_get_name(context, binop->left->type),
                        type_get_name(context, binop->right->type));
            }
        }
    }

    if (!binary_op_is_allowed(binop->operation, binop->left->type)) {
        report_bad_binaryop(context, binop);
        return Check_Error;
    }

    binop->type = context->types.basic[Basic_Kind_Bool];
    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(context, (AstTyped *) binop);
    }

    return Check_Success;
}

CHECK_FUNC(binaryop_bool, AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    b32 left_is_bool = 0;
    b32 right_is_bool = 0;

    if (type_is_bool(binop->left->type)) {
        left_is_bool = 1;
    } else {
        TypeMatch implicit_cast = implicit_cast_to_bool(context, &binop->left);
        if (implicit_cast == TYPE_MATCH_YIELD) YIELD(binop->token->pos, "Waiting for implicit cast to bool to check.");
        if (implicit_cast == TYPE_MATCH_SUCCESS) {
            left_is_bool = 1;
        }
    }

    if (type_is_bool(binop->right->type)) {
        right_is_bool = 1;
    } else {
        TypeMatch implicit_cast = implicit_cast_to_bool(context, &binop->right);
        if (implicit_cast == TYPE_MATCH_YIELD) YIELD(binop->token->pos, "Waiting for implicit cast to bool to check.");
        if (implicit_cast == TYPE_MATCH_SUCCESS) {
            right_is_bool = 1;
        }
    }

    if (!left_is_bool || !right_is_bool) {
        report_bad_binaryop(context, binop);
        return Check_Error;
    }

    binop->type = context->types.basic[Basic_Kind_Bool];

    if (binop->flags & Ast_Flag_Comptime) {
        // NOTE: Not a binary op
        *pbinop = (AstBinaryOp *) ast_reduce(context, (AstTyped *) binop);
    }
    return Check_Success;
}

static inline b32 type_is_not_basic_or_pointer(Type *t) {
    return (t != NULL
        && (t->kind != Type_Kind_Basic || (t->Basic.flags & Basic_Flag_SIMD) != 0)
        && (t->kind != Type_Kind_Pointer));
}

CHECK_FUNC(binaryop, AstBinaryOp** pbinop) {
    AstBinaryOp* binop = *pbinop;

    if (binop->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    if (binop->operation == Binary_Op_Assign && binop->left->kind == Ast_Kind_Subscript && bh_arr_length(context->operator_overloads[Binary_Op_Subscript_Equals]) > 0) {
        AstSubscript* sub = (AstSubscript *) binop->left;

        if (binop->potential_substitute == NULL) {
            u32 current_checking_level_store = context->checker.current_checking_level;
            CHECK(expression, &sub->addr);
            CHECK(expression, &sub->expr);
            CHECK(expression, &binop->right);
            context->checker.current_checking_level = current_checking_level_store;

            AstBinaryOp *op = onyx_ast_node_new(context->ast_alloc, sizeof(AstBinaryOp), Ast_Kind_Binary_Op);
            op->token = binop->token;
            op->operation = Binary_Op_Subscript_Equals;
            op->left  = ((AstSubscript *) binop->left)->addr;
            op->right = ((AstSubscript *) binop->left)->expr;

            binop->potential_substitute = op;
        }

        AstCall* call = binaryop_try_operator_overload(context, binop->potential_substitute, binop->right);
        if (call == (AstCall *) &context->node_that_signals_a_yield) YIELD(binop->token->pos, "Waiting on potential operator overload.");
        if (call != NULL) {
            call->next = binop->next;
            *(AstCall **) pbinop = call;

            CHECK(call, (AstCall **) pbinop);
            return Check_Success;
        }

    }

    u32 current_checking_level_store = context->checker.current_checking_level;
    CHECK(expression, &binop->left);
    CHECK(expression, &binop->right);
    context->checker.current_checking_level = current_checking_level_store;

    // :UnaryFieldAccessIsGross
    if (binop->left->kind == Ast_Kind_Unary_Field_Access || binop->right->kind == Ast_Kind_Unary_Field_Access) {
        TYPE_CHECK(&binop->left, binop->right->type) {
            TYPE_CHECK(&binop->right, binop->left->type) {
                // TODO: This should report a better error about the Unary_Field_Access not be able to be resolved given whatever type.
                //                                                                        - brendanfh 2021/12/31
                report_bad_binaryop(context, binop);
                return Check_Error;
            }
        }
    }

    if ((binop->left->flags & Ast_Flag_Comptime) && (binop->right->flags & Ast_Flag_Comptime)) {
        binop->flags |= Ast_Flag_Comptime;
    }

    if (context->checker.expression_types_must_be_known) {
        if (binop->left->type == NULL || binop->right->type == NULL) {
            ERROR(binop->token->pos, "Internal compiler error: one of the operands types is unknown here.");
        }
    }

    // NOTE: Try operator overloading before checking everything else.
    if (type_is_not_basic_or_pointer(binop->left->type) || type_is_not_basic_or_pointer(binop->right->type)) {
        u64 cache_key = 0;
        if (binop->left->type && binop->right->type) {
            if (!context->checker.__binop_impossible_cache[binop->operation].hashes) {
                bh_imap_init(&context->checker.__binop_impossible_cache[binop->operation], context->gp_alloc, 256);
            }

            cache_key = ((u64) (binop->left->type->id) << 32ll) | (u64) binop->right->type->id;

            if (bh_imap_has(&context->checker.__binop_impossible_cache[binop->operation], cache_key)) {
                goto definitely_not_op_overload;
            }
        }

        AstCall *implicit_call = binaryop_try_operator_overload(context, binop, NULL);

        if (implicit_call == (AstCall *) &context->node_that_signals_a_yield)
            YIELD(binop->token->pos, "Trying to resolve operator overload.");

        if (implicit_call != NULL && implicit_call != &context->checker.__op_maybe_overloaded) {
            // NOTE: Not a binary op
            implicit_call->next = binop->next;
            *pbinop = (AstBinaryOp *) implicit_call;

            CHECK(call, (AstCall **) pbinop);
            return Check_Success;
        }

        if (cache_key && implicit_call != &context->checker.__op_maybe_overloaded) {
            bh_imap_put(&context->checker.__binop_impossible_cache[binop->operation], cache_key, 1);
        }
    }

  definitely_not_op_overload:

    if (binop_is_assignment(binop->operation)) return check_binaryop_assignment(context, pbinop);

    if (binop->left->type == NULL && binop->left->entity && binop->left->entity->state <= Entity_State_Check_Types) {
        YIELD(binop->left->token->pos, "Waiting for this type to be known");
    }
    if (binop->right->type == NULL && binop->right->entity && binop->right->entity->state <= Entity_State_Check_Types) {
        YIELD(binop->right->token->pos, "Waiting for this type to be known");
    }

    // NOTE: Comparision operators and boolean operators are handled separately.
    if (binop_is_compare(binop->operation))
        return check_binaryop_compare(context, pbinop);
    if (binop->operation == Binary_Op_Bool_And || binop->operation == Binary_Op_Bool_Or)
        return check_binaryop_bool(context, pbinop);

    // NOTE: The left side cannot be rawptr.
    if (type_is_rawptr(binop->left->type)) {
        ERROR(binop->token->pos, "Cannot operate on a 'rawptr'. Cast it to a another pointer type first.");
    }

    // NOTE: Handle basic pointer math.
    if (type_is_multi_pointer(binop->left->type)) {
        if (binop->operation != Binary_Op_Add && binop->operation != Binary_Op_Minus) goto bad_binaryop;

        resolve_expression_type(context, binop->right);
        if (!type_is_integer(binop->right->type)) goto bad_binaryop;

        AstNumLit* numlit = make_int_literal(context, type_size_of(binop->left->type->MultiPointer.elem));
        numlit->token = binop->right->token;
        numlit->type = binop->right->type;

        AstBinaryOp* binop_node = make_binary_op(context, Binary_Op_Multiply, binop->right, (AstTyped *) numlit);
        binop_node->token = binop->token;
        CHECK(binaryop, &binop_node);

        binop->right = (AstTyped *) binop_node;
        binop->type = binop->left->type;
        binop->right->type = binop->left->type;
    }

    if (!types_are_compatible(context, binop->left->type, binop->right->type)) {
        b32 left_ac  = node_is_auto_cast((AstNode *) binop->left);
        b32 right_ac = node_is_auto_cast((AstNode *) binop->right);
        if (left_ac && right_ac) {
            ERROR(binop->token->pos, "Cannot have auto cast on both sides of binary operator.");
        }

        TYPE_CHECK(&binop->left, binop->right->type) {
            TYPE_CHECK(&binop->right, binop->left->type) {
                ERROR_(binop->token->pos,
                        "Binary operation '%s' not understood for types '%s' and '%s'.",
                        binaryop_string[binop->operation],
                        node_get_type_name(context, binop->left),
                        node_get_type_name(context, binop->right));
            }
        }
    }

    binop->type = binop->left->type;
    if (!binary_op_is_allowed(binop->operation, binop->type)) goto bad_binaryop;

    // NOTE: Enum flags with '&' result in a boolean value
    if (binop->type->kind == Type_Kind_Enum && binop->type->Enum.is_flags && binop->operation == Binary_Op_And) {
         binop->type = context->types.basic[Basic_Kind_Bool];
    }

    if (context->checker.all_checks_are_final) {
        binop->flags |= Ast_Flag_Has_Been_Checked;

        if (binop->flags & Ast_Flag_Comptime) {
            // NOTE: Not a binary op
            *pbinop = (AstBinaryOp *) ast_reduce(context, (AstTyped *) binop);
        }
    }

    return Check_Success;

bad_binaryop:
    report_bad_binaryop(context, binop);

    return Check_Error;
}

CHECK_FUNC(unaryop, AstUnaryOp** punop) {
    AstUnaryOp* unaryop = *punop;

    CHECK(expression, &unaryop->expr);

    if (unaryop->operation != Unary_Op_Negate) {
        resolve_expression_type(context, unaryop->expr);
    }

    if (unaryop->operation == Unary_Op_Cast) {
        CHECK(type, &unaryop->type_node);

        char* err;
        if (unaryop->type == NULL)
            YIELD(unaryop->token->pos, "Trying to resolve destination type for cast.");

        if (!cast_is_legal(context, unaryop->expr->type, unaryop->type, &err)) {
            ERROR_(unaryop->token->pos, "Cast Error: %s", err);
        }
    }

    if (unaryop->operation == Unary_Op_Not) {
        if (!type_is_bool(unaryop->expr->type)) {
            TypeMatch implicit_cast = implicit_cast_to_bool(context, &unaryop->expr);
            if (implicit_cast == TYPE_MATCH_YIELD) YIELD(unaryop->token->pos, "Waiting for implicit cast to bool to check.");
            if (implicit_cast == TYPE_MATCH_FAILED) {
                ERROR_(unaryop->token->pos,
                        "Bool negation operator expected bool type, got '%s'.",
                        node_get_type_name(context, unaryop->expr));
            }
        }
    }

    if (unaryop->operation == Unary_Op_Bitwise_Not) {
        if (!type_is_integer(unaryop->expr->type)) {
            ERROR_(unaryop->token->pos,
                    "Bitwise operator expected integer type, got '%s'.",
                    node_get_type_name(context, unaryop->expr));
        }
    }

    if (unaryop->operation != Unary_Op_Cast) {
        unaryop->type = unaryop->expr->type;
    }

    if (unaryop->operation == Unary_Op_Try || unaryop->operation == Unary_Op_Unwrap) {
        AstCall* call = unaryop_try_operator_overload(context, unaryop);
        if (call == (AstCall *) &context->node_that_signals_a_yield) YIELD(unaryop->token->pos, "Waiting on potential operator overload.");
        if (call != NULL && call != &context->checker.__op_maybe_overloaded) {
            call->next = unaryop->next;
            *(AstCall **) punop = call;

            CHECK(call, (AstCall **) punop);
            return Check_Success;
        }

        if (unaryop->operation == Unary_Op_Try)
            ERROR_(unaryop->token->pos, "'%s' does not support '?' operator.", type_get_name(context, unaryop->expr->type));

        if (unaryop->operation == Unary_Op_Unwrap)
            ERROR_(unaryop->token->pos, "'%s' does not support '!' operator.", type_get_name(context, unaryop->expr->type));
    }


    if (unaryop->expr->flags & Ast_Flag_Comptime) {
        unaryop->flags |= Ast_Flag_Comptime;
        // NOTE: Not a unary op
        *punop = (AstUnaryOp *) ast_reduce(context, (AstTyped *) unaryop);
    }

    return Check_Success;
}

CHECK_FUNC(struct_literal, AstStructLiteral* sl) {
    if (sl->stnode) {
        CHECK(expression, &sl->stnode);
    }

    sl->type_node = (AstType *) sl->stnode;
    while (sl->type_node && sl->type_node->kind == Ast_Kind_Type_Alias)
        sl->type_node = ((AstTypeAlias *) sl->type_node)->to;

    if (sl->extension_value) {
        CHECK(expression, &sl->extension_value);

        // Use the type of the extension value if no type of the structure literal was given.
        if (!sl->type && sl->extension_value->type) {
            sl->type = sl->extension_value->type;
        }
    }

    if (sl->type == NULL) {
        // NOTE: This is used for automatically typed struct literals. If there is no provided
        // type for the struct literal, assume that it is passes successfully. When it is used
        // elsewhere, it will be added as an expression entity that will be processed once the
        // stnode is filled out.
        if (sl->stnode == NULL) {
            CHECK(arguments, &sl->args);

            return Check_Success;
        }

        CHECK(expression, &sl->stnode);
        if (!node_is_type((AstNode *) sl->stnode)) {
            ERROR(sl->token->pos, "Type used for struct literal is not a type.");
        }

        sl->type = type_build_from_ast(context, (AstType *) sl->stnode);
        if (sl->type == NULL)
            YIELD(sl->token->pos, "Trying to resolve type of struct literal.");
    }

    if (sl->values_to_initialize == NULL) {
        bh_arr_new(context->gp_alloc, sl->values_to_initialize, 2);
    }

    //
    // Union literal are internally structure literals with a single named member.
    if (sl->type->kind == Type_Kind_Union) {
        if ((sl->flags & Ast_Flag_Has_Been_Checked) != 0) return Check_Success;
        
        if (sl->extension_value) {
            ERROR_(sl->token->pos, "Cannot use field-update syntax on '%s' because it is a 'union'.", type_get_name(context, sl->type));
        }

        Type *union_type = sl->type;

        if (bh_arr_length(sl->args.values) == 0 && bh_arr_length(sl->args.named_values) == 0) {
            // Produce an empty value of the first union type.
            UnionVariant *uv = union_type->Union.variants[0].value;

            AstNumLit *tag_value = make_int_literal(context, uv->tag_value);
            tag_value->type = union_type->Union.tag_type;

            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) {
                (AstTyped *) tag_value,
                0
            }));

            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) {
                (AstTyped *) make_zero_value(context, sl->token, uv->type),
                union_type->Union.alignment
            }));

            sl->flags |= Ast_Flag_Has_Been_Checked;
            return Check_Success;
        }

        if (bh_arr_length(sl->args.values) != 0 || bh_arr_length(sl->args.named_values) != 1) {
            ERROR_(sl->token->pos, "Expected exactly one named member when constructing an instance of a union type, '%s'.", type_get_name(context, sl->type));
        }

        AstNamedValue* value = sl->args.named_values[0];
        token_toggle_end(value->token);

        UnionVariant *matched_variant = union_type->Union.variants[
            shgeti(union_type->Union.variants, value->token->text)
        ].value;
        token_toggle_end(value->token);

        if (!matched_variant) {
            ERROR_(value->token->pos, "'%b' is not a variant of '%s'.",
                    value->token->text, value->token->length, type_get_name(context, union_type));
        }

        CHECK(expression, &value->value);

        TYPE_CHECK(&value->value, matched_variant->type) {
            ERROR_(value->token->pos,
                   "Mismatched type in initialized type. Expected something of type '%s', got '%s'.",
                   type_get_name(context, matched_variant->type),
                   type_get_name(context, value->value->type));
        }

        AstNumLit *tag_value = make_int_literal(context, matched_variant->tag_value);
        tag_value->type = union_type->Union.tag_type;

        bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) {
            (AstTyped *) tag_value,
            0
        }));

        bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) {
            value->value,
            union_type->Union.alignment
        }));

        sl->flags |= Ast_Flag_Has_Been_Checked;
        return Check_Success;
    }

    if (sl->type->kind == Type_Kind_Array) {
        if (bh_arr_length(sl->args.named_values) > 0) {
            ERROR_(sl->token->pos, "Cannot specify named values when creating a '%s'.", type_get_name(context, sl->type));
        }

        u32 value_count = bh_arr_length(sl->args.values);
        if (value_count == 0) {
            AstZeroValue *zv = make_zero_value(context, sl->token, sl->type);
            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) { (AstTyped *) zv, 0 }));

            sl->flags |= Ast_Flag_Has_Been_Checked;
            return Check_Success;
        }

        if (value_count != sl->type->Array.count) {
            ERROR_(sl->token->pos,
                    "Expected exactly '%d' values when constructing a '%s', but got '%d' value%s.",
                    sl->type->Array.count,
                    type_get_name(context, sl->type),
                    value_count,
                    bh_num_plural(value_count));
        }

        Type* type_to_match = sl->type->Array.elem;
        i32 offset = 0;
        bh_arr_each(AstTyped *, pval, sl->args.values) {
            CHECK(expression, pval);

            TYPE_CHECK(pval, type_to_match) {
                ERROR_(sl->token->pos,
                       "Mismatched type. Expected something of type '%s', got '%s'.",
                       type_get_name(context, type_to_match),
                       type_get_name(context, (*pval)->type));
            }

            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) { *pval, offset }));
            offset += type_size_of(type_to_match);
            bh_align(offset, type_alignment_of(type_to_match));
        }

        return Check_Success;
    }


    if (!type_is_structlike_strict(sl->type)) {
        if ((sl->flags & Ast_Flag_Has_Been_Checked) != 0) return Check_Success;

        //
        // If there are no given arguments to a structure literal, it is treated as a 'zero-value',
        // and can be used to create a completely zeroed value of any type.
        if (bh_arr_length(sl->args.values) == 0 && bh_arr_length(sl->args.named_values) == 0) {
            AstZeroValue *zv = make_zero_value(context, sl->token, sl->type);
            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) { (AstTyped *) zv, 0 }));

            sl->flags |= Ast_Flag_Has_Been_Checked;
            return Check_Success;
        }

        if (bh_arr_length(sl->args.values) == 1) {
            CHECK(expression, &sl->args.values[0]);

            Type* type_to_match = sl->type;
            if (sl->type->kind == Type_Kind_Distinct) {
                type_to_match = sl->type->Distinct.base_type;
            }

            TYPE_CHECK(&sl->args.values[0], type_to_match) {
                ERROR_(sl->token->pos,
                       "Mismatched type in initialized type. Expected something of type '%s', got '%s'.",
                       type_get_name(context, type_to_match),
                       type_get_name(context, sl->args.values[0]->type));
            }

            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) { sl->args.values[0], 0 }));

            sl->flags |= Ast_Flag_Has_Been_Checked;
            return Check_Success;
        }

        //
        // Otherwise, it is not possible to construct the type if it is not a structure.
        ERROR_(sl->token->pos,
                "'%s' is not constructable using a struct literal.",
                type_get_name(context, sl->type));
    }

    bh_arr_clear(sl->values_to_initialize);

    if (sl->extension_value) {
        TYPE_CHECK(&sl->extension_value, sl->type) {
            ERROR_(sl->token->pos, "Expected base value for field-update to be of type '%s', but it was '%s' instead.",
                type_get_name(context, sl->type), type_get_name(context, sl->extension_value->type));
        }

        bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) { sl->extension_value, 0 }));
    }

    i32 mem_count = type_structlike_mem_count(sl->type);
    arguments_ensure_length(context, &sl->args, mem_count);

    // :Idempotency
    if ((sl->flags & Ast_Flag_Has_Been_Checked) == 0) {
        char* err_msg = NULL;
        if (!fill_in_arguments(context, &sl->args, (AstNode *) sl, &err_msg, 1)) {
            ONYX_ERROR(sl->token->pos, Error_Critical, err_msg);

            // bh_arr_each(AstTyped *, value, sl->args.values) {
            //     if (*value == NULL) {
            //         i32 member_idx = value - sl->args.values; // Pointer subtraction hack
            //         StructMember smem;
            //         type_lookup_member_by_idx(sl->type, member_idx, &smem);

            //         ONYX_ERROR(sl->token->pos, Error_Critical,
            //             "Value not given for %d%s member, '%s', for type '%s'.",
            //             member_idx + 1, bh_num_suffix(member_idx + 1),
            //             smem.name, type_get_name(context, sl->type));
            //     }
            // }

            return Check_Error;
        }
    }
    sl->flags |= Ast_Flag_Has_Been_Checked;

    if (!type_is_ready_for_lookup(sl->type)) {
        YIELD(sl->token->pos, "Waiting for structure type to be ready.");
    }

    AstTyped** actual = sl->args.values;
    StructMember smem;

    // BUG: There are problems setting the comptime flag this late in the checking because
    // if the struct literal was type inferred, then the literal won't be correctly determined
    // to be comptime on the first pass, which is needed for top level expressions.
    sl->flags |= Ast_Flag_Comptime;

    fori (i, 0, mem_count) {
        // NOTE: Not checking the return on this function because
        // this for loop is bounded by the number of members in the
        // type.
        type_lookup_member_by_idx(context, sl->type, i, &smem);
        Type* formal = smem.type;

        CHECK(expression, actual);
        if ((*actual)->type == NULL && (*actual)->entity != NULL && (*actual)->entity->state <= Entity_State_Check_Types) {
            YIELD((*actual)->token->pos, "Trying to resolve type of expression for member.");
        }

        TYPE_CHECK(actual, formal) {
            ERROR_(sl->token->pos,
                    "Mismatched types for %d%s member named '%s', expected '%s', got '%s'.",
                    i + 1, bh_num_suffix(i + 1),
                    smem.name,
                    type_get_name(context, formal),
                    node_get_type_name(context, *actual));
        }

        if (!sl->extension_value || (*actual)->kind != Ast_Kind_Zero_Value) {
            bh_arr_push(sl->values_to_initialize, ((ValueWithOffset) { *actual, smem.offset }));
        }

        sl->flags &= ((*actual)->flags & Ast_Flag_Comptime) | (sl->flags &~ Ast_Flag_Comptime);
        actual++;
    }

    return Check_Success;
}

CHECK_FUNC(array_literal, AstArrayLiteral* al) {
    if (al->atnode) {
        CHECK(expression, &al->atnode);
    }

    al->type_node = (AstType *) al->atnode;
    while (al->type_node && al->type_node->kind == Ast_Kind_Type_Alias)
        al->type_node = ((AstTypeAlias *) al->type_node)->to;

    bh_arr_each(AstTyped *, expr, al->values) {
        CHECK(expression, expr);
    }

    // :Idempotency
    if ((al->flags & Ast_Flag_Array_Literal_Typed) == 0) {
        if (al->atnode == NULL) return Check_Success;
            // YIELD(al->token->pos, "Waiting for array literal type to be known.");

        CHECK(expression, &al->atnode);
        if (!node_is_type((AstNode *) al->atnode))
            ERROR(al->token->pos, "Array type is not a type.");

        al->type = type_build_from_ast(context, (AstType *) al->atnode);
        if (al->type == NULL)
            YIELD(al->token->pos, "Trying to resolve type of array literal.");

        al->type = type_make_array(context, al->type, bh_arr_length(al->values));
        if (al->type == NULL || al->type->kind != Type_Kind_Array)
            ERROR(al->token->pos, "Expected array type for array literal. This is a compiler bug.");

        al->flags |= Ast_Flag_Array_Literal_Typed;
    }

    if (al->type->Array.count != (u32) bh_arr_length(al->values)) {
        ERROR_(al->token->pos, "Wrong number of values provided in array literal. Wanted %d values, got %d values.",
            al->type->Array.count, bh_arr_length(al->values));
    }

    al->flags |= Ast_Flag_Comptime;
    assert(al->type->kind == Type_Kind_Array);

    Type* elem_type = al->type->Array.elem;
    bh_arr_each(AstTyped *, expr, al->values) {
        // HACK HACK HACK
        if ((*expr)->type == NULL &&
            (*expr)->entity != NULL &&
            (*expr)->entity->state <= Entity_State_Check_Types) {
            YIELD_(al->token->pos, "Trying to resolve type of %d%s element of array literal.", expr - al->values, bh_num_suffix(expr - al->values));
        }

        al->flags &= ((*expr)->flags & Ast_Flag_Comptime) | (al->flags &~ Ast_Flag_Comptime);

        TYPE_CHECK(expr, elem_type) {
            ERROR_((*expr)->token->pos, "Mismatched types for value in array. Expected something of type '%s', got '%s' instead.",
                type_get_name(context, elem_type),
                node_get_type_name(context, *expr));
        }
    }

    return Check_Success;
}

CHECK_FUNC(range_literal, AstRangeLiteral** prange) {
    AstRangeLiteral* range = *prange;
    if (range->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    CHECK(expression, &range->low);
    CHECK(expression, &range->high);

    // HACK HACK These should already be checked but they might node be!
    CHECK(type, &context->builtins.range_type);
    CHECK(type, &context->builtins.range64_type);

    context->builtins.range_type_type = type_build_from_ast(context, context->builtins.range_type);
    context->builtins.range64_type_type = type_build_from_ast(context, context->builtins.range64_type);
    if (context->builtins.range_type_type   == NULL) YIELD(range->token->pos, "Waiting for 'range' structure to be built.");
    if (context->builtins.range64_type_type == NULL) YIELD(range->token->pos, "Waiting for 'range64' structure to be built.");

    Type* expected_range_type = NULL;
    TYPE_QUERY(&range->low, context->types.basic[Basic_Kind_I32]) {
        TYPE_QUERY(&range->high, context->types.basic[Basic_Kind_I32]) {
            expected_range_type = context->builtins.range_type_type;
        }
    }

    if (expected_range_type == NULL) {
        TYPE_QUERY(&range->low, context->types.basic[Basic_Kind_I64]) {
            TYPE_QUERY(&range->high, context->types.basic[Basic_Kind_I64]) {
                expected_range_type = context->builtins.range64_type_type;
            }
        }
    }

    if (expected_range_type == NULL) {
        ERROR_(range->token->pos, "Range operator '..' not understood for types '%s' and '%s'.",
               node_get_type_name(context, range->low), node_get_type_name(context, range->high));
    }

    StructMember smem;

    type_lookup_member(context, expected_range_type, "low", &smem);
    TYPE_CHECK(&range->low, smem.type) {
        ERROR_(range->token->pos,
            "Expected left side of range to be a '%s', got '%s'.",
            type_get_name(context, smem.type), node_get_type_name(context, range->low));
    }

    type_lookup_member(context, expected_range_type, "high", &smem);
    TYPE_CHECK(&range->high, smem.type) {
        ERROR_(range->token->pos,
            "Expected left side of range to be a '%s', got '%s'.",
            type_get_name(context, smem.type), node_get_type_name(context, range->high));
    }

    if (range->step == NULL) {
        type_lookup_member(context, expected_range_type, "step", &smem);
        assert(smem.initial_value != NULL);
        CHECK(expression, smem.initial_value);

        range->step = *smem.initial_value;
    }

    if (range->inclusive) {
        AstTyped *one = (AstTyped *) make_int_literal(context, 1);
        one->type = smem.type;

        range->high = (AstTyped *) make_binary_op(context, Binary_Op_Add, range->high, one);

        CHECK(binaryop, (AstBinaryOp **) &range->high);
    }

    range->flags |= Ast_Flag_Has_Been_Checked;
    range->type = expected_range_type;
    return Check_Success;
}

CHECK_FUNC(compound, AstCompound* compound) {
    bh_arr_each(AstTyped *, expr, compound->exprs) {
        CHECK(expression, expr);
    }

    compound->type = type_build_compound_type(context, compound);
    return Check_Success;
}

CHECK_FUNC(if_expression, AstIfExpression* if_expr) {
    CHECK(expression, &if_expr->cond);
    CHECK(expression, &if_expr->true_expr);
    CHECK(expression, &if_expr->false_expr);

    TYPE_CHECK(&if_expr->cond, context->types.basic[Basic_Kind_Bool]) {
        TypeMatch implicit_cast = implicit_cast_to_bool(context, &if_expr->cond);
        if (implicit_cast == TYPE_MATCH_YIELD) YIELD(if_expr->token->pos, "Waiting for implicit cast to bool to check.");
        if (implicit_cast == TYPE_MATCH_FAILED) {
            ERROR_(if_expr->token->pos, "If-expression expected boolean for condition, got '%s'.",
                type_get_name(context, if_expr->cond->type));
        }
    }

    resolve_expression_type(context, (AstTyped *) if_expr);

    if (!types_are_compatible(context, if_expr->true_expr->type, if_expr->false_expr->type)) {
        ERROR_(if_expr->token->pos, "Mismatched types for if-expression, left side is '%s', and right side is '%s'.",
            type_get_name(context, if_expr->true_expr->type), type_get_name(context, if_expr->false_expr->type));
    }

    return Check_Success;
}

CHECK_FUNC(pipe, AstBinaryOp** ppipe) {
    AstBinaryOp *pipe = *ppipe;

    //
    // Handle x |> y()? or x |> y()!
    if (pipe->right->kind == Ast_Kind_Unary_Op) {
        AstUnaryOp *the_try = (AstUnaryOp *) pipe->right;
        if (the_try->operation == Unary_Op_Try || the_try->operation == Unary_Op_Unwrap) {
            // Shuffle the tree!
            AstBinaryOp *the_pipe = pipe;

            the_pipe->right = the_try->expr;
            the_try->expr   = (AstTyped *) the_pipe;
            *ppipe          = (AstBinaryOp *) the_try;

            CHECK(expression, (AstTyped **) ppipe);
            return Check_Success;
        }
    }

    AstCall* base_call_node = (AstCall *) pipe->right;
    AstCall* call_node = base_call_node;

    if (call_node->kind == Ast_Kind_Method_Call) {
        call_node = (AstCall *) ((AstBinaryOp *) call_node)->right;
    }

    //
    // Handle x |> y === x |> y() === y(x)
    if (call_node->kind != Ast_Kind_Call) {
        AstCall *new_call = onyx_ast_node_new(context->ast_alloc, sizeof(AstCall), Ast_Kind_Call);
        new_call->token = call_node->token;
        new_call->callee = (AstTyped *) call_node;
        arguments_initialize(context, &new_call->args);

        pipe->right = (AstTyped *) new_call;
        base_call_node = new_call;
        call_node = new_call;
    }

    if (call_node->callee->kind == Ast_Kind_Unary_Field_Access) {
        AstAlias *left_alias = onyx_ast_node_new(context->ast_alloc, sizeof(AstAlias), Ast_Kind_Alias);
        left_alias->token = pipe->left->token;
        left_alias->alias = pipe->left;
        pipe->left = (AstTyped *) left_alias;

        AstFieldAccess *implicit_field_access = make_field_access(context, (AstTyped *) left_alias, NULL);
        implicit_field_access->token = call_node->callee->token;

        call_node->callee = (AstTyped *) implicit_field_access;

        AstAddressOf *address_of = make_address_of(context, pipe->left);
        address_of->can_be_removed = 1;
        pipe->left = (AstTyped *) address_of;
    }

    if (!call_node || call_node->kind != Ast_Kind_Call) {
        ERROR(pipe->token->pos, "Pipe operator expected call on right side.");
    }

    // CLEANUP: Why is this here?
    if (pipe->left == NULL) return Check_Error;

    if (call_node->placeholder_argument_position > 0) {
        assert(call_node->placeholder_argument_position - 1 < bh_arr_length(call_node->args.values));
        call_node->args.values[call_node->placeholder_argument_position - 1] = (AstTyped *) make_argument(context, pipe->left);
        call_node->placeholder_argument_position = 0;

    } else {
        bh_arr_insertn(call_node->args.values, 0, 1);
        call_node->args.values[0] = (AstTyped *) make_argument(context, pipe->left);
    }

    base_call_node->next = pipe->next;
    *ppipe = (AstBinaryOp *) base_call_node;

    CHECK(expression, (AstTyped **) ppipe);
    return Check_Success;
}

CHECK_FUNC(do_block, AstDoBlock** pdoblock) {
    AstDoBlock* doblock = *pdoblock;
    if (doblock->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    fill_in_type(context, (AstTyped *) doblock);

    bh_arr_push(context->checker.expected_return_type_stack, &doblock->type);
    bh_arr_push(context->checker.named_return_values_stack, doblock->named_return_locals);

    doblock->block->rules = Block_Rule_Do_Block;

    CHECK(block, doblock->block);

    if (doblock->type == context->types.auto_return) doblock->type = context->types.basic[Basic_Kind_Void];

    bh_arr_pop(context->checker.expected_return_type_stack);
    bh_arr_pop(context->checker.named_return_values_stack);

    doblock->flags |= Ast_Flag_Has_Been_Checked;
    return Check_Success;
}

CHECK_FUNC(address_of, AstAddressOf** paof) {
    AstAddressOf* aof = *paof;

    AstTyped* expr = (AstTyped *) strip_aliases((AstNode *) aof->expr);
    if (expr->kind == Ast_Kind_Subscript && bh_arr_length(context->operator_overloads[Binary_Op_Ptr_Subscript]) > 0) {
        if (aof->potential_substitute == NULL) {
            CHECK(expression, &((AstSubscript *) expr)->addr);
            CHECK(expression, &((AstSubscript *) expr)->expr);

            AstBinaryOp *op = onyx_ast_node_new(context->ast_alloc, sizeof(AstBinaryOp), Ast_Kind_Binary_Op);
            op->operation = Binary_Op_Ptr_Subscript;
            op->left  = ((AstSubscript *) expr)->addr;
            op->right = ((AstSubscript *) expr)->expr;
            op->token = aof->token;

            aof->potential_substitute = op;
        }

        AstCall* call = binaryop_try_operator_overload(context, aof->potential_substitute, NULL);
        if (call == (AstCall *) &context->node_that_signals_a_yield) YIELD(aof->token->pos, "Waiting for operator overload to possibly resolve.");
        if (call != NULL) {
            call->next = aof->next;
            *(AstCall **) paof = call;

            CHECK(call, (AstCall **) paof);
            return Check_Success;
        }
    }

    if (node_is_type((AstNode *) expr)) {
        AstPointerType *pt = onyx_ast_node_new(context->ast_alloc, sizeof(AstPointerType), Ast_Kind_Pointer_Type);
        pt->token     = aof->token;
        pt->elem      = (AstType *) expr;
        pt->next      = aof->next;
        *paof         = (AstAddressOf *) pt;
        CHECK(type, (AstType **) &pt);
        return Check_Success;
    }

    CHECK(expression, &aof->expr);

    if (node_is_addressable_literal((AstNode *) aof->expr)) {
        resolve_expression_type(context, aof->expr);
    }

    if (aof->expr->type == NULL) {
        YIELD(aof->token->pos, "Trying to resolve type of expression to take a reference.");
    }

    expr = (AstTyped *) strip_aliases((AstNode *) aof->expr);
    if (node_is_type((AstNode *) expr)) {
        AstPointerType *pt = onyx_ast_node_new(context->ast_alloc, sizeof(AstPointerType), Ast_Kind_Pointer_Type);
        pt->token     = aof->token;
        pt->elem      = (AstType *) expr;
        pt->next      = aof->next;
        *paof         = (AstAddressOf *) pt;
        CHECK(type, (AstType **) &pt);
        return Check_Success;
    }

    if ((expr->kind != Ast_Kind_Subscript
            && expr->kind != Ast_Kind_Dereference
            && expr->kind != Ast_Kind_Field_Access
            && expr->kind != Ast_Kind_Memres
            && expr->kind != Ast_Kind_Local
            && expr->kind != Ast_Kind_Capture_Local
            && expr->kind != Ast_Kind_Constraint_Sentinel
            && !node_is_addressable_literal((AstNode *) expr))
            || (expr->flags & Ast_Flag_Cannot_Take_Addr) != 0) {

        if (aof->can_be_removed) {
            *(AstTyped **) paof = aof->expr;
            return Check_Yield;
        }

        ERROR_(aof->token->pos, "Cannot take the address of something that is not an l-value. %s", onyx_ast_node_kind_string(expr->kind));
    }

    expr->flags |= Ast_Flag_Address_Taken;

    aof->type = type_make_pointer(context, expr->type);

    if (expr->kind == Ast_Kind_Memres && !((AstMemRes *) expr)->threadlocal) {
        aof->flags |= Ast_Flag_Comptime;
    }

    return Check_Success;
}

CHECK_FUNC(dereference, AstDereference* deref) {
    CHECK(expression, &deref->expr);

    if (!type_is_pointer(deref->expr->type))
        ERROR_(deref->token->pos, "Cannot dereference non-pointer value, '%s'.", type_get_name(context, deref->expr->type));

    if (deref->expr->type == context->types.basic[Basic_Kind_Rawptr])
        ERROR(deref->token->pos, "Cannot dereference 'rawptr'. Cast to another pointer type first.");

    deref->type = deref->expr->type->Pointer.elem;

    return Check_Success;
}

CHECK_FUNC(subscript, AstSubscript** psub) {
    AstSubscript* sub = *psub;
    CHECK(expression, &sub->addr);
    CHECK(expression, &sub->expr);

    if (sub->addr->type == NULL) YIELD(sub->token->pos, "Waiting to know type of left-hand side of subscript.");

    // NOTE: Try operator overloading before checking everything else.
    if (sub->expr->type != NULL &&
        (sub->addr->type->kind != Type_Kind_Basic || sub->expr->type->kind != Type_Kind_Basic)
        && !(type_is_array_accessible(sub->addr->type))) {
        // AstSubscript is the same as AstBinaryOp for the first sizeof(AstBinaryOp) bytes
        AstBinaryOp* binop = (AstBinaryOp *) sub;
        AstCall *implicit_call = binaryop_try_operator_overload(context, binop, NULL);

        if (implicit_call == (AstCall *) &context->node_that_signals_a_yield)
            YIELD(sub->token->pos, "Trying to resolve operator overload.");

        if (implicit_call != NULL) {
            // NOTE: Not an array access
            implicit_call->next = sub->next;
            *psub = (AstSubscript *) implicit_call;

            CHECK(call, (AstCall **) psub);
            return Check_Success;
        }
    }

    if (!type_is_array_accessible(sub->addr->type)) {
        report_bad_binaryop(context, (AstBinaryOp *) sub);
        return Check_Error;
    }

    if (sub->addr->type->kind == Type_Kind_Slice || sub->addr->type->kind == Type_Kind_DynArray || sub->addr->type->kind == Type_Kind_VarArgs) {
        // If we are accessing on a slice or a dynamic array, implicitly add a field access for the data member
        StructMember smem;
        type_lookup_member(context, sub->addr->type, "data", &smem);

        AstFieldAccess* fa = make_field_access(context, sub->addr, "data");
        fa->type   = smem.type;
        fa->offset = smem.offset;
        fa->idx    = smem.idx;

        sub->addr = (AstTyped *) fa;
    }

    if (types_are_compatible(context, sub->expr->type, context->builtins.range_type_type)) {
        Type *of = type_get_contained_type(sub->addr->type);
        if (of == NULL) {
            // FIXME: Slice creation should be allowed for slice types and dynamic array types, like it
            // is below, but this code doesn't look at that.
            report_bad_binaryop(context, (AstBinaryOp *) sub);
            ERROR(sub->token->pos, "Invalid type for left of slice creation.");
        }

        sub->kind = Ast_Kind_Slice;
        sub->type = type_make_slice(context, of);
        sub->elem_size = type_size_of(of);

        return Check_Success;
    }

    resolve_expression_type(context, sub->expr);
    if (!type_is_small_integer(sub->expr->type)) {
        report_bad_binaryop(context, (AstBinaryOp *) sub);
        ERROR_(sub->token->pos, "Expected small integer type for index, got '%s'.", node_get_type_name(context, sub->expr));
    }

    sub->type = type_get_contained_type(sub->addr->type);
    if (sub->type == NULL) {
        report_bad_binaryop(context, (AstBinaryOp *) sub);
        ERROR(sub->token->pos, "Invalid type for left of array access.");
    }

    sub->elem_size = type_size_of(sub->type);
    return Check_Success;
}

CHECK_FUNC(field_access, AstFieldAccess** pfield) {
    AstFieldAccess* field = *pfield;
    if (field->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    if (field->token != NULL && field->field == NULL) {
        token_toggle_end(field->token);
        field->field = bh_strdup(context->ast_alloc, field->token->text);
        token_toggle_end(field->token);
    }

    //
    // Here we "pre-check" the expression to resolve any symbols, but maybe
    // leave unknown types hanging around. This does not matter for any of
    // the cases that exist below. If we are not looking at one of the cases
    // below, we will "properly" check the expression type again. Not the 
    // fastest way of going about this, but I am aiming for correctness
    // at the moment.
    //
    //         - brendanfh 03 January 2025
    //
    check_expression(context, &field->expr);
    AstTyped *expr;
    {
        expr = (AstTyped *) strip_aliases((AstNode *) field->expr);
        while (expr->kind == Ast_Kind_Type_Alias) {
            expr = (AstTyped *)((AstTypeAlias *) expr)->to;
        }

        if (expr->kind == Ast_Kind_Struct_Type ||
            expr->kind == Ast_Kind_Poly_Struct_Type ||
            expr->kind == Ast_Kind_Enum_Type ||
            expr->kind == Ast_Kind_Type_Raw_Alias ||
            expr->kind == Ast_Kind_Union_Type ||
            expr->kind == Ast_Kind_Poly_Union_Type ||
            expr->kind == Ast_Kind_Slice_Type ||
            expr->kind == Ast_Kind_DynArr_Type ||
            expr->kind == Ast_Kind_Distinct_Type ||
            expr->kind == Ast_Kind_Interface ||
            expr->kind == Ast_Kind_Compiler_Extension ||
            expr->kind == Ast_Kind_Package ||
            expr->kind == Ast_Kind_Code_Block) {
            goto try_resolve_from_node;
        }
    }

    CHECK(expression, &field->expr);

    if (field->expr->type == NULL) {
        YIELD(field->token->pos, "Trying to resolve type of source expression.");
    }

    if (!type_is_structlike(field->expr->type)) {
        goto try_resolve_from_type;
    }

    if (!type_is_ready_for_lookup(field->expr->type)) {
        YIELD(field->token->pos, "Waiting for struct type to be completed before looking up members.");
    }

    StructMember smem;
    if (!type_lookup_member(context, field->expr->type, field->field, &smem)) {
        if (field->expr->type->kind == Type_Kind_Array) {
            u32 field_count = field->expr->type->Array.count;

            if (!strcmp(field->field, "count")) {
                *pfield = (AstFieldAccess *) make_int_literal(context, field_count);
                return Check_Success;
            }

            // This allows simple field access on fixed-size arrays.
            // Index 0-3 are mapped to x, y, z, w (vectors) or r, g, b, a (colors).
            if (field_count <= 4) {
                u32   index;
                b32   valid    = 0;
                char* accessor = field->field;

                // @todo(judah): this should be a small lookup table rather than multiple strcmps.
                     if (!strcmp(accessor, "x") || !strcmp(accessor, "r")) valid = field_count >= 1, index = 0;
                else if (!strcmp(accessor, "y") || !strcmp(accessor, "g")) valid = field_count >= 2, index = 1;
                else if (!strcmp(accessor, "z") || !strcmp(accessor, "b")) valid = field_count >= 3, index = 2;
                else if (!strcmp(accessor, "w") || !strcmp(accessor, "a")) valid = field_count >= 4, index = 3;

                if (valid) {
                    *pfield = make_field_access(context, field->expr, field->field);
                    (*pfield)->type   = field->expr->type->Array.elem;
                    (*pfield)->offset = index * type_size_of(field->expr->type->Array.elem);
                    (*pfield)->idx    = index;
                    (*pfield)->flags |= Ast_Flag_Has_Been_Checked;
                    return Check_Success;
                }
            }
        }

        if (type_union_get_variant_count(field->expr->type) > 0) {
            UnionVariant *uv = type_lookup_union_variant_by_name(field->expr->type, field->field);
            if (uv) {
                field->is_union_variant_access = 1;
                field->idx = uv->tag_value;

                // HACK make a function for this.
                if (!field->type_node) {
                    AstPolyCallType* pctype = onyx_ast_node_new(context->ast_alloc, sizeof(AstPolyCallType), Ast_Kind_Poly_Call_Type);
                    pctype->token = field->token;
                    pctype->callee = context->builtins.optional_type;
                    bh_arr_new(context->ast_alloc, pctype->params, 1);
                    bh_arr_push(pctype->params, (AstNode *) uv->type->ast_type);

                    field->type_node = (AstType *) pctype;
                }

                field->type = type_build_from_ast(context, field->type_node);
                if (!field->type) YIELD(field->token->pos, "Waiting for field access type to be constructed.");

                field->flags |= Ast_Flag_Has_Been_Checked;
                return Check_Success;
            }
        }

        goto try_resolve_from_type;
    }

    // NOTE: If this member was included into the structure through a "use x: ^T" kind of statement,
    // then we have to insert a intermediate field access in order to access the correct member.
    if (smem.use_through_pointer_index >= 0) {
        StructMember containing_member = smem;

        // TODO: The following code is not right after it loops, but this should never loop
        // due to a check in types.c line 947. When nested use-through-pointers are allowed,
        // thing will have to be reconsidered.
        AstTyped **dest = &field->expr;
        do {
            assert(type_lookup_member_by_idx(context, (*dest)->type, containing_member.use_through_pointer_index, &containing_member));

            AstFieldAccess *new_access = onyx_ast_node_new(context->ast_alloc, sizeof(AstFieldAccess), Ast_Kind_Field_Access);
            new_access->token = field->token;
            new_access->offset = containing_member.offset;
            new_access->idx = containing_member.idx;
            new_access->type = containing_member.type;
            new_access->expr = *dest;
            new_access->flags |= Ast_Flag_Has_Been_Checked;
            new_access->flags |= Ast_Flag_Extra_Field_Access;

            *dest = (AstTyped *) new_access;
            dest = &new_access->expr;
        } while (containing_member.use_through_pointer_index >= 0);
    }

    field->offset = smem.offset;
    field->idx = smem.idx;
    field->type = smem.type;
    field->flags |= Ast_Flag_Has_Been_Checked;

    track_resolution_for_symbol_info(context, (AstNode *) field, (AstNode *) smem.member_node);
    return Check_Success;

    // Field access is the general term for "a.b". In the early stages of the language,
    // a field access in the checker was only used for accessing a member on a struct.
    // However, as the language matured, I decided to add looking up things inside of
    // the static scope a struct, and then more and more places to look up symbol after
    // you know the type of the expression. The code below tries to lookup the symbol
    // within the type and/or node of the expression. On failure, it will yield, as
    // there might be an `#inject` that will add a symbol later. When a cycle is
    // detected however, it uses the levenschtein distance to find the closest symbol
    // to the attempted lookup.
    AstNode *n = NULL;
    AstType *type_node = NULL;

  try_resolve_from_type:
    type_node = field->expr->type->ast_type;

    n = try_symbol_raw_resolve_from_type(context, field->expr->type, field->field);
    if (n) goto resolved;

  try_resolve_from_node:
    type_node = NULL;
    n = try_symbol_raw_resolve_from_node(context, (AstNode *) field->expr, field->field);

  resolved:
    if (n) {
        track_resolution_for_symbol_info(context, (AstNode *) *pfield, n);

        *pfield = (AstFieldAccess *) n;
        CHECK(expression, (AstTyped **) pfield);
        return Check_Success;
    }

    //
    // This has to be cycle_almost_detected, not cycle_detected, because interface
    // constraints relay on Check_Error being returned, not Check_Yield. For
    // this reason, I have to produce an error at the last minute, BEFORE the loop
    // enters a cycle detected state, when there is no point of return.
    if (!context->cycle_almost_detected && !context->cycle_detected) {
        // Skipping the slightly expensive symbol lookup
        // below by not using YIELD_ERROR.
        return Check_Yield;
    }

    if (expr->kind == Ast_Kind_Package) {
        if (context->cycle_detected) {
            char *closest = find_closest_symbol_in_node(context, (AstNode *) expr, field->field);

            AstPackage *package = (AstPackage *) strip_aliases((AstNode *) field->expr);
            char *package_name = "unknown (compiler bug)";
            if (package && package->package) {
                package_name = package->package->name;
            }

            if (closest) {
                ERROR_(field->token->pos, "'%b' was not found in package '%s'. Did you mean '%s'?",
                    field->token->text,
                    field->token->length,
                    package_name,
                    closest);
            } else {
                ERROR_(field->token->pos, "'%b' was not found in package '%s'. Perhaps it is defined in a file that was not loaded?",
                    field->token->text,
                    field->token->length,
                    package_name);
            }
        }

        return Check_Yield;
    }

    if (context->cycle_detected || context->cycle_almost_detected >= 2) {
        ERROR_(field->token->pos, "'%b' does not exist here. This is a bad error message.",
            field->token->text,
            field->token->length);
    }

    char* type_name = (char *) node_get_type_name(context, field->expr);
    if (field->expr->type == context->types.basic[Basic_Kind_Type_Index]) {
        Type *actual_type = type_build_from_ast(context, (AstType *) field->expr);
        type_name = (char *) type_get_name(context, actual_type);
    }

    if (!type_node) goto closest_not_found;

    char* closest = find_closest_symbol_in_node(context, (AstNode *) type_node, field->field);
    if (closest) {
        ERROR_(field->token->pos, "Field '%s' does not exist on '%s'. Did you mean '%s'?", field->field, type_name, closest);
    }

  closest_not_found:
    ERROR_(field->token->pos, "Field '%s' does not exist on '%s'.", field->field, type_name);
}

// CLEANUP: This is an experimental feature and might be removed in the future.
// I noticed a common pattern when writing in Onyx is something that looks like this:
//
//     foo.member_function(&foo, ...)
//
// I decided it would be worth adding a bit of syntactic sugar for such as call. I
// decided to use the '->' operator for this purpose. The snippet below is the exact
// same as the snippet above (after the nodes have been processed by the function below)
//
//     foo->member_function(...)
CHECK_FUNC(method_call, AstBinaryOp** pmcall) {
    AstBinaryOp* mcall = *pmcall;

    CHECK(expression, &mcall->left);

    if ((mcall->flags & Ast_Flag_Has_Been_Symres) == 0) {
        if (mcall->left == NULL) return Check_Error;

        if (mcall->right->kind != Ast_Kind_Call) {
            ERROR(mcall->token->pos, "'->' expected procedure call on right side.");
        }

        //
        // This is a small hack that makes chaining method calls
        // work. Because check_method_call replaces the method call
        // and marks it as completed, if there are multiple references
        // to the same method call node, one of them will be left dangling.
        // To remedy this, an alias node an be placed around the method call
        // so that when check_method_call replaces it, it is replaced
        // within the alias, and all references are updated.
        if (mcall->left->kind == Ast_Kind_Method_Call) {
            AstAlias *left_alias = onyx_ast_node_new(context->ast_alloc, sizeof(AstAlias), Ast_Kind_Alias);
            left_alias->token = mcall->left->token;
            left_alias->alias = mcall->left;

            mcall->left = (AstTyped *) left_alias;
        }

        AstFieldAccess* implicit_field_access = make_field_access(context, mcall->left, NULL);
        implicit_field_access->token = ((AstCall *) mcall->right)->callee->token;
        ((AstCall *) mcall->right)->callee = (AstTyped *) implicit_field_access;

        mcall->flags |= Ast_Flag_Has_Been_Symres;
    }

    // :Idempotency
    if ((mcall->flags & Ast_Flag_Has_Been_Checked) == 0) {
        if (mcall->left->type == NULL) {
            YIELD(mcall->token->pos, "Trying to resolve type of left hand side.");
        }

        AstTyped* implicit_argument = mcall->left;
        AstCall* call_node = (AstCall *) mcall->right;

        // Symbol resolution should have ensured that this is call node.
        assert(call_node->kind == Ast_Kind_Call);

        // Implicitly take the address of the value if it is not already a pointer type.
        // This could be weird to think about semantically so some testing with real code
        // would be good.                                      - brendanfh 2020/02/05
        if (implicit_argument->type->kind != Type_Kind_Pointer) {
            AstAddressOf *address_of = make_address_of(context, implicit_argument);
            address_of->can_be_removed = 1;
            implicit_argument = (AstTyped *) address_of;
        }

        AstArgument *new_arg = make_argument(context, implicit_argument);
        new_arg->used_as_lval_of_method_call = 1;

        bh_arr_insertn(call_node->args.values, 0, 1);
        call_node->args.values[0] = (AstTyped *) new_arg;

        mcall->right->next = mcall->next;
        mcall->flags |= Ast_Flag_Has_Been_Checked;
    }

    //
    // This can happen now that method calls which expand via a macro are not replaced and
    // instead are passed all the way to the code generator.
    //
    if (mcall->right->kind != Ast_Kind_Call) {
        *pmcall = (AstBinaryOp *) mcall->right;
        // CHECK(expression, (AstCall **) pmcall);
        return Check_Yield;

    } else {
        CHECK(call, (AstCall **) &mcall->right);
        mcall->type = mcall->right->type;
    }

    return Check_Success;
}

CHECK_FUNC(size_of, AstSizeOf* so) {
    CHECK(type, &so->type_node);
    CHECK(type, &so->so_ast_type);

    so->so_type = type_build_from_ast(context, so->so_ast_type);
    if (so->so_type == NULL)
        YIELD(so->token->pos, "Trying to resolve type to take the size of.");

    // HACK
    if (
        (so->so_type->kind == Type_Kind_Struct && so->so_type->Struct.status == SPS_Start) ||
        (so->so_type->kind == Type_Kind_Union && so->so_type->Union.status == SPS_Start)
    ) {
        YIELD(so->token->pos, "Waiting until type has a size.");
    }

    so->size = type_size_of(so->so_type);
    so->flags |= Ast_Flag_Comptime;
    return Check_Success;
}

CHECK_FUNC(align_of, AstAlignOf* ao) {
    CHECK(type, &ao->type_node);
    CHECK(type, &ao->ao_ast_type);

    ao->ao_type = type_build_from_ast(context, ao->ao_ast_type);
    if (ao->ao_type == NULL)
        YIELD(ao->token->pos, "Trying to resolve type to take the alignment of.");

    // HACK
    if (
        (ao->ao_type->kind == Type_Kind_Struct && ao->ao_type->Struct.status == SPS_Start) ||
        (ao->ao_type->kind == Type_Kind_Union && ao->ao_type->Union.status == SPS_Start)
    ) {
        YIELD(ao->token->pos, "Waiting until type has an alignment.");
    }

    ao->alignment = type_alignment_of(ao->ao_type);
    ao->flags |= Ast_Flag_Comptime;

    return Check_Success;
}

CHECK_FUNC(expression, AstTyped** pexpr) {
    if ((*pexpr)->kind == Ast_Kind_Symbol) {
        CHECK(symbol, (AstNode **) pexpr);

        // HACK?
        // I don't know how I never ran into this problem before,
        // but when a symbol is resolved, there is never a "double
        // check" that its type node is symbol resolved as well.
        // This only proved to be an issue when using constraint
        // sentinels, so I only added that case here. This should
        // maybe be considered in the future because I think this
        // lack of double checking could be causing other bugs.
        if ((*pexpr)->kind == Ast_Kind_Constraint_Sentinel) {
            CHECK(type, &(*pexpr)->type_node);
        }

        CHECK(expression, (AstTyped **) pexpr);
        return Check_Success;
    }

    AstTyped* expr = *pexpr;

    if (node_is_type((AstNode *) expr)) {
        // This is to ensure that the type will exist when compiling. For example, a poly-call type
        // would have to wait for the entity to pass through, which the code generation does not know
        // about.
        CHECK(type, (AstType **) pexpr);
        expr = (AstTyped *) strip_aliases((AstNode *) *pexpr);

        // Don't try to construct a polystruct ahead of time because you can't.
        if (expr->kind != Ast_Kind_Poly_Struct_Type &&
            expr->kind != Ast_Kind_Poly_Union_Type) {
            if (type_build_from_ast(context, (AstType*) expr) == NULL) {
                YIELD(expr->token->pos, "Trying to construct type.");
            }
        } else {
            type_build_from_ast(context, (AstType*) expr);
        }

        expr->type = context->types.basic[Basic_Kind_Type_Index];
        return Check_Success;
    }

    if (expr->kind == Ast_Kind_Polymorphic_Proc) {
        // polymorphic procedures do not need to be checked. Their concrete instantiations
        // will be checked when they are created.
        
        if (((AstFunction *) expr)->captures) {
            ((AstFunction *) expr)->scope_to_lookup_captured_values = context->checker.current_scope;
        }

        return Check_Success;
    }

    if (expr->kind == Ast_Kind_Macro) {
        return Check_Success;
    }

    if (expr->kind == Ast_Kind_Directive_Init) {
        if (!mode_enabled(context, CM_Allow_Init_Expressions)) {
            ERROR(expr->token->pos, "#init declarations are not allowed in normal expressions, only in #after clauses.");
        }

        return Check_Success;
    }

    // We have to set the type_node of string literals outside of the parser,
    // because the actual nodes for builtins could be NULL if we are parsing
    // the builtin.onyx file. Setting them here resolves that. Maybe we could
    // make the string-like types internal to the compiler so we wouldn't need
    // this hack?
    //
    //      - brendanfh  01 January 2025
    //
    if (expr->kind == Ast_Kind_StrLit) {
        AstStrLit* str = (AstStrLit *) expr;
        if (str->is_cstr) {
            CHECK(type, &context->builtins.cstring_type);
            str->type_node = context->builtins.cstring_type;

        } else {
            CHECK(type, &context->builtins.string_type);
            str->type_node = context->builtins.string_type;
        }
    }

    fill_in_type(context, expr);
    context->checker.current_checking_level = EXPRESSION_LEVEL;

    CheckStatus retval = Check_Success;
    switch (expr->kind) {
        case Ast_Kind_Binary_Op: retval = check_binaryop(context, (AstBinaryOp **) pexpr); break;
        case Ast_Kind_Unary_Op:  retval = check_unaryop(context, (AstUnaryOp **) pexpr); break;
        case Ast_Kind_Pipe:      retval = check_pipe(context, (AstBinaryOp **) pexpr); break;

        case Ast_Kind_Intrinsic_Call:
        case Ast_Kind_Call:     retval = check_call(context, (AstCall **) pexpr); break;
        case Ast_Kind_Argument: retval = check_argument(context, (AstArgument **) pexpr); break;
        case Ast_Kind_Block:    retval = check_block(context, (AstBlock *) expr); break;

        case Ast_Kind_Symbol: assert(0); break;

        case Ast_Kind_Param:
            if (expr->flags & Ast_Flag_Param_Symbol_Dirty) {
                assert(expr->token->type == Token_Type_Symbol);
                *pexpr = (AstTyped *) make_symbol(context, expr->token);
                CHECK(expression, pexpr);
            }

            if (expr->type == NULL) {
                YIELD(expr->token->pos, "Waiting on parameter type.");
            }
            break;

        case Ast_Kind_Local: break;

        case Ast_Kind_Address_Of:    retval = check_address_of(context, (AstAddressOf **) pexpr); break;
        case Ast_Kind_Dereference:   retval = check_dereference(context, (AstDereference *) expr); break;
        case Ast_Kind_Slice:
        case Ast_Kind_Subscript:     retval = check_subscript(context, (AstSubscript **) pexpr); break;
        case Ast_Kind_Field_Access:  retval = check_field_access(context, (AstFieldAccess **) pexpr); break;
        case Ast_Kind_Method_Call:   retval = check_method_call(context, (AstBinaryOp **) pexpr); break;
        case Ast_Kind_Size_Of:       retval = check_size_of(context, (AstSizeOf *) expr); break;
        case Ast_Kind_Align_Of:      retval = check_align_of(context, (AstAlignOf *) expr); break;
        case Ast_Kind_Range_Literal: retval = check_range_literal(context, (AstRangeLiteral **) pexpr); break;

        case Ast_Kind_Global:
            if (expr->type == NULL) {
                ONYX_ERROR(expr->token->pos, Error_Critical, "Global with unknown type.");
                retval = Check_Error;
            }
            break;

        case Ast_Kind_NumLit:
            if (!expr->type) {
                return Check_Yield;
            }
            break;

        case Ast_Kind_Struct_Literal:
            retval = check_struct_literal(context, (AstStructLiteral *) expr);
            break;

        case Ast_Kind_Array_Literal:
            retval = check_array_literal(context, (AstArrayLiteral *) expr);
            break;

        case Ast_Kind_Function:
            // We do not need to check the type, because fill_in_type should already have done that for us.
            // CHECK(type, &(*expr)->type_node);

            if (((AstFunction *) expr)->captures) {
                ((AstFunction *) expr)->scope_to_lookup_captured_values = context->checker.current_scope;
            }

            if (expr->type == NULL)
                YIELD(expr->token->pos, "Waiting for function type to be resolved.");

            break;

        case Ast_Kind_Directive_Solidify:
            CHECK(directive_solidify, (AstDirectiveSolidify **) pexpr);
            break;

        case Ast_Kind_Directive_Defined:
            CHECK(directive_defined, (AstDirectiveDefined **) pexpr);

            *pexpr = (AstTyped *) make_bool_literal(context, ((AstDirectiveDefined *) expr)->is_defined);
            fill_in_type(context, *pexpr);
            break;

        case Ast_Kind_Compound:
            CHECK(compound, (AstCompound *) expr);
            break;

        case Ast_Kind_Call_Site:
            // NOTE: This has to be set here because if it were to be set in the parser,
            // context->builtins.callsite_type wouldn't be known when parsing the builtin.onyx file.
            expr->type_node = context->builtins.callsite_type;
            break;

        case Ast_Kind_If_Expression:
            CHECK(if_expression, (AstIfExpression *) expr);
            break;

        case Ast_Kind_Alias: {
            AstAlias *alias = (AstAlias *) expr;

            //
            // If an alias has an entity, do not force checking on it here.
            // Wait for the alias to pass type-checking in the normal way.
            // Otherwise, there can be weird cases where symbols resolve
            // incorrectly because they are being checked in the wrong scope.
            //
            if (alias->entity && context->checker.current_entity != alias->entity) {
                if (alias->entity->state < Entity_State_Code_Gen) {
                    YIELD(expr->token->pos, "Waiting for alias to pass type checking.");
                }
            } else {
                CHECK_INVISIBLE(expression, alias, &alias->alias);
            }

            expr->flags |= (((AstAlias *) expr)->alias->flags & Ast_Flag_Comptime);
            expr->type = ((AstAlias *) expr)->alias->type;
            break;
        }

        case Ast_Kind_Directive_Insert:
            retval = check_insert_directive(context, (AstDirectiveInsert **) pexpr, 1);
            break;

        case Ast_Kind_Code_Block:
            expr->flags |= Ast_Flag_Comptime;
            fill_in_type(context, expr);
            bh_arr_each(CodeBlockBindingSymbol, sym, ((AstCodeBlock *) expr)->binding_symbols) {
                if (sym->type_node) {
                    CHECK(expression, (AstTyped **) &sym->type_node);
                }
            }
            break;

        case Ast_Kind_Do_Block: {
            Scope* old_current_scope = context->checker.current_scope;
            retval = check_do_block(context, (AstDoBlock **) pexpr);
            context->checker.current_scope = old_current_scope;
            break;
        }

        case Ast_Kind_Memres:
            if (expr->type == NULL || expr->type->kind == Type_Kind_Invalid) YIELD(expr->token->pos, "Waiting to know globals type.");
            break;

        case Ast_Kind_Directive_First:
            CHECK(directive_first, (AstDirectiveFirst *) expr);
            break;

        case Ast_Kind_Constraint_Sentinel:
            if (expr->type == NULL) YIELD(expr->token->pos, "Waiting to know constraint sentinel's type.");
            break;

        case Ast_Kind_Directive_Export_Name:
            retval = check_directive_export_name(context, (AstDirectiveExportName *) expr);
            break;

        case Ast_Kind_StrLit: {
            if (expr->type == NULL) YIELD(expr->token->pos, "Waiting to know string literals type. This is a weird one...") ;
            break;
        }

        case Ast_Kind_Directive_This_Package:
            YIELD(expr->token->pos, "Waiting to resolve #this_package.");
            break;

        case Ast_Kind_Capture_Local: {
            AstCaptureLocal *cl = (AstCaptureLocal *) expr;
            if (cl->by_reference) {
                if (!is_lval((AstNode *) cl->captured_value)) {
                    ERROR_(cl->token->pos, "Cannot pass '%b' by pointer because it is not an l-value.", cl->token->text, cl->token->length);
                }

                if (cl->captured_value->kind == Ast_Kind_Local) {
                    cl->captured_value->flags |= Ast_Flag_Address_Taken;
                }

                expr->type = type_make_pointer(context, cl->captured_value->type);

            } else {
                expr->type = cl->captured_value->type;
            }
            break;
        }

        case Ast_Kind_Switch: {
            AstSwitch* switch_node = (AstSwitch *) expr;
            assert(switch_node->is_expr);

            CHECK(switch, switch_node);
            break;
        }

        case Ast_Kind_Package:
            CHECK(package, (AstPackage *) expr);
            break;

        case Ast_Kind_Procedural_Expansion:
            CHECK(proc_expansion, (AstProceduralExpansion **) pexpr, PMEK_Expression);
            break;

        case Ast_Kind_Switch_Case: break;
        case Ast_Kind_File_Contents: break;
        case Ast_Kind_Overloaded_Function: break;
        case Ast_Kind_Enum_Value: break;
        case Ast_Kind_Polymorphic_Proc: break;
        case Ast_Kind_Error: break;
        case Ast_Kind_Unary_Field_Access: break;
        case Ast_Kind_Foreign_Block: break;
        case Ast_Kind_Zero_Value: break;
        case Ast_Kind_Interface: break;
        case Ast_Kind_Compiler_Extension: break;
        case Ast_Kind_Procedural_Macro: break;

        default:
            retval = Check_Error;
            ONYX_ERROR(expr->token->pos, Error_Critical, "UNEXPECTED INTERNAL COMPILER ERROR");
            DEBUG_HERE;
            break;
    }

    return retval;
}

CHECK_FUNC(global_header, AstGlobal *global) {
    CHECK(type, &global->type_node);
    return Check_Success;
}

CHECK_FUNC(global, AstGlobal* global) {
    fill_in_type(context, (AstTyped *) global);

    if (global->type == NULL) {
        YIELD(global->token->pos, "Trying to resolve type for global.");
    }

    return Check_Success;
}

CHECK_FUNC(insert_directive, AstDirectiveInsert** pinsert, b32 expected_expression) {
    AstDirectiveInsert* insert = *pinsert;
    if (insert->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    CHECK(expression, &insert->code_expr);
    if (insert->code_expr->type == NULL) {
        if (insert->code_expr->entity && insert->code_expr->entity->state >= Entity_State_Code_Gen) {
            ERROR(insert->token->pos, "Expected expression of type 'Code'.");
        }

        // Bad wording for the message.
        YIELD(insert->token->pos, "Waiting for resolution to code expression type.");
    }

    bh_arr_each(AstTyped *, pexpr, insert->binding_exprs) {
        CHECK(expression, pexpr);
    }

    if (insert->skip_scope_index) {
        CHECK(expression, &insert->skip_scope_index);
    }

    Type* code_type = type_build_from_ast(context, context->builtins.code_type);
    TYPE_CHECK(&insert->code_expr, code_type) {
        ERROR_(insert->token->pos, "#unquote expected a value of type 'Code', got '%s'.",
            type_get_name(context, insert->code_expr->type));
    }

    AstCodeBlock* code_block = (AstCodeBlock *) insert->code_expr;
    code_block = (AstCodeBlock *) strip_aliases((AstNode *) code_block);

    if (code_block->kind != Ast_Kind_Code_Block) {
        ERROR(insert->token->pos, "Expected compile-time known expression of type 'Code'.");
    }

    if (!code_block->is_expression && expected_expression) {
        ONYX_ERROR(insert->token->pos, Error_Critical, "Expected a code block that is an expression here, but got a code block that is statements.");
        ONYX_ERROR(code_block->token->pos, Error_Critical, "Try changing { expr } into ( expr ) here.");
        return Check_Error;
    }

    u32 bound_symbol_count = bh_arr_length(code_block->binding_symbols);
    u32 bound_expr_count   = bh_arr_length(insert->binding_exprs);
    if (bound_symbol_count > bound_expr_count) {
        ONYX_ERROR(insert->token->pos, Error_Critical,
                "Expected at least %d argument%s to unquote code block, only got %d.",
                bound_symbol_count, bh_num_plural(bound_symbol_count), bound_expr_count);
        ERROR(code_block->token->pos, "Here is the code block being unquoted.");
    }

    AstNode* cloned_block = ast_clone(context, code_block->code);
    cloned_block->next = insert->next;

    i32 skip_scope_index = get_expression_integer_value(context, insert->skip_scope_index, NULL);
    Scope *scope_for_cloned_block = NULL;
    if (skip_scope_index > 0) {
        Scope *skip_scope = context->checker.current_scope;
        fori (i, 0, skip_scope_index) {
            if (!skip_scope->parent) break;
            skip_scope = skip_scope->parent;
        }

        scope_for_cloned_block = scope_create(context, skip_scope, cloned_block->token->pos);
    }

    if (bound_expr_count > 0) {
        Scope **scope = NULL;

        if (cloned_block->kind == Ast_Kind_Block) {
            ((AstBlock *) cloned_block)->scope = scope_for_cloned_block;
            scope = &((AstBlock *) cloned_block)->quoted_block_capture_scope;

        } else if (bound_symbol_count > 0) {
            AstReturn* return_node = onyx_ast_node_new(context->ast_alloc, sizeof(AstReturn), Ast_Kind_Return);
            return_node->token = cloned_block->token;
            return_node->expr = (AstTyped *) cloned_block;

            AstBlock* body_block = onyx_ast_node_new(context->ast_alloc, sizeof(AstBlock), Ast_Kind_Block);
            body_block->token = cloned_block->token;
            body_block->body = (AstNode *) return_node;
            body_block->rules = Block_Rule_Code_Block;
            ((AstBlock *) body_block)->scope = scope_for_cloned_block;
            scope = &((AstBlock *) body_block)->quoted_block_capture_scope;

            AstDoBlock* doblock = (AstDoBlock *) onyx_ast_node_new(context->ast_alloc, sizeof(AstDoBlock), Ast_Kind_Do_Block);
            doblock->token = cloned_block->token;
            doblock->block = body_block;
            doblock->type = context->types.auto_return;
            doblock->next = cloned_block->next;

            cloned_block = (AstNode *) doblock;
        }

        if (bound_symbol_count > 0) {
            assert(scope);
            *scope = scope_create(context, NULL, code_block->token->pos);

            fori (i, 0, bound_symbol_count) {
                CodeBlockBindingSymbol sym = code_block->binding_symbols[i];
                if (sym.type_node) {
                    Type *type = type_build_from_ast(context, sym.type_node);

                    TYPE_CHECK(&insert->binding_exprs[i], type) {
                        ERROR_(insert->token->pos, "Expected type '%s' but got type '%s' for the '%d%s' argument to the code block.", 
                               type_get_name(context, type), type_get_name(context, insert->binding_exprs[i]->type),
                               i + 1, bh_num_suffix(i + 1));
                    }
                }

                AstNode *value = (void *) insert->binding_exprs[i];
                symbol_introduce(context, *scope, sym.symbol, value);
            }
        }
    }

    *(AstNode **) pinsert = cloned_block;

    insert->flags |= Ast_Flag_Has_Been_Checked;

    return Check_Yield;
}

CHECK_FUNC(directive_defined, AstDirectiveDefined** pdefined) {
    AstDirectiveDefined* defined = *pdefined;

    b32 has_to_be_resolved = context->cycle_almost_detected >= 1;

    // We disable errors here so if we fail a symbol resolution, we don't generate any errors
    // and instead can capture that as "not defined".
    onyx_errors_disable(context);
    context->checker.resolved_a_symbol = 0;

    CheckStatus ss = check_expression(context, &defined->expr);
    if (has_to_be_resolved && ss != Check_Success && !context->checker.resolved_a_symbol) {
        // The symbol definitely was not found and there is no chance that it could be found.
        defined->is_defined = 0;

        onyx_errors_enable(context);
        return Check_Success;
    }

    if (ss == Check_Success) {
        defined->is_defined = 1;

        onyx_errors_enable(context);
        return Check_Success;
    }

    onyx_errors_enable(context);
    return Check_Yield;
}

CHECK_FUNC(directive_solidify, AstDirectiveSolidify** psolid) {
    AstDirectiveSolidify* solid = *psolid;

    CHECK(expression, (AstTyped **) &solid->poly_proc);

    if (solid->poly_proc && solid->poly_proc->kind == Ast_Kind_Directive_Solidify) {
        AstFunction* potentially_resolved_proc = (AstFunction *) ((AstDirectiveSolidify *) solid->poly_proc)->resolved_proc;
        if (!potentially_resolved_proc) return Check_Yield;

        solid->poly_proc = potentially_resolved_proc;
    }

    if (!solid->poly_proc || solid->poly_proc->kind != Ast_Kind_Polymorphic_Proc) {
        ERROR(solid->token->pos, "Expected polymorphic procedure in #solidify directive.");
    }

    bh_arr_each(AstPolySolution, sln, solid->known_polyvars) {
        // HACK: This assumes that 'ast_type' and 'value' are at the same offset.
        CHECK(expression, &sln->value);

        if (node_is_type((AstNode *) sln->value)) {
            sln->type = type_build_from_ast(context, sln->ast_type);
            sln->kind = PSK_Type;
        } else {
            sln->kind = PSK_Value;
        }
    }

    solid->resolved_proc = polymorphic_proc_try_solidify(context, solid->poly_proc, solid->known_polyvars, solid->token);
    if (solid->resolved_proc == (AstNode *) &context->node_that_signals_a_yield) {
        solid->resolved_proc = NULL;
        YIELD(solid->token->pos, "Waiting for partially solidified procedure.");
    }

    // NOTE: Not a DirectiveSolidify.
    *psolid = (AstDirectiveSolidify *) solid->resolved_proc;

    return Check_Success;
}

CHECK_FUNC(directive_first, AstDirectiveFirst *first) {
    if (bh_arr_length(context->checker.while_node_stack) == 0) {
        ERROR(first->token->pos, "#first is only allowed in the body of a while-loop or for-loop.");
    }

    first->while_node = bh_arr_last(context->checker.while_node_stack);
    assert(first->while_node);

    first->while_node->has_first = 1;

    return Check_Success;
}

CHECK_FUNC(directive_export_name, AstDirectiveExportName *ename) {
    CHECK(expression, (AstTyped **) &ename->func);

    if (ename->func->kind != Ast_Kind_Function) {
        ERROR(ename->token->pos, "#export_name can only be used on functions.");
    }

    if (ename->type == NULL) YIELD(ename->token->pos, "This should never yield here...");

    ename->flags |= Ast_Flag_Comptime;

    //
    // TODO: Cleanup this code. I feel like there should be some convenience functions
    // to make string literals, tokens, exports, etc...
    if (ename->func->exported_name == NULL) {
        if (ename->created_export_entity) {
            return Check_Yield;
        }

        // In this case, we know the function is not exported.
        assert(ename->func->is_exported == 0);

        char *random_name = bh_alloc_array(context->ast_alloc, char, 16);
        random_name[15] = 0;
        fori (i, 0, 15) random_name[i] = (rand() % 26) + 'a';

        OnyxToken *name_token = bh_alloc_item(context->ast_alloc, OnyxToken);
        memset(name_token, 0, sizeof(*name_token));
        name_token->type = Token_Type_Literal_String;
        name_token->length = 15;
        name_token->text = random_name;

        AstStrLit* name = bh_alloc_item(context->ast_alloc, AstStrLit);
        memset(name, 0, sizeof(AstStrLit));
        name->kind  = Ast_Kind_StrLit;
        name->token = name_token;
        name->type_node = context->builtins.string_type;

        add_entities_for_node(&context->entities, NULL, (AstNode *) name, NULL, NULL);
        ename->name = name;

        AstDirectiveExport *export = onyx_ast_node_new(context->ast_alloc, sizeof(AstDirectiveExport), Ast_Kind_Directive_Export);
        export->token = ename->token;
        export->export_name_expr = (AstTyped *) name;
        export->export = (AstTyped *) ename->func;

        add_entities_for_node(&context->entities, NULL, (AstNode *) export, NULL, NULL);

        ename->created_export_entity = 1;
        return Check_Yield;

    } else {
        AstStrLit* name = bh_alloc_item(context->ast_alloc, AstStrLit);
        memset(name, 0, sizeof(AstStrLit));
        name->kind  = Ast_Kind_StrLit;
        name->token = ename->func->exported_name;
        name->type_node = context->builtins.string_type;

        add_entities_for_node(&context->entities, NULL, (AstNode *) name, NULL, NULL);
        ename->name = name;
    }

    return Check_Success;
}

CHECK_FUNC(capture_block, AstCaptureBlock *block, Scope *captured_scope) {
    //
    // Reserve 8 bytes at the beginning of the closure block for the size of the closure.
    block->total_size_in_bytes = 8;

    bh_arr_each(AstCaptureLocal *, capture, block->captures) {
        OnyxToken *token = (*capture)->token;
        AstTyped *resolved = (AstTyped *) symbol_resolve(context, captured_scope, token);

        if (!resolved) {
            // Should this do a yield? In there any case that that would make sense?
            ERROR_(token->pos, "'%b' is not found in the enclosing scope.", token->text, token->length);
        }

        (*capture)->captured_value = resolved;

        CHECK(expression, (AstTyped **) capture);
        if (!(*capture)->type) YIELD((*capture)->token->pos, "Waiting to resolve captures type.");

        (*capture)->offset = block->total_size_in_bytes;
        block->total_size_in_bytes += type_size_of((*capture)->type);
    }

    bh_arr_each(AstCaptureLocal *, capture, block->captures) {
        symbol_introduce(context, context->checker.current_scope, (*capture)->token, (AstNode *) *capture);
    }

    return Check_Success;
}

CHECK_FUNC(statement, AstNode** pstmt) {
    AstNode* stmt = *pstmt;

    context->checker.current_checking_level = STATEMENT_LEVEL;

    switch (stmt->kind) {
        case Ast_Kind_Jump:        return Check_Success;

        case Ast_Kind_Return:      return check_return(context, (AstReturn *) stmt);
        case Ast_Kind_If:          return check_if(context, (AstIfWhile *) stmt);
        case Ast_Kind_Static_If:   return check_if(context, (AstIfWhile *) stmt);
        case Ast_Kind_While:       return check_while(context, (AstIfWhile *) stmt);
        case Ast_Kind_For:         return check_for(context, (AstFor **) pstmt);
        case Ast_Kind_Switch:      return check_switch(context, (AstSwitch *) stmt);
        case Ast_Kind_Switch_Case: return check_case(context, (AstSwitchCase *) stmt);
        case Ast_Kind_Block:       return check_block(context, (AstBlock *) stmt);
        case Ast_Kind_Defer:       return check_statement(context, &((AstDefer *) stmt)->stmt);
        case Ast_Kind_Argument:    return check_expression(context, (AstTyped **) &((AstArgument *) stmt)->value);

        case Ast_Kind_Directive_Insert: return check_insert_directive(context, (AstDirectiveInsert **) pstmt, 0);
        case Ast_Kind_Procedural_Expansion: return check_proc_expansion(context, (AstProceduralExpansion **) pstmt, PMEK_Statement);


        //
        // Call and Binary op nodes need to be treated differently here, because they use the current_checking_level
        // to determine if an assignment is legal.
        //
        case Ast_Kind_Call:
            CHECK(call, (AstCall **) pstmt);
            (*pstmt)->flags |= Ast_Flag_Expr_Ignored;
            break;

        case Ast_Kind_Binary_Op:
            CHECK(binaryop, (AstBinaryOp **) pstmt);
            (*pstmt)->flags |= Ast_Flag_Expr_Ignored;
            return Check_Success;


        // NOTE: Local variable declarations used to be removed after the symbol
        // resolution phase because long long ago, all locals needed to be known
        // in a block in order to efficiently allocate enough space and registers
        // for them all. Now with LocalAllocator, this is no longer necessary.
        // Therefore, locals stay in the tree and need to be passed along.
        case Ast_Kind_Local: {
            CHECK(local, (AstLocal **) pstmt);

            AstTyped* typed_stmt = (AstTyped *) stmt;
            fill_in_type(context, typed_stmt);
            if (typed_stmt->type_node != NULL && typed_stmt->type == NULL) {
                CHECK(type, &typed_stmt->type_node);

                if (!node_is_type((AstNode *) typed_stmt->type_node)) {
                    if (typed_stmt->type_node->type == context->types.basic[Basic_Kind_Type_Index]) {
                        ONYX_ERROR(stmt->token->pos, Error_Critical, "The type of this local variable is a runtime-known type, not a compile-time known type.");

                        if (typed_stmt->type_node->kind == Ast_Kind_Param) {
                            ONYX_ERROR(stmt->token->pos, Error_Critical, "Try adding a '$' to the parameter name to make this a compile-time known type.");
                        }

                        return Check_Error;
                    } else {
                        ERROR_(stmt->token->pos, "The type of this local is not a type. It is a %s.", onyx_ast_node_kind_string(typed_stmt->type_node->kind));
                    }
                }

                YIELD(typed_stmt->token->pos, "Waiting for local variable's type.");
            }

            if (typed_stmt->next != NULL && typed_stmt->next->kind == Ast_Kind_Binary_Op) {
                AstBinaryOp *next = (AstBinaryOp *) typed_stmt->next;

                //
                // :BrokenFollowedByInitFlag
                if (next->operation == Binary_Op_Assign && next->left == typed_stmt) {
                    typed_stmt->flags |= Ast_Flag_Decl_Followed_By_Init;
                }
            }
            
            if (typed_stmt->type != NULL && typed_stmt->type == context->types.basic[Basic_Kind_Void]) {
                ERROR(stmt->token->pos, "This local variable has a type of 'void', which is not allowed.");
            }

            //
            // Investigate: Why is something return a "node" when it should be returning a type?
            // Where is this value coming from? Likely in types.c...
            //
            if (typed_stmt->type == (Type *) &context->node_that_signals_failure) {
                ERROR(stmt->token->pos, "Invalid type for this local variable.");
            }

            return Check_Success;
        }

        //
        // I'm 99.99% sure this node can never appear here, but the code for it
        // was there in the past so I am adding an assert false just in case it
        // is actually possible through some mechanism I am unaware of.
        //
        case Ast_Kind_Import: assert(0); break;

        default:
            CHECK(expression, (AstTyped **) pstmt);
            (*pstmt)->flags |= Ast_Flag_Expr_Ignored;
            return Check_Success;
    }

    return Check_Success;
}

CHECK_FUNC(statement_chain, AstNode** walker) {
    while (*walker) {
        CHECK(statement, walker);
        walker = &(*walker)->next;
    }

    return Check_Success;
}

CHECK_FUNC(block, AstBlock* block) {
    // This used to use statement_chain, but since block optimize which statements need to be rechecked,
    // it has to be its own thing.

    if (block->rules & Block_Rule_New_Scope) {
        if (block->scope == NULL)
            block->scope = scope_create(context, context->checker.current_scope, block->token->pos);

        scope_enter(context, block->scope);
    }

    if (block->binding_scope != NULL)
        scope_include(context, context->checker.current_scope, block->binding_scope, block->token->pos);

    if (block->quoted_block_capture_scope != NULL)
        scope_include(context, context->checker.current_scope, block->quoted_block_capture_scope, block->token->pos);

    if (!block->body) {
        if (block->rules & Block_Rule_New_Scope) {
            scope_leave(context);
        }

        return Check_Success;
    }

    AstNode *last = block->body;
    AstNode** start = &block->body;
    fori (i, 0, block->statement_idx) {
        last = *start;
        start = &(*start)->next;
    }

    while (*start) {
        if ((*start)->kind == Ast_Kind_Return) {
            block->flags |= Ast_Flag_Block_Returns;
        }

        CheckStatus cs = check_statement(context, start);
        switch (cs) {
            case Check_Success:
                last = *start;
                start = &(*start)->next;
                block->statement_idx++;
                break;

            case Check_Failed:
            case Check_Error:
                if (block->macro_generated_from) {
                    ONYX_ERROR(
                        block->macro_generated_from->pos,
                        Error_Critical,
                        "Error in 'macro' that was generated from here."
                    );
                }
                return cs;

            default:
                return cs;
        }
    }

    if (last && last->flags & Ast_Flag_Block_Returns) {
        block->flags |= Ast_Flag_Block_Returns;
    }

    if (block->rules & Block_Rule_New_Scope)
        scope_leave(context);

    return Check_Success;
}

CHECK_FUNC(polyproc, AstFunction* pp) {
    pp->flags |= Ast_Flag_Comptime;
    pp->parent_scope_of_poly_proc = context->checker.current_scope;

    bh_arr_each(AstPolyParam, p, pp->poly_params) {
        if (p->kind != PSK_Value) continue;

        AstParam *param = &pp->params[p->idx];
        if (param->default_value != NULL) {
            CHECK(expression, &param->default_value);
        }
    }

    return Check_Complete;
}

CHECK_FUNC(function, AstFunction* func) {
    if (func->kind == Ast_Kind_Polymorphic_Proc) return Check_Complete;
    if (func->flags & Ast_Flag_Function_Is_Lambda_Inside_PolyProc) return Check_Complete;

    if (func->flags & Ast_Flag_Has_Been_Checked) return Check_Success;
    if (!func->ready_for_body_to_be_checked || !func->type) {
        YIELD(func->token->pos, "Waiting for procedure header to pass type-checking");
    }

    bh_arr_clear(context->checker.expected_return_type_stack);
    bh_arr_clear(context->checker.named_return_values_stack);
    bh_arr_push(context->checker.expected_return_type_stack, &func->type->Function.return_type);
    bh_arr_push(context->checker.named_return_values_stack, func->named_return_locals);

    if (context->checker.while_node_stack) {
        bh_arr_clear(context->checker.while_node_stack);
    }

    assert(func->scope);

    scope_enter(context, func->scope);

    if ((func->flags & Ast_Flag_Has_Been_Symres) == 0) {
        bh_arr_each(AstParam, param, func->params) {
            // CLEANUP: Currently, in order to 'use' parameters, the type must be completely
            // resolved and built. This is excessive because all that should need to be known
            // is the names of the members, since all that happens is implicit field accesses
            // are placed in the scope. So instead, there should be a way to just query all the
            // member names in the structure, without needing to know their type. This would be
            // easy if it were not for 'use' statements in structs. It is made even more complicated
            // by this situtation:
            //
            //     Foo :: struct (T: type_expr) {
            //         use t : T;
            //
            //         something_else := 5 + 6 * 8;
            //     }
            //
            // The 'use t : T' member requires completely knowing the type of T, to know which
            // members should be brought in. At the moment, that requires completely building the
            // type of Foo($T).
            if (param->is_used && !param->use_processed) {
                fill_in_type(context, (AstTyped *) param->local);
                if (!param->local->type) {
                    YIELD(param->local->token->pos, "Waiting for parameter type to be known.");
                }

                if (type_is_struct(param->local->type)) {
                    Type* st;
                    if (param->local->type->kind == Type_Kind_Struct) {
                        st = param->local->type;
                    } else {
                        st = param->local->type->Pointer.elem;
                    }

                    if (st->Struct.status != SPS_Uses_Done) return Check_Yield;

                    fori (i, 0, shlen(st->Struct.members)) {
                        StructMember* value = st->Struct.members[i].value;
                        AstFieldAccess* fa = make_field_access(context, (AstTyped *) param->local, value->name);
                        symbol_raw_introduce(context, context->checker.current_scope, value->name, param->local->token->pos, (AstNode *) fa);
                    }

                    param->use_processed = 1;

                } else if (param->local->type != NULL) {
                    ONYX_ERROR(param->local->token->pos, Error_Critical, "Can only 'use' structures or pointers to structures.");

                } else {
                    // :ExplicitTyping
                    ERROR_(param->local->token->pos, "Cannot deduce type of parameter '%b'; Try adding it explicitly.",
                        param->local->token->text,
                        param->local->token->length);
                }
            }
        }

        func->flags |= Ast_Flag_Has_Been_Symres;
    }

    if (func->named_return_locals) {
        bh_arr_each(AstLocal *, named_return, func->named_return_locals) {
            CHECK(local, named_return);
        }

        if (!func->named_return_locals_added) {
            func->named_return_locals_added = 1;
            
            AstNode **prev = &func->body->body;
            bh_arr_each(AstLocal *, named_return, func->named_return_locals) {
                (*named_return)->next = *prev;
                *prev = (AstNode *) *named_return;
            }
        }
    }


    if (func->body) {
        CheckStatus status = Check_Success;
        if (func->captures) {
            status = check_capture_block(context, func->captures, func->scope_to_lookup_captured_values);
        }

        if (status == Check_Success && func->stack_trace_local) {
            status = check_expression(context, (AstTyped **) &func->stack_trace_local);
        }

        if (status == Check_Success) {
            status = check_block(context, func->body);
        }

        if (status == Check_Success &&
            !(func->body->flags & Ast_Flag_Block_Returns) &&
            *bh_arr_last(context->checker.expected_return_type_stack) != context->types.basic[Basic_Kind_Void] &&
            *bh_arr_last(context->checker.expected_return_type_stack) != context->types.auto_return &&
            !func->is_intrinsic &&
            !func->is_foreign
        ) {
            status = Check_Error;
            ONYX_ERROR(func->token->pos, Error_Critical, "Not all code paths return a value.");
        }

        if (status == Check_Error && func->generated_from && context->cycle_detected == 0)
            ERROR(func->generated_from->pos, "Error in polymorphic procedure generated from this location.");

        if (status != Check_Success) {
            return status;
        }
    }

    if (*bh_arr_last(context->checker.expected_return_type_stack) == context->types.auto_return) {
        *bh_arr_last(context->checker.expected_return_type_stack) = context->types.basic[Basic_Kind_Void];
    }

    func->flags |= Ast_Flag_Has_Been_Checked;

    scope_leave(context);

    if (bh_arr_length(func->tags) > 0 || (func->flags & Ast_Flag_Proc_Is_Null) != 0) {
        func->flags |= Ast_Flag_Has_Been_Scheduled_For_Emit;
        return Check_Success;
    }

    return Check_Complete;
}

CHECK_FUNC(overloaded_function, AstOverloadedFunction* ofunc) {
    bh_arr_each(OverloadOption, overload, ofunc->overloads) {
        CHECK(expression, &overload->option);
    }

    if (ofunc->expected_return_node) {
        CHECK(type, &ofunc->expected_return_node);
    }

    b32 done = 1;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, context->gp_alloc, 4);
    build_all_overload_options(ofunc->overloads, &all_overloads);

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped* node = (AstTyped *) strip_aliases((AstNode *) entry->key);
        if (node->kind == Ast_Kind_Overloaded_Function) continue;

        if (   node->kind != Ast_Kind_Function
            && node->kind != Ast_Kind_Polymorphic_Proc
            && node->kind != Ast_Kind_Macro) {
            ONYX_ERROR(node->token->pos, Error_Critical, "Overload option not procedure or macro. Got '%s'",
                onyx_ast_node_kind_string(node->kind));

            bh_imap_free(&all_overloads);
            return Check_Error;
        }

        node->flags &= ~Ast_Flag_Function_Is_Lambda;

        if (node->kind == Ast_Kind_Function) {
            AstFunction* func = (AstFunction *) node;

            if (func->entity_header && func->entity_header->state <= Entity_State_Check_Types) {
                done = 0;
            }
        }
    }

    if (!done) {
        bh_imap_free(&all_overloads);
        YIELD(ofunc->token->pos, "Waiting for all options to pass type-checking.");
    }

    if (ofunc->expected_return_node) {
        AstType *expected_return_node = (AstType *) strip_aliases((AstNode *) ofunc->expected_return_node);

        if (expected_return_node->kind == Ast_Kind_Poly_Struct_Type ||
            expected_return_node->kind == Ast_Kind_Poly_Union_Type) {
            //
            // When you declare the expected return type of a #match'ed procedure to
            // be a polymorphic structure type, a special case has to happen. By trying
            // to build the type, the polymorphic structure will be given a type-id.
            // type_build_from_ast() will never return a polymorphic structure type
            // because that is never valid in the type system. However, we can by-pass
            // this and look it up directly using type_lookup_by_id.
            type_build_from_ast(context, expected_return_node);

            if (expected_return_node->type_id) {
                ofunc->expected_return_type = type_lookup_by_id(context, expected_return_node->type_id);

                // Return early here because the following code does not work with a
                // polymorphic expected return type.
                bh_imap_free(&all_overloads);
                return Check_Success;
            }
        }

        ofunc->expected_return_type = type_build_from_ast(context, expected_return_node);
        if (!ofunc->expected_return_type) YIELD(ofunc->token->pos, "Waiting to construct expected return type.");

        bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
            AstTyped* node = (AstTyped *) entry->key;

            if (node->kind == Ast_Kind_Function) {
                AstFunction *func = (AstFunction *) node;

                if (!func->type) continue;
                if (!func->type->Function.return_type) continue;

                Type *return_type = func->type->Function.return_type;
                if (return_type == context->types.auto_return) continue;

                if (!types_are_compatible(context, return_type, ofunc->expected_return_type)) {
                    report_incorrect_overload_expected_type(context, return_type, ofunc->expected_return_type, func->token, ofunc->token);
                    bh_imap_free(&all_overloads);
                    return Check_Error;
                }
            }
        }
    }


    bh_imap_free(&all_overloads);
    return Check_Success;
}

CHECK_FUNC(package, AstPackage* package) {
    if (package->package == NULL) {
        if (!package->package_name) {
            ERROR(package->token->pos, "Internal compiler error: Expected package to have a name");
        }

        package->package = package_lookup(context, package->package_name);
    }

    if (package->package) {
        package_mark_as_used(context, package->package);
        return Check_Success;

    } else {
        YIELD_ERROR_(package->token->pos, "Package '%s' not found in included source files.", package->package_name);
    }
}

CHECK_FUNC(enum, AstEnumType* enum_node) {
    if (!enum_node->backing_type) {
        CHECK(type, (AstType **) &enum_node->backing);

        enum_node->backing_type = type_build_from_ast(context, enum_node->backing);
        if (enum_node->backing_type == NULL) {
            YIELD(enum_node->token->pos, "Unable to construct the backing type of this enum.");
        }
    }

    if (enum_node->scope == NULL) {
        enum_node->scope = scope_create(context, context->checker.current_scope, enum_node->token->pos);

        symbol_raw_introduce(context, enum_node->scope, "__backing_type", enum_node->token->pos, (AstNode *) enum_node->backing);

        type_build_from_ast(context, (AstType *) enum_node);
    }

    scope_enter(context, enum_node->scope);

    u64 next_assign_value = enum_node->is_flags ? 1 : 0;
    bh_arr_each(AstEnumValue *, pvalue, enum_node->values) {
        AstEnumValue *value = *pvalue;
        if (value->flags & Ast_Flag_Has_Been_Checked) continue;

        value->type = enum_node->etcache;
        value->flags |= Ast_Flag_Comptime;

        if (value->value != NULL) {
            CHECK(expression, &value->value);

            if (value->value->kind == Ast_Kind_Enum_Value) {
                value->value = ((AstEnumValue *) value->value)->value;
                value->value->type = enum_node->etcache;
            }

            if (value->value->kind == Ast_Kind_NumLit) {
                AstNumLit *n_value = (AstNumLit *) value->value;
                resolve_expression_type(context, (AstTyped *) n_value);

                if (type_is_small_integer(n_value->type)) {
                    next_assign_value = n_value->value.i;
                } else if (type_is_integer(n_value->type)) {
                    next_assign_value = n_value->value.l;
                } else {
                    ERROR_(value->token->pos, "expected numeric integer literal for enum initialization, got '%s'", type_get_name(context, n_value->type));
                }

                n_value->type = enum_node->etcache;

            } else {
                if (value->entity == NULL) {
                    add_entities_for_node(&context->entities, NULL, (AstNode *) value, enum_node->scope, NULL);
                }

                YIELD(value->token->pos, "Expected compile time known value for enum initialization.");
            }

        } else {
            AstNumLit* num = make_int_literal(context, next_assign_value);
            num->type = enum_node->etcache;

            value->value = (AstTyped *) num;
        }

        symbol_introduce(context, enum_node->scope, value->token, (AstNode *) value);

        value->flags |= Ast_Flag_Comptime | Ast_Flag_Has_Been_Checked;

        if (enum_node->is_flags) {
            next_assign_value <<= 1;
        } else {
            next_assign_value++;
        }
    }

    scope_leave(context);

    // HACK this ensure that you can only lookup symbols in an Enum that are actually defined in the enum.
    // However, during the symbol resolution of the values in an enum, they need to be able to see the
    // enclosing scope.
    enum_node->scope->parent = NULL;

    return Check_Success;
}

CHECK_FUNC(meta_tags, bh_arr(AstTyped *) tags) {
    if (tags) {
        bh_arr_each(AstTyped *, meta, tags) {
            CHECK(expression, meta);
            resolve_expression_type(context, *meta);

            if (((*meta)->flags & Ast_Flag_Comptime) == 0) {
                ONYX_ERROR((*meta)->token->pos, Error_Critical, "#tag expressions are expected to be compile-time known.");
                return Check_Error;
            }
        }
    }

    return Check_Success;
}

CHECK_FUNC(struct, AstStructType* s_node) {
    if (s_node->entity_defaults && s_node->entity_defaults->state < Entity_State_Check_Types)
        YIELD(s_node->token->pos, "Waiting for struct member defaults to pass symbol resolution.");

    s_node->flags |= Ast_Flag_Comptime;

    assert(s_node->scope);
    scope_enter(context, s_node->scope);

    if (s_node->min_size_)      CHECK(expression, &s_node->min_size_);
    if (s_node->min_alignment_) CHECK(expression, &s_node->min_alignment_);

    if (s_node->polymorphic_argument_types) {
        assert(s_node->polymorphic_arguments);

        fori (i, 0, (i64) bh_arr_length(s_node->polymorphic_argument_types)) {
            CHECK(type, &s_node->polymorphic_argument_types[i]);

            Type *arg_type = type_build_from_ast(context, s_node->polymorphic_argument_types[i]);
            if (arg_type == NULL) YIELD(s_node->polymorphic_argument_types[i]->token->pos, "Waiting to build type for polymorph argument.");

            //
            // This check should always be false, but it handles
            // the case where somewhere a type was expected, but
            // not enough values were provided. This is checked
            // elsewhere when instantiating a polymorphic sturucture.
            if (i >= bh_arr_length(s_node->polymorphic_arguments)
                || !s_node->polymorphic_arguments[i].value) continue;

            if (s_node->polymorphic_arguments[i].value) {
                CHECK(expression, &s_node->polymorphic_arguments[i].value);
            }

            TYPE_CHECK(&s_node->polymorphic_arguments[i].value, arg_type) {
                ERROR_(s_node->polymorphic_arguments[i].value->token->pos, "Expected value of type '%s', got '%s'.",
                    type_get_name(context, arg_type),
                    type_get_name(context, s_node->polymorphic_arguments[i].value->type));
            }
        }
    }

    if (s_node->constraints.constraints) {
        s_node->constraints.produce_errors = (s_node->flags & Ast_Flag_Header_Check_No_Error) == 0;

        OnyxFilePos pos = s_node->token->pos;
        if (s_node->polymorphic_error_loc.filename) {
            pos = s_node->polymorphic_error_loc;
        }

        CHECK(constraint_context, &s_node->constraints, s_node->scope, pos);
    }

    bh_arr_each(AstStructMember *, smem, s_node->members) {
        AstStructMember *member = *smem;
        if (member->initial_value) {
            CHECK(expression, &member->initial_value);
        }
    }

    bh_arr_each(AstStructMember *, smem, s_node->members) {
        AstStructMember *member = *smem;
        track_declaration_for_symbol_info(context, member->token->pos, (AstNode *) member);

        if (member->type_node) {
            CHECK(type, &member->type_node);
        }

        if (member->type_node == NULL && member->initial_value != NULL) {
            CHECK(expression, &member->initial_value);

            fill_in_type(context, member->initial_value);
            if (member->initial_value->type == NULL)
                YIELD(member->initial_value->token->pos, "Trying to resolve type for initial value for member.");

            resolve_expression_type(context, member->initial_value);
            if (member->type == NULL) member->type = member->initial_value->type;

            if (member->type == NULL) {
                ERROR(member->initial_value->token->pos, "Unable to deduce type of initial value. This is probably a compiler bug.");
            }
        }
    }

    // NOTE: fills in the pending_type.
    s_node->ready_to_build_type = 1;
    type_build_from_ast(context, (AstType *) s_node);
    if (s_node->pending_type == NULL || !s_node->pending_type_is_valid)
        YIELD(s_node->token->pos, "Waiting for type to be constructed.");

    bh_arr_each(StructMember *, smem, s_node->pending_type->Struct.memarr) {
        if ((*smem)->type->kind == Type_Kind_Compound) {
            ERROR(s_node->token->pos, "Compound types are not allowed as struct member types.");
        }

        if ((*smem)->used && !(*smem)->use_processed) {
            if (!type_struct_member_apply_use(context, s_node->pending_type, *smem)) {
                YIELD((*smem)->token->pos, "Waiting for use to be applied.");
            }

            (*smem)->use_processed = 1;
        }
    }

    s_node->stcache = s_node->pending_type;
    s_node->stcache->Struct.status = SPS_Uses_Done;

    scope_leave(context);
    return Check_Success;
}

CHECK_FUNC(struct_defaults, AstStructType* s_node) {
    if (s_node->entity_type && s_node->entity_type->state < Entity_State_Code_Gen)
        YIELD(s_node->token->pos, "Waiting for struct type to be constructed before checking defaulted members.");
    if (s_node->entity_type && s_node->entity_type->state == Entity_State_Failed)
        return Check_Failed;

    if (s_node->scope) {
        scope_enter(context, s_node->scope);
    }

    CHECK(meta_tags, s_node->meta_tags);

    bh_arr_each(StructMember *, smem, s_node->stcache->Struct.memarr) {
        if ((*smem)->initial_value && *(*smem)->initial_value) {
            CHECK(expression, (*smem)->initial_value);

            TYPE_CHECK((*smem)->initial_value, (*smem)->type) {
                ERROR_((*(*smem)->initial_value)->token->pos,
                        "Mismatched type for initial value, expected '%s', got '%s'.",
                        type_get_name(context, (*smem)->type),
                        type_get_name(context, (*(*smem)->initial_value)->type));
            }

            resolve_expression_type(context, *(*smem)->initial_value);
        }

        CHECK(meta_tags, (*smem)->meta_tags);
    }

    if (s_node->scope) {
        scope_leave(context);
    }

    return Check_Success;
}

CHECK_FUNC(union, AstUnionType *u_node) {
    u_node->flags |= Ast_Flag_Comptime;

    if (!u_node->tag_backing_type) {
        int n = (31 - bh_clz(bh_arr_length(u_node->variants) - 1)) >> 3;
        if      (n == 0) u_node->tag_backing_type = (AstType *) &context->basic_types.type_u8;
        else if (n == 1) u_node->tag_backing_type = (AstType *) &context->basic_types.type_u16;
        else if (n <= 3) u_node->tag_backing_type = (AstType *) &context->basic_types.type_u32;
        else {
            ERROR(u_node->token->pos, "Too many union variants. How did you even do this...?");
        }
    }

    CHECK(type, &u_node->tag_backing_type);
    Type *tag_type = type_build_from_ast(context, u_node->tag_backing_type);
    if (!type_is_integer(tag_type)) {
        ERROR_(u_node->token->pos, "Union tag types must be an integer, got '%s'.", type_get_name(context, tag_type));
    }

    assert(u_node->scope);
    scope_enter(context, u_node->scope);

    if (u_node->polymorphic_argument_types) {
        assert(u_node->polymorphic_arguments);

        fori (i, 0, (i64) bh_arr_length(u_node->polymorphic_argument_types)) {
            CHECK(type, &u_node->polymorphic_argument_types[i]);

            Type *arg_type = type_build_from_ast(context, u_node->polymorphic_argument_types[i]);
            if (arg_type == NULL) YIELD(u_node->polymorphic_argument_types[i]->token->pos, "Waiting to build type for polymorph argument.");

            //
            // This check should always be false, but it handles
            // the case where somewhere a type was expected, but
            // not enough values were provided. This is checked
            // elsewhere when instantiating a polymorphic sturucture.
            if (i >= bh_arr_length(u_node->polymorphic_arguments)
                || !u_node->polymorphic_arguments[i].value) continue;

            CHECK(expression, &u_node->polymorphic_arguments[i].value);

            TYPE_CHECK(&u_node->polymorphic_arguments[i].value, arg_type) {
                ERROR_(u_node->polymorphic_arguments[i].value->token->pos, "Expected value of type %s, got %s.",
                    type_get_name(context, arg_type),
                    type_get_name(context, u_node->polymorphic_arguments[i].value->type));
            }
        }
    }

    if (u_node->constraints.constraints) {
        // bh_arr_each(AstConstraint *, constraint, u_node->constraints.constraints) {
        //     CHECK(constraint, *constraint);
        // }

        u_node->constraints.produce_errors = (u_node->flags & Ast_Flag_Header_Check_No_Error) == 0;

        OnyxFilePos pos = u_node->token->pos;
        if (u_node->polymorphic_error_loc.filename) {
            pos = u_node->polymorphic_error_loc;
        }
        CHECK(constraint_context, &u_node->constraints, u_node->scope, pos);
    }

    CHECK(meta_tags, u_node->meta_tags);

    bh_arr_each(AstUnionVariant *, pvariant, u_node->variants) {
        AstUnionVariant *variant = *pvariant;
        track_declaration_for_symbol_info(context, variant->token->pos, (AstNode *) variant);

        assert(variant->type_node);

        CHECK(type, &variant->type_node);
        if (variant->explicit_tag_value) {
            CHECK(expression, &variant->explicit_tag_value);
        }

        CHECK(meta_tags, variant->meta_tags);
    }

    type_build_from_ast(context, (AstType *) u_node);
    if (u_node->pending_type == NULL || !u_node->pending_type_is_valid)
        YIELD(u_node->token->pos, "Waiting for type to be constructed.");

    scope_leave(context);
    u_node->utcache = u_node->pending_type;
    return Check_Success;
}

CHECK_FUNC(temp_function_header, AstFunction* func) {
    if (func->flags & Ast_Flag_Header_Check_No_Error) {
        onyx_errors_disable(context);
    }

    CheckStatus cs = check_function_header(context, func);
    onyx_errors_enable(context);

    if (cs == Check_Error)  return Check_Failed;
    if (cs != Check_Success) return cs;

    return Check_Complete;
}

CHECK_FUNC(function_header, AstFunction* func) {
    func->flags |= Ast_Flag_Comptime;

    if (!(func->flags & Ast_Flag_Function_Is_Lambda) && func->captures) {
        ONYX_ERROR(func->captures->token->pos, Error_Critical, "This procedure cannot capture values as it is not defined in an expression.");
        return Check_Error;
    }

    if (func->captures && !func->scope_to_lookup_captured_values) {
        if (func->flags & Ast_Flag_Function_Is_Lambda_Inside_PolyProc) return Check_Complete;

        return Check_Yield;
    }

    b32 expect_default_param = 0;
    b32 has_had_varargs = 0;

    if (func->scope == NULL) {
        func->scope = scope_create(context, context->checker.current_scope, func->token->pos);
    }

    if (func->constraints.constraints != NULL && func->constraints.constraints_met == 0) {
        // bh_arr_each(AstConstraint *, constraint, func->constraints.constraints) {
        //     CHECK(constraint, *constraint);
        // }

        func->constraints.produce_errors = (func->flags & Ast_Flag_Header_Check_No_Error) == 0;

        OnyxToken *tkn = func->token;
        if (func->generated_from) tkn = func->generated_from;

        CHECK(constraint_context, &func->constraints, func->scope, tkn->pos);
    }

    scope_enter(context, func->scope);

    if (!mode_enabled(context, CM_Dont_Resolve_Symbols)) {
        if (func->captures) {
            CHECK(capture_block, func->captures, func->scope_to_lookup_captured_values);
        }

        bh_arr_each(AstParam, param, func->params) {
            symbol_introduce(context, context->checker.current_scope, param->local->token, (AstNode *) param->local);
        }

        //
        // We have to pre-check the type nodes of the parameters.
        bh_arr_each(AstParam, param, func->params) {
            if (param->local->type_node != NULL) {
                param->local->type_node->flags |= (func->flags & Ast_Flag_Header_Check_No_Error);
                param->local->flags |= Ast_Flag_Symbol_Invisible;
                check_type(context, &param->local->type_node);
                param->local->flags &= ~Ast_Flag_Symbol_Invisible;
            }
        }

        if (potentially_convert_function_to_polyproc(context, func)) {
            return Check_Complete;
        }
    }

    if (func->nodes_that_need_entities_after_clone && bh_arr_length(func->nodes_that_need_entities_after_clone) > 0 && func->entity) {
        bh_arr_each(AstNode *, node, func->nodes_that_need_entities_after_clone) {
            // This makes a lot of assumptions about how these nodes are being processed,
            // and I don't want to start using this with other nodes without considering
            // what the ramifications of that is.
            assert((*node)->kind == Ast_Kind_Static_If || (*node)->kind == Ast_Kind_File_Contents
                    || (*node)->kind == Ast_Kind_Function || (*node)->kind == Ast_Kind_Polymorphic_Proc);

            // Need to use current_scope->parent because current_scope is the function body scope.
            Scope *scope = context->checker.current_scope->parent;

            if ((*node)->kind == Ast_Kind_Static_If) {
                AstIf *static_if = (AstIf *) *node;
                assert(static_if->defined_in_scope);
                scope = static_if->defined_in_scope;

                if (func->poly_scope) {
                    scope = scope_create(context, scope, static_if->token->pos);
                    scope_include(context, scope, func->poly_scope, static_if->token->pos);
                }
            }

            add_entities_for_node(&context->entities, NULL, *node, scope, func->entity->package);
        }

        bh_arr_set_length(func->nodes_that_need_entities_after_clone, 0);
    }

    bh_arr_each(AstParam, param, func->params) {
        AstLocal* local = param->local;

        if (expect_default_param && param->default_value == NULL) {
            ERROR(local->token->pos,
                    "All parameters must have default values after the first default valued parameter.");
        }

        if (has_had_varargs && param->vararg_kind != VA_Kind_Not_VA) {
            ERROR(local->token->pos,
                    "Can only have one param that is of variable argument type.");
        }

        if (param->vararg_kind == VA_Kind_Untyped) {
            // HACK
            if (context->builtins.vararg_type_type == NULL)
                context->builtins.vararg_type_type = type_build_from_ast(context, context->builtins.vararg_type);

            local->type = context->builtins.vararg_type_type;
        }

        if (param->default_value != NULL) {
            if (param->vararg_kind != VA_Kind_Not_VA) {
                ERROR(local->token->pos, "Variadic arguments cannot have default values.");
            }

            CHECK(expression, &param->default_value);

            if (local->type_node == NULL && local->type == NULL) {
                local->type = resolve_expression_type(context, param->default_value);
            }

            expect_default_param = 1;
        }

        if (local->type_node != NULL) {
            // If the function has the no_error flag, then the type node should have it set too.
            // This allows for polymorphic structures with constraints to fail gracefully.
            local->type_node->flags |= (func->flags & Ast_Flag_Header_Check_No_Error);
            CHECK_INVISIBLE(type, local, &local->type_node);
        }

        fill_in_type(context, (AstTyped *) local);
        if (local->type == NULL) {
            YIELD(local->token->pos, "Waiting for parameter type to be known.");
        }

        if (local->type == (Type *) &context->node_that_signals_failure) {
            ONYX_ERROR(local->token->pos, Error_Critical, "BAD TYPE");
            return Check_Failed;
        }

        if (local->type->kind == Type_Kind_Compound) {
            ERROR(param->local->token->pos, "Compound types are not allowed as parameter types. Try splitting this into multiple parameters.");
        }

        if (param->vararg_kind != VA_Kind_Not_VA) has_had_varargs = 1;

        if (local->type->kind != Type_Kind_Array && type_size_of(local->type) == 0) {
            ERROR(local->token->pos, "Function parameters cannot have 'void' as their type.");
        }

        if (local->type->kind == Type_Kind_Array && type_size_of(local->type) >= 128) {
            ONYX_WARNING(local->token->pos, "Since arrays are passed by value, this array parameter would copy %d bytes per function call. Unless this is what you want, you should make this parameter a slice instead ('[] %s').",
                type_size_of(local->type),
                type_get_name(context, local->type->Array.elem)
            );
        }
    }

    CHECK(type, &func->return_type);

    if (func->deprecated_warning) {
        CHECK(expression, (AstTyped **) &func->deprecated_warning);
        if (func->deprecated_warning->kind != Ast_Kind_StrLit) {
            ERROR(func->token->pos, "Expected deprecation warning to be a string literal.");
        }
    }

    bh_arr_each(AstTyped *, pexpr, func->tags) {
        CHECK(expression, pexpr);

        if (((*pexpr)->flags & Ast_Flag_Comptime) == 0) {
            ERROR((*pexpr)->token->pos, "#tag expressions should be compile time known.");
        }
    }

    func->ready_for_body_to_be_checked = 1;

    func->type = type_build_function_type(context, func);
    if (func->type == NULL) {
        YIELD(func->token->pos, "Waiting for function type to be constructed");
    }

    if (func->foreign.import_name) {
        CHECK(expression, &func->foreign.module_name);
        CHECK(expression, &func->foreign.import_name);
    }

    if (context->options->stack_trace_enabled) {
        if (!func->stack_trace_local) {
            OnyxToken *stack_trace_token = bh_alloc_item(context->ast_alloc, OnyxToken);
            stack_trace_token->type = Token_Type_Symbol;
            stack_trace_token->length = 13;
            stack_trace_token->text = bh_strdup(context->ast_alloc, "__stack_trace ");
            stack_trace_token->pos = func->token->pos;

            assert(context->builtins.stack_trace_type);
            func->stack_trace_local = make_local(context, stack_trace_token, context->builtins.stack_trace_type);
            func->stack_trace_local->flags |= Ast_Flag_Decl_Followed_By_Init;
        }

        CHECK(local, &func->stack_trace_local);
    }

    scope_leave(context);

    if (bh_arr_length(func->tags) > 0 || (func->flags & Ast_Flag_Proc_Is_Null) != 0) {
        func->flags |= Ast_Flag_Has_Been_Scheduled_For_Emit;
        return Check_Success;
    }

    return Check_Complete;
}

CHECK_FUNC(memres_type, AstMemRes* memres) {
    CHECK(type, &memres->type_node);
    fill_in_type(context, (AstTyped *) memres);
    if (memres->type_node && !memres->type) YIELD(memres->token->pos, "Waiting for global type to be constructed.");

    if (bh_arr_length(memres->tags) > 0) {
        memres->flags |= Ast_Flag_Has_Been_Scheduled_For_Emit;
        return Check_Success;
    }

    return Check_Complete;
}

CHECK_FUNC(memres, AstMemRes* memres) {
    assert(memres->type_entity);
    if (memres->type_entity->state < Entity_State_Code_Gen) YIELD(memres->token->pos, "Waiting for global to pass type construction.");

    if (memres->initial_value != NULL) {
        if (memres->threadlocal) {
            ERROR(memres->token->pos, "'#thread_local' variables cannot have an initializer at the moment.");
        }

        CHECK(expression, &memres->initial_value);

        if (memres->type != NULL) {
            Type* memres_type = memres->type;
            TYPE_CHECK(&memres->initial_value, memres_type) {
                ERROR_(memres->token->pos,
                        "Cannot assign value of type '%s' to a '%s'.",
                        node_get_type_name(context, memres->initial_value),
                        type_get_name(context, memres_type));
            }

        } else {
            resolve_expression_type(context, memres->initial_value);
            if (memres->initial_value->type == NULL && memres->initial_value->entity != NULL && memres->initial_value->entity->state <= Entity_State_Check_Types) {
                YIELD(memres->token->pos, "Waiting for global type to be constructed.");
            }
            memres->type = memres->initial_value->type;
        }

        if ((memres->initial_value->flags & Ast_Flag_Comptime) == 0) {
            if (memres->initial_value->entity != NULL && memres->initial_value->entity->state <= Entity_State_Check_Types) {
                YIELD(memres->token->pos, "Waiting for initial value to be checked.");
            }

            ERROR(memres->initial_value->token->pos, "Top level expressions must be compile time known.");
        }
    }

    bh_arr_each(AstTyped *, ptag, memres->tags) {
        CHECK(expression, ptag);
    }

    if (bh_arr_length(memres->tags) > 0) {
        memres->flags |= Ast_Flag_Has_Been_Scheduled_For_Emit;
        return Check_Success;
    }

    return Check_Complete;
}

CHECK_FUNC(type, AstType** ptype) {
    if (ptype == NULL || *ptype == NULL) return Check_Success;

    AstType* type = *ptype;
    AstType* original_type = type;
    while (type->kind == Ast_Kind_Type_Alias)
        type = ((AstTypeAlias *) type)->to;
    
    if (type->kind == Ast_Kind_Symbol) {
        CHECK(symbol, (AstNode **) ptype);
        type = *ptype;
        original_type = type;
    }

    if (type->flags & Ast_Flag_Has_Been_Checked) return Check_Success;

    switch (type->kind) {
        case Ast_Kind_Basic_Type: break;

        case Ast_Kind_Typeof: {
            AstTypeOf *type_of = (AstTypeOf *) type;
            CHECK(expression, (AstTyped **) &type_of->expr);
            resolve_expression_type(context, type_of->expr);

            if (type_of->expr->type == NULL) {
                YIELD(type_of->token->pos, "Trying to check type for type-of expression.");
            }

            type_of->resolved_type = type_of->expr->type;
            break;
        }

        case Ast_Kind_Pointer_Type:
            ((AstPointerType *) type)->elem->flags |= type->flags & Ast_Flag_Header_Check_No_Error;
            CHECK(type, &((AstPointerType *) type)->elem);
            break;

        case Ast_Kind_Slice_Type:
            ((AstSliceType *) type)->elem->flags |= type->flags & Ast_Flag_Header_Check_No_Error;
            CHECK(type, &((AstSliceType *) type)->elem);
            break;

        case Ast_Kind_DynArr_Type:
            ((AstDynArrType *) type)->elem->flags |= type->flags & Ast_Flag_Header_Check_No_Error;
            CHECK(type, &((AstDynArrType *) type)->elem);
            break;

        case Ast_Kind_VarArg_Type:
            ((AstVarArgType *) type)->elem->flags |= type->flags & Ast_Flag_Header_Check_No_Error;
            CHECK(type, &((AstVarArgType *) type)->elem);
            break;

        case Ast_Kind_Multi_Pointer_Type: 
            ((AstMultiPointerType *) type)->elem->flags |= type->flags & Ast_Flag_Header_Check_No_Error;
            CHECK(type, &((AstMultiPointerType *) type)->elem);
            break;

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype = (AstFunctionType *) type;

            //
            // We have to check the parameter types here before the return type,
            // because when doing a nested polymorph lookup, the parameter types
            // need to be constructable in order to create the polymorph variant
            // and return type can be whatever (since it is replaced with void).
            //

            if (ftype->param_count > 0) {
                fori (i, 0, (i64) ftype->param_count) {
                    CHECK(type, &ftype->params[i]);
                }
            }

            CHECK(type, &ftype->return_type);
            break;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* ctype = (AstCompoundType *) type;

            bh_arr_each(AstType *, type, ctype->types) CHECK(type, type);
            break;
        }

        case Ast_Kind_Array_Type: {
            AstArrayType* atype = (AstArrayType *) type;
            CHECK(type, &atype->elem);

            if (atype->count_expr) {
                CHECK(expression, &atype->count_expr);
                resolve_expression_type(context, atype->count_expr);
            }

            break;
        }

        case Ast_Kind_Field_Access: {
            CHECK(field_access, (AstFieldAccess **) ptype);
            type = *ptype;
            original_type = type;

            if (!node_is_type((AstNode *) type)) {
                ERROR_(original_type->token->pos, "This field access did not resolve to be a type. It resolved to be a '%s'.", onyx_ast_node_kind_string(type->kind));
            }
            break;
        }

        //
        // We do not recurse down to check structs, unions and enums at this point,
        // as they should be checked separately using their entity. check_entity
        // will automatically directive Entity_Type_Type_Alias to check_struct/union
        // so we don't have to do it here.
        //
        // case Ast_Kind_Struct_Type: CHECK(struct, (AstStructType *) type)); break;
        // case Ast_Kind_Union_Type:  CHECK(union,  (AstUnionType *) type));  break;

        case Ast_Kind_Enum_Type: break;

        case Ast_Kind_Poly_Struct_Type: {
            AstPolyStructType* pst_node = (AstPolyStructType *) type;
            assert(pst_node->scope);

            if (type == context->builtins.array_type) {
                assert(((AstPolyStructType *) context->builtins.slice_type)->scope);
                pst_node->scope->parent = ((AstPolyStructType *) context->builtins.slice_type)->scope;
            }
            break;
        }

        case Ast_Kind_Poly_Union_Type: {
            AstPolyUnionType* put_node = (AstPolyUnionType *) type;
            assert(put_node->scope);
            break;
        }

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pc_node = (AstPolyCallType *) type;

            CHECK(type, &pc_node->callee);

            bh_arr_each(AstNode *, param, pc_node->params) {
                if (node_is_type(*param)) {
                    CHECK(type, (AstType **) param);
                } else {
                    CHECK(expression, (AstTyped **) param);
                    resolve_expression_type(context, (AstTyped *) *param);
                    fill_in_type(context, (AstTyped *) *param);
                }
            }

            break;
        }

        case Ast_Kind_Type_Alias: {
            AstTypeAlias *type_alias = (AstTypeAlias *) type;
            CHECK(type, &type_alias->to);
            break;
        }

        case Ast_Kind_Alias: {
            AstAlias* alias = (AstAlias *) type;
            CHECK_INVISIBLE(type, alias, (AstType **) &alias->alias);

            break;
        }

        case Ast_Kind_Distinct_Type: {
            AstDistinctType *distinct = (AstDistinctType *) type;
            CHECK(type, &distinct->base_type);
            break;
        }

        default: break;
    }

    type = original_type;

    // CLEANUP: Should Type_Alias nodes just be made comptime at creation? Since they will always be set to comptime here?
    type->flags |= Ast_Flag_Comptime;
    while (type->kind == Ast_Kind_Type_Alias) {
        type->flags |= Ast_Flag_Comptime;
        type = ((AstTypeAlias *) type)->to;
    }

    type->flags |= Ast_Flag_Has_Been_Checked;
    return Check_Success;
}

CHECK_FUNC(static_if, AstIf* static_if) {
    if (static_if->flags & Ast_Flag_Dead) return Check_Complete;

    context->checker.expression_types_must_be_known = 1;
    CheckStatus result = check_expression(context, &static_if->cond);
    context->checker.expression_types_must_be_known = 0;

    if (result == Check_Yield) return Check_Yield;

    if (result > Check_Errors_Start || !(static_if->cond->flags & Ast_Flag_Comptime)) {
        ERROR(static_if->token->pos, "Expected this condition to be compile time known.");
    }

    if (!type_is_bool(static_if->cond->type)) {
        ERROR(static_if->token->pos, "Expected this condition to be a boolean value.");
    }

    static_if->flags |= Ast_Flag_Static_If_Resolved;

    b32 resolution = static_if_resolution(context, static_if);

    if (context->options->print_static_if_results)
        bh_printf("Static if statement at %s:%d:%d resulted in %s\n",
            static_if->token->pos.filename,
            static_if->token->pos.line,
            static_if->token->pos.column,
            resolution ? "true" : "false");

    if (resolution) {
        bh_arr_each(Entity *, ent, static_if->true_entities) {
            entity_heap_insert_existing(&context->entities, *ent);
        }

    } else {
        bh_arr_each(Entity *, ent, static_if->false_entities) {
            entity_heap_insert_existing(&context->entities, *ent);
        }
    }

    return Check_Complete;
}

CHECK_FUNC(process_directive, AstNode* directive) {
    if (directive->kind == Ast_Kind_Directive_Add_Overload) {
        AstDirectiveAddOverload *add_overload = (AstDirectiveAddOverload *) directive;

        CHECK(expression, (AstTyped **) &add_overload->overloaded_function);
        if (add_overload->overloaded_function == NULL) {
            // NOTE: Error message will already be generated
            return Check_Error;
        }

        AstOverloadedFunction *ofunc = (AstOverloadedFunction *) strip_aliases((AstNode *) add_overload->overloaded_function);
        if (ofunc->kind != Ast_Kind_Overloaded_Function) {
            YIELD_ERROR_(add_overload->token->pos, "#overload directive expects a matched procedure, got '%s'.",
                        onyx_ast_node_kind_string(ofunc->kind));
        }

        if (ofunc->locked) {
            ONYX_ERROR(add_overload->token->pos, Error_Critical, "Cannot add match option here as the original #match was declared as #locked.");
            ONYX_ERROR(ofunc->token->pos, Error_Critical, "Here is the original #match.");
            return Check_Error;
        }

        if (ofunc->only_local_functions) {
            if (!token_same_file(add_overload->token, ofunc->token)) {
                ONYX_ERROR(add_overload->token->pos, Error_Critical, "Cannot add match option here as this option is not within the same file as the original #match declared with #local.");
                ONYX_ERROR(ofunc->token->pos, Error_Critical, "Here is the original #match.");
                return Check_Error;
            }
        }

        AstKind kind = add_overload->overload->kind;
        if (kind != Ast_Kind_Function && kind != Ast_Kind_Polymorphic_Proc && kind != Ast_Kind_Overloaded_Function && kind != Ast_Kind_Macro) {
            // This check could be converted to something like `is_node_function_like()`?
            CHECK(expression, (AstTyped **) &add_overload->overload);
        }

        add_overload->overload->flags &= ~Ast_Flag_Function_Is_Lambda;

        add_overload_option(&ofunc->overloads, add_overload->order, add_overload->overload);
        return Check_Success;
    }

    if (directive->kind == Ast_Kind_Directive_Operator) {
        AstDirectiveOperator *operator = (AstDirectiveOperator *) directive;
        CHECK(expression, &operator->overload);
        if (!operator->overload) {
            return Check_Error;
        }

        AstFunction* overload = get_function_from_node((AstNode *) operator->overload);
        if (overload == NULL) {
            ERROR(operator->token->pos, "This cannot be used as an operator overload.");
        }

        overload->flags &= ~Ast_Flag_Function_Is_Lambda;
        
        // First try unary operator overloading
        // CLEANUP This is not written well at all...
        if (operator->operator == Binary_Op_Count) {
            if (bh_arr_length(overload->params) != 1) {
                ERROR(operator->token->pos, "Expected exactly 1 argument for unary operator overload.");
            }

            UnaryOp unop = Unary_Op_Count;
            if (operator->operator_token->type == (TokenType) '?') {
                unop = Unary_Op_Try;
            }

            if (operator->operator_token->type == (TokenType) '!') {
                unop = Unary_Op_Unwrap;
            }

            if (unop == Unary_Op_Count) {
                ERROR(operator->token->pos, "Unknown operator.");
            }

            add_overload_option(&context->unary_operator_overloads[unop], operator->order, operator->overload);
            return Check_Success;
        }

        if (operator->operator != Binary_Op_Subscript_Equals && bh_arr_length(overload->params) != 2) {
            ERROR(operator->token->pos, "Expected exactly 2 arguments for binary operator overload.");
        }

        add_overload_option(&context->operator_overloads[operator->operator], operator->order, operator->overload);
        return Check_Success;
    }

    if (directive->kind == Ast_Kind_Directive_Export) {
        AstDirectiveExport *export = (AstDirectiveExport *) directive;
        CHECK(expression, &export->export);
        CHECK(expression, &export->export_name_expr);

        AstTyped *exported = export->export;

        if (exported->kind == Ast_Kind_Polymorphic_Proc) {
            ERROR(export->token->pos, "Cannot export a polymorphic function.");
        }

        if (exported->kind == Ast_Kind_Function) {
            AstFunction *func = (AstFunction *) export->export;
            func->is_exported = 1;

            if (func->is_foreign) {
                ERROR(export->token->pos, "Cannot export a foreign function.");
            }

            if (func->is_intrinsic) {
                ERROR(export->token->pos, "Cannot export an intrinsic function.");
            }
        }

        if (exported->entity && exported->entity->state <= Entity_State_Check_Types)
            YIELD(directive->token->pos, "Waiting for exported type to be known.");

        if (exported->kind != Ast_Kind_Function) {
            ONYX_ERROR(export->token->pos, Error_Critical, "Cannot export something that is not a procedure.");
            ERROR(exported->token->pos, "Here is the thing being exported that is not a procedure.");
        }

        if (export->export_name_expr->kind != Ast_Kind_StrLit) {
            ERROR_(export->token->pos, "Expected export name to be a string literal, got '%s'.", onyx_ast_node_kind_string(export->export_name_expr->kind));
        }

        export->export_name = export->export_name_expr->token;

        AstFunction *exported_func = (AstFunction *) export->export;
        if (exported_func->exported_name == NULL) {
            exported_func->exported_name = export->export_name;
        }

        return Check_Success;
    }

    if (directive->kind == Ast_Kind_Directive_Init) {
        AstDirectiveInit *init = (AstDirectiveInit *) directive;
        if ((init->flags & Ast_Flag_Has_Been_Checked) == 0) {
            CHECK(expression, &init->init_proc);

            if (init->init_proc->kind != Ast_Kind_Function) {
                ERROR_(init->token->pos, "#init only works for functions, got '%s'", onyx_ast_node_kind_string(init->init_proc->kind));
            }

            assert(init->init_proc->type);
            if (init->init_proc->type->Function.param_count != 0) {
                ERROR(init->token->pos, "#init expects a function that takes 0 arguments.");
            }
        }

        init->flags |= Ast_Flag_Has_Been_Checked;

        if (init->dependencies) {
            i32 i = 0;
            bh_arr_each(AstDirectiveInit *, dependency, init->dependencies) {
                enable_mode(context, CM_Allow_Init_Expressions);
                CHECK(expression, (AstTyped **) dependency);
                disable_mode(context, CM_Allow_Init_Expressions);
                
                AstTyped *d = (AstTyped *) strip_aliases((AstNode *) *dependency);
                if (d->kind != Ast_Kind_Directive_Init) {
                    ERROR_(init->token->pos, "All dependencies of an #init must be another #init. The %d%s dependency was not.", i + 1, bh_num_suffix(i + 1));
                }

                assert(d->entity);
                if (d->entity->state != Entity_State_Finalized) {
                    YIELD(init->token->pos, "Circular dependency in #init nodes. Here are the nodes involved.");
                }

                i++;
            }
        }

        bh_arr_push(context->builtins.init_procedures, (AstFunction *) init->init_proc);
        return Check_Complete;
    }

    if (directive->kind == Ast_Kind_Directive_Library) {
        AstDirectiveLibrary *library = (AstDirectiveLibrary *) directive;
        CHECK(expression, &library->library_symbol);

        if (library->library_symbol->kind != Ast_Kind_StrLit) {
            ERROR_(library->token->pos, "#library directive expected compile-time known string for library name. Got '%s'.",
                onyx_ast_node_kind_string(library->library_symbol->kind));
        }

        AstStrLit *symbol = (AstStrLit *) library->library_symbol;
        char* temp_name     = bh_alloc_array(context->scratch_alloc, char, symbol->token->length);
        i32   temp_name_len = string_process_escape_seqs(temp_name, symbol->token->text, symbol->token->length);
        library->library_name = bh_strdup(context->gp_alloc, temp_name);
        return Check_Success;
    }

    if (directive->kind == Ast_Kind_Injection) {
        AstInjection *inject = (AstInjection *) directive;

        if (inject->dest == NULL) {
            if (inject->full_loc == NULL) return Check_Error;

            AstTyped *full_loc = (AstTyped *) strip_aliases((AstNode *) inject->full_loc);

            if (full_loc->kind != Ast_Kind_Field_Access) {
                ERROR(inject->token->pos, "#inject expects a dot expression (a.b) for the injection point.");
                return Check_Error;
            }

            AstFieldAccess *acc = (AstFieldAccess *) full_loc;
            inject->dest = acc->expr;
            inject->symbol = acc->token;
        }

        //
        // We do not "properly" handle the check status of this function here, because
        // we actually don't care if it is completely done type checking. We only care
        // if we can get a scope from it. We are effectively just using this call as a
        // means to resolve the symbols in the destination
        //
        check_expression(context, &inject->dest);

        Scope *scope = get_scope_from_node_or_create(context, (AstNode *) inject->dest);
        if (scope == NULL) {
            YIELD_ERROR(inject->token->pos, "Cannot #inject here.");
        }

        inject->binding->token = inject->symbol;

        if (inject->binding->kind == Ast_Kind_Function || inject->binding->kind == Ast_Kind_Polymorphic_Proc) {
            AstFunction *func = (void *) inject->binding;
            func->name = generate_name_within_scope(context, scope, inject->symbol);
        }

        Package *pac = NULL;
        if (inject->dest->kind == Ast_Kind_Package) {
            pac = ((AstPackage *) inject->dest)->package;
        } else {
            pac = context->checker.current_entity->package;
        }

        add_entities_for_node(&context->entities, NULL, (AstNode *) inject->binding, scope, pac);
        return Check_Complete;
    }

    if (directive->kind == Ast_Kind_Directive_This_Package) {
        AstPackage *package = (AstPackage *) directive;
        package->kind = Ast_Kind_Package;
        package->package = context->checker.current_entity->package;
        return Check_Complete;
    }

    if (directive->kind == Ast_Kind_Directive_Wasm_Section) {
        AstDirectiveWasmSection *section = (AstDirectiveWasmSection *) directive;

        CHECK(expression, &section->section_name);
        CHECK(expression, &section->section_contents);

        if (section->section_name->kind     != Ast_Kind_StrLit) ERROR(section->token->pos, "Expect section name to be a compile-time known string.");
        if (section->section_contents->kind != Ast_Kind_StrLit) ERROR(section->token->pos, "Expect section contents to be a compile-time known string.");

        AstStrLit *symbol = (AstStrLit *) section->section_name;
        char* temp_str    = bh_alloc_array(context->scratch_alloc, char, symbol->token->length);
        string_process_escape_seqs(temp_str, symbol->token->text, symbol->token->length);
        section->name = bh_strdup(context->gp_alloc, temp_str);

        symbol   = (AstStrLit *) section->section_contents;
        temp_str = bh_alloc_array(context->scratch_alloc, char, symbol->token->length + 1);
        u32 content_length = string_process_escape_seqs(temp_str, symbol->token->text, symbol->token->length);

        if (section->from_file) {
            const char *containing_filename = section->token->pos.filename;
            char *parent_folder = bh_path_get_parent(containing_filename, context->scratch_alloc);

            char *path = bh_strdup(
                context->scratch_alloc,
                bh_lookup_file(temp_str, parent_folder, NULL, NULL, NULL, context->scratch_alloc)
            );

            if (!bh_file_exists(path)) {
                ERROR_(section->token->pos, "Failed to open file '%s' for custom section.", path);
            }

            bh_file_contents contents = bh_file_read_contents(context->gp_alloc, path);
            section->contents = contents.data;
            section->length = contents.length;
        } else {
            section->contents = bh_strdup(context->gp_alloc, temp_str);
            section->length = content_length;
        }

        return Check_Success;
    }

    assert("Bad directive in check_process_directive" && 0);

    return Check_Success;
}

CHECK_FUNC(macro, AstMacro* macro) {
    macro->flags |= Ast_Flag_Comptime;

    if (macro->body->kind == Ast_Kind_Function) {
        CHECK(function_header, (AstFunction *) macro->body);
    }
    else if (macro->body->kind == Ast_Kind_Polymorphic_Proc) {
        CHECK(polyproc, (AstFunction *) macro->body);
    }

    return Check_Success;
}

CHECK_FUNC(interface, AstInterface *interface) {
    bh_arr_each(InterfaceParam, param, interface->params) {
        CHECK(type, &param->value_type);

        param->type = type_build_from_ast(context, param->value_type);
        if (!param->type) {
            YIELD(param->value_type->token->pos, "Waiting for interface parameter's type to be constructed.");
        }
    }

    return Check_Success;
}

CHECK_FUNC(interface_constraint, AstConstraint *constraint) {
    if (constraint->interface->kind != Ast_Kind_Interface) {
        // CLEANUP: This error message might not look totally right in some cases.
        ERROR_(constraint->token->pos, "'%b' is not an interface. It is a '%s'.",
            constraint->token->text, constraint->token->length,
            onyx_ast_node_kind_string(constraint->interface->kind));
    }

    // #intrinsic interfaces
    if (constraint->interface->is_intrinsic) {
        b32 success = resolve_intrinsic_interface_constraint(context, constraint);
        if (success) {
            *constraint->report_status = Constraint_Check_Status_Success;
            return Check_Complete;
        } else {
            *constraint->report_status = Constraint_Check_Status_Failed;
            return Check_Failed;
        }
    }

    bh_arr_new(context->gp_alloc, constraint->exprs, bh_arr_length(constraint->interface->exprs));
    bh_arr_each(InterfaceConstraint, ic, constraint->interface->exprs) {
        InterfaceConstraint new_ic = {0};
        new_ic.expr = (AstTyped *) ast_clone(context, (AstNode *) ic->expr);
        new_ic.expected_type_expr = (AstType *) ast_clone(context, (AstNode *) ic->expected_type_expr);
        new_ic.invert_condition = ic->invert_condition;
        bh_arr_push(constraint->exprs, new_ic);
    }

    assert(constraint->interface->entity && constraint->interface->entity->scope);
    assert(constraint->interface->scope);
    assert(constraint->interface->scope->parent == constraint->interface->entity->scope);

    if (constraint->scope == NULL) {
        constraint->scope = scope_create(context, constraint->interface->scope, constraint->token->pos);
    }

    if (bh_arr_length(constraint->args) != bh_arr_length(constraint->interface->params)) {
        ERROR_(constraint->token->pos, "Wrong number of arguments given to interface. Expected %d, got %d.",
            bh_arr_length(constraint->interface->params),
            bh_arr_length(constraint->args));
    }

    fori (i, 0, bh_arr_length(constraint->interface->params)) {
        InterfaceParam *ip = &constraint->interface->params[i];

        AstTyped **arg = &constraint->args[i];
        CHECK(expression, arg);

        TYPE_CHECK(arg, ip->type) {
            ERROR_((*arg)->token->pos, "Mismatched type in interface construction. Expected something of type '%s', but got something of type '%s'.", type_get_name(context, ip->type), type_get_name(context, (*arg)->type));
        }

        AstAlias *type_alias = onyx_ast_node_new(context->ast_alloc, sizeof(AstAlias), Ast_Kind_Alias);
        type_alias->token = ip->value_token;
        type_alias->alias = *arg;

        symbol_introduce(context, constraint->scope, ip->value_token, (AstNode *) type_alias);
    }

    fori (i, 0, bh_arr_length(constraint->interface->sentinels)) {
        InterfaceSentinel *is = &constraint->interface->sentinels[i];

        AstTyped *sentinel = onyx_ast_node_new(context->ast_alloc, sizeof(AstTyped), Ast_Kind_Constraint_Sentinel);
        sentinel->token = is->name;
        sentinel->type_node = (AstType *) ast_clone(context, (AstNode *) is->type);

        symbol_introduce(context, constraint->scope, is->name, (AstNode *) sentinel);
    }

    assert(constraint->entity);
    constraint->entity->scope = constraint->scope;

    constraint->phase = Constraint_Phase_Checking_Expressions;
    return Check_Yield;
}

CHECK_FUNC(expression_constraint, AstConstraint *constraint) {
    onyx_errors_enable(context);

    AstTyped* expr = constraint->const_expr;

    context->checker.expression_types_must_be_known = 1;
    CheckStatus result = check_expression(context, &expr);
    context->checker.expression_types_must_be_known = 0;

    if (result == Check_Yield) return Check_Yield;

    if (result > Check_Errors_Start || !(expr->flags & Ast_Flag_Comptime)) {
        ERROR(expr->token->pos, "Where clauses must be a constant expressions.");
    }

    if (!type_is_bool(expr->type)) {
        ERROR(expr->token->pos, "Where clauses must result in a boolean.");
    }

    b32 value = (b32)get_expression_integer_value(context, expr, NULL);
    if (!value) {
        *constraint->report_status = Constraint_Check_Status_Failed;
        return Check_Failed;
    }

    expr = (AstTyped *) make_bool_literal(context, 1);
    *constraint->report_status = Constraint_Check_Status_Success;

    return Check_Complete;
}

CHECK_FUNC(constraint, AstConstraint *constraint) {
    switch (constraint->phase) {
        // nocheckin
        case Constraint_Phase_Waiting_To_Be_Queued: return Check_Success;

        case Constraint_Phase_Cloning_Expressions: {
            CHECK(expression, (AstTyped **) &constraint->interface);

            bh_arr_each(AstTyped *, arg, constraint->args) {
                CHECK(expression, arg);
            }

            if (constraint->flags & Ast_Flag_Constraint_Is_Expression) {
                return check_expression_constraint(context, constraint);
            }
            else {
                return check_interface_constraint(context, constraint);
            }
        }

        case Constraint_Phase_Checking_Expressions: {
            onyx_errors_disable(context);

            fori (i, constraint->expr_idx, bh_arr_length(constraint->exprs)) {
                InterfaceConstraint* ic = &constraint->exprs[i];

                CheckStatus cs = check_expression(context, &ic->expr);
                if (cs == Check_Yield) {
                    onyx_errors_enable(context);
                    return cs;
                }

                if (cs == Check_Error && !ic->invert_condition) {
                    goto constraint_error;
                }

                if (cs == Check_Success && ic->invert_condition) {
                    goto constraint_error;
                }

                if (ic->expected_type_expr) {
                    cs = check_type(context, &ic->expected_type_expr);
                    if (cs == Check_Yield) {
                        onyx_errors_enable(context);
                        return cs;
                    }

                    ic->expected_type = type_build_from_ast(context, ic->expected_type_expr);
                    if (ic->expected_type == NULL) {
                        //
                        // To make interfaces easier to use, I wanted to have
                        // the ability to easily specify the return type as a
                        // polymorphic structure, without needing to support
                        // some crazy syntax like "-> Iterator($V)". For this
                        // reason, I allow you to say "-> Iterator" in the
                        // expected return type. Normally type_build_from_ast
                        // does not return an instance of a polymorphic structure.
                        // This prevents needing to handle that case across
                        // the entire compiler. HOWEVER, in this case I actually
                        // do want to get the polymorphic structure type if
                        // it was one. So, I check here to see if the expected_type_expr
                        // node has a type_id assigned. If it does, then there
                        // is a chance it is a polymorphic struct. This bypasses
                        // the usual code and looks up the type directly, from
                        // then it will be used in the TYPE_CHECK below.
                        if (ic->expected_type_expr->type_id) {
                            ic->expected_type = type_lookup_by_id(context, ic->expected_type_expr->type_id);

                        } else {
                            onyx_errors_enable(context);
                            YIELD_ERROR(ic->expected_type_expr->token->pos, "Waiting on expected type expression to be resolved.");
                        }
                    }

                    TYPE_CHECK(&ic->expr, ic->expected_type) {
                        if (!ic->invert_condition) {
                            ic->error_msg = bh_aprintf(context->gp_alloc, "Expected expression to be of type %s, got expression of type %s.",
                                    type_get_name(context, ic->expected_type), type_get_name(context, ic->expr->type));
                            goto constraint_error;
                        }
                    }
                }

                constraint->expr_idx++;
                continue;

              constraint_error:
                onyx_errors_enable(context);
                *constraint->report_status = Constraint_Check_Status_Failed;
                return Check_Failed;
            }

            // HACK HACK HACK
            onyx_errors_enable(context);
            *constraint->report_status = Constraint_Check_Status_Success;
            return Check_Complete;
        }

        default: break;
    }

    onyx_errors_enable(context);
    return Check_Success;
}

CHECK_FUNC(constraint_context, ConstraintContext *cc, Scope *scope, OnyxFilePos pos) {
    if (cc->constraint_checks) {
        if (cc->constraints_met == 1) return Check_Success;

        fori (i, 0, bh_arr_length(cc->constraints)) {
            if (cc->constraint_checks[i] == Constraint_Check_Status_Failed) {
                if (cc->produce_errors) {
                    AstConstraint *constraint = cc->constraints[i];
                    char constraint_map[512] = {0};
                    fori (i, 0, bh_arr_length(constraint->args)) {
                        if (!node_is_type((AstNode *) constraint->args[i])) {
                            continue;
                        }

                        if (i != 0) strncat(constraint_map, ", ", 511);

                        OnyxToken* symbol = constraint->interface->params[i].value_token;
                        token_toggle_end(symbol);
                        strncat(constraint_map, symbol->text, 511);
                        token_toggle_end(symbol);

                        strncat(constraint_map, " is of type '", 511);
                        strncat(constraint_map, type_get_name(context, type_build_from_ast(context, (AstType *) constraint->args[i])), 511);
                        strncat(constraint_map, "'", 511);
                    }

                    OnyxFilePos error_pos;
                    char *error_msg = NULL;
                    if (constraint->exprs) {
                        error_pos = constraint->exprs[constraint->expr_idx].expr->token->pos;
                        error_msg = constraint->exprs[constraint->expr_idx].error_msg;
                    } else {
                        error_pos = constraint->interface->token->pos;
                    }

                    if (constraint->flags & Ast_Flag_Constraint_Is_Expression) {
                        ONYX_ERROR(error_pos, Error_Critical, "Where clause did not evaluate to true.");
                    }
                    else {
                        ONYX_ERROR(error_pos, Error_Critical, "Failed to satisfy constraint where %s.", constraint_map);
                    }

                    if (error_msg) {
                        ONYX_ERROR(error_pos, Error_Critical, error_msg);
                    }

                    if (!(constraint->flags & Ast_Flag_Constraint_Is_Expression)) {
                        ONYX_ERROR(constraint->token->pos, Error_Critical, "Here is where the interface was used.");
                    }

                    ONYX_ERROR(pos, Error_Critical, "Here is the code that caused this constraint to be checked.");

                    return Check_Error;

                } else {
                    // If no error are suppose to be produced, we still need to signal that
                    // the node reached a completed state.
                    return Check_Failed;
                }
            }

            if (cc->constraint_checks[i] == Constraint_Check_Status_Queued) {
                YIELD(pos, "Waiting for constraints to be checked.");
            }
        }

        cc->constraints_met = 1;
        return Check_Success;

    } else {
        u32 count = bh_arr_length(cc->constraints);
        ConstraintCheckStatus *ccs = bh_alloc_array(context->ast_alloc, ConstraintCheckStatus, count);

        cc->constraint_checks = ccs;

        fori (i, 0, count) {
            ccs[i] = Constraint_Check_Status_Queued;
            cc->constraints[i]->report_status = &ccs[i];
            cc->constraints[i]->phase = Constraint_Phase_Cloning_Expressions;

            add_entities_for_node(&context->entities, NULL, (AstNode *) cc->constraints[i], scope, NULL);
        }

        return Check_Yield;
    }
}

CHECK_FUNC(polyquery, AstPolyQuery *query) {
    if (query->function_header->scope == NULL)
        query->function_header->scope = scope_create(context, query->proc->parent_scope_of_poly_proc, query->token->pos);

    enable_mode(context, CM_Dont_Resolve_Symbols);
    check_temp_function_header(context, query->function_header);
    disable_mode(context, CM_Dont_Resolve_Symbols);

    scope_enter(context, query->function_header->scope);

    {
        u32 idx = 0;
        bh_arr_each(AstParam, param, query->function_header->params) {
            bh_arr_each(AstPolyParam, pp, query->proc->poly_params) {
                if (pp->kind == PPK_Baked_Value && pp->idx == idx) goto skip_introducing_symbol;
            }

            symbol_introduce(context, context->checker.current_scope, param->local->token, (AstNode *) param->local);

        skip_introducing_symbol:
            idx++;
        }
    }

    b32 solved_something = 0;
    bh_arr_each(AstParam, param, query->function_header->params) {
        if (param->local->type_node != NULL) {
            context->checker.resolved_a_symbol = 0;

            onyx_errors_disable(context);
            param->local->flags |= Ast_Flag_Symbol_Invisible;
            check_type(context, &param->local->type_node);
            param->local->flags &= ~Ast_Flag_Symbol_Invisible;
            onyx_errors_enable(context);
            
            if (context->checker.resolved_a_symbol) {
                solved_something = 1;
            }
        }
    }

    i32 solved_count = 0;
    OnyxError err_msg = { 0 };

    bh_arr_each(AstPolyParam, param, query->proc->poly_params) {
        AstPolySolution sln;
        bh_arr_each(AstPolySolution, solved_sln, query->slns) {
            if (token_equals(param->poly_sym->token, solved_sln->poly_sym->token)) {
                goto poly_query_done;
            }
        }

        // CLEANUP: I think this can go away because it is already done in polymorph.c
        bh_arr_each(AstPolySolution, known_sln, query->proc->known_slns) {
            if (token_equals(param->poly_sym->token, known_sln->poly_sym->token)) {
                sln = *known_sln;
                goto poly_var_solved;
            }
        }

        TypeMatch result = find_polymorphic_sln(context, &sln, param, query->function_header, query->pp_lookup, query->given, &err_msg);

        switch (result) {
            case TYPE_MATCH_SUCCESS:
                goto poly_var_solved;

            case TYPE_MATCH_SPECIAL:
                return Check_Yield;

            case TYPE_MATCH_YIELD:
            case TYPE_MATCH_FAILED: {
                if (solved_something) continue;

                if (query->error_on_fail || context->cycle_detected) {
                    ONYX_ERROR(query->token->pos, Error_Critical, "Error solving for polymorphic variable '%b'.", param->poly_sym->token->text, param->poly_sym->token->length);
                    if (err_msg.text != NULL) onyx_submit_error(context, err_msg);
                    if (query->error_loc) ONYX_ERROR(query->error_loc->pos, Error_Critical, "Here is where the call is located."); // :ErrorMessage
                }

                return Check_Failed;
            }
        }

poly_var_solved:
        solved_something = 1;
        bh_arr_push(query->slns, sln);
        insert_poly_sln_into_scope(context, query->function_header->scope, &sln);

poly_query_done:
        solved_count += 1;
    }

    scope_leave(context);

    if (solved_count != bh_arr_length(query->proc->poly_params)) {
        if (solved_something) {
            return Check_Yield;
        } else {
            return Check_Failed;
        }
    }

    return Check_Complete;
}

CHECK_FUNC(arbitrary_job, EntityJobData *job) {
    TypeMatch result = job->func(context, job->job_data);

    switch (result) {
        case TYPE_MATCH_SUCCESS: return Check_Complete;
        case TYPE_MATCH_FAILED:  return Check_Error;
        case TYPE_MATCH_YIELD:   return Check_Yield;
        case TYPE_MATCH_SPECIAL: return Check_Yield;
    }

    return Check_Error;
}

CHECK_FUNC(js_node, AstJsNode *js) {
    if (js->order_expr) {
        CHECK(expression, &js->order_expr);

        TYPE_CHECK(&js->order_expr, context->types.basic[Basic_Kind_I32]) {
            ERROR_(js->token->pos, "Expected an expression of type 'i32' for '#order', but got a '%s' instead.", type_get_name(context, js->order_expr->type));
        }

        b32 valid = 0;
        i64 value = get_expression_integer_value(context, js->order_expr, &valid);
        assert(valid);

        js->order = (u32) value;
    }

    if (js->code) {
        CHECK(expression, &js->code);
        if (js->code->kind != Ast_Kind_StrLit) {
            ERROR(js->token->pos, "Expected the provided code to be a string-literal, but it was not.");
        }
    }

    if (js->filepath) {
        CHECK(expression, &js->filepath);
        if (js->filepath->kind != Ast_Kind_StrLit) {
            ERROR(js->token->pos, "Expected the provided file path to be a string-literal, but it was not.");
        }
    }

    return Check_Success;
}

CHECK_FUNC(file_contents, AstFileContents* fc) {
    CHECK(expression, &fc->filename_expr);

    if (fc->filename_expr->kind != Ast_Kind_StrLit) {
        ERROR(fc->token->pos, "Expected given expression to be a compile-time stirng literal.");
    }

    if (context->options->no_file_contents) {
        ERROR(fc->token->pos, "#file_contents is disabled for this compilation.");
    }

    return Check_Complete;
}

CHECK_FUNC(foreign_block, AstForeignBlock *fb) {
    if (fb->scope == NULL)
        fb->scope = scope_create(context, context->checker.current_scope, fb->token->pos);

    CHECK(expression, &fb->module_name);

    if (fb->module_name->kind != Ast_Kind_StrLit) {
        ERROR(fb->token->pos, "Expected module name to be a compile-time string literal.");
    }

    bh_arr_each(Entity *, pent, fb->captured_entities) {
        Entity *ent = *pent;
        if (ent->type == Entity_Type_Function_Header) {
            if (ent->function->body->next != NULL) {
                ERROR(ent->function->token->pos, "Procedures declared in a #foreign block should not have bodies.");
            }

            ent->function->foreign.import_name = (AstTyped *) make_string_literal(context, ent->function->intrinsic_name);
            ent->function->foreign.module_name = fb->module_name;
            ent->function->is_foreign = 1;
            ent->function->is_foreign_dyncall = fb->uses_dyncall;
            ent->function->entity = NULL;
            ent->function->entity_header = NULL;
            ent->function->entity_body = NULL;

            add_entities_for_node(&context->entities, NULL, (AstNode *) ent->function, ent->scope, ent->package);
            continue;
        }

        if (ent->type == Entity_Type_Binding) {
            AstBinding* new_binding = onyx_ast_node_new(context->ast_alloc, sizeof(AstBinding), Ast_Kind_Binding);
            new_binding->token = ent->binding->token;
            new_binding->node = ent->binding->node;

            Entity e;
            memset(&e, 0, sizeof(e));
            e.type = Entity_Type_Binding;
            e.state = Entity_State_Introduce_Symbols;
            e.binding = new_binding;
            e.scope = fb->scope;
            e.package = ent->package;

            entity_heap_insert(&context->entities, e);
        }

        if (ent->type != Entity_Type_Function) {
            entity_heap_insert_existing(&context->entities, ent);
        }
    }

    if (context->options->generate_foreign_info) {
        // When generating foreign info, we have to pass this on to codegen
        // so it can build the static data that goes in the binary.
        return Check_Success;

    } else {
        return Check_Complete;
    }
}

CHECK_FUNC(include, AstInclude* include) {
    if (include->name != NULL) return Check_Goto_Parse;

    CHECK(expression, &include->name_node);

    if (include->name_node->kind != Ast_Kind_StrLit) {
        ERROR_(include->token->pos, "Expected compile-time known string literal here. Got '%s'.", onyx_ast_node_kind_string(include->name_node->kind));
    }

    OnyxToken* str_token = include->name_node->token;
    if (str_token != NULL) {
        token_toggle_end(str_token);
        include->name = bh_strdup(context->ast_alloc, str_token->text);
        string_process_escape_seqs(include->name, include->name, strlen(include->name));
        token_toggle_end(str_token);
    }

    return Check_Goto_Parse;
}

CHECK_FUNC(import, AstImport* import) {
    AstPackage* package = import->imported_package;
    CHECK(package, package);

    if (import->import_package_itself) {
        OnyxToken *name = bh_arr_last(package->path);
        name = import->qualified_package_name ? import->qualified_package_name : name;

        symbol_introduce(context, context->checker.current_entity->scope, name, (AstNode *) package);
    }

    if (import->specified_imports) {
        package_track_use_package(context, package->package, import->entity);

        Scope *import_scope = package->package->scope;
        if (import_scope == context->checker.current_scope) return Check_Complete;

        // use X { * }
        if (import->only == NULL) {
            OnyxFilePos pos = import->token->pos;
            scope_include(context, context->checker.current_scope, import_scope, pos);
            return Check_Complete;
        }


        // use X { a, b, c }
        bh_arr_each(QualifiedImport, qi, import->only) {
            AstNode* imported = symbol_resolve(context, import_scope, qi->symbol_name);
            if (imported == NULL) {
                YIELD_(qi->symbol_name->pos,
                        "The symbol '%b' was not found package '%s'.",
                        qi->symbol_name->text, qi->symbol_name->length, package->package->name);
            }

            symbol_introduce(context, context->checker.current_scope, qi->as_name, imported);
        }
    }

    return Check_Complete;
}

CHECK_FUNC(compiler_extension, AstCompilerExtension *ext) {
    if (context->options->no_compiler_extensions) {
        ERROR(ext->token->pos, "Compiler extensions are disabled in this compilation.");
    }

    token_toggle_end(ext->name);
    TypeMatch status = compiler_extension_start(context, ext->name->text, ext->token->pos.filename, context->checker.current_entity, &ext->extension_id);
    token_toggle_end(ext->name);

    if (status == TYPE_MATCH_FAILED) {
        ERROR(ext->token->pos, "Failed to initialize this compiler extension.");
    }

    if (status == TYPE_MATCH_YIELD) {
        return Check_Yield;
    }

    return Check_Complete;
}

CHECK_FUNC(proc_expansion, AstProceduralExpansion **pexp, ProceduralMacroExpansionKind exp_kind) {
    AstProceduralExpansion *exp = *pexp;
    CHECK(expression, &exp->proc_macro);

    exp->proc_macro = (AstTyped *) strip_aliases((AstNode *) exp->proc_macro);

    if (exp->proc_macro->kind != Ast_Kind_Procedural_Macro) {
        YIELD_ERROR_(exp->token->pos, "Procedural macro expansion expected a procedural macro before the '!', but got '%s' instead.",
                onyx_ast_node_kind_string(exp->proc_macro->kind));
    }

    AstProceduralMacro *proc_macro = (AstProceduralMacro *) exp->proc_macro;

    token_toggle_end(proc_macro->token);
    // HACK store this differently so a copy is not necessary here.
    char *macro_name = bh_strdup(context->scratch_alloc, proc_macro->token->text);
    token_toggle_end(proc_macro->token);

    AstNode *expansion = NULL;

    TypeMatch expansion_state = compiler_extension_expand_macro(
        context,
        proc_macro->extension->extension_id,
        exp_kind,
        macro_name,
        exp->expansion_body,
        context->checker.current_entity,
        &expansion,
        &exp->expansion_id,
        context->cycle_almost_detected > 0);

    if (expansion_state == TYPE_MATCH_FAILED) {
        ERROR(exp->token->pos, "Procedural macro expansion failed. See other errors generated by procedural macro.");
    }

    if (expansion_state == TYPE_MATCH_YIELD) {
        return Check_Yield;
    }

    if (expansion == NULL) {
        if (exp_kind == PMEK_Expression) {
            ERROR(exp->token->pos, "Expected this procedural macro to expand to an expression, but it expanded to nothing.");
        }

        if (exp_kind == PMEK_Statement) {
            *(AstNode **) pexp = exp->next;
            CHECK(expression, (AstTyped **) pexp);
            return Check_Success;
        }

        // Top-level expansions do not turn into nodes, but instead will become other entities
        // that will get queued separately.
        return Check_Complete;
    }

    // Stitch the expansion into the tree.
    AstNode *last_expanded_node = expansion;
    while (last_expanded_node->next != NULL) last_expanded_node = last_expanded_node->next;
    last_expanded_node->next = (*pexp)->next;

    *pexp = (AstProceduralExpansion *) expansion;
    switch (exp_kind) {
        case PMEK_Expression: CHECK(expression, (AstTyped **) pexp); break;
        case PMEK_Statement:  CHECK(statement, (AstNode **) pexp); break;
        case PMEK_Top_Level:  return Check_Complete;
    }

    return Check_Success;
}

void check_entity(Context *context, Entity* ent) {
    CheckStatus cs = Check_Success;

    context->checker.current_entity = ent;
    context->checker.all_checks_are_final = 1;

    bh_arr_clear(context->checker.scope_stack);
    clear_modes(context);

    if (ent->scope) scope_enter(context, ent->scope);

    switch (ent->type) {
        case Entity_Type_Binding: {
            symbol_introduce(context, context->checker.current_scope, ent->binding->token, ent->binding->node);
            track_documentation_for_symbol_info(context, ent->binding->node, ent->binding);

            onyx_docs_submit(context->doc_info, ent->binding);

            package_reinsert_use_packages(context, ent->package);

            cs = Check_Complete;
            break;
        }

        case Entity_Type_Load_Path:
        case Entity_Type_Load_File:                cs = check_include(context, ent->include); break;

        case Entity_Type_Foreign_Function_Header:
        case Entity_Type_Function_Header:          cs = check_function_header(context, ent->function); break;
        case Entity_Type_Temp_Function_Header:     cs = check_temp_function_header(context, ent->function); break;
        case Entity_Type_Function:                 cs = check_function(context, ent->function); break;
        case Entity_Type_Overloaded_Function:      cs = check_overloaded_function(context, ent->overloaded_function); break;
        case Entity_Type_Global_Header:            cs = check_global_header(context, ent->global); break;
        case Entity_Type_Global:                   cs = check_global(context, ent->global); break;
        case Entity_Type_Struct_Member_Default:    cs = check_struct_defaults(context, (AstStructType *) ent->type_alias); break;
        case Entity_Type_Memory_Reservation_Type:  cs = check_memres_type(context, ent->mem_res); break;
        case Entity_Type_Memory_Reservation:       cs = check_memres(context, ent->mem_res); break;
        case Entity_Type_Static_If:                cs = check_static_if(context, ent->static_if); break;
        case Entity_Type_Macro:                    cs = check_macro(context, ent->macro); break;
        case Entity_Type_Constraint_Check:         cs = check_constraint(context, ent->constraint); break;
        case Entity_Type_Polymorphic_Proc:         cs = check_polyproc(context, ent->poly_proc); break;
        case Entity_Type_Polymorph_Query:          cs = check_polyquery(context, ent->poly_query); break;
        case Entity_Type_Enum:                     cs = check_enum(context, ent->enum_type); break;
        case Entity_Type_Enum_Value:               cs = check_expression(context, &ent->enum_value->value); break;
        case Entity_Type_Process_Directive:        cs = check_process_directive(context, (AstNode *) ent->expr); break;
        case Entity_Type_Interface:                cs = check_interface(context, ent->interface); break;

        case Entity_Type_String_Literal:
        case Entity_Type_Expression:
            cs = check_expression(context, &ent->expr);
            resolve_expression_type(context, ent->expr);
            if (cs == Check_Success) cs = Check_Complete;
            break;

        case Entity_Type_Type_Alias:
            if (ent->type_alias->kind == Ast_Kind_Struct_Type)
                cs = check_struct(context, (AstStructType *) ent->type_alias);
            else if (ent->type_alias->kind == Ast_Kind_Union_Type)
                cs = check_union(context, (AstUnionType *) ent->type_alias);
            else
                cs = check_type(context, &ent->type_alias);
            break;

        case Entity_Type_File_Contents: cs = check_file_contents(context, ent->file_contents); break;
        case Entity_Type_Job:           cs = check_arbitrary_job(context, ent->job_data); break;
        case Entity_Type_JS:            cs = check_js_node(context, ent->js); break;
        case Entity_Type_Foreign_Block: cs = check_foreign_block(context, ent->foreign_block); break;
        case Entity_Type_Import:        cs = check_import(context, ent->import); break;
            
        case Entity_Type_Compiler_Extension:   cs = check_compiler_extension(context, ent->compiler_extension); break;
        case Entity_Type_Procedural_Expansion: cs = check_proc_expansion(context, &ent->proc_expansion, PMEK_Top_Level); break;

        default: break;
    }

    switch (cs) {
        case Check_Yield:            ent->macro_attempts++; break;
        case Check_Success:          ent->state = Entity_State_Code_Gen;        goto clear_attempts;
        case Check_Complete:         ent->state = Entity_State_Finalized;       goto clear_attempts;
        case Check_Goto_Parse:       ent->state = Entity_State_Parse;           goto clear_attempts;
        case Check_Failed:           ent->state = Entity_State_Failed;          goto clear_attempts;

    clear_attempts:
        ent->macro_attempts = 0;
        ent->micro_attempts = 0;

        default: break;
    }

    context->checker.current_scope = NULL;
    context->checker.current_entity = NULL;
}
