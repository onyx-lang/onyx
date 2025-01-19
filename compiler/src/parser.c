// The sole job of the parser for Onyx is to submit nodes to the
// entity heap for further processing. These nodes include things
// such as procedure definitions, string literals, struct definitions
// and declarations to be introduced into scopes.

#include "astnodes.h"
#include "parser.h"
#include "lex.h"
#include "errors.h"
#include "utils.h"

#define make_node(nclass, kind)             onyx_ast_node_new(parser->allocator, sizeof(nclass), kind)
// :LinearTokenDependent
#define peek_token(ahead)                   (parser->curr + ahead)

static AstNode error_node = { Ast_Kind_Error, 0, NULL, NULL };

#define ENTITY_SUBMIT(node)                 (submit_entity_in_scope(parser, (AstNode *) (node), parser->current_scope, parser->package))
#define ENTITY_SUBMIT_IN_SCOPE(node, scope) (submit_entity_in_scope(parser, (AstNode *) (node), scope, parser->package))

#undef ONYX_ERROR
#undef ONYX_WARNING
#define ONYX_ERROR(pos, rank, ...) (onyx_report_error(parser->context, (pos), (rank), __VA_ARGS__))
#define ONYX_WARNING(pos, ...) (onyx_report_warning(parser->context, (pos), __VA_ARGS__))

#undef BH_INTERNAL_ALLOCATOR
#define BH_INTERNAL_ALLOCATOR (parser->context->gp_alloc)

void submit_entity_in_scope(OnyxParser* parser, AstNode* node, Scope* scope, Package* package) {
    if (bh_arr_length(parser->alternate_entity_placement_stack) == 0) {
        add_entities_for_node(&parser->context->entities, NULL, node, scope, package);

    } else {
        bh_arr(Entity *) *entity_array = bh_arr_last(parser->alternate_entity_placement_stack);
        add_entities_for_node(&parser->context->entities, entity_array, node, scope, package);
    }
}

// Parsing Utilities
static void consume_token(OnyxParser* parser);
static OnyxToken* expect_token(OnyxParser* parser, TokenType token_type);
static b32 consume_token_if_next(OnyxParser* parser, TokenType token_type);
static b32 next_tokens_are(OnyxParser* parser, i32 n, ...);
static OnyxToken* find_matching_paren(OnyxToken* paren);

static AstNumLit*     parse_int_literal(OnyxParser* parser);
static AstNumLit*     parse_float_literal(OnyxParser* parser);
static b32            parse_possible_struct_literal(OnyxParser* parser, AstTyped* left, AstTyped** ret);
static b32            parse_possible_array_literal(OnyxParser* parser, AstTyped* left, AstTyped** ret);
static b32            parse_possible_unary_field_access(OnyxParser* parser, AstTyped** ret);
static void           parse_arguments(OnyxParser* parser, TokenType end_token, Arguments* args);
static AstTyped*      parse_factor(OnyxParser* parser);
static AstTyped*      parse_compound_assignment(OnyxParser* parser, AstTyped* lhs);
static AstTyped*      parse_compound_expression(OnyxParser* parser, b32 assignment_allowed);
static AstTyped*      parse_expression(OnyxParser* parser, b32 assignment_allowed);
static AstIfWhile*    parse_if_stmt(OnyxParser* parser);
static AstIfWhile*    parse_while_stmt(OnyxParser* parser);
static AstFor*        parse_for_stmt(OnyxParser* parser);
static AstSwitchCase* parse_case_stmt(OnyxParser* parser);
static AstSwitch*     parse_switch_stmt(OnyxParser* parser, b32 switch_is_expr);
static i32            parse_possible_symbol_declaration(OnyxParser* parser, AstNode** ret);
static AstReturn*     parse_return_stmt(OnyxParser* parser);
static AstBlock*      parse_block(OnyxParser* parser, b32 make_a_new_scope, char* block_name);
static AstNode*       parse_statement(OnyxParser* parser);
static void           parse_polymorphic_variable(OnyxParser* parser, AstType*** next_insertion);
static AstType*       parse_type(OnyxParser* parser);
static AstTypeOf*     parse_typeof(OnyxParser* parser);
static AstStructType* parse_struct(OnyxParser* parser);
static AstInterface*  parse_interface(OnyxParser* parser);
static AstConstraint* parse_constraint(OnyxParser* parser);
static void           parse_constraints(OnyxParser* parser, ConstraintContext *constraints);
static void           parse_function_params(OnyxParser* parser, AstFunction* func);
static b32            parse_possible_directive(OnyxParser* parser, const char* dir);
static b32            parse_possible_function_definition_no_consume(OnyxParser* parser);
static b32            parse_possible_function_definition(OnyxParser* parser, AstTyped** ret);
static b32            parse_possible_quick_function_definition_no_consume(OnyxParser* parser);
static b32            parse_possible_quick_function_definition(OnyxParser* parser, AstTyped** ret);
static AstFunction*   parse_function_definition(OnyxParser* parser, OnyxToken* token);
static AstTyped*      parse_global_declaration(OnyxParser* parser);
static AstEnumType*   parse_enum_declaration(OnyxParser* parser);
static AstMacro*      parse_macro(OnyxParser* parser);
static AstIf*         parse_static_if_stmt(OnyxParser* parser, b32 parse_block_as_statements);
static AstMemRes*     parse_memory_reservation(OnyxParser* parser, OnyxToken* symbol, b32 thread_local);
static AstTyped*      parse_top_level_expression(OnyxParser* parser);
static AstBinding*    parse_top_level_binding(OnyxParser* parser, OnyxToken* symbol);
static void           parse_top_level_statement(OnyxParser* parser);
static AstPackage*    parse_package_expression(OnyxParser* parser);
static void           parse_top_level_statements_until(OnyxParser* parser, TokenType tt);
static void           parse_import_statement(OnyxParser* parser, OnyxToken *token);

static void consume_token(OnyxParser* parser) {
    if (parser->hit_unexpected_token) return;

    parser->prev = parser->curr;
    // :LinearTokenDependent
    parser->curr++;
    while (parser->curr->type == Token_Type_Comment) {
        parser->curr++;
    }
}

static OnyxToken* find_matching_paren(OnyxToken* paren) {
    TokenType match = 0;
    switch ((u16) paren->type) {
        case '(': match = ')'; break;
        case '[': match = ']'; break;
        case '{': match = '}'; break;
        case '<': match = '>'; break;
        default: return NULL;
    }

    i32 paren_count = 1;
    i32 i = 1;
    while (paren_count > 0) {
        // :LinearTokenDependent
        TokenType type = (paren + i)->type;
        if (type == Token_Type_End_Stream) return NULL;

        if (type == paren->type) paren_count++;
        else if (type == match)  paren_count--;

        i++;
    }

    // :LinearTokenDependent
    return paren + (i - 1);
}

// NOTE: This advances to next token no matter what
static OnyxToken* expect_token(OnyxParser* parser, TokenType token_type) {
    if (parser->hit_unexpected_token) return NULL;

    OnyxToken* token = parser->curr;
    if (token_type == ';' && token->type == Token_Type_End_Stream) {
        return token;
    }

    consume_token(parser);

    if (token->type == Token_Type_Inserted_Semicolon) {
        if (token_type == ';' || token_type == ',') {
            return token;
        } else {
            token = parser->curr;
            consume_token(parser);
        }
    }

    if (token->type != token_type) {
        ONYX_ERROR(token->pos, Error_Critical, "expected token '%s', got '%s'.", token_type_name(token_type), token_name(token));
        parser->hit_unexpected_token = 1;
        // :LinearTokenDependent
        parser->curr = &parser->tokenizer->tokens[bh_arr_length(parser->tokenizer->tokens) - 1];
        return NULL;
    }

    return token;
}

static b32 consume_token_if_next(OnyxParser* parser, TokenType token_type) {
    if (parser->hit_unexpected_token) return 0;

    if (parser->curr->type == Token_Type_Inserted_Semicolon && token_type == ';') {
        consume_token(parser);
        return 1;
    }

    if (parser->curr->type == token_type) {
        consume_token(parser);
        return 1;
    } else {
        return 0;
    }
}

static void consume_tokens(OnyxParser* parser, i32 n) {
    fori (i, 0, n) {
        if (parser->curr->type == Token_Type_Inserted_Semicolon) {
            i--;
        }

        consume_token(parser);
    }
}

static b32 next_tokens_are(OnyxParser* parser, i32 n, ...) {
    va_list va;
    va_start(va, n);

    i32 matched = 1;
    i32 skipped = 0;

    // BUG: This does not take into consideration comments that can occur between any tokens.
    fori (i, 0, n) {
        TokenType expected_type = va_arg(va, TokenType);
        OnyxToken *peeked_token = peek_token(i + skipped);

        if (peeked_token->type != expected_type) {
            matched = 0;
            break;
        }
    }

    va_end(va);
    return matched;
}


static void expect_no_stored_tags_pos(OnyxParser *parser, OnyxFilePos pos) {
    if (bh_arr_length(parser->stored_tags) > 0) {
        ONYX_ERROR(pos, Error_Critical, "#tag is not allowed on this element.");
        parser->hit_unexpected_token = 1;
    }
}

static void expect_no_stored_tags(OnyxParser *parser) {
    expect_no_stored_tags_pos(parser, parser->curr->pos);
}

static b32 parse_possible_tag(OnyxParser *parser) {
    b32 parsed = 0;
    while (parse_possible_directive(parser, "tag") || consume_token_if_next(parser, '@')) {
        parser->tag_depth += 1;
        parsed = 1;

        do {
            AstTyped* expr = parse_expression(parser, 0);
            bh_arr_push(parser->stored_tags, expr);
        } while (consume_token_if_next(parser, ','));

        parser->tag_depth -= 1;
    }

    return parsed;
}

static void flush_stored_tags(OnyxParser *parser, bh_arr(AstTyped *) *out_arr) {
    //
    // When tag_depth > 0, no tags will be added to the element.
    // This happens if you have a something like so,
    //
    // #tag "asdf"
    // #tag handler.{ (x: i32) => x * 2 }
    // foo :: () { ... }
    //
    // In this situation, the inner procedure defined in the second
    // tag should NOT consume the "asdf" tag.
    //
    if (bh_arr_length(parser->stored_tags) == 0 || parser->tag_depth > 0) return;

    bh_arr(AstTyped *) arr = *out_arr;

    if (arr == NULL) {
        bh_arr_new(parser->allocator, arr, bh_arr_length(parser->stored_tags));
    }

    bh_arr_each(AstTyped *, pexpr, parser->stored_tags) {
        bh_arr_push(arr, *pexpr);
    }

    bh_arr_clear(parser->stored_tags);

    *out_arr = arr;
}

static void flush_doc_tokens(OnyxParser *parser, const char **out_string, OnyxToken **out_token) {
    if (parser->last_documentation_token) {
        if (out_token) *out_token = parser->last_documentation_token;
        parser->last_documentation_token = NULL;
    }

    if (bh_arr_is_empty(parser->documentation_tokens)) {
        if (out_string) *out_string = "";
        return;
    }

    if (!out_string) {
        bh_arr_clear(parser->documentation_tokens);
        return;
    }

    // Build the full doc string from the tokens

    i32 doc_len = 0;
    bh_arr_each(OnyxToken *, ptoken, parser->documentation_tokens) {
        doc_len += (*ptoken)->length;
        doc_len += 1; // For the \n
    }

    bh_buffer str_buf;
    bh_buffer_init(&str_buf, parser->allocator, doc_len + 1); // +1 for null byte

    bh_arr_each(OnyxToken *, ptoken, parser->documentation_tokens) {
        bh_buffer_append(&str_buf, (*ptoken)->text, (*ptoken)->length);
        bh_buffer_write_byte(&str_buf, '\n');
    }
    bh_arr_clear(parser->documentation_tokens);

    bh_buffer_write_byte(&str_buf, 0);
    *out_string = (const char *) str_buf.data;
}


static u64 parse_int_token(OnyxToken *int_token) {
    u64 value = 0;

    token_toggle_end(int_token);

    char *buf = int_token->text;
    i32     i = 0;
    i64  base = 10;

    if (buf[0] == '0' && buf[1] == 'x') {
        base = 16;
        i = 2;
    }

    while (i < int_token->length) {
        char c = buf[i++];

        if ('0' <= c && c <= '9') { value *= base; value += (c - '0'); }
        if ('A' <= c && c <= 'Z') { value *= base; value += ((c - 'A') + 10); }
        if ('a' <= c && c <= 'z') { value *= base; value += ((c - 'a') + 10); }
    }

    token_toggle_end(int_token);
    return value;
}

static f64 parse_float_sign(char **s) {
    if (**s == '-') {
        *s += 1;
        return -1;
    }

    if (**s == '+') {
        *s += 1;
        return 1;
    }

    return 1;
}

static f64 parse_float_digit(char **s, i32 *digit_count) {
    f64 value = 0;
    while (**s) {
        char c = **s;
        if ('0' <= c && c <= '9') {
            value = value * 10 + (c - '0');
            *s += 1;
            *digit_count += 1;

        }
        else if (c == '_') { *s += 1; continue; }
        else { break; }
    }

    return value;
}

static f64 parse_float_token(OnyxToken *float_token) {
    token_toggle_end(float_token);

    char *s = float_token->text;
    i32 digit_count = 0;

    f64 sign  = parse_float_sign(&s);
    f64 value = parse_float_digit(&s, &digit_count);

    if (*s == '.') {
        s++;
        digit_count = 0;
        f64 fraction = parse_float_digit(&s, &digit_count);
        while (digit_count > 0) {
            digit_count -= 1;
            fraction /= 10;
        }
        value += fraction;
    }

    value *= sign;

    if (*s == 'e') {
        s++;

        digit_count = 0;
        f64 exponent_sign = parse_float_sign(&s);
        f64 exponent      = parse_float_digit(&s, &digit_count);

        if (exponent_sign > 0) {
            while (exponent > 0) {
                value *= 10;
                exponent -= 1;
            }
        } else {
            while (exponent > 0) {
                value /= 10;
                exponent -= 1;
            }
        }
    }

    token_toggle_end(float_token);
    return value;
}

static AstNumLit* parse_int_literal(OnyxParser* parser) {
    AstNumLit* int_node = make_node(AstNumLit, Ast_Kind_NumLit);
    int_node->token = expect_token(parser, Token_Type_Literal_Integer);
    int_node->flags |= Ast_Flag_Comptime;

    int_node->value.l = parse_int_token(int_node->token);
    int_node->type_node = (AstType *) &parser->context->basic_types.type_int_unsized;

    // NOTE: Hex literals are unsigned.
    if (int_node->token->length >= 2 && int_node->token->text[1] == 'x') {
        int_node->was_hex_literal = 1;
    }
    return int_node;
}

static AstNumLit* parse_float_literal(OnyxParser* parser) {
    AstNumLit* float_node = make_node(AstNumLit, Ast_Kind_NumLit);
    float_node->token = expect_token(parser, Token_Type_Literal_Float);
    float_node->flags |= Ast_Flag_Comptime;

    AstType* type = (AstType *) &parser->context->basic_types.type_float_unsized;

    float_node->value.d = parse_float_token(float_node->token);

    if (float_node->token->text[float_node->token->length - 1] == 'f') {
        type = (AstType *) &parser->context->basic_types.type_f32;
        float_node->value.f = float_node->value.d;
    }

    float_node->type_node = type;
    return float_node;
}

static b32 parse_possible_directive(OnyxParser* parser, const char* dir) {
    if (!next_tokens_are(parser, 2, '#', Token_Type_Symbol)) return 0;

    OnyxToken* sym = peek_token(1);

    b32 match = (strlen(dir) == (u64) sym->length) && (strncmp(dir, sym->text, sym->length) == 0);
    if (match) consume_tokens(parser, 2);

    return match;
}

static b32 parse_possible_struct_literal(OnyxParser* parser, AstTyped* left, AstTyped** ret) {
    if (!next_tokens_are(parser, 2, '.', '{')) return 0;

    AstStructLiteral* sl = make_node(AstStructLiteral, Ast_Kind_Struct_Literal);
    sl->token = parser->curr;
    sl->stnode = left;

    arguments_initialize(parser->context, &sl->args);

    expect_token(parser, '.');
    expect_token(parser, '{');

    if (consume_token_if_next(parser, Token_Type_Dot_Dot)) {
        sl->extension_value = parse_expression(parser, 0);

        if (peek_token(0)->type != '}') {
            expect_token(parser, ',');
        }
    }

    parse_arguments(parser, '}', &sl->args);

    if (sl->extension_value && bh_arr_length(sl->args.values) > 0) {
        ONYX_ERROR(sl->token->pos, Error_Critical, "All initializers must be named when using '..value' in a struct literal.");
    }

    *ret = (AstTyped *) sl;
    return 1;
}

static b32 parse_possible_array_literal(OnyxParser* parser, AstTyped* left, AstTyped** ret) {
    if (!next_tokens_are(parser, 2, '.', '[')) return 0;

    AstArrayLiteral* al = make_node(AstArrayLiteral, Ast_Kind_Array_Literal);
    al->token = parser->curr;
    al->atnode = left;

    bh_arr_new(parser->context->gp_alloc, al->values, 4);
    fori (i, 0, 4) al->values[i] = NULL;

    expect_token(parser, '.');
    expect_token(parser, '[');
    while (!consume_token_if_next(parser, ']')) {
        if (parser->hit_unexpected_token) return 1;

        AstTyped* value = parse_expression(parser, 0);
        bh_arr_push(al->values, value);

        if (parser->curr->type != ']')
            expect_token(parser, ',');
    }

    *ret = (AstTyped *) al;
    return 1;
}

static b32 parse_possible_unary_field_access(OnyxParser* parser, AstTyped** ret) {
    if (!next_tokens_are(parser, 2, '.', Token_Type_Symbol)) return 0;

    AstUnaryFieldAccess* ufl = make_node(AstUnaryFieldAccess, Ast_Kind_Unary_Field_Access);
    expect_token(parser, '.');
    ufl->token = expect_token(parser, Token_Type_Symbol);

    *ret = (AstTyped *) ufl;
    return 1;
}

static void parse_arguments(OnyxParser* parser, TokenType end_token, Arguments* args) {
    while (!consume_token_if_next(parser, end_token)) {
        if (parser->hit_unexpected_token) return;

        //
        // This has a weird condition to avoid the problem of using a quick function as an argument:
        //     f(x => x + 1)
        // This shouldn't be a named argument, but this should:
        //     f(g = x => x + 1)
        //
        if (next_tokens_are(parser, 2, Token_Type_Symbol, '=')) {
            OnyxToken* name = expect_token(parser, Token_Type_Symbol);
            expect_token(parser, '=');

            AstNamedValue* named_value = make_node(AstNamedValue, Ast_Kind_Named_Value);
            named_value->token = name;
            named_value->value = parse_expression(parser, 0);

            bh_arr_push(args->named_values, named_value);

        } else {
            AstTyped* value = parse_expression(parser, 0);
            bh_arr_push(args->values, value);
        }

        if (parser->curr->type != end_token)
            expect_token(parser, ',');
    }
}

static b32 value_is_placeholder(AstTyped *arg) {
    if (arg->kind != Ast_Kind_Symbol) return 0;
    if (arg->token->length > 1) return 0;
    if (arg->token->text[0] != '_') return 0;
    return 1;
}

static AstCall* parse_function_call(OnyxParser *parser, AstTyped *callee) {
    AstCall* call_node = make_node(AstCall, Ast_Kind_Call);
    call_node->token = expect_token(parser, '(');
    call_node->callee = callee;

    arguments_initialize(parser->context, &call_node->args);

    parse_arguments(parser, ')', &call_node->args);

    // while (consume_token_if_next(parser, '!')) {
    //     AstCodeBlock* code_block = make_node(AstCodeBlock, Ast_Kind_Code_Block);
    //     code_block->token = parser->curr;
    //     code_block->type_node = parser->context->builtins.code_type;

    //     code_block->code = (AstNode *) parse_block(parser, 1, NULL);
    //     ((AstBlock *) code_block->code)->rules = Block_Rule_Code_Block;
    //     bh_arr_push(call_node->args.values, (AstTyped *) code_block);
    // }

    // Wrap expressions in AstArgument
    bh_arr_each(AstTyped *, arg, call_node->args.values) {
        if ((*arg) == NULL) continue;

        if (value_is_placeholder(*arg)) {
            if (call_node->placeholder_argument_position > 0) {
                ONYX_ERROR((*arg)->token->pos, Error_Critical, "Cannot have more than one placeholder argument ('_').");
            }

            call_node->placeholder_argument_position = (arg - call_node->args.values) + 1;
            *arg = NULL;
        } else {
            *arg = (AstTyped *) make_argument(parser->context, *arg);
        }
    }

    bh_arr_each(AstNamedValue *, named_value, call_node->args.named_values) {
        if ((*named_value)->value == NULL) continue;
        (*named_value)->value = (AstTyped *) make_argument(parser->context, (AstTyped *) (*named_value)->value);
    }

    return call_node;
}

static b32 parse_placeholder(OnyxParser* parser) {
    OnyxToken *sym = peek_token(0);
    if (sym->type != Token_Type_Symbol) return 0;
    if (sym->length != 1) return 0;
    if (sym->text[0] != '_') return 0;

    consume_token(parser);
    return 1;
}

static AstTyped* parse_factor(OnyxParser* parser) {
    AstTyped* retval = NULL;

    consume_token_if_next(parser, Token_Type_Inserted_Semicolon);

    switch ((u16) parser->curr->type) {
        case '(': {
            if (parse_possible_function_definition(parser, &retval)) {
                retval->flags |= Ast_Flag_Function_Is_Lambda;
                ENTITY_SUBMIT(retval);
                break;
            }
            if (parse_possible_quick_function_definition(parser, &retval)) {
                retval->flags |= Ast_Flag_Function_Is_Lambda;
                ENTITY_SUBMIT(retval);
                break;
            }

            consume_token(parser);
            retval = parse_compound_expression(parser, 0);
            expect_token(parser, ')');
            break;
        }

        case '-': {
            AstUnaryOp* negate_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            negate_node->operation = Unary_Op_Negate;
            negate_node->token     = expect_token(parser, '-');
            negate_node->expr      = parse_factor(parser);

            retval = (AstTyped *) negate_node;
            break;
        }

        case '!': {
            AstUnaryOp* not_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            not_node->operation = Unary_Op_Not;
            not_node->token = expect_token(parser, '!');
            not_node->expr = parse_factor(parser);

            retval = (AstTyped *) not_node;
            break;
        }

        case '~': {
            AstUnaryOp* not_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            not_node->operation = Unary_Op_Bitwise_Not;
            not_node->token = expect_token(parser, '~');
            not_node->expr = parse_factor(parser);

            retval = (AstTyped *) not_node;
            break;
        }

        case '*': {
            AstDereference* deref_node = make_node(AstDereference, Ast_Kind_Dereference);
            deref_node->token = expect_token(parser, '*');
            deref_node->expr  = parse_factor(parser);

            retval = (AstTyped *) deref_node;
            break;
        }

        case '&':
        case '^': {
            AstAddressOf* aof_node = make_node(AstAddressOf, Ast_Kind_Address_Of);
            aof_node->token = parser->curr->type == '^' ? expect_token(parser, '^') : expect_token(parser, '&'); // HACK
            aof_node->expr  = parse_factor(parser);

            retval = (AstTyped *) aof_node;
            break;
        }

        case '.': {
            if (parse_possible_struct_literal(parser, NULL, &retval)) return retval;
            if (parse_possible_array_literal(parser, NULL, &retval))  return retval;
            if (parse_possible_unary_field_access(parser, &retval))   break;
            goto no_match;
        }

        case Token_Type_Tilde_Tilde: {
            AstUnaryOp* ac_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            ac_node->operation = Unary_Op_Auto_Cast;
            ac_node->token = expect_token(parser, Token_Type_Tilde_Tilde);
            ac_node->expr = parse_factor(parser);

            retval = (AstTyped *) ac_node;
            break;
        }

        case Token_Type_Keyword_Cast: {
            AstUnaryOp* cast_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            cast_node->operation = Unary_Op_Cast;
            cast_node->token = expect_token(parser, Token_Type_Keyword_Cast);

            expect_token(parser, '(');
            cast_node->type_node = parse_type(parser);

            if (peek_token(0)->type == ',') {
                expect_token(parser, ',');
                cast_node->expr = parse_expression(parser, 0);
                expect_token(parser, ')');

            } else {
                expect_token(parser, ')');
                cast_node->expr = parse_factor(parser);
            }

            retval = (AstTyped *) cast_node;
            break;
        }

        case Token_Type_Keyword_Sizeof: {
            AstSizeOf* so_node = make_node(AstSizeOf, Ast_Kind_Size_Of);
            so_node->token = expect_token(parser, Token_Type_Keyword_Sizeof);
            so_node->so_ast_type = (AstType *) parse_type(parser);
            so_node->type_node = (AstType *) &parser->context->basic_types.type_i32;

            retval = (AstTyped *) so_node;
            break;
        }

        case Token_Type_Keyword_Alignof: {
            AstAlignOf* ao_node = make_node(AstAlignOf, Ast_Kind_Align_Of);
            ao_node->token = expect_token(parser, Token_Type_Keyword_Alignof);
            ao_node->ao_ast_type = (AstType *) parse_type(parser);
            ao_node->type_node = (AstType *) &parser->context->basic_types.type_i32;

            retval = (AstTyped *) ao_node;
            break;
        }

        case Token_Type_Keyword_Typeof: {
            retval = (AstTyped *) parse_typeof(parser);
            break;
        }

        case Token_Type_Symbol: {
            if (parse_possible_quick_function_definition(parser, &retval)) {
                retval->flags |= Ast_Flag_Function_Is_Lambda;
                ENTITY_SUBMIT(retval);
                break;
            }

            OnyxToken* sym_token = expect_token(parser, Token_Type_Symbol);
            AstTyped* sym_node = make_node(AstTyped, Ast_Kind_Symbol);
            sym_node->token = sym_token;

            retval = sym_node;
            break;
        }

        case Token_Type_Literal_Integer:
            retval = (AstTyped *) parse_int_literal(parser);
            break;

        case Token_Type_Literal_Float:
            retval = (AstTyped *) parse_float_literal(parser);
            break;

        case Token_Type_Literal_String: {
            AstStrLit* str_node = make_node(AstStrLit, Ast_Kind_StrLit);
            str_node->token     = expect_token(parser, Token_Type_Literal_String);
            str_node->data_id   = 0;
            str_node->flags    |= Ast_Flag_Comptime;

            ENTITY_SUBMIT(str_node);

            retval = (AstTyped *) str_node;
            break;
        }

        case Token_Type_Literal_True: {
            AstNumLit* bool_node = make_node(AstNumLit, Ast_Kind_NumLit);
            bool_node->type_node = (AstType *) &parser->context->basic_types.type_bool;
            bool_node->token = expect_token(parser, Token_Type_Literal_True);
            bool_node->value.i = 1;
            bool_node->flags |= Ast_Flag_Comptime;
            retval = (AstTyped *) bool_node;
            break;
        }

        case Token_Type_Literal_False: {
            AstNumLit* bool_node = make_node(AstNumLit, Ast_Kind_NumLit);
            bool_node->type_node = (AstType *) &parser->context->basic_types.type_bool;
            bool_node->token = expect_token(parser, Token_Type_Literal_False);
            bool_node->value.i = 0;
            bool_node->flags |= Ast_Flag_Comptime;
            retval = (AstTyped *) bool_node;
            break;
        }

        case Token_Type_Keyword_Package: {
            if (!parser->allow_package_expressions)
                ONYX_WARNING(peek_token(-1)->pos, "Use of deprecated feature: package expression.");

            retval = (AstTyped *) parse_package_expression(parser);
            break;
        }

        case Token_Type_Keyword_Macro: {
            retval = (AstTyped *) parse_macro(parser);
            break;
        }

        case Token_Type_Keyword_Do: {
            OnyxToken* do_token = expect_token(parser, Token_Type_Keyword_Do);
            AstDoBlock* do_block = make_node(AstDoBlock, Ast_Kind_Do_Block);
            do_block->token = do_token;
            do_block->type_node = (AstType *) &parser->context->basic_types.type_auto_return;

            if (consume_token_if_next(parser, Token_Type_Right_Arrow)) {
                do_block->type_node = parse_type(parser);
            }

            if (parser->curr->type != '{') {
                AstBlock *tmp_block = make_node(AstBlock, Ast_Kind_Block);
                tmp_block->token = do_token;

                tmp_block->binding_scope = scope_create(parser->context, parser->current_scope, parser->curr->pos);
                tmp_block->binding_scope->name = "<anonymous do block>";

                parser->current_scope = tmp_block->binding_scope;
                tmp_block->body = parse_statement(parser);
                parser->current_scope = parser->current_scope->parent;

                do_block->block = tmp_block;

            } else {
                do_block->block = parse_block(parser, 1, NULL);
            }

            retval = (AstTyped *) do_block;
            break;
        }

        case Token_Type_Keyword_Switch: {
            retval = (AstTyped *) parse_switch_stmt(parser, 1);
            break;
        }

        case '[': {
            // HACK CLEANUP
            // :LinearTokenDependent
            OnyxToken *matching = find_matching_paren(parser->curr);
            if (matching->type == ']' &&
                ((matching + 1)->type == '{' || (matching + 1)->type == '(')) {
                AstCodeBlock* code_block = make_node(AstCodeBlock, Ast_Kind_Code_Block);
                code_block->token = expect_token(parser, '[');

                assert(parser->context->builtins.code_type != NULL);
                code_block->type_node = parser->context->builtins.code_type;

                bh_arr_new(parser->context->gp_alloc, code_block->binding_symbols, 4);
                while (!consume_token_if_next(parser, ']')) {
                    if (parser->hit_unexpected_token) return (AstTyped *) code_block;

                    CodeBlockBindingSymbol sym;
                    sym.symbol = expect_token(parser, Token_Type_Symbol);
                    sym.type_node = NULL;

                    if (consume_token_if_next(parser, ':')) {
                        sym.type_node = parse_type(parser);
                    }

                    bh_arr_push(code_block->binding_symbols, sym);

                    if (parser->curr->type != ']')
                        expect_token(parser, ',');
                }

                if (parser->curr->type == '{') {
                    code_block->code = (AstNode *) parse_block(parser, 1, NULL);
                    ((AstBlock *) code_block->code)->rules = Block_Rule_Code_Block;
                } else {
                    code_block->code = (AstNode *) parse_expression(parser, 1);
                    code_block->is_expression = 1;
                }

                retval = (AstTyped *) code_block;
                break;
            }

            // :fallthrough
        }

        case Token_Type_Keyword_Struct:
        case '?': {
            AstType *type = parse_type(parser);
            retval = (AstTyped *) type;
            break;
        }

        case '$': {
            AstType **tmp = (AstType **) &retval;
            parse_polymorphic_variable(parser, &tmp);
            break;
        }

        case Token_Type_Literal_Char: {
            AstNumLit* char_lit = make_node(AstNumLit, Ast_Kind_NumLit);
            char_lit->flags |= Ast_Flag_Comptime;
            char_lit->type_node = (AstType *) &parser->context->basic_types.type_int_unsized;
            char_lit->token = expect_token(parser, Token_Type_Literal_Char);
            char_lit->was_char_literal = 1;

            char dest[2];
            i32 length = string_process_escape_seqs((char *) &dest, char_lit->token->text, 1);
            char_lit->value.i = (u32) dest[0];

            if (length != 1) {
                ONYX_ERROR(char_lit->token->pos, Error_Critical, "Expected only a single character in character literal.");
            }

            retval = (AstTyped *) char_lit;
            break;
        }

        case '#': {
            if (parse_possible_directive(parser, "file_contents")) {
                AstFileContents* fc = make_node(AstFileContents, Ast_Kind_File_Contents);
                fc->token = parser->prev - 1;
                fc->filename_expr = parse_expression(parser, 0);
                fc->type = type_make_slice(parser->context, parser->context->types.basic[Basic_Kind_U8]);

                if (parser->current_function_stack && bh_arr_length(parser->current_function_stack) > 0) {
                    bh_arr_push(bh_arr_last(parser->current_function_stack)->nodes_that_need_entities_after_clone, (AstNode *) fc);

                } else {
                    ENTITY_SUBMIT(fc);
                }

                retval = (AstTyped *) fc;
                break;
            }
            else if (parse_possible_directive(parser, "file")) {
                OnyxToken* dir_token = parser->curr - 2;

                OnyxToken* str_token = bh_alloc(parser->allocator, sizeof(OnyxToken));
                str_token->text  = bh_strdup(parser->context->gp_alloc, (char *) dir_token->pos.filename);
                str_token->length = strlen(dir_token->pos.filename);
                str_token->pos = dir_token->pos;
                str_token->type = Token_Type_Literal_String;

                AstStrLit* filename = make_node(AstStrLit, Ast_Kind_StrLit);
                filename->token = str_token;
                filename->data_id = 0;

                ENTITY_SUBMIT(filename);
                retval = (AstTyped *) filename;
                break;
            }
            else if (parse_possible_directive(parser, "line")) {
                OnyxToken* dir_token = parser->curr - 2;

                AstNumLit* line_num = make_int_literal(parser->context, dir_token->pos.line);
                retval = (AstTyped *) line_num;
                break;
            }
            else if (parse_possible_directive(parser, "column")) {
                OnyxToken* dir_token = parser->curr - 2;

                AstNumLit* col_num = make_int_literal(parser->context, dir_token->pos.column);
                retval = (AstTyped *) col_num;
                break;
            }
            else if (parse_possible_directive(parser, "char")) {
                AstNumLit* char_lit = make_node(AstNumLit, Ast_Kind_NumLit);
                char_lit->flags |= Ast_Flag_Comptime;
                char_lit->type_node = (AstType *) &parser->context->basic_types.type_int_unsized;
                char_lit->token = expect_token(parser, Token_Type_Literal_String);
                char_lit->was_char_literal = 1;

                char dest[2];
                i32 length = string_process_escape_seqs((char *) &dest, char_lit->token->text, 1);
                char_lit->value.i = (u32) dest[0];

                if (length != 1) {
                    ONYX_ERROR(char_lit->token->pos, Error_Critical, "Expected only a single character in character literal.");
                }

                retval = (AstTyped *) char_lit;
                break;
            }
            else if (parse_possible_directive(parser, "type")) {
                AstTypeAlias* alias = make_node(AstTypeAlias, Ast_Kind_Type_Alias);
                alias->token = parser->curr - 2;
                alias->to = parse_type(parser);
                retval = (AstTyped *) alias;
                break;
            }
            else if (parse_possible_directive(parser, "solidify")) {
                AstDirectiveSolidify* solid = make_node(AstDirectiveSolidify, Ast_Kind_Directive_Solidify);
                // :LinearTokenDependent
                solid->token = parser->curr - 1;

                solid->poly_proc = (AstFunction *) parse_factor(parser);

                solid->known_polyvars = NULL;
                bh_arr_new(parser->context->gp_alloc, solid->known_polyvars, 2);

                expect_token(parser, '{');
                while (!consume_token_if_next(parser, '}')) {
                    if (parser->hit_unexpected_token) break;

                    AstNode* poly_var = (void *) make_node(AstTyped, Ast_Kind_Symbol);
                    poly_var->token = expect_token(parser, Token_Type_Symbol);

                    expect_token(parser, '=');
                    AstType* poly_type = (AstType *) parse_expression(parser, 0);

                    bh_arr_push(solid->known_polyvars, ((AstPolySolution) {
                        .kind     = PSK_Undefined,
                        .poly_sym = poly_var,
                        .ast_type = poly_type,
                        .type     = NULL
                    }));

                    if (parser->curr->type != '}')
                        expect_token(parser, ',');
                }

                retval = (AstTyped *) solid;
                break;
            }
            else if (parse_possible_directive(parser, "defined")) {
                AstDirectiveDefined* defined = make_node(AstDirectiveDefined, Ast_Kind_Directive_Defined);
                // :LinearTokenDependent
                defined->token = parser->curr - 1;
                defined->type_node = (AstType *) &parser->context->basic_types.type_bool;

                parser->allow_package_expressions = 1;
                expect_token(parser, '(');
                defined->expr = parse_expression(parser, 0);
                expect_token(parser, ')');
                parser->allow_package_expressions = 0;

                retval = (AstTyped *) defined;
                break;
            }
            else if (parse_possible_directive(parser, "unquote")) {
                AstDirectiveInsert* insert = make_node(AstDirectiveInsert, Ast_Kind_Directive_Insert);
                insert->token = parser->curr - 1;

                // Parsing calls is disabled here for the potential future feature
                // of using a call-like syntax for passing "parameters" to inserted
                // code blocks. Something like `#unquote foo(x, y)`. This would require
                // different parsing than the normal call so it would just be detected
                // here manually. Also, it does not hurt having this here because there
                // is currently no way to dynamically get a code block to insert from
                // a call, because it is impossible to "return" a code block.
                parser->parse_calls = 0;
                insert->code_expr = parse_expression(parser, 0);
                parser->parse_calls = 1;

                if (consume_token_if_next(parser, '(')) {
                    bh_arr_new(parser->context->gp_alloc, insert->binding_exprs, 4);
                    while (!consume_token_if_next(parser, ')')) {
                        if (parser->hit_unexpected_token) break;

                        AstTyped *expr = parse_expression(parser, 0);
                        bh_arr_push(insert->binding_exprs, expr);

                        if (parser->curr->type != ')')
                            expect_token(parser, ',');
                    }
                }

                if (parse_possible_directive(parser, "skip_scope")) {
                    insert->skip_scope_index = parse_factor(parser);
                }

                retval = (AstTyped *) insert;
                break;
            }
            else if (parse_possible_directive(parser, "cstr")) {
                // Copy pasted from above.
                AstStrLit* str_node = make_node(AstStrLit, Ast_Kind_StrLit);
                str_node->token     = expect_token(parser, Token_Type_Literal_String);
                str_node->data_id   = 0;
                str_node->flags    |= Ast_Flag_Comptime;
                str_node->is_cstr   = 1;

                ENTITY_SUBMIT(str_node);

                retval = (AstTyped *) str_node;
                break;
            }
            else if (parse_possible_directive(parser, "first")) {
                AstDirectiveFirst *first = make_node(AstDirectiveFirst, Ast_Kind_Directive_First);
                first->token = parser->curr - 1;
                first->type_node = (AstType *) &parser->context->basic_types.type_bool;

                retval = (AstTyped *) first;
                break;
            }
            else if (parse_possible_directive(parser, "export_name")) {
                AstDirectiveExportName *export_name = make_node(AstDirectiveExportName, Ast_Kind_Directive_Export_Name);
                export_name->token = parser->curr - 1;
                export_name->func  = (AstFunction *) parse_factor(parser);
                export_name->type_node = parser->context->builtins.string_type;

                retval = (AstTyped *) export_name;
                break;
            }
            else if (parse_possible_directive(parser, "this_package")) {
                AstPackage *this_package = make_node(AstPackage, Ast_Kind_Directive_This_Package);
                this_package->token = parser->curr - 1;
                this_package->type_node = parser->context->builtins.package_id_type;
                ENTITY_SUBMIT(this_package);

                retval = (AstTyped *) this_package;
                break;
            }
            else if (parse_possible_directive(parser, "Self")) {
                if (parser->injection_point == NULL) {
                    ONYX_ERROR((parser->curr - 2)->pos, Error_Critical, "#Self is only allowed in an #inject block.");
                }

                retval = (AstTyped *) parser->injection_point;
                break;
            }

            ONYX_ERROR(parser->curr->pos, Error_Critical, "Invalid directive in expression.");
            return NULL;
        }

        default:
        no_match:
            ONYX_ERROR(parser->curr->pos, Error_Critical, "Unexpected token '%s'.", token_name(parser->curr));
            return NULL;
    }

    while (1) {
        if (parser->hit_unexpected_token) return retval;

        switch ((u16) parser->curr->type) {
            case '[': {
                OnyxToken *open_bracket = expect_token(parser, '[');
                AstTyped *expr = parse_compound_expression(parser, 0);

                AstSubscript *sub_node = make_node(AstSubscript, Ast_Kind_Subscript);
                sub_node->token = open_bracket;
                sub_node->addr = retval;
                sub_node->expr = expr;
                sub_node->__unused_operation = Binary_Op_Subscript;

                retval = (AstTyped *) sub_node;
                expect_token(parser, ']');
                break;
            }

            case '.': {
                if (parse_possible_struct_literal(parser, retval, &retval)) return retval;
                if (parse_possible_array_literal(parser, retval, &retval))  return retval;

                consume_token(parser);
                if (parser->curr->type == '*') {
                    AstDereference* deref_node = make_node(AstDereference, Ast_Kind_Dereference);
                    deref_node->token = expect_token(parser, '*');
                    deref_node->expr  = retval;

                    retval = (AstTyped *) deref_node;

                } else {
                    AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                    field->token = expect_token(parser, Token_Type_Symbol);
                    field->expr  = retval;

                    retval = (AstTyped *) field;
                }
                break;
            }

            case '(': {
                if (!parser->parse_calls) goto factor_parsed;
                retval = (AstTyped *) parse_function_call(parser, retval);
                break;
            }

            case '?': {
                AstUnaryOp* unop = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
                unop->token = expect_token(parser, '?');
                unop->operation = Unary_Op_Try;

                unop->expr = retval;

                retval = (AstTyped *) unop;
                break;
            }

            case '!': {
                AstUnaryOp* unop = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
                unop->token = expect_token(parser, '!');
                unop->operation = Unary_Op_Unwrap;

                unop->expr = retval;

                retval = (AstTyped *) unop;
                break;
            }

            case Token_Type_Inserted_Semicolon: {
                //
                // This is a special case for -> method calls because they should be able to be split across
                // multiple lines, unlike all other postfix operators. This is a personal choice, but I think
                // it reads better than:
                //      iter.as_iter(1 .. 10)->
                //      map(x => x * 2)->
                //      collect()
                //
                if (peek_token(1)->type != Token_Type_Right_Arrow) goto factor_parsed;

                consume_token(parser);
                // fallthrough
            }

            case Token_Type_Right_Arrow: {
                AstBinaryOp* method_call = make_node(AstBinaryOp, Ast_Kind_Method_Call);
                method_call->token = expect_token(parser, Token_Type_Right_Arrow);

                method_call->left = retval;

                OnyxToken *method_name = expect_token(parser, Token_Type_Symbol);
                AstNode *method = make_symbol(parser->context, method_name);

                if (parser->curr->type != '(') {
                    // CLEANUP: This error message is horrendous.
                    ONYX_ERROR(parser->curr->pos, Error_Critical, "Bad method call. Expected object->method(arguments), got something else.");
                    break;
                }

                method_call->right = (AstTyped *) parse_function_call(parser, (AstTyped *) method);

                retval = (AstTyped *) method_call;
                break;
            }

            case Token_Type_Proc_Macro_Body: {
                OnyxToken *tkn = expect_token(parser, Token_Type_Proc_Macro_Body);
                AstProceduralExpansion *proc_expand = make_node(AstProceduralExpansion, Ast_Kind_Procedural_Expansion);
                proc_expand->token = tkn - 1;
                proc_expand->expansion_body = tkn;
                proc_expand->proc_macro = retval;

                retval = (AstTyped *) proc_expand;
                break;
            }

            default: goto factor_parsed;
        }
    }

factor_parsed:

    return retval;
}

static inline i32 get_precedence(BinaryOp kind) {
    switch (kind) {
        case Binary_Op_Assign:          return 1;
        case Binary_Op_Assign_Add:      return 1;
        case Binary_Op_Assign_Minus:    return 1;
        case Binary_Op_Assign_Multiply: return 1;
        case Binary_Op_Assign_Divide:   return 1;
        case Binary_Op_Assign_Modulus:  return 1;
        case Binary_Op_Assign_And:      return 1;
        case Binary_Op_Assign_Or:       return 1;
        case Binary_Op_Assign_Xor:      return 1;
        case Binary_Op_Assign_Shl:      return 1;
        case Binary_Op_Assign_Shr:      return 1;
        case Binary_Op_Assign_Sar:      return 1;

        case Binary_Op_Pipe:            return 2;
        case Binary_Op_Range:           return 2;

        case Binary_Op_Bool_And:        return 3;
        case Binary_Op_Bool_Or:         return 3;

        case Binary_Op_Equal:           return 4;
        case Binary_Op_Not_Equal:       return 4;

        case Binary_Op_Less_Equal:      return 5;
        case Binary_Op_Less:            return 5;
        case Binary_Op_Greater_Equal:   return 5;
        case Binary_Op_Greater:         return 5;

        case Binary_Op_And:             return 6;
        case Binary_Op_Or:              return 6;
        case Binary_Op_Xor:             return 6;
        case Binary_Op_Shl:             return 6;
        case Binary_Op_Shr:             return 6;
        case Binary_Op_Sar:             return 6;

        case Binary_Op_Add:             return 7;
        case Binary_Op_Minus:           return 7;

        case Binary_Op_Multiply:        return 8;
        case Binary_Op_Divide:          return 8;

        case Binary_Op_Modulus:         return 9;

        case Binary_Op_Coalesce:        return 11;

        default:                        return -1;
    }
}

static BinaryOp binary_op_from_token_type(TokenType t) {
    switch ((u16) t) {
        case Token_Type_Equal_Equal:       return Binary_Op_Equal;
        case Token_Type_Not_Equal:         return Binary_Op_Not_Equal;
        case Token_Type_Less_Equal:        return Binary_Op_Less_Equal;
        case Token_Type_Greater_Equal:     return Binary_Op_Greater_Equal;
        case '<':                          return Binary_Op_Less;
        case '>':                          return Binary_Op_Greater;

        case '+':                          return Binary_Op_Add;
        case '-':                          return Binary_Op_Minus;
        case '*':                          return Binary_Op_Multiply;
        case '/':                          return Binary_Op_Divide;
        case '%':                          return Binary_Op_Modulus;

        case '&':                          return Binary_Op_And;
        case '|':                          return Binary_Op_Or;
        case '^':                          return Binary_Op_Xor;
        case Token_Type_Shift_Left:        return Binary_Op_Shl;
        case Token_Type_Shift_Right:       return Binary_Op_Shr;
        case Token_Type_Shift_Arith_Right: return Binary_Op_Sar;

        case Token_Type_And_And:           return Binary_Op_Bool_And;
        case Token_Type_Or_Or:             return Binary_Op_Bool_Or;

        case '=':                          return Binary_Op_Assign;
        case Token_Type_Plus_Equal:        return Binary_Op_Assign_Add;
        case Token_Type_Minus_Equal:       return Binary_Op_Assign_Minus;
        case Token_Type_Star_Equal:        return Binary_Op_Assign_Multiply;
        case Token_Type_Fslash_Equal:      return Binary_Op_Assign_Divide;
        case Token_Type_Percent_Equal:     return Binary_Op_Assign_Modulus;
        case Token_Type_And_Equal:         return Binary_Op_Assign_And;
        case Token_Type_Or_Equal:          return Binary_Op_Assign_Or;
        case Token_Type_Xor_Equal:         return Binary_Op_Assign_Xor;
        case Token_Type_Shl_Equal:         return Binary_Op_Assign_Shl;
        case Token_Type_Shr_Equal:         return Binary_Op_Assign_Shr;
        case Token_Type_Sar_Equal:         return Binary_Op_Assign_Sar;

        case Token_Type_Pipe:              return Binary_Op_Pipe;
        case Token_Type_Dot_Dot:           return Binary_Op_Range;
        case Token_Type_Dot_Dot_Equal:     return Binary_Op_Range_Equal;
        case '[':                          return Binary_Op_Subscript;
        case Token_Type_Question_Question: return Binary_Op_Coalesce;
        default: return Binary_Op_Count;
    }
}

static BinaryOp binary_op_from_current_token(OnyxParser *parser) {
    BinaryOp op = binary_op_from_token_type(parser->curr->type);

    if (op == Binary_Op_Count && parser->curr->type == Token_Type_Inserted_Semicolon) {
        int n = 1;

        while (peek_token(n)->type == Token_Type_Comment) {
            n++;
        }

        if (peek_token(n)->type == Token_Type_Pipe) {
            // This is a slight hack. Though we have peeked ahead n tokens in order
            // to skip the potential comments, `consume_token` will eat the comments
            // automatically, so we don't need to call `consume_token` n times, just
            // once.
            //                                              - brendanfh, 2024/09/25
            consume_token(parser);

            op = Binary_Op_Pipe;
        }
    }

    return op;
}

static AstTyped* parse_compound_assignment(OnyxParser* parser, AstTyped* lhs) {
    if (parser->curr->type != '=') return lhs;

    AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
    assignment->token = expect_token(parser, '=');
    assignment->operation = Binary_Op_Assign;
    assignment->left = lhs;
    assignment->right = parse_compound_expression(parser, 0);

    return (AstTyped *) assignment;
}

static AstTyped* parse_compound_expression(OnyxParser* parser, b32 assignment_allowed) {
    AstTyped* first = parse_expression(parser, assignment_allowed);

    if (parser->curr->type == ',') {
        AstCompound* compound = make_node(AstCompound, Ast_Kind_Compound);
        compound->token = parser->curr;

        bh_arr_new(parser->context->gp_alloc, compound->exprs, 2);
        bh_arr_push(compound->exprs, first);

        while (consume_token_if_next(parser, ',')) {
            if (parser->hit_unexpected_token) return (AstTyped *) compound;

            AstTyped* expr = parse_expression(parser, 0);
            bh_arr_push(compound->exprs, expr);

            if (assignment_allowed && parser->curr->type == '=') {
                return parse_compound_assignment(parser, (AstTyped *) compound);
            }
        }

        return (AstTyped *) compound;

    } else {
        return first;
    }
}

static AstTyped* parse_expression(OnyxParser* parser, b32 assignment_allowed) {
    bh_arr(AstBinaryOp*) tree_stack = NULL;
    bh_arr_new(parser->context->gp_alloc, tree_stack, 4);
    bh_arr_set_length(tree_stack, 0);

    AstTyped* left = parse_factor(parser);
    AstTyped* right;
    AstTyped* root = left;

    BinaryOp bin_op_kind;
    OnyxToken* bin_op_tok;

    while (1) {
        if (parser->hit_unexpected_token) return root;

        if (parser->curr->type == Token_Type_Keyword_If) {
            AstIfExpression *ifexpr = make_node(AstIfExpression, Ast_Kind_If_Expression);
            ifexpr->token = expect_token(parser, Token_Type_Keyword_If);

            ifexpr->true_expr = root;
            ifexpr->cond = parse_expression(parser, 0);
            expect_token(parser, Token_Type_Keyword_Else);
            ifexpr->false_expr = parse_expression(parser, 0);

            // Check for the x = y if cond else z case.
            // This would parse as (x = 1) if cond else z, but that is incorrect.
            if (root->kind == Ast_Kind_Binary_Op && binop_is_assignment(((AstBinaryOp *) root)->operation)) {
                AstBinaryOp *r = (AstBinaryOp *) root;
                ifexpr->true_expr = r->right;
                r->right = (AstTyped *) ifexpr;

            } else {
                root = (AstTyped *) ifexpr;
            }

            goto expression_done;
        }

        bin_op_kind = binary_op_from_current_token(parser);
        if (bin_op_kind == Binary_Op_Count) goto expression_done;
        if (binop_is_assignment(bin_op_kind) && !assignment_allowed) goto expression_done;
        if (bin_op_kind == Binary_Op_Subscript) goto expression_done;

        bin_op_tok = parser->curr;
        consume_token(parser);

        AstBinaryOp* bin_op;
        if      (bin_op_kind == Binary_Op_Pipe)        bin_op = make_node(AstBinaryOp, Ast_Kind_Pipe);
        else if (bin_op_kind == Binary_Op_Range)       bin_op = (AstBinaryOp *) make_node(AstRangeLiteral, Ast_Kind_Range_Literal);
        else if (bin_op_kind == Binary_Op_Range_Equal) {
            bin_op = (AstBinaryOp *) make_node(AstRangeLiteral, Ast_Kind_Range_Literal);
            ((AstRangeLiteral *) bin_op)->inclusive = 1;
        }
        else                                           bin_op = make_node(AstBinaryOp, Ast_Kind_Binary_Op);

        bin_op->token = bin_op_tok;
        bin_op->operation = bin_op_kind;

        while ( !bh_arr_is_empty(tree_stack) &&
                get_precedence(bh_arr_last(tree_stack)->operation) >= get_precedence(bin_op_kind))
            bh_arr_pop(tree_stack);

        if (bh_arr_is_empty(tree_stack)) {
            // NOTE: new is now the root node
            bin_op->left = root;
            root = (AstTyped *) bin_op;

        } else {
            bin_op->left = bh_arr_last(tree_stack)->right;
            bh_arr_last(tree_stack)->right = (AstTyped *) bin_op;
        }

        bh_arr_push(tree_stack, bin_op);

        right = parse_factor(parser);
        bin_op->right = right;
    }

expression_done:
    bh_arr_free(tree_stack);
    return root;
}

static AstIfWhile* parse_if_stmt(OnyxParser* parser) {
    AstIfWhile* if_node = make_node(AstIfWhile, Ast_Kind_If);
    if_node->token = expect_token(parser, Token_Type_Keyword_If);

    AstIfWhile* root_if = if_node;
    AstTyped* cond;
    AstNode* initialization_or_cond=NULL;
    b32 had_initialization = 0;
    if (parse_possible_symbol_declaration(parser, &initialization_or_cond)) {
        had_initialization = 1;

    } else {
        // NOTE: Assignment is allowed here because instead of not parsing correctly,
        // an error is reported in the typechecking, saying that assignment isn't allowed
        // here, which is better than an unexpected token error.
        initialization_or_cond = (AstNode *) parse_compound_expression(parser, 1);
    }

    if (had_initialization || parser->curr->type == ';') {
        expect_token(parser, ';');
        cond = parse_expression(parser, 1);
    } else {
        cond = (AstTyped *) initialization_or_cond;
        initialization_or_cond = NULL;
    }

    AstBlock* true_stmt = parse_block(parser, 1, NULL);
    consume_token_if_next(parser, ';');

    if_node->initialization = initialization_or_cond;
    if_node->cond = cond;
    if (true_stmt != NULL)
        if_node->true_stmt = true_stmt;

    while (consume_token_if_next(parser, Token_Type_Keyword_Elseif)) {
        if (parser->hit_unexpected_token) return root_if;

        AstIfWhile* elseif_node = make_node(AstIfWhile, Ast_Kind_If);
        elseif_node->token = parser->curr - 1;

        cond = parse_expression(parser, 1);
        true_stmt = parse_block(parser, 1, NULL);

        elseif_node->cond = cond;
        if (true_stmt != NULL)
            elseif_node->true_stmt = true_stmt;

        if_node->false_stmt = (AstBlock *) elseif_node;
        if_node = elseif_node;

        consume_token_if_next(parser, ';');
    }

    if (consume_token_if_next(parser, Token_Type_Keyword_Else)) {
        AstBlock* false_stmt = parse_block(parser, 1, NULL);
        if (false_stmt != NULL)
            if_node->false_stmt = false_stmt;
    }

    return root_if;
}

static AstIfWhile* parse_while_stmt(OnyxParser* parser) {
    OnyxToken* while_token = expect_token(parser, Token_Type_Keyword_While);
    AstIfWhile* while_node = make_node(AstIfWhile, Ast_Kind_While);
    while_node->token = while_token;

    if (parse_possible_directive(parser, "bottom_test")
        || consume_token_if_next(parser, Token_Type_Keyword_Defer)) {
        while_node->bottom_test = 1;
    }

    AstTyped* cond;
    AstNode* initialization_or_cond=NULL;
    b32 had_initialization = 0;
    if (parse_possible_symbol_declaration(parser, &initialization_or_cond)) {
        had_initialization = 1;

    } else {
        // NOTE: Assignment is allowed here because instead of not parsing correctly,
        // an error is reported in the typechecking, saying that assignment isn't allowed
        // here, which is better than an unexpected token error.
        initialization_or_cond = (AstNode *) parse_compound_expression(parser, 1);
    }

    if (had_initialization || parser->curr->type == ';') {
        expect_token(parser, ';');
        if (parse_possible_directive(parser, "bottom_test")
            || consume_token_if_next(parser, Token_Type_Keyword_Defer)) {
            while_node->bottom_test = 1;
        }

        cond = parse_expression(parser, 1);
    } else {
        cond = (AstTyped *) initialization_or_cond;
        initialization_or_cond = NULL;
    }

    while_node->initialization = initialization_or_cond;
    while_node->cond = cond;
    while_node->true_stmt = parse_block(parser, 1, NULL);

    if (consume_token_if_next(parser, Token_Type_Keyword_Else)) {
        while_node->false_stmt = parse_block(parser, 1, NULL);
    }

    return while_node;
}

static AstFor* parse_for_stmt(OnyxParser* parser) {
    AstFor* for_node = make_node(AstFor, Ast_Kind_For);
    for_node->token = expect_token(parser, Token_Type_Keyword_For);

    if (parse_possible_directive(parser, "no_close")) {
        for_node->no_close = 1;
    }

    if (consume_token_if_next(parser, '^') || consume_token_if_next(parser, '&')) {
        for_node->by_pointer = 1;
    }

    //
    // For loops can take on a lot of shapes. Look for "sym (:|,|in)".
    //     for value in iter
    //     for value: i64 in iter
    //     for value, index in iter
    //     for value: i64, index in iter
    //     for value: i64, index: i32 in iter
    //
    bh_arr_new(parser->context->gp_alloc, for_node->indexing_variables, 1);

    if (next_tokens_are(parser, 2, Token_Type_Symbol, Token_Type_Keyword_In)
        || next_tokens_are(parser, 2, Token_Type_Symbol, ':')
        || next_tokens_are(parser, 2, Token_Type_Symbol, ',')
    ) {
        while (!consume_token_if_next(parser, Token_Type_Keyword_In)) {
            if (parser->hit_unexpected_token) return for_node;

            AstLocal *var = make_local(
                parser->context,
                expect_token(parser, Token_Type_Symbol),
                NULL
            );

            if (consume_token_if_next(parser, ':')) {
                var->type_node = parse_type(parser);
            }

            bh_arr_push(for_node->indexing_variables, var);

            if (peek_token(0)->type != Token_Type_Keyword_In) {
                expect_token(parser, ',');
            }
        }
    } else {
        // HACK
        static char it_name[] = "it ";
        static OnyxToken it_token = { Token_Type_Symbol, 2, it_name, { 0 } };

        AstLocal* var_node = make_local(parser->context, &it_token, NULL);
        bh_arr_push(for_node->indexing_variables, var_node);
    }

    for_node->iter = parse_expression(parser, 1);
    for_node->stmt = parse_block(parser, 1, NULL);

    return for_node;
}

static AstSwitchCase* parse_case_stmt(OnyxParser* parser) {
    AstSwitchCase *sc_node = make_node(AstSwitchCase, Ast_Kind_Switch_Case);
    sc_node->token = expect_token(parser, Token_Type_Keyword_Case);

    if (
        parse_possible_directive(parser, "default") ||
        parse_placeholder(parser)
    ) {
        sc_node->is_default = 1;

    } else {
        bh_arr_new(parser->context->gp_alloc, sc_node->values, 1);

        parser->parse_quick_functions = 0;
        AstTyped* value = parse_expression(parser, 1);
        bh_arr_push(sc_node->values, value);
        while (consume_token_if_next(parser, ',')) {
            if (parser->hit_unexpected_token) return sc_node;

            value = parse_expression(parser, 1);
            bh_arr_push(sc_node->values, value);
        }

        parser->parse_quick_functions = 1;

        if (   next_tokens_are(parser, 3, Token_Type_Keyword_As, '&', Token_Type_Symbol)
            || next_tokens_are(parser, 3, Token_Type_Keyword_As, '^', Token_Type_Symbol)
            || next_tokens_are(parser, 2, Token_Type_Keyword_As, Token_Type_Symbol)
        ) {
            expect_token(parser, Token_Type_Keyword_As);

            b32 is_pointer = 0;
            if (consume_token_if_next(parser, '&') || consume_token_if_next(parser, '^'))
                is_pointer = 1;

            OnyxToken *capture_symbol = expect_token(parser, Token_Type_Symbol);
            AstLocal *capture = make_local(parser->context, capture_symbol, NULL);

            sc_node->capture = capture;
            sc_node->capture_is_by_pointer = is_pointer;
        }
    }

    if (consume_token_if_next(parser, Token_Type_Fat_Right_Arrow)) {
        sc_node->expr = parse_expression(parser, 0);
        sc_node->body_is_expr = 1;
    } else {
        sc_node->block = parse_block(parser, 1, NULL);
    }

    return sc_node;
}

static AstSwitch* parse_switch_stmt(OnyxParser* parser, b32 switch_is_expr) {
    AstSwitch* switch_node = make_node(AstSwitch, Ast_Kind_Switch);
    switch_node->token = expect_token(parser, Token_Type_Keyword_Switch);
    switch_node->is_expr = switch_is_expr;

    AstTyped* expr;
    AstNode* initialization_or_expr=NULL;
    b32 had_initialization = 0;
    if (parse_possible_symbol_declaration(parser, &initialization_or_expr)) {
        had_initialization = 1;

    } else {
        // NOTE: Assignment is allowed here because instead of not parsing correctly,
        // an error is reported in the typechecking, saying that assignment isn't allowed
        // here, which is better than an unexpected token error.
        initialization_or_expr = (AstNode *) parse_compound_expression(parser, 1);
    }

    if (had_initialization || parser->curr->type == ';') {
        expect_token(parser, ';');
        expr = parse_expression(parser, 1);

    } else {
        expr = (AstTyped *) initialization_or_expr;
        initialization_or_expr = NULL;
    }

    switch_node->initialization = initialization_or_expr;
    switch_node->expr = expr;

    switch_node->case_block = parse_block(parser, 1, NULL);
    return switch_node;
}

static i32 parse_possible_compound_symbol_declaration(OnyxParser* parser, AstNode** ret) {
    u32 token_offset = 0;
    while (peek_token(token_offset)->type == Token_Type_Symbol) {
        token_offset += 1;

        if (peek_token(token_offset)->type == '~') token_offset += 1;

        if (peek_token(token_offset)->type != ',') break;
        token_offset += 1;
    }

    if (peek_token(token_offset)->type != ':') return 0;

    // At this point, we are sure it is a compound declaration.
    AstCompound* local_compound = make_node(AstCompound, Ast_Kind_Compound);
    bh_arr_new(parser->context->gp_alloc, local_compound->exprs, token_offset / 2);

    AstLocal* first_local = NULL;
    AstLocal* prev_local  = NULL;

    while (parser->curr->type == Token_Type_Symbol) {
        if (parser->hit_unexpected_token) return 1;

        OnyxToken* local_sym = expect_token(parser, Token_Type_Symbol);
        AstNode* sym_node = make_symbol(parser->context, local_sym);
        bh_arr_push(local_compound->exprs, (AstTyped *) sym_node);

        if (!consume_token_if_next(parser, '~')) {
            AstLocal* new_local = make_local(parser->context, local_sym, NULL);
            if (prev_local == NULL) {
                first_local = new_local;
            } else {
                prev_local->next = (AstNode *) new_local;
            }
            prev_local = new_local;
        }

        consume_token_if_next(parser, ',');
    }

    expect_token(parser, ':');

    if (parser->curr->type != '=') {
        AstType* type_for_all = NULL;

        // See comment in parse_possible_symbol_declaration about "#auto"
        if (!parse_possible_directive(parser, "auto")) {
            type_for_all = parse_type(parser);

            // Placeholders (_) are discarded and allow for type inference.
            if (value_is_placeholder((AstTyped *) type_for_all)) {
                type_for_all = NULL;
            }
        }

        forll (AstLocal, local, first_local, next) {
            local->type_node = type_for_all;
        }
    }

    if (parser->curr->type == '=') {
        AstBinaryOp* assignment = make_binary_op(parser->context, Binary_Op_Assign, (AstTyped *) local_compound, NULL);
        assignment->token = expect_token(parser, '=');
        assignment->right = parse_compound_expression(parser, 0);

        prev_local->next = (AstNode *) assignment;
    }

    *ret = (AstNode *) first_local;
    return 1;
}

// Returns:
//     0 - if this was not a symbol declaration.
//     1 - if this was a local declaration.
//     2 - if this was binding declaration.
// ret is set to the statement to insert
static i32 parse_possible_symbol_declaration(OnyxParser* parser, AstNode** ret) {
    // Has to start with a symbol to be a declaration
    if (parser->curr->type != Token_Type_Symbol) return 0;

    // If the token after the symbol is a comma, assume this is a compound declaration.
    if (peek_token(1)->type == ',' ||
        (peek_token(1)->type == '~' && peek_token(2)->type == ',')) {
        return parse_possible_compound_symbol_declaration(parser, ret);
    }

    if (peek_token(1)->type != ':') return 0;

    OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
    expect_token(parser, ':');

    if (parser->curr->type == ':') {
        bh_arr_push(parser->current_symbol_stack, symbol);
        AstBinding* binding = parse_top_level_binding(parser, symbol);
        bh_arr_pop(parser->current_symbol_stack);
        if (parser->hit_unexpected_token || !binding) return 2;

        ENTITY_SUBMIT(binding);
        return 2;
    }

    AstType* type_node = NULL;
    if (parser->curr->type != '=') {
        if (parse_possible_directive(parser, "auto")) {
            // Do nothing here.
            // This allows for "x: #auto" to declare an x that will automatically be
            // typed on the first assignment.
        } else {
            type_node = parse_type(parser);

            // Placeholders (_) are discarded and allow for type inference.
            if (value_is_placeholder((AstTyped *) type_node)) {
                type_node = NULL;
            }
        }
    }

    AstLocal* local = make_local(parser->context, symbol, type_node);
    *ret = (AstNode *) local;

    if (parser->curr->type == '=') {
        AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
        assignment->operation = Binary_Op_Assign;
        assignment->token = expect_token(parser, '=');
        local->next = (AstNode *) assignment;

        AstTyped* expr = parse_expression(parser, 1);
        if (expr == NULL) return 1;
        assignment->right = expr;

        // INVESTIGATE: I don't know why, but appearantly, this has to be a
        // symbol node, not a direct link to the local. There is an error about
        // being unable to resolve the type of the local if it is immediately set.
        AstTyped* left_symbol = make_node(AstTyped, Ast_Kind_Symbol);
        left_symbol->token = symbol;
        assignment->left = left_symbol;
    }

    return 1;
}

static AstReturn* parse_return_stmt(OnyxParser* parser) {
    AstReturn* return_node = make_node(AstReturn, Ast_Kind_Return);
    return_node->token = expect_token(parser, Token_Type_Keyword_Return);
    return_node->count = 0;

    while (parser->curr->type == Token_Type_Keyword_Return) {
        consume_token(parser);
        return_node->count += 1;
    }

    if (parse_possible_directive(parser, "from_proc")) {
        return_node->from_proc = 1;
    }

    AstTyped* expr = NULL;

    if (parser->curr->type != ';' && parser->curr->type != Token_Type_Inserted_Semicolon) {
        expr = parse_compound_expression(parser, 0);

        if (expr == NULL || expr == (AstTyped *) &error_node) {
            return (AstReturn *) &error_node;
        } else {
            return_node->expr = expr;
        }
    }

    return return_node;
}

static AstNode* parse_jump_stmt(OnyxParser* parser, TokenType token_type, JumpType jump_type) {
    AstJump* jnode = make_node(AstJump, Ast_Kind_Jump);
    jnode->token = expect_token(parser, token_type);
    jnode->jump  = jump_type;

    u64 count = 1;
    while (consume_token_if_next(parser, token_type)) count++;
    jnode->count = count;

    return (AstNode *) jnode;
}

static AstNode* parse_statement(OnyxParser* parser) {
    b32 needs_semicolon = 1;
    AstNode* retval = NULL;

    switch ((u16) parser->curr->type) {
        case Token_Type_Keyword_Return:
            retval = (AstNode *) parse_return_stmt(parser);
            break;

        case '{':
        case Token_Type_Empty_Block:
        case Token_Type_Keyword_Do:
            needs_semicolon = 0;
            retval = (AstNode *) parse_block(parser, 1, NULL);
            break;

        case Token_Type_Symbol: {
            i32 symbol_res = parse_possible_symbol_declaration(parser, &retval);
            if (symbol_res == 2) needs_semicolon = 0;
            if (symbol_res != 0) break;

            // fallthrough
        }

        case '(': case '+': case '-': case '!': case '*': case '^': case '&':
        case Token_Type_Literal_Integer:
        case Token_Type_Literal_Float:
        case Token_Type_Literal_String:
        case Token_Type_Keyword_Cast:
            retval = (AstNode *) parse_compound_expression(parser, 1);
            if (!retval) break;

            if (retval->kind == Ast_Kind_Call || retval->kind == Ast_Kind_Method_Call) {
                if (parser->curr->type == '{') {
                    AstCodeBlock* code_block = make_node(AstCodeBlock, Ast_Kind_Code_Block);
                    code_block->token = parser->curr;
                    code_block->type_node = parser->context->builtins.code_type;

                    code_block->code = (AstNode *) parse_block(parser, 1, NULL);
                    ((AstBlock *) code_block->code)->rules = Block_Rule_Code_Block;

                    AstCall *dest = (AstCall *) retval;
                    if (dest->kind == Ast_Kind_Method_Call) {
                        dest = (AstCall *) ((AstBinaryOp *) dest)->right;
                        if (dest->kind != Ast_Kind_Call) {
                            ONYX_ERROR(retval->token->pos, Error_Critical, "Expected function call on right side of '->'.");
                            needs_semicolon = 0;
                            break;
                        }
                    }

                    bh_arr_push(dest->args.values, (AstTyped *) make_argument(parser->context, (AstTyped *) code_block));
                    needs_semicolon = 0;
                }
            }
            break;

        case Token_Type_Keyword_If:
            needs_semicolon = 0;
            retval = (AstNode *) parse_if_stmt(parser);
            break;

        case Token_Type_Keyword_While:
            needs_semicolon = 0;
            retval = (AstNode *) parse_while_stmt(parser);
            break;

        case Token_Type_Keyword_For:
            needs_semicolon = 0;
            retval = (AstNode *) parse_for_stmt(parser);
            break;

        case Token_Type_Keyword_Switch:
            needs_semicolon = 0;
            retval = (AstNode *) parse_switch_stmt(parser, 0);
            break;

        case Token_Type_Keyword_Case:
            needs_semicolon = 0;
            retval = (AstNode *) parse_case_stmt(parser);
            break;

        case Token_Type_Keyword_Break:
            retval = parse_jump_stmt(parser, Token_Type_Keyword_Break, Jump_Type_Break);
            break;

        case Token_Type_Keyword_Continue:
            retval = parse_jump_stmt(parser, Token_Type_Keyword_Continue, Jump_Type_Continue);
            break;

        case Token_Type_Keyword_Fallthrough:
            retval = parse_jump_stmt(parser, Token_Type_Keyword_Fallthrough, Jump_Type_Fallthrough);
            break;

        case Token_Type_Keyword_Defer: {
            needs_semicolon = 0;

            AstDefer* defer = make_node(AstDefer, Ast_Kind_Defer);
            defer->token = expect_token(parser, Token_Type_Keyword_Defer);
            defer->stmt  = parse_statement(parser);

            retval = (AstNode *) defer;
            break;
        }

        case Token_Type_Keyword_Use: {
            needs_semicolon = 0;

            if (next_tokens_are(parser, 3, Token_Type_Keyword_Use, Token_Type_Symbol, ':')) {
                OnyxToken *use_token = expect_token(parser, Token_Type_Keyword_Use);

                AstLocal *out = NULL;
                i32 res = parse_possible_symbol_declaration(parser, (AstNode **) &out);
                if (res == 2) {
                    ONYX_ERROR(use_token->pos, Error_Critical, "You cannot 'use' a binding in this way. Remove the 'use'.");
                    parser->hit_unexpected_token = 1;
                    break;
                }

                out->auto_dispose = 1;
                retval = (AstNode *) out;

            } else {
                OnyxToken *use_token = expect_token(parser, Token_Type_Keyword_Use);
                parse_import_statement(parser, use_token);
            }

            break;
        }

        case '#': {
            if (parse_possible_directive(parser, "context_scope")) {
                // :LinearTokenDependent
                OnyxToken* directive_token = parser->curr - 2;

                OnyxToken* sym_token = bh_alloc_item(parser->allocator, OnyxToken);
                sym_token->type = Token_Type_Symbol;
                sym_token->length = 15;
                sym_token->text = bh_strdup(parser->allocator, "__saved_context ");
                sym_token->pos = ((OnyxFilePos) {0});

                AstNode *sym_node = make_symbol(parser->context, sym_token);

                AstLocal* context_tmp = make_local(parser->context, sym_token, NULL);
                retval = (AstNode *) context_tmp;

                AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
                assignment->token = directive_token;
                assignment->operation = Binary_Op_Assign;
                assignment->left = (AstTyped *) sym_node;
                assignment->right = parser->context->builtins.context_variable;
                context_tmp->next = (AstNode *) assignment;

                AstBinaryOp* assignment2 = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
                assignment2->token = directive_token + 1;
                assignment2->operation = Binary_Op_Assign;
                assignment2->left = parser->context->builtins.context_variable;
                assignment2->right = (AstTyped *) sym_node;

                AstDefer* defer_node = make_node(AstDefer, Ast_Kind_Defer);
                defer_node->token = directive_token;
                defer_node->stmt = (AstNode *) assignment2;
                assignment->next = (AstNode *) defer_node;

                AstBlock* context_block = parse_block(parser, 1, NULL);
                defer_node->next = context_block->body;
                context_block->body = (AstNode *) context_tmp;

                needs_semicolon = 0;
                break;
            }

            if (next_tokens_are(parser, 2, '#', Token_Type_Keyword_If)) {
                AstIf* static_if = parse_static_if_stmt(parser, 1);

                assert(parser->current_function_stack && bh_arr_length(parser->current_function_stack) > 0);
                bh_arr_push(bh_arr_last(parser->current_function_stack)->nodes_that_need_entities_after_clone, (AstNode *) static_if);

                needs_semicolon = 0;
                retval = (AstNode *) static_if;
                break;
            }

            if (parse_possible_directive(parser, "persist")) {
                b32 thread_local = parse_possible_directive(parser, "thread_local");

                OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
                AstMemRes* memres = parse_memory_reservation(parser, symbol, thread_local);

                AstBinding* binding = make_node(AstBinding, Ast_Kind_Binding);
                binding->token = memres->token;
                binding->node = (AstNode *) memres;
                ENTITY_SUBMIT(binding);
                break;
            }

            if (parse_possible_directive(parser, "error")) {
                AstDirectiveError *error = make_node(AstDirectiveError, Ast_Kind_Directive_Error);
                error->token = parser->curr - 2;
                error->error_msg = expect_token(parser, Token_Type_Literal_String);

                ENTITY_SUBMIT(error);
                break;
            }

            if (next_tokens_are(parser, 2, '#', Token_Type_Symbol)) {
                retval = (AstNode *) parse_factor(parser);
                break;
            }
        }

        default:
            break;
    }

    if (needs_semicolon) {
        // Allows for not needing the semicolon when the '}' is on the same line.
        //     if x { print() }
        if (peek_token(0)->type != '}') {
            if (!consume_token_if_next(parser, ';')) {
                ONYX_ERROR((parser->curr - 1)->pos, Error_Critical, "Expected a semi-colon after this token.");
                parser->hit_unexpected_token = 1;
            }
        }
    }

    return retval;
}

static AstNode *parse_statements_until(OnyxParser *parser, TokenType end_token) {
    AstNode **next = NULL;
    AstNode *root = NULL;
    while (!consume_token_if_next(parser, end_token)) {
        AstNode *stmt = parse_statement(parser);
        if (parser->hit_unexpected_token) {
            break;
        }

        if (!root) {
            root = stmt;
            next = &root->next;
        } else {
            *next = stmt;
            while (stmt->next != NULL)  stmt = stmt->next;
            next = &stmt->next;
        }
    }

    return root;
}

static AstBlock* parse_block(OnyxParser* parser, b32 make_a_new_scope, char* block_name) {
    AstBlock* block = make_node(AstBlock, Ast_Kind_Block);
    block->rules = Block_Rule_Normal;

    // NOTE: --- is for an empty block
    if (parser->curr->type == Token_Type_Empty_Block) {
        block->token = expect_token(parser, Token_Type_Empty_Block);
        return block;
    }

    if (make_a_new_scope) {
        block->binding_scope = scope_create(parser->context, parser->current_scope, parser->curr->pos);
        block->binding_scope->name = block_name;
        parser->current_scope = block->binding_scope;
    }

    if (parser->curr->type == Token_Type_Keyword_Do) {
        block->token = expect_token(parser, Token_Type_Keyword_Do);
        block->body = parse_statement(parser);
        if (make_a_new_scope) parser->current_scope = parser->current_scope->parent;
        return block;
    }

    block->token = expect_token(parser, '{');

    AstNode** next = &block->body;
    AstNode* stmt = NULL;
    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) {
            if (make_a_new_scope) parser->current_scope = parser->current_scope->parent;
            return block;
        }

        stmt = parse_statement(parser);

        if (stmt != NULL && stmt->kind != Ast_Kind_Error) {
            *next = stmt;

            while (stmt->next != NULL) stmt = stmt->next;
            next = &stmt->next;
        }
    }

    if (make_a_new_scope) parser->current_scope = parser->current_scope->parent;
    return block;
}

static void parse_polymorphic_variable(OnyxParser* parser, AstType*** next_insertion) {
    bh_arr(AstPolyParam) pv = NULL;

    if (parser->polymorph_context.poly_params == NULL)
        ONYX_ERROR(parser->curr->pos, Error_Critical, "Polymorphic variable not valid here.");
    else
        pv = *parser->polymorph_context.poly_params;

    consume_token(parser);

    AstTyped* symbol_node = make_node(AstTyped, Ast_Kind_Symbol);
    symbol_node->token = expect_token(parser, Token_Type_Symbol);
    symbol_node->flags |= Ast_Flag_Symbol_Is_PolyVar;

    AstNode *implicit_interface = NULL;
    if (consume_token_if_next(parser, '/')) {
        implicit_interface = (AstNode *) parse_factor(parser);
    }

    **next_insertion = (AstType *) symbol_node;
    *next_insertion = NULL;

    if (pv != NULL) {
        bh_arr_push(pv, ((AstPolyParam) {
            .kind     = PPK_Poly_Type,
            .poly_sym = (AstNode *) symbol_node,
            .implicit_interface = implicit_interface,

            // These will be filled out by function_params()
            .type_expr = NULL,
            .idx = -1,
        }));

        *parser->polymorph_context.poly_params = pv;
    }
}

static AstType* parse_return_type(OnyxParser* parser, bh_arr(AstLocal *) *pimplicit_locals) {
    b32 values_are_named = 0;
    OnyxToken *last_named_value = NULL;

    if (!consume_token_if_next(parser, '(')) {
        return parse_type(parser);
    }

    if (next_tokens_are(parser, 2, Token_Type_Symbol, ':')) {
        values_are_named = 1;
        last_named_value = expect_token(parser, Token_Type_Symbol);
        consume_tokens(parser, 1);
    }

    AstType* return_type = parse_type(parser);

    bh_arr(AstLocal *) implicit_locals = NULL;
    if (pimplicit_locals && values_are_named) {
        implicit_locals = *pimplicit_locals;
        if (!implicit_locals) bh_arr_new(parser->context->gp_alloc, implicit_locals, 2);

        bh_arr_push(implicit_locals, make_local(parser->context, last_named_value, return_type));
    }

    if (parser->curr->type != ',') {
        expect_token(parser, ')');
        if (pimplicit_locals) *pimplicit_locals = implicit_locals;
        return return_type;
    }

    AstCompoundType* ctype = make_node(AstCompoundType, Ast_Kind_Type_Compound);
    ctype->token = parser->curr;

    bh_arr_new(parser->context->gp_alloc, ctype->types, 2);
    bh_arr_push(ctype->types, return_type);

    while (consume_token_if_next(parser, ',')) {
        if (parser->hit_unexpected_token) return (AstType *) ctype;

        if (values_are_named) {
            last_named_value = expect_token(parser, Token_Type_Symbol);
            expect_token(parser, ':');
        }

        return_type = parse_type(parser);
        bh_arr_push(ctype->types, return_type);

        if (pimplicit_locals && values_are_named) {
            bh_arr_push(implicit_locals, make_local(parser->context, last_named_value, return_type));
        }
    }

    expect_token(parser, ')');
    if (pimplicit_locals) *pimplicit_locals = implicit_locals;
    return (AstType *) ctype;
}

static AstType* parse_function_type(OnyxParser* parser, OnyxToken* proc_token) {
    bh_arr(AstType *) params = NULL;
    bh_arr_new(parser->context->scratch_alloc, params, 4);
    bh_arr_set_length(params, 0);

    expect_token(parser, '(');
    while (!consume_token_if_next(parser, ')')) {
        if (parser->hit_unexpected_token) return NULL;

        // NOTE: Allows for names to be put in the function types, just for readability.
        if (next_tokens_are(parser, 2, Token_Type_Symbol, ':')) consume_tokens(parser, 2);

        AstType* param_type = parse_type(parser);
        bh_arr_push(params, param_type);

        if (parser->curr->type != ')')
            expect_token(parser, ',');
    }

    expect_token(parser, Token_Type_Right_Arrow);
    AstType* return_type = parse_return_type(parser, NULL);

    i64 param_count = bh_arr_length(params);
    AstFunctionType* new = onyx_ast_node_new(parser->allocator,
            sizeof(AstFunctionType) + sizeof(AstType*) * param_count,
            Ast_Kind_Function_Type);
    new->token = proc_token;
    new->param_count = param_count;
    new->return_type = return_type;

    if (param_count > 0)
        fori (i, 0, param_count) new->params[i] = params[i];

    return (AstType *) new;
}

static AstType* parse_type(OnyxParser* parser) {
    AstType* root = NULL;
    AstType** next_insertion = &root;

    b32 type_can_be_done = 0;

    while (next_insertion != NULL) {
        if (parser->hit_unexpected_token) return root;

        switch ((u16) parser->curr->type) {
            case '&':
            case '^': {
                AstPointerType* new = make_node(AstPointerType, Ast_Kind_Pointer_Type);
                new->flags |= Basic_Flag_Pointer;
                // new->token = expect_token(parser, '^');
                new->token = parser->curr->type == '^' ? expect_token(parser, '^') : expect_token(parser, '&'); // HACK

                *next_insertion = (AstType *) new;
                next_insertion = &new->elem;
                type_can_be_done = 0;
                break;
            }

            case '[': {
                AstType *new;
                OnyxToken *open_bracket = expect_token(parser, '[');

                if (parser->curr->type == ']') {
                    new = make_node(AstSliceType, Ast_Kind_Slice_Type);
                    new->token = open_bracket;

                } else if (parser->curr->type == '&' || parser->curr->type == '^') {
                    consume_token(parser);

                    new = make_node(AstMultiPointerType, Ast_Kind_Multi_Pointer_Type);
                    new->flags |= Basic_Flag_Pointer;
                    new->token = open_bracket;

                } else if (parser->curr->type == Token_Type_Dot_Dot) {
                    new = make_node(AstDynArrType, Ast_Kind_DynArr_Type);
                    new->token = open_bracket;
                    consume_token(parser);

                } else {
                    new = make_node(AstArrayType, Ast_Kind_Array_Type);
                    new->token = open_bracket;

                    if (parser->curr->type == '$') {
                        AstType** insertion = (AstType **) &((AstArrayType *) new)->count_expr;
                        parse_polymorphic_variable(parser, &insertion);
                    } else {
                        ((AstArrayType *) new)->count_expr = parse_expression(parser, 0);
                    }
                }

                expect_token(parser, ']');
                *next_insertion = (AstType *) new;
                next_insertion = &((AstSliceType *) new)->elem;
                type_can_be_done = 0;
                break;
            }

            case '$': {
                parse_polymorphic_variable(parser, &next_insertion);
                type_can_be_done = 1;
                break;
            }

            case Token_Type_Symbol: {
                AstTyped* symbol_node = make_node(AstTyped, Ast_Kind_Symbol);
                symbol_node->token = expect_token(parser, Token_Type_Symbol);

                *next_insertion = (AstType *) symbol_node;

                while (consume_token_if_next(parser, '.')) {
                    AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                    field->token = expect_token(parser, Token_Type_Symbol);
                    field->expr  = (AstTyped *) *next_insertion;

                    *next_insertion = (AstType *) field;
                }

                if (parser->curr->type == '(' && parser->parse_calls) {
                    OnyxToken* paren_token = expect_token(parser, '(');

                    bh_arr(AstNode *) params = NULL;
                    bh_arr_new(parser->context->gp_alloc, params, 2);

                    while (!consume_token_if_next(parser, ')')) {
                        if (parser->hit_unexpected_token) break;

                        AstNode* t = (AstNode *) parse_expression(parser, 0);
                        bh_arr_push(params, t);

                        if (parser->curr->type != ')')
                            expect_token(parser, ',');
                    }

                    AstPolyCallType* pc_type = make_node(AstPolyCallType, Ast_Kind_Poly_Call_Type);
                    pc_type->token = paren_token;
                    pc_type->callee = *next_insertion;
                    pc_type->params = params;

                    *next_insertion = (AstType *) pc_type;
                }

                if (peek_token(0)->type != '.')
                    next_insertion = NULL;

                type_can_be_done = 1;
                break;
            }

            case Token_Type_Keyword_Struct: {
                AstStructType* s_node = parse_struct(parser);
                *next_insertion = (AstType *) s_node;
                next_insertion = NULL;
                type_can_be_done = 1;
                break;
            }

            //
            // I don't think any of these cases are necesary any more?
            case Token_Type_Literal_Integer:
            case Token_Type_Literal_String:
            case Token_Type_Literal_Float:
            case Token_Type_Literal_True:
            case Token_Type_Literal_False:
            case '-': {
                *next_insertion = (AstType *) parse_expression(parser, 0);
                next_insertion = NULL;
                type_can_be_done = 1;
                break;
            }

            case '(': {
                OnyxToken* matching = find_matching_paren(parser->curr);
                *next_insertion = parse_function_type(parser, parser->curr);
                next_insertion = NULL;
                type_can_be_done = 1;
                break;
            }

            case Token_Type_Keyword_Typeof: {
                *next_insertion = (AstType *) parse_typeof(parser);
                next_insertion = NULL;
                type_can_be_done = 1;
                break;
            }

            case '?': {
                assert(parser->context->builtins.optional_type);

                bh_arr(AstNode *) params = NULL;
                bh_arr_new(parser->context->gp_alloc, params, 1);
                bh_arr_set_length(params, 1);

                AstPolyCallType* pc_type = make_node(AstPolyCallType, Ast_Kind_Poly_Call_Type);
                pc_type->token = expect_token(parser, '?');
                pc_type->callee = parser->context->builtins.optional_type;
                pc_type->params = params;
                *next_insertion = (AstType *) pc_type;

                next_insertion = (AstType **) &params[0];
                type_can_be_done = 0;
                break;
            }

            case '#': {
                if (parse_possible_directive(parser, "Self")) {
                    if (parser->injection_point == NULL) {
                        ONYX_ERROR((parser->curr - 2)->pos, Error_Critical, "#Self is only allowed in an #inject block.");
                    }

                    *next_insertion = (AstType *) parser->injection_point;
                    next_insertion = NULL;
                    type_can_be_done = 1;
                    break;
                }
            }

            case '.': {
                consume_token(parser);
                AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                field->token = expect_token(parser, Token_Type_Symbol);
                field->expr  = (AstTyped *) *next_insertion;

                *next_insertion = (AstType *) field;
                type_can_be_done = 1;
                break;
            }

            default:
                next_insertion = NULL;
                break;
        }
    }

    if (!type_can_be_done) {
        if (root) {
            ONYX_ERROR(root->token->pos, Error_Critical, "Incomplete type when parsing.");
        } else {
            ONYX_ERROR(parser->curr->pos, Error_Critical, "Expected a type here.");
        }
    }

    return root;
}

static AstTypeOf* parse_typeof(OnyxParser* parser) {
    OnyxToken* token = expect_token(parser, Token_Type_Keyword_Typeof);

    AstTypeOf* type_of = make_node(AstTypeOf, Ast_Kind_Typeof);
    type_of->token = token;
    type_of->expr = parse_expression(parser, 0);
    type_of->resolved_type = NULL;

    return type_of;
}

static void type_create_scope(OnyxParser *parser, Scope ** scope, OnyxToken* token) {
    if (scope && !*scope) {
        *scope = scope_create(parser->context, parser->current_scope, token->pos);

        if (bh_arr_length(parser->current_symbol_stack) == 0) {
            (*scope)->name = "<anonymous>";

        } else {
            OnyxToken* current_symbol = bh_arr_last(parser->current_symbol_stack);
            (*scope)->name = bh_aprintf(parser->context->gp_alloc, "%b", current_symbol->text, current_symbol->length);
        }
    }
}

static AstStructType* parse_struct(OnyxParser* parser) {
    OnyxToken *s_token = expect_token(parser, Token_Type_Keyword_Struct);

    AstStructType* s_node;
    AstPolyStructType* poly_struct = NULL;

    s_node = make_node(AstStructType, Ast_Kind_Struct_Type);
    s_node->token = s_token;

    flush_stored_tags(parser, &s_node->meta_tags);

    type_create_scope(parser, &s_node->scope, s_node->token);
    Scope *scope_to_restore_parser_to = parser->current_scope;
    Scope *scope_symbols_in_structures_should_be_bound_to = s_node->scope;

    // Parse polymorphic parameters
    if (consume_token_if_next(parser, '(')) {
        bh_arr(AstPolyStructParam) poly_params = NULL;
        bh_arr_new(parser->context->gp_alloc, poly_params, 1);

        while (!consume_token_if_next(parser, ')')) {
            if (parser->hit_unexpected_token) return NULL;

            OnyxToken* sym_token = expect_token(parser, Token_Type_Symbol);
            expect_token(parser, ':');

            AstType* param_type = parse_type(parser);

            bh_arr_push(poly_params, ((AstPolyStructParam) {
                .token = sym_token,
                .type_node = param_type,
                .type = NULL,
            }));

            if (parser->curr->type != ')')
                expect_token(parser, ',');
        }

        poly_struct = make_node(AstPolyStructType, Ast_Kind_Poly_Struct_Type);
        poly_struct->token = s_token;
        poly_struct->poly_params = poly_params;
        poly_struct->base_struct = s_node;
        poly_struct->scope = s_node->scope;
        s_node->scope = NULL;
    }

    // Parse constraints clause
    if (parser->curr->type == Token_Type_Keyword_Where) {
        parse_constraints(parser, &s_node->constraints);
    }

    bh_arr_new(parser->context->gp_alloc, s_node->members, 4);

    // Parse directives
    while (parser->curr->type == '#') {
        if (parser->hit_unexpected_token) return NULL;

        if (next_tokens_are(parser, 2, '#', Token_Type_Keyword_Union)) {
            consume_tokens(parser, 2);
            s_node->is_union = 1;
        }

        else if (parse_possible_directive(parser, "pack")) s_node->is_packed = 1;

        else if (parse_possible_directive(parser, "align")) {
            s_node->min_alignment_ = parse_expression(parser, 0);
        }

        else if (parse_possible_directive(parser, "size")) {
            s_node->min_size_ = parse_expression(parser, 0);
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            ONYX_ERROR(directive_token->pos, Error_Critical, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    expect_token(parser, '{');

    parser->current_scope = scope_symbols_in_structures_should_be_bound_to;

    b32 member_is_used = 0;
    bh_arr(OnyxToken *) member_list_temp = NULL;
    bh_arr_new(parser->context->gp_alloc, member_list_temp, 4);

    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return s_node;

        if (parse_possible_tag(parser)) {
            consume_token_if_next(parser, ';');
            continue;
        }

        if (parse_possible_directive(parser, "persist")) {
            b32 thread_local = parse_possible_directive(parser, "thread_local");

            OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
            AstMemRes* memres = parse_memory_reservation(parser, symbol, thread_local);
            consume_token_if_next(parser, ';');

            AstBinding* binding = make_node(AstBinding, Ast_Kind_Binding);
            binding->token = memres->token;
            binding->node = (AstNode *) memres;
            ENTITY_SUBMIT(binding);
            continue;
        }

        if (next_tokens_are(parser, 3, Token_Type_Symbol, ':', ':')) {
            OnyxToken* binding_name = expect_token(parser, Token_Type_Symbol);
            consume_token(parser);

            AstBinding* binding = parse_top_level_binding(parser, binding_name);
            if (binding) ENTITY_SUBMIT(binding);

            consume_token_if_next(parser, ';');
            continue;
        }

        bh_arr(AstTyped *) meta_tags=NULL;
        flush_stored_tags(parser, &meta_tags);

        if (parser->curr->type == '}') {
            consume_token(parser);
            break;
        }

        member_is_used = consume_token_if_next(parser, Token_Type_Keyword_Use);

        bh_arr_clear(member_list_temp);
        while (!consume_token_if_next(parser, ':')) {
            if (parser->hit_unexpected_token) return NULL;
            if (parser->curr->type == Token_Type_Doc_Comment) {
                consume_token(parser);
                continue;
            }

            bh_arr_push(member_list_temp, expect_token(parser, Token_Type_Symbol));

            if (parser->curr->type != ':')
                expect_token(parser, ',');
        }

        AstType* member_type = NULL;
        if (parser->curr->type != '=')
            member_type = parse_type(parser);

        AstTyped* initial_value = NULL;
        if (consume_token_if_next(parser, '='))
            initial_value = parse_expression(parser, 0);

        // RECONSIDER: There are seamingly arbitrary limitations put in place here which do two things:
        //   1. Prevent multiple struct members being used in the same declaration.
        //      This makes sense because the members will be of the same type, which means
        //      they have the same members. Using both of the members would immediately result
        //      in name collisions.
        //
        //   2. Prevent multiple struct members having an initializer set for them.
        //      I think the semantics could be confusing either way, so I'm deciding to leave
        //      them out of discussion for now. Initialized members should be treated special and
        //      deserve their own line.
        if (bh_arr_length(member_list_temp) > 1) {
            if (member_is_used) ONYX_ERROR((member_list_temp[0] - 1)->pos, Error_Critical, "'use' is only allowed for a single struct member declaration. Try splitting this compound declaration into multiple lines.");
            if (initial_value)  ONYX_ERROR(initial_value->token->pos, Error_Critical, "Intialized values are only allowed on single struct member declarations. Try splitting this compound initializer into multiple lines.");
        }

        bh_arr_each(OnyxToken *, member_name, member_list_temp) {
            AstStructMember* mem = make_node(AstStructMember, Ast_Kind_Struct_Member);
            mem->token = *member_name;
            mem->type_node = member_type;
            mem->initial_value = initial_value;
            mem->meta_tags = meta_tags;

            if (member_is_used) mem->is_used = 1;

            bh_arr_push(s_node->members, mem);
        }

        if (peek_token(0)->type != '}') {
            expect_token(parser, ';');
        }
    }

    parser->current_scope = scope_to_restore_parser_to;

    bh_arr_free(member_list_temp);

    if (poly_struct != NULL) {
        // NOTE: Not a StructType
        return (AstStructType *) poly_struct;

    } else {
        ENTITY_SUBMIT(s_node);
        return s_node;
    }
}

static AstUnionType* parse_union(OnyxParser* parser) {
    OnyxToken* union_token = expect_token(parser, Token_Type_Keyword_Union);

    AstUnionType* u_node;
    AstPolyUnionType* poly_union = NULL;

    u_node = make_node(AstUnionType, Ast_Kind_Union_Type);
    u_node->token = union_token;

    flush_stored_tags(parser, &u_node->meta_tags);

    type_create_scope(parser, &u_node->scope, u_node->token);
    Scope *scope_to_restore_parser_to = parser->current_scope;
    Scope *scope_symbols_in_unions_should_be_bound_to = u_node->scope;

    if (parse_possible_directive(parser, "tag_type")) {
        AstType *backing_type = parse_type(parser);
        u_node->tag_backing_type = backing_type;
    } else {
        u_node->tag_backing_type = NULL;
    }

    if (consume_token_if_next(parser, '(')) {
        bh_arr(AstPolyStructParam) poly_params = NULL;
        bh_arr_new(parser->context->gp_alloc, poly_params, 1);

        while (!consume_token_if_next(parser, ')')) {
            if (parser->hit_unexpected_token) return NULL;

            OnyxToken* sym_token = expect_token(parser, Token_Type_Symbol);
            expect_token(parser, ':');

            AstType* param_type = parse_type(parser);

            bh_arr_push(poly_params, ((AstPolyStructParam) {
                .token = sym_token,
                .type_node = param_type,
                .type = NULL,
            }));

            if (parser->curr->type != ')')
                expect_token(parser, ',');
        }

        poly_union = make_node(AstPolyUnionType, Ast_Kind_Poly_Union_Type);
        poly_union->token = union_token;
        poly_union->poly_params = poly_params;
        poly_union->base_union = u_node;
        poly_union->scope = u_node->scope;
        u_node->scope = NULL;
    }

    // Parse constraints clause
    if (parser->curr->type == Token_Type_Keyword_Where) {
        parse_constraints(parser, &u_node->constraints);
    }

    parser->current_scope = scope_symbols_in_unions_should_be_bound_to;
    bh_arr_new(parser->context->gp_alloc, u_node->variants, 4);

    expect_token(parser, '{');
    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return u_node;

        parse_possible_tag(parser);

        if (next_tokens_are(parser, 3, Token_Type_Symbol, ':', ':')) {
            OnyxToken* binding_name = expect_token(parser, Token_Type_Symbol);
            consume_token(parser);

            AstBinding* binding = parse_top_level_binding(parser, binding_name);
            if (binding) ENTITY_SUBMIT(binding);

            consume_token_if_next(parser, ';');
            continue;
        }

        bh_arr(AstTyped *) meta_tags=NULL;
        flush_stored_tags(parser, &meta_tags);

        if (parser->curr->type == '}') {
            consume_token(parser);
            break;
        }

        AstUnionVariant *variant = make_node(AstUnionVariant, Ast_Kind_Union_Variant);
        variant->meta_tags = meta_tags;
        variant->token = expect_token(parser, Token_Type_Symbol);

        if (consume_token_if_next(parser, Token_Type_Keyword_As)) {
            variant->explicit_tag_value = parse_factor(parser);
        }

        expect_token(parser, ':');

        variant->type_node = parse_type(parser);

        bh_arr_push(u_node->variants, variant);

        if (peek_token(0)->type != '}') {
            expect_token(parser, ';');
        }
    }

    parser->current_scope = scope_to_restore_parser_to;

    if (poly_union != NULL) {
        // NOTE: Not a UnionType
        return (AstUnionType *) poly_union;

    } else {
        ENTITY_SUBMIT(u_node);
        return u_node;
    }
}

static AstInterface* parse_interface(OnyxParser* parser) {
    AstInterface *interface = make_node(AstInterface, Ast_Kind_Interface);
    interface->token = expect_token(parser, Token_Type_Keyword_Interface);

    bh_arr_new(parser->context->gp_alloc, interface->params, 2);

    expect_token(parser, '(');
    while (!consume_token_if_next(parser, ')')) {
        if (parser->hit_unexpected_token) return interface;

        InterfaceParam ip;
        ip.value_token = expect_token(parser, Token_Type_Symbol);
        expect_token(parser, ':');
        ip.value_type  = parse_type(parser);

        bh_arr_push(interface->params, ip);

        if (parser->curr->type != ')')
            expect_token(parser, ',');
    }

    if (parse_possible_directive(parser, "intrinsic")) {
        interface->is_intrinsic = 1;
        return interface;
    }

    bh_arr_new(parser->context->gp_alloc, interface->exprs, 2);
    bh_arr_new(parser->context->gp_alloc, interface->sentinels, 2);

    type_create_scope(parser, &interface->scope, interface->token);
    parser->current_scope = interface->scope;

    expect_token(parser, '{');
    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return interface;

        if (next_tokens_are(parser, 3, Token_Type_Symbol, ':', ':')) {
            OnyxToken* binding_name = expect_token(parser, Token_Type_Symbol);
            consume_token(parser);

            AstBinding* binding = parse_top_level_binding(parser, binding_name);
            if (binding) ENTITY_SUBMIT(binding);

            consume_token_if_next(parser, ';');
            continue;
        }

        if (next_tokens_are(parser, 2, Token_Type_Symbol, Token_Type_Keyword_As)) {
            InterfaceSentinel sentinel;
            sentinel.name = expect_token(parser, Token_Type_Symbol);
            consume_token(parser);

            sentinel.type = parse_type(parser);
            bh_arr_push(interface->sentinels, sentinel);

            consume_token_if_next(parser, ';');
            continue;
        }

        InterfaceConstraint ic = {0};
        if (parse_possible_directive(parser, "not")) {
            ic.invert_condition = 1;
        }

        if (consume_token_if_next(parser, '{')) {
            ic.expr = parse_expression(parser, 0);

            expect_token(parser, '}');
            expect_token(parser, Token_Type_Right_Arrow);

            ic.expected_type_expr = parse_type(parser);

        } else {
            ic.expr = parse_expression(parser, 0);
        }

        bh_arr_push(interface->exprs, ic);

        expect_token(parser, ';');
    }

    parser->current_scope = parser->current_scope->parent;
    return interface;
}

// :InterfacesAsExpressionsRefactor

// @Todo(Judah): This can be significantly improved by not treating interface calls as a separate thing.
// Maybe a flag on parser called 'calls_are_interfaces' that tells parse_expression to do what we do below?
// This should allow us to remove the N token lookahead and give better error reporting for invalid interface usage.
static AstConstraint* parse_constraint(OnyxParser* parser) {
    AstConstraint* constraint = make_node(AstConstraint, Ast_Kind_Constraint);

    // @Note(Judah): We lookahead N tokens to see which kind of constraint
    // this is. A simple check will not match things like: 'foo.bar.Baz(T)'

    i32 i               = 0;
    b32 parse_interface = 0;
    while (1) {
        OnyxToken* next = peek_token(i);
        if (!next || next->type == '{') break;

        if (next->type == Token_Type_Symbol) {
            next = peek_token(i + 1);
            if (next && next->type == '(') {
                parse_interface = 1;
                break;
            }
        }

        i += 1;
    }

    // Interface constraint: Foo(T)
    if (parse_interface) {
        parser->parse_calls = 0;
        constraint->interface = (AstInterface *) parse_factor(parser);
        parser->parse_calls = 1;

        constraint->token = constraint->interface->token;

        bh_arr_new(parser->context->gp_alloc, constraint->args, 2);

        expect_token(parser, '(');
        while (!consume_token_if_next(parser, ')')) {
            if (parser->hit_unexpected_token) return constraint;

            AstTyped* type_node = parse_expression(parser, 0);
            bh_arr_push(constraint->args, type_node);

            if (parser->curr->type != ')')
                expect_token(parser, ',');
        }
    }
    // Expression constraint: T == X
    else {
        constraint->const_expr = parse_expression(parser, 0);
        if (parser->hit_unexpected_token || !constraint->const_expr) return constraint;

        constraint->token  = constraint->const_expr->token;
        constraint->flags |= Ast_Flag_Constraint_Is_Expression;
    }

    return constraint;
}

static void parse_constraints(OnyxParser* parser, ConstraintContext *out_constraints) {
    bh_arr_new(parser->context->gp_alloc, out_constraints->constraints, 2);

    expect_token(parser, Token_Type_Keyword_Where);

    do {
        AstConstraint *constraint = parse_constraint(parser);
        if (parser->hit_unexpected_token) return;

        bh_arr_push(out_constraints->constraints, constraint);
    } while (consume_token_if_next(parser, ','));
}

static AstCaptureBlock *parse_capture_list(OnyxParser* parser, TokenType end_token) {
    // :LinearTokenDependent
    AstCaptureBlock *captures = make_node(AstCaptureBlock, Ast_Kind_Capture_Block);
    captures->token = parser->curr - 1;

    bh_arr_new(parser->context->gp_alloc, captures->captures, 2);

    while (!consume_token_if_next(parser, end_token)) {
        if (parser->hit_unexpected_token) break;

        AstCaptureLocal *capture = make_node(AstCaptureLocal, Ast_Kind_Capture_Local);

        if (consume_token_if_next(parser, '&')) capture->by_reference = 1;

        capture->token = expect_token(parser, Token_Type_Symbol);

        bh_arr_push(captures->captures, capture);

        if (peek_token(0)->type != end_token)
            expect_token(parser, ',');
    }

    return captures;
}

static void parse_function_params(OnyxParser* parser, AstFunction* func) {
    expect_token(parser, '(');

    if (consume_token_if_next(parser, ')')) return;

    u32 param_idx = 0;
    assert(parser->polymorph_context.poly_params != NULL);

    bh_arr(AstParam) param_buffer=NULL;
    bh_arr_new(parser->context->gp_alloc, param_buffer, 2);

    OnyxToken* symbol;
    while (!consume_token_if_next(parser, ')')) {
        do {
            if (parser->hit_unexpected_token) return;

            b32 param_use = 0;
            b32 param_is_baked = 0;
            AstParam curr_param = { 0 };

            if (consume_token_if_next(parser, Token_Type_Keyword_Use)) param_use = 1;
            if (consume_token_if_next(parser, '$'))                    param_is_baked = 1;

            symbol = expect_token(parser, Token_Type_Symbol);

            curr_param.vararg_kind = VA_Kind_Not_VA;
            curr_param.local = make_local(parser->context, symbol, NULL);
            curr_param.local->kind = Ast_Kind_Param;

            if (param_use) {
                curr_param.is_used = 1;
                param_use = 0;
            }

            if (param_is_baked) {
                curr_param.is_baked = 1;
                param_is_baked = 0;
            }

            bh_arr_push(param_buffer, curr_param);
        } while (consume_token_if_next(parser, ','));

        expect_token(parser, ':');

        VarArgKind vararg_kind=VA_Kind_Not_VA;
        AstType* type_node=NULL;
        AstTyped* default_value=NULL;

        if (parser->curr->type != '=') {
            if (consume_token_if_next(parser, Token_Type_Dot_Dot)) {
                if (consume_token_if_next(parser, '.')) vararg_kind = VA_Kind_Untyped;
                else                                    vararg_kind = VA_Kind_Typed;
            }

            if (vararg_kind != VA_Kind_Untyped) {
                // CLEANUP: This is mess and it is hard to follow what is going on here.
                // I think with recent rewrites, this should be easier to do.
                i32 old_len = bh_arr_length(*parser->polymorph_context.poly_params);
                type_node = parse_type(parser);
                i32 new_len = bh_arr_length(*parser->polymorph_context.poly_params);

                if (vararg_kind == VA_Kind_Typed) {
                    AstVarArgType* va_type = make_node(AstVarArgType, Ast_Kind_VarArg_Type);
                    va_type->elem = type_node;
                    va_type->token = type_node->token;
                    type_node = (AstType *) va_type;
                }

                fori (i, 0, new_len - old_len) {
                    (*parser->polymorph_context.poly_params)[old_len + i].type_expr = type_node;
                    (*parser->polymorph_context.poly_params)[old_len + i].idx = param_idx;
                }
            }
        }

        if (vararg_kind == VA_Kind_Not_VA && consume_token_if_next(parser, '=')) {
            OnyxToken* directive_token = parser->curr;

            // :Callsite  currently #callsite is only valid as a default value for a funciton parameter.
            if (parse_possible_directive(parser, "callsite")) {
                AstCallSite* cs = make_node(AstCallSite, Ast_Kind_Call_Site);
                cs->token = directive_token;
                default_value = (AstTyped *) cs;

            } else {
                default_value = parse_expression(parser, 0);
            }
        }

        bh_arr_each(AstParam, param, param_buffer) {
            param->vararg_kind      = vararg_kind;
            param->local->type_node = type_node;
            param->default_value    = default_value;

            if (param->is_baked) {
                bh_arr(AstPolyParam) pv = *parser->polymorph_context.poly_params;
                bh_arr_push(pv, ((AstPolyParam) {
                    .kind = PPK_Baked_Value,
                    .idx = param_idx,

                    .poly_sym = (AstNode *) param->local,
                    .type_expr = type_node,
                }));

                *parser->polymorph_context.poly_params = pv;
            }

            bh_arr_push(func->params, *param);
            param_idx++;
        }

        bh_arr_clear(param_buffer);

        if (parser->curr->type != ')')
            expect_token(parser, ',');
    }

    bh_arr_free(param_buffer);
    return;
}

static AstOverloadedFunction* parse_overloaded_function(OnyxParser* parser, OnyxToken* token) {
    b32 locked = 0;
    if (parse_possible_directive(parser, "locked")) {
        locked = 1;
    }

    b32 local = 0;
    if (parse_possible_directive(parser, "local")) {
        local = 1;
    }

    // This could be checked elsewhere?
    if (locked && local) {
        ONYX_ERROR(token->pos, Error_Critical, "Only one of '#locked' and '#local' can be use at a time.");
    }

    AstOverloadedFunction* ofunc = make_node(AstOverloadedFunction, Ast_Kind_Overloaded_Function);
    ofunc->token = token;
    ofunc->flags |= Ast_Flag_Comptime;
    ofunc->locked = locked;
    ofunc->only_local_functions = local;

    bh_arr_new(parser->context->gp_alloc, ofunc->overloads, 4);

    if (peek_token(0)->type == Token_Type_Right_Arrow) {
        expect_token(parser, Token_Type_Right_Arrow);

        ofunc->expected_return_node = parse_type(parser);
    }

    expect_token(parser, '{');

    u64 order = 0;
    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return ofunc;

        if (parse_possible_directive(parser, "order")) {
            AstNumLit* pre = parse_int_literal(parser);
            if (parser->hit_unexpected_token) return ofunc;

            order = bh_max(pre->value.l, 0);
        }

        AstTyped* option = parse_expression(parser, 0);
        option->flags &= ~Ast_Flag_Function_Is_Lambda;
        add_overload_option(&ofunc->overloads, order++, option);

        if (parser->curr->type != '}')
            expect_token(parser, ',');
    }

    ENTITY_SUBMIT(ofunc);
    return ofunc;
}

static AstFunction* parse_function_definition(OnyxParser* parser, OnyxToken* token) {
    AstFunction* func_def = make_node(AstFunction, Ast_Kind_Function);
    func_def->token = token;
    bh_arr_push(parser->current_function_stack, func_def);

    flush_stored_tags(parser, &func_def->tags);

    bh_arr_new(parser->context->gp_alloc, func_def->params, 4);

    bh_arr(AstPolyParam) polymorphic_vars = NULL;
    bh_arr_new(parser->context->gp_alloc, polymorphic_vars, 4);

    void *prev_poly_params = parser->polymorph_context.poly_params;
    parser->polymorph_context.poly_params = &polymorphic_vars;
    parse_function_params(parser, func_def);
    parser->polymorph_context.poly_params = prev_poly_params;

    if (bh_arr_length(polymorphic_vars) > 0) {
        func_def->kind = Ast_Kind_Polymorphic_Proc;
        func_def->poly_params = polymorphic_vars;

    } else {
        bh_arr_free(polymorphic_vars);
    }

    func_def->return_type = (AstType *) &parser->context->basic_types.type_void;

    char* name = NULL;
    if (bh_arr_length(parser->current_symbol_stack) > 0) {
        OnyxToken *current_symbol = bh_arr_last(parser->current_symbol_stack);
        name = bh_aprintf(parser->context->gp_alloc, "%b", current_symbol->text, current_symbol->length);
    }

    if (consume_token_if_next(parser, Token_Type_Keyword_Use)) {
        expect_token(parser, '(');
        func_def->captures = parse_capture_list(parser, ')');
        consume_token_if_next(parser, ',');

        if (bh_arr_length(parser->current_function_stack) > 1) {
            AstFunction *parent_func = parser->current_function_stack[bh_arr_length(parser->current_function_stack) - 2];
            if (parent_func->kind == Ast_Kind_Polymorphic_Proc) {
                func_def->flags |= Ast_Flag_Function_Is_Lambda_Inside_PolyProc;
            }
        }
    }

    if (consume_token_if_next(parser, Token_Type_Fat_Right_Arrow)) {
        func_def->return_type = (AstType *) &parser->context->basic_types.type_auto_return;

        if (parser->curr->type == '{') {
            func_def->body = parse_block(parser, 1, name);

        } else {
            AstTyped* returned_value = parse_compound_expression(parser, 0);
            if (returned_value == NULL) goto function_defined;

            AstReturn* return_node = make_node(AstReturn, Ast_Kind_Return);
            return_node->token = returned_value->token;
            return_node->expr = returned_value;

            AstBlock* body_block = make_node(AstBlock, Ast_Kind_Block);
            body_block->token = returned_value->token;
            body_block->body = (AstNode *) return_node;

            func_def->body = body_block;
        }

        goto function_defined;
    }

    if (consume_token_if_next(parser, Token_Type_Right_Arrow)) {
        if (parse_possible_directive(parser, "auto")) {
            func_def->return_type = (AstType *) &parser->context->basic_types.type_auto_return;
        } else {
            func_def->return_type = parse_return_type(parser, &func_def->named_return_locals);

            if (value_is_placeholder((AstTyped *) func_def->return_type)) {
                func_def->return_type = (AstType *) &parser->context->basic_types.type_auto_return;
            }
        }
    }

    if (parser->curr->type == Token_Type_Keyword_Where) {
        parse_constraints(parser, &func_def->constraints);
    }

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "intrinsic")) {
            func_def->is_intrinsic = 1;

            if (parser->curr->type == Token_Type_Literal_String) {
                func_def->intrinsic_name = expect_token(parser, Token_Type_Literal_String);
            }
        }

        else if (parse_possible_directive(parser, "foreign")) {
            func_def->foreign.module_name = parse_expression(parser, 0);
            func_def->foreign.import_name = parse_expression(parser, 0);

            func_def->is_foreign = 1;
        }

        // HACK: NullProcHack
        else if (parse_possible_directive(parser, "null")) {
            func_def->flags |= Ast_Flag_Proc_Is_Null;
        }

        else if (parse_possible_directive(parser, "deprecated")) {
            func_def->deprecated_warning = (AstStrLit *) parse_expression(parser, 0);
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            ONYX_ERROR(directive_token->pos, Error_Critical, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    func_def->body = parse_block(parser, 1, name);

    // :LinearTokenDependent
    func_def->closing_brace = parser->curr - 1;

function_defined:
    bh_arr_pop(parser->current_function_stack);
    return func_def;
}

static b32 parse_possible_function_definition_no_consume(OnyxParser* parser) {
    if (parser->curr->type != '(') return 0;

    if (peek_token(1)->type == ')') {
        return 1;
    }

    int offset = 1;

keep_going:
    if (peek_token(offset)->type == Token_Type_Keyword_Use) offset += 1;
    if (peek_token(offset)->type == '$') offset += 1;
    if (peek_token(offset)->type == Token_Type_Symbol) {
        offset += 1;
        if (peek_token(offset)->type == ',') {
            offset += 1;
            goto keep_going;

        } else if (peek_token(offset)->type == ':') {
            return 1;
        }
    }

    return 0;
}

static b32 parse_possible_function_definition(OnyxParser* parser, AstTyped** ret) {
    if (parse_possible_function_definition_no_consume(parser)) {
        OnyxToken* proc_token = parser->curr;
        AstFunction* func_node = parse_function_definition(parser, proc_token);
        *ret = (AstTyped *) func_node;

        return 1;
    }

    return 0;
}

typedef struct QuickParam {
    OnyxToken* token;
    b32 is_baked;
} QuickParam;

static b32 parse_possible_quick_function_definition_no_consume(OnyxParser* parser) {
    if (!parser->parse_quick_functions) return 0;

    //
    // x => x + 1 case.
    if (next_tokens_are(parser, 2, Token_Type_Symbol, Token_Type_Fat_Right_Arrow)) {
        return 1;
    }

    if (parser->curr->type != '(') return 0;

    i32 offset = 1;
    while (peek_token(offset)->type != ')') {
        if (peek_token(offset)->type != Token_Type_Symbol) return 0;
        offset += 1;

        if (peek_token(offset)->type == ')') break;

        if (peek_token(offset)->type != ',') return 0;
        offset += 1;
    }

    // :LinearTokenDependent
    OnyxToken* token_after_paren = peek_token(offset + 1);
    if (token_after_paren->type == Token_Type_Fat_Right_Arrow)
        return 1;

    if (token_after_paren->type == Token_Type_Keyword_Use)
        return 1;

    return 0;
}

static b32 parse_possible_quick_function_definition(OnyxParser* parser, AstTyped** ret) {
    if (!parse_possible_quick_function_definition_no_consume(parser)) return 0;

    bh_arr(QuickParam) params=NULL;
    bh_arr_new(parser->context->gp_alloc, params, 4);

    OnyxToken* proc_token;
    AstCaptureBlock *captures = NULL;

    if (parser->curr->type == Token_Type_Symbol) {
        QuickParam param = { 0 };
        param.token = expect_token(parser, Token_Type_Symbol);
        proc_token = param.token;
        bh_arr_push(params, param);

    } else {
        proc_token = expect_token(parser, '(');
        while (!consume_token_if_next(parser, ')')) {
            if (parser->hit_unexpected_token) return 0;

            QuickParam param = { 0 };
            if (consume_token_if_next(parser, '$')) param.is_baked = 1;
            param.token = expect_token(parser, Token_Type_Symbol);

            bh_arr_push(params, param);

            if (parser->curr->type != ')') {
                expect_token(parser, ',');
            }
        }

        if (consume_token_if_next(parser, Token_Type_Keyword_Use)) {
            expect_token(parser, '(');
            captures = parse_capture_list(parser, ')');
        }
    }

    expect_token(parser, Token_Type_Fat_Right_Arrow);

    bh_arr(AstNode *) poly_params=NULL;
    bh_arr_new(parser->context->gp_alloc, poly_params, bh_arr_length(params));
    bh_arr_each(QuickParam, param, params) {
        char text[512];
        memset(text, 0, 512);
        strncat(text, "__type_", 511);

        token_toggle_end(param->token);
        if (!strcmp(param->token->text, "_")) {
            int index = param - params;
            int len = strnlen(text, 511);
            snprintf(text + len, 511 - len, "%d", index);
        } else {
            strncat(text, param->token->text, 511);
        }
        token_toggle_end(param->token);

        OnyxToken* new_token = bh_alloc(parser->allocator, sizeof(OnyxToken));
        new_token->type = Token_Type_Symbol;
        new_token->length = 7 + param->token->length;
        new_token->text = bh_strdup(parser->allocator, text);
        new_token->pos = param->token->pos;

        AstNode* type_node = make_symbol(parser->context, new_token);
        type_node->flags |= Ast_Flag_Symbol_Is_PolyVar;
        bh_arr_push(poly_params, type_node);
    }

    AstFunction* poly_proc = make_node(AstFunction, Ast_Kind_Polymorphic_Proc);

    bh_arr_new(parser->context->gp_alloc, poly_proc->params, bh_arr_length(params));
    fori (i, 0, bh_arr_length(params)) {
        AstLocal* param_local = make_local(parser->context, params[i].token, (AstType *) poly_params[i]);
        param_local->kind = Ast_Kind_Param;

        bh_arr_push(poly_proc->params, ((AstParam) {
            .local = param_local,
            .default_value = NULL,

            .vararg_kind = 0,
            .use_processed = 0,
        }));
    }

    AstBlock* body_block;
    AstType*  return_type;
    bh_arr_push(parser->current_function_stack, poly_proc);

    if (parser->curr->type == '{') {
        char* name = NULL;
        if (bh_arr_length(parser->current_symbol_stack) > 0) {
            OnyxToken *current_symbol = bh_arr_last(parser->current_symbol_stack);
            name = bh_aprintf(parser->context->gp_alloc, "%b", current_symbol->text, current_symbol->length);
        }

        body_block = parse_block(parser, 1, name);
        return_type = (AstType *) &parser->context->basic_types.type_auto_return;

    } else {
        AstTyped* body = parse_expression(parser, 0);
        if (body == NULL) {
            ONYX_ERROR(parser->curr->pos, Error_Critical, "Expected an expression here.");
            parser->hit_unexpected_token = 1;
            return 0;
        }

        AstReturn* return_node = make_node(AstReturn, Ast_Kind_Return);
        return_node->token = body->token;
        return_node->expr = body;

        body_block = make_node(AstBlock, Ast_Kind_Block);
        body_block->token = body->token;
        body_block->body = (AstNode *) return_node;

        AstTypeOf* return_type_of = make_node(AstTypeOf, Ast_Kind_Typeof);
        return_type_of->token = body->token;
        return_type_of->expr = body;
        return_type = (AstType *) return_type_of;
    }

    poly_proc->token = proc_token;
    poly_proc->body = body_block;
    poly_proc->return_type = (AstType *) return_type;
    poly_proc->captures = captures;

    bh_arr_new(parser->context->gp_alloc, poly_proc->poly_params, bh_arr_length(params));
    fori (i, 0, bh_arr_length(params)) {
        bh_arr_push(poly_proc->poly_params, ((AstPolyParam) {
            .kind = PPK_Poly_Type,
            .idx  = i,
            .poly_sym = poly_params[i],
            .type_expr = (AstType *) poly_params[i],
            .type = NULL,
        }));

        if (params[i].is_baked) {
            // This is not handled currently, as you cannot say f :: ($x: $T) yet, which is what this would have to do.
        }
    }

    *ret = (AstTyped *) poly_proc;

    bh_arr_pop(parser->current_function_stack);
    bh_arr_free(params);
    bh_arr_free(poly_params);
    return 1;
}

static AstTyped* parse_global_declaration(OnyxParser* parser) {
    expect_no_stored_tags(parser);

    AstGlobal* global_node = make_node(AstGlobal, Ast_Kind_Global);
    global_node->token = expect_token(parser, Token_Type_Keyword_Global);

    global_node->type_node = parse_type(parser);

    ENTITY_SUBMIT(global_node);

    return (AstTyped *) global_node;
}

static AstEnumType* parse_enum_declaration(OnyxParser* parser) {
    expect_no_stored_tags(parser);

    AstEnumType* enum_node = make_node(AstEnumType, Ast_Kind_Enum_Type);
    enum_node->token = expect_token(parser, Token_Type_Keyword_Enum);

    bh_arr_new(parser->context->gp_alloc, enum_node->values, 4);

    while (parser->curr->type == '#') {
        if (parser->hit_unexpected_token) return enum_node;

        if (parse_possible_directive(parser, "flags")) {
            enum_node->is_flags = 1;
        } else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            ONYX_ERROR(directive_token->pos, Error_Critical, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    AstType* backing = (AstType *) &parser->context->basic_types.type_u32;
    if (consume_token_if_next(parser, '(')) {
        AstTyped* backing_sym = make_node(AstTyped, Ast_Kind_Symbol);
        backing_sym->token = expect_token(parser, Token_Type_Symbol);
        backing = (AstType *) backing_sym;

        expect_token(parser, ')');
    }
    enum_node->backing = backing;

    expect_token(parser, '{');

    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return enum_node;
        if (parser->curr->type == Token_Type_Doc_Comment) {
            consume_token(parser);
            continue;
        }

        AstEnumValue* evalue = make_node(AstEnumValue, Ast_Kind_Enum_Value);
        evalue->token = expect_token(parser, Token_Type_Symbol);
        evalue->type_node = (AstType *) enum_node;

        if (consume_token_if_next(parser, ':')) {
            expect_token(parser, ':');
            evalue->value = parse_expression(parser, 0);
        }

        expect_token(parser, ';');

        bh_arr_push(enum_node->values, evalue);
    }

    return enum_node;
}

static AstIf* parse_static_if_stmt(OnyxParser* parser, b32 parse_block_as_statements) {
    expect_no_stored_tags(parser);

    AstIf* static_if_node = make_node(AstIf, Ast_Kind_Static_If);
    static_if_node->token = expect_token(parser, '#');
    static_if_node->defined_in_scope = parser->current_scope;
    expect_token(parser, Token_Type_Keyword_If);

    static_if_node->cond = parse_expression(parser, 0);

    bh_arr_new(parser->context->gp_alloc, static_if_node->true_entities, 2);
    bh_arr_push(parser->alternate_entity_placement_stack, &static_if_node->true_entities);

    if (parse_block_as_statements) {
        static_if_node->true_stmt = parse_block(parser, 0, NULL);

    } else {
        expect_token(parser, '{');
        while (!consume_token_if_next(parser, '}')) {
            if (parser->hit_unexpected_token) return static_if_node;

            parse_top_level_statement(parser);
            consume_token_if_next(parser, ';');
        }
    }

    bh_arr_pop(parser->alternate_entity_placement_stack);

    if (consume_token_if_next(parser, Token_Type_Keyword_Else)) {
        bh_arr_new(parser->context->gp_alloc, static_if_node->false_entities, 2);
        bh_arr_push(parser->alternate_entity_placement_stack, &static_if_node->false_entities);

        if (parse_block_as_statements) {
            static_if_node->false_stmt = parse_block(parser, 0, NULL);

        } else {
            expect_token(parser, '{');
            while (!consume_token_if_next(parser, '}')) {
                if (parser->hit_unexpected_token) return static_if_node;

                parse_top_level_statement(parser);
                consume_token_if_next(parser, ';');
            }
        }

        bh_arr_pop(parser->alternate_entity_placement_stack);
    }

    return static_if_node;
}

static AstMemRes* parse_memory_reservation(OnyxParser* parser, OnyxToken* symbol, b32 threadlocal) {
    expect_token(parser, ':');

    AstMemRes* memres = make_node(AstMemRes, Ast_Kind_Memres);
    memres->threadlocal = threadlocal;
    memres->token = symbol;

    flush_stored_tags(parser, &memres->tags);

    if (parser->curr->type != '=')
        memres->type_node = parse_type(parser);

    if (consume_token_if_next(parser, '='))
        memres->initial_value = parse_expression(parser, 1);

    ENTITY_SUBMIT(memres);
    return memres;
}

static AstMacro* parse_macro(OnyxParser* parser) {
    expect_no_stored_tags(parser);

    AstMacro* macro = make_node(AstMacro, Ast_Kind_Macro);
    macro->token = expect_token(parser, Token_Type_Keyword_Macro);

    if (parse_possible_function_definition(parser, &macro->body)) {
        ENTITY_SUBMIT(macro);
        return macro;
    }

    if (parse_possible_quick_function_definition(parser, &macro->body)) {
        ENTITY_SUBMIT(macro);
        return macro;
    }

    ONYX_ERROR(parser->curr->pos, Error_Critical, "'macro' expects to be followed by a producure definition.");
    return NULL;
}

static AstDirectiveInit* parse_init_directive(OnyxParser *parser, OnyxToken *token) {
    AstDirectiveInit *init = make_node(AstDirectiveInit, Ast_Kind_Directive_Init);
    init->token = token;

    parser->parse_calls = 0;
    while (parse_possible_directive(parser, "after")) {
        if (parser->hit_unexpected_token) return init;
        if (init->dependencies == NULL) bh_arr_new(parser->context->gp_alloc, init->dependencies, 2);

        AstTyped *dependency = parse_expression(parser, 0);
        bh_arr_push(init->dependencies, (AstDirectiveInit *) dependency);
    }
    parser->parse_calls = 1;

    init->init_proc = parse_expression(parser, 0);
    ENTITY_SUBMIT(init);
    return init;
}

static AstForeignBlock* parse_foreign_block(OnyxParser* parser, OnyxToken *token) {
    // :LinearTokenDependent
    AstForeignBlock *fb = make_node(AstForeignBlock, Ast_Kind_Foreign_Block);
    fb->token = token;

    if (parse_possible_directive(parser, "dyncall")) {
        fb->uses_dyncall = 1;
    }

    fb->module_name = parse_expression(parser, 0);

    //
    // This has a fun implication that there cannot be foreign blocks in the builtin
    // or type_info packages, as those are loaded before foreign_block_type has a value.
    fb->type_node = parser->context->builtins.foreign_block_type;

    bh_arr_new(parser->context->gp_alloc, fb->captured_entities, 4);
    bh_arr_push(parser->alternate_entity_placement_stack, &fb->captured_entities);

    expect_token(parser, '{');
    parse_top_level_statements_until(parser, '}');
    expect_token(parser, '}');

    bh_arr_pop(parser->alternate_entity_placement_stack);
    ENTITY_SUBMIT(fb);

    return fb;
}

static AstCompilerExtension* parse_compiler_extension(OnyxParser* parser, OnyxToken *token) {
    AstCompilerExtension *ext = make_node(AstCompilerExtension, Ast_Kind_Compiler_Extension);
    ext->token = token;

    ext->name = expect_token(parser, Token_Type_Literal_String);

    bh_arr_new(parser->context->gp_alloc, ext->proc_macros, 2);
    expect_token(parser, '{');
    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) break;

        AstProceduralMacro *pmacro = make_node(AstProceduralMacro, Ast_Kind_Procedural_Macro);
        pmacro->token = expect_token(parser, Token_Type_Symbol);
        pmacro->extension = ext;

        bh_arr_push(ext->proc_macros, pmacro);

        if (parser->curr->type != '}')
            expect_token(parser, ',');
    }

    ENTITY_SUBMIT(ext);
    return ext;
}

static AstTyped* parse_top_level_expression(OnyxParser* parser) {
    if (parser->curr->type == Token_Type_Keyword_Global)    return parse_global_declaration(parser);
    if (parser->curr->type == Token_Type_Keyword_Struct)    return (AstTyped *) parse_struct(parser);
    if (parser->curr->type == Token_Type_Keyword_Interface) return (AstTyped *) parse_interface(parser);
    if (parser->curr->type == Token_Type_Keyword_Enum)      return (AstTyped *) parse_enum_declaration(parser);
    if (parser->curr->type == Token_Type_Keyword_Macro)     return (AstTyped *) parse_macro(parser);
    if (parser->curr->type == Token_Type_Keyword_Union)     return (AstTyped *) parse_union(parser);

    if (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "type")) {
            AstTypeAlias* alias = make_node(AstTypeAlias, Ast_Kind_Type_Alias);
            alias->to = parse_type(parser);
            return (AstTyped *) alias;
        }

        if (parse_possible_directive(parser, "match")) {
            // :LinearTokenDependent
            OnyxToken* directive_token = parser->curr - 2;
            AstOverloadedFunction* ofunc = parse_overloaded_function(parser, directive_token);
            return (AstTyped *) ofunc;
        }

        if (parse_possible_directive(parser, "init")) {
            // :LinearTokenDependent
            AstDirectiveInit *init = parse_init_directive(parser, parser->curr - 2);
            return (AstTyped *) init;
        }

        if (parse_possible_directive(parser, "distinct")) {
            // :LinearTokenDependent
            AstDistinctType *distinct = make_node(AstDistinctType, Ast_Kind_Distinct_Type);
            distinct->token = parser->curr - 2;
            distinct->base_type = parse_type(parser);
            type_create_scope(parser, &distinct->scope, distinct->token);
            return (AstTyped *) distinct;
        }

        if (parse_possible_directive(parser, "foreign")) {
            AstForeignBlock *foreign = parse_foreign_block(parser, parser->curr - 2);
            return (AstTyped *) foreign;
        }

        if (parse_possible_directive(parser, "compiler_extension") || parse_possible_directive(parser, "extension")) {
            return (AstTyped *) parse_compiler_extension(parser, parser->curr - 2);
        }
    }

    return parse_expression(parser, 1);
}

static AstBinding* parse_top_level_binding(OnyxParser* parser, OnyxToken* symbol) {
    OnyxToken *after_second_colon = expect_token(parser, ':');
    if (after_second_colon) after_second_colon += 1;

    AstTyped* node = parse_top_level_expression(parser);
    if (parser->hit_unexpected_token || node == NULL)
        return NULL;

    switch (node->kind) {
        case Ast_Kind_Function:
        case Ast_Kind_Polymorphic_Proc: {
            AstFunction* func = (AstFunction *) node;

            if (func->intrinsic_name == NULL) func->intrinsic_name = symbol;

            func->name = generate_name_within_scope(parser->context, parser->current_scope, symbol);
            func->assembly_name = func->name;
            func->flags &= ~Ast_Flag_Function_Is_Lambda;
            break;
        }

        case Ast_Kind_Macro: {
            AstMacro* macro = (AstMacro *) node;

            AstFunction* func = (AstFunction *) macro->body;
            func->name = generate_name_within_scope(parser->context, parser->current_scope, symbol);
            break;
        }

        case Ast_Kind_Directive_Init: break;

        case Ast_Kind_Global: ((AstGlobal *) node)->name = generate_name_within_scope(parser->context, parser->current_scope, symbol);

        case Ast_Kind_Overloaded_Function:
        case Ast_Kind_StrLit:
            break;

        // This makes a large assumption that the "name" member is in the same
        // place on all of these structures. It is, but maybe there should be a
        // "base" struct that these structure "inherit" from, so that is guaranteed?
        case Ast_Kind_Interface:
        case Ast_Kind_Struct_Type:
        case Ast_Kind_Poly_Struct_Type:
        case Ast_Kind_Enum_Type:
        case Ast_Kind_Distinct_Type:
        case Ast_Kind_Union_Type:
        case Ast_Kind_Poly_Union_Type:
            ((AstStructType *) node)->name = generate_name_within_scope(parser->context, parser->current_scope, symbol);
            goto default_case;

        case Ast_Kind_Type_Alias:
            node->token = symbol;
            goto default_case;

        case Ast_Kind_Package: goto default_case;
        case Ast_Kind_NumLit:  goto default_case;

        default: {
            if (!node_is_type((AstNode *) node)) {
                AstAlias* alias = make_node(AstAlias, Ast_Kind_Alias);
                alias->token = node->token;
                alias->alias = node;
                node = (AstTyped *) alias;
            }

default_case:
            ENTITY_SUBMIT(node);
        }
    }

    AstBinding* binding = make_node(AstBinding, Ast_Kind_Binding);
    binding->token = symbol;
    binding->node = (AstNode *) node;

    if (after_second_colon) expect_no_stored_tags_pos(parser, after_second_colon->pos);
    return binding;
}


static void parse_implicit_injection(OnyxParser* parser) {
    if (parser->injection_point) {
        ONYX_ERROR(parser->curr->pos, Error_Critical, "Implicit injection is not allowed here.");
        parser->hit_unexpected_token = 1;
        return;
    }

    AstFieldAccess *injection_expression = (AstFieldAccess *) parse_type(parser);

    if (peek_token(0)->type == Token_Type_Proc_Macro_Body) {
        AstProceduralExpansion *proc_expand = make_node(AstProceduralExpansion, Ast_Kind_Procedural_Expansion);
        proc_expand->token = injection_expression->token;
        proc_expand->expansion_body = expect_token(parser, Token_Type_Proc_Macro_Body);
        proc_expand->proc_macro = (AstTyped *) injection_expression;

        ENTITY_SUBMIT(proc_expand);
        return;
    }

    // Experimental syntax for overload adding.
    //
    // overload :: #match {}
    // overload <- (...) { ... }
    //
    // if (peek_token(0)->type == Token_Type_Left_Arrow) {
    //     AstDirectiveAddOverload *add_overload = make_node(AstDirectiveAddOverload, Ast_Kind_Directive_Add_Overload);
    //     add_overload->overloaded_function = (AstNode *) injection_expression;
    //     add_overload->token = expect_token(parser, Token_Type_Left_Arrow);
    //     add_overload->order = parser->overload_count++;
    //     add_overload->overload = parse_expression(parser, 0);

    //     if (add_overload->overload) {
    //         add_overload->overload->flags &= ~Ast_Flag_Function_Is_Lambda;
    //     }

    //     ENTITY_SUBMIT(add_overload);
    //     return;
    // }

    if (injection_expression->kind != Ast_Kind_Field_Access) {
        ONYX_ERROR(parser->curr->pos, Error_Critical, "Expected binding target to end in something like '.xyz'.");
        parser->hit_unexpected_token = 1;
        return;
    }

    AstInjection *inject = make_node(AstInjection, Ast_Kind_Injection);
    inject->token = injection_expression->token;
    inject->full_loc = (AstTyped *) injection_expression;

    AstTyped *target = injection_expression->expr;
    parser->injection_point = target;

    if (next_tokens_are(parser, 2, ':', ':')) {
        consume_token(parser);
        inject->binding = parse_top_level_binding(parser, inject->token);
        if (inject->binding) {
            flush_doc_tokens(parser, &inject->binding->documentation_string, &inject->binding->documentation_token_old);
        }

    } else {
        AstMemRes* memres = parse_memory_reservation(parser, inject->token, 0);

        inject->binding = make_node(AstBinding, Ast_Kind_Binding);
        inject->binding->token = inject->token;
        inject->binding->node = (AstNode *) memres;
    }

    ENTITY_SUBMIT(inject);

    parser->injection_point = NULL;
    return;
}


static void parse_top_level_statement(OnyxParser* parser) {
    AstFlags private_kind = 0;

  retry_because_inserted_semicolon:
    if (bh_arr_length(parser->scope_flags) > 0)
        private_kind = bh_arr_last(parser->scope_flags);

    // :CLEANUP this very repetetive code...
    if (next_tokens_are(parser, 2, '#', Token_Type_Keyword_Package)) {
        consume_tokens(parser, 2);
        private_kind = Ast_Flag_Private_Package;
        if (parser->curr->type == '{') {
            bh_arr_push(parser->scope_flags, private_kind);

            expect_token(parser, '{');
            parse_top_level_statements_until(parser, '}');
            expect_token(parser, '}');

            bh_arr_pop(parser->scope_flags);
            return;
        }
    }
    else if (parse_possible_directive(parser, "local")) {
        private_kind = Ast_Flag_Private_File;
        if (parser->curr->type == '{') {
            bh_arr_push(parser->scope_flags, private_kind);

            expect_token(parser, '{');
            parse_top_level_statements_until(parser, '}');
            expect_token(parser, '}');

            bh_arr_pop(parser->scope_flags);
            return;
        }
    }

    AstBinding* binding = NULL;

    if (parse_possible_tag(parser)) return;

    switch ((u16) parser->curr->type) {
        case Token_Type_Keyword_Use: {
            OnyxToken *use_token = expect_token(parser, Token_Type_Keyword_Use);
            parse_import_statement(parser, use_token);
            return;
        }

        case Token_Type_Symbol: {
            // Handle implicit injections as 'Foo.bar ::' or 'Foo(T).bar ::'
            if (   peek_token(1)->type == '.'
                || peek_token(1)->type == '('
                || peek_token(1)->type == Token_Type_Left_Arrow) {
                parse_implicit_injection(parser);
                return;
            }

            OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);

            if (next_tokens_are(parser, 2, ':', ':')) {
                expect_token(parser, ':');

                bh_arr_push(parser->current_symbol_stack, symbol);
                binding = parse_top_level_binding(parser, symbol);
                bh_arr_pop(parser->current_symbol_stack);

                // bh_printf("%b: %d\n", symbol->text, symbol->length, private_kind);
                if (binding != NULL) binding->flags |= private_kind;

                goto submit_binding_to_entities;
            }

            AstMemRes* memres = parse_memory_reservation(parser, symbol, 0);

            binding = make_node(AstBinding, Ast_Kind_Binding);
            binding->token = symbol;
            binding->flags |= private_kind;
            binding->node = (AstNode *) memres;

            goto submit_binding_to_entities;
        }

        case '(': {
            AstTyped *retval = NULL;
            if (parse_possible_function_definition(parser, &retval)) {
                ENTITY_SUBMIT(retval);
                return;
            }
            if (parse_possible_quick_function_definition(parser, &retval)) {
                ENTITY_SUBMIT(retval);
                return;
            }

            ONYX_ERROR(parser->curr->pos, Error_Critical, "Unexpected '(' at top-level.");
            parser->hit_unexpected_token = 1;
            break;
        }

        case Token_Type_Doc_Comment: {
            bh_arr_push(parser->documentation_tokens, expect_token(parser, Token_Type_Doc_Comment));
            break;
        }

        case '#': {
            if (next_tokens_are(parser, 2, '#', Token_Type_Keyword_If)) {
                AstIf* static_if = parse_static_if_stmt(parser, 0);
                ENTITY_SUBMIT(static_if);
                return;
            }

            OnyxToken* dir_token = parser->curr;

            if (parse_possible_directive(parser, "load")) {
                AstInclude* include = make_node(AstInclude, Ast_Kind_Load_File);
                include->token = dir_token;
                include->name_node = parse_expression(parser, 0);

                ENTITY_SUBMIT(include);
                return;
            }
            else if (parse_possible_directive(parser, "load_all")) {
                AstInclude* include = make_node(AstInclude, Ast_Kind_Load_All);
                include->token = dir_token;
                include->name_node = parse_expression(parser, 0);

                ENTITY_SUBMIT(include);
                return;
            }
            else if (parse_possible_directive(parser, "load_all_recursive")) {
                AstInclude* include = make_node(AstInclude, Ast_Kind_Load_All);
                include->token = dir_token;
                include->name_node = parse_expression(parser, 0);
                include->recursive = 1;

                ENTITY_SUBMIT(include);
                return;
            }
            else if (parse_possible_directive(parser, "load_path")) {
                AstInclude* include = make_node(AstInclude, Ast_Kind_Load_Path);
                include->token = dir_token;
                include->name_node = parse_expression(parser, 0);

                ENTITY_SUBMIT(include);
                return;
            }
            else if (parse_possible_directive(parser, "library_path")) {
                AstInclude* include = make_node(AstInclude, Ast_Kind_Library_Path);
                include->token = dir_token;
                include->name_node = parse_expression(parser, 0);

                ENTITY_SUBMIT(include);
                return;
            }
            else if (parse_possible_directive(parser, "error")) {
                AstDirectiveError *error = make_node(AstDirectiveError, Ast_Kind_Directive_Error);
                error->token = dir_token;
                error->error_msg = expect_token(parser, Token_Type_Literal_String);

                ENTITY_SUBMIT(error);
                return;
            }
            else if (parse_possible_directive(parser, "foreign")) {
                parse_foreign_block(parser, parser->curr - 2);
                return;
            }
            else if (parse_possible_directive(parser, "operator")) {
                AstDirectiveOperator *operator = make_node(AstDirectiveOperator, Ast_Kind_Directive_Operator);
                operator->token = dir_token;
                operator->operator_token = parser->curr;

                // These cases have to happen first because these are not necessarily "binary operators",
                // they are just things that I want to be able to overload. []= is technically a ternary
                // operator so all these things are horribly named anyway.
                if (next_tokens_are(parser, 3, '^', '[', ']')
                    || next_tokens_are(parser, 3, '&', '[', ']')) {
                    consume_tokens(parser, 3);
                    operator->operator = Binary_Op_Ptr_Subscript;
                    goto operator_determined;
                }

                if (next_tokens_are(parser, 3, '[', ']', '=')) {
                    consume_tokens(parser, 3);
                    operator->operator = Binary_Op_Subscript_Equals;
                    goto operator_determined;
                }

                // The default case
                BinaryOp op = binary_op_from_token_type(parser->curr->type);
                consume_token(parser);
                if (op == Binary_Op_Subscript) expect_token(parser, ']');    // #operator [] ... needs to consume the other ']'

                operator->operator = op;

              operator_determined:
                if (parse_possible_directive(parser, "order")) {
                    AstNumLit* pre = parse_int_literal(parser);
                    if (parser->hit_unexpected_token) return;

                    operator->order = bh_max(pre->value.l, 0);

                } else {
                    operator->order = parser->overload_count++;
                }

                if (next_tokens_are(parser, 2, ':', ':')) {
                    consume_tokens(parser, 2);
                }

                operator->overload = parse_expression(parser, 0);

                ENTITY_SUBMIT(operator);
                return;
            }
            else if (parse_possible_directive(parser, "match") || parse_possible_directive(parser, "overload")) {
                AstDirectiveAddOverload *add_overload = make_node(AstDirectiveAddOverload, Ast_Kind_Directive_Add_Overload);
                add_overload->token = dir_token;

                if (parse_possible_directive(parser, "order")) {
                    AstNumLit* pre = parse_int_literal(parser);
                    if (parser->hit_unexpected_token) return;

                    add_overload->order = bh_max(pre->value.l, 0);
                } else {
                    add_overload->order = parser->overload_count++;
                }

                parser->parse_calls = 0;
                add_overload->overloaded_function = (AstNode *) parse_expression(parser, 0);
                parser->parse_calls = 1;

                // Allow for
                //      #match
                //      something :: (....) {
                //      }
                //
                // This will make converting something to a overloaded
                // function easier and require less copying by the programmer.
                if (next_tokens_are(parser, 2, ':', ':')) {
                    consume_tokens(parser, 2);
                }

                add_overload->overload = parse_expression(parser, 0);
                add_overload->overload->flags &= ~Ast_Flag_Function_Is_Lambda;

                ENTITY_SUBMIT(add_overload);
                return;
            }
            else if (parse_possible_directive(parser, "inject")) {
                AstAlias *injection_point = make_node(AstAlias, Ast_Kind_Alias);
                injection_point->alias = parse_expression(parser, 0);
                injection_point->token = injection_point->alias->token;

                consume_token_if_next(parser, Token_Type_Inserted_Semicolon);
                if (peek_token(0)->type == '{') {
                    if (parser->injection_point) {
                        ONYX_ERROR(dir_token->pos, Error_Critical, "#inject blocks cannot be nested.");
                        return;
                    }

                    parser->injection_point = (AstTyped *) injection_point;

                    expect_token(parser, '{');
                    parse_top_level_statements_until(parser, '}');
                    expect_token(parser, '}');

                    parser->injection_point = NULL;
                    return;
                }

                // See comment above
                if (next_tokens_are(parser, 2, ':', ':')) {
                    consume_token(parser);
                }

                AstInjection *inject = make_node(AstInjection, Ast_Kind_Injection);
                inject->token = dir_token;
                inject->full_loc = (AstTyped *) injection_point;
                inject->binding = parse_top_level_binding(parser, injection_point->token);
                if (inject->binding) {
                    flush_doc_tokens(parser, &inject->binding->documentation_string, &inject->binding->documentation_token_old);
                }

                ENTITY_SUBMIT(inject);
                return;
            }
            else if (parse_possible_directive(parser, "export")) {
                AstDirectiveExport *export = make_node(AstDirectiveExport, Ast_Kind_Directive_Export);
                export->token = dir_token;
                parser->parse_calls = 0;
                export->export_name_expr = parse_expression(parser, 0); // expect_token(parser, Token_Type_Literal_String);
                parser->parse_calls = 1;

                export->export = parse_expression(parser, 0);

                ENTITY_SUBMIT(export);
                return;
            }
            else if (parse_possible_directive(parser, "thread_local")) {
                OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
                AstMemRes* memres = parse_memory_reservation(parser, symbol, 1);

                binding = make_node(AstBinding, Ast_Kind_Binding);
                binding->token = symbol;
                binding->flags |= private_kind;
                binding->node = (AstNode *) memres;

                goto submit_binding_to_entities;
            }
            else if (parse_possible_directive(parser, "init")) {
                // :LinearTokenDependent
                parse_init_directive(parser, parser->curr - 2);
                return;
            }
            else if (parse_possible_directive(parser, "library")) {
                // :LinearTokenDependent
                AstDirectiveLibrary *library = make_node(AstDirectiveLibrary, Ast_Kind_Directive_Library);
                library->token = parser->curr - 2;
                library->library_symbol = parse_expression(parser, 0);

                ENTITY_SUBMIT(library);
                return;
            }
            else if (parse_possible_directive(parser, "js")) {
                AstJsNode *jsNode = make_node(AstJsNode, Ast_Kind_Js_Code);
                jsNode->token = parser->curr - 2;
                jsNode->order = 0xffffffff;

                if (parse_possible_directive(parser, "order")) {
                    jsNode->order_expr = parse_expression(parser, 0);
                }

                if (parse_possible_directive(parser, "file")) {
                    jsNode->filepath = parse_expression(parser, 0);
                } else {
                    jsNode->code = parse_expression(parser, 0);
                }

                ENTITY_SUBMIT(jsNode);
                return;
            }
            else if (parse_possible_directive(parser, "doc")) {
                // This is a future feature I want to add to the language, proper docstrings.
                // For now (and so I start documenting thing...), #doc can be used anywhere
                // at top level, followed by a string to add a doc string.
                parser->last_documentation_token = expect_token(parser, Token_Type_Literal_String);
                return;
            }
            else if (parse_possible_directive(parser, "wasm_section")) {
                AstDirectiveWasmSection *section = make_node(AstDirectiveWasmSection, Ast_Kind_Directive_Wasm_Section);
                section->token = parser->curr - 2;
                section->section_name = parse_expression(parser, 0);

                if (parse_possible_directive(parser, "file")) {
                    section->from_file = 1;
                }

                section->section_contents = parse_expression(parser, 0);

                ENTITY_SUBMIT(section);
                return;
            }
            else {
                OnyxToken* directive_token = expect_token(parser, '#');
                OnyxToken* symbol_token = parser->curr;
                consume_token(parser);

                ONYX_ERROR(directive_token->pos, Error_Critical, "Unknown directive '#%b'.", symbol_token->text, symbol_token->length);

                if (symbol_token->type > Token_Type_Keyword_Start && symbol_token->type < Token_Type_Keyword_End) {
                    ONYX_ERROR(directive_token->pos, Error_Critical, "Did you mean the keyword, '%s'?",
                        token_name(symbol_token));
                }

                return;
            }

            break;
        }

        case ';':
            break;

        case Token_Type_Inserted_Semicolon:
            consume_token(parser);
            goto retry_because_inserted_semicolon;

        default:
            ONYX_ERROR(parser->curr->pos, Error_Critical, "Unexpected token in top-level statement, '%s'", token_name(parser->curr));
            parser->hit_unexpected_token = 1;
            break;
    }

    return;

submit_binding_to_entities:
    {
        if (!binding) return;

        flush_doc_tokens(parser, &binding->documentation_string, &binding->documentation_token_old);

        //
        // If this binding is inside an #inject block,
        // swap the binding node for an injection node
        // onto the injection point. This is the easiest
        // way I could think of doing this.
        if (parser->injection_point) {
            AstInjection *injection = make_node(AstInjection, Ast_Kind_Injection);
            injection->token = parser->injection_point->token;
            injection->dest = parser->injection_point;
            injection->symbol = binding->token;
            injection->binding = binding;

            ENTITY_SUBMIT(injection);
            return;
        }

        Scope* target_scope = parser->package->scope;

        if (binding->flags & Ast_Flag_Private_Package)
            target_scope = parser->package->private_scope;
        if (binding->flags & Ast_Flag_Private_File)
            target_scope = parser->file_scope;

        binding->flags |= Ast_Flag_Binding_Isnt_Captured;

        ENTITY_SUBMIT_IN_SCOPE(binding, target_scope);
    }
}

static b32 parse_package_name(OnyxParser *parser, AstPackage *package) {
    bh_arr_new(parser->context->gp_alloc, package->path, 2);

    while (parser->curr->type == Token_Type_Symbol) {
        if (parser->hit_unexpected_token) return 0;

        OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
        bh_arr_push(package->path, symbol);

        if (!consume_token_if_next(parser, '.')) break;
    }

    i32 total_package_name_length = 0;
    bh_arr_each(OnyxToken *, token, package->path) {
        total_package_name_length += (*token)->length + 1;
    }

    char* package_name = bh_alloc_array(parser->context->ast_alloc, char, total_package_name_length);
    *package_name = '\0';

    bh_arr_each(OnyxToken *, token, package->path) {
        token_toggle_end(*token);
        strncat(package_name, (*token)->text, total_package_name_length - 1);
        token_toggle_end(*token);

        if (token != &bh_arr_last(package->path)) {
            strncat(package_name, ".", total_package_name_length - 1);
        }
    }

    package->package_name = package_name;
    return 1;
}

static AstPackage* parse_package_expression(OnyxParser* parser) {
    AstPackage* package_node = make_node(AstPackage, Ast_Kind_Package);
    package_node->flags |= Ast_Flag_Comptime;
    package_node->type_node = parser->context->builtins.package_id_type;
    package_node->token = expect_token(parser, Token_Type_Keyword_Package);

    if (!parse_package_name(parser, package_node)) return NULL;

    return package_node;
}

static void parse_import_statement(OnyxParser* parser, OnyxToken *token) {
    AstPackage* package_node = make_node(AstPackage, Ast_Kind_Package);
    package_node->flags |= Ast_Flag_Comptime;
    package_node->type_node = parser->context->builtins.package_id_type;
    package_node->token = token;

    if (peek_token(0)->type == Token_Type_Keyword_Package) {
        package_node->token = expect_token(parser, Token_Type_Keyword_Package);
    }

    if (!parse_package_name(parser, package_node)) return;

    AstImport *import_node = make_node(AstImport, Ast_Kind_Import);
    import_node->flags |= Ast_Flag_Comptime;
    import_node->token = token;
    import_node->imported_package = package_node;
    import_node->import_package_itself = 1;

    consume_token_if_next(parser, Token_Type_Inserted_Semicolon);
    if (consume_token_if_next(parser, '{')) {
        import_node->specified_imports = 1;
        import_node->import_package_itself = 0;

        if (next_tokens_are(parser, 4, Token_Type_Symbol, ':', ':', Token_Type_Keyword_Package)) {
            import_node->qualified_package_name = expect_token(parser, Token_Type_Symbol);
            consume_tokens(parser, 3);

            import_node->import_package_itself = 1;
            if (parser->curr->type != '}')
                expect_token(parser, ',');
        }

        else if (consume_token_if_next(parser, Token_Type_Keyword_Package)) {
            import_node->import_package_itself = 1;
            if (parser->curr->type != '}')
                expect_token(parser, ',');
        }

        if (next_tokens_are(parser, 2, '*', '}')) {
            consume_tokens(parser, 2);
            goto import_parsed;
        }

        bh_arr_new(parser->context->gp_alloc, import_node->only, 4);
        while (!consume_token_if_next(parser, '}')) {
            if (parser->hit_unexpected_token) return;

            QualifiedImport qi;
            qi.as_name = expect_token(parser, Token_Type_Symbol);
            qi.symbol_name = qi.as_name;

            if (consume_token_if_next(parser, ':')) {
                expect_token(parser, ':');
                qi.symbol_name = expect_token(parser, Token_Type_Symbol);
            }

            bh_arr_push(import_node->only, qi);

            if (parser->curr->type != '}')
                expect_token(parser, ',');
        }
    }

  import_parsed:
    ENTITY_SUBMIT(import_node);
}

static Package* parse_file_package(OnyxParser* parser) {
    if (parser->curr->type != Token_Type_Keyword_Package) {
        return package_lookup_or_create(parser->context, "main", parser->context->global_scope, parser->curr->pos);
    }

    AstPackage* package_node = parse_package_expression(parser);

    char aggregate_name[2048];
    aggregate_name[0] = '\0';

    Package* prevpackage = NULL;

    bh_arr_each(OnyxToken *, symbol, package_node->path) {
        token_toggle_end(*symbol);

        strncat(aggregate_name, (*symbol)->text, 2047);
        Package* newpackage = package_lookup_or_create(parser->context, aggregate_name, parser->context->global_scope, package_node->token->pos);
        newpackage->parent_id = prevpackage ? prevpackage->id : 0xffffffff;

        AstPackage* pnode = make_node(AstPackage, Ast_Kind_Package);
        pnode->token = *symbol;
        pnode->package = newpackage;
        pnode->package_name = newpackage->name;
        pnode->type_node = parser->context->builtins.package_id_type;
        pnode->flags |= Ast_Flag_Comptime;

        if (prevpackage != NULL) {
            symbol_subpackage_introduce(parser->context, prevpackage, (*symbol)->text, pnode);
            package_reinsert_use_packages(parser->context, prevpackage);
        }

        token_toggle_end(*symbol);
        strncat(aggregate_name, ".", 2047);

        prevpackage = newpackage;
    }

    package_node->package = prevpackage;

    return package_node->package;
}

static void parse_top_level_statements_until(OnyxParser* parser, TokenType tt) {
    while (parser->curr->type != tt) {
        if (parser->hit_unexpected_token) break;
        if (onyx_has_errors(parser->context)) break;
        parse_top_level_statement(parser);
        consume_token_if_next(parser, ';');
    }
}


// NOTE: This returns a void* so I don't need to cast it everytime I use it
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind) {
    AstNode* node = bh_alloc(alloc, size);

    memset(node, 0, size);
    node->kind = kind;

    return node;
}

OnyxParser onyx_parser_create(Context *context, OnyxTokenizer *tokenizer) {
    OnyxParser parser;

    parser.allocator = context->ast_alloc;
    parser.context = context;
    parser.tokenizer = tokenizer;
    parser.curr = tokenizer->tokens;
    parser.prev = NULL;
    parser.hit_unexpected_token = 0;
    parser.current_scope = NULL;
    parser.alternate_entity_placement_stack = NULL;
    parser.current_symbol_stack = NULL;
    parser.current_function_stack = NULL;
    parser.scope_flags = NULL;
    parser.stored_tags = NULL;
    parser.parse_calls = 1;
    parser.parse_quick_functions = 1;
    parser.tag_depth = 0;
    parser.overload_count = 0;
    parser.injection_point = NULL;
    parser.last_documentation_token = NULL;
    parser.allow_package_expressions = 0;
    parser.documentation_tokens = NULL;

    parser.polymorph_context = (PolymorphicContext) {
        .root_node = NULL,
        .poly_params = NULL,
    };

    bh_arr_new(context->gp_alloc, parser.alternate_entity_placement_stack, 4);
    bh_arr_new(context->gp_alloc, parser.current_symbol_stack, 4);
    bh_arr_new(context->gp_alloc, parser.scope_flags, 4);
    bh_arr_new(context->gp_alloc, parser.stored_tags, 4);
    bh_arr_new(context->gp_alloc, parser.current_function_stack, 4);
    bh_arr_new(context->gp_alloc, parser.documentation_tokens, 8);

    return parser;
}

void onyx_parser_free(OnyxParser* parser) {
    bh_arr_free(parser->alternate_entity_placement_stack);
    bh_arr_free(parser->current_symbol_stack);
    bh_arr_free(parser->scope_flags);
    bh_arr_free(parser->stored_tags);
    bh_arr_free(parser->current_function_stack);
    bh_arr_free(parser->documentation_tokens);
}

AstTyped *onyx_parse_expression(OnyxParser *parser, Scope *scope) {
    parser->current_scope = scope;
    AstTyped *expr = parse_expression(parser, 0);

    return expr;
}

AstNode  *onyx_parse_statement(OnyxParser *parser, Scope *scope) {
    parser->current_scope = scope;
    AstNode *stmt = parse_statements_until(parser, Token_Type_End_Stream);

    return stmt;
}

void onyx_parse_top_level_statements(OnyxParser *parser, Scope *scope) {
    if (peek_token(0)->type == Token_Type_Keyword_Package) {
        parser->package = parse_file_package(parser);
        assert(parser->package);
    }

    parser->current_scope = scope;
    parse_top_level_statements_until(parser, Token_Type_End_Stream);
}

void onyx_parse(OnyxParser *parser) {
    // NOTE: Skip comments at the beginning of the file
    while (consume_token_if_next(parser, Token_Type_Comment));

    while (parser->curr->type == Token_Type_Doc_Comment) {
        bh_arr_push(parser->documentation_tokens, expect_token(parser, Token_Type_Doc_Comment));
    }

    parser->package = parse_file_package(parser);
    assert(parser->package);

    {
        const char *doc_string = NULL;
        flush_doc_tokens(parser, &doc_string, NULL);
        if (doc_string && strlen(doc_string) > 0) {
            bh_arr_push(parser->package->doc_strings, doc_string);
        }
    }

    parser->file_scope = scope_create(parser->context, parser->package->private_scope, parser->tokenizer->tokens[0].pos);
    parser->current_scope = parser->file_scope;

    consume_token_if_next(parser, ';');

    if (parse_possible_directive(parser, "allow_stale_code")
        && !parser->package->is_included_somewhere
        && !parser->context->options->no_stale_code) {
        bh_arr_new(parser->context->gp_alloc, parser->package->buffered_entities, 32);
        bh_arr_push(parser->alternate_entity_placement_stack, &parser->package->buffered_entities);
    }

    consume_token_if_next(parser, ';');

    while (parse_possible_directive(parser, "package_doc")) {
        OnyxToken *doc_string = expect_token(parser, Token_Type_Literal_String);
        consume_token_if_next(parser, ';');

        bh_arr_push(parser->package->doc_string_tokens, doc_string);
    }

    parse_top_level_statements_until(parser, Token_Type_End_Stream);

    parser->current_scope = parser->current_scope->parent;
}
