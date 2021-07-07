// The sole job of the parser for Onyx is to submit nodes to the
// entity heap for further processing. These nodes include things
// such as procedure definitions, string literals, struct definitions
// and declarations to be introduced into scopes.

// Things that need to be cleaned up in the parser:
//  - control block local variables should be more extensible and reuse more code

#include "onyxlex.h"
#include "onyxerrors.h"
#include "onyxparser.h"
#include "onyxutils.h"

#define make_node(nclass, kind)             onyx_ast_node_new(parser->allocator, sizeof(nclass), kind)
// :LinearTokenDependent
#define peek_token(ahead)                   (parser->curr + ahead)

static AstNode error_node = { Ast_Kind_Error, 0, NULL, NULL };

#define ENTITY_SUBMIT(node)                 (submit_entity_in_scope(parser, (AstNode *) (node), parser->current_scope, parser->package))
#define ENTITY_SUBMIT_IN_SCOPE(node, scope) (submit_entity_in_scope(parser, (AstNode *) (node), scope, parser->package))

void submit_entity_in_scope(OnyxParser* parser, AstNode* node, Scope* scope, Package* package) {
    if (bh_arr_length(parser->alternate_entity_placement_stack) == 0) {
        add_entities_for_node(NULL, node, scope, package);

    } else {
        bh_arr(Entity *) *entity_array = bh_arr_last(parser->alternate_entity_placement_stack);
        add_entities_for_node(entity_array, node, scope, package);
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
static AstSwitch*     parse_switch_stmt(OnyxParser* parser);
static i32            parse_possible_symbol_declaration(OnyxParser* parser, AstNode** ret);
static AstReturn*     parse_return_stmt(OnyxParser* parser);
static AstNode*       parse_use_stmt(OnyxParser* parser);
static AstBlock*      parse_block(OnyxParser* parser, b32 make_a_new_scope);
static AstNode*       parse_statement(OnyxParser* parser);
static AstType*       parse_type(OnyxParser* parser);
static AstStructType* parse_struct(OnyxParser* parser);
static void           parse_function_params(OnyxParser* parser, AstFunction* func);
static b32            parse_possible_directive(OnyxParser* parser, const char* dir);
static b32            parse_possible_function_definition(OnyxParser* parser, AstTyped** ret);
static AstFunction*   parse_function_definition(OnyxParser* parser, OnyxToken* token);
static AstTyped*      parse_global_declaration(OnyxParser* parser);
static AstEnumType*   parse_enum_declaration(OnyxParser* parser);
static AstIf*         parse_static_if_stmt(OnyxParser* parser, b32 parse_block_as_statements);
static AstTyped*      parse_top_level_expression(OnyxParser* parser);
static AstBinding*    parse_top_level_binding(OnyxParser* parser, OnyxToken* symbol);
static void           parse_top_level_statement(OnyxParser* parser);
static AstPackage*    parse_package_expression(OnyxParser* parser);

static void consume_token(OnyxParser* parser) {
    if (parser->hit_unexpected_token) return;

    parser->prev = parser->curr;
    // :LinearTokenDependent
    parser->curr++;
    while (parser->curr->type == Token_Type_Comment || parser->curr->type == Token_Type_Note) {
        if (parser->curr->type == Token_Type_Note) {
            AstNote* note = make_node(AstNode, Ast_Kind_Note);
            note->token = parser->curr;
            ENTITY_SUBMIT(note);
        }

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
    consume_token(parser);

    if (token->type != token_type) {
        onyx_report_error(token->pos, "expected token '%s', got '%s'.", token_name(token_type), token_name(token->type));
        parser->hit_unexpected_token = 1;
        // :LinearTokenDependent
        parser->curr = &parser->tokenizer->tokens[bh_arr_length(parser->tokenizer->tokens) - 1];
        return NULL;
    }

    return token;
}

static b32 consume_token_if_next(OnyxParser* parser, TokenType token_type) {
    if (parser->hit_unexpected_token) return 0;

    if (parser->curr->type == token_type) {
        consume_token(parser);
        return 1;
    } else {
        return 0;
    }
}

static void consume_tokens(OnyxParser* parser, i32 n) {
    fori (i, 0, n) consume_token(parser);
}

static b32 next_tokens_are(OnyxParser* parser, i32 n, ...) {
    va_list va;
    va_start(va, n);

    i32 matched = 1;

    // BUG: This does not take into consideration comments and notes that can occur between any tokens.
    fori (i, 0, n) {
        TokenType expected_type = va_arg(va, TokenType);
        if (peek_token(i)->type != expected_type) {
            matched = 0;
            break;
        }
    }

    va_end(va);
    return matched;
}


// TODO: Make parsing numeric literals not rely on the C standard libary.
static AstNumLit* parse_int_literal(OnyxParser* parser) {
    AstNumLit* int_node = make_node(AstNumLit, Ast_Kind_NumLit);
    int_node->token = expect_token(parser, Token_Type_Literal_Integer);
    int_node->flags |= Ast_Flag_Comptime;
    int_node->value.l = 0ll;

    token_toggle_end(int_node->token);

    char* first_invalid = NULL;
    i64 value = strtoll(int_node->token->text, &first_invalid, 0);

    int_node->value.l = value;

    // NOTE: Hex literals are unsigned.
    if (int_node->token->length >= 2 && int_node->token->text[1] == 'x') {
        if ((u64) value >= ((u64) 1 << 32))
            int_node->type_node = (AstType *) &basic_type_u64;
        else
            int_node->type_node = (AstType *) &basic_type_u32;
    } else {
        int_node->type_node = (AstType *) &basic_type_int_unsized;
    }

    token_toggle_end(int_node->token);
    return int_node;
}

static AstNumLit* parse_float_literal(OnyxParser* parser) {
    AstNumLit* float_node = make_node(AstNumLit, Ast_Kind_NumLit);
    float_node->token = expect_token(parser, Token_Type_Literal_Float);
    float_node->flags |= Ast_Flag_Comptime;
    float_node->value.d = 0.0;

    AstType* type = (AstType *) &basic_type_float_unsized;
    token_toggle_end(float_node->token);

    if (float_node->token->text[float_node->token->length - 1] == 'f') {
        type = (AstType *) &basic_type_f32;
        float_node->value.f = strtof(float_node->token->text, NULL);
    } else {
        float_node->value.d = strtod(float_node->token->text, NULL);
    }

    float_node->type_node = type;

    token_toggle_end(float_node->token);
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

    arguments_initialize(&sl->args);

    expect_token(parser, '.');
    expect_token(parser, '{');

    parse_arguments(parser, '}', &sl->args);

    *ret = (AstTyped *) sl;
    return 1;
}

static b32 parse_possible_array_literal(OnyxParser* parser, AstTyped* left, AstTyped** ret) {
    if (!next_tokens_are(parser, 2, '.', '[')) return 0;
    
    AstArrayLiteral* al = make_node(AstArrayLiteral, Ast_Kind_Array_Literal);
    al->token = parser->curr;
    al->atnode = left;

    bh_arr_new(global_heap_allocator, al->values, 4);
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

static AstTyped* parse_factor(OnyxParser* parser) {
    AstTyped* retval = NULL;

    switch ((u16) parser->curr->type) {
        case '(': {
            if (parse_possible_function_definition(parser, &retval)) break;

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

        case '^': {
            AstAddressOf* aof_node = make_node(AstAddressOf, Ast_Kind_Address_Of);
            aof_node->token = expect_token(parser, '^');
            aof_node->expr  = parse_factor(parser);

            retval = (AstTyped *) aof_node;
            break;
        }

        case '.': {
            if (parse_possible_struct_literal(parser, NULL, &retval)) return retval;
            if (parse_possible_array_literal(parser, NULL, &retval))  return retval;
            if (parse_possible_unary_field_access(parser, &retval))   return retval;
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
            expect_token(parser, ')');

            cast_node->expr = parse_factor(parser);

            retval = (AstTyped *) cast_node;
            break;
        }

        case Token_Type_Keyword_Sizeof: {
            AstSizeOf* so_node = make_node(AstSizeOf, Ast_Kind_Size_Of);
            so_node->token = expect_token(parser, Token_Type_Keyword_Sizeof);
            so_node->so_ast_type = (AstType *) parse_type(parser);
            so_node->type_node = (AstType *) &basic_type_i32;

            retval = (AstTyped *) so_node;
            break;
        }

        case Token_Type_Keyword_Alignof: {
            AstAlignOf* ao_node = make_node(AstAlignOf, Ast_Kind_Align_Of);
            ao_node->token = expect_token(parser, Token_Type_Keyword_Alignof);
            ao_node->ao_ast_type = (AstType *) parse_type(parser);
            ao_node->type_node = (AstType *) &basic_type_i32;

            retval = (AstTyped *) ao_node;
            break;
        }

        case Token_Type_Symbol: {
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
            str_node->addr      = 0;
            str_node->flags    |= Ast_Flag_Comptime;
            
            ENTITY_SUBMIT(str_node);
            
            retval = (AstTyped *) str_node;
            break;
        }

        case Token_Type_Literal_True: {
            AstNumLit* bool_node = make_node(AstNumLit, Ast_Kind_NumLit);
            bool_node->type_node = (AstType *) &basic_type_bool;
            bool_node->token = expect_token(parser, Token_Type_Literal_True);
            bool_node->value.i = 1;
            bool_node->flags |= Ast_Flag_Comptime;
            retval = (AstTyped *) bool_node;
            break;
        }

        case Token_Type_Literal_False: {
            AstNumLit* bool_node = make_node(AstNumLit, Ast_Kind_NumLit);
            bool_node->type_node = (AstType *) &basic_type_bool;
            bool_node->token = expect_token(parser, Token_Type_Literal_False);
            bool_node->value.i = 0;
            bool_node->flags |= Ast_Flag_Comptime;
            retval = (AstTyped *) bool_node;
            break;
        }

        case Token_Type_Keyword_Proc: {
            OnyxToken* proc_token = expect_token(parser, Token_Type_Keyword_Proc);
            onyx_report_warning(proc_token->pos, "Warning: 'proc' is a deprecated keyword.");
            retval = (AstTyped *) parse_function_definition(parser, proc_token);
            break;
        }

        case Token_Type_Keyword_Package: {
            retval = (AstTyped *) parse_package_expression(parser);
            break;
        }

        // :TypeValueInterchange
        case '<': {
            AstTypeAlias* alias = make_node(AstTypeAlias, Ast_Kind_Type_Alias);
            alias->token = expect_token(parser, '<');
            alias->to = parse_type(parser);
            expect_token(parser, '>');

            retval = (AstTyped *) alias;
            break;
        }

        case '#': {
            if (parse_possible_directive(parser, "file_contents")) {
                AstFileContents* fc = make_node(AstFileContents, Ast_Kind_File_Contents);
                fc->token = parser->prev - 1;
                fc->filename_token = expect_token(parser, Token_Type_Literal_String);
                fc->type = type_make_slice(parser->allocator, &basic_types[Basic_Kind_U8]);
                
                ENTITY_SUBMIT(fc);
                
                retval = (AstTyped *) fc;
                break;
            }
            else if (parse_possible_directive(parser, "file")) {
                OnyxToken* dir_token = parser->curr - 2;

                OnyxToken* str_token = bh_alloc(parser->allocator, sizeof(OnyxToken));
                str_token->text  = bh_strdup(global_heap_allocator, (char *) dir_token->pos.filename);
                str_token->length = strlen(dir_token->pos.filename);
                str_token->pos = dir_token->pos;
                str_token->type = Token_Type_Literal_String;

                AstStrLit* filename = make_node(AstStrLit, Ast_Kind_StrLit);
                filename->token = str_token;
                filename->addr  = 0;
                
                ENTITY_SUBMIT(filename);
                retval = (AstTyped *) filename;
                break;
            }
            else if (parse_possible_directive(parser, "line")) {
                OnyxToken* dir_token = parser->curr - 2;

                AstNumLit* line_num = make_int_literal(parser->allocator, dir_token->pos.line);
                retval = (AstTyped *) line_num;
                break;
            }
            else if (parse_possible_directive(parser, "column")) {
                OnyxToken* dir_token = parser->curr - 2;

                AstNumLit* col_num = make_int_literal(parser->allocator, dir_token->pos.column);
                retval = (AstTyped *) col_num;
                break;
            }
            else if (parse_possible_directive(parser, "char")) {
                AstNumLit* char_lit = make_node(AstNumLit, Ast_Kind_NumLit);
                char_lit->flags |= Ast_Flag_Comptime;
                char_lit->type_node = (AstType *) &basic_type_int_unsized;
                char_lit->token = expect_token(parser, Token_Type_Literal_String);

                i8 dest = '\0';
                i32 length = string_process_escape_seqs((char *) &dest, char_lit->token->text, 1);
                char_lit->value.i = (u32) dest;

                if (length != 1) {
                    onyx_report_error(char_lit->token->pos, "Expected only a single character in character literal.");
                }

                retval = (AstTyped *) char_lit;
                break;
            }
            else if (parse_possible_directive(parser, "type")) {
                AstTypeAlias* alias = make_node(AstTypeAlias, Ast_Kind_Type_Alias);
                alias->to = parse_type(parser);
                retval = (AstTyped *) alias;
                break;
            }
            else if (parse_possible_directive(parser, "solidify")) {
                AstDirectiveSolidify* solid = make_node(AstDirectiveSolidify, Ast_Kind_Directive_Solidify);
                // :LinearTokenDependent
                solid->token = parser->curr - 1;

                solid->poly_proc = (AstPolyProc *) parse_factor(parser);

                solid->known_polyvars = NULL;
                bh_arr_new(global_heap_allocator, solid->known_polyvars, 2);

                expect_token(parser, '{');
                while (!consume_token_if_next(parser, '}')) {
                    if (parser->hit_unexpected_token) break;

                    AstNode* poly_var = make_node(AstNode, Ast_Kind_Symbol);
                    poly_var->token = expect_token(parser, Token_Type_Symbol);

                    expect_token(parser, '=');
                    AstType* poly_type = parse_type(parser);

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
                defined->type_node = (AstType *) &basic_type_bool;

                expect_token(parser, '(');
                defined->expr = parse_expression(parser, 0);
                expect_token(parser, ')');

                retval = (AstTyped *) defined;
                break;
            }

            onyx_report_error(parser->curr->pos, "Invalid directive in expression.");
            return NULL;
        }

        default:
        no_match:
            onyx_report_error(parser->curr->pos, "Unexpected token '%s'.", token_name(parser->curr->type));
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
                AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                field->token = expect_token(parser, Token_Type_Symbol);
                field->expr  = retval;

                retval = (AstTyped *) field;
                break;
            }

            case '(': {
                AstCall* call_node = make_node(AstCall, Ast_Kind_Call);
                call_node->token = expect_token(parser, '(');
                call_node->callee = retval;

                arguments_initialize(&call_node->args);

                parse_arguments(parser, ')', &call_node->args);

                // Wrap expressions in AstArgument
                bh_arr_each(AstTyped *, arg, call_node->args.values) {
                    if ((*arg) == NULL) continue;
                    *arg = (AstTyped *) make_argument(parser->allocator, *arg);
                }

                bh_arr_each(AstNamedValue *, named_value, call_node->args.named_values) {
                    if ((*named_value)->value == NULL) continue;
                    (*named_value)->value = (AstTyped *) make_argument(parser->allocator, (AstTyped *) (*named_value)->value);
                }

                retval = (AstTyped *) call_node;
                break;
            }

            case Token_Type_Right_Arrow: {
                AstBinaryOp* method_call = make_node(AstBinaryOp, Ast_Kind_Method_Call);
                method_call->token = expect_token(parser, Token_Type_Right_Arrow);
                method_call->left = retval;
                method_call->right = parse_factor(parser);

                retval = (AstTyped *) method_call;
                break;
            }

            case Token_Type_Keyword_If: {
                AstIfExpression* if_expression = make_node(AstIfExpression, Ast_Kind_If_Expression);
                if_expression->token = expect_token(parser, Token_Type_Keyword_If);

                if_expression->true_expr  = retval;
                if_expression->cond       = parse_expression(parser, 0);
                expect_token(parser, Token_Type_Keyword_Else);
                if_expression->false_expr = parse_expression(parser, 0);

                retval = (AstTyped *) if_expression;

                // nocheckin This should maybe be goto factor_parsed; ??
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
        case '[':                          return Binary_Op_Subscript;
        default: return Binary_Op_Count;
    }
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

        bh_arr_new(global_heap_allocator, compound->exprs, 2);
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
    bh_arr_new(global_heap_allocator, tree_stack, 4);
    bh_arr_set_length(tree_stack, 0);

    AstTyped* left = parse_factor(parser);
    AstTyped* right;
    AstTyped* root = left;

    BinaryOp bin_op_kind;
    OnyxToken* bin_op_tok;

    while (1) {
        if (parser->hit_unexpected_token) return root;

        bin_op_kind = binary_op_from_token_type(parser->curr->type);
        if (bin_op_kind == Binary_Op_Count) goto expression_done;
        if (binop_is_assignment(bin_op_kind) && !assignment_allowed) goto expression_done;
        if (bin_op_kind == Binary_Op_Subscript) goto expression_done;

        bin_op_tok = parser->curr;
        consume_token(parser);

        AstBinaryOp* bin_op;
        if      (bin_op_kind == Binary_Op_Pipe)  bin_op = make_node(AstBinaryOp, Ast_Kind_Pipe);
        else if (bin_op_kind == Binary_Op_Range) bin_op = (AstBinaryOp *) make_node(AstRangeLiteral, Ast_Kind_Range_Literal);
        else                                     bin_op = make_node(AstBinaryOp, Ast_Kind_Binary_Op);

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

    // CLEANUP: All of the control blocks use this same kind of logic, and it
    // is underpowered for what I think should be possible. Factor it out and
    // make it better?
    if (peek_token(1)->type == ':') {
        OnyxToken* local_sym = expect_token(parser, Token_Type_Symbol);
        if_node->local = make_local(parser->allocator, local_sym, NULL);

        expect_token(parser, ':');

        AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
        assignment->operation = Binary_Op_Assign;
        assignment->token = expect_token(parser, '=');
        assignment->left = (AstTyped *) if_node->local;
        assignment->right = parse_expression(parser, 0);

        if_node->assignment = assignment;
        expect_token(parser, ';');
    }

    // NOTE: Assignment is allowed here because instead of not parsing correctly,
    // an error is reported in the typechecking, saying that assignment isn't allowed
    // here, which is better than an unexpected token error.
    AstTyped* cond = parse_expression(parser, 1);
    AstBlock* true_stmt = parse_block(parser, 1);

    if_node->cond = cond;
    if (true_stmt != NULL)
        if_node->true_stmt = true_stmt;

    while (consume_token_if_next(parser, Token_Type_Keyword_Elseif)) {
        if (parser->hit_unexpected_token) return root_if;

        AstIfWhile* elseif_node = make_node(AstIfWhile, Ast_Kind_If);
        elseif_node->token = parser->curr - 1;

        cond = parse_expression(parser, 1);
        true_stmt = parse_block(parser, 1);

        elseif_node->cond = cond;
        if (true_stmt != NULL)
            elseif_node->true_stmt = true_stmt;

        if_node->false_stmt = (AstBlock *) elseif_node;
        if_node = elseif_node;
    }

    if (consume_token_if_next(parser, Token_Type_Keyword_Else)) {
        AstBlock* false_stmt = parse_block(parser, 1);
        if (false_stmt != NULL)
            if_node->false_stmt = false_stmt;
    }

    return root_if;
}

static AstIfWhile* parse_while_stmt(OnyxParser* parser) {
    OnyxToken* while_token = expect_token(parser, Token_Type_Keyword_While);
    AstIfWhile* while_node = make_node(AstIfWhile, Ast_Kind_While);
    while_node->token = while_token;

    // CLEANUP: See above in parse_if_stmt
    if (peek_token(1)->type == ':') {
        OnyxToken* local_sym = expect_token(parser, Token_Type_Symbol);
        while_node->local = make_local(parser->allocator, local_sym, NULL);

        expect_token(parser, ':');

        AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
        assignment->operation = Binary_Op_Assign;
        assignment->token = expect_token(parser, '=');
        assignment->left = (AstTyped *) while_node->local;
        assignment->right = parse_expression(parser, 0);

        while_node->assignment = assignment;
        expect_token(parser, ';');
    }

    while_node->cond = parse_expression(parser, 1);
    while_node->true_stmt = parse_block(parser, 1);

    if (consume_token_if_next(parser, Token_Type_Keyword_Else)) {
        while_node->false_stmt = parse_block(parser, 1);
    }

    return while_node;
}

static AstFor* parse_for_stmt(OnyxParser* parser) {
    AstFor* for_node = make_node(AstFor, Ast_Kind_For);
    for_node->token = expect_token(parser, Token_Type_Keyword_For);

    if (consume_token_if_next(parser, '^')) {
        for_node->by_pointer = 1;
    }

    OnyxToken* local_sym = expect_token(parser, Token_Type_Symbol);
    AstLocal* var_node = make_local(parser->allocator, local_sym, NULL);

    for_node->var = var_node;

    expect_token(parser, ':');
    for_node->iter = parse_expression(parser, 1);
    for_node->stmt = parse_block(parser, 1);

    return for_node;
}

static AstSwitch* parse_switch_stmt(OnyxParser* parser) {
    AstSwitch* switch_node = make_node(AstSwitch, Ast_Kind_Switch);
    switch_node->token = expect_token(parser, Token_Type_Keyword_Switch);

    bh_arr_new(global_heap_allocator, switch_node->cases, 4);

    // CLEANUP: See above in parse_if_stmt
    if (peek_token(1)->type == ':') {
        OnyxToken* local_sym = expect_token(parser, Token_Type_Symbol);
        switch_node->local = make_local(parser->allocator, local_sym, NULL);

        expect_token(parser, ':');

        AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
        assignment->operation = Binary_Op_Assign;
        assignment->token = expect_token(parser, '=');
        assignment->left = (AstTyped *) switch_node->local;
        assignment->right = parse_expression(parser, 0);

        switch_node->assignment = assignment;
        expect_token(parser, ';');
    }

    switch_node->expr = parse_expression(parser, 1);
    expect_token(parser, '{');

    while (consume_token_if_next(parser, Token_Type_Keyword_Case)) {
        if (parser->hit_unexpected_token) return switch_node;

        bh_arr(AstTyped *) case_values = NULL;
        bh_arr_new(global_heap_allocator, case_values, 1);

        if (parse_possible_directive(parser, "default")) {
            switch_node->default_case = parse_block(parser, 1);

            if (parser->curr->type != '}') {
                onyx_report_error(parser->curr->pos, "The #default case must be the last case in a switch statement.\n");
            }
            break;
        }

        AstTyped* value = parse_expression(parser, 1);
        bh_arr_push(case_values, value);
        while (consume_token_if_next(parser, ',')) {
            if (parser->hit_unexpected_token) return switch_node;

            value = parse_expression(parser, 1);
            bh_arr_push(case_values, value);
        }

        AstBlock* block = parse_block(parser, 1);

        AstSwitchCase sc_node;
        sc_node.block  = block;
        sc_node.values = case_values;

        bh_arr_push(switch_node->cases, sc_node);
    }

    expect_token(parser, '}');
    return switch_node;
}

static i32 parse_possible_compound_symbol_declaration(OnyxParser* parser, AstNode** ret) {
    u32 token_offset = 0;
    while (peek_token(token_offset)->type == Token_Type_Symbol) {
        token_offset += 1;

        if (peek_token(token_offset)->type != ',') break;
        token_offset += 1;
    }

    if (peek_token(token_offset)->type != ':') return 0;

    // At this point, we are sure it is a compound declaration.
    AstCompound* local_compound = make_node(AstCompound, Ast_Kind_Compound);
    bh_arr_new(global_heap_allocator, local_compound->exprs, token_offset / 2);

    AstLocal* first_local = NULL;
    AstLocal* prev_local  = NULL;

    while (parser->curr->type == Token_Type_Symbol) {
        if (parser->hit_unexpected_token) return 1;

        OnyxToken* local_sym = expect_token(parser, Token_Type_Symbol);
        AstLocal* new_local = make_local(parser->allocator, local_sym, NULL);

        if (prev_local == NULL) {
            first_local = new_local;
        } else {
            prev_local->next = (AstNode *) new_local;
        }
        prev_local = new_local;

        AstNode* sym_node = make_symbol(parser->allocator, local_sym);
        bh_arr_push(local_compound->exprs, (AstTyped *) sym_node);

        consume_token_if_next(parser, ',');
    }

    expect_token(parser, ':');

    if (parser->curr->type == '=') {
        AstBinaryOp* assignment = make_binary_op(parser->allocator, Binary_Op_Assign, (AstTyped *) local_compound, NULL);
        assignment->token = expect_token(parser, '=');
        assignment->right = parse_compound_expression(parser, 0);

        prev_local->next = (AstNode *) assignment;

    } else {
        AstType* type_for_all = parse_type(parser);
        forll (AstLocal, local, first_local, next) {
            local->type_node = type_for_all;
        }
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
    if (peek_token(1)->type == ',') {
        return parse_possible_compound_symbol_declaration(parser, ret);
    }

    if (peek_token(1)->type != ':') return 0;

    OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
    expect_token(parser, ':');

    if (parser->curr->type == ':') {
        AstBinding* binding = parse_top_level_binding(parser, symbol);
        if (parser->hit_unexpected_token) return 2;
        
        ENTITY_SUBMIT(binding);
        return 2;
    }

    AstType* type_node = NULL;
    if (parser->curr->type != '=') {
        type_node = parse_type(parser);
    }

    AstLocal* local = make_local(parser->allocator, symbol, type_node);
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
        AstNode* left_symbol = make_node(AstNode, Ast_Kind_Symbol);
        left_symbol->token = symbol;
        assignment->left = (AstTyped *) left_symbol;
    }

    return 1;
}

static AstReturn* parse_return_stmt(OnyxParser* parser) {
    AstReturn* return_node = make_node(AstReturn, Ast_Kind_Return);
    return_node->token = expect_token(parser, Token_Type_Keyword_Return);

    AstTyped* expr = NULL;

    if (parser->curr->type != ';') {
        expr = parse_compound_expression(parser, 0);

        if (expr == NULL || expr == (AstTyped *) &error_node) {
            return (AstReturn *) &error_node;
        } else {
            return_node->expr = expr;
        }
    }

    return return_node;
}

static AstNode* parse_use_stmt(OnyxParser* parser) {
    OnyxToken* use_token = expect_token(parser, Token_Type_Keyword_Use);
    AstUse* use_node = make_node(AstUse, Ast_Kind_Use);
    use_node->token = use_token;
    use_node->expr = parse_expression(parser, 1);

    if (consume_token_if_next(parser, '{')) {
        bh_arr_new(global_heap_allocator, use_node->only, 4);

        while (!consume_token_if_next(parser, '}')) {
            if (parser->hit_unexpected_token) return NULL;

            QualifiedUse qu;
            qu.symbol_name = expect_token(parser, Token_Type_Symbol);

            if (consume_token_if_next(parser, Token_Type_Keyword_As))
                qu.as_name = expect_token(parser, Token_Type_Symbol);
            else
                qu.as_name = qu.symbol_name;

            bh_arr_push(use_node->only, qu);

            if (parser->curr->type != '}')
                expect_token(parser, ',');
        }
    }

    if (use_node->expr->kind == Ast_Kind_Package) {
        ENTITY_SUBMIT(use_node);
        return NULL;

    } else {
        return (AstNode *) use_node;
    }
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
            retval = (AstNode *) parse_block(parser, 1);
            break;

        case Token_Type_Symbol: {
            i32 symbol_res = parse_possible_symbol_declaration(parser, &retval);
            if (symbol_res == 2) needs_semicolon = 0;
            if (symbol_res != 0) break;
            
            // fallthrough
        }

        case '(': case '+': case '-': case '!': case '*': case '^':
        case Token_Type_Literal_Integer:
        case Token_Type_Literal_Float:
        case Token_Type_Literal_String:
            retval = (AstNode *) parse_compound_expression(parser, 1);
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
            retval = (AstNode *) parse_switch_stmt(parser);
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

            retval = (AstNode *) parse_use_stmt(parser);
            break;
        }

        case '#': {
            if (parse_possible_directive(parser, "context_scope")) {
                AstLocal* context_tmp = make_local(parser->allocator, NULL, builtin_context_variable->type_node);

                AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
                assignment->operation = Binary_Op_Assign;
                assignment->left = (AstTyped *) context_tmp;
                assignment->right = builtin_context_variable;
                context_tmp->next = (AstNode *) assignment;

                AstBinaryOp* assignment2 = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
                assignment2->operation = Binary_Op_Assign;
                assignment2->left = builtin_context_variable;
                assignment2->right = (AstTyped *) context_tmp;

                AstBlock* context_block = parse_block(parser, 1);
                assignment->next = (AstNode *) context_block;

                AstDefer* defer_node = make_node(AstDefer, Ast_Kind_Defer);
                defer_node->stmt = (AstNode *) assignment2;
                defer_node->next = context_block->body;
                context_block->body = (AstNode *) defer_node;

                needs_semicolon = 0;
                retval = (AstNode *) context_tmp;
                break;
            }

            /*
            This is in theory where the static if in procedures will be parsed. However,
            this breaks many things because static if statements currently only parse top
            level expressions in them, not general statements.
            */

            if (next_tokens_are(parser, 2, '#', Token_Type_Keyword_If)) {
                AstIf* static_if = parse_static_if_stmt(parser, 1);
                ENTITY_SUBMIT(static_if);

                needs_semicolon = 0;
                retval = (AstNode *) static_if;
                break;
            }
        }

        default:
            break;
    }

    if (needs_semicolon) expect_token(parser, ';');

    return retval;
}

static AstBlock* parse_block(OnyxParser* parser, b32 make_a_new_scope) {
    AstBlock* block = make_node(AstBlock, Ast_Kind_Block);

    // NOTE: --- is for an empty block
    if (parser->curr->type == Token_Type_Empty_Block) {
        block->token = expect_token(parser, Token_Type_Empty_Block);
        return block;
    }

    if (make_a_new_scope) {
        block->binding_scope = scope_create(parser->allocator, parser->current_scope, parser->curr->pos);
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
        onyx_report_error(parser->curr->pos, "Polymorphic variable not valid here.");
    else
        pv = *parser->polymorph_context.poly_params;

    consume_token(parser);

    AstNode* symbol_node = make_node(AstNode, Ast_Kind_Symbol);
    symbol_node->token = expect_token(parser, Token_Type_Symbol);

    **next_insertion = (AstType *) symbol_node;
    *next_insertion = NULL;

    if (pv != NULL) {
        bh_arr_push(pv, ((AstPolyParam) {
            .kind     = PPK_Poly_Type,
            .poly_sym = symbol_node,

            // These will be filled out by function_params()
            .type_expr = NULL,
            .idx = -1,
        }));

        *parser->polymorph_context.poly_params = pv;
    }
}

static AstType* parse_compound_type(OnyxParser* parser) {
    // CLEANUP this is little weird having this here because it means that this parses:
    //
    //      foo :: (x: (something_here: i32)) -> void ---
    //
    if (next_tokens_are(parser, 2, Token_Type_Symbol, ':')) {
        consume_tokens(parser, 2);
    }

    AstType* first = parse_type(parser);

    if (parser->curr->type == ',') {
        AstCompoundType* ctype = make_node(AstCompoundType, Ast_Kind_Type_Compound);
        ctype->token = parser->curr;

        bh_arr_new(global_heap_allocator, ctype->types, 2);
        bh_arr_push(ctype->types, first);

        while (consume_token_if_next(parser, ',')) {
            if (parser->hit_unexpected_token) return (AstType *) ctype;

            if (next_tokens_are(parser, 2, Token_Type_Symbol, ':')) {
                consume_tokens(parser, 2);
            }

            bh_arr_push(ctype->types, parse_type(parser));
        }

        return (AstType *) ctype;

    } else {
        return first;
    }
}

static AstType* parse_function_type(OnyxParser* parser, OnyxToken* proc_token) {
    bh_arr(AstType *) params = NULL;
    bh_arr_new(global_scratch_allocator, params, 4);
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

    AstType* return_type = (AstType *) &basic_type_void;
    if (consume_token_if_next(parser, Token_Type_Right_Arrow))
        return_type = parse_type(parser);

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

    while (1) {
        if (parser->hit_unexpected_token) return root;

        switch ((u16) parser->curr->type) {
            case '^': {
                AstPointerType* new = make_node(AstPointerType, Ast_Kind_Pointer_Type);
                new->flags |= Basic_Flag_Pointer;
                new->token = expect_token(parser, '^');

                *next_insertion = (AstType *) new;
                next_insertion = &new->elem;
                break;
            }

            case '[': {
                AstType *new;
                OnyxToken *open_bracket = expect_token(parser, '[');

                if (parser->curr->type == ']') {
                    new = make_node(AstSliceType, Ast_Kind_Slice_Type);
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
                break;
            }

            case Token_Type_Keyword_Proc: {
                OnyxToken* proc_token = expect_token(parser, Token_Type_Keyword_Proc);
                onyx_report_warning(proc_token->pos, "Warning: 'proc' is a deprecated keyword.");
                *next_insertion = parse_function_type(parser, proc_token);
                next_insertion = NULL;
                break;
            }

            case '$': {
                parse_polymorphic_variable(parser, &next_insertion);
                break;
            }

            case Token_Type_Symbol: {
                AstNode* symbol_node = make_node(AstNode, Ast_Kind_Symbol);
                symbol_node->token = expect_token(parser, Token_Type_Symbol);

                *next_insertion = (AstType *) symbol_node;

                while (consume_token_if_next(parser, '.')) {
                    AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                    field->token = expect_token(parser, Token_Type_Symbol);
                    field->expr  = (AstTyped *) *next_insertion;

                    *next_insertion = (AstType *) field;
                }

                if (parser->curr->type == '(') {
                    OnyxToken* paren_token = expect_token(parser, '(');

                    bh_arr(AstNode *) params = NULL;
                    bh_arr_new(global_heap_allocator, params, 2);

                    while (!consume_token_if_next(parser, ')')) {
                        if (parser->hit_unexpected_token) break;

                        AstNode* t = (AstNode *) parse_type(parser);
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

                next_insertion = NULL;
                break;
            }

            case Token_Type_Keyword_Struct: {
                AstStructType* s_node = parse_struct(parser);
                *next_insertion = (AstType *) s_node;
                next_insertion = NULL;
                break;
            }

            case '#': {
                // :ValueDirectiveHack
                if (parse_possible_directive(parser, "value")) {
                // It is very weird to put these here.
                case Token_Type_Literal_Integer:
                case Token_Type_Literal_String:
                case Token_Type_Literal_Float:
                case Token_Type_Literal_True:
                case Token_Type_Literal_False:
                    *next_insertion = (AstType *) parse_expression(parser, 0);
                    next_insertion = NULL;
                    break;
                }

            }

            case '(': {
                OnyxToken* matching = find_matching_paren(parser->curr);

                // :LinearTokenDependent
                if ((matching + 1)->type == Token_Type_Right_Arrow) {
                    *next_insertion = parse_function_type(parser, parser->curr);

                } else {
                    expect_token(parser, '(');
                    *next_insertion = parse_compound_type(parser);
                    expect_token(parser, ')');
                }

                next_insertion = NULL;
                break;
            }

            default:
                onyx_report_error(parser->curr->pos, "unexpected token '%b'.", parser->curr->text, parser->curr->length);
                consume_token(parser);
                break;
        }

        if (next_insertion == NULL) break;
    }

    return root;
}

static AstStructType* parse_struct(OnyxParser* parser) {
    OnyxToken *s_token = expect_token(parser, Token_Type_Keyword_Struct);

    AstStructType* s_node;
    AstPolyStructType* poly_struct = NULL;

    s_node = make_node(AstStructType, Ast_Kind_Struct_Type);
    s_node->token = s_token;

    if (consume_token_if_next(parser, '(')) {
        bh_arr(AstPolyStructParam) poly_params = NULL;
        bh_arr_new(global_heap_allocator, poly_params, 1);

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
    }

    bh_arr_new(global_heap_allocator, s_node->members, 4);

    while (parser->curr->type == '#') {
        if (parser->hit_unexpected_token) return NULL;

        if (parse_possible_directive(parser, "union")) {
            s_node->flags |= Ast_Flag_Struct_Is_Union;
        }

        else if (parse_possible_directive(parser, "align")) {
            AstNumLit* numlit = parse_int_literal(parser);
            if (numlit == NULL) return NULL;

            s_node->min_alignment = numlit->value.i;
        }

        else if (parse_possible_directive(parser, "size")) {
            AstNumLit* numlit = parse_int_literal(parser);
            if (numlit == NULL) return NULL;

            s_node->min_size = numlit->value.i;
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_report_error(directive_token->pos, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    expect_token(parser, '{');

    b32 member_is_used = 0;
    bh_arr(OnyxToken *) member_list_temp = NULL;
    bh_arr_new(global_heap_allocator, member_list_temp, 4);

    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return s_node;
        
        member_is_used = consume_token_if_next(parser, Token_Type_Keyword_Use);

        if (next_tokens_are(parser, 3, Token_Type_Symbol, ':', ':')) {
            if (!s_node->scope) {
                s_node->scope = scope_create(context.ast_alloc, parser->current_scope, s_node->token->pos);
                parser->current_scope = s_node->scope;
            }

            OnyxToken* binding_name = expect_token(parser, Token_Type_Symbol);
            consume_token(parser);

            AstBinding* binding = parse_top_level_binding(parser, binding_name);
            if (binding) ENTITY_SUBMIT(binding);
            
            consume_token_if_next(parser, ';');

        } else {
            bh_arr_clear(member_list_temp);
            while (!consume_token_if_next(parser, ':')) {
                if (parser->hit_unexpected_token) return NULL;
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
                if (member_is_used) onyx_report_error((member_list_temp[0] - 1)->pos, "'use' is only allowed for a single struct member declaration. Try splitting this compound declaration into multiple lines.");
                if (initial_value)  onyx_report_error(initial_value->token->pos, "Intialized values are only allowed on single struct member declarations. Try splitting this compound initializer into multiple lines.");
            }

            bh_arr_each(OnyxToken *, member_name, member_list_temp) {
                AstStructMember* mem = make_node(AstStructMember, Ast_Kind_Struct_Member);
                mem->token = *member_name;
                mem->type_node = member_type;
                mem->initial_value = initial_value;

                if (member_is_used) mem->flags |= Ast_Flag_Struct_Mem_Used;

                bh_arr_push(s_node->members, mem);
            }

            expect_token(parser, ';');
        }
    }

    if (s_node->scope) parser->current_scope = parser->current_scope->parent;

    bh_arr_free(member_list_temp);

    if (poly_struct != NULL) {
        // NOTE: Not a StructType
        return (AstStructType *) poly_struct;

    } else {
        return s_node;
    }
}

static void parse_function_params(OnyxParser* parser, AstFunction* func) {
    expect_token(parser, '(');

    if (consume_token_if_next(parser, ')')) return;

    u32 param_idx = 0;
    assert(parser->polymorph_context.poly_params != NULL);

    b32 param_use = 0;
    b32 param_is_baked = 0;
    OnyxToken* symbol;
    while (!consume_token_if_next(parser, ')')) {
        if (parser->hit_unexpected_token) return;

        AstParam curr_param = { 0 };

        if (consume_token_if_next(parser, Token_Type_Keyword_Use)) param_use = 1;
        if (consume_token_if_next(parser, '$'))                    param_is_baked = 1;

        symbol = expect_token(parser, Token_Type_Symbol);
        expect_token(parser, ':');

        curr_param.vararg_kind = VA_Kind_Not_VA;
        curr_param.local = make_local(parser->allocator, symbol, NULL);
        curr_param.local->kind = Ast_Kind_Param;

        if (param_use) {
            curr_param.local->flags |= Ast_Flag_Param_Use;
            param_use = 0;
        }

        if (parser->curr->type != '=') {
            if (consume_token_if_next(parser, Token_Type_Dot_Dot)) {
                if (consume_token_if_next(parser, '.')) curr_param.vararg_kind = VA_Kind_Untyped;
                else                                    curr_param.vararg_kind = VA_Kind_Typed;
            }

            if (curr_param.vararg_kind != VA_Kind_Untyped) {
                // CLEANUP: This is mess and it is hard to follow what is going on here.
                // I think with recent rewrites, this should be easier to do.
                i32 old_len = bh_arr_length(*parser->polymorph_context.poly_params);
                curr_param.local->type_node = parse_type(parser);
                i32 new_len = bh_arr_length(*parser->polymorph_context.poly_params);

                if (curr_param.vararg_kind == VA_Kind_Typed) {
                    AstVarArgType* va_type = make_node(AstVarArgType, Ast_Kind_VarArg_Type);
                    va_type->elem = curr_param.local->type_node;
                    va_type->token = curr_param.local->type_node->token;
                    curr_param.local->type_node = (AstType *) va_type;
                }

                fori (i, 0, new_len - old_len) {
                    (*parser->polymorph_context.poly_params)[old_len + i].type_expr = curr_param.local->type_node;
                    (*parser->polymorph_context.poly_params)[old_len + i].idx = param_idx;
                }
            }
        }

        if (curr_param.vararg_kind == VA_Kind_Not_VA && consume_token_if_next(parser, '=')) {
            OnyxToken* directive_token = parser->curr;

            // :Callsite  currently #callsite is only valid as a default value for a funciton parameter.
            if (parse_possible_directive(parser, "callsite")) {
                AstCallSite* cs = make_node(AstCallSite, Ast_Kind_Call_Site);
                cs->token = directive_token;
                curr_param.default_value = (AstTyped *) cs;

            } else {
                curr_param.default_value = parse_expression(parser, 0);
            }
        }

        if (param_is_baked) {
            param_is_baked = 0;

            bh_arr(AstPolyParam) pv = *parser->polymorph_context.poly_params;
            bh_arr_push(pv, ((AstPolyParam) {
                .kind = PPK_Baked_Value,
                .idx = param_idx,

                .poly_sym = (AstNode *) curr_param.local,
                .type_expr = curr_param.local->type_node,
            }));

            *parser->polymorph_context.poly_params = pv;
        }

        bh_arr_push(func->params, curr_param);
        param_idx++;

        if (parser->curr->type != ')')
            expect_token(parser, ',');
    }
    return;
}

static AstOverloadedFunction* parse_overloaded_function(OnyxParser* parser, OnyxToken* token) {
    expect_token(parser, '{');

    AstOverloadedFunction* ofunc = make_node(AstOverloadedFunction, Ast_Kind_Overloaded_Function);
    ofunc->token = token;
    ofunc->flags |= Ast_Flag_Comptime;

    bh_arr_new(global_heap_allocator, ofunc->overloads, 4);

    u64 precedence = 0;
    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return ofunc;

        if (parse_possible_directive(parser, "precedence")) {
            AstNumLit* pre = parse_int_literal(parser);
            if (parser->hit_unexpected_token) return ofunc;

            precedence = bh_max(pre->value.l, 0);
        }

        AstTyped* option = parse_expression(parser, 0);
        add_overload_option(&ofunc->overloads, precedence++, option);

        if (parser->curr->type != '}')
            expect_token(parser, ',');
    }
    
    ENTITY_SUBMIT(ofunc);
    return ofunc;
}

static AstFunction* parse_function_definition(OnyxParser* parser, OnyxToken* token) {
    // :TemporaryForProcRemoval
    if (parser->curr->type == '{') {
        return (AstFunction *) parse_overloaded_function(parser, token);
    }

    AstFunction* func_def = make_node(AstFunction, Ast_Kind_Function);
    func_def->token = token;

    bh_arr_new(global_heap_allocator, func_def->params, 4);

    bh_arr(AstPolyParam) polymorphic_vars = NULL;
    bh_arr_new(global_heap_allocator, polymorphic_vars, 4);

    parser->polymorph_context.poly_params = &polymorphic_vars;
    parse_function_params(parser, func_def);
    parser->polymorph_context.poly_params = NULL;

    func_def->return_type = (AstType *) &basic_type_void;
    if (consume_token_if_next(parser, Token_Type_Right_Arrow))
        func_def->return_type = parse_type(parser);

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "intrinsic")) {
            func_def->flags |= Ast_Flag_Intrinsic;

            if (parser->curr->type == Token_Type_Literal_String) {
                func_def->intrinsic_name = expect_token(parser, Token_Type_Literal_String);
            }
        }

        else if (parse_possible_directive(parser, "foreign")) {
            func_def->foreign_module = expect_token(parser, Token_Type_Literal_String);
            func_def->foreign_name   = expect_token(parser, Token_Type_Literal_String);

            func_def->flags |= Ast_Flag_Foreign;
        }

        // HACK: NullProcHack
        else if (parse_possible_directive(parser, "null")) {
            func_def->flags |= Ast_Flag_Proc_Is_Null;
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_report_error(directive_token->pos, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    func_def->body = parse_block(parser, 1);

    if (bh_arr_length(polymorphic_vars) > 0) {
        AstPolyProc* pp = make_node(AstPolyProc, Ast_Kind_Polymorphic_Proc);
        pp->token = func_def->token;
        pp->poly_params = polymorphic_vars;
        pp->base_func = func_def;
        
        ENTITY_SUBMIT(pp);
        return (AstFunction *) pp;
    } else {
        bh_arr_free(polymorphic_vars);
        
        ENTITY_SUBMIT(func_def);
        return func_def;
    }
}

static b32 parse_possible_function_definition(OnyxParser* parser, AstTyped** ret) {
    if (parser->curr->type == Token_Type_Keyword_Proc) {
        OnyxToken* proc_token = expect_token(parser, Token_Type_Keyword_Proc);
        onyx_report_warning(proc_token->pos, "Warning: 'proc' is a deprecated keyword.");
        AstFunction* func_node = parse_function_definition(parser, proc_token);
        *ret = (AstTyped *) func_node;
        return 1;
    }

    if (parser->curr->type == '(') {
        OnyxToken* matching_paren = find_matching_paren(parser->curr);
        if (matching_paren == NULL) return 0;

        // :LinearTokenDependent
        OnyxToken* token_after_paren = matching_paren + 1;
        if (token_after_paren->type != Token_Type_Right_Arrow
            && token_after_paren->type != '{'
            && token_after_paren->type != Token_Type_Keyword_Do
            && token_after_paren->type != Token_Type_Empty_Block)
            return 0;

        // :LinearTokenDependent
        b32 is_params = (parser->curr + 1) == matching_paren;
        OnyxToken* tmp_token = parser->curr;
        while (!is_params && tmp_token < matching_paren) {
            if (tmp_token->type == ':') is_params = 1;

            tmp_token++;
        }

        if (!is_params) return 0;

        OnyxToken* proc_token = parser->curr;
        AstFunction* func_node = parse_function_definition(parser, proc_token);
        *ret = (AstTyped *) func_node;
        return 1;
    }

    return 0;
}

static AstTyped* parse_global_declaration(OnyxParser* parser) {
    AstGlobal* global_node = make_node(AstGlobal, Ast_Kind_Global);
    global_node->token = expect_token(parser, Token_Type_Keyword_Global);

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "foreign")) {
            global_node->foreign_module = expect_token(parser, Token_Type_Literal_String);
            global_node->foreign_name   = expect_token(parser, Token_Type_Literal_String);

            global_node->flags |= Ast_Flag_Foreign;
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_report_error(directive_token->pos, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    global_node->type_node = parse_type(parser);
    
    ENTITY_SUBMIT(global_node);

    return (AstTyped *) global_node;
}

static AstEnumType* parse_enum_declaration(OnyxParser* parser) {
    AstEnumType* enum_node = make_node(AstEnumType, Ast_Kind_Enum_Type);
    enum_node->token = expect_token(parser, Token_Type_Keyword_Enum);

    bh_arr_new(global_heap_allocator, enum_node->values, 4);

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "flags")) {
            enum_node->flags |= Ast_Flag_Enum_Is_Flags;
        } else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_report_error(directive_token->pos, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
        }
    }

    AstType* backing = (AstType *) &basic_type_u32;
    if (consume_token_if_next(parser, '(')) {
        AstNode* backing_sym = make_node(AstNode, Ast_Kind_Symbol);
        backing_sym->token = expect_token(parser, Token_Type_Symbol);
        backing = (AstType *) backing_sym;

        expect_token(parser, ')');
    }
    enum_node->backing = backing;

    expect_token(parser, '{');

    while (!consume_token_if_next(parser, '}')) {
        if (parser->hit_unexpected_token) return enum_node;

        AstEnumValue* evalue = make_node(AstEnumValue, Ast_Kind_Enum_Value);
        evalue->token = expect_token(parser, Token_Type_Symbol);
        evalue->type_node = (AstType *) enum_node;

        if (consume_token_if_next(parser, ':')) {
            expect_token(parser, ':');

            // TODO: Make this work for any expression.
            evalue->value = parse_int_literal(parser);
        }

        expect_token(parser, ';');

        bh_arr_push(enum_node->values, evalue);
    }

    return enum_node;
}

static AstIf* parse_static_if_stmt(OnyxParser* parser, b32 parse_block_as_statements) {
    AstIf* static_if_node = make_node(AstIf, Ast_Kind_Static_If);
    static_if_node->token = expect_token(parser, '#');
    expect_token(parser, Token_Type_Keyword_If);

    static_if_node->cond = parse_expression(parser, 0);

    bh_arr_new(global_heap_allocator, static_if_node->true_entities, 2);
    bh_arr_push(parser->alternate_entity_placement_stack, &static_if_node->true_entities);

    if (parse_block_as_statements) {
        static_if_node->true_stmt = parse_block(parser, 0);

    } else {
        expect_token(parser, '{');
        while (!consume_token_if_next(parser, '}')) {
            if (parser->hit_unexpected_token) return static_if_node;

            parse_top_level_statement(parser);
        }
    }

    bh_arr_pop(parser->alternate_entity_placement_stack);

    if (consume_token_if_next(parser, Token_Type_Keyword_Else)) {
        bh_arr_new(global_heap_allocator, static_if_node->false_entities, 2);
        bh_arr_push(parser->alternate_entity_placement_stack, &static_if_node->false_entities);

        if (parse_block_as_statements) {
            static_if_node->false_stmt = parse_block(parser, 0);

        } else {
            expect_token(parser, '{');
            while (!consume_token_if_next(parser, '}')) {
                if (parser->hit_unexpected_token) return static_if_node;

                parse_top_level_statement(parser);
            }
        }

        bh_arr_pop(parser->alternate_entity_placement_stack);
    }

    return static_if_node;
}

static AstTyped* parse_top_level_expression(OnyxParser* parser) {
    if (parser->curr->type == Token_Type_Keyword_Proc) {
        OnyxToken* proc_token = expect_token(parser, Token_Type_Keyword_Proc);
        onyx_report_warning(proc_token->pos, "Warning: 'proc' is a deprecated keyword.");
        AstFunction* func_node = parse_function_definition(parser, proc_token);

        return (AstTyped *) func_node;
    }
    
    if (parser->curr->type == Token_Type_Keyword_Global) return parse_global_declaration(parser);
    if (parser->curr->type == Token_Type_Keyword_Struct) return (AstTyped *) parse_struct(parser);
    if (parser->curr->type == Token_Type_Keyword_Enum)   return (AstTyped *) parse_enum_declaration(parser);
    
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
    
    return parse_expression(parser, 1);
}

static AstBinding* parse_top_level_binding(OnyxParser* parser, OnyxToken* symbol) {
    expect_token(parser, ':');

    AstTyped* node = parse_top_level_expression(parser);
    if (parser->hit_unexpected_token || node == NULL)
        return NULL;
    
    // CLEANUP
    if (node->kind == Ast_Kind_Function) {
        AstFunction* func = (AstFunction *) node;

        if (func->intrinsic_name == NULL)
            func->intrinsic_name = symbol;

        func->name = symbol;

    } else if (node->kind == Ast_Kind_Polymorphic_Proc) {
        AstPolyProc* proc = (AstPolyProc *) node;

        if (proc->base_func->intrinsic_name == NULL)
            proc->base_func->intrinsic_name = symbol;

        proc->base_func->name = symbol;

    } else if (node->kind == Ast_Kind_Global) {
        AstGlobal* global = (AstGlobal *) node;

        global->name = symbol;

    } else if (node->kind != Ast_Kind_Overloaded_Function
            && node->kind != Ast_Kind_StrLit) {

        if (node->kind == Ast_Kind_Struct_Type
                || node->kind == Ast_Kind_Enum_Type
                || node->kind == Ast_Kind_Poly_Struct_Type) {
            ((AstStructType *)node)->name = bh_aprintf(global_heap_allocator,
                "%b", symbol->text, symbol->length);
        }

        if (node->kind == Ast_Kind_Type_Alias) {
            node->token = symbol;
        }

        // HACK: This should maybe be entered elsewhere?
        ENTITY_SUBMIT(node);
    }
    
    AstBinding* binding = make_node(AstBinding, Ast_Kind_Binding);
    binding->token = symbol;
    binding->node = (AstNode *) node;

    return binding;
}

static void parse_top_level_statement(OnyxParser* parser) {
    AstFlags private_kind = 0;
    if      (parse_possible_directive(parser, "private"))      private_kind = Ast_Flag_Private_Package;
    else if (parse_possible_directive(parser, "private_file")) private_kind = Ast_Flag_Private_File;

    AstBinding* binding = NULL;
    
    switch ((u16) parser->curr->type) {
        case Token_Type_Keyword_Use: {
            AstNode* use_node = parse_use_stmt(parser);
            if (use_node) ENTITY_SUBMIT(use_node);
            return;
        }

        // case Token_Type_Keyword_Proc:
        //     onyx_report_warning(parser->curr->pos, "Warning: 'proc' is a deprecated keyword.");
        //     parse_top_level_expression(parser);
        //     return;

        case Token_Type_Symbol: {
            OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
            expect_token(parser, ':');

            if (parser->curr->type == ':') {
                binding = parse_top_level_binding(parser, symbol);
                if (binding != NULL) binding->flags |= private_kind;

                goto submit_binding_to_entities;
            }
            
            AstMemRes* memres = make_node(AstMemRes, Ast_Kind_Memres);
            memres->token = symbol;

            if (parser->curr->type != '=')
                memres->type_node = parse_type(parser);
            
            if (consume_token_if_next(parser, '='))
                memres->initial_value = parse_expression(parser, 1);
            
            
            ENTITY_SUBMIT(memres);
            
            binding = make_node(AstBinding, Ast_Kind_Binding);
            binding->token = symbol;
            binding->flags |= private_kind;
            binding->node = (AstNode *) memres;

            goto submit_binding_to_entities;
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

                OnyxToken* str_token = expect_token(parser, Token_Type_Literal_String);
                if (str_token != NULL) {
                    token_toggle_end(str_token);
                    include->name = bh_strdup(parser->allocator, str_token->text);
                    token_toggle_end(str_token);
                }
                
                ENTITY_SUBMIT(include);
                return;
            }
            else if (parse_possible_directive(parser, "load_path")) {
                AstInclude* include = make_node(AstInclude, Ast_Kind_Load_Path);
                include->token = dir_token;
                
                OnyxToken* str_token = expect_token(parser, Token_Type_Literal_String);
                if (str_token != NULL) {
                    token_toggle_end(str_token);
                    include->name = bh_strdup(parser->allocator, str_token->text);
                    token_toggle_end(str_token);
                }
                
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
            else if (parse_possible_directive(parser, "operator")) {
                AstDirectiveOperator *operator = make_node(AstDirectiveOperator, Ast_Kind_Directive_Operator);
                operator->token = dir_token;

                BinaryOp op = binary_op_from_token_type(parser->curr->type);
                consume_token(parser);
                if (op == Binary_Op_Subscript) expect_token(parser, ']');    // #operator [] ... needs to consume the other ']'
                
                if (op == Binary_Op_Count) {
                    onyx_report_error(parser->curr->pos, "Invalid binary operator.");
                } else {
                    operator->operator = op;
                }

                operator->overload = parse_expression(parser, 0);

                ENTITY_SUBMIT(operator);
                return;
            }
            else if (parse_possible_directive(parser, "add_overload") || parse_possible_directive(parser, "add_match")) {
                AstDirectiveAddOverload *add_overload = make_node(AstDirectiveAddOverload, Ast_Kind_Directive_Add_Overload);
                add_overload->token = dir_token;
                add_overload->overloaded_function = (AstNode *) parse_expression(parser, 0);

                expect_token(parser, ',');

                if (parse_possible_directive(parser, "precedence")) {
                    AstNumLit* pre = parse_int_literal(parser);
                    if (parser->hit_unexpected_token) return;

                    add_overload->precedence = bh_max(pre->value.l, 0);
                } else {
                    add_overload->precedence = 0;
                }

                add_overload->overload = parse_expression(parser, 0);

                ENTITY_SUBMIT(add_overload);
                return;
            }
            else if (parse_possible_directive(parser, "export")) {
                AstDirectiveExport *export = make_node(AstDirectiveExport, Ast_Kind_Directive_Export);
                export->token = dir_token;
                export->export_name = expect_token(parser, Token_Type_Literal_String);

                export->export = parse_expression(parser, 0);

                ENTITY_SUBMIT(export);
                return;
            }
            else {
                OnyxToken* directive_token = expect_token(parser, '#');
                OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

                onyx_report_error(directive_token->pos, "unknown directive '#%b'.", symbol_token->text, symbol_token->length);
                return;
            }
        }

        default: break;
    }

    expect_token(parser, ';');
    return;

submit_binding_to_entities:
    {
        if (!binding) return;

        Scope* target_scope = parser->package->scope;

        if (binding->flags & Ast_Flag_Private_Package)
            target_scope = parser->package->private_scope;
        if (binding->flags & Ast_Flag_Private_File)
            target_scope = parser->file_scope;
        
        ENTITY_SUBMIT_IN_SCOPE(binding, target_scope);
    }
}

static AstPackage* parse_package_expression(OnyxParser* parser) {
    AstPackage* package_node = make_node(AstPackage, Ast_Kind_Package);
    package_node->token = expect_token(parser, Token_Type_Keyword_Package);

    bh_arr_new(global_heap_allocator, package_node->path, 2);

    while (parser->curr->type == Token_Type_Symbol) {
        if (parser->hit_unexpected_token) return package_node;

        OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);

        bh_arr_push(package_node->path, symbol);
        
        if (consume_token_if_next(parser, '.'));
        else break;
    }

    i32 total_package_name_length = 0;
    bh_arr_each(OnyxToken *, token, package_node->path) {
        total_package_name_length += (*token)->length + 1;
    }

    char* package_name = bh_alloc_array(context.ast_alloc, char, total_package_name_length); 
    *package_name = '\0';

    bh_arr_each(OnyxToken *, token, package_node->path) {
        token_toggle_end(*token);
        strncat(package_name, (*token)->text, total_package_name_length - 1);
        token_toggle_end(*token);

        if (token != &bh_arr_last(package_node->path)) {
            strncat(package_name, ".", total_package_name_length - 1);
        }
    }

    package_node->package_name = package_name;
    package_node->package = package_lookup(package_name);

    return package_node;
}

static Package* parse_file_package(OnyxParser* parser) {
    if (parser->curr->type != Token_Type_Keyword_Package) {
        return package_lookup_or_create("main", context.global_scope, parser->allocator);
    }

    AstPackage* package_node = parse_package_expression(parser);
    
    char aggregate_name[2048];
    aggregate_name[0] = '\0';

    Package* prevpackage = NULL;

    bh_arr_each(OnyxToken *, symbol, package_node->path) {
        token_toggle_end(*symbol);

        strncat(aggregate_name, (*symbol)->text, 2047);
        Package* newpackage = package_lookup_or_create(aggregate_name, context.global_scope, parser->allocator);
        
        AstPackage* pnode = make_node(AstPackage, Ast_Kind_Package);
        pnode->token = *symbol;
        pnode->package = newpackage;
        pnode->package_name = newpackage->name;

        if (prevpackage != NULL) {
            symbol_subpackage_introduce(prevpackage->scope, (*symbol)->text, pnode);
            package_reinsert_use_packages(prevpackage);
        }

        token_toggle_end(*symbol);
        strncat(aggregate_name, ".", 2047);

        prevpackage = newpackage;
    }

    package_node->package = prevpackage;
    
    return package_node->package;
}


// NOTE: This returns a void* so I don't need to cast it everytime I use it
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind) {
    void* node = bh_alloc(alloc, size);

    memset(node, 0, size);
    *(AstKind *) node = kind;

    return node;
}

OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer) {
    OnyxParser parser;

    parser.allocator = alloc;
    parser.tokenizer = tokenizer;
    parser.curr = tokenizer->tokens;
    parser.prev = NULL;
    parser.hit_unexpected_token = 0;
    parser.current_scope = NULL;
    parser.alternate_entity_placement_stack = NULL;

    parser.polymorph_context = (PolymorphicContext) {
        .root_node = NULL,
        .poly_params = NULL,
    };

    bh_arr_new(global_heap_allocator, parser.alternate_entity_placement_stack, 4);

    return parser;
}

void onyx_parser_free(OnyxParser* parser) {
}

void onyx_parse(OnyxParser *parser) {
    // NOTE: Skip comments at the beginning of the file
    while (consume_token_if_next(parser, Token_Type_Comment) || consume_token_if_next(parser, Token_Type_Note));

    parser->package = parse_file_package(parser);
    parser->file_scope = scope_create(parser->allocator, parser->package->private_scope, parser->tokenizer->tokens[0].pos);
    parser->current_scope = parser->file_scope;

    AstUse* implicit_use_builtin = make_node(AstUse, Ast_Kind_Use);
    AstPackage* implicit_builtin_package = make_node(AstPackage, Ast_Kind_Package);
    implicit_builtin_package->package_name = "builtin";
    implicit_use_builtin->expr = (AstTyped *) implicit_builtin_package;
    ENTITY_SUBMIT(implicit_use_builtin);

    while (parser->curr->type != Token_Type_End_Stream) {
        if (parser->hit_unexpected_token) break;
        if (onyx_has_errors()) break;
        parse_top_level_statement(parser);
    }

    parser->current_scope = parser->current_scope->parent;
}
