#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxutils.h"

// NOTE: The one weird define you need to know before read the code below
#define make_node(nclass, kind) onyx_ast_node_new(parser->allocator, sizeof(nclass), kind)

static const char* ast_node_names[] = {
    "ERROR",
    "PROGRAM",
    "USE",

    "FUNCTION",
    "FOREIGN",
    "BLOCK",
    "SCOPE",
    "LOCAL",
    "GLOBAL",
    "SYMBOL",

    "UN_OP",
    "BIN_OP",

    "TYPE",
    "LITERAL",
    "PARAM",
    "ARGUMENT",
    "CALL",
    "ASSIGN",
    "RETURN",

    "IF",
    "WHILE",
    "BREAK",
    "CONTINUE",

    "AST_NODE_KIND_COUNT",
};

struct TypeInfo builtin_types[] = {
    { TYPE_INFO_KIND_UNKNOWN, 0, "unknown" },
    { TYPE_INFO_KIND_VOID, 0, "void", 0, 0, 0, 0, 1 },

    { TYPE_INFO_KIND_BOOL, 1, "bool", 0, 1, 0, 1, 1 },

    { TYPE_INFO_KIND_UINT32, 4, "u32", 1, 1, 0, 0, 1 },
    { TYPE_INFO_KIND_UINT64, 8, "u64", 1, 1, 0, 0, 1 },

    { TYPE_INFO_KIND_INT32, 4, "i32", 1, 0, 0, 0, 1 },
    { TYPE_INFO_KIND_INT64, 8, "i64", 1, 0, 0, 0, 1 },

    { TYPE_INFO_KIND_FLOAT32, 4, "f32", 0, 0, 1, 0, 1 },
    { TYPE_INFO_KIND_FLOAT64, 8, "f64", 0, 0, 1, 0, 1},
    { TYPE_INFO_KIND_SOFT_FLOAT, 8, "sf64", 0, 0, 1, 0, 1 },

    { 0xffffffff } // Sentinel
};

static AstNode error_node = { AST_NODE_KIND_ERROR, 0, NULL, NULL };

// NOTE: Forward declarations
static void parser_next_token(OnyxParser* parser);
static void parser_prev_token(OnyxParser* parser);
static b32 is_terminating_token(TokenType token_type);
static OnyxToken* expect(OnyxParser* parser, TokenType token_type);
static AstNodeScope* enter_scope(OnyxParser* parser);
static AstNodeScope* leave_scope(OnyxParser* parser);
static void insert_identifier(OnyxParser* parser, AstNode* ident, b32 is_local);
static void remove_identifier(OnyxParser* parser, AstNode* ident);
static AstNodeNumLit* parse_numeric_literal(OnyxParser* parser);
static AstNodeTyped* parse_factor(OnyxParser* parser);
static AstNodeTyped* parse_bin_op(OnyxParser* parser, AstNode* left);
static AstNodeTyped* parse_expression(OnyxParser* parser);
static AstNodeIf* parse_if_stmt(OnyxParser* parser);
static AstNodeWhile* parse_while_stmt(OnyxParser* parser);
static b32 parse_symbol_statement(OnyxParser* parser, AstNode** ret);
static AstNodeReturn* parse_return_statement(OnyxParser* parser);
static AstNodeBlock* parse_block(OnyxParser* parser);
static AstNode* parse_statement(OnyxParser* parser);
static TypeInfo* parse_type(OnyxParser* parser);
static AstNodeLocal* parse_function_params(OnyxParser* parser);
static AstNodeFunction* parse_function_definition(OnyxParser* parser);
static AstNode* parse_top_level_statement(OnyxParser* parser);

static void parser_next_token(OnyxParser* parser) {
    parser->prev_token = parser->curr_token;
    parser->curr_token++;
    while (parser->curr_token->type == TOKEN_TYPE_COMMENT) parser->curr_token++;
}

static void parser_prev_token(OnyxParser* parser) {
    // TODO: This is probably wrong
    while (parser->prev_token->type == TOKEN_TYPE_COMMENT) parser->prev_token--;
    parser->curr_token = parser->prev_token;
    parser->prev_token--;
}

static b32 is_terminating_token(TokenType token_type) {
    switch (token_type) {
    case TOKEN_TYPE_SYM_SEMICOLON:
    case TOKEN_TYPE_CLOSE_BRACE:
    case TOKEN_TYPE_OPEN_BRACE:
    case TOKEN_TYPE_END_STREAM:
        return 1;
    default:
        return 0;
    }
}

static void find_token(OnyxParser* parser, TokenType token_type) {
    while (parser->curr_token->type != token_type && !is_terminating_token(parser->curr_token->type)) {
        parser_next_token(parser);
    }
}

// Advances to next token no matter what
static OnyxToken* expect(OnyxParser* parser, TokenType token_type) {
    OnyxToken* token = parser->curr_token;
    parser_next_token(parser);

    if (token->type != token_type) {
        onyx_message_add(parser->msgs,
                         ONYX_MESSAGE_TYPE_EXPECTED_TOKEN,
                         token->pos,
                         onyx_get_token_type_name(token_type), onyx_get_token_type_name(token->type));
        return NULL;
    }

    return token;
}

static AstNodeNumLit* parse_numeric_literal(OnyxParser* parser) {
    AstNodeNumLit* lit_node = make_node(AstNodeNumLit, AST_NODE_KIND_LITERAL);
    lit_node->base.token = expect(parser, TOKEN_TYPE_LITERAL_NUMERIC);
    lit_node->base.flags |= ONYX_AST_FLAG_COMPTIME;
    lit_node->value.l = 0ll;

    onyx_token_null_toggle(lit_node->base.token);

    TypeInfo* type;
    char* tok = lit_node->base.token->token;

    // NOTE: charset_contains() behaves more like string_contains()
    // so I'm using it in this case
    if (charset_contains(tok, '.')) {
        if (tok[lit_node->base.token->length - 1] == 'f') {
            type = &builtin_types[TYPE_INFO_KIND_FLOAT32];
            lit_node->value.f = strtof(tok, NULL);
        } else {
            type = &builtin_types[TYPE_INFO_KIND_FLOAT64];
            lit_node->value.d = strtod(tok, NULL);
        }
    } else {
        i64 value = strtoll(tok, NULL, 0);
        if (bh_abs(value) < ((u64) 1 << 32)) {
            type = &builtin_types[TYPE_INFO_KIND_INT32];
        } else {
            type = &builtin_types[TYPE_INFO_KIND_INT64];
        }

        lit_node->value.l = value;
    }

    lit_node->base.type = type;
    onyx_token_null_toggle(lit_node->base.token);
    return lit_node;
}

static AstNodeTyped* parse_factor(OnyxParser* parser) {
    AstNodeTyped* retval = NULL;

    switch (parser->curr_token->type) {
        case TOKEN_TYPE_OPEN_PAREN:
            {
                parser_next_token(parser);
                AstNodeTyped* expr = parse_expression(parser);
                expect(parser, TOKEN_TYPE_CLOSE_PAREN);
                retval = expr;
                break;
            }

        case TOKEN_TYPE_SYM_MINUS:
            {
                parser_next_token(parser);
                AstNodeTyped* factor = parse_factor(parser);

                AstNodeUnaryOp* negate_node = make_node(AstNodeUnaryOp, AST_NODE_KIND_UNARY_OP);
                negate_node->operation = ONYX_UNARY_OP_NEGATE;
                negate_node->expr = factor;

                if ((factor->flags & ONYX_AST_FLAG_COMPTIME) != 0) {
                    negate_node->base.flags |= ONYX_AST_FLAG_COMPTIME;
                }

                retval = (AstNodeTyped *) negate_node;
                break;
            }

        case TOKEN_TYPE_SYMBOL:
            {
                OnyxToken* sym_token = expect(parser, TOKEN_TYPE_SYMBOL);
                AstNodeTyped* sym_node = make_node(AstNode, AST_NODE_KIND_SYMBOL);
                sym_node->token = sym_token;

                if (parser->curr_token->type != TOKEN_TYPE_OPEN_PAREN) {
                    retval = sym_node;
                    break;
                }

                // NOTE: Function call
                AstNodeCall* call_node = make_node(AstNodeCall, AST_NODE_KIND_CALL);
                call_node->base.token = expect(parser, TOKEN_TYPE_OPEN_PAREN);
                call_node->callee = (AstNode *) sym_node;

                AstNodeArgument** prev = &call_node->arguments;
                AstNodeArgument* curr = NULL;
                while (parser->curr_token->type != TOKEN_TYPE_CLOSE_PAREN) {
                    curr = make_node(AstNodeArgument, AST_NODE_KIND_ARGUMENT);
                    curr->value = parse_expression(parser);

                    if (curr != NULL && curr->base.kind != AST_NODE_KIND_ERROR) {
                        *prev = curr;
                        prev = (AstNodeArgument **) &curr->base.next;
                    }

                    if (parser->curr_token->type == TOKEN_TYPE_CLOSE_PAREN)
                        break;

                    if (parser->curr_token->type != TOKEN_TYPE_SYM_COMMA) {
                        onyx_message_add(parser->msgs,
                                ONYX_MESSAGE_TYPE_EXPECTED_TOKEN,
                                parser->curr_token->pos,
                                onyx_get_token_type_name(TOKEN_TYPE_SYM_COMMA),
                                onyx_get_token_type_name(parser->curr_token->type));
                        return (AstNodeTyped *) &error_node;
                    }

                    parser_next_token(parser);
                }
                parser_next_token(parser);

                retval = (AstNodeTyped *) call_node;
                break;
            }

        case TOKEN_TYPE_LITERAL_NUMERIC:
            retval = (AstNodeTyped *) parse_numeric_literal(parser);
            break;

        case TOKEN_TYPE_LITERAL_BOOL_TRUE:
            {
                AstNodeNumLit* bool_node = make_node(AstNodeNumLit, AST_NODE_KIND_LITERAL);
                bool_node->base.type = &builtin_types[TYPE_INFO_KIND_BOOL];
                bool_node->base.token = expect(parser, TOKEN_TYPE_LITERAL_BOOL_TRUE);
                bool_node->value.i = 1;
                retval = (AstNodeTyped *) bool_node;
                break;
            }

        case TOKEN_TYPE_LITERAL_BOOL_FALSE:
            {
                AstNodeNumLit* bool_node = make_node(AstNodeNumLit, AST_NODE_KIND_LITERAL);
                bool_node->base.type = &builtin_types[TYPE_INFO_KIND_BOOL];
                bool_node->base.token = expect(parser, TOKEN_TYPE_LITERAL_BOOL_FALSE);
                bool_node->value.i = 0;
                retval = (AstNodeTyped *) bool_node;
                break;
            }

        default:
            onyx_message_add(parser->msgs,
                    ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
                    parser->curr_token->pos,
                    onyx_get_token_type_name(parser->curr_token->type));
            return NULL;
    }

    if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_CAST) {
        parser_next_token(parser);

        AstNodeUnaryOp* cast_node = make_node(AstNodeUnaryOp, AST_NODE_KIND_UNARY_OP);
        cast_node->base.type = parse_type(parser);
        cast_node->operation = ONYX_UNARY_OP_CAST;
        cast_node->expr = retval;
        retval = (AstNodeTyped *) cast_node;
    }

    return retval;
}

static inline i32 get_precedence(OnyxBinaryOp kind) {
    switch (kind) {
        case ONYX_BINARY_OP_EQUAL: return 3;
        case ONYX_BINARY_OP_NOT_EQUAL: return 3;

        case ONYX_BINARY_OP_LESS_EQUAL: return 4;
        case ONYX_BINARY_OP_LESS: return 4;
        case ONYX_BINARY_OP_GREATER_EQUAL: return 4;
        case ONYX_BINARY_OP_GREATER: return 4;

        case ONYX_BINARY_OP_ADD: return 5;
        case ONYX_BINARY_OP_MINUS: return 5;

        case ONYX_BINARY_OP_MULTIPLY: return 6;
        case ONYX_BINARY_OP_DIVIDE: return 6;

        case ONYX_BINARY_OP_MODULUS: return 7;
        default: return -1;
    }
}

static AstNodeTyped* parse_expression(OnyxParser* parser) {
    bh_arr(AstNodeBinOp*) tree_stack = NULL;
    bh_arr_new(global_scratch_allocator, tree_stack, 4);
    bh_arr_set_length(tree_stack, 0);

    AstNodeTyped* left = parse_factor(parser);
    AstNodeTyped* right;
    AstNodeTyped* root = left;

    OnyxBinaryOp bin_op_kind;
    OnyxToken* bin_op_tok;

    while (1) {
        bin_op_kind = -1;
        switch (parser->curr_token->type) {
            case TOKEN_TYPE_SYM_EQUAL_EQUAL:    bin_op_kind = ONYX_BINARY_OP_EQUAL; break;
            case TOKEN_TYPE_SYM_NOT_EQUAL:      bin_op_kind = ONYX_BINARY_OP_NOT_EQUAL; break;
            case TOKEN_TYPE_SYM_LESS_EQUAL:     bin_op_kind = ONYX_BINARY_OP_LESS_EQUAL; break;
            case TOKEN_TYPE_SYM_LESS:           bin_op_kind = ONYX_BINARY_OP_LESS; break;
            case TOKEN_TYPE_SYM_GREATER_EQUAL:  bin_op_kind = ONYX_BINARY_OP_GREATER_EQUAL; break;
            case TOKEN_TYPE_SYM_GREATER:        bin_op_kind = ONYX_BINARY_OP_GREATER; break;

            case TOKEN_TYPE_SYM_PLUS:       bin_op_kind = ONYX_BINARY_OP_ADD; break;
            case TOKEN_TYPE_SYM_MINUS:      bin_op_kind = ONYX_BINARY_OP_MINUS; break;
            case TOKEN_TYPE_SYM_STAR:       bin_op_kind = ONYX_BINARY_OP_MULTIPLY; break;
            case TOKEN_TYPE_SYM_FSLASH:     bin_op_kind = ONYX_BINARY_OP_DIVIDE; break;
            case TOKEN_TYPE_SYM_PERCENT:    bin_op_kind = ONYX_BINARY_OP_MODULUS; break;
            default: goto expression_done;
        }

        if (bin_op_kind != -1) {
            bin_op_tok = parser->curr_token;
            parser_next_token(parser);

            AstNodeBinOp* bin_op = make_node(AstNodeBinOp, AST_NODE_KIND_BIN_OP);
            bin_op->operation = bin_op_kind;
            bin_op->base.token = bin_op_tok;

            while ( !bh_arr_is_empty(tree_stack) &&
                    get_precedence(bh_arr_last(tree_stack)->operation) >= get_precedence(bin_op_kind))
                bh_arr_pop(tree_stack);

            if (bh_arr_is_empty(tree_stack)) {
                // NOTE: new is now the root node
                bin_op->left = root;
                root = (AstNodeTyped *) bin_op;
            } else {
                bin_op->left = bh_arr_last(tree_stack)->right;
                bh_arr_last(tree_stack)->right = (AstNodeTyped *) bin_op;
            }

            bh_arr_push(tree_stack, bin_op);

            right = parse_factor(parser);
            bin_op->right = right;

            if ((left->flags & ONYX_AST_FLAG_COMPTIME) != 0 && (right->flags & ONYX_AST_FLAG_COMPTIME) != 0) {
                bin_op->base.flags |= ONYX_AST_FLAG_COMPTIME;
            }
        }
    }

expression_done:
    return root;
}

static AstNodeIf* parse_if_stmt(OnyxParser* parser) {
    expect(parser, TOKEN_TYPE_KEYWORD_IF);

    AstNodeTyped* cond = parse_expression(parser);
    AstNode* true_block = (AstNode *) parse_block(parser);

    AstNodeIf* if_node = make_node(AstNodeIf, AST_NODE_KIND_IF);
    AstNodeIf* root_if = if_node;

    if_node->cond = cond;
    if (true_block != NULL)
        if_node->true_block = true_block;

    while (parser->curr_token->type == TOKEN_TYPE_KEYWORD_ELSEIF) {
        parser_next_token(parser);
        AstNodeIf* elseif_node = make_node(AstNodeIf, AST_NODE_KIND_IF);

        cond = parse_expression(parser);
        true_block = (AstNode *) parse_block(parser);

        elseif_node->cond = cond;
        if (true_block != NULL)
            elseif_node->true_block = true_block;

        if_node->false_block = (AstNode *) elseif_node;
        if_node = elseif_node;
    }

    if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_ELSE) {
        parser_next_token(parser);

        AstNode* false_block = (AstNode *) parse_block(parser);
        if (false_block != NULL)
            if_node->false_block = false_block;
    }

    return root_if;
}

static AstNodeWhile* parse_while_stmt(OnyxParser* parser) {
    OnyxToken* while_token = expect(parser, TOKEN_TYPE_KEYWORD_WHILE);

    AstNodeTyped* cond = parse_expression(parser);
    AstNodeBlock* body = parse_block(parser);

    AstNodeWhile* while_node = make_node(AstNodeWhile, AST_NODE_KIND_WHILE);
    while_node->base.token = while_token;
    while_node->cond = cond;
    while_node->body = body;

    return while_node;
}

// Returns 1 if the symbol was consumed. Returns 0 otherwise
// ret is set to the statement to insert
static b32 parse_symbol_statement(OnyxParser* parser, AstNode** ret) {
    if (parser->curr_token->type != TOKEN_TYPE_SYMBOL) return 0;
    OnyxToken* symbol = expect(parser, TOKEN_TYPE_SYMBOL);

    switch (parser->curr_token->type) {
        // NOTE: Declaration
        case TOKEN_TYPE_SYM_COLON:
            {
                parser_next_token(parser);
                TypeInfo* type = &builtin_types[TYPE_INFO_KIND_UNKNOWN];

                // NOTE: var: type
                if (parser->curr_token->type == TOKEN_TYPE_SYMBOL) {
                    type = parse_type(parser);
                }

                AstNodeLocal* local = make_node(AstNodeLocal, AST_NODE_KIND_LOCAL);
                local->base.token = symbol;
                local->base.type = type;
                local->base.flags |= ONYX_AST_FLAG_LVAL; // NOTE: DELETE
                *ret = (AstNode *) local;

                if (parser->curr_token->type == TOKEN_TYPE_SYM_EQUALS || parser->curr_token->type == TOKEN_TYPE_SYM_COLON) {
                    if (parser->curr_token->type == TOKEN_TYPE_SYM_COLON) {
                        local->base.flags |= ONYX_AST_FLAG_CONST;
                    }

                    AstNodeAssign* assignment = make_node(AstNodeAssign, AST_NODE_KIND_ASSIGNMENT);
                    local->base.next = (AstNode *) assignment;
                    assignment->base.token = parser->curr_token;
                    parser_next_token(parser);

                    AstNodeTyped* expr = parse_expression(parser);
                    if (expr == NULL) {
                        onyx_token_null_toggle(parser->curr_token);
                        onyx_message_add(parser->msgs,
                                ONYX_MESSAGE_TYPE_EXPECTED_EXPRESSION,
                                assignment->base.token->pos,
                                parser->curr_token->token);
                        onyx_token_null_toggle(parser->curr_token);
                        return 1;
                    }
                    assignment->expr = expr;

                    AstNode* left_symbol = make_node(AstNode, AST_NODE_KIND_SYMBOL);
                    left_symbol->token = symbol;
                    assignment->lval = (AstNodeTyped *) left_symbol;
                }
                return 1;
            }

            // NOTE: Assignment
        case TOKEN_TYPE_SYM_EQUALS:
            {
                AstNodeAssign* assignment = make_node(AstNodeAssign, AST_NODE_KIND_ASSIGNMENT);
                assignment->base.token = parser->curr_token;
                parser_next_token(parser);

                AstNode* lval = make_node(AstNode, AST_NODE_KIND_SYMBOL);
                lval->token = symbol;

                AstNodeTyped* rval = parse_expression(parser);
                assignment->expr = rval;
                assignment->lval = (AstNodeTyped *) lval;
                *ret = (AstNode *) assignment;
                return 1;
            }

        case TOKEN_TYPE_SYM_PLUS_EQUAL:
        case TOKEN_TYPE_SYM_MINUS_EQUAL:
        case TOKEN_TYPE_SYM_STAR_EQUAL:
        case TOKEN_TYPE_SYM_FSLASH_EQUAL:
        case TOKEN_TYPE_SYM_PERCENT_EQUAL:
            {
                OnyxBinaryOp bin_op;
                if      (parser->curr_token->type == TOKEN_TYPE_SYM_PLUS_EQUAL)    bin_op = ONYX_BINARY_OP_ADD;
                else if (parser->curr_token->type == TOKEN_TYPE_SYM_MINUS_EQUAL)   bin_op = ONYX_BINARY_OP_MINUS;
                else if (parser->curr_token->type == TOKEN_TYPE_SYM_STAR_EQUAL)    bin_op = ONYX_BINARY_OP_MULTIPLY;
                else if (parser->curr_token->type == TOKEN_TYPE_SYM_FSLASH_EQUAL)  bin_op = ONYX_BINARY_OP_DIVIDE;
                else if (parser->curr_token->type == TOKEN_TYPE_SYM_PERCENT_EQUAL) bin_op = ONYX_BINARY_OP_MODULUS;

                parser_next_token(parser);

                AstNodeTyped* expr = parse_expression(parser);

                AstNodeBinOp* bin_op_node = make_node(AstNodeBinOp, AST_NODE_KIND_BIN_OP);
                bin_op_node->operation = bin_op;

                AstNode* bin_op_left = make_node(AstNode, AST_NODE_KIND_SYMBOL);
                bin_op_left->token = symbol;
                bin_op_node->left = (AstNodeTyped *) bin_op_left;
                bin_op_node->right = expr;

                AstNodeAssign* assign_node = make_node(AstNodeAssign, AST_NODE_KIND_ASSIGNMENT);

                AstNode* lval = make_node(AstNode, AST_NODE_KIND_SYMBOL);
                lval->token = symbol;
                assign_node->lval = (AstNodeTyped *) lval;
                assign_node->expr = (AstNodeTyped *) bin_op_node;

                *ret = (AstNode *) assign_node;

                return 1;
            }

        default:
            parser_prev_token(parser);
    }

    return 0;
}

static AstNodeReturn* parse_return_statement(OnyxParser* parser) {
    AstNodeReturn* return_node = make_node(AstNodeReturn, AST_NODE_KIND_RETURN);
    return_node->base.token = expect(parser, TOKEN_TYPE_KEYWORD_RETURN);

    AstNodeTyped* expr = NULL;

    if (parser->curr_token->type != TOKEN_TYPE_SYM_SEMICOLON) {
        expr = parse_expression(parser);

        if (expr == NULL || expr == (AstNodeTyped *) &error_node) {
            return (AstNodeReturn *) &error_node;
        } else {
            return_node->expr = expr;
        }
    }

    return return_node;
}

static AstNode* parse_statement(OnyxParser* parser) {
    b32 needs_semicolon = 1;
    AstNode* retval = NULL;

    switch (parser->curr_token->type) {
        case TOKEN_TYPE_KEYWORD_RETURN:
            retval = (AstNode *) parse_return_statement(parser);
            break;

        case TOKEN_TYPE_OPEN_BRACE:
            needs_semicolon = 0;
            retval = (AstNode *) parse_block(parser);
            break;

        case TOKEN_TYPE_SYMBOL:
            if (parse_symbol_statement(parser, &retval)) break;
            // fallthrough

        case TOKEN_TYPE_OPEN_PAREN:
        case TOKEN_TYPE_SYM_PLUS:
        case TOKEN_TYPE_SYM_MINUS:
        case TOKEN_TYPE_SYM_BANG:
        case TOKEN_TYPE_LITERAL_NUMERIC:
        case TOKEN_TYPE_LITERAL_STRING:
            retval = (AstNode *) parse_expression(parser);
            break;

        case TOKEN_TYPE_KEYWORD_IF:
            needs_semicolon = 0;
            retval = (AstNode *) parse_if_stmt(parser);
            break;

        case TOKEN_TYPE_KEYWORD_WHILE:
            needs_semicolon = 0;
            retval = (AstNode *) parse_while_stmt(parser);
            break;

        case TOKEN_TYPE_KEYWORD_BREAK:
            retval = make_node(AstNode, AST_NODE_KIND_BREAK);
            retval->token = expect(parser, TOKEN_TYPE_KEYWORD_BREAK);
            break;

        case TOKEN_TYPE_KEYWORD_CONTINUE:
            retval = make_node(AstNode, AST_NODE_KIND_BREAK);
            retval->token = expect(parser, TOKEN_TYPE_KEYWORD_CONTINUE);
            break;

        default:
            break;
    }

    if (needs_semicolon) {
        if (parser->curr_token->type != TOKEN_TYPE_SYM_SEMICOLON) {
            onyx_message_add(parser->msgs,
                ONYX_MESSAGE_TYPE_EXPECTED_TOKEN,
                parser->curr_token->pos,
                onyx_get_token_type_name(TOKEN_TYPE_SYM_SEMICOLON),
                onyx_get_token_type_name(parser->curr_token->type));

            find_token(parser, TOKEN_TYPE_SYM_SEMICOLON);
        }
        parser_next_token(parser);
    }

    return retval;
}

static AstNodeBlock* parse_block(OnyxParser* parser) {
    AstNodeBlock* block = make_node(AstNodeBlock, AST_NODE_KIND_BLOCK);
    AstNodeScope* scope = make_node(AstNodeScope, AST_NODE_KIND_SCOPE);
    block->scope = scope;

    // --- is for an empty block
    if (parser->curr_token->type == TOKEN_TYPE_SYM_MINUS) {
        expect(parser, TOKEN_TYPE_SYM_MINUS);
        expect(parser, TOKEN_TYPE_SYM_MINUS);
        expect(parser, TOKEN_TYPE_SYM_MINUS);
        return block;
    }

    expect(parser, TOKEN_TYPE_OPEN_BRACE);

    AstNode** next = &block->body;
    AstNode* stmt = NULL;
    while (parser->curr_token->type != TOKEN_TYPE_CLOSE_BRACE) {
        stmt = parse_statement(parser);

        if (stmt != NULL && stmt->kind != AST_NODE_KIND_ERROR) {
            *next = stmt;

            while (stmt->next != NULL) stmt = stmt->next;
            next = &stmt->next;
        }
    }

    expect(parser, TOKEN_TYPE_CLOSE_BRACE);

    return block;
}

static TypeInfo* parse_type(OnyxParser* parser) {
    TypeInfo* type_info = &builtin_types[TYPE_INFO_KIND_UNKNOWN];

    OnyxToken* symbol = expect(parser, TOKEN_TYPE_SYMBOL);
    if (symbol == NULL) return type_info;

    onyx_token_null_toggle(symbol);

    if (!bh_table_has(AstNode*, parser->identifiers, symbol->token)) {
        onyx_message_add(parser->msgs, ONYX_MESSAGE_TYPE_UNKNOWN_TYPE, symbol->pos, symbol->token);
    } else {
        AstNodeTyped* type_info_node = bh_table_get(AstNodeTyped*, parser->identifiers, symbol->token);

        if (type_info_node->kind == AST_NODE_KIND_TYPE) {
            type_info = type_info_node->type;
        }
    }

    onyx_token_null_toggle(symbol);
    return type_info;
}

static AstNodeLocal* parse_function_params(OnyxParser* parser) {
    if (parser->curr_token->type != TOKEN_TYPE_OPEN_PAREN)
        return NULL;

    expect(parser, TOKEN_TYPE_OPEN_PAREN);

    if (parser->curr_token->type == TOKEN_TYPE_CLOSE_PAREN) {
        parser_next_token(parser);
        return NULL;
    }

    AstNodeLocal* first_param = NULL;
    AstNodeLocal* curr_param = NULL;
    AstNodeLocal* trailer = NULL;

    OnyxToken* symbol;
    while (parser->curr_token->type != TOKEN_TYPE_CLOSE_PAREN) {
        if (parser->curr_token->type == TOKEN_TYPE_SYM_COMMA) parser_next_token(parser);

        symbol = expect(parser, TOKEN_TYPE_SYMBOL);
        expect(parser, TOKEN_TYPE_SYM_COLON);

        curr_param = make_node(AstNodeLocal, AST_NODE_KIND_PARAM);
        curr_param->base.token = symbol;
        curr_param->base.flags |= ONYX_AST_FLAG_CONST;
        curr_param->base.type = parse_type(parser);

        if (first_param == NULL) first_param = curr_param;

        curr_param->base.next = NULL;
        if (trailer) trailer->base.next = (AstNode *) curr_param;

        trailer = curr_param;
    }

    parser_next_token(parser); // Skip the )
    return first_param;
}

static AstNodeFunction* parse_function_definition(OnyxParser* parser) {
    expect(parser, TOKEN_TYPE_KEYWORD_PROC);

    AstNodeFunction* func_def = make_node(AstNodeFunction, AST_NODE_KIND_FUNCTION);

    AstNodeLocal* params = parse_function_params(parser);
    func_def->params = params;

    if (parser->curr_token->type == TOKEN_TYPE_RIGHT_ARROW) {
        expect(parser, TOKEN_TYPE_RIGHT_ARROW);

        TypeInfo* return_type = parse_type(parser);
        func_def->base.type = return_type;
    } else {
        func_def->base.type = &builtin_types[TYPE_INFO_KIND_VOID];
    }

    func_def->body = parse_block(parser);

    return func_def;
}

static AstNode* parse_foreign(OnyxParser* parser) {
    expect(parser, TOKEN_TYPE_KEYWORD_FOREIGN);

    AstNodeForeign* foreign = make_node(AstNodeForeign, AST_NODE_KIND_FOREIGN);
    foreign->mod_token = expect(parser, TOKEN_TYPE_LITERAL_STRING);
    foreign->name_token = expect(parser, TOKEN_TYPE_LITERAL_STRING);

    if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_PROC) {
        foreign->import = (AstNode *) parse_function_definition(parser);

    } else {
        TypeInfo* type = parse_type(parser);

        AstNodeGlobal* global = make_node(AstNodeGlobal, AST_NODE_KIND_GLOBAL);
        global->base.type = type;
        global->base.flags |= ONYX_AST_FLAG_LVAL;

        foreign->import = (AstNode *) global;
    }

    return (AstNode *) foreign;
}

static AstNode* parse_top_level_constant_symbol(OnyxParser* parser) {
    if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_PROC) {
        return (AstNode *) parse_function_definition(parser);

    } else if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_STRUCT) {
        // Handle struct case
        assert(0);

    } else if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_FOREIGN) {
        return (AstNode *) parse_foreign(parser);

    } else {
        // Global constant with initial value
        AstNodeGlobal* global = make_node(AstNodeGlobal, AST_NODE_KIND_GLOBAL);
        global->initial_value = parse_expression(parser);
        global->base.type = &builtin_types[TYPE_INFO_KIND_UNKNOWN];
        global->base.flags |= ONYX_AST_FLAG_CONST;
        global->base.flags |= ONYX_AST_FLAG_LVAL;
        global->base.flags |= ONYX_AST_FLAG_COMPTIME;

        return (AstNode *) global;
    }
}

static AstNode* parse_top_level_statement(OnyxParser* parser) {
    switch (parser->curr_token->type) {
        case TOKEN_TYPE_KEYWORD_USE:
            {
                AstNodeUse* use_node = make_node(AstNodeUse, AST_NODE_KIND_USE);
                use_node->base.token = expect(parser, TOKEN_TYPE_KEYWORD_USE);
                use_node->filename = expect(parser, TOKEN_TYPE_LITERAL_STRING);

                return (AstNode *) use_node;
            }

        case TOKEN_TYPE_KEYWORD_EXPORT:
            {
                expect(parser, TOKEN_TYPE_KEYWORD_EXPORT);
                if (parser->curr_token->type != TOKEN_TYPE_SYMBOL) {
                    onyx_message_add(parser->msgs,
                            ONYX_MESSAGE_TYPE_EXPECTED_TOKEN,
                            parser->curr_token->pos,
                            onyx_get_token_type_name(TOKEN_TYPE_SYMBOL),
                            onyx_get_token_type_name(parser->curr_token->type));
                    break;
                }

                AstNode* top_level_decl = parse_top_level_statement(parser);
                top_level_decl->flags |= ONYX_AST_FLAG_EXPORTED;
                return top_level_decl;
            }

        case TOKEN_TYPE_SYMBOL:
            {
                OnyxToken* symbol = parser->curr_token;
                parser_next_token(parser);

                expect(parser, TOKEN_TYPE_SYM_COLON);

                TypeInfo* type = &builtin_types[TYPE_INFO_KIND_UNKNOWN];

                if (parser->curr_token->type == TOKEN_TYPE_SYMBOL) {
                    type = parse_type(parser);
                }

                if (parser->curr_token->type == TOKEN_TYPE_SYM_COLON) {
                    parser_next_token(parser);

                    AstNode* node = parse_top_level_constant_symbol(parser);

                    if (node->kind == AST_NODE_KIND_GLOBAL) {
                        ((AstNodeGlobal *) node)->base.type = type;
                    }

                    if (node->kind == AST_NODE_KIND_FOREIGN) {
                        ((AstNodeForeign *) node)->import->token = symbol;

                    } else {
                        node->token = symbol;
                    }

                    return node;

                } else if (parser->curr_token->type == TOKEN_TYPE_SYM_EQUALS) {
                    parser_next_token(parser);

                    AstNodeGlobal* global = make_node(AstNodeGlobal, AST_NODE_KIND_GLOBAL);
                    global->base.token = symbol;
                    global->base.flags |= ONYX_AST_FLAG_LVAL;
                    global->initial_value = parse_expression(parser);
                    global->base.type = type;

                    return (AstNode *) global;

                } else {
                    onyx_token_null_toggle(parser->curr_token);
                    onyx_message_add(parser->msgs,
                            ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
                            parser->curr_token->pos,
                            parser->curr_token->token);
                    onyx_token_null_toggle(parser->curr_token);
                }

                return &error_node;
            }

        default: break;
    }

    parser_next_token(parser);
    return NULL;
}





const char* onyx_ast_node_kind_string(AstNodeKind kind) {
    return ast_node_names[kind];
}

// NOTE: This returns a void* so I don't need to cast it everytime I use it
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstNodeKind kind) {
    void* node = bh_alloc(alloc, size);

    memset(node, 0, size);
    *(AstNodeKind *) node = kind;

    return node;
}

OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs) {
    OnyxParser parser;

    bh_table_init(bh_heap_allocator(), parser.identifiers, 61);

    TypeInfo* it = &builtin_types[0];
    while (it->kind != 0xffffffff) {
        AstNodeTyped* tmp = onyx_ast_node_new(alloc, sizeof(AstNodeTyped), AST_NODE_KIND_TYPE);
        tmp->type = it;
        bh_table_put(AstNode*, parser.identifiers, (char *)it->name, tmp);
        it++;
    }

    parser.allocator = alloc;
    parser.tokenizer = tokenizer;
    parser.curr_token = tokenizer->tokens;
    parser.prev_token = NULL;
    parser.msgs = msgs;

    return parser;
}

void onyx_parser_free(OnyxParser* parser) {
    bh_table_free(parser->identifiers);
}

AstNode* onyx_parse(OnyxParser *parser) {
    AstNode* program = make_node(AstNode, AST_NODE_KIND_PROGRAM);

    AstNode** prev_stmt = &program->next;
    AstNode* curr_stmt = NULL;
    while (parser->curr_token->type != TOKEN_TYPE_END_STREAM) {
        curr_stmt = parse_top_level_statement(parser);

        // Building a linked list of statements down the "next" chain
        if (curr_stmt != NULL && curr_stmt != &error_node) {
            *prev_stmt = curr_stmt;

            while (curr_stmt->next != NULL) curr_stmt = curr_stmt->next;
            prev_stmt = &curr_stmt->next;
        }
    }

    return program;
}
