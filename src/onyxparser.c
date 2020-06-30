#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxutils.h"

static const char* ast_node_names[] = {
    "ERROR",
    "PROGRAM",

    "FUNCDEF",
    "FOREIGN",
    "BLOCK",
    "SCOPE",
    "LOCAL",
    "SYMBOL",

    "UN_OP",
    "BIN_OP",

    "TYPE",
    "LITERAL",
    "CAST",
    "PARAM",
    "ARGUMENT",
    "CALL",
    "ASSIGN",
    "RETURN",

    "IF",
    "WHILE",

    "ONYX_AST_NODE_KIND_COUNT",
};

struct OnyxTypeInfo builtin_types[] = {
    { ONYX_TYPE_INFO_KIND_UNKNOWN, 0, "unknown" },
    { ONYX_TYPE_INFO_KIND_VOID, 0, "void", 0, 0, 0, 0, 1 },

    { ONYX_TYPE_INFO_KIND_BOOL, 1, "bool", 0, 1, 0, 1, 1 },

    { ONYX_TYPE_INFO_KIND_UINT32, 4, "u32", 1, 1, 0, 0, 1 },
    { ONYX_TYPE_INFO_KIND_UINT64, 8, "u64", 1, 1, 0, 0, 1 },

    { ONYX_TYPE_INFO_KIND_INT32, 4, "i32", 1, 0, 0, 0, 1 },
    { ONYX_TYPE_INFO_KIND_INT64, 8, "i64", 1, 0, 0, 0, 1 },

    { ONYX_TYPE_INFO_KIND_FLOAT32, 4, "f32", 0, 0, 1, 0, 1 },
    { ONYX_TYPE_INFO_KIND_FLOAT64, 8, "f64", 0, 0, 1, 0, 1},
    { ONYX_TYPE_INFO_KIND_SOFT_FLOAT, 8, "sf64", 0, 0, 1, 0, 1 },

    { 0xffffffff } // Sentinel
};

static OnyxAstNode error_node = { { ONYX_AST_NODE_KIND_ERROR, 0, NULL, &builtin_types[0], 0, NULL, NULL, NULL } };

// NOTE: Forward declarations
static void parser_next_token(OnyxParser* parser);
static void parser_prev_token(OnyxParser* parser);
static b32 is_terminating_token(OnyxTokenType token_type);
static OnyxToken* expect(OnyxParser* parser, OnyxTokenType token_type);
static OnyxAstNodeScope* enter_scope(OnyxParser* parser);
static OnyxAstNodeScope* leave_scope(OnyxParser* parser);
static void insert_identifier(OnyxParser* parser, OnyxAstNode* ident, b32 is_local);
static void remove_identifier(OnyxParser* parser, OnyxAstNode* ident);
static OnyxAstNodeNumLit* parse_numeric_literal(OnyxParser* parser);
static OnyxAstNode* parse_factor(OnyxParser* parser);
static OnyxAstNode* parse_bin_op(OnyxParser* parser, OnyxAstNode* left);
static OnyxAstNode* parse_expression(OnyxParser* parser);
static OnyxAstNodeIf* parse_if_stmt(OnyxParser* parser);
static OnyxAstNodeWhile* parse_while_stmt(OnyxParser* parser);
static b32 parse_symbol_statement(OnyxParser* parser, OnyxAstNode** ret);
static OnyxAstNode* parse_return_statement(OnyxParser* parser);
static OnyxAstNodeBlock* parse_block(OnyxParser* parser);
static OnyxAstNode* parse_statement(OnyxParser* parser);
static OnyxTypeInfo* parse_type(OnyxParser* parser);
static OnyxAstNodeParam* parse_function_params(OnyxParser* parser);
static OnyxAstNodeFuncDef* parse_function_definition(OnyxParser* parser);
static OnyxAstNode* parse_top_level_statement(OnyxParser* parser);

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

static b32 is_terminating_token(OnyxTokenType token_type) {
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

static void find_token(OnyxParser* parser, OnyxTokenType token_type) {
    while (parser->curr_token->type != token_type && !is_terminating_token(parser->curr_token->type)) {
        parser_next_token(parser);
    }
}

// Advances to next token no matter what
static OnyxToken* expect(OnyxParser* parser, OnyxTokenType token_type) {
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

static OnyxAstNodeNumLit* parse_numeric_literal(OnyxParser* parser) {
    OnyxAstNodeNumLit* lit_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_LITERAL);
    lit_node->token = expect(parser, TOKEN_TYPE_LITERAL_NUMERIC);
    lit_node->flags |= ONYX_AST_FLAG_COMPTIME;
    lit_node->value.l = 0ll;

    onyx_token_null_toggle(*lit_node->token);

    OnyxTypeInfo* type;
    char* tok = lit_node->token->token;

    // NOTE: charset_contains() behaves more like string_contains()
    // so I'm using it in this case
    if (charset_contains(tok, '.')) {
        if (tok[lit_node->token->length - 1] == 'f') {
            type = &builtin_types[ONYX_TYPE_INFO_KIND_FLOAT32];
            lit_node->value.f = strtof(tok, NULL);
        } else {
            type = &builtin_types[ONYX_TYPE_INFO_KIND_FLOAT64];
            lit_node->value.d = strtod(tok, NULL);
        }
    } else {
        i64 value = strtoll(tok, NULL, 0);
        if (bh_abs(value) < ((u64) 1 << 32)) {
            type = &builtin_types[ONYX_TYPE_INFO_KIND_INT32];
        } else {
            type = &builtin_types[ONYX_TYPE_INFO_KIND_INT64];
        }

        lit_node->value.l = value;
    }

    lit_node->type = type;
    onyx_token_null_toggle(*lit_node->token);
    return lit_node;
}

static OnyxAstNode* parse_factor(OnyxParser* parser) {
    OnyxAstNode* retval = NULL;

    switch (parser->curr_token->type) {
        case TOKEN_TYPE_OPEN_PAREN:
            {
                parser_next_token(parser);
                OnyxAstNode* expr = parse_expression(parser);
                expect(parser, TOKEN_TYPE_CLOSE_PAREN);
                retval = expr;
                break;
            }

        case TOKEN_TYPE_SYM_MINUS:
            {
                parser_next_token(parser);
                OnyxAstNode* factor = parse_factor(parser);

                OnyxAstNodeUnaryOp* negate_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_UNARY_OP);
                negate_node->operation = ONYX_UNARY_OP_NEGATE;
                negate_node->left = factor;
                negate_node->type = factor->type;

                if ((factor->flags & ONYX_AST_FLAG_COMPTIME) != 0) {
                    negate_node->flags |= ONYX_AST_FLAG_COMPTIME;
                }

                retval = (OnyxAstNode *) negate_node;
                break;
            }

        case TOKEN_TYPE_SYMBOL:
            {
                OnyxToken* sym_token = expect(parser, TOKEN_TYPE_SYMBOL);
                OnyxAstNode* sym_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_SYMBOL);
                sym_node->token = sym_token;

                if (parser->curr_token->type != TOKEN_TYPE_OPEN_PAREN) {
                    retval = sym_node;
                    break;
                }

                // NOTE: Function call
                OnyxAstNodeCall* call_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_CALL);
                call_node->token = expect(parser, TOKEN_TYPE_OPEN_PAREN);
                call_node->callee = sym_node;
                // NOTE: Return type is stored on function definition's type
                // This may have to change if we want multiple returns
                call_node->type = sym_node->type;

                OnyxAstNode** prev = &call_node->arguments;
                OnyxAstNode* curr = NULL;
                while (parser->curr_token->type != TOKEN_TYPE_CLOSE_PAREN) {
                    curr = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_ARGUMENT);
                    curr->left = parse_expression(parser);
                    curr->type = curr->left->type;

                    if (curr != NULL && curr->kind != ONYX_AST_NODE_KIND_ERROR) {
                        *prev = curr;
                        prev = &curr->next;
                    }

                    if (parser->curr_token->type == TOKEN_TYPE_CLOSE_PAREN)
                        break;

                    if (parser->curr_token->type != TOKEN_TYPE_SYM_COMMA) {
                        onyx_message_add(parser->msgs,
                                ONYX_MESSAGE_TYPE_EXPECTED_TOKEN,
                                parser->curr_token->pos,
                                onyx_get_token_type_name(TOKEN_TYPE_SYM_COMMA),
                                onyx_get_token_type_name(parser->curr_token->type));
                        return &error_node;
                    }

                    parser_next_token(parser);
                }
                parser_next_token(parser);

                retval = (OnyxAstNode *) call_node;
                break;
            }

        case TOKEN_TYPE_LITERAL_NUMERIC:
            retval = (OnyxAstNode *) parse_numeric_literal(parser);
            break;

        case TOKEN_TYPE_LITERAL_BOOL_TRUE:
            {
                OnyxAstNodeNumLit* bool_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_LITERAL);
                bool_node->type = &builtin_types[ONYX_TYPE_INFO_KIND_BOOL];
                bool_node->token = expect(parser, TOKEN_TYPE_LITERAL_BOOL_TRUE);
                bool_node->value.i = 1;
                retval = (OnyxAstNode *) bool_node;
                break;
            }

        case TOKEN_TYPE_LITERAL_BOOL_FALSE:
            {
                OnyxAstNodeNumLit* bool_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_LITERAL);
                bool_node->type = &builtin_types[ONYX_TYPE_INFO_KIND_BOOL];
                bool_node->token = expect(parser, TOKEN_TYPE_LITERAL_BOOL_FALSE);
                bool_node->value.i = 0;
                retval = (OnyxAstNode *) bool_node;
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
        OnyxAstNodeUnaryOp* cast_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_UNARY_OP);
        cast_node->operation = ONYX_UNARY_OP_CAST;
        cast_node->type = parse_type(parser);
        cast_node->left = retval;
        retval = (OnyxAstNode *) cast_node;
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

static OnyxAstNode* parse_expression(OnyxParser* parser) {
    bh_arr(OnyxAstNodeBinOp*) tree_stack = NULL;
    bh_arr_new(global_scratch_allocator, tree_stack, 4);
    bh_arr_set_length(tree_stack, 0);

    OnyxAstNode* left = parse_factor(parser);
    OnyxAstNode* right;
    OnyxAstNode* root = left;

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

            OnyxAstNodeBinOp* bin_op = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_BIN_OP);
            bin_op->operation = bin_op_kind;
            bin_op->token = bin_op_tok;

            while ( !bh_arr_is_empty(tree_stack) &&
                    get_precedence(bh_arr_last(tree_stack)->operation) >= get_precedence(bin_op_kind))
                bh_arr_pop(tree_stack);

            if (bh_arr_is_empty(tree_stack)) {
                // NOTE: new is now the root node
                bin_op->left = root;
                root = (OnyxAstNode *) bin_op;
            } else {
                bin_op->left = bh_arr_last(tree_stack)->right;
                bh_arr_last(tree_stack)->right = (OnyxAstNode *) bin_op;
            }

            bh_arr_push(tree_stack, bin_op);

            right = parse_factor(parser);
            bin_op->right = right;
            bin_op->type = right->type;

            if ((left->flags & ONYX_AST_FLAG_COMPTIME) != 0 && (right->flags & ONYX_AST_FLAG_COMPTIME) != 0) {
                bin_op->flags |= ONYX_AST_FLAG_COMPTIME;
            }
        }
    }

expression_done:
    return root;
}

static OnyxAstNodeIf* parse_if_stmt(OnyxParser* parser) {
    expect(parser, TOKEN_TYPE_KEYWORD_IF);

    OnyxAstNode* cond = parse_expression(parser);
    OnyxAstNode* true_block = (OnyxAstNode *) parse_block(parser);

    OnyxAstNodeIf* if_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_IF);
    OnyxAstNodeIf* root_if = if_node;

    if_node->cond = cond;
    if (true_block != NULL)
        if_node->true_block = true_block;

    while (parser->curr_token->type == TOKEN_TYPE_KEYWORD_ELSEIF) {
        parser_next_token(parser);
        OnyxAstNodeIf* elseif_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_IF);

        cond = parse_expression(parser);
        true_block = (OnyxAstNode *) parse_block(parser);

        elseif_node->cond = cond;
        if (true_block != NULL)
            elseif_node->true_block = true_block;

        if_node->false_block = (OnyxAstNode *) elseif_node;
        if_node = elseif_node;
    }

    if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_ELSE) {
        parser_next_token(parser);

        OnyxAstNode* false_block = (OnyxAstNode *) parse_block(parser);
        if (false_block != NULL)
            if_node->false_block = false_block;
    }

    return root_if;
}

static OnyxAstNodeWhile* parse_while_stmt(OnyxParser* parser) {
    OnyxToken* while_token = expect(parser, TOKEN_TYPE_KEYWORD_WHILE);

    OnyxAstNode* cond = parse_expression(parser);
    OnyxAstNodeBlock* body = parse_block(parser);

    OnyxAstNodeWhile* while_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_WHILE);
    while_node->token = while_token;
    while_node->cond = cond;
    while_node->body = body;

    return while_node;
}

// Returns 1 if the symbol was consumed. Returns 0 otherwise
// ret is set to the statement to insert
static b32 parse_symbol_statement(OnyxParser* parser, OnyxAstNode** ret) {
    if (parser->curr_token->type != TOKEN_TYPE_SYMBOL) return 0;
    OnyxToken* symbol = expect(parser, TOKEN_TYPE_SYMBOL);

    switch (parser->curr_token->type) {
        // NOTE: Declaration
        case TOKEN_TYPE_SYM_COLON:
            {
                parser_next_token(parser);
                OnyxTypeInfo* type = &builtin_types[ONYX_TYPE_INFO_KIND_UNKNOWN];

                // NOTE: var: type
                if (parser->curr_token->type == TOKEN_TYPE_SYMBOL) {
                    type = parse_type(parser);
                }

                OnyxAstNodeLocal* local = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_LOCAL);
                local->token = symbol;
                local->type = type;
                local->flags |= ONYX_AST_FLAG_LVAL; // NOTE: DELETE
                *ret = (OnyxAstNode *) local;

                if (parser->curr_token->type == TOKEN_TYPE_SYM_EQUALS || parser->curr_token->type == TOKEN_TYPE_SYM_COLON) {
                    if (parser->curr_token->type == TOKEN_TYPE_SYM_COLON) {
                        local->flags |= ONYX_AST_FLAG_CONST;
                    }

                    OnyxAstNode* assignment = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_ASSIGNMENT);
                    local->next = assignment;
                    assignment->token = parser->curr_token;
                    parser_next_token(parser);

                    OnyxAstNode* expr = parse_expression(parser);
                    if (expr == NULL) {
                        onyx_token_null_toggle(*parser->curr_token);
                        onyx_message_add(parser->msgs,
                                ONYX_MESSAGE_TYPE_EXPECTED_EXPRESSION,
                                assignment->token->pos,
                                parser->curr_token->token);
                        onyx_token_null_toggle(*parser->curr_token);
                        return 1;
                    }
                    assignment->right = expr;

                    OnyxAstNode* left_symbol = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_SYMBOL);
                    left_symbol->token = symbol;
                    assignment->left = left_symbol;
                }
                return 1;
            }

            // NOTE: Assignment
        case TOKEN_TYPE_SYM_EQUALS:
            {
                OnyxAstNode* assignment = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_ASSIGNMENT);
                assignment->token = parser->curr_token;
                parser_next_token(parser);

                OnyxAstNode* lval = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_SYMBOL);
                lval->token = symbol;

                OnyxAstNode* rval = parse_expression(parser);
                assignment->right = rval;
                assignment->left = lval;
                *ret = assignment;
                return 1;
            }

        default:
            parser_prev_token(parser);
    }

    return 0;
}

static OnyxAstNode* parse_return_statement(OnyxParser* parser) {
    OnyxAstNode* return_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_RETURN);
    return_node->token = expect(parser, TOKEN_TYPE_KEYWORD_RETURN);

    OnyxAstNode* expr = NULL;

    if (parser->curr_token->type != TOKEN_TYPE_SYM_SEMICOLON) {
        expr = parse_expression(parser);

        if (expr == NULL || expr == &error_node) {
            return &error_node;
        } else {
            return_node->left = expr;
        }
    }

    return return_node;
}

static OnyxAstNode* parse_statement(OnyxParser* parser) {
    b32 needs_semicolon = 1;
    OnyxAstNode* retval = NULL;

    switch (parser->curr_token->type) {
        case TOKEN_TYPE_KEYWORD_RETURN:
            retval = parse_return_statement(parser);
            break;

        case TOKEN_TYPE_OPEN_BRACE:
            needs_semicolon = 0;
            retval = (OnyxAstNode *) parse_block(parser);
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
            retval = parse_expression(parser);
            break;

        case TOKEN_TYPE_KEYWORD_IF:
            needs_semicolon = 0;
            retval = (OnyxAstNode *) parse_if_stmt(parser);
            break;

        case TOKEN_TYPE_KEYWORD_WHILE:
            needs_semicolon = 0;
            retval = (OnyxAstNode *) parse_while_stmt(parser);
            break;

        case TOKEN_TYPE_KEYWORD_BREAK:
            retval = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_BREAK);
            retval->token = expect(parser, TOKEN_TYPE_KEYWORD_BREAK);
            break;

        case TOKEN_TYPE_KEYWORD_CONTINUE:
            retval = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_CONTINUE);
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

static OnyxAstNodeBlock* parse_block(OnyxParser* parser) {
    OnyxAstNodeBlock* block = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_BLOCK);
    OnyxAstNodeScope* scope = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_SCOPE);
    block->scope = scope;

    // --- is for an empty block
    if (parser->curr_token->type == TOKEN_TYPE_SYM_MINUS) {
        expect(parser, TOKEN_TYPE_SYM_MINUS);
        expect(parser, TOKEN_TYPE_SYM_MINUS);
        expect(parser, TOKEN_TYPE_SYM_MINUS);
        return block;
    }

    expect(parser, TOKEN_TYPE_OPEN_BRACE);

    OnyxAstNode** next = &block->body;
    OnyxAstNode* stmt = NULL;
    while (parser->curr_token->type != TOKEN_TYPE_CLOSE_BRACE) {
        stmt = parse_statement(parser);

        if (stmt != NULL && stmt->kind != ONYX_AST_NODE_KIND_ERROR) {
            *next = stmt;

            while (stmt->next != NULL) stmt = stmt->next;
            next = &stmt->next;
        }
    }

    expect(parser, TOKEN_TYPE_CLOSE_BRACE);

    return block;
}

static OnyxTypeInfo* parse_type(OnyxParser* parser) {
    OnyxTypeInfo* type_info = &builtin_types[ONYX_TYPE_INFO_KIND_UNKNOWN];

    OnyxToken* symbol = expect(parser, TOKEN_TYPE_SYMBOL);
    if (symbol == NULL) return type_info;

    onyx_token_null_toggle(*symbol);

    if (!bh_table_has(OnyxAstNode*, parser->identifiers, symbol->token)) {
        onyx_message_add(parser->msgs, ONYX_MESSAGE_TYPE_UNKNOWN_TYPE, symbol->pos, symbol->token);
    } else {
        OnyxAstNode* type_info_node = bh_table_get(OnyxAstNode*, parser->identifiers, symbol->token);

        if (type_info_node->kind == ONYX_AST_NODE_KIND_TYPE) {
            type_info = type_info_node->type;
        }
    }

    onyx_token_null_toggle(*symbol);
    return type_info;
}

static OnyxAstNodeParam* parse_function_params(OnyxParser* parser) {
    if (parser->curr_token->type != TOKEN_TYPE_OPEN_PAREN)
        return NULL;

    expect(parser, TOKEN_TYPE_OPEN_PAREN);

    if (parser->curr_token->type == TOKEN_TYPE_CLOSE_PAREN) {
        parser_next_token(parser);
        return NULL;
    }

    OnyxAstNodeParam* first_param = NULL;
    OnyxAstNodeParam* curr_param = NULL;
    OnyxAstNodeParam* trailer = NULL;

    OnyxToken* symbol;
    while (parser->curr_token->type != TOKEN_TYPE_CLOSE_PAREN) {
        if (parser->curr_token->type == TOKEN_TYPE_SYM_COMMA) parser_next_token(parser);

        symbol = expect(parser, TOKEN_TYPE_SYMBOL);

        curr_param = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_PARAM);
        curr_param->token = symbol;
        curr_param->type = parse_type(parser);
        curr_param->flags |= ONYX_AST_FLAG_CONST;

        if (first_param == NULL) first_param = curr_param;

        curr_param->next = NULL;
        if (trailer) trailer->next = curr_param;

        trailer = curr_param;
    }

    parser_next_token(parser); // Skip the )
    return first_param;
}

static OnyxAstNodeFuncDef* parse_function_definition(OnyxParser* parser) {
    expect(parser, TOKEN_TYPE_KEYWORD_PROC);

    OnyxAstNodeFuncDef* func_def = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_FUNCDEF);

    OnyxAstNodeParam* params = parse_function_params(parser);
    func_def->params = params;

    if (parser->curr_token->type == TOKEN_TYPE_RIGHT_ARROW) {
        expect(parser, TOKEN_TYPE_RIGHT_ARROW);

        OnyxTypeInfo* return_type = parse_type(parser);
        func_def->return_type = return_type;
    } else {
        func_def->return_type = &builtin_types[ONYX_TYPE_INFO_KIND_VOID];
    }

    func_def->body = parse_block(parser);

    return func_def;
}

static OnyxAstNode* parse_top_level_symbol(OnyxParser* parser) {
    if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_PROC) {
        OnyxAstNodeFuncDef* func_def = parse_function_definition(parser);
        return (OnyxAstNode *) func_def;

    } else if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_STRUCT) {
        // Handle struct case
        assert(0);
    } else if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_FOREIGN) {
        parser_next_token(parser);

        OnyxAstNodeForeign* foreign = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_FOREIGN);
        foreign->mod_token = expect(parser, TOKEN_TYPE_LITERAL_STRING);
        foreign->name_token = expect(parser, TOKEN_TYPE_LITERAL_STRING);
        foreign->import = parse_top_level_symbol(parser);

        return (OnyxAstNode *) foreign;
    } else {
        onyx_message_add(parser->msgs,
                ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
                parser->curr_token->pos,
                onyx_get_token_type_name(parser->curr_token->type));
        return &error_node;
    }
}

static OnyxAstNode* parse_top_level_statement(OnyxParser* parser) {
    switch (parser->curr_token->type) {
        case TOKEN_TYPE_KEYWORD_USE:
            assert(0);
            break;

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

                OnyxAstNode* top_level_decl = parse_top_level_statement(parser);
                top_level_decl->flags |= ONYX_AST_FLAG_EXPORTED;
                return top_level_decl;
            }

        case TOKEN_TYPE_SYMBOL:
            {
                OnyxToken* symbol = parser->curr_token;
                parser_next_token(parser);

                expect(parser, TOKEN_TYPE_SYM_COLON);
                expect(parser, TOKEN_TYPE_SYM_COLON);

                OnyxAstNode* node = parse_top_level_symbol(parser);
                if (node->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
                    node->token = symbol;
                }

                if (node->kind == ONYX_AST_NODE_KIND_FOREIGN) {
                    OnyxAstNodeForeign* foreign = &node->as_foreign;

                    foreign->import->token = symbol;
                }

                return node;
            }

        default: break;
    }

    parser_next_token(parser);
    return NULL;
}





const char* onyx_ast_node_kind_string(OnyxAstNodeKind kind) {
    return ast_node_names[kind];
}

// NOTE: This returns a void* so I don't need to cast it everytime I use it
void* onyx_ast_node_new(bh_allocator alloc, OnyxAstNodeKind kind) {
    OnyxAstNode* node =  bh_alloc_item(alloc, OnyxAstNode);
    node->kind = kind;
    node->flags = 0;
    node->token = NULL;
    node->type = NULL;
    node->data = 0;
    node->next = NULL;
    node->left = NULL;
    node->right = NULL;

    return node;
}

OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs) {
    OnyxParser parser;

    bh_table_init(bh_heap_allocator(), parser.identifiers, 61);

    OnyxTypeInfo* it = &builtin_types[0];
    while (it->kind != 0xffffffff) {
        OnyxAstNode* tmp = onyx_ast_node_new(alloc, ONYX_AST_NODE_KIND_TYPE);
        tmp->type = it;
        bh_table_put(OnyxAstNode*, parser.identifiers, (char *)it->name, tmp);
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

OnyxAstNodeFile* onyx_parse(OnyxParser *parser) {
    OnyxAstNodeFile* program = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_PROGRAM);

    OnyxAstNode** prev_stmt = &program->contents;
    OnyxAstNode* curr_stmt = NULL;
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
