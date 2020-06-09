
#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"

static const char* ast_node_names[] = {
	"ERROR",
	"PROGRAM",

	"FUNCDEF",
	"BLOCK",
	"SCOPE",
	"LOCAL",

	"ADD",
	"MINUS",
	"MULTIPLY",
	"DIVIDE",
	"MODULUS",
	"NEGATE",

	"TYPE",
	"LITERAL",
	"CAST",
	"PARAM",
	"CALL",
	"ASSIGN",
	"RETURN",

	"EQUAL",
	"NOT_EQUAL",
	"GREATER",
	"GREATER_EQUAL",
	"LESS",
	"LESS_EQUAL",
	"NOT",

	"IF",
	"LOOP",

	"ONYX_AST_NODE_KIND_COUNT",
};

struct OnyxTypeInfo builtin_types[] = {
	{ ONYX_TYPE_INFO_KIND_UNKNOWN, 0, "unknown" },
	{ ONYX_TYPE_INFO_KIND_VOID, 0, "void", 0, 0, 0, 0, 1 },

	{ ONYX_TYPE_INFO_KIND_BOOL, 1, "bool", 0, 0, 0, 1, 1 },

	{ ONYX_TYPE_INFO_KIND_UINT32, 4, "u32", 1, 1, 0, 0, 1 },
	{ ONYX_TYPE_INFO_KIND_UINT64, 8, "u64", 1, 1, 0, 0, 1 },

	{ ONYX_TYPE_INFO_KIND_INT32, 4, "i32", 1, 0, 0, 0, 1 },
	{ ONYX_TYPE_INFO_KIND_INT64, 8, "i64", 1, 0, 0, 0, 1 },

	{ ONYX_TYPE_INFO_KIND_FLOAT32, 4, "f32", 0, 0, 1, 0, 1 },
	{ ONYX_TYPE_INFO_KIND_FLOAT64, 8, "f64", 0, 0, 1, 0, 1},
	{ ONYX_TYPE_INFO_KIND_SOFT_FLOAT, 8, "sf64", 0, 0, 1, 0, 1 },

	{ 0xffffffff } // Sentinel
};

static OnyxAstNode error_node = { { ONYX_AST_NODE_KIND_ERROR, 0, NULL, &builtin_types[0], NULL, NULL, NULL } };

// NOTE: Forward declarations
static void parser_next_token(OnyxParser* parser);
static void parser_prev_token(OnyxParser* parser);
static b32 is_terminating_token(OnyxTokenType token_type);
static OnyxToken* expect(OnyxParser* parser, OnyxTokenType token_type);
static OnyxAstNodeScope* enter_scope(OnyxParser* parser);
static OnyxAstNodeScope* leave_scope(OnyxParser* parser);
static void insert_identifier(OnyxParser* parser, OnyxAstNode* ident, b32 is_local);
static void remove_identifier(OnyxParser* parser, OnyxAstNode* ident);
static OnyxAstNode* parse_factor(OnyxParser* parser);
static OnyxAstNode* parse_bin_op(OnyxParser* parser, OnyxAstNode* left);
static OnyxAstNode* parse_expression(OnyxParser* parser);
static OnyxAstNode* parse_if_stmt(OnyxParser* parser);
static b32 parse_symbol_statement(OnyxParser* parser, OnyxAstNode** ret);
static OnyxAstNode* parse_return_statement(OnyxParser* parser);
static OnyxAstNodeBlock* parse_block(OnyxParser* parser, b32 belongs_to_function);
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
	parser->prev_token--;
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

static OnyxAstNodeScope* enter_scope(OnyxParser* parser) {
	OnyxAstNodeScope* scope = (OnyxAstNodeScope*) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_SCOPE);
	scope->prev_scope = parser->curr_scope;
	parser->curr_scope = scope;
	return scope;
}

static OnyxAstNodeScope* leave_scope(OnyxParser* parser) {
	// NOTE: Can't leave a scope if there is no scope
	assert(parser->curr_scope != NULL);

	for (OnyxAstNodeLocal *walker = parser->curr_scope->last_local; walker != NULL; walker = walker->prev_local) {
		remove_identifier(parser, (OnyxAstNode *) walker);
	}

	parser->curr_scope = parser->curr_scope->prev_scope;
	return parser->curr_scope;
}

static OnyxAstNode* lookup_identifier(OnyxParser* parser, OnyxToken* token) {
	OnyxAstNode* ident = NULL;

	onyx_token_null_toggle(*token);
	if (bh_hash_has(OnyxAstNode*, parser->identifiers, token->token)) {
		ident = bh_hash_get(OnyxAstNode*, parser->identifiers, token->token);
	}
	onyx_token_null_toggle(*token);

	return ident;
}

static void insert_identifier(OnyxParser* parser, OnyxAstNode* ident, b32 is_local) {
	OnyxAstNodeLocal* local = (OnyxAstNodeLocal *) ident;
	if (is_local) {
		OnyxAstNodeScope* scope = parser->curr_scope;
		local->prev_local = scope->last_local;
		scope->last_local = local;
	}

	onyx_token_null_toggle(*local->token);
	if (bh_hash_has(OnyxAstNode*, parser->identifiers, local->token->token)) {
		local->shadowed = bh_hash_get(OnyxAstNode*, parser->identifiers, local->token->token);
	}

	bh_hash_put(OnyxAstNodeLocal*, parser->identifiers, local->token->token, local);
	onyx_token_null_toggle(*local->token);
}

static void remove_identifier(OnyxParser* parser, OnyxAstNode* ident) {
	OnyxAstNodeLocal* local = (OnyxAstNodeLocal *) ident;

	onyx_token_null_toggle(*local->token);
	if (local->shadowed) {
		bh_hash_put(OnyxAstNode*, parser->identifiers, local->token->token, local->shadowed);
	} else {
		bh_hash_delete(OnyxAstNode*, parser->identifiers, local->token->token);
	}
	onyx_token_null_toggle(*local->token);
}

static OnyxAstNode* parse_factor(OnyxParser* parser) {
	switch (parser->curr_token->type) {
		case TOKEN_TYPE_OPEN_PAREN:
			{
				parser_next_token(parser);
				OnyxAstNode* expr = parse_expression(parser);
				expect(parser, TOKEN_TYPE_CLOSE_PAREN);
				return expr;
			}

		case TOKEN_TYPE_SYMBOL:
			{
				OnyxToken* sym_token = expect(parser, TOKEN_TYPE_SYMBOL);
				OnyxAstNode* sym_node = lookup_identifier(parser, sym_token);
				if (sym_node == NULL) {
					onyx_token_null_toggle(*sym_token);
					onyx_message_add(parser->msgs,
							ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
							sym_token->pos, sym_token->token);
					onyx_token_null_toggle(*sym_token);
				}

				// TODO: Handle calling a function
				return sym_node;
			}

		case TOKEN_TYPE_LITERAL_NUMERIC:
			{
				OnyxAstNode* lit_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_LITERAL);
				lit_node->type = &builtin_types[ONYX_TYPE_INFO_KIND_INT64];
				lit_node->token = expect(parser, TOKEN_TYPE_LITERAL_NUMERIC);
				lit_node->flags |= ONYX_AST_FLAG_COMPTIME;
				return lit_node;
			}

		default:
			onyx_message_add(parser->msgs,
					ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
					parser->curr_token->pos,
					onyx_get_token_type_name(parser->curr_token->type));
	}

	return NULL;
}

static OnyxAstNode* parse_bin_op(OnyxParser* parser, OnyxAstNode* left) {
	OnyxAstNodeKind kind = -1;

	switch (parser->curr_token->type) {
	case TOKEN_TYPE_SYM_PLUS:		kind = ONYX_AST_NODE_KIND_ADD; break;
	case TOKEN_TYPE_SYM_MINUS:		kind = ONYX_AST_NODE_KIND_MINUS; break;
	case TOKEN_TYPE_SYM_STAR:		kind = ONYX_AST_NODE_KIND_MULTIPLY; break;
	case TOKEN_TYPE_SYM_FSLASH:		kind = ONYX_AST_NODE_KIND_DIVIDE; break;
	case TOKEN_TYPE_SYM_PERCENT:	kind = ONYX_AST_NODE_KIND_MODULUS; break;
	}

	if (kind != -1) {
		OnyxToken* bin_op_tok = parser->curr_token;
		parser_next_token(parser);

		OnyxAstNode* right = parse_factor(parser);

		// NOTE: Typechecking like this will not be here for long but
		// want to start working on it early
		if (left->type != right->type) {
			onyx_message_add(parser->msgs,
				ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
				bin_op_tok->pos,
				left->type->name, right->type->name);
			return &error_node;
		}

		OnyxAstNode* bin_op = onyx_ast_node_new(parser->allocator, kind);
		bin_op->left = left;
		bin_op->right = right;
		bin_op->type = left->type;
		return bin_op;
	}

	return &error_node;
}

static OnyxAstNode* parse_expression(OnyxParser* parser) {
	OnyxAstNode* left = parse_factor(parser);

	switch (parser->curr_token->type) {
		case TOKEN_TYPE_SYM_PLUS:
		case TOKEN_TYPE_SYM_MINUS:
		case TOKEN_TYPE_SYM_STAR:
		case TOKEN_TYPE_SYM_FSLASH:
		case TOKEN_TYPE_SYM_PERCENT:
			{
				OnyxAstNode* expr = parse_bin_op(parser, left);
				return expr;
			}
		case TOKEN_TYPE_KEYWORD_CAST:
			{
				parser_next_token(parser);
				OnyxTypeInfo* type = parse_type(parser);

				// NOTE: This may be premature in the parser but a cast
				// node doesn't need to exist if the source and target
				// types are the same
				if (type == left->type) return left;

				OnyxAstNode* cast_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_CAST);
				cast_node->type = type;
				cast_node->left = left;

				return cast_node;
			}
	}

	return left;
}

static OnyxAstNode* parse_if_stmt(OnyxParser* parser) {
	return &error_node;
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
				u32 flags = ONYX_AST_FLAG_LVAL;

				// NOTE: var: const ...
				if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_CONST) {
					parser_next_token(parser);
					flags |= ONYX_AST_FLAG_CONST;
				}

				// NOTE: var: type
				if (parser->curr_token->type == TOKEN_TYPE_SYMBOL) {
					type = parse_type(parser);
				}

				OnyxAstNodeLocal* local = (OnyxAstNodeLocal*) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_LOCAL);
				local->token = symbol;
				local->type = type;
				local->flags |= flags;

				insert_identifier(parser, (OnyxAstNode *) local, 1);

				if (parser->curr_token->type == TOKEN_TYPE_SYM_EQUALS) {
					OnyxAstNode* assignment = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_ASSIGNMENT);
					assignment->token = parser->curr_token;
					parser_next_token(parser);

					OnyxAstNode* expr = parse_expression(parser);
					assignment->right = expr;
					assignment->left = (OnyxAstNode*) local;

					if (local->type->is_known && local->type != expr->type) {
						onyx_message_add(parser->msgs,
								ONYX_MESSAGE_TYPE_ASSIGNMENT_TYPE_MISMATCH,
								assignment->token->pos,
								local->type->name, expr->type->name);
						return 1;
					} else if (!local->type->is_known) {
						local->type = expr->type;
					}
					*ret = assignment;
				}
				return 1;
			}

			// NOTE: Assignment
		case TOKEN_TYPE_SYM_EQUALS:
			{
				parser_next_token(parser);

				OnyxAstNode* lval = lookup_identifier(parser, symbol);

				if (lval != NULL && lval->flags & ONYX_AST_FLAG_LVAL && (lval->flags & ONYX_AST_FLAG_CONST) == 0) {
					OnyxAstNode* rval = parse_expression(parser);
					OnyxAstNode* assignment = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_ASSIGNMENT);
					assignment->right = rval;
					assignment->left = lval;
					*ret = assignment;
					return 1;
				}

				onyx_token_null_toggle(*symbol);
				if (lval == NULL) {
					onyx_message_add(parser->msgs,
							ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
							symbol->pos, symbol->token);
				}

				else if ((lval->flags & ONYX_AST_FLAG_LVAL) == 0) {
					onyx_message_add(parser->msgs,
							ONYX_MESSAGE_TYPE_NOT_LVAL,
							symbol->pos, symbol->token);
				}

				else if (lval->flags & ONYX_AST_FLAG_CONST) {
					onyx_message_add(parser->msgs,
							ONYX_MESSAGE_TYPE_ASSIGN_CONST,
							symbol->pos, symbol->token);
				}
				onyx_token_null_toggle(*symbol);

				find_token(parser, TOKEN_TYPE_SYM_SEMICOLON);
				return 1;
			}

		default:
			parser_prev_token(parser);
	}

	return 0;
}

static OnyxAstNode* parse_return_statement(OnyxParser* parser) {
	expect(parser, TOKEN_TYPE_KEYWORD_RETURN);

	OnyxAstNode* return_node = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_RETURN);
	return_node->type = &builtin_types[ONYX_TYPE_INFO_KIND_VOID];
	OnyxAstNode* expr = NULL;

	if (parser->curr_token->type != TOKEN_TYPE_SYM_SEMICOLON) {
		expr = parse_expression(parser);

		if (expr == NULL || expr == &error_node) {
			return &error_node;
		} else {
			return_node->next = expr;
		}
	}

	return return_node;
}

static OnyxAstNode* parse_statement(OnyxParser* parser) {
	switch (parser->curr_token->type) {
		case TOKEN_TYPE_KEYWORD_RETURN:
			return parse_return_statement(parser);

		case TOKEN_TYPE_OPEN_BRACE:
			return (OnyxAstNode *) parse_block(parser, 0);

		case TOKEN_TYPE_SYMBOL:
			{
				OnyxAstNode* ret = NULL;
				if (parse_symbol_statement(parser, &ret)) return ret;
				// fallthrough
			}

		case TOKEN_TYPE_OPEN_PAREN:
		case TOKEN_TYPE_SYM_PLUS:
		case TOKEN_TYPE_SYM_MINUS:
		case TOKEN_TYPE_SYM_BANG:
		case TOKEN_TYPE_LITERAL_NUMERIC:
		case TOKEN_TYPE_LITERAL_STRING:
			return parse_expression(parser);

		case TOKEN_TYPE_KEYWORD_IF:
			return parse_if_stmt(parser);

		default:
			return NULL;
	}
}

static OnyxAstNodeBlock* parse_block(OnyxParser* parser, b32 belongs_to_function) {
	// --- is for an empty block
	if (parser->curr_token->type == TOKEN_TYPE_SYM_MINUS) {
		expect(parser, TOKEN_TYPE_SYM_MINUS);
		expect(parser, TOKEN_TYPE_SYM_MINUS);
		expect(parser, TOKEN_TYPE_SYM_MINUS);
		return NULL;
	}

	expect(parser, TOKEN_TYPE_OPEN_BRACE);

	OnyxAstNodeBlock* block = (OnyxAstNodeBlock *) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_BLOCK);
	if (belongs_to_function) {
		OnyxAstNodeScope* scope = enter_scope(parser);
		block->scope = scope;
	}

	OnyxAstNode** next = &block->body;
	OnyxAstNode* stmt = NULL;
	while (parser->curr_token->type != TOKEN_TYPE_CLOSE_BRACE) {
		stmt = parse_statement(parser);

		if (stmt != NULL && stmt->kind != ONYX_AST_NODE_KIND_ERROR) {
			*next = stmt;
			next = &stmt->next;
		}

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

	expect(parser, TOKEN_TYPE_CLOSE_BRACE);

	if (belongs_to_function) {
		leave_scope(parser);
	}

	return block;
}

static OnyxTypeInfo* parse_type(OnyxParser* parser) {
	OnyxTypeInfo* type_info = &builtin_types[ONYX_TYPE_INFO_KIND_UNKNOWN];

	OnyxToken* symbol = expect(parser, TOKEN_TYPE_SYMBOL);
	if (symbol == NULL) return type_info;

	onyx_token_null_toggle(*symbol);

	if (!bh_hash_has(OnyxAstNode*, parser->identifiers, symbol->token)) {
		onyx_message_add(parser->msgs, ONYX_MESSAGE_TYPE_UNKNOWN_TYPE, symbol->pos, symbol->token);
	} else {
		OnyxAstNode* type_info_node = bh_hash_get(OnyxAstNode*, parser->identifiers, symbol->token);

		if (type_info_node->kind == ONYX_AST_NODE_KIND_TYPE) {
			type_info = type_info_node->type;
		}
	}

	onyx_token_null_toggle(*symbol);
	return type_info;
}

static OnyxAstNodeParam* parse_function_params(OnyxParser* parser) {
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

		curr_param = (OnyxAstNodeParam *) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_PARAM);
		curr_param->token = symbol;
		curr_param->type = parse_type(parser);

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

	OnyxAstNodeFuncDef* func_def = (OnyxAstNodeFuncDef *) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_FUNCDEF);

	OnyxAstNodeParam* params = parse_function_params(parser);
	func_def->params = params;

	expect(parser, TOKEN_TYPE_RIGHT_ARROW);

	OnyxTypeInfo* return_type = parse_type(parser);
	func_def->return_type = return_type;

	for (OnyxAstNodeParam* p = func_def->params; p != NULL; p = p->next) {
		insert_identifier(parser, (OnyxAstNode *) p, 0);
	}

	func_def->body = parse_block(parser, 1);

	for (OnyxAstNodeParam* p = func_def->params; p != NULL; p = p->next) {
		remove_identifier(parser, (OnyxAstNode *) p);
	}

	return func_def;
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

				if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_PROC) {
					OnyxAstNodeFuncDef* func_def = parse_function_definition(parser);
					func_def->token = symbol;

					onyx_token_null_toggle(*symbol);

					if (!bh_hash_has(OnyxAstNode *, parser->identifiers, symbol->token)) {
						bh_hash_put(OnyxAstNode *, parser->identifiers, symbol->token, (OnyxAstNode *) func_def);
					} else {
						onyx_message_add(parser->msgs,
								ONYX_MESSAGE_TYPE_FUNCTION_REDEFINITION,
								symbol->pos,
								symbol->token);

						// NOTE: I really wish C had defer...
						onyx_token_null_toggle(*symbol);
						return NULL;
					}

					onyx_token_null_toggle(*symbol);
					return (OnyxAstNode *) func_def;

				} else if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_STRUCT) {
					// Handle struct case
					assert(0);
				} else {
					onyx_message_add(parser->msgs,
							ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
							parser->curr_token->pos,
							onyx_get_token_type_name(parser->curr_token->type));
					break;
				}
			}
	}

	parser_next_token(parser);
	return NULL;
}





const char* onyx_ast_node_kind_string(OnyxAstNodeKind kind) {
	return ast_node_names[kind];
}

OnyxAstNode* onyx_ast_node_new(bh_allocator alloc, OnyxAstNodeKind kind) {\
	OnyxAstNode* node = (OnyxAstNode *) bh_alloc(alloc, sizeof(OnyxAstNode));
	node->kind = kind;
	node->flags = 0;
	node->token = NULL;
	node->type = NULL;
	node->next = NULL;
	node->left = NULL;
	node->right = NULL;

	return node;
}

OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs) {
	OnyxParser parser;

	bh_hash_init(bh_heap_allocator(), parser.identifiers, 61);

	OnyxTypeInfo* it = &builtin_types[0];
	while (it->kind != 0xffffffff) {
		OnyxAstNode* tmp = onyx_ast_node_new(alloc, ONYX_AST_NODE_KIND_TYPE);
		tmp->type = it;
		bh_hash_put(OnyxAstNode*, parser.identifiers, (char *)it->name, tmp);
		it++;
	}

	parser.allocator = alloc;
	parser.tokenizer = tokenizer;
	parser.curr_token = tokenizer->tokens;
	parser.prev_token = NULL;
	parser.msgs = msgs;
	parser.curr_scope = NULL;

	return parser;
}

void onyx_parser_free(OnyxParser* parser) {
	bh_hash_free(parser->identifiers);
}

OnyxAstNode* onyx_parse(OnyxParser *parser) {
	OnyxAstNode* program = onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_PROGRAM);

	OnyxAstNode** prev_stmt = &program->next;
	OnyxAstNode* curr_stmt = NULL;
	while (parser->curr_token->type != TOKEN_TYPE_END_STREAM) {
		curr_stmt = parse_top_level_statement(parser);

		// Building a linked list of statements down the "next" chain
		if (curr_stmt != NULL && curr_stmt != &error_node) {
			*prev_stmt = curr_stmt;
			prev_stmt = &curr_stmt->next;
		}
	}

	return program;
}
