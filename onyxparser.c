
#include "onyxlex.h"
#include "onyxparser.h"

struct OnyxTypeInfo builtin_types[] = {
	{ ONYX_TYPE_INFO_KIND_UNKNOWN, 0, "unknown" },
	{ ONYX_TYPE_INFO_KIND_VOID, 0, "void" },

	{ ONYX_TYPE_INFO_KIND_BOOL, 1, "bool", 0, 0, 0, 1 },

	{ ONYX_TYPE_INFO_KIND_UINT8, 1, "u8", 1, 1, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_UINT16, 2, "u16", 1, 1, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_UINT32, 4, "u32", 1, 1, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_UINT64, 8, "u64", 1, 1, 0, 0 },

	{ ONYX_TYPE_INFO_KIND_INT8, 1, "i8", 1, 0, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_INT16, 2, "i16", 1, 0, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_INT32, 4, "i32", 1, 0, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_INT64, 8, "i64", 1, 0, 0, 0 },

	{ ONYX_TYPE_INFO_KIND_FLOAT32, 4, "f32", 0, 0, 1, 0 },
	{ ONYX_TYPE_INFO_KIND_FLOAT64, 8, "f64", 0, 0, 1, 0 },
	{ ONYX_TYPE_INFO_KIND_SOFT_FLOAT, 8, "sf64", 0, 0, 1, 0 },

	{ 0xffffffff }
};

static OnyxAstNode error_node = { { ONYX_AST_NODE_KIND_ERROR, 0, NULL, &builtin_types[0], NULL, NULL, NULL } };

static void parser_next_token(OnyxParser* parser) {
	parser->prev_token = parser->curr_token;
	parser->curr_token++;
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

// Advances to next token no matter what
static OnyxToken* expect(OnyxParser* parser, OnyxTokenType token_type) {
	OnyxToken* token = parser->curr_token;
	if (token->type != token_type) {
		onyx_message_add(parser->msgs, ONYX_MESSAGE_TYPE_EXPECTED_TOKEN, token->pos, onyx_get_token_type_name(*token));
		return NULL;
	}

	parser_next_token(parser);
	return token;
}

static OnyxAstNode* parse_expression(OnyxParser* parser) {
	return &error_node;
}

static OnyxAstNode* parse_if_stmt(OnyxParser* parser) {
	return &error_node;
}

static OnyxAstNode* parse_block(OnyxParser* parser) {
	assert(parser->curr_token->type == TOKEN_TYPE_OPEN_BRACE);

	return &error_node;
}

static OnyxAstNode* parse_expression_statement(OnyxParser* parser) {

}

static OnyxAstNode* parse_return_statement(OnyxParser* parser) {
	// Only should get here with a return as the current token
	assert(parser->curr_token->type == TOKEN_TYPE_KEYWORD_RETURN);

	OnyxAstNode* expr = NULL;

	OnyxToken* return_token = parser->curr_token;
	parser_next_token(parser);
	if (parser->curr_token->type != TOKEN_TYPE_SYM_SEMICOLON) {
		expr = parse_expression(parser);

		if (expr == &error_node) {
			return &error_node;
		}
	}
}

static OnyxAstNode* parse_statement(OnyxParser* parser, b32 is_top_level) {
	switch (parser->curr_token->type) {
	case TOKEN_TYPE_KEYWORD_RETURN:
		return parse_return_statement(parser);

	case TOKEN_TYPE_OPEN_BRACE:
		return (OnyxAstNode *) parse_block(parser);

	case TOKEN_TYPE_SYMBOL:
	case TOKEN_TYPE_OPEN_PAREN:
	case TOKEN_TYPE_SYM_PLUS:
	case TOKEN_TYPE_SYM_MINUS:
	case TOKEN_TYPE_SYM_BANG:
	case TOKEN_TYPE_LITERAL_NUMERIC:
	case TOKEN_TYPE_LITERAL_STRING:
		return parse_expression_statement(parser);

	case TOKEN_TYPE_KEYWORD_IF:
		return parse_if_stmt(parser);

	case TOKEN_TYPE_SYM_SEMICOLON:
		return NULL;

	default:
		printf("ERROR\n");
		parser_next_token(parser);
		return NULL;
	}
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
	OnyxAstNodeParam** walker = NULL;

	OnyxToken* symbol;
	while (parser->curr_token->type != TOKEN_TYPE_CLOSE_PAREN) {
		if (parser->curr_token->type == TOKEN_TYPE_SYM_COMMA) parser_next_token(parser);

		symbol = expect(parser, TOKEN_TYPE_SYMBOL);
		curr_param = (OnyxAstNodeParam *) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_PARAM);
		curr_param->token = symbol;
		curr_param->type = parse_type(parser);

		curr_param->next = NULL;
		if (first_param == NULL) {
			first_param = curr_param;
		} else {
			(*walker)->next = curr_param;
		}
		walker = &curr_param;
	}

	parser_next_token(parser); // Skip the )
	return first_param;
}

static OnyxAstNodeFuncDef* parse_function_definition(OnyxParser* parser) {
	expect(parser, TOKEN_TYPE_KEYWORD_PROC);

	OnyxAstNodeFuncDef* func_def = (OnyxAstNodeFuncDef *) onyx_ast_node_new(parser->allocator, ONYX_AST_NODE_KIND_FUNCDEF);
	func_def->param_count = 0;

	OnyxAstNodeParam* params = parse_function_params(parser);
	func_def->params = params;

	for (OnyxAstNode* walker = (OnyxAstNode *) params; walker != NULL; walker = walker->next)
		func_def->param_count++;

	expect(parser, TOKEN_TYPE_RIGHT_ARROW);

	OnyxTypeInfo* return_type = parse_type(parser);
	func_def->return_type = return_type;

	func_def->body = NULL;
	return func_def;
}


static OnyxAstNode* parse_top_level_statement(OnyxParser* parser) {
	switch (parser->curr_token->type) {
	case TOKEN_TYPE_KEYWORD_USE:
		assert(0);
		break;

	case TOKEN_TYPE_KEYWORD_EXPORT:
		assert(0);
		break;	

	case TOKEN_TYPE_SYMBOL: {
		OnyxToken* symbol = parser->curr_token;
		parser_next_token(parser);

		expect(parser, TOKEN_TYPE_SYM_COLON);
		expect(parser, TOKEN_TYPE_SYM_COLON);

		if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_PROC) {
			OnyxAstNodeFuncDef* func_def = parse_function_definition(parser);
			func_def->token = symbol;
			return (OnyxAstNode *) func_def;

		} else if (parser->curr_token->type == TOKEN_TYPE_KEYWORD_STRUCT) {
			// Handle struct case
			assert(0);
		} else {
			onyx_message_add(parser->msgs,
				ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
				parser->curr_token->pos,
				onyx_get_token_type_name(*parser->curr_token));
		}
	} break;
	}
	parser_next_token(parser);
	return NULL;
}







OnyxAstNode* onyx_ast_node_new(bh_allocator alloc, OnyxAstNodeKind kind) {\
	OnyxAstNode* node = (OnyxAstNode *) bh_alloc(alloc, sizeof(OnyxAstNode));
	node->kind = kind;

	return node;
}

OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs) {
	OnyxParser parser;

	bh_hash_init(bh_heap_allocator(), parser.identifiers);

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

	return parser;
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
