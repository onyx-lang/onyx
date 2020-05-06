#include <stdio.h> // TODO: Replace with custom lib
#include <stdlib.h> // TODO: Replace with custom lib
#include "bh.h"

typedef struct Tokenizer {
	char *start, *curr, *end;
	u64 line_number;
} Tokenizer;

typedef enum TokenType {
	TOKEN_TYPE_UNKNOWN,
	TOKEN_TYPE_END_STREAM,

	TOKEN_TYPE_KEYWORD_STRUCT,
	TOKEN_TYPE_KEYWORD_USE,
	TOKEN_TYPE_KEYWORD_EXPORT,
	TOKEN_TYPE_KEYWORD_IF,
	TOKEN_TYPE_KEYWORD_ELSE,
	TOKEN_TYPE_KEYWORD_FOR,
	TOKEN_TYPE_KEYWORD_RETURN,

	TOKEN_TYPE_RIGHT_ARROW,
	TOKEN_TYPE_OPEN_PAREN,
	TOKEN_TYPE_CLOSE_PAREN,
	TOKEN_TYPE_OPEN_BRACE,
	TOKEN_TYPE_CLOSE_BRACE,
	TOKEN_TYPE_OPEN_BRACKET,
	TOKEN_TYPE_CLOSE_BRACKET,

	TOKEN_TYPE_OP_ADD,
	TOKEN_TYPE_OP_SUB,
	TOKEN_TYPE_OP_MUL,
	TOKEN_TYPE_OP_DIV,
	TOKEN_TYPE_OP_MOD,

	TOKEN_TYPE_COUNT
} TokenType;

static const char* TokenTypeNames[] = {
	"TOKEN_TYPE_UNKNOWN",
	"TOKEN_TYPE_END_STREAM",

	"TOKEN_TYPE_KEYWORD_STRUCT",
	"TOKEN_TYPE_KEYWORD_USE",
	"TOKEN_TYPE_KEYWORD_EXPORT",
	"TOKEN_TYPE_KEYWORD_IF",
	"TOKEN_TYPE_KEYWORD_ELSE",
	"TOKEN_TYPE_KEYWORD_FOR",
	"TOKEN_TYPE_KEYWORD_RETURN",

	"TOKEN_TYPE_RIGHT_ARROW",
	"TOKEN_TYPE_OPEN_PAREN",
	"TOKEN_TYPE_CLOSE_PAREN",
	"TOKEN_TYPE_OPEN_BRACE",
	"TOKEN_TYPE_CLOSE_BRACE",
	"TOKEN_TYPE_OPEN_BRACKET",
	"TOKEN_TYPE_CLOSE_BRACKET",

	"TOKEN_TYPE_OP_ADD",
	"TOKEN_TYPE_OP_SUB",
	"TOKEN_TYPE_OP_MUL",
	"TOKEN_TYPE_OP_DIV",
	"TOKEN_TYPE_OP_MOD",

	"TOKEN_TYPE_COUNT"
};

typedef struct Token {
	TokenType type;
	char* token;
	isize length;
	u64 line_number, line_column;
} Token;

b32 token_lit(Tokenizer* tokenizer, Token* tk, char* lit, TokenType type) {
	i64 len = chars_match(tokenizer->curr, lit);
	if (len > 0) {
		tk->type = type;
		tk->token = tokenizer->curr;
		tk->length = len;
		tokenizer->curr += len;
		return 1;
	}
	return 0;
}

Token get_token(Tokenizer* tokenizer) {
	#ifndef LITERAL_TOKEN
	#define LITERAL_TOKEN(token, token_type) \
		if (token_lit(tokenizer, &tk, token, token_type)) goto token_parsed;
	#endif

	Token tk;

	tk.type = TOKEN_TYPE_UNKNOWN;
	tk.token = tokenizer->curr;
	tk.length = 1;
	tk.line_number = 0;
	tk.line_column = 0;

	if (tokenizer->curr == tokenizer->end) {
		tk.type = TOKEN_TYPE_END_STREAM;
		goto token_parsed;
	}

	LITERAL_TOKEN("struct", TOKEN_TYPE_KEYWORD_STRUCT);
	LITERAL_TOKEN("export", TOKEN_TYPE_KEYWORD_EXPORT);
	LITERAL_TOKEN("use", TOKEN_TYPE_KEYWORD_USE);
	LITERAL_TOKEN("if", TOKEN_TYPE_KEYWORD_IF);
	LITERAL_TOKEN("else", TOKEN_TYPE_KEYWORD_IF);
	LITERAL_TOKEN("for", TOKEN_TYPE_KEYWORD_FOR);
	LITERAL_TOKEN("return", TOKEN_TYPE_KEYWORD_RETURN);
	LITERAL_TOKEN("->", TOKEN_TYPE_RIGHT_ARROW);
	LITERAL_TOKEN("(", TOKEN_TYPE_OPEN_PAREN);
	LITERAL_TOKEN(")", TOKEN_TYPE_CLOSE_PAREN);
	LITERAL_TOKEN("{", TOKEN_TYPE_OPEN_BRACE);
	LITERAL_TOKEN("}", TOKEN_TYPE_CLOSE_BRACE);
	LITERAL_TOKEN("[", TOKEN_TYPE_OPEN_BRACKET);
	LITERAL_TOKEN("]", TOKEN_TYPE_CLOSE_BRACKET);
	LITERAL_TOKEN("+", TOKEN_TYPE_OP_ADD);
	LITERAL_TOKEN("-", TOKEN_TYPE_OP_SUB);
	LITERAL_TOKEN("*", TOKEN_TYPE_OP_MUL);
	LITERAL_TOKEN("/", TOKEN_TYPE_OP_DIV);
	LITERAL_TOKEN("%", TOKEN_TYPE_OP_MOD);

	tokenizer->curr++; // Ignore token

token_parsed:
	return tk;
}

int main(int argc, char *argv[]) {
	bh_file source_file;
	bh_file_error err = bh_file_open(&source_file, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		fprintf(stderr, "Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	bh_file_contents fc = bh_file_read_contents(&source_file);
	bh_file_close(&source_file);

	Tokenizer tknizer = {
		.start = fc.data,
		.curr = fc.data,
		.end = fc.data + fc.length,
		.line_number = 1,
	};

	Token tk;
	do {
		tk = get_token(&tknizer);
		char c = *(tk.token + tk.length);
		*(tk.token + tk.length) = '\0';
		printf("%s: %s\n", TokenTypeNames[tk.type], tk.token);
		*(tk.token + tk.length) = c;
	} while (tk.type != TOKEN_TYPE_END_STREAM);


	bh_file_contents_delete(&fc);

	return 0;
}
