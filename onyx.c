#include <stdio.h> // TODO: Replace with custom lib
#include "bh.h"

typedef struct Tokenizer {
	char *start, *curr, *end;

	// TODO: Fix the line number and column count
	u64 line_number;
	u64 line_column;
} Tokenizer;

typedef enum TokenType {
	TOKEN_TYPE_UNKNOWN,
	TOKEN_TYPE_END_STREAM,

	TOKEN_TYPE_COMMENT,

	TOKEN_TYPE_KEYWORD_STRUCT,
	TOKEN_TYPE_KEYWORD_USE,
	TOKEN_TYPE_KEYWORD_EXPORT,
	TOKEN_TYPE_KEYWORD_IF,
	TOKEN_TYPE_KEYWORD_ELSE,
	TOKEN_TYPE_KEYWORD_FOR,
	TOKEN_TYPE_KEYWORD_RETURN,

	TOKEN_TYPE_RIGHT_ARROW,
	TOKEN_TYPE_LEFT_ARROW,
	TOKEN_TYPE_OPEN_PAREN,
	TOKEN_TYPE_CLOSE_PAREN,
	TOKEN_TYPE_OPEN_BRACE,
	TOKEN_TYPE_CLOSE_BRACE,
	TOKEN_TYPE_OPEN_BRACKET,
	TOKEN_TYPE_CLOSE_BRACKET,
	TOKEN_TYPE_OPEN_ANGLE,
	TOKEN_TYPE_CLOSE_ANGLE,

	TOKEN_TYPE_SYM_PLUS,
	TOKEN_TYPE_SYM_MINUS,
	TOKEN_TYPE_SYM_STAR,
	TOKEN_TYPE_SYM_PERCENT,
	TOKEN_TYPE_SYM_FSLASH,
	TOKEN_TYPE_SYM_BSLASH,
	TOKEN_TYPE_SYM_COLON,
	TOKEN_TYPE_SYM_SEMICOLON,
	TOKEN_TYPE_SYM_COMMA,
	TOKEN_TYPE_SYM_EQUALS,
	TOKEN_TYPE_SYM_GRAVE,
	TOKEN_TYPE_SYM_TILDE,
	TOKEN_TYPE_SYM_BANG,

	TOKEN_TYPE_SYMBOL,
	TOKEN_TYPE_LITERAL_STRING,
	TOKEN_TYPE_LITERAL_NUMERIC,

	TOKEN_TYPE_COUNT
} TokenType;

static const char* TokenTypeNames[] = {
	"TOKEN_TYPE_UNKNOWN",
	"TOKEN_TYPE_END_STREAM",

	"TOKEN_TYPE_COMMENT",

	"TOKEN_TYPE_KEYWORD_STRUCT",
	"TOKEN_TYPE_KEYWORD_USE",
	"TOKEN_TYPE_KEYWORD_EXPORT",
	"TOKEN_TYPE_KEYWORD_IF",
	"TOKEN_TYPE_KEYWORD_ELSE",
	"TOKEN_TYPE_KEYWORD_FOR",
	"TOKEN_TYPE_KEYWORD_RETURN",

	"TOKEN_TYPE_RIGHT_ARROW",
	"TOKEN_TYPE_LEFT_ARROW",
	"TOKEN_TYPE_OPEN_PAREN",
	"TOKEN_TYPE_CLOSE_PAREN",
	"TOKEN_TYPE_OPEN_BRACE",
	"TOKEN_TYPE_CLOSE_BRACE",
	"TOKEN_TYPE_OPEN_BRACKET",
	"TOKEN_TYPE_CLOSE_BRACKET",
	"TOKEN_TYPE_OPEN_ANGLE",
	"TOKEN_TYPE_CLOSE_ANGLE",

	"TOKEN_TYPE_SYM_PLUS",
	"TOKEN_TYPE_SYM_MINUS",
	"TOKEN_TYPE_SYM_STAR",
	"TOKEN_TYPE_SYM_PERCENT",
	"TOKEN_TYPE_SYM_FSLASH",
	"TOKEN_TYPE_SYM_BSLASH",
	"TOKEN_TYPE_SYM_COLON",
	"TOKEN_TYPE_SYM_SEMICOLON",
	"TOKEN_TYPE_SYM_COMMA",
	"TOKEN_TYPE_SYM_EQUALS",
	"TOKEN_TYPE_SYM_GRAVE",
	"TOKEN_TYPE_SYM_TILDE",
	"TOKEN_TYPE_SYM_BANG",

	"TOKEN_TYPE_SYMBOL",
	"TOKEN_TYPE_LITERAL_STRING",
	"TOKEN_TYPE_LITERAL_NUMERIC",

	"TOKEN_TYPE_COUNT"
};

typedef struct Token {
	TokenType type;
	char* token;
	isize length;
	u64 line_number, line_column;
} Token;

#ifndef LITERAL_TOKEN
#define LITERAL_TOKEN(token, token_type) \
	if (token_lit(tokenizer, &tk, token, token_type)) goto token_parsed;
#endif

#ifndef INCREMENT_CURR_TOKEN
#define INCREMENT_CURR_TOKEN(tkn) { \
	tkn->curr++; \
	tkn->line_column++; \
	if (*tkn->curr == '\n') { \
		tkn->line_number++; \
		tkn->line_column = 1; \
	} \
}
#endif

b32 token_lit(Tokenizer* tokenizer, Token* tk, char* lit, TokenType type) {
	i64 len = chars_match(tokenizer->curr, lit);
	if (len > 0) {
		tk->type = type;
		tk->token = tokenizer->curr;
		tk->length = len;

		tokenizer->curr += len;
		tokenizer->line_column += len;

		return 1;
	}
	return 0;
}

Token get_token(Tokenizer* tokenizer) {
	Token tk;

	// Skip whitespace
	while (char_is_whitespace(*tokenizer->curr) && tokenizer->curr != tokenizer->end)
		INCREMENT_CURR_TOKEN(tokenizer)

	tk.type = TOKEN_TYPE_UNKNOWN;
	tk.token = tokenizer->curr;
	tk.length = 1;
	tk.line_number = tokenizer->line_number;
	tk.line_column = tokenizer->line_column;

	if (tokenizer->curr == tokenizer->end) {
		tk.type = TOKEN_TYPE_END_STREAM;
		goto token_parsed;
	}

	// Comments
	if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '*') {
		tokenizer->curr += 2;	
		tk.type = TOKEN_TYPE_COMMENT;
		tk.token = tokenizer->curr;
		u16 layers = 1;

		while (layers >= 1) {
			INCREMENT_CURR_TOKEN(tokenizer);

			if (tokenizer->curr == tokenizer->end) {
				tk.type = TOKEN_TYPE_END_STREAM;
				break;
			}
			
			if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '*') {
				layers++;
				INCREMENT_CURR_TOKEN(tokenizer);
			}

			if (*tokenizer->curr == '*' && *(tokenizer->curr + 1) == '/') {
				layers--;
				INCREMENT_CURR_TOKEN(tokenizer);
			}
		}	

		INCREMENT_CURR_TOKEN(tokenizer);

		tk.length = tokenizer->curr - tk.token - 2;
		goto token_parsed;
	}

	LITERAL_TOKEN("struct", TOKEN_TYPE_KEYWORD_STRUCT);
	LITERAL_TOKEN("export", TOKEN_TYPE_KEYWORD_EXPORT);
	LITERAL_TOKEN("use", TOKEN_TYPE_KEYWORD_USE);
	LITERAL_TOKEN("if", TOKEN_TYPE_KEYWORD_IF);
	LITERAL_TOKEN("else", TOKEN_TYPE_KEYWORD_ELSE);
	LITERAL_TOKEN("for", TOKEN_TYPE_KEYWORD_FOR);
	LITERAL_TOKEN("return", TOKEN_TYPE_KEYWORD_RETURN);
	LITERAL_TOKEN("->", TOKEN_TYPE_RIGHT_ARROW);
	LITERAL_TOKEN("<-", TOKEN_TYPE_RIGHT_ARROW);
	LITERAL_TOKEN("(", TOKEN_TYPE_OPEN_PAREN);
	LITERAL_TOKEN(")", TOKEN_TYPE_CLOSE_PAREN);
	LITERAL_TOKEN("{", TOKEN_TYPE_OPEN_BRACE);
	LITERAL_TOKEN("}", TOKEN_TYPE_CLOSE_BRACE);
	LITERAL_TOKEN("[", TOKEN_TYPE_OPEN_BRACKET);
	LITERAL_TOKEN("]", TOKEN_TYPE_CLOSE_BRACKET);
	LITERAL_TOKEN("<", TOKEN_TYPE_OPEN_ANGLE);
	LITERAL_TOKEN(">", TOKEN_TYPE_CLOSE_ANGLE);
	LITERAL_TOKEN("+", TOKEN_TYPE_SYM_PLUS);
	LITERAL_TOKEN("-", TOKEN_TYPE_SYM_MINUS);
	LITERAL_TOKEN("*", TOKEN_TYPE_SYM_STAR);
	LITERAL_TOKEN("/", TOKEN_TYPE_SYM_FSLASH);
	LITERAL_TOKEN("%", TOKEN_TYPE_SYM_PERCENT);
	LITERAL_TOKEN("\\", TOKEN_TYPE_SYM_BSLASH);
	LITERAL_TOKEN(":", TOKEN_TYPE_SYM_COLON);
	LITERAL_TOKEN(";", TOKEN_TYPE_SYM_SEMICOLON);
	LITERAL_TOKEN(",", TOKEN_TYPE_SYM_COMMA);
	LITERAL_TOKEN("=", TOKEN_TYPE_SYM_EQUALS);
	LITERAL_TOKEN("`", TOKEN_TYPE_SYM_GRAVE);
	LITERAL_TOKEN("~", TOKEN_TYPE_SYM_TILDE);
	LITERAL_TOKEN("!", TOKEN_TYPE_SYM_BANG);

	// Symbols
	if (char_is_alpha(*tk.token)) {
		u64 len = 0;
		while (char_is_alphanum(*tokenizer->curr) || charset_contains("_$", *tokenizer->curr)) {
			len++;
			INCREMENT_CURR_TOKEN(tokenizer);
		}

		tk.length = len;
		tk.type = TOKEN_TYPE_SYMBOL;
		goto token_parsed;
	}

	// String literal
	if (*tk.token == '"') {
		u64 len = 0;
		u64 slash_count = 0;

		INCREMENT_CURR_TOKEN(tokenizer);

		while (!(*tokenizer->curr == '"' && slash_count == 0)) {
			len++;

			if (*tokenizer->curr == '\\') {
				slash_count += 1;
				slash_count %= 2;
			} else {
				slash_count = 0;
			}

			INCREMENT_CURR_TOKEN(tokenizer);
		}

		INCREMENT_CURR_TOKEN(tokenizer);

		tk.token++;
		tk.type = TOKEN_TYPE_LITERAL_STRING;
		tk.length = len;
		goto token_parsed;
	}

	// Number literal
	if (char_is_num(*tokenizer->curr)) {
		u64 len = 0;
		while (char_is_num(*tokenizer->curr) || *tokenizer->curr == '.') {
			len++;
			INCREMENT_CURR_TOKEN(tokenizer);
		}

		tk.type = TOKEN_TYPE_LITERAL_NUMERIC;
		tk.length = len;
	}

	INCREMENT_CURR_TOKEN(tokenizer);

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
		.end = fc.data + fc.length - 1,
		.line_number = 1,
		.line_column = 1,
	};

	Token tk;
	do {
		tk = get_token(&tknizer);
		char c = *(tk.token + tk.length);
		*(tk.token + tk.length) = '\0';
		printf("Line %ld, Column %ld: \n%s: %s\n", tk.line_number, tk.line_column, TokenTypeNames[tk.type], tk.token);
		*(tk.token + tk.length) = c;
	} while (tk.type != TOKEN_TYPE_END_STREAM);


	bh_file_contents_delete(&fc);

	return 0;
}
