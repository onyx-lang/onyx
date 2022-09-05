#ifndef _ASSEMBLER_H
#define _ASSEMBLER_H

typedef enum token_type_t token_type_t;
typedef struct token_t token_t;

enum token_type_t {
    token_none,
    token_newline,

    token_integer,
    token_integer_long,
    token_float,
    token_float_double,

    token_command,
    token_symbol,
    token_register,
    token_label,
    token_comma,
};

struct token_t {
    token_type_t type;
    char        *text;
    int          line;
};

token_t asm_lexer_next_token();
token_t asm_lexer_peek_token();
void asm_lexer_input(char *data, int size);

extern char *token_names[];

#endif

