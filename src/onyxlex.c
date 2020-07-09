#include "bh.h"
#include "onyxlex.h"
#include "onyxutils.h"

static const char* token_type_names[] = {
    "TOKEN_TYPE_UNKNOWN",
    "TOKEN_TYPE_END_STREAM",

    "TOKEN_TYPE_COMMENT",

    "struct",        //"TOKEN_TYPE_KEYWORD_STRUCT",
    "use",            //"TOKEN_TYPE_KEYWORD_USE",
    "export",        //"TOKEN_TYPE_KEYWORD_EXPORT",
    "if",            //"TOKEN_TYPE_KEYWORD_IF",
    "else",            //"TOKEN_TYPE_KEYWORD_ELSE",
    "elseif",        //"TOKEN_TYPE_KEYWORD_ELSEIF",
    "return",        //"TOKEN_TYPE_KEYWORD_RETURN",
    "foreign",        //"TOKEN_TYPE_KEYWORD_FOREIGN",
    "proc",            //"TOKEN_TYPE_KEYWORD_PROC",
    "as",             //"TOKEN_TYPE_KEYWORD_CAST",
    "while",        //"TOKEN_TYPE_KEYWORD_WHILE",
    "break",        //"TOKEN_TYPE_KEYWORD_BREAK",
    "continue",     //"TOKEN_TYPE_KEYWORD_CONTINUE,

    "->", //"TOKEN_TYPE_RIGHT_ARROW",
    "<-", //"TOKEN_TYPE_LEFT_ARROW",

    ">=", // "TOKEN_TYPE_SYM_GREATER_EQUAL",
    "<=", // "TOKEN_TYPE_SYM_LESS_EQUAL",
    "==", // "TOKEN_TYPE_SYM_EQUALS_EQUALS",
    "!=", // "TOKEN_TYPE_SYM_NOT_EQUAL",
    "+=", // "TOKEN_TYPE_SYM_PLUS_EQUAL",
    "-=", // "TOKEN_TYPE_SYM_MINUS_EQUAL",
    "*=", // "TOKEN_TYPE_SYM_STAR_EQUAL",
    "/=", // "TOKEN_TYPE_SYM_FSLASH_EQUAL",
    "%=", // "TOKEN_TYPE_SYM_PERCENT_EQUAL",

    "TOKEN_TYPE_SYMBOL",
    "TOKEN_TYPE_LITERAL_STRING",
    "TOKEN_TYPE_LITERAL_NUMERIC",
    "true",
    "false",

    "TOKEN_TYPE_COUNT"
};

#ifndef LITERAL_TOKEN
#define LITERAL_TOKEN(token, word, token_type) \
    if (token_lit(tokenizer, &tk, token, word, token_type)) goto token_parsed;
#endif

#ifndef INCREMENT_CURR_TOKEN
#define INCREMENT_CURR_TOKEN(tkn) { \
    if (*(tkn)->curr == '\n') { \
        (tkn)->line_number++; \
        (tkn)->line_start = (tkn)->curr + 1; \
    } \
    (tkn)->curr++; \
}
#endif

static b32 token_lit(OnyxTokenizer* tokenizer, OnyxToken* tk, char* lit, b32 is_word, TokenType type) {
    i64 len = chars_match(tokenizer->curr, lit);
    if (len > 0) {
        if (is_word && char_is_alphanum(*(tokenizer->curr + len)) || charset_contains("_$", *(tokenizer->curr + len)))
            return 0;

        tk->type = type;
        tk->text = tokenizer->curr;
        tk->length = len;
        tk->pos.line = tokenizer->line_number;
        tk->pos.column = (i32)(tokenizer->curr - tokenizer->line_start) + 1;

        tokenizer->curr += len;

        return 1;
    }
    return 0;
}

const char* onyx_get_token_type_name(TokenType tkn_type) {
    if (tkn_type < TOKEN_TYPE_ASCII_END) {
        return bh_aprintf(global_scratch_allocator, "%c", (char) tkn_type);
    } else {
        return token_type_names[tkn_type - TOKEN_TYPE_ASCII_END];
    }
}

void onyx_token_null_toggle(OnyxToken* tkn) {
    static char backup = 0;
    char tmp = tkn->text[tkn->length];
    tkn->text[tkn->length] = backup;
    backup = tmp;
}

OnyxToken* onyx_get_token(OnyxTokenizer* tokenizer) {
    OnyxToken tk;

    // Skip whitespace
    while (char_is_whitespace(*tokenizer->curr) && tokenizer->curr != tokenizer->end)
        INCREMENT_CURR_TOKEN(tokenizer)

    tk.type = TOKEN_TYPE_UNKNOWN;
    tk.text = tokenizer->curr;
    tk.length = 1;
    tk.pos.line_start = tokenizer->line_start;
    tk.pos.filename = tokenizer->filename;
    tk.pos.line = tokenizer->line_number;
    tk.pos.column = (i32)(tokenizer->curr - tokenizer->line_start) + 1;

    if (tokenizer->curr == tokenizer->end) {
        tk.type = TOKEN_TYPE_END_STREAM;
        goto token_parsed;
    }

    // Comments
    if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '/') {
        tokenizer->curr += 2;
        tk.type = TOKEN_TYPE_COMMENT;
        tk.text = tokenizer->curr;

        while (*tokenizer->curr != '\n') {
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.length = tokenizer->curr - tk.text - 2;
        goto token_parsed;
    }

    LITERAL_TOKEN("struct",     1, TOKEN_TYPE_KEYWORD_STRUCT);
    LITERAL_TOKEN("export",     1, TOKEN_TYPE_KEYWORD_EXPORT);
    LITERAL_TOKEN("use",        1, TOKEN_TYPE_KEYWORD_USE);
    LITERAL_TOKEN("if",         1, TOKEN_TYPE_KEYWORD_IF);
    LITERAL_TOKEN("elseif",     1, TOKEN_TYPE_KEYWORD_ELSEIF);
    LITERAL_TOKEN("else",       1, TOKEN_TYPE_KEYWORD_ELSE);
    LITERAL_TOKEN("foreign",    1, TOKEN_TYPE_KEYWORD_FOREIGN);
    LITERAL_TOKEN("return",     1, TOKEN_TYPE_KEYWORD_RETURN);
    LITERAL_TOKEN("proc",       1, TOKEN_TYPE_KEYWORD_PROC);
    LITERAL_TOKEN("as",         1, TOKEN_TYPE_KEYWORD_CAST);
    LITERAL_TOKEN("while",      1, TOKEN_TYPE_KEYWORD_WHILE);
    LITERAL_TOKEN("break",      1, TOKEN_TYPE_KEYWORD_BREAK);
    LITERAL_TOKEN("continue",   1, TOKEN_TYPE_KEYWORD_CONTINUE);
    LITERAL_TOKEN("true",       1, TOKEN_TYPE_LITERAL_BOOL_TRUE);
    LITERAL_TOKEN("false",      1, TOKEN_TYPE_LITERAL_BOOL_FALSE);
    LITERAL_TOKEN("->",         0, TOKEN_TYPE_RIGHT_ARROW);
    LITERAL_TOKEN("<-",         0, TOKEN_TYPE_RIGHT_ARROW);
    LITERAL_TOKEN("<=",         0, TOKEN_TYPE_LESS_EQUAL);
    LITERAL_TOKEN(">=",         0, TOKEN_TYPE_GREATER_EQUAL);
    LITERAL_TOKEN("==",         0, TOKEN_TYPE_EQUAL_EQUAL);
    LITERAL_TOKEN("!=",         0, TOKEN_TYPE_NOT_EQUAL);
    LITERAL_TOKEN("+=",         0, TOKEN_TYPE_PLUS_EQUAL);
    LITERAL_TOKEN("-=",         0, TOKEN_TYPE_MINUS_EQUAL);
    LITERAL_TOKEN("*=",         0, TOKEN_TYPE_STAR_EQUAL);
    LITERAL_TOKEN("/=",         0, TOKEN_TYPE_FSLASH_EQUAL);
    LITERAL_TOKEN("%=",         0, TOKEN_TYPE_PERCENT_EQUAL);

    // Symbols
    if (char_is_alpha(*tk.text)) {
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
    if (*tk.text == '"') {
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

        tk.text++;
        tk.type = TOKEN_TYPE_LITERAL_STRING;
        tk.length = len;
        goto token_parsed;
    }

    // Number literal
    if (char_is_num(*tokenizer->curr)) {
        u32 len = 1;
        while (char_is_num(*(tokenizer->curr + 1)) || *(tokenizer->curr + 1) == '.') {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        if (*(tokenizer->curr + 1) == 'f') {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.type = TOKEN_TYPE_LITERAL_NUMERIC;
        tk.length = len;

        INCREMENT_CURR_TOKEN(tokenizer);
        goto token_parsed;
    }

    tk.type = (TokenType) *tokenizer->curr;
    INCREMENT_CURR_TOKEN(tokenizer);

token_parsed:
    bh_arr_push(tokenizer->tokens, tk);

    return &tokenizer->tokens[bh_arr_length(tokenizer->tokens) - 1];
}

OnyxTokenizer onyx_tokenizer_create(bh_allocator allocator, bh_file_contents *fc) {
    OnyxTokenizer tknizer = {
        .start          = fc->data,
        .curr           = fc->data,
        .end            = fc->data + fc->length,

        .filename       = fc->filename,

        .line_number    = 1,
        .line_start     = fc->data,
        .tokens         = NULL,
    };

    bh_arr_new(allocator, tknizer.tokens, 512);
    return tknizer;
}

void onyx_tokenizer_free(OnyxTokenizer* tokenizer) {
    bh_arr_free(tokenizer->tokens);
}

void onyx_lex_tokens(OnyxTokenizer* tokenizer) {
    OnyxToken* tk;
    do {
        tk = onyx_get_token(tokenizer);
    } while (tk->type != TOKEN_TYPE_END_STREAM);
}
