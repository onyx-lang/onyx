#include "bh.h"
#include "onyxlex.h"
#include "onyxutils.h"

static const char* token_type_names[] = {
    "TOKEN_TYPE_UNKNOWN",
    "TOKEN_TYPE_END_STREAM",

    "TOKEN_TYPE_COMMENT",

    "struct",
    "use",
    "export",
    "if",
    "else",
    "elseif",
    "return",
    "global",
    "proc",
    "as",
    "while",
    "break",
    "continue",

    "->",
    "<-",
    "---",

    ">=",
    "<=",
    "==",
    "!=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",

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

const char* token_name(TokenType tkn_type) {
    if (tkn_type < Token_Type_Ascii_End) {
        return bh_aprintf(global_scratch_allocator, "%c", (char) tkn_type);
    } else {
        return token_type_names[tkn_type - Token_Type_Ascii_End];
    }
}

void token_toggle_end(OnyxToken* tkn) {
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

    tk.type = Token_Type_Unknown;
    tk.text = tokenizer->curr;
    tk.length = 1;
    tk.pos.line_start = tokenizer->line_start;
    tk.pos.filename = tokenizer->filename;
    tk.pos.line = tokenizer->line_number;
    tk.pos.column = (i32)(tokenizer->curr - tokenizer->line_start) + 1;

    if (tokenizer->curr == tokenizer->end) {
        tk.type = Token_Type_End_Stream;
        goto token_parsed;
    }

    // Comments
    if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '/') {
        tokenizer->curr += 2;
        tk.type = Token_Type_Comment;
        tk.text = tokenizer->curr;

        while (*tokenizer->curr != '\n') {
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.length = tokenizer->curr - tk.text - 2;
        goto token_parsed;
    }

    LITERAL_TOKEN("struct",     1, Token_Type_Keyword_Struct);
    LITERAL_TOKEN("use",        1, Token_Type_Keyword_Use);
    LITERAL_TOKEN("if",         1, Token_Type_Keyword_If);
    LITERAL_TOKEN("elseif",     1, Token_Type_Keyword_Elseif);
    LITERAL_TOKEN("else",       1, Token_Type_Keyword_Else);
    LITERAL_TOKEN("global",     1, Token_Type_Keyword_Global);
    LITERAL_TOKEN("return",     1, Token_Type_Keyword_Return);
    LITERAL_TOKEN("proc",       1, Token_Type_Keyword_Proc);
    LITERAL_TOKEN("as",         1, Token_Type_Keyword_Cast);
    LITERAL_TOKEN("while",      1, Token_Type_Keyword_While);
    LITERAL_TOKEN("break",      1, Token_Type_Keyword_Break);
    LITERAL_TOKEN("continue",   1, Token_Type_Keyword_Continue);
    LITERAL_TOKEN("true",       1, Token_Type_Literal_True);
    LITERAL_TOKEN("false",      1, Token_Type_Literal_False);
    LITERAL_TOKEN("->",         0, Token_Type_Right_Arrow);
    LITERAL_TOKEN("<-",         0, Token_Type_Right_Arrow);
    LITERAL_TOKEN("---",        0, Token_Type_Empty_Block);
    LITERAL_TOKEN("<=",         0, Token_Type_Less_Equal);
    LITERAL_TOKEN(">=",         0, Token_Type_Greater_Equal);
    LITERAL_TOKEN("==",         0, Token_Type_Equal_Equal);
    LITERAL_TOKEN("!=",         0, Token_Type_Not_Equal);
    LITERAL_TOKEN("+=",         0, Token_Type_Plus_Equal);
    LITERAL_TOKEN("-=",         0, Token_Type_Minus_Equal);
    LITERAL_TOKEN("*=",         0, Token_Type_Star_Equal);
    LITERAL_TOKEN("/=",         0, Token_Type_Fslash_Equal);
    LITERAL_TOKEN("%=",         0, Token_Type_Percent_Equal);

    // Symbols
    if (char_is_alpha(*tk.text)) {
        u64 len = 0;
        while (char_is_alphanum(*tokenizer->curr) || charset_contains("_$", *tokenizer->curr)) {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.length = len;
        tk.type = Token_Type_Symbol;
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
        tk.type = Token_Type_Literal_String;
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

        tk.type = Token_Type_Literal_Numeric;
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
    } while (tk->type != Token_Type_End_Stream);
}
