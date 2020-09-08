#include "bh.h"
#include "onyxlex.h"
#include "onyxutils.h"

static const char* token_type_names[] = {
    "TOKEN_TYPE_UNKNOWN",
    "TOKEN_TYPE_END_STREAM",

    "TOKEN_TYPE_COMMENT",

    "package",
    "struct",
    "enum",
    "use",
    "if",
    "else",
    "elseif",
    "return",
    "global",
    "proc",
    "as",
    "cast",
    "while",
    "for",
    "break",
    "continue",
    "sizeof",
    "alignof",
    "defer",
    "do",
    "switch",
    "case",
    "fallthrough",

    "->",
    "<-",
    "---",
    "|>",

    ">=",
    "<=",
    "==",
    "!=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "&=",
    "|=",
    "^=",
    "&&",
    "||",
    "<<",
    ">>",
    ">>>",
    "<<=",
    ">>=",
    ">>>=",
    "..",

    "TOKEN_TYPE_SYMBOL",
    "TOKEN_TYPE_LITERAL_STRING",
    "TOKEN_TYPE_LITERAL_INTEGER",
    "TOKEN_TYPE_LITERAL_FLOAT",
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

#define char_is_alphanum(c) (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || ((c) >= '0' && (c) <= '9'))

static inline b32 token_lit(OnyxTokenizer* tokenizer, OnyxToken* tk, char* lit, b32 is_word, TokenType type) {
    i64 len = 0;
    char* ptr1 = tokenizer->curr;
    char* ptr2 = lit;
    while (*ptr2 != '\0' && *ptr1 == *ptr2) ptr1++, ptr2++, len++;
    if (*ptr2 != '\0') return 0;

    if (is_word && char_is_alphanum(*ptr1) || *ptr1 == '_')
        return 0;

    tk->type = type;
    tk->text = tokenizer->curr;
    tk->length = len;
    tk->pos.line = tokenizer->line_number;
    tk->pos.column = (i32)(tokenizer->curr - tokenizer->line_start) + 1;

    tokenizer->curr += len;

    return 1;
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
    while (1) {
        if (tokenizer->curr == tokenizer->end) break;

        switch (*tokenizer->curr) {
            case ' ':
            case '\n':
            case '\t':
            case '\r':
                INCREMENT_CURR_TOKEN(tokenizer);
                break;
            default:
                goto whitespace_skipped;
        }
    }

whitespace_skipped:

    tk.type = Token_Type_Unknown;
    tk.text = tokenizer->curr;
    tk.length = 1;
    tk.pos.line_start = tokenizer->line_start;
    tk.pos.filename = tokenizer->filename;
    tk.pos.line = tokenizer->line_number;
    tk.pos.column = (u16)(tokenizer->curr - tokenizer->line_start) + 1;

    if (tokenizer->curr == tokenizer->end) {
        tk.type = Token_Type_End_Stream;
        goto token_parsed;
    }

    // Comments
    if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '/') {
        tokenizer->curr += 2;
        tk.type = Token_Type_Comment;
        tk.text = tokenizer->curr;

        while (*tokenizer->curr != '\n' && tokenizer->curr != tokenizer->end) {
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.length = tokenizer->curr - tk.text - 2;
        goto token_parsed;
    }

    if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '*') {
        tokenizer->curr += 2;
        tk.type = Token_Type_Comment;
        tk.text = tokenizer->curr;

        i32 comment_depth = 1;

        while (comment_depth > 0 && tokenizer->curr != tokenizer->end) {
            if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '*') {
                tokenizer->curr += 2;
                comment_depth += 1;
            }

            else if (*tokenizer->curr == '*' && *(tokenizer->curr + 1) == '/') {
                tokenizer->curr += 2;
                comment_depth -= 1;
            }

            else {
                INCREMENT_CURR_TOKEN(tokenizer);
            }
        }

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

    // Hex literal
    if (*tokenizer->curr == '0' && *(tokenizer->curr + 1) == 'x' && charset_contains("0123456789abcdefABCDEF", *(tokenizer->curr + 2))) {
        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);
        u32 len = 3;
        while (char_is_num(*(tokenizer->curr + 1)) || charset_contains("abcdefABCDEF", *(tokenizer->curr + 1))) {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.type = Token_Type_Literal_Integer;
        tk.length = len;

        INCREMENT_CURR_TOKEN(tokenizer);
        goto token_parsed;
    }

    // Number literal
    if (char_is_num(*tokenizer->curr)
        || (*(tokenizer->curr) == '.' && char_is_num(*(tokenizer->curr + 1)))) {
        tk.type = Token_Type_Literal_Integer;

        b32 hit_decimal = 0;
        if (*tokenizer->curr == '.') hit_decimal = 1;

        u32 len = 1;
        while (char_is_num(*(tokenizer->curr + 1)) || (!hit_decimal && *(tokenizer->curr + 1) == '.')) {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);

            if (*tokenizer->curr == '.') hit_decimal = 1;
        }

       if (!hit_decimal && *(tokenizer->curr + 1) == 'l') {
            tk.type = Token_Type_Literal_Integer;

            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }
        else if (*(tokenizer->curr + 1) == 'f') {
            tk.type = Token_Type_Literal_Float;

            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        if (hit_decimal) tk.type = Token_Type_Literal_Float;

        tk.length = len;

        INCREMENT_CURR_TOKEN(tokenizer);
        goto token_parsed;
    }

    char curr = *tokenizer->curr;
    switch (curr) {
    case 'a':
        LITERAL_TOKEN("alignof",     1, Token_Type_Keyword_Alignof);
        LITERAL_TOKEN("as",          1, Token_Type_Keyword_As);
        break;
    case 'b':
        LITERAL_TOKEN("break",       1, Token_Type_Keyword_Break);
        break;
    case 'c':
        LITERAL_TOKEN("case",        1, Token_Type_Keyword_Case);
        LITERAL_TOKEN("cast",        1, Token_Type_Keyword_Cast);
        LITERAL_TOKEN("continue",    1, Token_Type_Keyword_Continue);
        break;
    case 'd':
        LITERAL_TOKEN("defer",       1, Token_Type_Keyword_Defer);
        LITERAL_TOKEN("do",          1, Token_Type_Keyword_Do);
        break;
    case 'e':
        LITERAL_TOKEN("enum",        1, Token_Type_Keyword_Enum);
        LITERAL_TOKEN("elseif",      1, Token_Type_Keyword_Elseif);
        LITERAL_TOKEN("else",        1, Token_Type_Keyword_Else);
        break;
    case 'f':
        LITERAL_TOKEN("for",         1, Token_Type_Keyword_For);
        LITERAL_TOKEN("false",       1, Token_Type_Literal_False);
        LITERAL_TOKEN("fallthrough", 1, Token_Type_Keyword_Fallthrough);
        break;
    case 'g':
        LITERAL_TOKEN("global",      1, Token_Type_Keyword_Global);
        break;
    case 'i':
        LITERAL_TOKEN("if",          1, Token_Type_Keyword_If);
        break;
    case 'p':
        LITERAL_TOKEN("package",     1, Token_Type_Keyword_Package);
        LITERAL_TOKEN("proc",        1, Token_Type_Keyword_Proc);
        break;
    case 'r':
        LITERAL_TOKEN("return",      1, Token_Type_Keyword_Return);
        break;
    case 's':
        LITERAL_TOKEN("sizeof",      1, Token_Type_Keyword_Sizeof);
        LITERAL_TOKEN("struct",      1, Token_Type_Keyword_Struct);
        LITERAL_TOKEN("switch",      1, Token_Type_Keyword_Switch);
        break;
    case 't':
        LITERAL_TOKEN("true",        1, Token_Type_Literal_True);
        break;
    case 'u':
        LITERAL_TOKEN("use",         1, Token_Type_Keyword_Use);
        break;
    case 'w':
        LITERAL_TOKEN("while",       1, Token_Type_Keyword_While);
        break;

    case '-':
        LITERAL_TOKEN("->",          0, Token_Type_Right_Arrow);
        LITERAL_TOKEN("---",         0, Token_Type_Empty_Block);
        LITERAL_TOKEN("-=",          0, Token_Type_Minus_Equal);
        break;

    case '<':
        LITERAL_TOKEN("<-",          0, Token_Type_Right_Arrow);
        LITERAL_TOKEN("<<=",         0, Token_Type_Shl_Equal);
        LITERAL_TOKEN("<<",          0, Token_Type_Shift_Left);
        LITERAL_TOKEN("<=",          0, Token_Type_Less_Equal);
        break;

    case '>':
        LITERAL_TOKEN(">>>=",        0, Token_Type_Sar_Equal);
        LITERAL_TOKEN(">>=",         0, Token_Type_Shr_Equal);
        LITERAL_TOKEN(">>>",         0, Token_Type_Shift_Arith_Right);
        LITERAL_TOKEN(">>",          0, Token_Type_Shift_Right);
        LITERAL_TOKEN(">=",          0, Token_Type_Greater_Equal);
        break;

    case '&':
        LITERAL_TOKEN("&&",          0, Token_Type_And_And);
        LITERAL_TOKEN("&=",          0, Token_Type_And_Equal);
        break;

    case '|':
        LITERAL_TOKEN("|>",          0, Token_Type_Pipe);
        LITERAL_TOKEN("||",          0, Token_Type_Or_Or);
        LITERAL_TOKEN("|=",          0, Token_Type_Or_Equal);
        break;

    default:
        LITERAL_TOKEN("==",          0, Token_Type_Equal_Equal);
        LITERAL_TOKEN("!=",          0, Token_Type_Not_Equal);
        LITERAL_TOKEN("+=",          0, Token_Type_Plus_Equal);
        LITERAL_TOKEN("*=",          0, Token_Type_Star_Equal);
        LITERAL_TOKEN("^=",          0, Token_Type_Xor_Equal);
        LITERAL_TOKEN("/=",          0, Token_Type_Fslash_Equal);
        LITERAL_TOKEN("%=",          0, Token_Type_Percent_Equal);
        LITERAL_TOKEN("..",          0, Token_Type_Dot_Dot);
        break;
    }

    // Symbols
    if (char_is_alpha(*tk.text) || *tokenizer->curr == '_') {
        u64 len = 0;
        while (char_is_alphanum(*tokenizer->curr) || charset_contains("_$", *tokenizer->curr)) {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.length = len;
        tk.type = Token_Type_Symbol;
        goto token_parsed;
    }


    tk.type = (TokenType) *tokenizer->curr;
    INCREMENT_CURR_TOKEN(tokenizer);

token_parsed:
    tk.pos.length = (u16) tk.length;
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

    bh_arr_new(allocator, tknizer.tokens, 1 << 16);
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
