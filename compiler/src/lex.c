#include "bh.h"
#include "lex.h"
#include "utils.h"
#include "errors.h"

static const char* token_type_names[] = {
    "UNKNOWN",
    "the end of file",

    "a comment",

    "", // start
    "package",
    "struct",
    "union",
    "enum",
    "use",
    "if",
    "else",
    "elseif",
    "return",
    "global",
    "cast",
    "while",
    "for",
    "break",
    "continue",
    "sizeof",
    "alignof",
    "typeof",
    "defer",
    "do",
    "case",
    "switch",
    "fallthrough",
    "macro",
    "interface",
    "where",
    "as",
    "in",
    "", // end

    "->",
    "=>",
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
    "..=",
    "~~",
    "??",

    "a symbol",
    "a string",
    "a character literal",
    "an integer",
    "a float",
    "true",
    "false",

    "an inserted semicolon",

    "a doc comment",

    "a procedural macro body",

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
    if ((tkn)->curr != (tkn)->end) (tkn)->curr++; \
}
#endif

#define char_is_alphanum(c) (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || ((c) >= '0' && (c) <= '9'))
#define char_is_num(c)      ((c) >= '0' && (c) <= '9')
#define char_is_alpha(c)    (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z'))

static inline b32 token_lit(OnyxTokenizer* tokenizer, OnyxToken* tk, char* lit, b32 is_word, TokenType type) {
    i64 len = 0;
    char* ptr1 = tokenizer->curr;
    char* ptr2 = lit;
    while (*ptr2 != '\0' && *ptr1 == *ptr2) ptr1++, ptr2++, len++;
    if (*ptr2 != '\0') return 0;

    if (is_word && (char_is_alphanum(*ptr1) || *ptr1 == '_'))
        return 0;

    tk->type = type;
    tk->text = tokenizer->curr;
    tk->length = len;
    tk->pos.line = tokenizer->line_number;
    tk->pos.column = (i32)(tokenizer->curr - tokenizer->line_start) + 1;

    tokenizer->curr += len;

    return 1;
}

const char *token_type_name(TokenType tkn_type) {
    static char hack_tmp_buffer[32];
    if (tkn_type < Token_Type_Ascii_End) {
        bh_snprintf(hack_tmp_buffer, 31, "%c", (char) tkn_type);
        return hack_tmp_buffer;
    } else {
        return token_type_names[tkn_type - Token_Type_Ascii_End];
    }
}

const char* token_name(OnyxToken * tkn) {
    TokenType tkn_type = tkn->type;

    if (tkn_type == Token_Type_Symbol) {
        static char hack_tmp_buffer[512];
        bh_snprintf(hack_tmp_buffer, 511, "%b", tkn->text, tkn->length);
        return hack_tmp_buffer;
    }

    return token_type_name(tkn_type);
}

void token_toggle_end(OnyxToken* tkn) {
    static char backup = 0;
    char tmp = tkn->text[tkn->length];
    assert(backup == '\0' || tmp == '\0'); // Sanity check
    tkn->text[tkn->length] = backup;
    backup = tmp;
}

OnyxToken* onyx_get_token(OnyxTokenizer* tokenizer) {
    OnyxToken tk;

    // Skip whitespace
    while (1) {
        if (tokenizer->curr == tokenizer->end) break;

        switch (*tokenizer->curr) {
            case '\n':
                if (tokenizer->insert_semicolon && tokenizer->optional_semicolons) {
                    OnyxToken semicolon_token;
                    semicolon_token.type = Token_Type_Inserted_Semicolon;
                    semicolon_token.text = "; ";
                    semicolon_token.length = 1;
                    semicolon_token.pos.line_start = tokenizer->line_start;
                    semicolon_token.pos.filename = tokenizer->filename;
                    semicolon_token.pos.line = tokenizer->line_number;
                    semicolon_token.pos.column = (u16)(tokenizer->curr - tokenizer->line_start) + 1;
                    bh_arr_push(tokenizer->tokens, semicolon_token);
                    tokenizer->insert_semicolon = 0;
                }
            case ' ':
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

    // She-bang
    if (tokenizer->curr == tokenizer->start) {
        if (*tokenizer->curr == '#' && *(tokenizer->curr + 1) == '!') {
            tk.type = Token_Type_Comment;

            tokenizer->curr += 2;
            while (*tokenizer->curr++ != '\n' && tokenizer->curr != tokenizer->end);

            tk.length = tokenizer->curr - tk.text;
            goto token_parsed;
        }
    }

    // Comments
    if (*tokenizer->curr == '/' && *(tokenizer->curr + 1) == '/') {
        tokenizer->curr += 2;

        if (*tokenizer->curr == '/') {
            tokenizer->curr += 1;
            tk.type = Token_Type_Doc_Comment;
        } else {
            tk.type = Token_Type_Comment;
        }

        tk.text = tokenizer->curr;
        tk.pos.column = (u16)(tokenizer->curr - tokenizer->line_start) + 1;

        while (*tokenizer->curr != '\n' && tokenizer->curr != tokenizer->end) {
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.length = tokenizer->curr - tk.text;

        if (bh_arr_length(tokenizer->tokens) == 0 && bh_str_starts_with(tk.text, "+optional-semicolons")) {
            tokenizer->optional_semicolons = 1;
        }

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

    if (*tk.text == '"' && *(tk.text + 1) == '"' && *(tk.text + 2) == '"') {
        u64 len = 0;

        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);

        while (!(*tokenizer->curr == '"' && *(tokenizer->curr + 1) == '"' && *(tokenizer->curr + 2) == '"') && tokenizer->curr != tokenizer->end) {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }
        
        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);

        tk.text += 3;
        tk.length = len;
        tk.type = Token_Type_Literal_String;
        goto token_parsed;
    }

    // String/Character literal
    if (*tk.text == '"' || *tk.text == '\'') {
        u64 len = 0;
        u64 slash_count = 0;

        char ch = *tk.text;
        INCREMENT_CURR_TOKEN(tokenizer);

        while (tokenizer->curr != tokenizer->end && !(*tokenizer->curr == ch && slash_count == 0)) {
            len++;

            if (*tokenizer->curr == '\n' && ch == '\'') {
                tk.pos.length = (u16) len;
                onyx_report_error(tokenizer->context, tk.pos, Error_Critical, "Character literal not terminated by end of line.");
                break;
            }

            if (*tokenizer->curr == '\\') {
                slash_count += 1;
                slash_count %= 2;
            } else {
                slash_count = 0;
            }

            INCREMENT_CURR_TOKEN(tokenizer);
            if (tokenizer->curr == tokenizer->end) {
                onyx_report_error(tokenizer->context, tk.pos, Error_Critical, "String literal not closed. String literal starts here.");
                break;
            }
        }

        INCREMENT_CURR_TOKEN(tokenizer);

        tk.text++;
        tk.type = ch == '"' ? Token_Type_Literal_String : Token_Type_Literal_Char;
        tk.length = len;
        goto token_parsed;
    }

    // Hex literal
    if (*tokenizer->curr == '0' && *(tokenizer->curr + 1) == 'x' && charset_contains("0123456789abcdefABCDEF", *(tokenizer->curr + 2))) {
        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);
        u32 len = 3;
        while (char_is_num(*(tokenizer->curr + 1)) || charset_contains("abcdefABCDEF_", *(tokenizer->curr + 1))) {
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
        b32 hit_exponent = 0;
        if (*tokenizer->curr == '.') hit_decimal = 1;

        u32 len = 1;
        while (char_is_num(*(tokenizer->curr + 1))
            || (*(tokenizer->curr + 1) == '_')
            || (!hit_exponent && *(tokenizer->curr + 1) == 'e')
            || (!hit_decimal && !hit_exponent && *(tokenizer->curr + 1) == '.' && *(tokenizer->curr + 2) != '.')
            || (hit_exponent && *(tokenizer->curr + 1) == '-')) {
            len++;
            INCREMENT_CURR_TOKEN(tokenizer);

            if (*tokenizer->curr == '.') hit_decimal  = 1;
            if (*tokenizer->curr == 'e') hit_exponent = 1;
        }

        if (*(tokenizer->curr + 1) == 'f') {
            tk.type = Token_Type_Literal_Float;

            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        if (hit_decimal || hit_exponent) tk.type = Token_Type_Literal_Float;

        tk.length = len;

        INCREMENT_CURR_TOKEN(tokenizer);
        goto token_parsed;
    }

    if (tokenizer->curr[0] == '!' && tokenizer->curr[1] == '{') {
        INCREMENT_CURR_TOKEN(tokenizer);
        INCREMENT_CURR_TOKEN(tokenizer);

        tk.text = tokenizer->curr;

        i32 bracket_count = 0;
        i32 len = 0;
        while ((bracket_count > 0 || tokenizer->curr[0] != '}') && tokenizer->curr != tokenizer->end) {
            if (tokenizer->curr[0] == '{') bracket_count += 1;
            if (tokenizer->curr[0] == '}') bracket_count -= 1;

            len++;
            INCREMENT_CURR_TOKEN(tokenizer);
        }

        tk.type = Token_Type_Proc_Macro_Body;
        tk.length = len;

        INCREMENT_CURR_TOKEN(tokenizer);
        goto token_parsed;
    }

    char curr = *tokenizer->curr;
    switch (curr) {
    case 'a':
        LITERAL_TOKEN("as",          1, Token_Type_Keyword_As);
        LITERAL_TOKEN("alignof",     1, Token_Type_Keyword_Alignof);
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
        LITERAL_TOKEN("in",          1, Token_Type_Keyword_In);
        LITERAL_TOKEN("interface",   1, Token_Type_Keyword_Interface);
        break;
    case 'm':
        LITERAL_TOKEN("macro",       1, Token_Type_Keyword_Macro);
        break;
    case 'p':
        LITERAL_TOKEN("package",     1, Token_Type_Keyword_Package);
        break;
    case 'r':
        LITERAL_TOKEN("return",      1, Token_Type_Keyword_Return);
        break;
    case 's':
        LITERAL_TOKEN("struct",      1, Token_Type_Keyword_Struct);
        LITERAL_TOKEN("sizeof",      1, Token_Type_Keyword_Sizeof);
        LITERAL_TOKEN("switch",      1, Token_Type_Keyword_Switch);
        break;
    case 't':
        LITERAL_TOKEN("true",        1, Token_Type_Literal_True);
        LITERAL_TOKEN("typeof",      1, Token_Type_Keyword_Typeof);
        break;
    case 'u':
        LITERAL_TOKEN("use",         1, Token_Type_Keyword_Use);
        LITERAL_TOKEN("union",       1, Token_Type_Keyword_Union);
        break;
    case 'w':
        LITERAL_TOKEN("while",       1, Token_Type_Keyword_While);
        LITERAL_TOKEN("where",       1, Token_Type_Keyword_Where);
        break;

    case '-':
        LITERAL_TOKEN("->",          0, Token_Type_Right_Arrow);
        LITERAL_TOKEN("---",         0, Token_Type_Empty_Block);
        LITERAL_TOKEN("-=",          0, Token_Type_Minus_Equal);
        break;

    case '<':
        LITERAL_TOKEN("<=",          0, Token_Type_Less_Equal);
        LITERAL_TOKEN("<-",          0, Token_Type_Left_Arrow);
        LITERAL_TOKEN("<<=",         0, Token_Type_Shl_Equal);
        LITERAL_TOKEN("<<",          0, Token_Type_Shift_Left);
        break;

    case '>':
        LITERAL_TOKEN(">=",          0, Token_Type_Greater_Equal);
        LITERAL_TOKEN(">>>=",        0, Token_Type_Sar_Equal);
        LITERAL_TOKEN(">>=",         0, Token_Type_Shr_Equal);
        LITERAL_TOKEN(">>>",         0, Token_Type_Shift_Arith_Right);
        LITERAL_TOKEN(">>",          0, Token_Type_Shift_Right);
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

    case '=':
        LITERAL_TOKEN("==",          0, Token_Type_Equal_Equal);
        LITERAL_TOKEN("=>",          0, Token_Type_Fat_Right_Arrow);
        break;

    case '!':
        LITERAL_TOKEN("!=",          0, Token_Type_Not_Equal);
        break;

    case '+':
        LITERAL_TOKEN("+=",          0, Token_Type_Plus_Equal);
        break;

    case '*':
        LITERAL_TOKEN("*=",          0, Token_Type_Star_Equal);
        break;

    case '^':
        LITERAL_TOKEN("^=",          0, Token_Type_Xor_Equal);
        break;

    case '/':
        LITERAL_TOKEN("/=",          0, Token_Type_Fslash_Equal);
        break;

    case '%':
        LITERAL_TOKEN("%=",          0, Token_Type_Percent_Equal);
        break;

    case '.':
        LITERAL_TOKEN("..=",         0, Token_Type_Dot_Dot_Equal);
        LITERAL_TOKEN("..",          0, Token_Type_Dot_Dot);
        break;

    case '~':
        LITERAL_TOKEN("~~",          0, Token_Type_Tilde_Tilde);
        break;

    case '?':
        LITERAL_TOKEN("??",          0, Token_Type_Question_Question);
        break;
    }

    // Symbols
    if (char_is_alpha(*tk.text) || *tokenizer->curr == '_') {
        u64 len = 0;
        while (char_is_alphanum(*tokenizer->curr) || *tokenizer->curr == '_') {
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

    switch ((u32) tk.type) {
        case Token_Type_Comment:
            break;

        case Token_Type_Symbol:
        case Token_Type_Keyword_Break:
        case Token_Type_Keyword_Continue:
        case Token_Type_Keyword_Fallthrough:
        case Token_Type_Keyword_Return:
        case Token_Type_Literal_String:
        case Token_Type_Literal_True:
        case Token_Type_Literal_False:
        case Token_Type_Literal_Integer:
        case Token_Type_Literal_Float:
        case Token_Type_Literal_Char:
        case Token_Type_Empty_Block:
        case Token_Type_Proc_Macro_Body:
        case '?':
        case '!':
        case ')':
        case '}':
        case ']':
            tokenizer->insert_semicolon = 1;
            break;

        default:
            tokenizer->insert_semicolon = 0;
    }

    return &tokenizer->tokens[bh_arr_length(tokenizer->tokens) - 1];
}

OnyxTokenizer onyx_tokenizer_create(Context *context, bh_file_contents *fc) {
    OnyxTokenizer tknizer = {
        .context = context,

        .start          = fc->data,
        .curr           = fc->data,
        .end            = bh_pointer_add(fc->data, fc->length),

        .filename       = fc->filename,

        .line_number    = 1,
        .line_start     = fc->data,
        .tokens         = NULL,

        .optional_semicolons = context->options->enable_optional_semicolons,
        .insert_semicolon = 0,
    };

    bh_arr_new(context->token_alloc, tknizer.tokens, 1 << 12);
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

    tokenizer->context->stats.lexer_lines_processed += tokenizer->line_number - 1;
    tokenizer->context->stats.lexer_tokens_processed += bh_arr_length(tokenizer->tokens);
}

b32 token_equals(OnyxToken* tkn1, OnyxToken* tkn2) {
    if (tkn1->length != tkn2->length) return 0;
    fori (i, 0, tkn1->length)
        if (tkn1->text[i] != tkn2->text[i]) return 0;
    return 1;
}

b32 token_text_equals(OnyxToken* tkn, char* text) {
    i32 text_len = strlen(text);
    if (tkn->length != text_len) return 0;

    fori (i, 0, tkn->length)
        if (tkn->text[i] != text[i]) return 0;
        
    return 1;
}

b32 token_same_file(OnyxToken *tkn1, OnyxToken *tkn2) {
    if (!tkn1 || !tkn2) return 0;
    
    if (tkn1->pos.filename == tkn2->pos.filename) return 1;

    // :Security?
    return strcmp(tkn1->pos.filename, tkn2->pos.filename) == 0;
}
