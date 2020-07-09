#ifndef ONYXLEX_H
#define ONYXLEX_H

#include "bh.h"

typedef enum TokenType {
    TOKEN_TYPE_ASCII_END            = 256,
    TOKEN_TYPE_UNKNOWN              = 256,
    TOKEN_TYPE_END_STREAM           = 257,

    TOKEN_TYPE_COMMENT              = 258,

    TOKEN_TYPE_KEYWORD_STRUCT       = 259,
    TOKEN_TYPE_KEYWORD_USE          = 260,
    TOKEN_TYPE_KEYWORD_EXPORT       = 261,
    TOKEN_TYPE_KEYWORD_IF           = 262,
    TOKEN_TYPE_KEYWORD_ELSE         = 263,
    TOKEN_TYPE_KEYWORD_ELSEIF       = 264,
    TOKEN_TYPE_KEYWORD_RETURN       = 265,
    TOKEN_TYPE_KEYWORD_FOREIGN      = 266,
    TOKEN_TYPE_KEYWORD_PROC         = 267,
    TOKEN_TYPE_KEYWORD_CAST         = 268,
    TOKEN_TYPE_KEYWORD_WHILE        = 269,
    TOKEN_TYPE_KEYWORD_BREAK        = 270,
    TOKEN_TYPE_KEYWORD_CONTINUE     = 271,

    TOKEN_TYPE_RIGHT_ARROW          = 272,
    TOKEN_TYPE_LEFT_ARROW           = 273,

    TOKEN_TYPE_GREATER_EQUAL        = 274,
    TOKEN_TYPE_LESS_EQUAL           = 275,
    TOKEN_TYPE_EQUAL_EQUAL          = 276,
    TOKEN_TYPE_NOT_EQUAL            = 277,
    TOKEN_TYPE_PLUS_EQUAL           = 278,
    TOKEN_TYPE_MINUS_EQUAL          = 279,
    TOKEN_TYPE_STAR_EQUAL           = 280,
    TOKEN_TYPE_FSLASH_EQUAL         = 281,
    TOKEN_TYPE_PERCENT_EQUAL        = 282,

    TOKEN_TYPE_SYMBOL               = 283,
    TOKEN_TYPE_LITERAL_STRING       = 284,
    TOKEN_TYPE_LITERAL_NUMERIC      = 285,
    TOKEN_TYPE_LITERAL_BOOL_TRUE    = 286,
    TOKEN_TYPE_LITERAL_BOOL_FALSE   = 287,

    TOKEN_TYPE_COUNT                = 288
} TokenType;

typedef struct OnyxFilePos {
    const char* filename;
    char* line_start;
    u32 line, column;
} OnyxFilePos;

typedef struct OnyxToken {
    TokenType type;
    i32 length;
    char* text;
    OnyxFilePos pos;
} OnyxToken;

typedef struct OnyxTokenizer {
    char *start, *curr, *end;

    const char* filename;

    char* line_start;
    u64 line_number;

    bh_arr(OnyxToken) tokens;
} OnyxTokenizer;

const char* onyx_get_token_type_name(TokenType tkn_type);
void onyx_token_null_toggle(OnyxToken* tkn);
OnyxToken* onyx_get_token(OnyxTokenizer* tokenizer);
OnyxTokenizer onyx_tokenizer_create(bh_allocator allocator, bh_file_contents *fc);
void onyx_tokenizer_free(OnyxTokenizer* tokenizer);
void onyx_lex_tokens(OnyxTokenizer* tokenizer);

#endif
