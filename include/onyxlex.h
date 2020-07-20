#ifndef ONYXLEX_H
#define ONYXLEX_H

#include "bh.h"

typedef enum TokenType {
    Token_Type_Ascii_End            = 256,
    Token_Type_Unknown              = 256,
    Token_Type_End_Stream           = 257,

    Token_Type_Comment              = 258,

    Token_Type_Keyword_Struct       = 259,
    Token_Type_Keyword_Use          = 260,
    Token_Type_Keyword_Export       = 261,
    Token_Type_Keyword_If           = 262,
    Token_Type_Keyword_Else         = 263,
    Token_Type_Keyword_Elseif       = 264,
    Token_Type_Keyword_Return       = 265,
    Token_Type_Keyword_Global       = 266,
    Token_Type_Keyword_Proc         = 267,
    Token_Type_Keyword_Cast         = 268,
    Token_Type_Keyword_While        = 269,
    Token_Type_Keyword_For          = 270,
    Token_Type_Keyword_Break        = 271,
    Token_Type_Keyword_Continue     = 272,
    Token_Type_Keyword_Sizeof       = 273,

    Token_Type_Right_Arrow          = 274,
    Token_Type_Left_Arrow           = 275,
    Token_Type_Empty_Block          = 276,

    Token_Type_Greater_Equal        = 277,
    Token_Type_Less_Equal           = 278,
    Token_Type_Equal_Equal          = 279,
    Token_Type_Not_Equal            = 280,
    Token_Type_Plus_Equal           = 281,
    Token_Type_Minus_Equal          = 282,
    Token_Type_Star_Equal           = 283,
    Token_Type_Fslash_Equal         = 284,
    Token_Type_Percent_Equal        = 285,

    Token_Type_Symbol               = 286,
    Token_Type_Literal_String       = 287,
    Token_Type_Literal_Numeric      = 288,
    Token_Type_Literal_True         = 289,
    Token_Type_Literal_False        = 290,

    Token_Type_Count                = 291,
} TokenType;

typedef struct OnyxFilePos {
    const char* filename;
    char* line_start;
    u32 line;

    // NOTE: This assumes that no line is no longer than 2^16 chars
    u16 column, length;
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

const char* token_name(TokenType tkn_type);
void token_toggle_end(OnyxToken* tkn);
OnyxToken* onyx_get_token(OnyxTokenizer* tokenizer);
OnyxTokenizer onyx_tokenizer_create(bh_allocator allocator, bh_file_contents *fc);
void onyx_tokenizer_free(OnyxTokenizer* tokenizer);
void onyx_lex_tokens(OnyxTokenizer* tokenizer);

#endif
