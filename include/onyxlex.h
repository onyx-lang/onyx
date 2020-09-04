#ifndef ONYXLEX_H
#define ONYXLEX_H

#include "bh.h"

typedef enum TokenType {
    Token_Type_Ascii_End            = 256,
    Token_Type_Unknown              = 256,
    Token_Type_End_Stream           = 257,

    Token_Type_Comment              = 258,

    Token_Type_Keyword_Package,
    Token_Type_Keyword_Struct,
    Token_Type_Keyword_Enum,
    Token_Type_Keyword_Use,
    Token_Type_Keyword_If,
    Token_Type_Keyword_Else,
    Token_Type_Keyword_Elseif,
    Token_Type_Keyword_Return,
    Token_Type_Keyword_Global,
    Token_Type_Keyword_Proc,
    Token_Type_Keyword_As,
    Token_Type_Keyword_Cast,
    Token_Type_Keyword_While,
    Token_Type_Keyword_For,
    Token_Type_Keyword_Break,
    Token_Type_Keyword_Continue,
    Token_Type_Keyword_Sizeof,
    Token_Type_Keyword_Alignof,
    Token_Type_Keyword_Defer,
    Token_Type_Keyword_Do,
    Token_Type_Keyword_Case,
    Token_Type_Keyword_Switch,
    Token_Type_Keyword_Fallthrough,

    Token_Type_Right_Arrow,
    Token_Type_Left_Arrow,
    Token_Type_Empty_Block,
    Token_Type_Pipe,

    Token_Type_Greater_Equal,
    Token_Type_Less_Equal,
    Token_Type_Equal_Equal,
    Token_Type_Not_Equal,
    Token_Type_Plus_Equal,
    Token_Type_Minus_Equal,
    Token_Type_Star_Equal,
    Token_Type_Fslash_Equal,
    Token_Type_Percent_Equal,
    Token_Type_And_Equal,
    Token_Type_Or_Equal,
    Token_Type_Xor_Equal,
    Token_Type_And_And,
    Token_Type_Or_Or,
    Token_Type_Shift_Left,
    Token_Type_Shift_Right,
    Token_Type_Shift_Arith_Right,
    Token_Type_Shl_Equal,
    Token_Type_Shr_Equal,
    Token_Type_Sar_Equal,

    Token_Type_Dot_Dot,

    Token_Type_Symbol,
    Token_Type_Literal_String,
    Token_Type_Literal_Integer,
    Token_Type_Literal_Float,
    Token_Type_Literal_True,
    Token_Type_Literal_False,

    Token_Type_Count,
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
