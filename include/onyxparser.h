#ifndef ONYXPARSER_H
#define ONYXPARSER_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxastnodes.h"

typedef struct ParseResults {
    // NOTE: The allocator used to make the arrays below
    bh_allocator allocator;

    bh_arr(AstUse *) uses;
    bh_arr(AstBinding *) bindings;

    // NOTE: Contains all the nodes that will need some processing (symbol resolution, type checking)
    bh_arr(AstNode *) nodes_to_process;
} ParseResults;

typedef struct OnyxParser {
    bh_allocator allocator;

    // NOTE: not used since all tokens are lexed before parsing starts
    OnyxTokenizer *tokenizer;
    OnyxToken *prev_token;
    OnyxToken *curr_token;

    OnyxMessages *msgs;

    ParseResults results;
} OnyxParser;

const char* onyx_ast_node_kind_string(AstKind kind);
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs);
void onyx_parser_free(OnyxParser* parser);
ParseResults onyx_parse(OnyxParser *parser);

#endif // #ifndef ONYXPARSER_H
