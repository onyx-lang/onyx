#ifndef ONYXPARSER_H
#define ONYXPARSER_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxastnodes.h"

typedef struct OnyxParser {
    OnyxTokenizer *tokenizer; // NOTE: not used since all tokens are lexed before parsing starts
    OnyxToken *prev_token;
    OnyxToken *curr_token;

    // NOTE: Identifiers currently is only used to resolve type names
    // at parse time, since these are the only symbols we know.
    bh_table(AstNode *) identifiers;
    OnyxMessages *msgs;

    bh_allocator allocator;
} OnyxParser;

const char* onyx_ast_node_kind_string(AstNodeKind kind);
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstNodeKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs);
void onyx_parser_free(OnyxParser* parser);
bh_arr(AstNode *) onyx_parse(OnyxParser *parser);

#endif // #ifndef ONYXPARSER_H
