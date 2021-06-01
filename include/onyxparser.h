#ifndef ONYXPARSER_H
#define ONYXPARSER_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxerrors.h"
#include "onyxastnodes.h"

typedef struct PolymorphicContext {
    AstType* root_node;
    bh_arr(AstPolyParam)* poly_params;
} PolymorphicContext;

typedef struct OnyxParser {
    bh_allocator allocator;

    Package *package;
    Scope *file_scope;

    // NOTE: not used since all tokens are lexed before parsing starts
    OnyxTokenizer *tokenizer;
    OnyxToken *prev;
    OnyxToken *curr;

    PolymorphicContext polymorph_context;

    Scope *current_scope;
    bh_arr(bh_arr(Entity *) *) alternate_entity_placement_stack;

    b32 hit_unexpected_token : 1;
} OnyxParser;

const char* onyx_ast_node_kind_string(AstKind kind);
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer);
void onyx_parser_free(OnyxParser* parser);
void onyx_parse(OnyxParser *parser);

#endif // #ifndef ONYXPARSER_H
