#ifndef ONYXPARSER_H
#define ONYXPARSER_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxerrors.h"
#include "onyxastnodes.h"

typedef struct NodeToProcess {
    Scope *scope;
    Package *package;
    AstNode *node;
} NodeToProcess;

typedef struct ParseResults {
    // NOTE: The allocator used to make the arrays below
    bh_allocator allocator;

    // NOTE: Contains all the nodes that will need some processing (symbol resolution, type checking)
    bh_arr(NodeToProcess) nodes_to_process;
} ParseResults;

typedef struct PolymorphicContext {
    AstType* root_node;
    bh_arr(AstPolyParam)* poly_params;
} PolymorphicContext;

typedef struct OnyxParser {
    bh_allocator allocator;

    ProgramInfo *program;
    Package *package;
    Scope *file_scope;

    // NOTE: not used since all tokens are lexed before parsing starts
    OnyxTokenizer *tokenizer;
    OnyxToken *prev;
    OnyxToken *curr;

    ParseResults results;

    PolymorphicContext polymorph_context;

    bh_arr(AstBlock *) block_stack;

    b32 hit_unexpected_token : 1;
} OnyxParser;

const char* onyx_ast_node_kind_string(AstKind kind);
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, ProgramInfo *program);
void onyx_parser_free(OnyxParser* parser);
ParseResults onyx_parse(OnyxParser *parser);

#endif // #ifndef ONYXPARSER_H
