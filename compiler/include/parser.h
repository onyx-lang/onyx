#ifndef ONYXPARSER_H
#define ONYXPARSER_H

#include "bh.h"

#include "lex.h"
#include "errors.h"
#include "astnodes.h"

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

    bh_arr(AstFunction *) current_function_stack;
    Scope *current_scope;
    bh_arr(bh_arr(Entity *) *) alternate_entity_placement_stack;
    bh_arr(OnyxToken *) current_symbol_stack;

    bh_arr(AstFlags) scope_flags;

    bh_arr(AstTyped *) stored_tags;

    // NOTE: When inside of an #inject block, this is set to the node
    // that should have the symbols injected onto it. If this is non-
    // NULL, then top-level binding nodes are converted into injects
    // onto this node.
    AstTyped *injection_point;

    // Used to set default precedence of #overload options.
    // This way, the precedence order of multiple #overload
    // options in the same file is given to be the lexical
    // order. This does not make any guarentees about #overloads
    // in other files.
    u32 overload_count;

    // Used by `#doc` directives to store their documentation
    // string. This is then used by binding nodes to capture
    // documentation.
    OnyxToken *last_documentation_token;

    u16 tag_depth : 16;

    b32 hit_unexpected_token : 1;
    b32 parse_calls : 1;
} OnyxParser;

const char* onyx_ast_node_kind_string(AstKind kind);
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer);
void onyx_parser_free(OnyxParser* parser);
void onyx_parse(OnyxParser *parser);

#endif // #ifndef ONYXPARSER_H
