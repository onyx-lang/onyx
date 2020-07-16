#ifndef ONYXSEMPASS_H
#define ONYXSEMPASS_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxmsgs.h"

typedef struct SemPassSymbol {
    AstNode *node;
    struct SemPassSymbol *shadowed;
} SemPassSymbol;

typedef struct OnyxSemPassState {
    // NOTE: Adding node_allocator in case we need
    // to make any more node in the tree
    bh_allocator allocator, node_allocator;
    OnyxMessages *msgs;

    // NOTE: Used in symbol resolution phase
    AstLocalGroup* curr_local_group;

    // NOTE: Used in type checking phase
    Type* expected_return_type;

    // NOTE: All symbols a given point that we can resolve
    bh_table(SemPassSymbol *) symbols;
} OnyxSemPassState;

// NOTE: Resolving all symbols in the tree
void onyx_resolve_symbols(OnyxSemPassState* state, ParserOutput* program);

// NOTE: Inferring and checking types in the tree
void onyx_type_check(OnyxSemPassState* state, ParserOutput* program);

// NOTE: Full semantic pass
OnyxSemPassState onyx_sempass_create(bh_allocator alloc, bh_allocator node_alloc, OnyxMessages* msgs);
void onyx_sempass(OnyxSemPassState* state, ParserOutput* program);

#endif
