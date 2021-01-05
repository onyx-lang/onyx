#ifndef ONYXSEMPASS_H
#define ONYXSEMPASS_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxerrors.h"

typedef struct SemState {
    // NOTE: Adding node_allocator in case we need
    // to make any more node in the tree
    bh_allocator allocator, node_allocator;

    // NOTE: Used wherever
    ProgramInfo* program;

    // NOTE: Used in symbol resolution phase
    Package*           curr_package;
    Scope*             global_scope;
    Scope*             curr_scope;
    AstFunction*       curr_function;
    bh_arr(AstBlock *) block_stack;

    // NOTE: Used in type checking phase
    Type* expected_return_type;

    u32 defer_allowed : 1;
} SemState;

extern SemState semstate;

AstType* symres_type(AstType* type);

// TODO: This should be moved elsewhere.
void onyx_sempass_init(bh_allocator alloc, bh_allocator node_alloc);

#endif
