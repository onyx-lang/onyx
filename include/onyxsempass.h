#ifndef ONYXSEMPASS_H
#define ONYXSEMPASS_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxparser.h"
#include "onyxmsgs.h"

typedef struct SemPassSymbol {
    OnyxAstNode *node;
    struct SemPassSymbol *shadowed;
} SemPassSymbol;

typedef struct OnyxSemPassState {
    // NOTE: Adding node_allocator in case we need
    // to make any more node in the tree
	bh_allocator allocator, node_allocator;
	OnyxMessages *msgs;

    OnyxAstNodeScope* curr_scope;

    bh_table(SemPassSymbol *) symbols;
} OnyxSemPassState;

OnyxSemPassState onyx_sempass_create(bh_allocator alloc, bh_allocator node_alloc, OnyxMessages* msgs);
void onyx_sempass(OnyxSemPassState* state, OnyxAstNode* root_node);

#endif
