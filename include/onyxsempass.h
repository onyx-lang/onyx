#ifndef ONYXSEMPASS_H
#define ONYXSEMPASS_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxmsgs.h"

typedef struct SemState {
    // NOTE: Adding node_allocator in case we need
    // to make any more node in the tree
    bh_allocator allocator, node_allocator;

    // NOTE: Used wherever
    ProgramInfo* program;
    bh_arr(Entity) other_entities;

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

// NOTE: Resolving all symbols in the tree
void onyx_resolve_symbols();

void symres_function(AstFunction* func);

// NOTE: Inferring and checking types in the tree
void onyx_type_check();

b32 check_expression(AstTyped** expr);
b32 check_function_header(AstFunction* func);
b32 check_function(AstFunction* func);

// NOTE: Full semantic pass
void onyx_sempass_init(bh_allocator alloc, bh_allocator node_alloc);
void onyx_sempass(ProgramInfo* program);

#endif
