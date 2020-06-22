#define BH_DEBUG
#include "onyxsempass.h"

OnyxSemPassState onyx_sempass_create(bh_allocator alloc, bh_allocator node_alloc, OnyxMessages* msgs) {
    OnyxSemPassState state = {
        .allocator = alloc,
        .node_allocator = node_alloc,

        .msgs = msgs,

        .curr_scope = NULL,
        .symbols = NULL,
    };

    bh_table_init(bh_heap_allocator(), state.symbols, 61);

    return state;
}

void onyx_sempass(OnyxSemPassState* state, OnyxAstNode* root_node) {
    onyx_resolve_symbols(state, root_node);
    onyx_type_check(state, root_node);
}
