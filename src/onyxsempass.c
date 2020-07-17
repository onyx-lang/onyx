#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxutils.h"

SemState onyx_sempass_create(bh_allocator alloc, bh_allocator node_alloc, OnyxMessages* msgs) {
    SemState state = {
        .allocator = alloc,
        .node_allocator = node_alloc,

        .msgs = msgs,

        .curr_local_group = NULL,
        .symbols = NULL,
    };

    bh_table_init(global_heap_allocator, state.symbols, 61);

    return state;
}

void onyx_sempass(SemState* state, ParserOutput* program) {
    onyx_resolve_symbols(state, program);
    if (onyx_message_has_errors(state->msgs)) return;

    onyx_type_check(state, program);
    if (onyx_message_has_errors(state->msgs)) return;
}
