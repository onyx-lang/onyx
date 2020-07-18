#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxutils.h"

SemState onyx_sempass_create(bh_allocator alloc, bh_allocator node_alloc, OnyxMessages* msgs) {
    SemState state = {
        .allocator = alloc,
        .node_allocator = node_alloc,

        .global_scope = NULL,
        .curr_scope = NULL,

        .msgs = msgs,
    };

    return state;
}

void onyx_sempass(SemState* state, ProgramInfo* program) {
    onyx_resolve_symbols(state, program);
    if (onyx_message_has_errors(state->msgs)) return;

    onyx_type_check(state, program);
    if (onyx_message_has_errors(state->msgs)) return;
}
