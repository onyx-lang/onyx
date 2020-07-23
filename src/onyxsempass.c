#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxutils.h"

SemState semstate;

void onyx_sempass_init(bh_allocator alloc, bh_allocator node_alloc) {
    semstate = (SemState) {
        .allocator = alloc,
        .node_allocator = node_alloc,

        .global_scope = NULL,
        .curr_scope = NULL,
    };
}

void onyx_sempass(ProgramInfo* program) {
    semstate.program = program;

    onyx_resolve_symbols(program);
    if (onyx_message_has_errors()) return;

    onyx_type_check(program);
    if (onyx_message_has_errors()) return;
}
