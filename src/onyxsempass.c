// TODO: This file has become useless. It should be removed soon.

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

        .block_stack = NULL,

        .defer_allowed = 1,
    };

    bh_arr_new(global_heap_allocator, semstate.block_stack, 4);
}
