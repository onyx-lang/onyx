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
        .other_entities = NULL,

        .defer_allowed = 1,
    };

    bh_arr_new(global_heap_allocator, semstate.block_stack, 4);
    bh_arr_new(global_heap_allocator, semstate.other_entities, 4);
}

void onyx_sempass(ProgramInfo* program) {
    semstate.program = program;

    onyx_resolve_symbols(program);
    if (onyx_has_errors()) return;

    onyx_type_check(program);

    if (bh_arr_length(semstate.other_entities) > 0) {
        bh_arr_each(Entity, e, semstate.other_entities)
            bh_arr_push(semstate.program->entities, *e);

        qsort(semstate.program->entities, bh_arr_length(semstate.program->entities), sizeof(Entity), sort_entities);
        bh_arr_clear(semstate.other_entities);
    }
    
    if (onyx_has_errors()) return;
}
