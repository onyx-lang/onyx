#include "bh.h"
#include "onyxastnodes.h"
#include "onyxutils.h"

// NOTE: Returns >0 if e1 should be processed after e2.
static i32 entity_compare(Entity* e1, Entity* e2) {
    if (e1->state != e2->state)
        return (i32) e1->state - (i32) e2->state;
    else
        return (i32) e1->type - (i32) e2->type;
}

#define eh_parent(index) (((index) - 1) / 2)
#define eh_lchild(index) (((index) * 2) + 1)
#define eh_rchild(index) (((index) * 2) + 2)

static void eh_shift_up(EntityHeap* entities, i32 index) {
	while (index > 0 && entity_compare(&entities->entities[eh_parent(index)], &entities->entities[index]) > 0) {
		Entity tmp = entities->entities[eh_parent(index)];
		entities->entities[eh_parent(index)] = entities->entities[index];			
		entities->entities[index] = tmp;

		index = eh_parent(index);
	}
}

static void eh_shift_down(EntityHeap* entities, i32 index) {
	i32 min_index = index;

	i32 l = eh_lchild(index);	
	if (l < bh_arr_length(entities->entities)
		&& entity_compare(&entities->entities[l], &entities->entities[min_index]) < 0) {
		min_index = l;
	}

	i32 r = eh_rchild(index);	
	if (r < bh_arr_length(entities->entities)
		&& entity_compare(&entities->entities[r], &entities->entities[min_index]) < 0) {
		min_index = r;
	}

	if (index != min_index) {
		Entity tmp = entities->entities[min_index];
		entities->entities[min_index] = entities->entities[index];
		entities->entities[index] = tmp;

		eh_shift_down(entities, min_index);
	}
}

void entity_heap_insert(EntityHeap* entities, Entity e) {
	if (entities->entities == NULL) {
		bh_arr_new(global_heap_allocator, entities->entities, 128);
	}	

	bh_arr_push(entities->entities, e);
	eh_shift_up(entities, bh_arr_length(entities->entities) - 1);

	entities->state_count[e.state]++;
}

Entity entity_heap_top(EntityHeap* entities) {
	return entities->entities[0];
}

void entity_heap_change_top(EntityHeap* entities, Entity new_top) {
	entities->state_count[entities->entities[0].state]--;
	entities->state_count[new_top.state]--;
	
	entities->entities[0] = new_top;
	eh_shift_down(entities, 0);
}

void entity_heap_remove_top(EntityHeap* entities) {
    entities->state_count[entities->entities[0].state]--;

	entities->entities[0] = entities->entities[bh_arr_length(entities->entities) - 1];
	bh_arr_pop(entities->entities);
	eh_shift_down(entities, 0);
}
