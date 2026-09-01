#include "bh.h"
#include "astnodes.h"
#include "utils.h"

static inline b32 entity_uses_ready_queue(Entity* e) {
    return e->macro_attempts == 0 && e->state < Entity_State_Code_Gen;
}

static void grid_push(bh_allocator a, EntityQueueGrid* g, Entity* e) {
    i32 s = (i32) e->state;
    i32 t = (i32) e->type;
    if (!g->items[s][t]) {
        bh_arr_new(a, g->items[s][t], 16);
    }
    bh_arr_push(g->items[s][t], e);
    g->count++;

    if (s < g->hint_state || (s == g->hint_state && t < g->hint_type)) {
        g->hint_state = s;
        g->hint_type = t;
    }
}

static Entity* grid_peek(EntityQueueGrid* g) {
    if (g->count == 0) return NULL;

    for (i32 s = g->hint_state; s < Entity_State_Count; s++) {
        i32 t0 = (s == g->hint_state) ? g->hint_type : 0;
        for (i32 t = t0; t < Entity_Type_Count; t++) {
            i32 head = g->head[s][t];
            if (head < bh_arr_length(g->items[s][t])) {
                g->hint_state = s;
                g->hint_type = t;
                return g->items[s][t][head];
            }
        }
    }

    return NULL;
}

static Entity* grid_pop(EntityQueueGrid* g) {
    Entity* e = grid_peek(g);
    if (!e) return NULL;

    i32 s = g->hint_state;
    i32 t = g->hint_type;
    i32 head = ++g->head[s][t];
    g->count--;

    bh_arr(Entity *) q = g->items[s][t];
    i32 len = bh_arr_length(q);
    if (head >= 32 && head * 2 >= len) {
        i32 remain = len - head;
        if (remain > 0) memmove(q, q + head, remain * sizeof(Entity *));
        bh_arr_set_length(q, remain);
        g->head[s][t] = 0;
        head = 0;
    }

    if (head >= bh_arr_length(g->items[s][t])) {
        if (t + 1 < Entity_Type_Count) {
            g->hint_type = t + 1;
        } else {
            g->hint_state = s + 1;
            g->hint_type = 0;
        }
    }

    return e;
}

// Same order as the old multi-field compare: phase, attempts, state, type,
// micro_attempts, then id. Cached on the entity so a sift is two u64 loads.
static inline u64 entity_sort_key(Entity* e) {
    u64 phase = (e->state <= Entity_State_Parse && e->macro_attempts == 0) ? 1
              : (e->state <  Entity_State_Code_Gen) ? 2 : 3;
    return (phase << 56)
         | ((u64) e->macro_attempts << 32)
         | ((u64) e->state << 24)
         | ((u64) e->type << 16)
         | (u64) (e->micro_attempts & 0xffff);
}

static inline void entity_refresh_key(Entity* e) {
    e->queue_key = entity_sort_key(e);
}

static inline i32 entity_compare(Entity* e1, Entity* e2) {
    if (e1->queue_key < e2->queue_key) return -1;
    if (e1->queue_key > e2->queue_key) return 1;
    return (i32) e1->id - (i32) e2->id;
}

static void eh_shift_up(EntityHeap* entities, i32 index) {
    Entity **h = entities->entities;
    while (index > 0) {
        i32 parent = (index - 1) / 4;
        if (entity_compare(h[parent], h[index]) <= 0) break;
        Entity* tmp = h[parent];
        h[parent] = h[index];
        h[index] = tmp;
        index = parent;
    }
}

static void eh_shift_down(EntityHeap* entities, i32 index) {
    Entity **h = entities->entities;
    i32 n = bh_arr_length(h);
    while (1) {
        i32 first = index * 4 + 1;
        if (first >= n) break;

        i32 min_index = index;
        i32 last = first + 4;
        if (last > n) last = n;
        for (i32 c = first; c < last; c++) {
            if (entity_compare(h[c], h[min_index]) < 0) min_index = c;
        }

        if (min_index == index) break;

        Entity* tmp = h[min_index];
        h[min_index] = h[index];
        h[index] = tmp;
        index = min_index;
    }
}

static void eh_commit_detach(EntityHeap* entities) {
    if (!entities->detached_top) return;
    entities->detached_top = NULL;

    i32 n = bh_arr_length(entities->entities);
    if (n == 0) return;

    entities->entities[0] = entities->entities[n - 1];
    bh_arr_pop(entities->entities);
    if (bh_arr_length(entities->entities) > 0) {
        eh_shift_down(entities, 0);
    }
}

void entity_heap_init(bh_allocator a, EntityHeap* entities) {
    memset(entities, 0, sizeof(*entities));
    entities->allocator = a;

    bh_arena_init(&entities->entity_arena, a, 32 * 1024);
    bh_arr_new(a, entities->entities, 128);
}

b32 entity_heap_empty(EntityHeap* entities) {
    i32 heap_n = bh_arr_length(entities->entities);
    if (entities->detached_top) heap_n -= 1;
    return entities->ready.count == 0 && heap_n <= 0;
}

// Allocates the entity in the entity heap. Don't quite feel this is necessary...
Entity* entity_heap_register(EntityHeap* entities, Entity e) {
    bh_allocator alloc = bh_arena_allocator(&entities->entity_arena);
    Entity* entity = bh_alloc_item(alloc, Entity);
    *entity = e;
    entity->id = entities->next_id++;
    entity->macro_attempts = 0;
    entity->micro_attempts = 0;
    entity->entered_in_queue = 0;

    return entity;
}

void entity_heap_insert_existing(EntityHeap* entities, Entity* e) {
    if (e->entered_in_queue) return;

    if (entities->detached_top == e) {
        entities->state_count[e->state]++;
        entities->type_count[e->type]++;
        entities->all_count[e->state][e->type]++;
        e->entered_in_queue = 1;
        entity_refresh_key(e);

        if (entity_uses_ready_queue(e)) {
            eh_commit_detach(entities);
            grid_push(entities->allocator, &entities->ready, e);
        } else {
            entities->detached_top = NULL;
            eh_shift_down(entities, 0);
        }
        return;
    }

    if (entities->detached_top) eh_commit_detach(entities);

    if (entity_uses_ready_queue(e)) {
        grid_push(entities->allocator, &entities->ready, e);
    } else {
        entity_refresh_key(e);
        bh_arr_push(entities->entities, e);
        eh_shift_up(entities, bh_arr_length(entities->entities) - 1);
    }

    e->entered_in_queue = 1;

    entities->state_count[e->state]++;
    entities->type_count[e->type]++;
    entities->all_count[e->state][e->type]++;
}

Entity* entity_heap_insert(EntityHeap* entities, Entity e) {
    Entity* entity = entity_heap_register(entities, e);
    entity_heap_insert_existing(entities, entity);
    return entity;
}

Entity* entity_heap_top(EntityHeap* entities) {
    Entity* e = grid_peek(&entities->ready);
    if (e) return e;
    if (entities->detached_top) eh_commit_detach(entities);
    return entities->entities[0];
}

void entity_heap_change_top(EntityHeap* entities, Entity* new_top) {
    if (entities->detached_top) eh_commit_detach(entities);

    entities->state_count[entities->entities[0]->state]--;
    entities->state_count[new_top->state]++;

    entities->type_count[entities->entities[0]->type]--;
    entities->type_count[new_top->type]++;

    entities->all_count[entities->entities[0]->state][entities->entities[0]->type]--;
    entities->all_count[new_top->state][new_top->type]++;

    entity_refresh_key(new_top);
    entities->entities[0] = new_top;
    eh_shift_down(entities, 0);
}

void entity_heap_remove_top(EntityHeap* entities) {
    Entity *e;

    if (entities->ready.count > 0) {
        e = grid_pop(&entities->ready);
    } else {
        if (entities->detached_top) eh_commit_detach(entities);
        e = entities->entities[0];
        entities->detached_top = e;
    }

    entities->state_count[e->state]--;
    entities->type_count[e->type]--;
    entities->all_count[e->state][e->type]--;
    e->entered_in_queue = 0;
}

void entity_change_type(EntityHeap* entities, Entity *ent, EntityType new_type) {
    entities->type_count[ent->type]--;
    entities->type_count[new_type]++;
    ent->type = new_type;
    entity_refresh_key(ent);
}

void entity_change_state(EntityHeap* entities, Entity *ent, EntityState new_state) {
    entities->state_count[ent->state]--;
    entities->state_count[new_state]++;
    ent->state = new_state;
    entity_refresh_key(ent);
}

void entity_heap_add_job(EntityHeap *entities, TypeMatch (*func)(Context *, void *), void *job_data) {
    EntityJobData *job = bh_alloc(entities->allocator, sizeof(*job));
    job->func = func;
    job->job_data = job_data;
    
    Entity ent = {0};
    ent.type = Entity_Type_Job;
    ent.state = Entity_State_Check_Types;
    ent.job_data = job;

    entity_heap_insert(entities, ent);
}

// NOTE(Brendan Hansen): Uses the entity heap in the context structure
void add_entities_for_node(EntityHeap *entities, bh_arr(Entity *) *target_arr, AstNode* node, Scope* scope, Package* package) {
#define ENTITY_INSERT(_ent)                                     \
    entity = entity_heap_register(entities, _ent);              \
    if (target_arr) {                                           \
        bh_arr(Entity *) __tmp_arr = *target_arr;               \
        bh_arr_push(__tmp_arr, entity);                         \
        *target_arr = __tmp_arr;                                \
    } else {                                                    \
        entity_heap_insert_existing(entities, entity);          \
    }                                                           \

    if (node->entity != NULL) return;

    Entity* entity;

    Entity ent;
    ent.id = entities->next_id++;
    ent.state = Entity_State_Check_Types;
    ent.package = package;
    ent.scope   = scope;

    switch (node->kind) {
        case Ast_Kind_Load: {
            ent.type = Entity_Type_Load_File;
            ent.include = (AstInclude *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Library_Path: {
            ent.type = Entity_Type_Load_Path;
            ent.include = (AstInclude *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Binding: {
            ent.state   = Entity_State_Introduce_Symbols;
            ent.type    = Entity_Type_Binding;
            ent.binding = (AstBinding *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Function: {
            if (((AstFunction *) node)->is_foreign != 0) {
                ent.type     = Entity_Type_Foreign_Function_Header;
                ent.function = (AstFunction *) node;
                ENTITY_INSERT(ent);

            } else {
                ent.type     = Entity_Type_Function_Header;
                ent.function = (AstFunction *) node;
                ENTITY_INSERT(ent);
                ((AstFunction *) node)->entity_header = entity;

                ent.id       = entities->next_id++;
                ent.type     = Entity_Type_Function;
                ent.function = (AstFunction *) node;
                ENTITY_INSERT(ent);
                ((AstFunction *) node)->entity_body = entity;
            }
            break;
        }

        case Ast_Kind_Overloaded_Function: {
            ent.type                = Entity_Type_Overloaded_Function;
            ent.overloaded_function = (AstOverloadedFunction *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Global: {
            ent.type   = Entity_Type_Global_Header;
            ent.global = (AstGlobal *) node;
            ENTITY_INSERT(ent);

            ent.id     = entities->next_id++;
            ent.type   = Entity_Type_Global;
            ent.global = (AstGlobal *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_StrLit: {
            ent.type   = Entity_Type_String_Literal;
            ent.strlit = (AstStrLit *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_File_Contents: {
            ent.type = Entity_Type_File_Contents;
            ent.file_contents = (AstFileContents *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Struct_Type: {
            ent.type = Entity_Type_Struct_Member_Default;
            ent.type_alias = (AstType *) node;
            ENTITY_INSERT(ent);
            ((AstStructType *) node)->entity_defaults = entity;

            ent.id       = entities->next_id++;
            // fallthrough
        }

        case Ast_Kind_Union_Type:
        case Ast_Kind_Poly_Union_Type:
        case Ast_Kind_Poly_Struct_Type:
        case Ast_Kind_Type_Alias: {
            ent.type = Entity_Type_Type_Alias;
            ent.type_alias = (AstType *) node;
            ENTITY_INSERT(ent);

            if (node->kind == Ast_Kind_Struct_Type) {
                ((AstStructType *) node)->entity_type = entity;
            }

            break;
        }

        case Ast_Kind_Enum_Type: {
            ent.type = Entity_Type_Enum;
            ent.enum_type = (AstEnumType *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Enum_Value: {
            ent.type = Entity_Type_Enum_Value;
            ent.state = Entity_State_Check_Types;
            ent.enum_value = (AstEnumValue *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Memres: {
            ent.type = Entity_Type_Memory_Reservation_Type;
            ent.mem_res = (AstMemRes *) node;
            ENTITY_INSERT(ent);
            ((AstMemRes *) node)->type_entity = entity;

            ent.id       = entities->next_id++;
            ent.type = Entity_Type_Memory_Reservation;
            ent.mem_res = (AstMemRes *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Macro: {
            ent.type = Entity_Type_Macro;
            ent.macro = (AstMacro *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Polymorphic_Proc: {
            ent.type = Entity_Type_Polymorphic_Proc;
            ent.poly_proc = (AstFunction *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Polymorph_Query: {
            ent.type = Entity_Type_Polymorph_Query;
            ent.state = Entity_State_Check_Types;
            ent.poly_query = (AstPolyQuery *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Static_If: {
            ent.type = Entity_Type_Static_If;
            ent.static_if = (AstIf *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Directive_Error: {
            ent.state = Entity_State_Error;
            ent.type = Entity_Type_Error;
            ent.error = (AstDirectiveError *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Directive_Export:
        case Ast_Kind_Directive_Add_Overload:
        case Ast_Kind_Directive_Operator:
        case Ast_Kind_Directive_Init:
        case Ast_Kind_Directive_Library:
        case Ast_Kind_Directive_This_Package:
        case Ast_Kind_Directive_Wasm_Section:
        case Ast_Kind_Injection: {
            ent.type = Entity_Type_Process_Directive;
            ent.expr = (AstTyped *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Interface: {
            ent.type = Entity_Type_Interface;
            ent.interface = (AstInterface *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Constraint: {
            ent.type = Entity_Type_Constraint_Check;
            ent.constraint = (AstConstraint *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Foreign_Block: {
            ent.type = Entity_Type_Foreign_Block;
            ent.foreign_block = (AstForeignBlock *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Import: {
            ent.type = Entity_Type_Import;
            ent.import = (AstImport *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Js_Code: {
            ent.type = Entity_Type_JS;
            ent.js = (AstJsNode *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Compiler_Extension: {
            ent.type = Entity_Type_Compiler_Extension;
            ent.compiler_extension = (AstCompilerExtension *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Procedural_Expansion: {
            ent.type = Entity_Type_Procedural_Expansion;
            ent.proc_expansion = (AstProceduralExpansion *) node;
            ENTITY_INSERT(ent);
            break;
        }

        default: {
            ent.type = Entity_Type_Expression;
            ent.expr = (AstTyped *) node;
            ENTITY_INSERT(ent);
            break;
        }
    }

    node->entity = entity;
}
