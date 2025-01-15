#include "bh.h"
#include "astnodes.h"
#include "utils.h"

static inline i32 entity_phase(Entity* e1) {
    if (e1->state <= Entity_State_Parse && e1->macro_attempts == 0) return 1;
    if (e1->state <  Entity_State_Code_Gen) return 2;
    return 3;
}

// NOTE: Returns >0 if e1 should be processed after e2.
static i32 entity_compare(Entity* e1, Entity* e2) {
    i32 phase1 = entity_phase(e1);
    i32 phase2 = entity_phase(e2);

    if (phase1 != phase2)
        return phase1 - phase2;
    else if (e1->macro_attempts != e2->macro_attempts)
        return (i32) e1->macro_attempts - (i32) e2->macro_attempts;
    else if (e1->state != e2->state)
        return (i32) e1->state - (i32) e2->state;
    else if (e1->type != e2->type)
        return (i32) e1->type - (i32) e2->type;
    else if (e1->micro_attempts != e2->micro_attempts)
        return (i32) (e1->micro_attempts - e2->micro_attempts);
    else
        return (i32) (e1->id - e2->id);
}

#define eh_parent(index) (((index) - 1) / 2)
#define eh_lchild(index) (((index) * 2) + 1)
#define eh_rchild(index) (((index) * 2) + 2)

static void eh_shift_up(EntityHeap* entities, i32 index) {
    while (index > 0 && entity_compare(entities->entities[eh_parent(index)], entities->entities[index]) > 0) {
        Entity* tmp = entities->entities[eh_parent(index)];
        entities->entities[eh_parent(index)] = entities->entities[index];
        entities->entities[index] = tmp;

        index = eh_parent(index);
    }
}

static void eh_shift_down(EntityHeap* entities, i32 index) {
    while (1) {
        i32 min_index = index;

        i32 l = eh_lchild(index);
        if (l < bh_arr_length(entities->entities)
            && entity_compare(entities->entities[l], entities->entities[min_index]) < 0) {
            min_index = l;
        }

        i32 r = eh_rchild(index);
        if (r < bh_arr_length(entities->entities)
            && entity_compare(entities->entities[r], entities->entities[min_index]) < 0) {
            min_index = r;
        }

        if (index != min_index) {
            Entity* tmp = entities->entities[min_index];
            entities->entities[min_index] = entities->entities[index];
            entities->entities[index] = tmp;

            index = min_index;
            continue;
        }

        break;
    }
}

void entity_heap_init(bh_allocator a, EntityHeap* entities) {
    memset(entities, 0, sizeof(*entities));
    entities->allocator = a;

    bh_arena_init(&entities->entity_arena, a, 32 * 1024);
    bh_arr_new(a, entities->entities, 128);
    bh_arr_new(a, entities->quick_unsorted_entities, 128);
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

    if (e->state <= Entity_State_Introduce_Symbols) {
        bh_arr_push(entities->quick_unsorted_entities, e);
    } else {
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
    if (bh_arr_length(entities->quick_unsorted_entities) > 0) {
        return entities->quick_unsorted_entities[0];
    }

    return entities->entities[0];
}

void entity_heap_change_top(EntityHeap* entities, Entity* new_top) {
    entities->state_count[entities->entities[0]->state]--;
    entities->state_count[new_top->state]++;

    entities->type_count[entities->entities[0]->type]--;
    entities->type_count[new_top->type]++;

    entities->all_count[entities->entities[0]->state][entities->entities[0]->type]--;
    entities->all_count[new_top->state][new_top->type]++;

    entities->entities[0] = new_top;
    eh_shift_down(entities, 0);
}

void entity_heap_remove_top(EntityHeap* entities) {
    Entity *e;

    if (bh_arr_length(entities->quick_unsorted_entities) > 0) {
        e = entities->quick_unsorted_entities[0];
    } else {
        e = entities->entities[0];
    }

    entities->state_count[e->state]--;
    entities->type_count[e->type]--;
    entities->all_count[e->state][e->type]--;
    e->entered_in_queue = 0;

    if (bh_arr_length(entities->quick_unsorted_entities) > 0) {
        bh_arr_fastdelete(entities->quick_unsorted_entities, 0);
    } else {
        entities->entities[0] = entities->entities[bh_arr_length(entities->entities) - 1];
        bh_arr_pop(entities->entities);
        eh_shift_down(entities, 0);
    }
}

void entity_change_type(EntityHeap* entities, Entity *ent, EntityType new_type) {
    entities->type_count[ent->type]--;
    entities->type_count[new_type]++;
    ent->type = new_type;
}

void entity_change_state(EntityHeap* entities, Entity *ent, EntityState new_state) {
    entities->state_count[ent->state]--;
    entities->state_count[new_state]++;
    ent->state = new_state;
}

void entity_heap_add_job(EntityHeap *entities, TypeMatch (*func)(Context *, void *), void *job_data) {
    EntityJobData *job = bh_alloc(entities->allocator, sizeof(*job));
    job->func = func;
    job->job_data = job_data;
    
    Entity ent;
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
        case Ast_Kind_Load_All:
        case Ast_Kind_Load_File: {
            ent.type = Entity_Type_Load_File;
            ent.include = (AstInclude *) node;
            ENTITY_INSERT(ent);
            break;
        }

        case Ast_Kind_Library_Path:
        case Ast_Kind_Load_Path: {
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
