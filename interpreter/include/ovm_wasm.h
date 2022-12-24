#ifndef _OVM_WASM_H
#define _OVM_WASM_H

#include "wasm.h"
#include "vm.h"
#include "ovm_debug.h"

// Core Utils

struct wasm_config_t {
    bool debug_enabled;
    char *listen_path;
};

void wasm_config_enable_debug(wasm_config_t *config, bool enabled);
void wasm_config_set_listen_path(wasm_config_t *config, char *listen_path);

struct wasm_engine_t {
    wasm_config_t *config;

    ovm_store_t  *store;
    ovm_engine_t *engine;
};

struct wasm_store_t {
    wasm_engine_t   *engine;
    wasm_instance_t *instance;
};


// Types

struct wasm_valtype_t {
    wasm_valkind_t kind;
};

struct wasm_functype_inner_t {
    wasm_valtype_vec_t params;
    wasm_valtype_vec_t results;
};

struct wasm_globaltype_inner_t {
    wasm_valtype_t *content;
    wasm_mutability_t mutability; 

    wasm_val_t initial_value;
};

struct wasm_tabletype_inner_t {
    wasm_valtype_t *element;
    wasm_limits_t   limits;

    i32 static_arr;
};

struct wasm_memorytype_inner_t {
    wasm_limits_t   limits;
};

struct wasm_externtype_t {
    wasm_externkind_t kind;
    union {
        struct wasm_functype_inner_t   func;
        struct wasm_globaltype_inner_t global;
        struct wasm_tabletype_inner_t  table;
        struct wasm_memorytype_inner_t memory;
    };
};

struct wasm_functype_t { wasm_externtype_t type; };
struct wasm_globaltype_t { wasm_externtype_t type; };
struct wasm_tabletype_t { wasm_externtype_t type; };
struct wasm_memorytype_t { wasm_externtype_t type; };

struct wasm_importtype_t {
    wasm_name_t module_name;
    wasm_name_t import_name;
    wasm_externtype_t *type;

    //
    // This is only used by imported functions
    // to specify which slot the function binding
    // should be placed. When making a functype by
    // hand, this proably will never be used.
    int external_func_idx;
};

struct wasm_exporttype_t {
    wasm_name_t name;
    wasm_externtype_t *type;
    int index;
};


// Runtime Objects

struct wasm_ref_t {
};

struct wasm_frame_t {
    wasm_instance_t *instance;
    int func_idx;
    size_t func_offset;
    size_t module_offset;
};

struct wasm_trap_t {
    wasm_message_t msg;
    wasm_frame_vec_t frames;

    wasm_store_t *store;
};

struct wasm_foreign_t {
};

struct wasm_data_t {
    void *data;
    unsigned int length;
    unsigned int offset;
    bool passive;
};

struct wasm_custom_section_t {
    unsigned int size;
    char        *data;
};

struct wasm_module_t {
    wasm_store_t *store;

    wasm_functype_vec_t type_section;

    wasm_functype_vec_t functypes;
    wasm_globaltype_vec_t globaltypes;
    wasm_tabletype_vec_t tabletypes;
    wasm_memorytype_vec_t memorytypes;
    wasm_importtype_vec_t imports;
    wasm_exporttype_vec_t exports;

    int start_func_idx;

    unsigned int elem_count;
    unsigned int *elem_entries; // Array of function indicies

    bool data_count_present;
    unsigned int data_count;
    struct wasm_data_t *data_entries;

    ovm_program_t *program;
    bool valid;
    
    int memory_init_idx;
    int memory_init_external_idx;

    Table(struct wasm_custom_section_t) custom_sections;

    debug_info_t debug_info;
};

struct wasm_func_inner_t {
    bool env_present;
    void *env;
    void (*func_ptr)();
    void (*finalizer)(void *);

    const wasm_functype_t *type;
};

struct wasm_global_inner_t {
    int register_index;
    ovm_state_t  *state;
    ovm_engine_t *engine;

    wasm_val_t initial_value;

    const wasm_globaltype_t *type;
};

struct wasm_table_inner_t {
    ovm_program_t *program;
    ovm_engine_t  *engine;

    int static_arr;

    const wasm_tabletype_t *type;
};

struct wasm_memory_inner_t {
    ovm_engine_t* engine;

    const wasm_memorytype_t *type;
};

struct wasm_extern_t {
    const wasm_externtype_t *type;
    union {
        struct wasm_func_inner_t   func;
        struct wasm_global_inner_t global;
        struct wasm_table_inner_t  table;
        struct wasm_memory_inner_t memory;
    };
};

struct wasm_func_t { wasm_extern_t inner; };
struct wasm_global_t { wasm_extern_t inner; };
struct wasm_table_t { wasm_extern_t inner; };
struct wasm_memory_t { wasm_extern_t inner; };

struct wasm_instance_t {
    const wasm_module_t *module;
    wasm_store_t  *store;

    bh_arr(wasm_func_t *)   funcs;
    bh_arr(wasm_memory_t *) memories;
    bh_arr(wasm_table_t *)  tables;
    bh_arr(wasm_global_t *) globals;

    wasm_extern_vec_t exports;

    ovm_state_t *state;
};


bool wasm_functype_equals(wasm_functype_t *a, wasm_functype_t *b);

wasm_functype_t   *wasm_module_index_functype(wasm_module_t *module, int index);
wasm_tabletype_t  *wasm_module_index_tabletype(wasm_module_t *module, int index);
wasm_globaltype_t *wasm_module_index_globaltype(wasm_module_t *module, int index);
wasm_memorytype_t *wasm_module_index_memorytype(wasm_module_t *module, int index);



#define WASM_DECLARE_VEC_IMPL(type, ptr_or_none) \
    void wasm_##type##_vec_new_empty(wasm_##type##_vec_t *out) { \
        out->size = 0; \
        out->data = NULL; \
    } \
     \
    void wasm_##type##_vec_new_uninitialized(wasm_##type##_vec_t *out, size_t size) { \
        out->data = malloc(sizeof(wasm_##type##_t ptr_or_none) * size); \
        out->size = size; \
    } \
     \
    void wasm_##type##_vec_new(wasm_##type##_vec_t *out, size_t size, wasm_##type##_t ptr_or_none const data[]) { \
        out->data = malloc(sizeof(wasm_##type##_t ptr_or_none) * size); \
        out->size = size; \
     \
        fori (i, 0, (i32) size) { \
            out->data[i] = data[i]; \
        } \
    } \
     \
    void wasm_##type##_vec_copy(wasm_##type##_vec_t *out, const wasm_##type##_vec_t *in) { \
        wasm_##type##_vec_new(out, in->size, in->data); \
    } \
     \
    void wasm_##type##_vec_delete(wasm_##type##_vec_t *vec) { \
        if (vec->data) free(vec->data); \
    }

#endif
