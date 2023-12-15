#ifndef _OVM_DEBUG_H
#define _OVM_DEBUG_H

#include "bh.h"
#include <stdbool.h>

#if defined(_BH_LINUX)
    #include <semaphore.h>

    typedef sem_t semaphore;

    static inline semaphore* semaphore_create(const char *name, int oflag, mode_t mode, unsigned int value) {
        return sem_open(name, oflag, mode, value);
    }

    static inline void semaphore_wait(semaphore* sem) {
        sem_wait(sem);
    }

    static inline void semaphore_post(semaphore* sem) {
        sem_post(sem);
    }
#elif defined(_BH_DARWIN)
    #include <dispatch/dispatch.h>

    typedef dispatch_semaphore_t semaphore;

    static inline semaphore* semaphore_create(const char *name, int oflag, mode_t mode, unsigned int value) {
        semaphore* sem = bh_alloc(bh_heap_allocator(), sizeof(semaphore));
        *sem = dispatch_semaphore_create(value);
        return sem;
    }

    static inline void semaphore_wait(semaphore* sem) {
        dispatch_semaphore_wait(*sem, DISPATCH_TIME_FOREVER);
    }

    static inline void semaphore_post(semaphore* sem) {
        dispatch_semaphore_signal(*sem);
    }
#else
    #error "Unsupported platform"
#endif

typedef struct debug_loc_info_t {
    u32 file_id;
    u32 line;
    u32 symbol_scope;
} debug_loc_info_t;

typedef struct debug_func_info_t {
    u32 func_id;
    u32 file_id;
    u32 line;
    char *name;
    b32 internal;
    u32 stack_ptr_idx;

    u32 debug_op_offset;
} debug_func_info_t;

typedef struct debug_file_info_t {
    char *name;
    u32 file_id;
    u32 line_count;
    i32 line_buffer_offset;
} debug_file_info_t;

typedef enum debug_sym_loc_kind_t {
    debug_sym_loc_unknown  = 0,
    debug_sym_loc_register = 1,
    debug_sym_loc_stack    = 2,
    debug_sym_loc_global   = 3
} debug_sym_loc_kind_t;

typedef struct debug_sym_info_t {
    char *name;
    u32 sym_id;
    debug_sym_loc_kind_t loc_kind;
    u32 loc;
    u32 type;
} debug_sym_info_t;

typedef struct debug_sym_scope_t {
    bh_arr(u32) symbols;
    i32 parent; // -1 for root
} debug_sym_scope_t;

typedef enum debug_type_kind_t {
    debug_type_kind_primitive = 1,
    debug_type_kind_modifier  = 2,
    debug_type_kind_structure = 3,
    debug_type_kind_array     = 4,
    debug_type_kind_alias     = 5,
    debug_type_kind_function  = 6,
    debug_type_kind_slice     = 7,
    debug_type_kind_enum      = 8,
    debug_type_kind_union     = 9,
} debug_type_kind_t;

typedef enum debug_type_primitive_kind_t {
    debug_type_primitive_kind_void = 0,
    debug_type_primitive_kind_signed_integer = 1,
    debug_type_primitive_kind_unsigned_integer = 2,
    debug_type_primitive_kind_float = 3,
    debug_type_primitive_kind_boolean = 4,
    debug_type_primitive_kind_character = 5,
    debug_type_primitive_kind_vector = 6,
} debug_primitive_kind_t;

typedef struct debug_type_primitive_t {
    debug_primitive_kind_t primitive_kind;
} debug_type_primitive_t;

typedef enum debug_type_modifier_kind_t {
    debug_type_modifier_kind_pointer  = 1,
    debug_type_modifier_kind_const    = 2,
    debug_type_modifier_kind_restrict = 3,
} debug_type_modifier_kind_t;

typedef struct debug_type_modifier_t {
    debug_type_modifier_kind_t modifier_kind;
    u32 modified_type;
} debug_type_modifier_t;

typedef struct debug_type_structure_member_t {
    u32 offset;
    u32 type;
    char *name;
} debug_type_structure_member_t;

typedef struct debug_type_structure_t {
    b32 simple;
    u32 member_count;
    debug_type_structure_member_t *members;
} debug_type_structure_t;

typedef struct debug_type_array_t {
    u32 count;
    u32 type;
} debug_type_array_t;

typedef enum debug_type_alias_kind_t {
    debug_type_alias_kind_transparent = 1,
    debug_type_alias_kind_distinct = 2
} debug_type_alias_kind_t;

typedef struct debug_type_alias_t {
    debug_type_alias_kind_t alias_kind;
    u32                     aliased_type;
} debug_type_alias_t;

typedef struct debug_type_function_t {
    u32  param_count;
    u32 *param_types;
    u32  return_type;
} debug_type_function_t;

typedef struct debug_type_slice_t {
    u32 type;
} debug_type_slice_t;

typedef struct debug_type_enum_value_t {
    u64 value;
    char *name;
} debug_type_enum_value_t;

typedef struct debug_type_enum_t {
    u32 backing_type;
    u32 value_count;
    debug_type_enum_value_t *values;
} debug_type_enum_t;

typedef struct debug_type_union_variant_t {
    char *name;
    u32 type;
} debug_type_union_variant_t;

typedef struct debug_type_union_t {
    u32 tag_size;
    u32 variant_count;
    debug_type_union_variant_t *variants;
} debug_type_union_t;

typedef struct debug_type_info_t {
    u32 id;
    char *name;
    u32 size;
    debug_type_kind_t kind;

    union {
        debug_type_primitive_t primitive;
        debug_type_modifier_t  modifier;
        debug_type_structure_t structure;
        debug_type_array_t     array;
        debug_type_alias_t     alias;
        debug_type_function_t  function;
        debug_type_slice_t     slice;
        debug_type_enum_t      enumeration;
        debug_type_union_t     onion;
    };
} debug_type_info_t;

typedef struct debug_info_t {
    bh_allocator alloc;

    bool has_debug_info;

    // func index -> func info
    bh_arr(debug_func_info_t) funcs;

    // reducer output -> line info
    bh_arr(debug_loc_info_t) line_info;

    // instruction index -> reducer output
    bh_arr(u32) instruction_reducer;

    // line index -> instruction index
    bh_arr(u32) line_to_instruction;

    // file_id -> file info
    bh_arr(debug_file_info_t) files;

    // sym_id -> sym info
    bh_arr(debug_sym_info_t) symbols;

    // scope id  -> symbol scope
    bh_arr(debug_sym_scope_t) symbol_scopes;

    // type id -> type info
    bh_arr(debug_type_info_t) types;
} debug_info_t;

void debug_info_init(debug_info_t *);
void debug_info_free(debug_info_t *);
void debug_info_import_file_info(debug_info_t *, u8 *data, u32 len);
void debug_info_import_func_info(debug_info_t *, u8 *data, u32 len);
void debug_info_import_sym_info(debug_info_t *, u8 *data, u32 len);
void debug_info_import_type_info(debug_info_t *, u8 *data, u32 len);

bool debug_info_lookup_location(debug_info_t *info, u32 instruction, debug_loc_info_t *out);
bool debug_info_lookup_file(debug_info_t *info, u32 file_id, debug_file_info_t *out);
bool debug_info_lookup_file_by_name(debug_info_t *info, char *name, debug_file_info_t *out);
bool debug_info_lookup_func(debug_info_t *info, u32 func_id, debug_func_info_t *out);
i32  debug_info_lookup_instr_by_file_line(debug_info_t *info, char *filename, u32 line);

char *debug_info_type_enum_find_name(debug_info_t *info, u32 enum_type, u64 value);

//
// This builder is used in conjunction with code builder to output
// debug information for each instruction that is generated in OVM.
//
typedef struct debug_info_builder_t {
    debug_info_t *info;

    u8 *data;
    u32 reader_offset;

    u32 current_file_id;
    u32 current_line;
    u32 next_file_line_offset;

    i32 current_scope;

    u32 remaining_reps;

    b32 locked : 1;
} debug_info_builder_t;

void debug_info_builder_init(debug_info_builder_t *, debug_info_t *);
void debug_info_builder_prepare(debug_info_builder_t *, u8 *);
void debug_info_builder_emit_location(debug_info_builder_t *);
void debug_info_builder_step(debug_info_builder_t *);
void debug_info_builder_begin_func(debug_info_builder_t *, i32 func_idx);
void debug_info_builder_end_func(debug_info_builder_t *);


typedef enum debug_exec_state_t {
    debug_state_starting,
    debug_state_ready,
    debug_state_running,
    debug_state_paused,

    debug_state_pausing,
    debug_state_hit_breakpoint,
} debug_exec_state_t;

typedef enum debug_pause_reason_t {
    debug_pause_entry = 1,
    debug_pause_step = 2,
    debug_pause_exception = 3,
} debug_pause_reason_t;

typedef struct debug_breakpoint_t {
    u32 id;
    u32 instr;
    u32 file_id;
    u32 line;
} debug_breakpoint_t;

typedef struct debug_thread_state_t {
    u32 id;

    debug_exec_state_t state;
    struct ovm_state_t *ovm_state;

    // This flag signals if the thread has done
    // ANY execution. When a thread first starts
    // the thread signals a `paused` event, with
    // an `entry` reason. When the thread pauses
    // later on, it should be a `step` reason.
    b32 started;

    i32 run_count;
    semaphore* wait_semaphore;

    bool pause_at_next_line;
    i32 pause_within;
    i32 extra_frames_since_last_pause;
    debug_pause_reason_t pause_reason;

    u32 last_breakpoint_hit;

    u32 state_change_write_fd;
} debug_thread_state_t;

//
// This represents known state of the debugger. There should only
// be one state per running program, as it is tied to the ovm_engine_t.
//
typedef struct debug_state_t {
    bh_allocator alloc;

    bh_arena tmp_arena;
    bh_allocator tmp_alloc;

    char *listen_path;

    debug_info_t *info;
    struct ovm_engine_t *ovm_engine;

    bh_arr(debug_thread_state_t *) threads;
    u32 next_thread_id;

    u32 next_breakpoint_id;
    bh_arr(debug_breakpoint_t) breakpoints;

    pthread_t debug_thread;
    bool debug_thread_running;

    u32 listen_socket_fd;
    u32 client_fd;

    bh_buffer send_buffer;

    u32 state_change_pipes[2];
} debug_state_t;

void debug_host_init(debug_state_t *debug, struct ovm_engine_t *ovm_engine);
void debug_host_start(debug_state_t *debug);
void debug_host_stop(debug_state_t *debug);
u32  debug_host_register_thread(debug_state_t *debug, struct ovm_state_t *ovm_state);
debug_thread_state_t *debug_host_lookup_thread(debug_state_t *debug, u32 id);



typedef struct debug_runtime_value_builder_t {
    debug_state_t *state;
    debug_info_t *info;

    bh_buffer output;
    struct ovm_state_t *ovm_state;
    struct ovm_stack_frame_t *ovm_frame;

    debug_sym_scope_t sym_scope;
    debug_func_info_t func_info;
    debug_file_info_t file_info;
    debug_loc_info_t loc_info;

    // "base_" refers to the top symbol. In a layered
    // query, this information is not outputted, only
    // used to lookup the values inside of it.
    debug_sym_loc_kind_t base_loc_kind;
    u32 base_loc;
    u32 base_type;

    // "it_" refers to the current symbol to be output.
    u32 max_index;
    u32 it_index;

    debug_sym_loc_kind_t it_loc_kind;
    u32 it_loc;
    u32 it_type;
    char *it_name;
    bool it_has_children;
} debug_runtime_value_builder_t;

void debug_runtime_value_build_init(debug_runtime_value_builder_t *builder, bh_allocator alloc);
void debug_runtime_value_build_set_location(debug_runtime_value_builder_t *builder, debug_sym_loc_kind_t loc_kind, u32 loc, u32 type, char *name);
void debug_runtime_value_build_descend(debug_runtime_value_builder_t *builder, u32 index);
bool debug_runtime_value_build_step(debug_runtime_value_builder_t *builder);
void debug_runtime_value_build_string(debug_runtime_value_builder_t *builder);
u32  debug_runtime_value_get_it_addr(debug_runtime_value_builder_t *builder);
void debug_runtime_value_build_clear(debug_runtime_value_builder_t *builder);
void debug_runtime_value_build_free(debug_runtime_value_builder_t *builder);

void *__debug_thread_entry(void *);

#endif
