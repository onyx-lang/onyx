#ifndef _ONYX_VM_H
#define _ONYX_VM_H

#include "bh.h"
#include "ovm_debug.h"
#include <stdbool.h>
#include <pthread.h>

typedef u8  ovm_valtype_t;
typedef i32 ovm_valnum_t;
typedef u32 ovm_instr_kind_t;

typedef struct ovm_store_t ovm_store_t;
typedef struct ovm_engine_t ovm_engine_t;
typedef struct ovm_program_t ovm_program_t;
typedef struct ovm_state_t ovm_state_t;
typedef struct ovm_stack_frame_t ovm_stack_frame_t;
typedef enum   ovm_func_kind_t ovm_func_kind_t;
typedef struct ovm_func_t ovm_func_t;
typedef struct ovm_external_func_t ovm_external_func_t;
typedef struct ovm_linkable_func_t ovm_linkable_func_t;
typedef struct ovm_value_t ovm_value_t;
typedef struct ovm_instr_t ovm_instr_t;
typedef struct ovm_static_data_t ovm_static_data_t;
typedef struct ovm_static_integer_array_t ovm_static_integer_array_t;


//
// Contains storage.
struct ovm_store_t {
    bh_allocator    heap_allocator;
    bh_atomic_arena arena;
    bh_allocator    arena_allocator;
};

ovm_store_t *ovm_store_new();
void         ovm_store_delete(ovm_store_t *store);


struct ovm_static_data_t {
    i64   dest_addr;
    void *data;
    i64   length;
};

struct ovm_static_integer_array_t {
    i32 start_idx;
    i32 len;
};

//
// Represents a program that is runnable by the
// VM. It can be constructed incrementally as needed.
//
struct ovm_program_t {
    bh_arr(ovm_instr_t)       code;
    bh_arr(ovm_func_t)        funcs;

    //
    // TODO: Document these, and rename them.
    bh_arr(i32) static_integers;
    bh_arr(ovm_static_integer_array_t) static_data;

    i32 register_count;
    ovm_store_t *store;
};

ovm_program_t *ovm_program_new(ovm_store_t *store);
void ovm_program_delete(ovm_program_t *program);
void ovm_program_add_instructions(ovm_program_t *program, i32 instr_count, ovm_instr_t *instrs);
void ovm_program_print_instructions(ovm_program_t *program, i32 start_instr, i32 instr_count);
void ovm_raw_print_instructions(i32 instr_count, ovm_instr_t *instrs);

int  ovm_program_register_static_ints(ovm_program_t *program, int len, int *data);
int  ovm_program_register_func(ovm_program_t *program, char *name, i32 instr, i32 param_count, i32 value_number_count);
int  ovm_program_register_external_func(ovm_program_t *program, char *name, i32 param_count, i32 external_func_idx);
void ovm_program_begin_func(ovm_program_t *program, char *name, i32 param_count, i32 value_number_count);
void ovm_program_modify_static_int(ovm_program_t *program, int arr, int idx, int new_value);

//
// Represents the running configuration and static
// data needed by the VM. This is for more "global" data.
// If multiple threads are used, only one engine is needed.
// 
struct ovm_engine_t {
    ovm_store_t *store;

    pthread_mutex_t atomic_mutex;

    i64   memory_size; // This is probably going to always be 4GiB.
    void *memory;

    debug_state_t *debug;
};

ovm_engine_t *ovm_engine_new(ovm_store_t *store);
void          ovm_engine_delete(ovm_engine_t *engine);
void          ovm_engine_memory_copy(ovm_engine_t *engine, i64 target, void *data, i64 size);

bool ovm_program_load_from_file(ovm_program_t *program, ovm_engine_t *engine, char *filename);

//
// Represents ephemeral state / execution context.
// If multiple threads are used, multiple states are needed.
// 
struct ovm_state_t {
    ovm_store_t *store;

    i32 pc;
    i32 value_number_offset;
    
    bh_arr(ovm_value_t) numbered_values;
    bh_arr(ovm_value_t) params;
    bh_arr(ovm_stack_frame_t) stack_frames;
    bh_arr(ovm_value_t) registers;

    //
    // Originally, these were stored on the ovm_program that
    // this state corresponds with. However, that does not line
    // up with the specifications needed by WASM. In theory, different
    // running instances of the program *could* have different
    // native functions linked.
    bh_arr(ovm_external_func_t) external_funcs;

    debug_thread_state_t *debug;
};

ovm_state_t *ovm_state_new(ovm_engine_t *engine, ovm_program_t *program);
void         ovm_state_delete(ovm_state_t *state);
void ovm_state_link_external_funcs(ovm_program_t *program, ovm_state_t *state, ovm_linkable_func_t *funcs);
void ovm_state_register_external_func(ovm_state_t *state, i32 idx, void (*func)(void *, ovm_value_t *, ovm_value_t *), void *data);
ovm_value_t ovm_state_register_get(ovm_state_t *state, i32 idx);
void ovm_state_register_set(ovm_state_t *state, i32 idx, ovm_value_t val);

//
//
struct ovm_stack_frame_t {
    ovm_func_t *func;
    i32 value_number_count;
    i32 value_number_base;

    i32 return_address;
    i32 return_number_value;
};


//
// Represents a function that can be executed on the VM.
//
enum ovm_func_kind_t {
    OVM_FUNC_INTERNAL,
    OVM_FUNC_EXTERNAL
};

struct ovm_func_t {
    //
    // This ID is used as the index into the `funcs` member on ovm_program_t
    // to reference this function. It is only here for debugging and posterity.
    i32 id;
    ovm_func_kind_t kind;
    char *name;
    i32 param_count;
    i32 value_number_count;

    union {
        i32 start_instr;
        i32 external_func_idx;
    };
};

struct ovm_external_func_t {
    void (*native_func)(void *userdata, ovm_value_t* params, ovm_value_t* result);
    void *userdata;
};

struct ovm_linkable_func_t {
    char *name;
    i32 param_count;
    ovm_external_func_t func;
};
 
ovm_func_t  *ovm_func_new();
ovm_instr_t *ovm_func_add_instruction(ovm_func_t *func, ovm_instr_kind_t instr, ovm_valtype_t type);
void         ovm_func_delete(ovm_func_t *func);

ovm_value_t ovm_func_call(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program, i32 func_idx,
        i32 param_count, ovm_value_t *params);
ovm_value_t ovm_run_code(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program);

//
// Instruction encoding
//
// This engine uses a simple Three Address Code (3AC) instruction representation
// with an "infinite" value number (register) count.
//

#define OVM_TYPE_NONE   0x00
#define OVM_TYPE_I8     0x01
#define OVM_TYPE_I16    0x02
#define OVM_TYPE_I32    0x03
#define OVM_TYPE_I64    0x04
#define OVM_TYPE_F32    0x05
#define OVM_TYPE_F64    0x06
#define OVM_TYPE_V128   0x07

struct ovm_value_t {
    ovm_valtype_t type;
    union {
        i8  i8;
        i16 i16;
        i32 i32;
        i64 i64;
        u8  u8;
        u16 u16;
        u32 u32;
        u64 u64;
        f32 f32;
        f64 f64;
    };
};

struct ovm_instr_t {
    u32 full_instr;

    // Destination value number.
    ovm_valnum_t r;

    union {
        // Input value numbers.
        struct {
            ovm_valnum_t a, b;
        };

        // Immediates in different types.
        i32 i;
        f32 f;
        i64 l;
        f64 d;
    };
};

#define OVM_INSTR_TYPE(instr)  ((instr).full_instr >> 24)
#define OVM_INSTR_INSTR(instr) ((instr).full_instr & 0xffffff)

#define OVMI_ATOMIC            0x00800000 // Flag an instruction as atomic

#define OVMI_NOP               0x00
#define OVMI_ADD               0x01   // %r = %a + %b
#define OVMI_SUB               0x02   // %r = %a - %b
#define OVMI_MUL               0x03   // %r = %a * %b
#define OVMI_DIV               0x04   // %r = %a / %b
#define OVMI_DIV_S             0x05   // %r = %a / %b
#define OVMI_REM               0x06   // %r = %a % %b
#define OVMI_REM_S             0x07   // %r = %a % %b

#define OVMI_AND               0x08   // %r = %a & %b
#define OVMI_OR                0x09   // %r = %a | %b
#define OVMI_XOR               0x0A   // %r = %a ^ %b
#define OVMI_SHL               0x0B   // %r = %a << %b
#define OVMI_SHR               0x0C   // %r = %a >> %b
#define OVMI_SAR               0x0D   // %r = %a >>> %b

#define OVMI_IMM               0x10   // %r = i/l/f/d
#define OVMI_MOV               0x11   // %r = %a
#define OVMI_LOAD              0x12   // %r = mem[%a + %b]
#define OVMI_STORE             0x13   // mem[%r + %b] = %a
#define OVMI_COPY              0x14   // memcpy(%r, %a, %b)
#define OVMI_FILL              0x15   // memset(%r, %a, %b)
#define OVMI_REG_GET           0x16   // %r = #a
#define OVMI_REG_SET           0x17   // #r = %a
#define OVMI_IDX_ARR           0x18   // %r = (a)[%b]

#define OVMI_LT                0x20   // %r = %a < %b
#define OVMI_LT_S              0x21   // %r = %a < %b
#define OVMI_LE                0x22   // %r = %a <= %b
#define OVMI_LE_S              0x23   // %r = %a <= %b
#define OVMI_EQ                0x24   // %r = %a == %b
#define OVMI_GE                0x25   // %r = %a >= %b
#define OVMI_GE_S              0x26   // %r = %a >= %b
#define OVMI_GT                0x27   // %r = %a > %b
#define OVMI_GT_S              0x28   // %r = %a > %b
#define OVMI_NE                0x29   // %r = %a != %b

#define OVMI_PARAM             0x30   // push %a
#define OVMI_RETURN            0x31   // return %a
#define OVMI_CALL              0x32   // %r = a(...)
#define OVMI_CALLI             0x33   // %r = %a(...)

#define OVMI_BR                0x40   // br pc + a              // Relative branching
#define OVMI_BR_Z              0x41   // br pc + a if %b == 0
#define OVMI_BR_NZ             0x42   // br pc + a if %b != 0
#define OVMI_BRI               0x43   // br pc + %a             // Relative branching
#define OVMI_BRI_Z             0x44   // br pc + %a if %b == 0
#define OVMI_BRI_NZ            0x45   // br pc + %a if %b != 0

#define OVMI_CLZ               0x50   // %r = clz(%a)
#define OVMI_CTZ               0x51   // %r = ctr(%a)
#define OVMI_POPCNT            0x52   // %r = popcnt(%a)
#define OVMI_ROTL              0x53   // %r = rotl(%a, %b)
#define OVMI_ROTR              0x54   // %r = rotr(%a, %b)

// These instructions are only implemented for floats.
#define OVMI_ABS               0x55   // %r = |%a|
#define OVMI_NEG               0x56   // %r = -%a
#define OVMI_CEIL              0x57   // %r = ceil(%a)
#define OVMI_FLOOR             0x58   // %r = floor(%a)
#define OVMI_TRUNC             0x59   // %r = trunc(%a)
#define OVMI_NEAREST           0x5A   // %r = nearest(%a)
#define OVMI_SQRT              0x5B   // %r = sqrt(%a)
#define OVMI_MIN               0x5C   // %r = min(%a, %b)
#define OVMI_MAX               0x5D   // %r = max(%a, %b)
#define OVMI_COPYSIGN          0x5E   // %r = copysign(%a, %b)

// For conversion operations, the "type" of the instruction is
// destination type, the type in the name is the source type.
//
// There are a couple of cast operations that are not available,
// such as unsigned conversion from 32-bit integers to floats.
#define OVMI_CVT_I8            0x60   // %r = (t) %a
#define OVMI_CVT_I8_S          0x61   // %r = (t) %a (sign aware)
#define OVMI_CVT_I16           0x62   // %r = (t) %a
#define OVMI_CVT_I16_S         0x63   // %r = (t) %a (sign aware)
#define OVMI_CVT_I32           0x64   // %r = (t) %a
#define OVMI_CVT_I32_S         0x65   // %r = (t) %a (sign aware)
#define OVMI_CVT_I64           0x66   // %r = (t) %a
#define OVMI_CVT_I64_S         0x67   // %r = (t) %a (sign aware)
#define OVMI_CVT_F32           0x68   // %r = (t) %a
#define OVMI_CVT_F32_S         0x69   // %r = (t) %a (sign aware)
#define OVMI_CVT_F64           0x6A   // %r = (t) %a
#define OVMI_CVT_F64_S         0x6B   // %r = (t) %a (sign aware)
#define OVMI_TRANSMUTE_I32     0x6C   // %r = *(t *) &%a (reinterpret bytes)
#define OVMI_TRANSMUTE_I64     0x6D   // %r = *(t *) &%a (reinterpret bytes)
#define OVMI_TRANSMUTE_F32     0x6E   // %r = *(t *) &%a (reinterpret bytes)
#define OVMI_TRANSMUTE_F64     0x6F   // %r = *(t *) &%a (reinterpret bytes)

#define OVMI_CMPXCHG           0x70   // %r = %r == %a ? %b : %r 

//
// OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I32) == instruction for adding i32s
//
#define OVM_TYPED_INSTR(instr, type)  (((type) << 24) | (instr))


#endif

