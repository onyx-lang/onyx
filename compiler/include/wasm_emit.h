#ifndef ONYXWASM_H
#define ONYXWASM_H

#include "bh.h"

#include "astnodes.h"
#include "errors.h"

typedef u8 WasmType;

typedef struct WasmFuncType {
    // NOTE: For now, WASM only allows for 1 return value.
    // This may be lifted in the future.
    i32 param_count;
    WasmType return_type;
    WasmType param_types[];
} WasmFuncType;

#define SIMD_INSTR_MASK   0x10000
#define EXT_INSTR_MASK    0x20000
#define ATOMIC_INSTR_MASK 0x40000

typedef enum WasmInstructionType {
    WI_UNREACHABLE                   = 0x00,
    WI_NOP                           = 0x01,

    // NOTE: Control flow
    WI_BLOCK_START                   = 0x02,
    WI_BLOCK_END                     = 0x0B, // NOTE: These ends are not unique
    WI_LOOP_START                    = 0x03,
    WI_LOOP_END                      = 0x0B,
    WI_IF_START                      = 0x04,
    WI_ELSE                          = 0x05,
    WI_IF_END                        = 0x0B,
    WI_JUMP                          = 0x0C,
    WI_COND_JUMP                     = 0x0D,
    WI_JUMP_TABLE                    = 0x0E,
    WI_RETURN                        = 0x0F,
    WI_CALL                          = 0x10,
    WI_CALL_INDIRECT                 = 0x11,

    // NOTE: Parametric instructions
    WI_DROP                          = 0x1A,
    WI_SELECT                        = 0x1B,

    // NOTE: Variable instructions
    WI_LOCAL_GET                     = 0x20,
    WI_LOCAL_SET                     = 0x21,
    WI_LOCAL_TEE                     = 0x22,
    WI_GLOBAL_GET                    = 0x23,
    WI_GLOBAL_SET                    = 0x24,

    // NOTE: Memory instructions
    WI_I32_LOAD                      = 0x28,
    WI_I64_LOAD                      = 0x29,
    WI_F32_LOAD                      = 0x2A,
    WI_F64_LOAD                      = 0x2B,
    WI_I32_LOAD_8_S                  = 0x2C,
    WI_I32_LOAD_8_U                  = 0x2D,
    WI_I32_LOAD_16_S                 = 0x2E,
    WI_I32_LOAD_16_U                 = 0x2F,
    WI_I64_LOAD_8_S                  = 0x30,
    WI_I64_LOAD_8_U                  = 0x31,
    WI_I64_LOAD_16_S                 = 0x32,
    WI_I64_LOAD_16_U                 = 0x33,
    WI_I64_LOAD_32_S                 = 0x34,
    WI_I64_LOAD_32_U                 = 0x35,
    WI_I32_STORE                     = 0x36,
    WI_I64_STORE                     = 0x37,
    WI_F32_STORE                     = 0x38,
    WI_F64_STORE                     = 0x39,
    WI_I32_STORE_8                   = 0x3A,
    WI_I32_STORE_16                  = 0x3B,
    WI_I64_STORE_8                   = 0x3C,
    WI_I64_STORE_16                  = 0x3D,
    WI_I64_STORE_32                  = 0x3E,
    WI_MEMORY_SIZE                   = 0x3F,
    WI_MEMORY_GROW                   = 0x40,

    // NOTE: Numeric Instructions
    WI_I32_CONST                     = 0x41,
    WI_I64_CONST                     = 0x42,
    WI_F32_CONST                     = 0x43,
    WI_F64_CONST                     = 0x44,

    WI_I32_EQZ                       = 0x45,
    WI_I32_EQ                        = 0x46,
    WI_I32_NE                        = 0x47,
    WI_I32_LT_S                      = 0x48,
    WI_I32_LT_U                      = 0x49,
    WI_I32_GT_S                      = 0x4a,
    WI_I32_GT_U                      = 0x4b,
    WI_I32_LE_S                      = 0x4c,
    WI_I32_LE_U                      = 0x4d,
    WI_I32_GE_S                      = 0x4e,
    WI_I32_GE_U                      = 0x4f,

    WI_I64_EQZ                       = 0x50,
    WI_I64_EQ                        = 0x51,
    WI_I64_NE                        = 0x52,
    WI_I64_LT_S                      = 0x53,
    WI_I64_LT_U                      = 0x54,
    WI_I64_GT_S                      = 0x55,
    WI_I64_GT_U                      = 0x56,
    WI_I64_LE_S                      = 0x57,
    WI_I64_LE_U                      = 0x58,
    WI_I64_GE_S                      = 0x59,
    WI_I64_GE_U                      = 0x5a,

    WI_F32_EQ                        = 0x5b,
    WI_F32_NE                        = 0x5c,
    WI_F32_LT                        = 0x5d,
    WI_F32_GT                        = 0x5e,
    WI_F32_LE                        = 0x5f,
    WI_F32_GE                        = 0x60,

    WI_F64_EQ                        = 0x61,
    WI_F64_NE                        = 0x62,
    WI_F64_LT                        = 0x63,
    WI_F64_GT                        = 0x64,
    WI_F64_LE                        = 0x65,
    WI_F64_GE                        = 0x66,

    WI_I32_CLZ                       = 0x67,
    WI_I32_CTZ                       = 0x68,
    WI_I32_POPCNT                    = 0x69,
    WI_I32_ADD                       = 0x6a,
    WI_I32_SUB                       = 0x6b,
    WI_I32_MUL                       = 0x6c,
    WI_I32_DIV_S                     = 0x6d,
    WI_I32_DIV_U                     = 0x6e,
    WI_I32_REM_S                     = 0x6f,
    WI_I32_REM_U                     = 0x70,
    WI_I32_AND                       = 0x71,
    WI_I32_OR                        = 0x72,
    WI_I32_XOR                       = 0x73,
    WI_I32_SHL                       = 0x74,
    WI_I32_SHR_S                     = 0x75,
    WI_I32_SHR_U                     = 0x76,
    WI_I32_ROTL                      = 0x77,
    WI_I32_ROTR                      = 0x78,

    WI_I64_CLZ                       = 0x79,
    WI_I64_CTZ                       = 0x7a,
    WI_I64_POPCNT                    = 0x7b,
    WI_I64_ADD                       = 0x7c,
    WI_I64_SUB                       = 0x7d,
    WI_I64_MUL                       = 0x7e,
    WI_I64_DIV_S                     = 0x7f,
    WI_I64_DIV_U                     = 0x80,
    WI_I64_REM_S                     = 0x81,
    WI_I64_REM_U                     = 0x82,
    WI_I64_AND                       = 0x83,
    WI_I64_OR                        = 0x84,
    WI_I64_XOR                       = 0x85,
    WI_I64_SHL                       = 0x86,
    WI_I64_SHR_S                     = 0x87,
    WI_I64_SHR_U                     = 0x88,
    WI_I64_ROTL                      = 0x89,
    WI_I64_ROTR                      = 0x8a,

    WI_F32_ABS                       = 0x8b,
    WI_F32_NEG                       = 0x8c,
    WI_F32_CEIL                      = 0x8d,
    WI_F32_FLOOR                     = 0x8e,
    WI_F32_TRUNC                     = 0x8f,
    WI_F32_NEAREST                   = 0x90,
    WI_F32_SQRT                      = 0x91,
    WI_F32_ADD                       = 0x92,
    WI_F32_SUB                       = 0x93,
    WI_F32_MUL                       = 0x94,
    WI_F32_DIV                       = 0x95,
    WI_F32_MIN                       = 0x96,
    WI_F32_MAX                       = 0x97,
    WI_F32_COPYSIGN                  = 0x98,

    WI_F64_ABS                       = 0x99,
    WI_F64_NEG                       = 0x9a,
    WI_F64_CEIL                      = 0x9b,
    WI_F64_FLOOR                     = 0x9c,
    WI_F64_TRUNC                     = 0x9d,
    WI_F64_NEAREST                   = 0x9e,
    WI_F64_SQRT                      = 0x9f,
    WI_F64_ADD                       = 0xA0,
    WI_F64_SUB                       = 0xA1,
    WI_F64_MUL                       = 0xA2,
    WI_F64_DIV                       = 0xA3,
    WI_F64_MIN                       = 0xA4,
    WI_F64_MAX                       = 0xA5,
    WI_F64_COPYSIGN                  = 0xA6,

    WI_I32_FROM_I64                  = 0xA7,
    WI_I32_FROM_F32_S                = 0xA8,
    WI_I32_FROM_F32_U                = 0xA9,
    WI_I32_FROM_F64_S                = 0xAA,
    WI_I32_FROM_F64_U                = 0xAB,

    WI_I64_FROM_I32_S                = 0xAC,
    WI_I64_FROM_I32_U                = 0xAD,
    WI_I64_FROM_F32_S                = 0xAE,
    WI_I64_FROM_F32_U                = 0xAF,
    WI_I64_FROM_F64_S                = 0xB0,
    WI_I64_FROM_F64_U                = 0xB1,

    WI_F32_FROM_I32_S                = 0xB2,
    WI_F32_FROM_I32_U                = 0xB3,
    WI_F32_FROM_I64_S                = 0xB4,
    WI_F32_FROM_I64_U                = 0xB5,
    WI_F32_FROM_F64                  = 0xB6,

    WI_F64_FROM_I32_S                = 0xB7,
    WI_F64_FROM_I32_U                = 0xB8,
    WI_F64_FROM_I64_S                = 0xB9,
    WI_F64_FROM_I64_U                = 0xBA,
    WI_F64_FROM_F32                  = 0xBB,

    WI_I32_REINTERPRET_F32           = 0xBC,
    WI_I64_REINTERPRET_F64           = 0xBD,
    WI_F32_REINTERPRET_I32           = 0xBE,
    WI_F64_REINTERPRET_I64           = 0xBF,

    WI_I32_EXTEND_8_S                = 0xC0,
    WI_I32_EXTEND_16_S               = 0xC1,
    WI_I64_EXTEND_8_S                = 0xC2,
    WI_I64_EXTEND_16_S               = 0xC3,
    WI_I64_EXTEND_32_S               = 0xC4,

    // Pointer stuff; this will make it easier to switch to 64-bit
    // pointers, whenever WebAssembly standarizes that.
    WI_PTR_CONST                     = WI_I32_CONST,
    WI_PTR_LOAD                      = WI_I32_LOAD,
    WI_PTR_STORE                     = WI_I32_STORE,
    WI_PTR_ADD                       = WI_I32_ADD,
    WI_PTR_SUB                       = WI_I32_SUB,
    WI_PTR_MUL                       = WI_I32_MUL,
    WI_PTR_GE                        = WI_I32_GE_U,
    WI_PTR_GT                        = WI_I32_GT_U,
    WI_PTR_EQ                        = WI_I32_EQ,

    WI_V128_LOAD                     = SIMD_INSTR_MASK | 0,
    WI_V128_STORE                    = SIMD_INSTR_MASK | 11,

    WI_V128_CONST                    = SIMD_INSTR_MASK | 12,

    WI_I8X16_SHUFFLE                 = SIMD_INSTR_MASK | 13,

    WI_I8X16_EXTRACT_LANE_S          = SIMD_INSTR_MASK | 21,
    WI_I8X16_EXTRACT_LANE_U          = SIMD_INSTR_MASK | 22,
    WI_I8X16_REPLACE_LANE            = SIMD_INSTR_MASK | 23,
    WI_I16X8_EXTRACT_LANE_S          = SIMD_INSTR_MASK | 24,
    WI_I16X8_EXTRACT_LANE_U          = SIMD_INSTR_MASK | 25,
    WI_I16X8_REPLACE_LANE            = SIMD_INSTR_MASK | 26,
    WI_I32X4_EXTRACT_LANE            = SIMD_INSTR_MASK | 27,
    WI_I32X4_REPLACE_LANE            = SIMD_INSTR_MASK | 28,
    WI_I64X2_EXTRACT_LANE            = SIMD_INSTR_MASK | 29,
    WI_I64X2_REPLACE_LANE            = SIMD_INSTR_MASK | 30,
    WI_F32X4_EXTRACT_LANE            = SIMD_INSTR_MASK | 31,
    WI_F32X4_REPLACE_LANE            = SIMD_INSTR_MASK | 32,
    WI_F64X2_EXTRACT_LANE            = SIMD_INSTR_MASK | 33,
    WI_F64X2_REPLACE_LANE            = SIMD_INSTR_MASK | 34,

    WI_I8X16_SWIZZLE                 = SIMD_INSTR_MASK | 14,
    WI_I8X16_SPLAT                   = SIMD_INSTR_MASK | 15,
    WI_I16X8_SPLAT                   = SIMD_INSTR_MASK | 16,
    WI_I32X4_SPLAT                   = SIMD_INSTR_MASK | 17,
    WI_I64X2_SPLAT                   = SIMD_INSTR_MASK | 18,
    WI_F32X4_SPLAT                   = SIMD_INSTR_MASK | 19,
    WI_F64X2_SPLAT                   = SIMD_INSTR_MASK | 20,

    WI_I8X16_EQ                      = SIMD_INSTR_MASK | 35,
    WI_I8X16_NEQ                     = SIMD_INSTR_MASK | 36,
    WI_I8X16_LT_S                    = SIMD_INSTR_MASK | 37,
    WI_I8X16_LT_U                    = SIMD_INSTR_MASK | 38,
    WI_I8X16_GT_S                    = SIMD_INSTR_MASK | 39,
    WI_I8X16_GT_U                    = SIMD_INSTR_MASK | 40,
    WI_I8X16_LE_S                    = SIMD_INSTR_MASK | 41,
    WI_I8X16_LE_U                    = SIMD_INSTR_MASK | 42,
    WI_I8X16_GE_S                    = SIMD_INSTR_MASK | 43,
    WI_I8X16_GE_U                    = SIMD_INSTR_MASK | 44,

    WI_I16X8_EQ                      = SIMD_INSTR_MASK | 45,
    WI_I16X8_NEQ                     = SIMD_INSTR_MASK | 46,
    WI_I16X8_LT_S                    = SIMD_INSTR_MASK | 47,
    WI_I16X8_LT_U                    = SIMD_INSTR_MASK | 48,
    WI_I16X8_GT_S                    = SIMD_INSTR_MASK | 49,
    WI_I16X8_GT_U                    = SIMD_INSTR_MASK | 50,
    WI_I16X8_LE_S                    = SIMD_INSTR_MASK | 51,
    WI_I16X8_LE_U                    = SIMD_INSTR_MASK | 52,
    WI_I16X8_GE_S                    = SIMD_INSTR_MASK | 53,
    WI_I16X8_GE_U                    = SIMD_INSTR_MASK | 54,

    WI_I32X4_EQ                      = SIMD_INSTR_MASK | 55,
    WI_I32X4_NEQ                     = SIMD_INSTR_MASK | 56,
    WI_I32X4_LT_S                    = SIMD_INSTR_MASK | 57,
    WI_I32X4_LT_U                    = SIMD_INSTR_MASK | 58,
    WI_I32X4_GT_S                    = SIMD_INSTR_MASK | 59,
    WI_I32X4_GT_U                    = SIMD_INSTR_MASK | 60,
    WI_I32X4_LE_S                    = SIMD_INSTR_MASK | 61,
    WI_I32X4_LE_U                    = SIMD_INSTR_MASK | 62,
    WI_I32X4_GE_S                    = SIMD_INSTR_MASK | 63,
    WI_I32X4_GE_U                    = SIMD_INSTR_MASK | 64,

    WI_F32X4_EQ                      = SIMD_INSTR_MASK | 65,
    WI_F32X4_NEQ                     = SIMD_INSTR_MASK | 66,
    WI_F32X4_LT                      = SIMD_INSTR_MASK | 67,
    WI_F32X4_GT                      = SIMD_INSTR_MASK | 68,
    WI_F32X4_LE                      = SIMD_INSTR_MASK | 69,
    WI_F32X4_GE                      = SIMD_INSTR_MASK | 70,

    WI_F64X2_EQ                      = SIMD_INSTR_MASK | 71,
    WI_F64X2_NEQ                     = SIMD_INSTR_MASK | 72,
    WI_F64X2_LT                      = SIMD_INSTR_MASK | 73,
    WI_F64X2_GT                      = SIMD_INSTR_MASK | 74,
    WI_F64X2_LE                      = SIMD_INSTR_MASK | 75,
    WI_F64X2_GE                      = SIMD_INSTR_MASK | 76,

    WI_V128_NOT                      = SIMD_INSTR_MASK | 77,
    WI_V128_AND                      = SIMD_INSTR_MASK | 78,
    WI_V128_ANDNOT                   = SIMD_INSTR_MASK | 79,
    WI_V128_OR                       = SIMD_INSTR_MASK | 80,
    WI_V128_XOR                      = SIMD_INSTR_MASK | 81,
    WI_V128_BITSELECT                = SIMD_INSTR_MASK | 82,

    WI_I8X16_ABS                     = SIMD_INSTR_MASK | 96,
    WI_I8X16_NEG                     = SIMD_INSTR_MASK | 97,
    WI_I8X16_ANY_TRUE                = SIMD_INSTR_MASK | 98,
    WI_I8X16_ALL_TRUE                = SIMD_INSTR_MASK | 99,
    WI_I8X16_BITMASK                 = SIMD_INSTR_MASK | 100,
    WI_I8X16_NARROW_I16X8_S          = SIMD_INSTR_MASK | 101,
    WI_I8X16_NARROW_I16X8_U          = SIMD_INSTR_MASK | 102,
    WI_I8X16_SHL                     = SIMD_INSTR_MASK | 107,
    WI_I8X16_SHR_S                   = SIMD_INSTR_MASK | 108,
    WI_I8X16_SHR_U                   = SIMD_INSTR_MASK | 109,
    WI_I8X16_ADD                     = SIMD_INSTR_MASK | 110,
    WI_I8X16_ADD_SAT_S               = SIMD_INSTR_MASK | 111,
    WI_I8X16_ADD_SAT_U               = SIMD_INSTR_MASK | 112,
    WI_I8X16_SUB                     = SIMD_INSTR_MASK | 113,
    WI_I8X16_SUB_SAT_S               = SIMD_INSTR_MASK | 114,
    WI_I8X16_SUB_SAT_U               = SIMD_INSTR_MASK | 115,
    WI_I8X16_MIN_S                   = SIMD_INSTR_MASK | 118,
    WI_I8X16_MIN_U                   = SIMD_INSTR_MASK | 119,
    WI_I8X16_MAX_S                   = SIMD_INSTR_MASK | 120,
    WI_I8X16_MAX_U                   = SIMD_INSTR_MASK | 121,
    WI_I8X16_AVGR_U                  = SIMD_INSTR_MASK | 123,

    WI_I16X8_ABS                     = SIMD_INSTR_MASK | 128,
    WI_I16X8_NEG                     = SIMD_INSTR_MASK | 129,
    WI_I16X8_ANY_TRUE                = SIMD_INSTR_MASK | 130,
    WI_I16X8_ALL_TRUE                = SIMD_INSTR_MASK | 131,
    WI_I16X8_BITMASK                 = SIMD_INSTR_MASK | 132,
    WI_I16X8_NARROW_I32X4_S          = SIMD_INSTR_MASK | 133,
    WI_I16X8_NARROW_I32X4_U          = SIMD_INSTR_MASK | 134,
    WI_I16X8_WIDEN_LOW_I8X16_S       = SIMD_INSTR_MASK | 135,
    WI_I16X8_WIDEN_HIGH_I8X16_S      = SIMD_INSTR_MASK | 136,
    WI_I16X8_WIDEN_LOW_I8X16_U       = SIMD_INSTR_MASK | 137,
    WI_I16X8_WIDEN_HIGH_I8X16_U      = SIMD_INSTR_MASK | 138,
    WI_I16X8_SHL                     = SIMD_INSTR_MASK | 139,
    WI_I16X8_SHR_S                   = SIMD_INSTR_MASK | 140,
    WI_I16X8_SHR_U                   = SIMD_INSTR_MASK | 141,
    WI_I16X8_ADD                     = SIMD_INSTR_MASK | 142,
    WI_I16X8_ADD_SAT_S               = SIMD_INSTR_MASK | 143,
    WI_I16X8_ADD_SAT_U               = SIMD_INSTR_MASK | 144,
    WI_I16X8_SUB                     = SIMD_INSTR_MASK | 145,
    WI_I16X8_SUB_SAT_S               = SIMD_INSTR_MASK | 146,
    WI_I16X8_SUB_SAT_U               = SIMD_INSTR_MASK | 147,
    WI_I16X8_MUL                     = SIMD_INSTR_MASK | 149,
    WI_I16X8_MIN_S                   = SIMD_INSTR_MASK | 150,
    WI_I16X8_MIN_U                   = SIMD_INSTR_MASK | 151,
    WI_I16X8_MAX_S                   = SIMD_INSTR_MASK | 152,
    WI_I16X8_MAX_U                   = SIMD_INSTR_MASK | 153,
    WI_I16X8_AVGR_U                  = SIMD_INSTR_MASK | 155,

    WI_I32X4_ABS                     = SIMD_INSTR_MASK | 160,
    WI_I32X4_NEG                     = SIMD_INSTR_MASK | 161,
    WI_I32X4_ANY_TRUE                = SIMD_INSTR_MASK | 162,
    WI_I32X4_ALL_TRUE                = SIMD_INSTR_MASK | 163,
    WI_I32X4_BITMASK                 = SIMD_INSTR_MASK | 164,
    WI_I32X4_WIDEN_LOW_I16X8_S       = SIMD_INSTR_MASK | 167,
    WI_I32X4_WIDEN_HIGH_I16X8_S      = SIMD_INSTR_MASK | 168,
    WI_I32X4_WIDEN_LOW_I16X8_U       = SIMD_INSTR_MASK | 169,
    WI_I32X4_WIDEN_HIGH_I16X8_U      = SIMD_INSTR_MASK | 170,
    WI_I32X4_SHL                     = SIMD_INSTR_MASK | 171,
    WI_I32X4_SHR_S                   = SIMD_INSTR_MASK | 172,
    WI_I32X4_SHR_U                   = SIMD_INSTR_MASK | 173,
    WI_I32X4_ADD                     = SIMD_INSTR_MASK | 174,
    WI_I32X4_SUB                     = SIMD_INSTR_MASK | 177,
    WI_I32X4_MUL                     = SIMD_INSTR_MASK | 181,
    WI_I32X4_MIN_S                   = SIMD_INSTR_MASK | 182,
    WI_I32X4_MIN_U                   = SIMD_INSTR_MASK | 183,
    WI_I32X4_MAX_S                   = SIMD_INSTR_MASK | 184,
    WI_I32X4_MAX_U                   = SIMD_INSTR_MASK | 185,

    WI_I64X2_NEG                     = SIMD_INSTR_MASK | 193,
    WI_I64X2_SHL                     = SIMD_INSTR_MASK | 203,
    WI_I64X2_SHR_S                   = SIMD_INSTR_MASK | 204,
    WI_I64X2_SHR_U                   = SIMD_INSTR_MASK | 205,
    WI_I64X2_ADD                     = SIMD_INSTR_MASK | 206,
    WI_I64X2_SUB                     = SIMD_INSTR_MASK | 209,
    WI_I64X2_MUL                     = SIMD_INSTR_MASK | 213,

    WI_F32X4_ABS                     = SIMD_INSTR_MASK | 224,
    WI_F32X4_NEG                     = SIMD_INSTR_MASK | 225,
    WI_F32X4_SQRT                    = SIMD_INSTR_MASK | 227,
    WI_F32X4_ADD                     = SIMD_INSTR_MASK | 228,
    WI_F32X4_SUB                     = SIMD_INSTR_MASK | 229,
    WI_F32X4_MUL                     = SIMD_INSTR_MASK | 230,
    WI_F32X4_DIV                     = SIMD_INSTR_MASK | 231,
    WI_F32X4_MIN                     = SIMD_INSTR_MASK | 232,
    WI_F32X4_MAX                     = SIMD_INSTR_MASK | 233,

    WI_F64X2_ABS                     = SIMD_INSTR_MASK | 236,
    WI_F64X2_NEG                     = SIMD_INSTR_MASK | 237,
    WI_F64X2_SQRT                    = SIMD_INSTR_MASK | 239,
    WI_F64X2_ADD                     = SIMD_INSTR_MASK | 240,
    WI_F64X2_SUB                     = SIMD_INSTR_MASK | 241,
    WI_F64X2_MUL                     = SIMD_INSTR_MASK | 242,
    WI_F64X2_DIV                     = SIMD_INSTR_MASK | 243,
    WI_F64X2_MIN                     = SIMD_INSTR_MASK | 244,
    WI_F64X2_MAX                     = SIMD_INSTR_MASK | 245,

    WI_I32X4_TRUNC_SAT_F32X4_S       = SIMD_INSTR_MASK | 248,
    WI_I32X4_TRUNC_SAT_F32X4_U       = SIMD_INSTR_MASK | 249,
    WI_F32X4_CONVERT_I32X4_S         = SIMD_INSTR_MASK | 250,
    WI_F32X4_CONVERT_I32X4_U         = SIMD_INSTR_MASK | 251,


    WI_MEMORY_INIT                   = EXT_INSTR_MASK | 0x08,
    WI_MEMORY_COPY                   = EXT_INSTR_MASK | 0x0a,
    WI_MEMORY_FILL                   = EXT_INSTR_MASK | 0x0b,

    WI_ATOMIC_NOTIFY                 = ATOMIC_INSTR_MASK | 0x00,
    WI_ATOMIC_WAIT32                 = ATOMIC_INSTR_MASK | 0x01,
    WI_ATOMIC_WAIT64                 = ATOMIC_INSTR_MASK | 0x02,

    WI_ATOMIC_FENCE                  = ATOMIC_INSTR_MASK | 0x03,

    WI_ATOMIC_I32_LOAD               = ATOMIC_INSTR_MASK | 0x10,
    WI_ATOMIC_I64_LOAD               = ATOMIC_INSTR_MASK | 0x11,
    WI_ATOMIC_I32_LOAD8_U            = ATOMIC_INSTR_MASK | 0x12,
    WI_ATOMIC_I32_LOAD16_U           = ATOMIC_INSTR_MASK | 0x13,
    WI_ATOMIC_I64_LOAD8_U            = ATOMIC_INSTR_MASK | 0x14,
    WI_ATOMIC_I64_LOAD16_U           = ATOMIC_INSTR_MASK | 0x15,
    WI_ATOMIC_I64_LOAD32_U           = ATOMIC_INSTR_MASK | 0x16,

    WI_ATOMIC_I32_STORE              = ATOMIC_INSTR_MASK | 0x17,
    WI_ATOMIC_I64_STORE              = ATOMIC_INSTR_MASK | 0x18,
    WI_ATOMIC_I32_STORE8             = ATOMIC_INSTR_MASK | 0x19,
    WI_ATOMIC_I32_STORE16            = ATOMIC_INSTR_MASK | 0x1a,
    WI_ATOMIC_I64_STORE8             = ATOMIC_INSTR_MASK | 0x1b,
    WI_ATOMIC_I64_STORE16            = ATOMIC_INSTR_MASK | 0x1c,
    WI_ATOMIC_I64_STORE32            = ATOMIC_INSTR_MASK | 0x1d,

    WI_ATOMIC_I32_ADD                = ATOMIC_INSTR_MASK | 0x1e,
    WI_ATOMIC_I64_ADD                = ATOMIC_INSTR_MASK | 0x1f,
    WI_ATOMIC_I32_ADD8_U             = ATOMIC_INSTR_MASK | 0x20,
    WI_ATOMIC_I32_ADD16_U            = ATOMIC_INSTR_MASK | 0x21,
    WI_ATOMIC_I64_ADD8_U             = ATOMIC_INSTR_MASK | 0x22,
    WI_ATOMIC_I64_ADD16_U            = ATOMIC_INSTR_MASK | 0x23,
    WI_ATOMIC_I64_ADD32_U            = ATOMIC_INSTR_MASK | 0x24,

    WI_ATOMIC_I32_SUB                = ATOMIC_INSTR_MASK | 0x25,
    WI_ATOMIC_I64_SUB                = ATOMIC_INSTR_MASK | 0x26,
    WI_ATOMIC_I32_SUB8_U             = ATOMIC_INSTR_MASK | 0x27,
    WI_ATOMIC_I32_SUB16_U            = ATOMIC_INSTR_MASK | 0x28,
    WI_ATOMIC_I64_SUB8_U             = ATOMIC_INSTR_MASK | 0x29,
    WI_ATOMIC_I64_SUB16_U            = ATOMIC_INSTR_MASK | 0x2a,
    WI_ATOMIC_I64_SUB32_U            = ATOMIC_INSTR_MASK | 0x2b,

    WI_ATOMIC_I32_AND                = ATOMIC_INSTR_MASK | 0x2c,
    WI_ATOMIC_I64_AND                = ATOMIC_INSTR_MASK | 0x2d,
    WI_ATOMIC_I32_AND8_U             = ATOMIC_INSTR_MASK | 0x2e,
    WI_ATOMIC_I32_AND16_U            = ATOMIC_INSTR_MASK | 0x2f,
    WI_ATOMIC_I64_AND8_U             = ATOMIC_INSTR_MASK | 0x30,
    WI_ATOMIC_I64_AND16_U            = ATOMIC_INSTR_MASK | 0x31,
    WI_ATOMIC_I64_AND32_U            = ATOMIC_INSTR_MASK | 0x32,

    WI_ATOMIC_I32_OR                 = ATOMIC_INSTR_MASK | 0x33,
    WI_ATOMIC_I64_OR                 = ATOMIC_INSTR_MASK | 0x34,
    WI_ATOMIC_I32_OR8_U              = ATOMIC_INSTR_MASK | 0x35,
    WI_ATOMIC_I32_OR16_U             = ATOMIC_INSTR_MASK | 0x36,
    WI_ATOMIC_I64_OR8_U              = ATOMIC_INSTR_MASK | 0x37,
    WI_ATOMIC_I64_OR16_U             = ATOMIC_INSTR_MASK | 0x38,
    WI_ATOMIC_I64_OR32_U             = ATOMIC_INSTR_MASK | 0x39,

    WI_ATOMIC_I32_XOR                = ATOMIC_INSTR_MASK | 0x3a,
    WI_ATOMIC_I64_XOR                = ATOMIC_INSTR_MASK | 0x3b,
    WI_ATOMIC_I32_XOR8_U             = ATOMIC_INSTR_MASK | 0x3c,
    WI_ATOMIC_I32_XOR16_U            = ATOMIC_INSTR_MASK | 0x3d,
    WI_ATOMIC_I64_XOR8_U             = ATOMIC_INSTR_MASK | 0x3e,
    WI_ATOMIC_I64_XOR16_U            = ATOMIC_INSTR_MASK | 0x3f,
    WI_ATOMIC_I64_XOR32_U            = ATOMIC_INSTR_MASK | 0x40,

    WI_ATOMIC_I32_XCHG               = ATOMIC_INSTR_MASK | 0x41,
    WI_ATOMIC_I64_XCHG               = ATOMIC_INSTR_MASK | 0x42,
    WI_ATOMIC_I32_XCHG8_U            = ATOMIC_INSTR_MASK | 0x43,
    WI_ATOMIC_I32_XCHG16_U           = ATOMIC_INSTR_MASK | 0x44,
    WI_ATOMIC_I64_XCHG8_U            = ATOMIC_INSTR_MASK | 0x45,
    WI_ATOMIC_I64_XCHG16_U           = ATOMIC_INSTR_MASK | 0x46,
    WI_ATOMIC_I64_XCHG32_U           = ATOMIC_INSTR_MASK | 0x47,

    WI_ATOMIC_I32_CMPXCHG            = ATOMIC_INSTR_MASK | 0x48,
    WI_ATOMIC_I64_CMPXCHG            = ATOMIC_INSTR_MASK | 0x49,
    WI_ATOMIC_I32_CMPXCHG8_U         = ATOMIC_INSTR_MASK | 0x4a,
    WI_ATOMIC_I32_CMPXCHG16_U        = ATOMIC_INSTR_MASK | 0x4b,
    WI_ATOMIC_I64_CMPXCHG8_U         = ATOMIC_INSTR_MASK | 0x4c,
    WI_ATOMIC_I64_CMPXCHG16_U        = ATOMIC_INSTR_MASK | 0x4d,
    WI_ATOMIC_I64_CMPXCHG32_U        = ATOMIC_INSTR_MASK | 0x4e,
} WasmInstructionType;

typedef union {
    struct {
        i32 i1, i2;
    };
    i64 l;
    float f;
    double d;
    ptr p;
} WasmInstructionData;

typedef struct WasmInstruction {
    WasmInstructionType type;
    WasmInstructionData data;
} WasmInstruction;

typedef struct BranchTable {
    u32 count;
    u32 default_case;
    u32 cases[];
} BranchTable;

#define LOCAL_IS_WASM 0x8000000000000
typedef struct LocalAllocator {
    u32 param_count;

    u32 allocated[5];
    u32 freed[5];

    i32 max_stack;
    i32 curr_stack;
} LocalAllocator;

typedef struct WasmFunc {
    i32 type_idx;
    LocalAllocator locals;
    bh_arr(WasmInstruction) code;
    OnyxToken *location;
    char *name;
} WasmFunc;

typedef struct WasmGlobal {
    WasmType type;
    u32 mutable : 1;
    bh_arr(WasmInstruction) initial_value;
} WasmGlobal;

typedef enum WasmForeignKind {
    WASM_FOREIGN_FUNCTION   = 0x00,
    WASM_FOREIGN_TABLE      = 0x01,
    WASM_FOREIGN_MEMORY     = 0x02,
    WASM_FOREIGN_GLOBAL     = 0x03,
} WasmForeignKind;

typedef struct WasmExport {
    WasmForeignKind kind;
    i32 idx;
} WasmExport;

typedef struct WasmImport {
    WasmForeignKind kind;
    union {
        i32 idx;
        struct {
            i32 min, max;
            b32 shared;
        };
    };
    char *mod, *name;
} WasmImport;

typedef struct WasmDatum {
    u32 id;
    u32 offset_, alignment;
    u32 length;
    ptr data;
} WasmDatum;

typedef struct WasmCustomSection {
    char *name;
    char *contents;
    u32 len;
} WasmCustomSection;

typedef enum DatumPatchInfoKind {
    Datum_Patch_Instruction,
    Datum_Patch_Data,
    Datum_Patch_Relative,
} DatumPatchInfoKind;

typedef enum CodePatchInfoKind {
    Code_Patch_Callee,
    Code_Patch_Element,
    Code_Patch_Export,
    Code_Patch_Tls_Offset,
    Code_Patch_String_Length,
    Code_Patch_String_Length_In_Data,
} CodePatchInfoKind;

//
// This represents a pointer that should be filled in
// later when the corresponding data element is placed.
//
// There are three kinds of patches:
//   - Instruction
//   - Data
//   - Relative
//
// In all cases, the `data_id` member is set to the id
// of the WasmDatum entry that will be the base address,
// and then the `offset` member will be added to that.
//
// In instruction patches, the `index` member is set
// to the index of the function where the instruction should
// be patched. The `location` member is set to the instruction
// that needs to have its data changed.
//
// In data patches, the `index` member is set to the id
// of the WasmDatum entry that needs to have a part of it
// updated. The `location` member is the offset into the
// data to update. It is assumed that 4 bytes will be reserved
// to be replaced with the pointer value.
//
// In relative patches, `index` member is set to the id
// of the WasmDatum entry that needs to have a part of it
// updated. The `location` member is the offset into the
// data to update. The difference between `Data` and `Relative`
// is that `Relative` *adds* the base address to the current
// value in the 4 bytes, as opposed to replacing it. As a
// convenience, if the value is 0 (null), it will remain as
// 0.
//
typedef struct DatumPatchInfo {
    DatumPatchInfoKind kind;
    u32 data_id;
    u32 offset;
    u32 location;
    u32 index;

    AstNode *node_to_use_if_data_id_is_null;
} DatumPatchInfo;

typedef struct CodePatchInfo {
    CodePatchInfoKind kind;
    u32 func_idx;
    u32 instr;

    AstNode *node_related_to_patch;
    OnyxToken *token_related_to_patch;
} CodePatchInfo;

// Context used when building a constexpr buffer
typedef struct ConstExprContext {
   struct OnyxWasmModule *module;
   ptr data;
   u32 data_id;
} ConstExprContext;

typedef enum DeferredStmtType {
    Deferred_Stmt_Node,
    Deferred_Stmt_Code,
} DeferredStmtType;

typedef struct DeferredStmt {
    DeferredStmtType type;
    u32 depth;
    AstDefer *defer_node;

    union {
        AstNode *stmt;
        struct {
            WasmInstruction* instructions;
            u32 instruction_count;
        };
    };
} DeferredStmt;

typedef struct AllocatedSpace {
    u64 depth;
    AstTyped *expr;
} AllocatedSpace;

typedef struct StrLitInfo {
    u32 data_id;
    u32 len;
} StrLitInfo;

typedef struct PatchInfo {
    u32 instruction_index;
} PatchInfo;

typedef struct ForRemoveInfo {
    // These are WASM locals
    u64 iterator_remove_func;
    u64 iterator_data_ptr;

    i32 remove_func_type_idx;
} ForRemoveInfo;

typedef struct JsPartial {
    u32 order;
    char *code;
} JsPartial;

typedef struct OnyxWasmModule {
    Context *context;
    bh_allocator allocator;

    bh_arena    *extended_instr_data;
    bh_allocator extended_instr_alloc;

    // NOTE: Mapping ptrs to function / global indicies
    bh_imap index_map;

    // NOTE: Mapping from local ast node ptrs to indicies or offsets, depending on the mode
    bh_imap local_map;

    i32 current_func_idx;
    LocalAllocator* local_alloc;

    // NOTE: Mapping ptrs to elements
    bh_imap elem_map;

    bh_arr(DeferredStmt)   deferred_stmts;
    bh_arr(AllocatedSpace) local_allocations;

    bh_arr(PatchInfo) stack_leave_patches;
    bh_arr(DatumPatchInfo) data_patches;
    bh_arr(CodePatchInfo)  code_patches;

    bh_arr(ForRemoveInfo) for_remove_info;

    bh_arr(AstForeignBlock *) foreign_blocks;
    u32 next_foreign_block_idx;

    bh_arr(AstFunction *) procedures_with_tags;
    bh_arr(AstMemRes *)   globals_with_tags;
    bh_arr(AstFunction *) all_procedures;

    // NOTE: Used internally as a map from strings that represent function types,
    // 0x7f 0x7f : 0x7f ( (i32, i32) -> i32 )
    // to the function type index if it has been created.
    Table(i32) type_map;

    Table(StrLitInfo) loaded_file_info;
    Table(StrLitInfo) string_literals;

    bh_arr(u8)  structured_jump_target;
    bh_arr(AstLocal*) return_location_stack;   // NOTE: Used for do-block return expressions.

    bh_arr(WasmFuncType*) types; // NOTE: This have to be pointers because the type is variadic in size
    bh_arr(WasmImport)    imports;
    Table(WasmExport)     exports;
    bh_arr(WasmGlobal)    globals;
    bh_arr(WasmFunc)      funcs;
    bh_arr(WasmDatum)     data;
    bh_arr(i32)           elems;
    bh_arr(char *)        libraries;
    bh_arr(char *)        library_paths;
    b32 needs_memory_section;
    u32 memory_min_size;
    u32 memory_max_size;

    Table(WasmCustomSection) custom_sections;

    bh_arr(JsPartial)     js_partials;

    // NOTE: Set of things used when compiling; not part of the actual module
    u32 export_count;
    u32 next_type_idx;
    u32 next_func_idx;
    u32 next_foreign_func_idx;
    u32 next_global_idx;
    u32 next_tls_offset;

    i32 *stack_top_ptr;
    i32 *tls_size_ptr;
    i32 *heap_start_ptr;
    u64 stack_base_idx;
    u64 stack_restore_idx;
    u64 stack_return_location_idx;
    u64 closure_base_idx;
    u64 stack_trace_idx;
    CallingConvention curr_cc;
    i32 null_proc_func_idx;

    i32 global_type_table_data_id;
    i32 type_info_size;
    i32 *type_info_entry_count;
    bh_arr(i32) types_enqueued_for_info;

    b32 has_stack_locals : 1;
    b32 doing_linking : 1;

#ifdef ENABLE_DEBUG_INFO
    struct DebugContext *debug_context;
#endif

} OnyxWasmModule;

typedef struct OnyxWasmLinkOptions {
    b32 stack_first;
    u32 stack_size;
    u32 stack_alignment;

    u32 null_reserve_size;

    b32 import_memory;
    char *import_memory_module_name;
    char *import_memory_import_name;

    b32 export_memory;
    char *export_memory_name;

    b32 export_func_table;
    char *export_func_table_name;

    u32 memory_min_size;
    u32 memory_max_size;
} OnyxWasmLinkOptions;

b32 onyx_wasm_build_link_options_from_node(Context *context, OnyxWasmLinkOptions *opts, struct AstTyped *node);

void onyx_wasm_module_initialize(Context *context, OnyxWasmModule *module);
void onyx_wasm_module_link(Context *context, OnyxWasmModule *module, OnyxWasmLinkOptions *options);
void onyx_wasm_module_free(OnyxWasmModule* module);
void onyx_wasm_module_write_to_buffer(OnyxWasmModule* module, bh_buffer* buffer);
void onyx_wasm_module_write_to_file(OnyxWasmModule* module, bh_file file);
void onyx_wasm_module_write_js_partials_to_buffer(OnyxWasmModule* module, bh_buffer* buffer);
void onyx_wasm_module_write_js_partials_to_file(OnyxWasmModule* module, bh_file file);

#ifdef ONYX_RUNTIME_LIBRARY
void onyx_run_initialize(b32 debug_enabled, const char *debug_socket);
b32 onyx_run_wasm_code(bh_buffer code_buffer, int argc, char *argv[]);
#endif

#ifdef ENABLE_DEBUG_INFO

typedef enum DebugOpType {
    DOT_END   = 0b00000000,
    DOT_SET   = 0b00000001,
    DOT_PUSHF = 0b00000010,
    DOT_POPF  = 0b00000011,
    DOT_SYM   = 0b00000100,

    DOT_INC   = 0b01000000,
    DOT_DEC   = 0b10000000,
    DOT_REP   = 0b11000000,
} DebugOpType;

typedef struct DebugFuncContext {
    u32 func_index;
    u32 file_id;
    u32 line;
    u32 op_offset;
    u64 stack_ptr_idx;

    u32 name_length;
    char *name;
} DebugFuncContext;

typedef struct DebugFileInfo {
    u32 file_id;
    u32 line_count;
} DebugFileInfo;

typedef enum DebugSymLoc {
    DSL_REGISTER = 1,
    DSL_STACK    = 2,
    DSL_GLOBAL   = 3,
} DebugSymLoc;

typedef struct DebugSymInfo {
    u32 sym_id;
    u32 location_type;
    u64 location_num;
    char *name;
    u32 type;
} DebugSymInfo;

typedef struct DebugSymPatch {
    u32 func_idx;
    u32 sym_id;
    u64 local_idx;
} DebugSymPatch;

typedef struct DebugContext {
    bh_allocator allocator;

    Table(DebugFileInfo) file_info;
    u32 next_file_id;

    bh_arr(DebugSymInfo)  sym_info;
    bh_arr(DebugSymPatch) sym_patches;
    u32 next_sym_id;

    bh_arr(DebugFuncContext) funcs;
    bh_buffer                op_buffer;

    // Used during building the debug info
    OnyxToken *last_token;
    b32 last_op_was_rep : 1;
} DebugContext;

#endif

#endif
