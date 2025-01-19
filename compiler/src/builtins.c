#include "astnodes.h"
#include "types.h"
#include "errors.h"
#include "utils.h"

static OnyxToken basic_type_void_token      = { Token_Type_Symbol, 4, "void " };
static OnyxToken basic_type_bool_token      = { Token_Type_Symbol, 4, "bool " };
static OnyxToken basic_type_i8_token        = { Token_Type_Symbol, 2, "i8 " };
static OnyxToken basic_type_u8_token        = { Token_Type_Symbol, 2, "u8 " };
static OnyxToken basic_type_i16_token       = { Token_Type_Symbol, 3, "i16 " };
static OnyxToken basic_type_u16_token       = { Token_Type_Symbol, 3, "u16 " };
static OnyxToken basic_type_i32_token       = { Token_Type_Symbol, 3, "i32 " };
static OnyxToken basic_type_u32_token       = { Token_Type_Symbol, 3, "u32 " };
static OnyxToken basic_type_i64_token       = { Token_Type_Symbol, 3, "i64 " };
static OnyxToken basic_type_u64_token       = { Token_Type_Symbol, 3, "u64 " };
static OnyxToken basic_type_f32_token       = { Token_Type_Symbol, 3, "f32 " };
static OnyxToken basic_type_f64_token       = { Token_Type_Symbol, 3, "f64 " };
static OnyxToken basic_type_rawptr_token    = { Token_Type_Symbol, 6, "rawptr " };
static OnyxToken basic_type_type_expr_token = { Token_Type_Symbol, 9, "type_expr " };

static OnyxToken builtin_heap_start_token  = { Token_Type_Symbol, 12, "__heap_start ", { 0 } };
static OnyxToken builtin_stack_top_token   = { Token_Type_Symbol, 11, "__stack_top ",  { 0 } };
static OnyxToken builtin_tls_base_token    = { Token_Type_Symbol, 10, "__tls_base ",  { 0 } };
static OnyxToken builtin_tls_size_token    = { Token_Type_Symbol, 10, "__tls_size ",  { 0 } };
static OnyxToken builtin_closure_base_token = { Token_Type_Symbol, 14, "__closure_base ",  { 0 } };
static OnyxToken builtin_stack_trace_token = { Token_Type_Symbol, 0, " ", { 0 } };

static OnyxToken simd_token = { Token_Type_Symbol, 0, "", { 0 } };


const BuiltinSymbol builtin_symbols[] = {
    #define OFFSET(member) (isize) &((Context *) 0)->basic_types.member
    { NULL, "void",       OFFSET(type_void) },
    { NULL, "bool",       OFFSET(type_bool) },
    { NULL, "i8",         OFFSET(type_i8) },
    { NULL, "u8",         OFFSET(type_u8) },
    { NULL, "i16",        OFFSET(type_i16) },
    { NULL, "u16",        OFFSET(type_u16) },
    { NULL, "i32",        OFFSET(type_i32) },
    { NULL, "u32",        OFFSET(type_u32) },
    { NULL, "i64",        OFFSET(type_i64) },
    { NULL, "u64",        OFFSET(type_u64) },
    { NULL, "f32",        OFFSET(type_f32) },
    { NULL, "f64",        OFFSET(type_f64) },
    { NULL, "rawptr",     OFFSET(type_rawptr) },
    { NULL, "type_expr",  OFFSET(type_type_expr) },

    { "simd", "i8x16",    OFFSET(type_i8x16) },
    { "simd", "i16x8",    OFFSET(type_i16x8) },
    { "simd", "i32x4",    OFFSET(type_i32x4) },
    { "simd", "i64x2",    OFFSET(type_i64x2) },
    { "simd", "f32x4",    OFFSET(type_f32x4) },
    { "simd", "f64x2",    OFFSET(type_f64x2) },
    { "simd", "v128",     OFFSET(type_v128 ) },
    #undef OFFSET

    #define OFFSET(member) (isize) &((Context *) 0)->builtins.member
    { "builtin", "__heap_start", OFFSET(heap_start) },
    { "builtin", "__stack_top",  OFFSET(stack_top) },
    { "builtin", "__tls_base",   OFFSET(tls_base) },
    { "builtin", "__tls_size",   OFFSET(tls_size) },
    { "builtin", "__closure_base",   OFFSET(closure_base) },
    #undef OFFSET

    { NULL, NULL, 0 },
};


const IntrinsicMap builtin_intrinsics[] = {
    { "unreachable",  ONYX_INTRINSIC_UNREACHABLE },

    { "memory_size",  ONYX_INTRINSIC_MEMORY_SIZE  },
    { "memory_grow",  ONYX_INTRINSIC_MEMORY_GROW  },
    { "memory_copy",  ONYX_INTRINSIC_MEMORY_COPY  },
    { "memory_fill",  ONYX_INTRINSIC_MEMORY_FILL  },
    { "memory_equal", ONYX_INTRINSIC_MEMORY_EQUAL },

    { "__initialize", ONYX_INTRINSIC_INITIALIZE },

    { "clz_i32",      ONYX_INTRINSIC_I32_CLZ },
    { "ctz_i32",      ONYX_INTRINSIC_I32_CTZ },
    { "popcnt_i32",   ONYX_INTRINSIC_I32_POPCNT },
    { "and_i32",      ONYX_INTRINSIC_I32_AND },
    { "or_i32",       ONYX_INTRINSIC_I32_OR },
    { "xor_i32",      ONYX_INTRINSIC_I32_XOR },
    { "shl_i32",      ONYX_INTRINSIC_I32_SHL },
    { "slr_i32",      ONYX_INTRINSIC_I32_SLR },
    { "sar_i32",      ONYX_INTRINSIC_I32_SAR },
    { "rotl_i32",     ONYX_INTRINSIC_I32_ROTL },
    { "rotr_i32",     ONYX_INTRINSIC_I32_ROTR },

    { "clz_i64",      ONYX_INTRINSIC_I64_CLZ },
    { "ctz_i64",      ONYX_INTRINSIC_I64_CTZ },
    { "popcnt_i64",   ONYX_INTRINSIC_I64_POPCNT },
    { "and_i64",      ONYX_INTRINSIC_I64_AND },
    { "or_i64",       ONYX_INTRINSIC_I64_OR },
    { "xor_i64",      ONYX_INTRINSIC_I64_XOR },
    { "shl_i64",      ONYX_INTRINSIC_I64_SHL },
    { "slr_i64",      ONYX_INTRINSIC_I64_SLR },
    { "sar_i64",      ONYX_INTRINSIC_I64_SAR },
    { "rotl_i64",     ONYX_INTRINSIC_I64_ROTL },
    { "rotr_i64",     ONYX_INTRINSIC_I64_ROTR },

    { "abs_f32",      ONYX_INTRINSIC_F32_ABS },
    { "ceil_f32",     ONYX_INTRINSIC_F32_CEIL },
    { "floor_f32",    ONYX_INTRINSIC_F32_FLOOR },
    { "trunc_f32",    ONYX_INTRINSIC_F32_TRUNC },
    { "nearest_f32",  ONYX_INTRINSIC_F32_NEAREST },
    { "sqrt_f32",     ONYX_INTRINSIC_F32_SQRT },
    { "min_f32",      ONYX_INTRINSIC_F32_MIN },
    { "max_f32",      ONYX_INTRINSIC_F32_MAX },
    { "copysign_f32", ONYX_INTRINSIC_F32_COPYSIGN },

    { "abs_f64",      ONYX_INTRINSIC_F64_ABS },
    { "ceil_f64",     ONYX_INTRINSIC_F64_CEIL },
    { "floor_f64",    ONYX_INTRINSIC_F64_FLOOR },
    { "trunc_f64",    ONYX_INTRINSIC_F64_TRUNC },
    { "nearest_f64",  ONYX_INTRINSIC_F64_NEAREST },
    { "sqrt_f64",     ONYX_INTRINSIC_F64_SQRT },
    { "min_f64",      ONYX_INTRINSIC_F64_MIN },
    { "max_f64",      ONYX_INTRINSIC_F64_MAX },
    { "copysign_f64", ONYX_INTRINSIC_F64_COPYSIGN },

    { "reinterpret_f32", ONYX_INTRINSIC_I32_REINTERPRET_F32 },
    { "reinterpret_f64", ONYX_INTRINSIC_I64_REINTERPRET_F64 },
    { "reinterpret_i32", ONYX_INTRINSIC_F32_REINTERPRET_I32 },
    { "reinterpret_i64", ONYX_INTRINSIC_F64_REINTERPRET_I64 },


    // SIMD Intrinsics
    { "v128_const", ONYX_INTRINSIC_V128_CONST },
    { "i8x16_const", ONYX_INTRINSIC_I8X16_CONST },
    { "i16x8_const", ONYX_INTRINSIC_I16X8_CONST },
    { "i32x4_const", ONYX_INTRINSIC_I32X4_CONST },
    { "i64x2_const", ONYX_INTRINSIC_I64X2_CONST },
    { "f32x4_const", ONYX_INTRINSIC_F32X4_CONST },
    { "f64x2_const", ONYX_INTRINSIC_F64X2_CONST },
    { "i8x16_shuffle", ONYX_INTRINSIC_I8X16_SHUFFLE },

    { "i8x16_extract_lane_s", ONYX_INTRINSIC_I8X16_EXTRACT_LANE_S },
    { "i8x16_extract_lane_u", ONYX_INTRINSIC_I8X16_EXTRACT_LANE_U },
	{ "i8x16_replace_lane",   ONYX_INTRINSIC_I8X16_REPLACE_LANE },
    { "i16x8_extract_lane_s", ONYX_INTRINSIC_I16X8_EXTRACT_LANE_S },
	{ "i16x8_extract_lane_u", ONYX_INTRINSIC_I16X8_EXTRACT_LANE_U },
	{ "i16x8_replace_lane",   ONYX_INTRINSIC_I16X8_REPLACE_LANE },
	{ "i32x4_extract_lane",   ONYX_INTRINSIC_I32X4_EXTRACT_LANE },
	{ "i32x4_replace_lane",   ONYX_INTRINSIC_I32X4_REPLACE_LANE },
    { "i64x2_extract_lane",   ONYX_INTRINSIC_I64X2_EXTRACT_LANE },
	{ "i64x2_replace_lane",   ONYX_INTRINSIC_I64X2_REPLACE_LANE },
	{ "f32x4_extract_lane",   ONYX_INTRINSIC_F32X4_EXTRACT_LANE },
	{ "f32x4_replace_lane",   ONYX_INTRINSIC_F32X4_REPLACE_LANE },
	{ "f64x2_extract_lane",   ONYX_INTRINSIC_F64X2_EXTRACT_LANE },
	{ "f64x2_replace_lane",   ONYX_INTRINSIC_F64X2_REPLACE_LANE },

    { "i8x16_swizzle", ONYX_INTRINSIC_I8X16_SWIZZLE },
    { "i8x16_splat",   ONYX_INTRINSIC_I8X16_SPLAT },
	{ "i16x8_splat",   ONYX_INTRINSIC_I16X8_SPLAT },
    { "i32x4_splat",   ONYX_INTRINSIC_I32X4_SPLAT },
	{ "i64x2_splat",   ONYX_INTRINSIC_I64X2_SPLAT },
    { "f32x4_splat",   ONYX_INTRINSIC_F32X4_SPLAT },
	{ "f64x2_splat",   ONYX_INTRINSIC_F64X2_SPLAT },

    { "i8x16_eq",   ONYX_INTRINSIC_I8X16_EQ },
	{ "i8x16_neq",  ONYX_INTRINSIC_I8X16_NEQ },
    { "i8x16_lt_s", ONYX_INTRINSIC_I8X16_LT_S },
	{ "i8x16_lt_u", ONYX_INTRINSIC_I8X16_LT_U },
    { "i8x16_gt_s", ONYX_INTRINSIC_I8X16_GT_S },
	{ "i8x16_gt_u", ONYX_INTRINSIC_I8X16_GT_U },
    { "i8x16_le_s", ONYX_INTRINSIC_I8X16_LE_S },
	{ "i8x16_le_u", ONYX_INTRINSIC_I8X16_LE_U },
    { "i8x16_ge_s", ONYX_INTRINSIC_I8X16_GE_S },
	{ "i8x16_ge_u", ONYX_INTRINSIC_I8X16_GE_U },

    { "i16x8_eq",   ONYX_INTRINSIC_I16X8_EQ },
	{ "i16x8_neq",  ONYX_INTRINSIC_I16X8_NEQ },
    { "i16x8_lt_s", ONYX_INTRINSIC_I16X8_LT_S },
	{ "i16x8_lt_u", ONYX_INTRINSIC_I16X8_LT_U },
    { "i16x8_gt_s", ONYX_INTRINSIC_I16X8_GT_S },
	{ "i16x8_gt_u", ONYX_INTRINSIC_I16X8_GT_U },
    { "i16x8_le_s", ONYX_INTRINSIC_I16X8_LE_S },
	{ "i16x8_le_u", ONYX_INTRINSIC_I16X8_LE_U },
    { "i16x8_ge_s", ONYX_INTRINSIC_I16X8_GE_S },
	{ "i16x8_ge_u", ONYX_INTRINSIC_I16X8_GE_U },

    { "i32x4_eq",   ONYX_INTRINSIC_I32X4_EQ },
	{ "i32x4_neq",  ONYX_INTRINSIC_I32X4_NEQ },
    { "i32x4_lt_s", ONYX_INTRINSIC_I32X4_LT_S },
	{ "i32x4_lt_u", ONYX_INTRINSIC_I32X4_LT_U },
    { "i32x4_gt_s", ONYX_INTRINSIC_I32X4_GT_S },
	{ "i32x4_gt_u", ONYX_INTRINSIC_I32X4_GT_U },
    { "i32x4_le_s", ONYX_INTRINSIC_I32X4_LE_S },
	{ "i32x4_le_u", ONYX_INTRINSIC_I32X4_LE_U },
    { "i32x4_ge_s", ONYX_INTRINSIC_I32X4_GE_S },
	{ "i32x4_ge_u", ONYX_INTRINSIC_I32X4_GE_U },

    { "f32x4_eq",  ONYX_INTRINSIC_F32X4_EQ },
	{ "f32x4_neq", ONYX_INTRINSIC_F32X4_NEQ },
    { "f32x4_lt",  ONYX_INTRINSIC_F32X4_LT },
	{ "f32x4_gt",  ONYX_INTRINSIC_F32X4_GT },
    { "f32x4_le",  ONYX_INTRINSIC_F32X4_LE },
	{ "f32x4_ge",  ONYX_INTRINSIC_F32X4_GE },

    { "f64x2_eq",  ONYX_INTRINSIC_F64X2_EQ },
	{ "f64x2_neq", ONYX_INTRINSIC_F64X2_NEQ },
    { "f64x2_lt",  ONYX_INTRINSIC_F64X2_LT },
	{ "f64x2_gt",  ONYX_INTRINSIC_F64X2_GT },
    { "f64x2_le",  ONYX_INTRINSIC_F64X2_LE },
	{ "f64x2_ge",  ONYX_INTRINSIC_F64X2_GE },

    { "v128_not",       ONYX_INTRINSIC_V128_NOT },
	{ "v128_and",       ONYX_INTRINSIC_V128_AND },
	{ "v128_andnot",    ONYX_INTRINSIC_V128_ANDNOT },
    { "v128_or",        ONYX_INTRINSIC_V128_OR },
	{ "v128_xor",       ONYX_INTRINSIC_V128_XOR },
	{ "v128_bitselect", ONYX_INTRINSIC_V128_BITSELECT },

    { "i8x16_abs",            ONYX_INTRINSIC_I8X16_ABS },
	{ "i8x16_neg",            ONYX_INTRINSIC_I8X16_NEG },
    { "i8x16_any_true",       ONYX_INTRINSIC_I8X16_ANY_TRUE },
	{ "i8x16_all_true",       ONYX_INTRINSIC_I8X16_ALL_TRUE },
    { "i8x16_bitmask",        ONYX_INTRINSIC_I8X16_BITMASK },
    { "i8x16_narrow_i16x8_s", ONYX_INTRINSIC_I8X16_NARROW_I16X8_S },
	{ "i8x16_narrow_i16x8_u", ONYX_INTRINSIC_I8X16_NARROW_I16X8_U },
    { "i8x16_shl",            ONYX_INTRINSIC_I8X16_SHL },
	{ "i8x16_shr_s",          ONYX_INTRINSIC_I8X16_SHR_S },
	{ "i8x16_shr_u",          ONYX_INTRINSIC_I8X16_SHR_U },
    { "i8x16_add",            ONYX_INTRINSIC_I8X16_ADD },
	{ "i8x16_add_sat_s",      ONYX_INTRINSIC_I8X16_ADD_SAT_S },
	{ "i8x16_add_sat_u",      ONYX_INTRINSIC_I8X16_ADD_SAT_U },
    { "i8x16_sub",            ONYX_INTRINSIC_I8X16_SUB },
	{ "i8x16_sub_sat_s",      ONYX_INTRINSIC_I8X16_SUB_SAT_S },
	{ "i8x16_sub_sat_u",      ONYX_INTRINSIC_I8X16_SUB_SAT_U },
    { "i8x16_min_s",          ONYX_INTRINSIC_I8X16_MIN_S },
	{ "i8x16_min_u",          ONYX_INTRINSIC_I8X16_MIN_U },
    { "i8x16_max_s",          ONYX_INTRINSIC_I8X16_MAX_S },
	{ "i8x16_max_u",          ONYX_INTRINSIC_I8X16_MAX_U },
    { "i8x16_avgr_u",         ONYX_INTRINSIC_I8X16_AVGR_U },

    { "i16x8_abs",                ONYX_INTRINSIC_I16X8_ABS },
	{ "i16x8_neg",                ONYX_INTRINSIC_I16X8_NEG },
    { "i16x8_any_true",           ONYX_INTRINSIC_I16X8_ANY_TRUE },
	{ "i16x8_all_true",           ONYX_INTRINSIC_I16X8_ALL_TRUE },
    { "i16x8_bitmask",            ONYX_INTRINSIC_I16X8_BITMASK },
    { "i16x8_narrow_i32x4_s",     ONYX_INTRINSIC_I16X8_NARROW_I32X4_S },
	{ "i16x8_narrow_i32x4_u",     ONYX_INTRINSIC_I16X8_NARROW_I32X4_U },
    { "i16x8_widen_low_i8x16_s",  ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_S },
	{ "i16x8_widen_high_i8x16_s", ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_S },
    { "i16x8_widen_low_i8x16_u",  ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_U },
	{ "i16x8_widen_high_i8x16_u", ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_U },
    { "i16x8_shl",                ONYX_INTRINSIC_I16X8_SHL },
	{ "i16x8_shr_s",              ONYX_INTRINSIC_I16X8_SHR_S },
	{ "i16x8_shr_u",              ONYX_INTRINSIC_I16X8_SHR_U },
    { "i16x8_add",                ONYX_INTRINSIC_I16X8_ADD },
	{ "i16x8_add_sat_s",          ONYX_INTRINSIC_I16X8_ADD_SAT_S },
	{ "i16x8_add_sat_u",          ONYX_INTRINSIC_I16X8_ADD_SAT_U },
    { "i16x8_sub",                ONYX_INTRINSIC_I16X8_SUB },
	{ "i16x8_sub_sat_s",          ONYX_INTRINSIC_I16X8_SUB_SAT_S },
	{ "i16x8_sub_sat_u",          ONYX_INTRINSIC_I16X8_SUB_SAT_U },
    { "i16x8_mul",                ONYX_INTRINSIC_I16X8_MUL },
    { "i16x8_min_s",              ONYX_INTRINSIC_I16X8_MIN_S },
	{ "i16x8_min_u",              ONYX_INTRINSIC_I16X8_MIN_U },
    { "i16x8_max_s",              ONYX_INTRINSIC_I16X8_MAX_S },
	{ "i16x8_max_u",              ONYX_INTRINSIC_I16X8_MAX_U },
    { "i16x8_avgr_u",             ONYX_INTRINSIC_I16X8_AVGR_U },

    { "i32x4_abs",                ONYX_INTRINSIC_I32X4_ABS },
	{ "i32x4_neg",                ONYX_INTRINSIC_I32X4_NEG },
    { "i32x4_any_true",           ONYX_INTRINSIC_I32X4_ANY_TRUE },
	{ "i32x4_all_true",           ONYX_INTRINSIC_I32X4_ALL_TRUE },
    { "i32x4_bitmask",            ONYX_INTRINSIC_I32X4_BITMASK },
    { "i32x4_widen_low_i16x8_s",  ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_S },
	{ "i32x4_widen_high_i16x8_s", ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_S },
    { "i32x4_widen_low_i16x8_u",  ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_U },
	{ "i32x4_widen_high_i16x8_u", ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_U },
    { "i32x4_shl",                ONYX_INTRINSIC_I32X4_SHL },
	{ "i32x4_shr_s",              ONYX_INTRINSIC_I32X4_SHR_S },
	{ "i32x4_shl_u",              ONYX_INTRINSIC_I32X4_SHR_U },
    { "i32x4_add",                ONYX_INTRINSIC_I32X4_ADD },
	{ "i32x4_sub",                ONYX_INTRINSIC_I32X4_SUB },
	{ "i32x4_mul",                ONYX_INTRINSIC_I32X4_MUL },
    { "i32x4_min_s",              ONYX_INTRINSIC_I32X4_MIN_S },
	{ "i32x4_min_u",              ONYX_INTRINSIC_I32X4_MIN_U },
    { "i32x4_max_s",              ONYX_INTRINSIC_I32X4_MAX_S },
	{ "i32x4_max_u",              ONYX_INTRINSIC_I32X4_MAX_U },

    { "i64x2_neg",   ONYX_INTRINSIC_I64X2_NEG },
	{ "i64x2_shl",   ONYX_INTRINSIC_I64X2_SHL },
    { "i64x2_shr_s", ONYX_INTRINSIC_I64X2_SHR_S },
	{ "i64x2_shr_u", ONYX_INTRINSIC_I64X2_SHR_U },
    { "i64x2_add",   ONYX_INTRINSIC_I64X2_ADD },
	{ "i64x2_sub",   ONYX_INTRINSIC_I64X2_SUB },
	{ "i64x2_mul",   ONYX_INTRINSIC_I64X2_MUL },

    { "f32x4_abs",  ONYX_INTRINSIC_F32X4_ABS },
	{ "f32x4_neg",  ONYX_INTRINSIC_F32X4_NEG },
	{ "f32x4_sqrt", ONYX_INTRINSIC_F32X4_SQRT },
    { "f32x4_add",  ONYX_INTRINSIC_F32X4_ADD },
	{ "f32x4_sub",  ONYX_INTRINSIC_F32X4_SUB },
    { "f32x4_mul",  ONYX_INTRINSIC_F32X4_MUL },
	{ "f32x4_div",  ONYX_INTRINSIC_F32X4_DIV },
    { "f32x4_min",  ONYX_INTRINSIC_F32X4_MIN },
	{ "f32x4_max",  ONYX_INTRINSIC_F32X4_MAX },

    { "f64x2_abs",  ONYX_INTRINSIC_F64X2_ABS },
	{ "f64x2_neg",  ONYX_INTRINSIC_F64X2_NEG },
	{ "f64x2_sqrt", ONYX_INTRINSIC_F64X2_SQRT },
    { "f64x2_add",  ONYX_INTRINSIC_F64X2_ADD },
	{ "f64x2_sub",  ONYX_INTRINSIC_F64X2_SUB },
    { "f64x2_mul",  ONYX_INTRINSIC_F64X2_MUL },
	{ "f64x2_div",  ONYX_INTRINSIC_F64X2_DIV },
    { "f64x2_min",  ONYX_INTRINSIC_F64X2_MIN },
	{ "f64x2_max",  ONYX_INTRINSIC_F64X2_MAX },

    { "i32x4_trunc_sat_f32x4_s", ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_S },
    { "i32x4_trunc_sat_f32x4_u", ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_U },
    { "f32x4_convert_i32x4_s",   ONYX_INTRINSIC_F32X4_CONVERT_I32X4_S },
    { "f32x4_convert_i32x4_u",   ONYX_INTRINSIC_F32X4_CONVERT_I32X4_U },

    { "__atomic_wait",    ONYX_INTRINSIC_ATOMIC_WAIT    },
    { "__atomic_notify",  ONYX_INTRINSIC_ATOMIC_NOTIFY  },

    { "__atomic_fence",   ONYX_INTRINSIC_ATOMIC_FENCE   },

    { "__atomic_load",    ONYX_INTRINSIC_ATOMIC_LOAD    },
    { "__atomic_store",   ONYX_INTRINSIC_ATOMIC_STORE   },
    { "__atomic_add",     ONYX_INTRINSIC_ATOMIC_ADD     },
    { "__atomic_sub",     ONYX_INTRINSIC_ATOMIC_SUB     },
    { "__atomic_and",     ONYX_INTRINSIC_ATOMIC_AND     },
    { "__atomic_or",      ONYX_INTRINSIC_ATOMIC_OR      },
    { "__atomic_xor",     ONYX_INTRINSIC_ATOMIC_XOR     },
    { "__atomic_xchg",    ONYX_INTRINSIC_ATOMIC_XCHG    },
    { "__atomic_cmpxchg", ONYX_INTRINSIC_ATOMIC_CMPXCHG },

    { NULL, ONYX_INTRINSIC_UNDEFINED },
};

void prepare_builtins(Context *context) {
    // `types_init()` needs to be called first so the pointers in context->types.basic are valid
    assert(context->types.basic[Basic_Kind_Void]);

    context->basic_types.type_void      = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_void_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_Void]  });
    context->basic_types.type_bool      = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_bool_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_Bool]  });
    context->basic_types.type_i8        = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_i8_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I8]    });
    context->basic_types.type_u8        = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_u8_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_U8]    });
    context->basic_types.type_i16       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_i16_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I16]   });
    context->basic_types.type_u16       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_u16_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_U16]   });
    context->basic_types.type_i32       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_i32_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I32]   });
    context->basic_types.type_u32       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_u32_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_U32]   });
    context->basic_types.type_i64       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_i64_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I64]   });
    context->basic_types.type_u64       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_u64_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_U64]   });
    context->basic_types.type_f32       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_f32_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_F32]   });
    context->basic_types.type_f64       = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_f64_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_F64]   });
    context->basic_types.type_rawptr    = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_rawptr_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_Rawptr] });
    context->basic_types.type_type_expr = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &basic_type_type_expr_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_Type_Index] });

    // NOTE: Types used for numeric literals
    context->basic_types.type_int_unsized   = ((AstBasicType) { Ast_Kind_Basic_Type, 0, NULL, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_Int_Unsized] });
    context->basic_types.type_float_unsized = ((AstBasicType) { Ast_Kind_Basic_Type, 0, NULL, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_Float_Unsized] });

    context->basic_types.type_i8x16 = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I8X16] });
    context->basic_types.type_i16x8 = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I16X8] });
    context->basic_types.type_i32x4 = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I32X4] });
    context->basic_types.type_i64x2 = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_I64X2] });
    context->basic_types.type_f32x4 = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_F32X4] });
    context->basic_types.type_f64x2 = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_F64X2] });
    context->basic_types.type_v128  = ((AstBasicType) { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, NULL, 0, NULL, context->types.basic[Basic_Kind_V128]  });

    // HACK
    // :AutoReturnType
    context->types.auto_return = bh_alloc_item(context->ast_alloc, Type);
    context->basic_types.type_auto_return = ((AstBasicType) { Ast_Kind_Basic_Type, 0, &simd_token, NULL, NULL, 0, NULL, context->types.auto_return });

    // Builtins
    context->builtins.heap_start   = ((AstGlobal) { Ast_Kind_Global, Ast_Flag_Const, &builtin_heap_start_token, NULL, NULL, (AstType *) &context->basic_types.type_rawptr, NULL });
    context->builtins.stack_top    = ((AstGlobal) { Ast_Kind_Global, 0, &builtin_stack_top_token, NULL, NULL, (AstType *) &context->basic_types.type_rawptr, NULL });
    context->builtins.tls_base     = ((AstGlobal) { Ast_Kind_Global, 0, &builtin_tls_base_token, NULL, NULL, (AstType *) &context->basic_types.type_rawptr, NULL });
    context->builtins.tls_size     = ((AstGlobal) { Ast_Kind_Global, 0, &builtin_tls_size_token, NULL, NULL, (AstType *) &context->basic_types.type_u32, NULL });
    context->builtins.closure_base = ((AstGlobal) { Ast_Kind_Global, 0, &builtin_closure_base_token, NULL, NULL, (AstType *) &context->basic_types.type_rawptr, NULL });
    context->builtins.stack_trace  = ((AstGlobal) { Ast_Kind_Global, 0, &builtin_stack_trace_token, NULL, NULL, (AstType *) &context->basic_types.type_rawptr, NULL });

    context->node_that_signals_a_yield.kind = Ast_Kind_Function;
}

void initialize_builtins(Context *context) {
    bh_allocator a = context->gp_alloc;

    BuiltinSymbol* bsym = (BuiltinSymbol *) &builtin_symbols[0];
    while (bsym->sym != NULL) {
        AstNode *node = (AstNode *) bh_pointer_add(context, bsym->offset);

        if (bsym->package == NULL) {
            symbol_builtin_introduce(context, context->global_scope, bsym->sym, node);

        } else {
            Package* p = package_lookup_or_create(context, bsym->package, context->global_scope, context->global_scope->created_at);
            assert(p);

            symbol_builtin_introduce(context, p->scope, bsym->sym, node);
        }

        bsym++;
    }

    Package* p = package_lookup_or_create(context, "builtin", context->global_scope, context->global_scope->created_at);

    context->builtins.string_type = (AstType *) symbol_raw_resolve(context, p->scope, "str");
    if (context->builtins.string_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'str' struct not found in builtin package.");
        return;
    }

    context->builtins.cstring_type = (AstType *) symbol_raw_resolve(context, p->scope, "cstr");
    if (context->builtins.cstring_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'cstr' type not found in builtin package.");
        return;
    }

    context->builtins.range_type = (AstType *) symbol_raw_resolve(context, p->scope, "range");
    if (context->builtins.range_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'range' struct not found in builtin package.");
        return;
    }

    context->builtins.range64_type = (AstType *) symbol_raw_resolve(context, p->scope, "range64");
    if (context->builtins.range64_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'range64' struct not found in builtin package.");
        return;
    }

    context->builtins.vararg_type = (AstType *) symbol_raw_resolve(context, p->scope, "vararg");
    if (context->builtins.range_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'vararg' struct not found in builtin package.");
        return;
    }

    context->builtins.context_variable = (AstTyped *) symbol_raw_resolve(context, p->scope, "context");
    if (context->builtins.context_variable == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'context' variable not found in builtin package.");
        return;
    }

    context->builtins.allocator_type = (AstType *) symbol_raw_resolve(context, p->scope, "Allocator");
    if (context->builtins.allocator_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Allocator' struct not found in builtin package.");
        return;
    }

    context->builtins.iterator_type = (AstType *) symbol_raw_resolve(context, p->scope, "Iterator");
    if (context->builtins.iterator_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Iterator' struct not found in builtin package.");
        return;
    }

    context->builtins.optional_type = (AstType *) symbol_raw_resolve(context, p->scope, "Optional");
    if (context->builtins.optional_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Optional' struct not found in builtin package.");
        return;
    }

    context->builtins.callsite_type = (AstType *) symbol_raw_resolve(context, p->scope, "CallSite");
    if (context->builtins.callsite_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'CallSite' struct not found in builtin package.");
        return;
    }

    context->builtins.any_type = (AstType *) symbol_raw_resolve(context, p->scope, "any");
    if (context->builtins.any_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'any' struct not found in builtin package.");
        return;
    }

    context->builtins.code_type = (AstType *) symbol_raw_resolve(context, p->scope, "Code");
    if (context->builtins.code_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Code' struct not found in builtin package.");
        return;
    }

    context->builtins.array_type = (AstType *) symbol_raw_resolve(context, p->scope, "Array");
    if (context->builtins.array_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Array' struct not found in builtin package.");
        return;
    }

    context->builtins.slice_type = (AstType *) symbol_raw_resolve(context, p->scope, "Slice");
    if (context->builtins.slice_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Slice' struct not found in builtin package.");
        return;
    }

    context->builtins.initialize_data_segments = (AstFunction *) symbol_raw_resolve(context, p->scope, "__initialize_data_segments");
    if (context->builtins.initialize_data_segments == NULL || context->builtins.initialize_data_segments->kind != Ast_Kind_Function) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__initialize_data_segments' procedure not found in builtin package.");
        return;
    }

    context->builtins.run_init_procedures = (AstFunction *) symbol_raw_resolve(context, p->scope, "__run_init_procedures");
    if (context->builtins.run_init_procedures == NULL || context->builtins.run_init_procedures->kind != Ast_Kind_Function) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__run_init_procedures' procedure not found.");
        return;
    }

    context->builtins.implicit_bool_cast = (AstOverloadedFunction *) symbol_raw_resolve(context, p->scope, "__implicit_bool_cast");
    if (context->builtins.implicit_bool_cast == NULL || context->builtins.implicit_bool_cast->kind != Ast_Kind_Overloaded_Function) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__implicit_bool_cast' #match procedure not found.");
        return;
    }

    context->builtins.dispose_used_local = (AstOverloadedFunction *) symbol_raw_resolve(context, p->scope, "__dispose_used_local");
    if (context->builtins.dispose_used_local == NULL || context->builtins.dispose_used_local->kind != Ast_Kind_Overloaded_Function) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__dispose_used_local' #match procedure not found.");
        return;
    }

    context->builtins.for_expansion = (AstOverloadedFunction *) symbol_raw_resolve(context, p->scope, "__for_expansion");
    if (context->builtins.for_expansion == NULL || context->builtins.for_expansion->kind != Ast_Kind_Overloaded_Function) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__for_expansion' #match procedure not found.");
        return;
    }

    context->builtins.for_expansion_flag_type = (AstType *) symbol_raw_resolve(context, p->scope, "__For_Expansion_Flags");
    if (context->builtins.for_expansion_flag_type == NULL || context->builtins.for_expansion_flag_type->kind != Ast_Kind_Enum_Type) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__For_Expansion_Flags' enum procedure not found.");
        return;
    }

    context->builtins.closure_block_allocate = (AstFunction *) symbol_raw_resolve(context, p->scope, "__closure_block_allocate");
    if (context->builtins.closure_block_allocate == NULL || context->builtins.closure_block_allocate->kind != Ast_Kind_Function) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'__closure_block_allocate' procedure not found.");
        return;
    }


    context->builtins.link_options_type = (AstType *) symbol_raw_resolve(context, p->scope, "Link_Options");
    if (context->builtins.link_options_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'Link_Options' type not found.");
        return;
    }

    context->builtins.package_id_type = (AstType *) symbol_raw_resolve(context, p->scope, "package_id");
    if (context->builtins.package_id_type == NULL) {
        ONYX_ERROR((OnyxFilePos) { 0 }, Error_Critical, "'package_id' type not found.");
        return;
    }

    context->builtins.init_procedures = NULL;
    bh_arr_new(context->gp_alloc, context->builtins.init_procedures, 4);

    fori (i, 0, Binary_Op_Count) {
        context->operator_overloads[i] = NULL;
        bh_arr_new(context->gp_alloc, context->operator_overloads[i], 4);
    }

    fori (i, 0, Unary_Op_Count) {
        context->unary_operator_overloads[i] = NULL;
        bh_arr_new(context->gp_alloc, context->unary_operator_overloads[i], 4);
    }
}

void initalize_special_globals(Context *context) {
    Package *p = package_lookup(context, "runtime.info");
    if (p != NULL) {
        context->builtins.type_table_node     = (AstTyped *) symbol_raw_resolve(context, p->scope, "type_table");
        context->builtins.foreign_blocks_node = (AstTyped *) symbol_raw_resolve(context, p->scope, "foreign_blocks");
        context->builtins.foreign_block_type  = (AstType *)  symbol_raw_resolve(context, p->scope, "foreign_block");
        context->builtins.tagged_procedures_node = (AstTyped *) symbol_raw_resolve(context, p->scope, "tagged_procedures");
        context->builtins.tagged_globals_node = (AstTyped *) symbol_raw_resolve(context, p->scope, "tagged_globals");

        if (context->options->stack_trace_enabled) {
            context->builtins.stack_trace_type = (AstType *) symbol_raw_resolve(context, p->scope, "Stack_Trace");
        }
    }
}

void introduce_build_options(Context *context) {
    bh_allocator a = context->ast_alloc;

    Package* p = package_lookup_or_create(context, "runtime", context->global_scope, context->global_scope->created_at);

    // HACK creating this for later
    package_lookup_or_create(context, "runtime.vars", p->scope, context->global_scope->created_at);

    AstType* Runtime_Type = (AstType *) symbol_raw_resolve(context, p->scope, "Runtime");
    if (Runtime_Type == NULL) {
        ONYX_ERROR((OnyxFilePos) {0}, Error_Critical, "'Runtime' type not found in package runtime.");
        return;
    }

    AstNumLit* runtime_type = make_int_literal(context, context->options->runtime);
    runtime_type->type_node = Runtime_Type;
    add_entities_for_node(&context->entities, NULL, (AstNode *) runtime_type, NULL, NULL);
    symbol_builtin_introduce(context, p->scope, "runtime", (AstNode *) runtime_type);

    AstNumLit* multi_threaded = make_int_literal(context, context->options->use_multi_threading);
    multi_threaded->type_node = (AstType *) &context->basic_types.type_bool;
    symbol_builtin_introduce(context, p->scope, "Multi_Threading_Enabled", (AstNode *) multi_threaded);

    AstNumLit* debug_mode = make_int_literal(context, context->options->debug_info_enabled);
    debug_mode->type_node = (AstType *) &context->basic_types.type_bool;
    symbol_builtin_introduce(context, p->scope, "Debug_Mode_Enabled", (AstNode *) debug_mode);

    AstNumLit* stack_trace = make_int_literal(context, context->options->stack_trace_enabled);
    stack_trace->type_node = (AstType *) &context->basic_types.type_bool;
    symbol_builtin_introduce(context, p->scope, "Stack_Trace_Enabled", (AstNode *) stack_trace);

    AstNumLit* version_major = make_int_literal(context, VERSION_MAJOR);
    version_major->type_node = (AstType *) &context->basic_types.type_i32;
    AstNumLit* version_minor = make_int_literal(context, VERSION_MINOR);
    version_minor->type_node = (AstType *) &context->basic_types.type_i32;
    AstNumLit* version_patch = make_int_literal(context, VERSION_PATCH);
    version_patch->type_node = (AstType *) &context->basic_types.type_i32;
    symbol_builtin_introduce(context, p->scope, "onyx_version_major", (AstNode *) version_major);
    symbol_builtin_introduce(context, p->scope, "onyx_version_minor", (AstNode *) version_minor);
    symbol_builtin_introduce(context, p->scope, "onyx_version_patch", (AstNode *) version_patch);


    i32 os;
    #ifdef _BH_LINUX
        os = 1;
    #endif
    #ifdef _BH_WINDOWS
        os = 2;
    #endif
    #ifdef _BH_DARWIN
        os = 3;
    #endif

    AstType* OS_Type = (AstType *) symbol_raw_resolve(context, p->scope, "OS");
    if (OS_Type == NULL) {
        ONYX_ERROR((OnyxFilePos) {0}, Error_Critical, "'OS' type not found in package runtime.");
        return;
    }

    AstNumLit* os_type = make_int_literal(context, os);
    os_type->type_node = OS_Type;
    add_entities_for_node(&context->entities, NULL, (AstNode *) os_type, NULL, NULL);
    symbol_builtin_introduce(context, p->scope, "compiler_os", (AstNode *) os_type);

    i32 arch = 0;
    #if defined(__x86_64__) || defined(_M_X64)
        arch = 1; // X86_64;
    #elif defined(i386) || defined(__i386__) || defined(__i386) || defined(_M_IX86)
        arch = 2; // X86_32;
    #elif defined(__aarch64__) || defined(_M_ARM64)
        arch = 3; // AARCH64;
    #endif

    AstType* Arch_Type = (AstType *) symbol_raw_resolve(context, p->scope, "Arch");
    if (Arch_Type == NULL) {
        ONYX_ERROR((OnyxFilePos) {0}, Error_Critical, "'Arch' type not found in package runtime.");
        return;
    }

    AstNumLit* arch_type = make_int_literal(context, arch);
    arch_type->type_node = Arch_Type;
    add_entities_for_node(&context->entities, NULL, (AstNode *) arch_type, NULL, NULL);
    symbol_builtin_introduce(context, p->scope, "arch", (AstNode *) arch_type);

    if (context->options->generate_foreign_info) {
        AstNumLit* foreign_info = make_int_literal(context, 1);
        foreign_info->type_node = (AstType *) &context->basic_types.type_bool;
        symbol_builtin_introduce(context, p->scope, "Generated_Foreign_Info", (AstNode *) foreign_info);
    }
}

