#include "astnodes.h"
#include "types.h"
#include "errors.h"
#include "utils.h"

AstBasicType basic_type_void      = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_Void]  };
AstBasicType basic_type_bool      = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_Bool]  };
AstBasicType basic_type_i8        = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I8]    };
AstBasicType basic_type_u8        = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_U8]    };
AstBasicType basic_type_i16       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I16]   };
AstBasicType basic_type_u16       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_U16]   };
AstBasicType basic_type_i32       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I32]   };
AstBasicType basic_type_u32       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_U32]   };
AstBasicType basic_type_i64       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I64]   };
AstBasicType basic_type_u64       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_U64]   };
AstBasicType basic_type_f32       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_F32]   };
AstBasicType basic_type_f64       = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_F64]   };
AstBasicType basic_type_rawptr    = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_Rawptr] };
AstBasicType basic_type_type_expr = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_Type_Index] };

// NOTE: Types used for numeric literals
AstBasicType basic_type_int_unsized   = { Ast_Kind_Basic_Type, 0, NULL, NULL, 0,   NULL, NULL, &basic_types[Basic_Kind_Int_Unsized] };
AstBasicType basic_type_float_unsized = { Ast_Kind_Basic_Type, 0, NULL, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_Float_Unsized] };

static OnyxToken simd_token = { Token_Type_Symbol, 0, "", { 0 } };
AstBasicType basic_type_i8x16 = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I8X16] };
AstBasicType basic_type_i16x8 = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I16X8] };
AstBasicType basic_type_i32x4 = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I32X4] };
AstBasicType basic_type_i64x2 = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_I64X2] };
AstBasicType basic_type_f32x4 = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_F32X4] };
AstBasicType basic_type_f64x2 = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0, NULL, NULL, &basic_types[Basic_Kind_F64X2] };
AstBasicType basic_type_v128  = { Ast_Kind_Basic_Type, Ast_Flag_Comptime, &simd_token, NULL, 0,  NULL, NULL, &basic_types[Basic_Kind_V128]  };

// HACK
// :AutoReturnType
Type         type_auto_return = { 0 };
AstBasicType basic_type_auto_return = { Ast_Kind_Basic_Type, 0, &simd_token, NULL, 0, NULL, NULL, &type_auto_return };

OnyxToken builtin_package_token = { Token_Type_Symbol, 7, "builtin ", { 0 } };

static OnyxToken builtin_heap_start_token = { Token_Type_Symbol, 12, "__heap_start ", { 0 } };
static OnyxToken builtin_stack_top_token  = { Token_Type_Symbol, 11, "__stack_top ",  { 0 } };
static OnyxToken builtin_tls_base_token   = { Token_Type_Symbol, 10, "__tls_base ",  { 0 } };
static OnyxToken builtin_tls_size_token   = { Token_Type_Symbol, 10, "__tls_size ",  { 0 } };
AstNumLit builtin_heap_start  = { Ast_Kind_NumLit, Ast_Flag_Const, &builtin_heap_start_token, NULL, NULL, (AstType *) &basic_type_rawptr, NULL, 0 };
AstGlobal builtin_stack_top   = { Ast_Kind_Global, Ast_Flag_Global_Stack_Top,  &builtin_stack_top_token, NULL, NULL, (AstType *) &basic_type_rawptr, NULL };
AstGlobal builtin_tls_base    = { Ast_Kind_Global, 0, &builtin_tls_base_token, NULL, NULL, (AstType *) &basic_type_rawptr, NULL };
AstGlobal builtin_tls_size    = { Ast_Kind_Global, 0, &builtin_tls_size_token, NULL, NULL, (AstType *) &basic_type_u32, NULL };

AstType  *builtin_string_type;
AstType  *builtin_range_type;
Type     *builtin_range_type_type;
AstType  *builtin_vararg_type;
Type     *builtin_vararg_type_type;
AstTyped *builtin_context_variable;
AstType  *builtin_allocator_type;
AstType  *builtin_iterator_type;
AstType  *builtin_callsite_type;
AstType  *builtin_any_type;
AstType  *builtin_code_type;

AstTyped *type_table_node = NULL;

const BuiltinSymbol builtin_symbols[] = {
    { NULL, "void",       (AstNode *) &basic_type_void },
    { NULL, "bool",       (AstNode *) &basic_type_bool },
    { NULL, "i8",         (AstNode *) &basic_type_i8 },
    { NULL, "u8",         (AstNode *) &basic_type_u8 },
    { NULL, "i16",        (AstNode *) &basic_type_i16 },
    { NULL, "u16",        (AstNode *) &basic_type_u16 },
    { NULL, "i32",        (AstNode *) &basic_type_i32 },
    { NULL, "u32",        (AstNode *) &basic_type_u32 },
    { NULL, "i64",        (AstNode *) &basic_type_i64 },
    { NULL, "u64",        (AstNode *) &basic_type_u64 },
    { NULL, "f32",        (AstNode *) &basic_type_f32 },
    { NULL, "f64",        (AstNode *) &basic_type_f64 },
    { NULL, "rawptr",     (AstNode *) &basic_type_rawptr },
    { NULL, "type_expr",  (AstNode *) &basic_type_type_expr },

    { "simd", "i8x16",    (AstNode *) &basic_type_i8x16 },
    { "simd", "i16x8",    (AstNode *) &basic_type_i16x8 },
    { "simd", "i32x4",    (AstNode *) &basic_type_i32x4 },
    { "simd", "i64x2",    (AstNode *) &basic_type_i64x2 },
    { "simd", "f32x4",    (AstNode *) &basic_type_f32x4 },
    { "simd", "f64x2",    (AstNode *) &basic_type_f64x2 },
    { "simd", "v128",     (AstNode *) &basic_type_v128  },

    { "builtin", "__heap_start", (AstNode *) &builtin_heap_start },
    { "builtin", "__stack_top",  (AstNode *) &builtin_stack_top },
    { "builtin", "__tls_base",   (AstNode *) &builtin_tls_base },
    { "builtin", "__tls_size",   (AstNode *) &builtin_tls_size },

    { NULL, NULL, NULL },
};

bh_table(OnyxIntrinsic) intrinsic_table;

static IntrinsicMap builtin_intrinsics[] = {
    { "memory_size",  ONYX_INTRINSIC_MEMORY_SIZE },
    { "memory_grow",  ONYX_INTRINSIC_MEMORY_GROW },
    { "memory_copy",  ONYX_INTRINSIC_MEMORY_COPY },
    { "memory_fill",  ONYX_INTRINSIC_MEMORY_FILL },

    { "__initialize", ONYX_INTRINSIC_INITIALIZE },
    { "__zero_value", ONYX_INTRINSIC_ZERO_VALUE },

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

bh_arr(OverloadOption) operator_overloads[Binary_Op_Count] = { 0 };

void initialize_builtins(bh_allocator a) {
    // HACK
    builtin_package_token.text = bh_strdup(global_heap_allocator, builtin_package_token.text);

    BuiltinSymbol* bsym = (BuiltinSymbol *) &builtin_symbols[0];
    while (bsym->sym != NULL) {
        if (bsym->package == NULL)
            symbol_builtin_introduce(context.global_scope, bsym->sym, bsym->node);
        else {
            Package* p = package_lookup_or_create(bsym->package, context.global_scope, a);
            assert(p);

            symbol_builtin_introduce(p->scope, bsym->sym, bsym->node);
        }
        bsym++;
    }

    Package* p = package_lookup_or_create("builtin", context.global_scope, a);

    builtin_string_type = (AstType *) symbol_raw_resolve(p->scope, "str");
    if (builtin_string_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'str' struct not found in builtin package.");
        return;
    }

    builtin_range_type = (AstType *) symbol_raw_resolve(p->scope, "range");
    if (builtin_range_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'range' struct not found in builtin package.");
        return;
    }

    builtin_vararg_type = (AstType *) symbol_raw_resolve(p->scope, "vararg");
    if (builtin_range_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'vararg' struct not found in builtin package.");
        return;
    }

    builtin_context_variable = (AstTyped *) symbol_raw_resolve(p->scope, "context");
    if (builtin_context_variable == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'context' variable not found in builtin package.");
        return;
    }

    builtin_allocator_type = (AstType *) symbol_raw_resolve(p->scope, "Allocator");
    if (builtin_allocator_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'Allocator' struct not found in builtin package.");
        return;
    }

    builtin_iterator_type = (AstType *) symbol_raw_resolve(p->scope, "Iterator");
    if (builtin_iterator_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'Iterator' struct not found in builtin package.");
        return;
    }

    builtin_callsite_type = (AstType *) symbol_raw_resolve(p->scope, "CallSite");
    if (builtin_callsite_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'CallSite' struct not found in builtin package.");
        return;
    }

    builtin_any_type = (AstType *) symbol_raw_resolve(p->scope, "any");
    if (builtin_any_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'any' struct not found in builtin package.");
        return;
    }

    builtin_code_type = (AstType *) symbol_raw_resolve(p->scope, "Code");
    if (builtin_code_type == NULL) {
        onyx_report_error((OnyxFilePos) { 0 }, "'Code' struct not found in builtin package.");
        return;
    }

    p = package_lookup("builtin.type_info");
    if (p != NULL) {
        type_table_node = (AstTyped *) symbol_raw_resolve(p->scope, "type_table");
    }

    fori (i, 0, Binary_Op_Count) {
        bh_arr_new(global_heap_allocator, operator_overloads[i], 4); 
    }

    bh_table_init(global_heap_allocator, intrinsic_table, 128);
    IntrinsicMap* intrinsic = &builtin_intrinsics[0];
    while (intrinsic->name != NULL) {
        bh_table_put(OnyxIntrinsic, intrinsic_table, intrinsic->name, intrinsic->intrinsic);
        intrinsic++;
    }
}

void introduce_build_options(bh_allocator a) {
    Package* p = package_lookup_or_create("runtime", context.global_scope, a);

    AstNumLit* runtime_type = make_int_literal(a, context.options->runtime);
    symbol_builtin_introduce(p->scope, "Runtime", (AstNode *) runtime_type);

    AstNumLit* multi_threaded = make_int_literal(a, context.options->use_multi_threading);
    multi_threaded->type_node = (AstType *) &basic_type_bool;
    symbol_builtin_introduce(p->scope, "Multi_Threading_Enabled", (AstNode *) multi_threaded);
}
