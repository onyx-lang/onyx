#ifndef ONYXWASM_H
#define ONYXWASM_H

#include "bh.h"

#include "onyxparser.h"

typedef u8 WasmType;

extern const WasmType WASM_TYPE_INT32;
extern const WasmType WASM_TYPE_INT64;
extern const WasmType WASM_TYPE_FLOAT32;
extern const WasmType WASM_TYPE_FLOAT64;

typedef struct WasmFuncType {
	// NOTE: For now, WASM only allows for 1 return value.
	// This may be lifted in the future.
	i32 param_count;
	WasmType return_type;
	WasmType param_types[];
} WasmFuncType;


typedef enum WasmInstructionType {
	WI_UNREACHABLE					= 0x00,
	WI_NOP							= 0x01,

	// NOTE: Control flow
	WI_BLOCK_START					= 0x02,
	WI_BLOCK_END					= 0x0B, // NOTE: These ends are not unique
	WI_LOOP_START					= 0x03,
	WI_LOOP_END						= 0x0B,
	WI_IF_START						= 0x04,
	WI_ELSE							= 0x05,
	WI_IF_END						= 0x0B,
	WI_JUMP							= 0x0C,
	WI_COND_JUMP					= 0x0D,
	WI_JUMP_TABLE					= 0x0E,
	WI_RETURN						= 0x0F,
	WI_CALL							= 0x10,
	WI_CALL_INDIRECT				= 0x11,

	// NOTE: Parametric instructions
	WI_DROP							= 0x1A,
	WI_SELECT						= 0x1B,

	// NOTE: Variable instructions
	WI_LOCAL_GET					= 0x20,
	WI_LOCAL_SET					= 0x21,
	WI_LOCAL_TEE					= 0x22,
	WI_GLOBAL_GET					= 0x23,
	WI_GLOBAL_SET					= 0x24,

	// NOTE: Memory instructions
	WI_I32_LOAD						= 0x28,
	WI_I64_LOAD						= 0x29,
	WI_F32_LOAD						= 0x2A,
	WI_F64_LOAD						= 0x2B,
	WI_I32_LOAD_8_S					= 0x2C,
	WI_I32_LOAD_8_U					= 0x2D,
	WI_I32_LOAD_16_S				= 0x2E,
	WI_I32_LOAD_16_U				= 0x2F,
	WI_I64_LOAD_8_S					= 0x30,
	WI_I64_LOAD_8_U					= 0x31,
	WI_I64_LOAD_16_S				= 0x32,
	WI_I64_LOAD_16_U				= 0x33,
	WI_I64_LOAD_32_S				= 0x34,
	WI_I64_LOAD_32_U				= 0x35,
	WI_I32_STORE					= 0x36,
	WI_I64_STORE					= 0x37,
	WI_F32_STORE					= 0x38,
	WI_F64_STORE					= 0x39,
	WI_I32_STORE_8					= 0x3A,
	WI_I32_STORE_16					= 0x3B,
	WI_I64_STORE_8					= 0x3C,
	WI_I64_STORE_16					= 0x3D,
	WI_I64_STORE_32					= 0x3E,
	WI_MEMORY_SIZE					= 0x3F,
	WI_MEMORY_GROW					= 0x40,

	// NOTE: Numeric Instructions
	WI_I32_CONST					= 0x41,
	WI_I64_CONST					= 0x42,
	WI_F32_CONST					= 0x43,
	WI_F64_CONST					= 0x44,

	WI_I32_EQZ						= 0x45, // NOTE: Autoincremented from here
	WI_I32_EQ,
	WI_I32_NE,
	WI_I32_LT_S,
	WI_I32_LT_U,
	WI_I32_GT_S,
	WI_I32_GT_U,
	WI_I32_LE_S,
	WI_I32_LE_U,
	WI_I32_GE_S,
	WI_I32_GE_U,

	WI_I64_EQZ,
	WI_I64_EQ,
	WI_I64_NE,
	WI_I64_LT_S,
	WI_I64_LT_U,
	WI_I64_GT_S,
	WI_I64_GT_U,
	WI_I64_LE_S,
	WI_I64_LE_U,
	WI_I64_GE_S,
	WI_I64_GE_U,

	WI_F32_EQ,
	WI_F32_NE,
	WI_F32_LT,
	WI_F32_GT,
	WI_F32_LE,
	WI_F32_GE,

	WI_F64_EQ,
	WI_F64_NE,
	WI_F64_LT,
	WI_F64_GT,
	WI_F64_LE,
	WI_F64_GE,

	WI_I32_CLZ,
	WI_I32_CTZ,
	WI_I32_POPCNT,
	WI_I32_ADD,
	WI_I32_SUB,
	WI_I32_MUL,
	WI_I32_DIV_S,
	WI_I32_DIV_U,
	WI_I32_REM_S,
	WI_I32_REM_U,
	WI_I32_AND,
	WI_I32_OR,
	WI_I32_XOR,
	WI_I32_SHL,
	WI_I32_SHR_S,
	WI_I32_SHR_U,
	WI_I32_ROTL,
	WI_I32_ROTR,

	WI_I64_CLZ,
	WI_I64_CTZ,
	WI_I64_POPCNT,
	WI_I64_ADD,
	WI_I64_SUB,
	WI_I64_MUL,
	WI_I64_DIV_S,
	WI_I64_DIV_U,
	WI_I64_REM_S,
	WI_I64_REM_U,
	WI_I64_AND,
	WI_I64_OR,
	WI_I64_XOR,
	WI_I64_SHL,
	WI_I64_SHR_S,
	WI_I64_SHR_U,
	WI_I64_ROTL,
	WI_I64_ROTR,

	WI_F32_ABS,
	WI_F32_NEG,
	WI_F32_CEIL,
	WI_F32_FLOOR,
	WI_F32_TRUNC,
	WI_F32_NEAREST,
	WI_F32_SQRT,
	WI_F32_ADD,
	WI_F32_SUB,
	WI_F32_MUL,
	WI_F32_DIV,
	WI_F32_MIN,
	WI_F32_MAX,
	WI_F32_COPYSIGN,

	WI_F64_ABS,
	WI_F64_NEG,
	WI_F64_CEIL,
	WI_F64_FLOOR,
	WI_F64_TRUNC,
	WI_F64_NEAREST,
	WI_F64_SQRT,
	WI_F64_ADD,
	WI_F64_SUB,
	WI_F64_MUL,
	WI_F64_DIV,
	WI_F64_MIN,
	WI_F64_MAX,
	WI_F64_COPYSIGN,

	WI_I32_FROM_I64					= 0xA7,
	WI_I32_FROM_F32_S				= 0xA8,
	WI_I32_FROM_F32_U				= 0xA9,
	WI_I32_FROM_F64_S				= 0xAA,
	WI_I32_FROM_F64_U				= 0xAB,

	WI_I64_FROM_I32_S				= 0xAC,
	WI_I64_FROM_I32_U				= 0xAD,
	WI_I64_FROM_F32_S				= 0xAE,
	WI_I64_FROM_F32_U				= 0xAF,
	WI_I64_FROM_F64_S				= 0xB0,
	WI_I64_FROM_F64_U				= 0xB1,

	WI_F32_FROM_I32_S				= 0xB2,
	WI_F32_FROM_I32_U				= 0xB3,
	WI_F32_FROM_I64_S				= 0xB4,
	WI_F32_FROM_I64_U				= 0xB5,
	WI_F32_FROM_F64					= 0xB6,

	WI_F64_FROM_I32_S				= 0xB7,
	WI_F64_FROM_I32_U				= 0xB8,
	WI_F64_FROM_I64_S				= 0xB9,
	WI_F64_FROM_I64_U				= 0xBA,
	WI_F64_FROM_F32					= 0xBB,

	WI_I32_REINTERPRET_F32			= 0xBC,
	WI_I64_REINTERPRET_F64			= 0xBD,
	WI_F32_REINTERPRET_I32			= 0xBE,
	WI_F64_REINTERPRET_I64			= 0xBF,
} WasmInstructionType;

typedef union {
	struct {
		u32 i1, i2;
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

typedef struct WasmFuncLocals {
	u8 i32_count;
	u8 i64_count;
	u8 f32_count;
	u8 f64_count;
} WasmFuncLocals;

typedef struct WasmFunc {
	i32 type_idx;
	WasmFuncLocals locals;
	bh_arr(WasmInstruction) code;
} WasmFunc;

typedef enum WasmExportKind {
	WASM_EXPORT_FUNCTION = 0x00,
	WASM_EXPORT_TABLE	 = 0x01,
	WASM_EXPORT_MEMORY	 = 0x02,
	WASM_EXPORT_GLOBAL	 = 0x03,
} WasmExportKind;

typedef struct WasmExport {
	WasmExportKind kind;
	i32 idx;
} WasmExport;

typedef struct OnyxWasmModule {
	bh_allocator allocator;

	// NOTE: Mapping from local ast node ptrs to indicies
	bh_imap local_map;

	// NOTE: Used internally as a map from strings that represent function types,
	// 0x7f 0x7f : 0x7f ( (i32, i32) -> i32 )
	// to the function type index if it has been created.
	bh_table(i32) type_map;
	i32 next_type_idx;
	// NOTE: This have to be pointers because the type is variadic in size
	bh_arr(WasmFuncType*) functypes;

	bh_arr(WasmFunc) funcs;
    // NOTE: Maps from ast node pointers to the function index
    bh_imap func_map;
	i32 next_func_idx;

	bh_table(WasmExport) exports;
	i32 export_count;
} OnyxWasmModule;

OnyxWasmModule onyx_wasm_generate_module(bh_allocator alloc, OnyxAstNode* program);
void onyx_wasm_module_free(OnyxWasmModule* module);
void onyx_wasm_module_write_to_file(OnyxWasmModule* module, bh_file file);

#endif
