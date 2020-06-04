#ifndef ONYXWASM_H
#define ONYXWASM_H

#define BH_NO_STRING
#include "bh.h"

#include "onyxparser.h"

typedef enum WasmType : char {
	WASM_TYPE_INT32 = 0x7F,
	WASM_TYPE_INT64 = 0x7E,
	WASM_TYPE_FLOAT32 = 0x7D,
	WASM_TYPE_FLOAT64 = 0x7C
} WasmType;

typedef struct WasmFuncType {
	// NOTE: For now, WASM only allows for 1 return value.
	// This may be lifted in the future.
	WasmType return_type;
	i32 param_count;
	WasmType param_types[];
} WasmFuncType;

typedef struct WasmFunc {
	WasmFuncType* type;
	i32 idx;
} WasmFunc;

typedef struct OnyxWasmModule {
	bh_allocator allocator;

	// NOTE: Used internally as a map from strings that represent function types,
	// 0x7f 0x7f : 0x7f ( (i32, i32) -> i32 )
	// to the function type index if it has been created.
	bh_hash(i32) type_map;
	i32 curr_type_idx;

	bh_arr(WasmFuncType*) functypes;
	bh_arr(WasmFunc) funcs;
} OnyxWasmModule;

OnyxWasmModule onyx_wasm_generate_module(bh_allocator alloc, OnyxAstNode* program);
void onyx_wasm_module_free(OnyxWasmModule* module);

#endif
