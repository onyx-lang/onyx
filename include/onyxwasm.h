#ifndef ONYXWASM_H
#define ONYXWASM_H

#define BH_NO_STRING
#include "bh.h"

#include "onyxparser.h"

enum WasmType {
	WASM_TYPE_INT32 = 0x7F,
	WASM_TYPE_INT64 = 0x7E,
	WASM_TYPE_FLOAT32 = 0x7D,
	WASM_TYPE_FLOAT64 = 0x7C
};

typedef struct OnyxWasmModule {

} OnyxWasmModule;

OnyxWasmModule onyx_wasm_generate_module(bh_allocator alloc, OnyxAstNode* program);

#endif
