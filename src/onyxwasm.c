#include "onyxwasm.h"

// NOTE: Allows easier testing of types since most of the characters
// corresponding to these values are not printable
#if 0
const WasmType WASM_TYPE_INT32 = 0x7F;
const WasmType WASM_TYPE_INT64 = 0x7E;
const WasmType WASM_TYPE_FLOAT32 = 0x7D;
const WasmType WASM_TYPE_FLOAT64 = 0x7C;
#else
const WasmType WASM_TYPE_INT32 = 'A';
const WasmType WASM_TYPE_INT64 = 'B';
const WasmType WASM_TYPE_FLOAT32 = 'C';
const WasmType WASM_TYPE_FLOAT64 = 'D';
#endif

static WasmType onyx_type_to_wasm_type(OnyxTypeInfo* type) {
	if (type->is_bool) return WASM_TYPE_INT32;
	else if (type->is_int) {
		if (type->size == 4) return WASM_TYPE_INT32;
		if (type->size == 8) return WASM_TYPE_INT64;
	}
	else if (type->is_float) {
		if (type->size == 4) return WASM_TYPE_FLOAT32;
		if (type->size == 8) return WASM_TYPE_FLOAT64;
	}

	// TODO: Should produce an error message if this isn't successful
	return WASM_TYPE_INT32;
}

static void process_function_definition(OnyxWasmModule* mod, OnyxAstNodeFuncDef* fd) {
	static char type_repr_buf[128];

	char* t = type_repr_buf;
	OnyxAstNodeParam* param = fd->params;
	i32 param_count = 0;
	while (param) {
		// HACK: Using these directly as part of a string feels weird but they are
		// valid characters so I don't think it is going to be much of an issue
		*(t++) = (char) onyx_type_to_wasm_type(param->type);
		param_count++;
		param = param->next;
	}
	*(t++) = ':';

	WasmType return_type = onyx_type_to_wasm_type(fd->return_type);
	*(t++) = (char) return_type;
	*t = '\0';

	i32 type_idx = 0;
	if (bh_hash_has(i32, mod->type_map, type_repr_buf)) {
		type_idx = bh_hash_get(i32, mod->type_map, type_repr_buf);
	} else {
		// NOTE: Make a new type
		// TODO: Ensure that this isn't going to break things because of alignment
		WasmFuncType* type = (WasmFuncType*) bh_alloc(mod->allocator, sizeof(WasmFuncType) + sizeof(WasmType) * param_count);
		type->return_type = return_type;
		type->param_count = param_count;

		// HACK ish thing
	 	memcpy(type->param_types, type_repr_buf, type->param_count);

		bh_arr_push(mod->functypes, type);

		bh_hash_put(i32, mod->type_map, type_repr_buf, mod->next_type_idx);
		type_idx = mod->next_type_idx;
		mod->next_type_idx++;
	}

	WasmFunc wasm_func = {
		.type_idx = type_idx
	};
	bh_arr_push(mod->funcs, wasm_func);
	i32 func_idx = mod->next_func_idx++;

	if (fd->flags & ONYX_AST_FLAG_EXPORTED) {
		onyx_token_null_toggle(*fd->token);

		WasmExport wasm_export = {
			.kind = WASM_EXPORT_FUNCTION,
			.idx = func_idx,
		};
		bh_hash_put(WasmExport, mod->exports, fd->token->token, wasm_export);

		onyx_token_null_toggle(*fd->token);
	}
}

OnyxWasmModule onyx_wasm_generate_module(bh_allocator alloc, OnyxAstNode* program) {
	OnyxWasmModule module = {
		.allocator = alloc,

		.type_map = NULL,
		.next_type_idx = 0,
		.functypes = NULL,

		.funcs = NULL,
		.next_func_idx = 0,

		.exports = NULL,
	};

	bh_arr_new(alloc, module.functypes, 4);
	bh_arr_new(alloc, module.funcs, 4);

	bh_hash_init(bh_heap_allocator(), module.type_map);
	bh_hash_init(bh_heap_allocator(), module.exports);

	OnyxAstNode* walker = program;
	while (walker) {
		switch (walker->kind) {
			case ONYX_AST_NODE_KIND_FUNCDEF:
				process_function_definition(&module, &walker->as_funcdef);
				break;
			default: break;
		}

		walker = walker->next;
	}

	return module;
}

void onyx_wasm_module_free(OnyxWasmModule* module) {
	bh_arr_free(module->functypes);
	bh_arr_free(module->funcs);
}
