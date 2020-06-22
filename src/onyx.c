// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxsempass.h"
#include "onyxutils.h"
#include "onyxwasm.h"

int main(int argc, char *argv[]) {

	bh_allocator alloc = bh_heap_allocator();

    bh_scratch_init(&global_scratch, alloc, 16 * 1024); // NOTE: 16 KB
    global_scratch_allocator = bh_scratch_allocator(&global_scratch);

	bh_file source_file;
	bh_file_error err = bh_file_open(&source_file, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		bh_printf_err("Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	// NOTE: 1st: Read file contents
	bh_file_contents fc = bh_file_read_contents(alloc, &source_file);
	bh_file_close(&source_file);

	// NOTE: 2nd: Tokenize the contents
	OnyxTokenizer tokenizer = onyx_tokenizer_create(alloc, &fc);
	onyx_lex_tokens(&tokenizer);
	bh_arr(OnyxToken) token_arr = tokenizer.tokens;

	// NOTE: Create the buffer for where compiler messages will be written
	bh_arena msg_arena;
	bh_arena_init(&msg_arena, alloc, 4096);
	bh_allocator msg_alloc = bh_arena_allocator(&msg_arena);

	OnyxMessages msgs;
	onyx_message_create(msg_alloc, &msgs);

	// NOTE: Create the arena where AST nodes will exist
	// Prevents nodes from being scattered across memory due to fragmentation
	bh_arena ast_arena;
	bh_arena_init(&ast_arena, alloc, 16 * 1024 * 1024); // 16MB
	bh_allocator ast_alloc = bh_arena_allocator(&ast_arena);

	// NOTE: 3rd: parse the tokens to an AST
	OnyxParser parser = onyx_parser_create(ast_alloc, &tokenizer, &msgs);
	OnyxAstNode* program = onyx_parse(&parser);

    bh_printf("BEFORE SYMBOL_RESOLUTION: ");
    onyx_ast_print(program, 1);

    bh_arena sp_arena;
    bh_arena_init(&sp_arena, alloc, 16 * 1024);
    bh_allocator sp_alloc = bh_arena_allocator(&sp_arena);

    OnyxSemPassState sp_state = onyx_sempass_create(sp_alloc, ast_alloc, &msgs);
    onyx_sempass(&sp_state, program);

	// NOTE: if there are errors, assume the parse tree was generated wrong,
	// even if it may have still been generated correctly.
	if (onyx_message_has_errors(&msgs)) {
        bh_printf("\n\n");
		onyx_message_print(&msgs);
		goto main_exit;
	} else {
        bh_printf("\n\nAFTER SYMBOL RESOLUTION: ");
		onyx_ast_print(program, 1);
	    bh_printf("\nNo errors.\n");
    }

	// NOTE: 4th: Generate a WASM module from the parse tree and
	// write it to a file.
//	OnyxWasmModule wasm_mod = onyx_wasm_generate_module(alloc, program);
//
//	bh_file out_file;
//	bh_file_create(&out_file, "out.wasm");
//	onyx_wasm_module_write_to_file(&wasm_mod, out_file);
//	bh_file_close(&out_file);
//
//	onyx_wasm_module_free(&wasm_mod);
main_exit: // NOTE: Cleanup, since C doesn't have defer
	bh_arena_free(&sp_arena);
	bh_arena_free(&msg_arena);
	bh_arena_free(&ast_arena);
	onyx_parser_free(&parser);
	onyx_tokenizer_free(&tokenizer);
	bh_file_contents_free(&fc);

	return 0;
}

// NOTE: Old bits of code that may be useful again at some point.
#if 0
	bh_printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

	bh_arr_each(OnyxToken, it, token_arr) {
		onyx_token_null_toggle(*it);
		bh_printf("%s (%s:%l:%l)\n", onyx_get_token_type_name(it->type), it->pos.filename, it->pos.line, it->pos.column);
		onyx_token_null_toggle(*it);
	}
#endif

#if 0
	// NOTE: Ensure type table made correctly

	bh_printf("Type map:\n");
	bh_hash_each_start(i32, wasm_mod.type_map);
		bh_printf("%s -> %d\n", key, value);
	bh_hash_each_end;

	bh_printf("Type list:\n");
	WasmFuncType** func_type = wasm_mod.functypes;
	while (!bh_arr_end(wasm_mod.functypes, func_type)) {
		for (int p = 0; p < (*func_type)->param_count; p++) {
			bh_printf("%c ", (*func_type)->param_types[p]);
		}
		bh_printf("-> ");
		bh_printf("%c\n", (*func_type)->return_type);

		func_type++;
	}
#endif

#if 0
	// NOTE: Ensure the export table was built correctly

	bh_printf("Function types:\n");
	bh_arr_each(WasmFunc, func_it, wasm_mod.funcs) {
		bh_printf("%d\n", func_it->type_idx);
	}

	bh_printf("Exports:\n");
	bh_hash_each_start(WasmExport, wasm_mod.exports);
		bh_printf("%s: %d %d\n", key, value.kind, value.idx);
	bh_hash_each_end;
#endif
