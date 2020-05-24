#define BH_NO_STRING
// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxutils.h"

int main(int argc, char *argv[]) {
	bh_file source_file;
	bh_file_error err = bh_file_open(&source_file, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		bh_printf_err("Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	bh_allocator alloc = bh_heap_allocator();

	bh_file_contents fc = bh_file_read_contents(alloc, &source_file);
	bh_file_close(&source_file);

	OnyxTokenizer tokenizer = onyx_tokenizer_create(alloc, &fc);
	onyx_lex_tokens(&tokenizer);
	bh_arr(OnyxToken) token_arr = tokenizer.tokens;

#if 0
	bh_printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

	for (OnyxToken* it = token_arr; !bh_arr_end(token_arr, it); it++) {
		onyx_token_null_toggle(*it);
		bh_printf("%s (%s:%l:%l)\n", onyx_get_token_type_name(it->type), it->pos.filename, it->pos.line, it->pos.column);
		onyx_token_null_toggle(*it);
	}
#endif

	bh_arena msg_arena;
	bh_arena_init(&msg_arena, alloc, 4096);
	bh_allocator msg_alloc = bh_arena_allocator(&msg_arena);

	OnyxMessages msgs;
	onyx_message_create(msg_alloc, &msgs);

	bh_arena ast_arena;
	bh_arena_init(&ast_arena, alloc, 16 * 1024 * 1024); // 16MB
	bh_allocator ast_alloc = bh_arena_allocator(&ast_arena);

	OnyxParser parser = onyx_parser_create(ast_alloc, &tokenizer, &msgs);
	OnyxAstNode* program = onyx_parse(&parser);

	// NOTE: if there are errors, assume the parse tree was generated wrong,
	// even if it may have still been generated correctly.
	if (onyx_message_has_errors(&msgs)) {
		onyx_message_print(&msgs);
	} else {
		// TODO: Make a better AST printer so there aren't infinite loops
		// onyx_ast_print(program);
		bh_printf("No errors.\n");
	}

	bh_file_contents_delete(&fc);
	onyx_tokenizer_free(&tokenizer);
	onyx_parser_free(&parser);
	bh_arena_free(&msg_arena);
	bh_arena_free(&ast_arena);


	return 0;
}
