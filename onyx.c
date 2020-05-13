#define BH_NO_STRING
#define BH_DEFINE
#include "bh.h"

#include <stdio.h> // TODO: Replace with custom lib

#include "onyxlex.h"

bh_arr(OnyxToken) parse_tokens(bh_file_contents *fc) {
	OnyxTokenizer tknizer = {
		.start 			= fc->data,
		.curr 			= fc->data,
		.end 			= fc->data + fc->length - 1,
		.line_number 	= 1,
		.line_start 	= fc->data,
	};

	bh_arr(OnyxToken) token_arr = NULL;
	bh_arr_grow(token_arr, 512);

	OnyxToken tk;
	do {
		tk = onyx_get_token(&tknizer);
		bh_arr_push(token_arr, tk);
	} while (tk.type != TOKEN_TYPE_END_STREAM);

	return token_arr;
}

int main(int argc, char *argv[]) {
	bh_file source_file;
	bh_file_error err = bh_file_open(&source_file, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		fprintf(stderr, "Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	bh_file_contents fc = bh_file_read_contents(&source_file);
	bh_file_close(&source_file);

	bh_arr(OnyxToken) token_arr = parse_tokens(&fc);

	printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

	for (OnyxToken* it = token_arr; !bh_arr_end(token_arr, it); it++) {
		printf("%s\n", onyx_get_token_type_name(*it));
	}

	bh_file_contents_delete(&fc);
	bh_arr_free(token_arr);

	return 0;
}
