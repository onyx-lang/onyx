#define BH_NO_STRING
#define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include <stdio.h> // TODO: Replace with custom lib

#include "onyxlex.h"

int main(int argc, char *argv[]) {
	bh_file source_file;
	bh_file_error err = bh_file_open(&source_file, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		fprintf(stderr, "Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	bh_allocator alloc = bh_heap_allocator();

	bh_file_contents fc = bh_file_read_contents(alloc, &source_file);
	bh_file_close(&source_file);

	bh_arr(OnyxToken) token_arr = onyx_parse_tokens(alloc, &fc);

	printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

	for (OnyxToken* it = token_arr; !bh_arr_end(token_arr, it); it++) {
		printf("%s '%c' (Line %ld, Col %ld)\n", onyx_get_token_type_name(*it), *(char *)it->token, it->line_number, it->line_column);
	}

	bh_file_contents_delete(&fc);
	bh_arr_free(token_arr);

	return 0;
}
