#define BH_NO_STRING
// #define BH_DEBUG
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

	bh_file_contents fc = bh_file_read_contents(&source_file);
	bh_file_close(&source_file);

	bh_hash(u16) symbol_count;
	bh_hash_init(symbol_count);
	bh_arr(OnyxToken) token_arr = onyx_parse_tokens(&fc, symbol_count);

	printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

	for (OnyxToken* it = token_arr; !bh_arr_end(token_arr, it); it++) {
		printf("%s\n", onyx_get_token_type_name(*it));
	}

	bh_hash_iterator it = bh_hash_iter_setup(u16, symbol_count);
	while (bh_hash_iter_next(&it)) {
		const char* sym = bh_hash_iter_key(it);
		u16 count = bh_hash_iter_value(u16, it);

		printf("%s was seen %d times.\n", sym, count);
	}

	bh_file_contents_delete(&fc);
	bh_arr_free(token_arr);
	bh_hash_free(symbol_count);

	return 0;
}
