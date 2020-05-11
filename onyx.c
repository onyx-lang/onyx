#define BH_DEFINE
#include "bh.h"

#include <stdio.h> // TODO: Replace with custom lib

#include "onyxlex.h"

int main(int argc, char const *argv[]) {
	bh_arr(int) arr = NULL; // Must initialize to NULL
	bh_arr_new(arr, 0);

	bh_arr_set_length(arr, 10);
	for (int i = 0; i < 10; i++)
		arr[i] = i;
	printf("Length: %d\nCapacity: %d\n", bh_arr_length(arr), bh_arr_capacity(arr));

	bh_arr_set_length(arr, 0);

	printf("Length: %d\nCapacity: %d\n", bh_arr_length(arr), bh_arr_capacity(arr));

	for (int* it = arr; !bh_arr_end(arr, it); it++) {
		printf("%d ", *it);
	}

	bh_arr_free(arr);

	return 0;
}

int main2(int argc, char *argv[]) {
	bh_file source_file;
	bh_file_error err = bh_file_open(&source_file, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		fprintf(stderr, "Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	bh_file_contents fc = bh_file_read_contents(&source_file);
	bh_file_close(&source_file);

	Tokenizer tknizer = {
		.start = fc.data,
		.curr = fc.data,
		.end = fc.data + fc.length - 1,
		.line_number = 1,
		.line_column = 1,
	};

	Token tk;
	do {
		tk = get_token(&tknizer);
		char c = *(tk.token + tk.length);
		*(tk.token + tk.length) = '\0';
		printf("Line %ld, Column %ld: \n%s: %s\n", tk.line_number, tk.line_column, get_token_type_name(tk), tk.token);
		*(tk.token + tk.length) = c;
	} while (tk.type != TOKEN_TYPE_END_STREAM);


	bh_file_contents_delete(&fc);

	return 0;
}
