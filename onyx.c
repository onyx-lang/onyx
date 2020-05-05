#include <stdio.h> // TODO: Replace with custom lib
#include <stdlib.h> // TODO: Replace with custom lib
#include "bh.h"

int main(int argc, char *argv[]) {
	bh_file demofile;
	bh_file_error err = bh_file_open(&demofile, argv[1]);
	if (err != BH_FILE_ERROR_NONE) {
		fprintf(stderr, "Failed to open file %s\n", argv[1]);
		return EXIT_FAILURE;
	}

	bh_file_contents fc = bh_file_read_contents(&demofile);
	printf("%ld: %s\n", fc.length, fc.data);

	bh_file_contents_delete(&fc);
	bh_file_close(&demofile);

	// bh_string test_str = bh_string_new(256);
	// bh_string world_str = bh_string_new("World FOO Bar test\n");

	// bh_string_append(&test_str, "Hello Frank!\n");
	// bh_string_replace_at(&test_str, &world_str, 6);
	// bh_string_replace_at(&test_str, "Hola ", 0);
	// bh_string_insert_at(&test_str, "World", 3);
	// bh_string_print(&test_str);

	// bh_string trim_str = bh_string_new("abcdeTesting words herezzzz\n   \t");
	// bh_string_print(&trim_str);
	// bh_string_trim_begin(&trim_str, "abcde");
	// bh_string_print(&trim_str);
	// bh_string_trim_end_space(&trim_str);
	// bh_string_print(&trim_str);

	// bh_string_delete(&test_str);
	// bh_string_delete(&world_str);
	// bh_string_delete(&trim_str);

	// bh_string file_contents = bh_file_read_contents("path");

	return 0;
}
