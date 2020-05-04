#include <stdio.h> // TODO: Replace with custom lib
#include <stdlib.h> // TODO: Replace with custom lib
#include "bh.h"
// 
// int main(int argc, char *argv[]) {
// 	if (argc < 2) {
// 		fprintf(stderr, "Expected file to compile\n");
// 		return -1;
// 	}
// 
// 	File file;
// 
// 	if (file_open(&file, argv[1], O_RDONLY)) {
// 		fprintf(stderr, "Failed to open file: %s\n", argv[1]);
// 		return -1;
// 	}
// 
// 	file_close(&file);
// 
// 	return 0;
// }
// 

int main(int argc, char *argv[]) {
	bh_string test_str = bh_string_new("Hello World!");

	

	bh_string_delete(&test_str);
}