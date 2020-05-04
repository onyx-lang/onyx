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
	bh_string test_str = bh_string_new(256);
	bh_string world_str = bh_string_new("World");

	bh_string_append(&test_str, "Hello Frank!");
	bh_string_replace_at(&test_str, &world_str, 6);
	bh_string_replace_at(&test_str, "Hola ", 0);
	bh_string_print(&test_str);

	bh_string_delete(&test_str);
	bh_string_delete(&world_str);
	
	return 0;
}

// int main(int argc, char const *argv[]) {
// 	FILE* file = fopen(argv[1], "r");
// 	long start = ftell(file);
// 	fseek(file, 0, SEEK_END);
// 	long end = ftell(file);
// 	fseek(file, 0, SEEK_SET);
// 
// 
// 	char* data = (char *) malloc(sizeof(u8) * (end - start + 1));
// 	read(file->, data, end - start);
// 	fclose(file);
// 	printf("%ld - %ld = %ld\n", end, start, end - start);
// 	printf("%s", data);
// }