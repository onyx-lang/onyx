#include <stdio.h> // TODO: Replace with custom lib
#include <stdlib.h> // TODO: Replace with custom lib

#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

typedef struct File {
	int fd;
	int open : 1;
	const char* path;
} File;

int file_open(File *f, const char* path, int flags) {
	f->fd = open(path, flags, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);

	if (f->fd < 0)
		return 1;

	f->open = 1;
	f->path = path;
	return 0;
}

int file_close(File* f) {
	if (close(f->fd) < 0) {
		return errno;
	}

	f->open = 0;
	return 0;
}

unsigned char file_read_byte(File* f) {
	if (!f->open) return 0;

	unsigned char byte;
	read(f->fd, &byte, 1);
	return byte;
}

int main(int argc, char *argv[]) {

	if (argc < 2) {
		fprintf(stderr, "Expected file to compile\n");
		return -1;
	}

	File file;

	if (!file_open(&file, argv[1], O_RDONLY)) {
		fprintf(stderr, "Failed to open file: %s\n", argv[1]);
		return -1;
	}

	file_close(&file);

	return 0;
}
