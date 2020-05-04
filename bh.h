#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <string.h> // TODO: Replace with neede functions

//-------------------------------------------------------------------------------------
// Better types
//-------------------------------------------------------------------------------------
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;
typedef unsigned long long u128;
typedef signed char i8;
typedef signed short i16;
typedef signed int i32;
typedef signed long i64;
typedef signed long long i128;

typedef struct bh_string {
	u8* data;
	u64 length;
	u64 capacity;
} bh_string;

#define bh_string_new(x) _Generic((x), \
	unsigned long: bh_string_new_cap, \
	const char*: bh_string_new_str, \
	char*: bh_string_new_str)(x)

bh_string bh_string_new_cap(unsigned long cap) {
	bh_string str;
	str.data = (u8*) malloc(sizeof(u8) * cap);
	str.length = 0;
	str.capacity = cap;
	return str;
}

bh_string bh_string_new_str(const char* cstr) {
	const int len = strlen(cstr);
	bh_string str;
	int i;

	str.data = (u8*) malloc(sizeof(u8) * len);
	for (i = 0; i < len; i++) {
		str.data[i] = cstr[i];
	}
	
	str.length = len;
	str.capacity = len;
	return str;
}

int bh_string_delete(bh_string* str) {
	free(str->data);
	str->length = 0;
	str->capacity = 0;
	return 1;
}



//-------------------------------------------------------------------------------------
// Better files
//-------------------------------------------------------------------------------------
typedef struct File {
	i32 fd;
	u8 open : 1;
	const char* path;
} File;

int file_open(File *f, const char* path, int flags) {
	f->fd = open(path, flags,
		S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH 	// +rw-rw-rw
	);

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
