#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <stdlib.h>
#include <string.h> // TODO: Replace with needed functions

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

//-------------------------------------------------------------------------------------
// Better strings
//-------------------------------------------------------------------------------------
typedef struct bh_string {
	u8* data;
	u64 length;
	u64 capacity;
} bh_string;

#define bh_string_new(x) _Generic((x), \
	unsigned long: bh_string_new_cap, \
	unsigned int: bh_string_new_cap, \
	int: bh_string_new_cap, \
	long: bh_string_new_cap, \
	const char*: bh_string_new_str, \
	char*: bh_string_new_str)(x)

#define bh_string_append(str1, str2) _Generic((str2), \
	bh_string*: bh_string_append_bh_string, \
	char*: bh_string_append_cstr, \
	const char*: bh_string_append_cstr)(str1, str2)

#define bh_string_replace_at(dest, src, offset) _Generic((src), \
	bh_string*: bh_string_replace_at_bh_string, \
	char*: bh_string_replace_at_cstr, \
	const char*: bh_string_replace_at_cstr)(dest, src, offset)

#define bh_string_insert_at(dest, src, offset) _Generic((src), \
	bh_string*: bh_string_insert_at_bh_string, \
	char*: bh_string_insert_at_cstr, \
	const char*: bh_string_insert_at_cstr)(dest, src, offset)

bh_string bh_string_new_cap(unsigned long cap);
bh_string bh_string_new_str(const char* cstr);
i32 bh_string_delete(bh_string* str);
i32 bh_string_ensure_capacity(bh_string* str, u64 cap);
void bh_string_append_bh_string(bh_string* str1, bh_string* str2);
void bh_string_append_cstr(bh_string* str1, const char* str2);
void bh_string_replace_at_bh_string(bh_string* dest, bh_string* src, u64 offset);
void bh_string_replace_at_cstr(bh_string* dest, const char* src, u64 offset);
void bh_string_insert_at_bh_string(bh_string* dest, bh_string* src, u64 offset);
void bh_string_insert_at_cstr(bh_string* dest, bh_string* src, u64 offset);
void bh_string_trim_end(bh_string* str, const char* charset);
void bh_string_trim_begin(bh_string* str, const char* charset);
void bh_string_trim_end_space(bh_string* str);
// TEMP
void bh_string_print(bh_string* str);


//-------------------------------------------------------------------------------------
// IMPLEMENTATIONS
//-------------------------------------------------------------------------------------

bh_string bh_string_new_cap(unsigned long cap) {
	bh_string str;
	str.data = (u8*) malloc(sizeof(u8) * cap);
	str.length = 0;
	str.capacity = cap;
	return str;
}

bh_string bh_string_new_str(const char* cstr) {
	const i32 len = strlen(cstr);
	bh_string str;
	i32 i;

	str.data = (u8*) malloc(sizeof(u8) * len);
	for (i = 0; i < len; i++) {
		str.data[i] = cstr[i];
	}

	str.length = len;
	str.capacity = len;
	return str;
}

i32 bh_string_delete(bh_string* str) {
	free(str->data);
	str->length = 0;
	str->capacity = 0;
	return 1;
}

i32 bh_string_ensure_capacity(bh_string* str, u64 cap) {
	if (str->capacity >= cap) return 1;

	//TODO: This could fail
	str->data = (u8*) realloc((void*) str->data, sizeof(u8) * cap);
	str->capacity = cap;

	return 1;
}

void bh_string_append_bh_string(bh_string* str1, bh_string* str2) {
	if (!bh_string_ensure_capacity(str1, str1->length + str2->length)) return;

	//TODO: Replace with custom memory management
	memcpy(str1->data + str1->length, str2->data, str2->length);
	str1->length += str2->length;
}

void bh_string_append_cstr(bh_string* str1, const char* str2) {
	const i32 str2len = strlen(str2);
	if (!bh_string_ensure_capacity(str1, str1->length + str2len)) return;

	//TODO: Replace with custom memory management
	memcpy(str1->data + str1->length, str2, str2len);
	str1->length += str2len;
}

void bh_string_replace_at_bh_string(bh_string* dest, bh_string* src, u64 offset) {
	if (offset >= dest->length) return;
	if (!bh_string_ensure_capacity(dest, offset + src->length)) return;

	memcpy(dest->data + offset, src->data, src->length);
	if (offset + src->length > dest->length)
		dest->length = offset + src->length;
}

void bh_string_replace_at_cstr(bh_string* dest, const char* src, u64 offset) {
	if (offset >= dest->length) return;
	const int srclen = strlen(src);
	if (!bh_string_ensure_capacity(dest, offset + srclen)) return;

	memcpy(dest->data + offset, src, srclen);
	if (offset + srclen > dest->length)
		dest->length = offset + srclen;
}

void bh_string_insert_at_bh_string(bh_string* dest, bh_string* src, u64 offset) {
}

void bh_string_insert_at_cstr(bh_string* dest, bh_string* src, u64 offset) {
}

void bh_string_trim_end(bh_string* str, const char* charset) {
}

void bh_string_trim_begin(bh_string* str, const char* charset) {
}

void bh_string_trim_end_space(bh_string* str) {
}

// TEMP
void bh_string_print(bh_string* str) {
	write(STDOUT_FILENO, str->data, str->capacity);
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
