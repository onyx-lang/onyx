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
typedef unsigned long isize;

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
void bh_string_insert_at_cstr(bh_string* dest, const char* src, u64 offset);
void bh_string_trim_end(bh_string* str, const char* charset);
void bh_string_trim_begin(bh_string* str, const char* charset);
void bh_string_trim_end_space(bh_string* str);
// TEMP
void bh_string_print(bh_string* str);


//-------------------------------------------------------------------------------------
// Better files
//-------------------------------------------------------------------------------------

typedef enum bh_file_error {
	BH_FILE_ERROR_NONE,
	BH_FILE_ERROR_INVALID
} bh_file_error;

typedef enum bh_file_mode {
	BH_FILE_MODE_READ = 1 << 0,
	BH_FILE_MODE_WRITE = 1 << 1,
	BH_FILE_MODE_APPEND = 1 << 2,
	BH_FILE_MODE_RW = 1 << 3,

	BH_FILE_MODE_MODES = BH_FILE_MODE_READ | BH_FILE_MODE_WRITE | BH_FILE_MODE_APPEND | BH_FILE_MODE_RW
} bh_file_mode;

typedef enum bh_file_whence {
	BH_FILE_WHENCE_BEGIN = SEEK_SET,
	BH_FILE_WHENCE_CURRENT = SEEK_CUR,
	BH_FILE_WHENCE_END = SEEK_END,
} bh_file_whence;

typedef int bh_file_descriptor;

typedef struct bh_file {
	bh_file_descriptor fd;
	char const* filename;
} bh_file;

typedef enum bh_file_standard {
	BH_FILE_STANDARD_INPUT,
	BH_FILE_STANDARD_OUTPUT,
	BH_FILE_STANDARD_ERROR
} bh_file_standard;

typedef struct bh_file_contents {
	// This will hold the allocator as well
	isize length;
	void* data;
} bh_file_contents;

bh_file_error bh_file_get_standard(bh_file* file, bh_file_standard stand);

bh_file_error bh_file_create(bh_file* file, char const* filename);
bh_file_error bh_file_open(bh_file* file, char const* filename);
bh_file_error bh_file_open_mode(bh_file* file, bh_file_mode mode, const char* filename);
bh_file_error bh_file_new(bh_file* file, bh_file_descriptor fd, const char* filename);
i32 bh_file_read_at(bh_file* file, i64 offset, void* buffer, isize buff_size, isize* bytes_read);
i32 bh_file_write_at(bh_file* file, i64 offset, void const* buffer, isize buff_size, isize* bytes_wrote);
static i32 bh__file_seek_wrapper(i32 fd, i64 offset, bh_file_whence whence, i64* new_offset);
i32 bh_file_seek(bh_file* file, i64 offset);
i64 bh_file_seek_to_end(bh_file* file);
i64 bh_file_skip(bh_file* file, i64 bytes);
i64 bh_file_tell(bh_file* file);
bh_file_error bh_file_close(bh_file* file);
i32 bh_file_read(bh_file* file, void* buffer, isize buff_size);
i32 bh_file_write(bh_file* file, void* buffer, isize buff_size);
i64 bh_file_size(bh_file* file);

#define bh_file_read_contents(x) _Generic((x), \
	bh_file*: bh_file_read_contents_bh_file, \
	const char*: bh_file_read_contents_direct, \
	char*: bh_file_read_contents_direct)(x)

bh_file_contents bh_file_read_contents_bh_file(bh_file* file);
bh_file_contents bh_file_read_contents_direct(const char* filename);
i32 bh_file_contents_delete(bh_file_contents* contents);












//-------------------------------------------------------------------------------------
// IMPLEMENTATIONS
//-------------------------------------------------------------------------------------

//-------------------------------------------------------------------------------------
// STRING IMPLEMENTATION
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
	if (!bh_string_ensure_capacity(dest, dest->length + src->length)) return;

	memmove(dest->data + offset + src->length, dest->data + offset, dest->length + src->length - offset);
	memcpy(dest->data + offset, src->data, src->length);
	dest->length += src->length;
}

void bh_string_insert_at_cstr(bh_string* dest, const char* src, u64 offset) {
	const int srclen = strlen(src);
	if (!bh_string_ensure_capacity(dest, dest->length + srclen)) return;

	// TODO: Use something better. This copies to a seperate buffer first
	memmove(dest->data + offset + srclen, dest->data + offset, dest->length + srclen - offset);
	memcpy(dest->data + offset, src, srclen);
	dest->length += srclen;
}

static inline u8 charset_contains(const char* charset, char ch) {
	while (*charset) {
		if (*charset == ch) return *charset;
		charset++;
	}

	return 0;
}

void bh_string_trim_end(bh_string* str, const char* charset) {
	while (charset_contains(charset, str->data[str->length - 1]))
		str->length--;
}

void bh_string_trim_begin(bh_string* str, const char* charset) {
	u32 off = 0, i;
	while (charset_contains(charset, str->data[off])) off++;

	if (off == 0) return;

	for (i = 0; i < str->length - off; i++) {
		str->data[i] = str->data[i + off];
	}

	str->length -= off;
}

void bh_string_trim_end_space(bh_string* str) {
	bh_string_trim_end(str, " \t\n\r");
}

// TEMP
void bh_string_print(bh_string* str) {
	write(STDOUT_FILENO, str->data, str->length);
}



//-------------------------------------------------------------------------------------
// FILE IMPLEMENTATION
//-------------------------------------------------------------------------------------
bh_file_error bh_file_get_standard(bh_file* file, bh_file_standard stand) {
	i32 sd_fd = -1;
	const char* filename = NULL;

	switch (stand) {
	case BH_FILE_STANDARD_INPUT:
		sd_fd = STDIN_FILENO;
		filename = "stdin"; // These are constants in the data section so everything should be okay
		break;
	case BH_FILE_STANDARD_OUTPUT:
		sd_fd = STDOUT_FILENO;
		filename = "stdout";
		break;
	case BH_FILE_STANDARD_ERROR:
		sd_fd = STDERR_FILENO;
		filename = "stderr";
		break;
	}

	file->fd = sd_fd;
	file->filename = filename;

	return BH_FILE_ERROR_NONE;
}

bh_file_error bh_file_create(bh_file* file, const char* filename) {
	// Need to do this to avoid compiler complaining about types
	bh_file_mode write_rw = (bh_file_mode) (BH_FILE_MODE_WRITE | BH_FILE_MODE_RW);
	return bh_file_open_mode(file, write_rw, filename);
}

bh_file_error bh_file_open(bh_file* file, const char* filename) {
	return bh_file_open_mode(file, BH_FILE_MODE_READ, filename);
}

bh_file_error bh_file_open_mode(bh_file* file, bh_file_mode mode, const char* filename) {

	i32 os_mode = 0;

	switch (mode & BH_FILE_MODE_MODES) {
	case BH_FILE_MODE_READ:   					  os_mode = O_RDONLY; break;
	case BH_FILE_MODE_WRITE:  					  os_mode = O_WRONLY | O_CREAT | O_TRUNC; break;
	case BH_FILE_MODE_APPEND: 					  os_mode = O_RDONLY | O_APPEND | O_CREAT; break;
	case BH_FILE_MODE_READ   | BH_FILE_MODE_RW:   os_mode = O_RDWR; break;
	case BH_FILE_MODE_WRITE  | BH_FILE_MODE_RW:   os_mode = O_RDWR | O_CREAT | O_TRUNC; break;
	case BH_FILE_MODE_APPEND | BH_FILE_MODE_RW:   os_mode = O_RDWR | O_APPEND | O_CREAT; break;
	//default: // TODO Handle errors
	}

	file->fd = open(filename, os_mode,
		S_IRUSR | S_IWUSR | S_IROTH | S_IWOTH | S_IRGRP | S_IWGRP //+rw-rw-rw-
	);
	if (file->fd < 0) {
		return BH_FILE_ERROR_INVALID;
	}

	// TODO: Set this using some allocator
	file->filename = filename;

	return BH_FILE_ERROR_NONE;
}

bh_file_error bh_file_new(bh_file* file, bh_file_descriptor fd, const char* filename) {
	file->filename = filename; // This may be unsafe
	file->fd = fd;
	return BH_FILE_ERROR_NONE;
}

i32 bh_file_read_at(bh_file* file, i64 offset, void* buffer, isize buff_size, isize* bytes_read) {
	isize res = pread(file->fd, buffer, buff_size, offset);
	if (res < 0) return 0;
	if (bytes_read) *bytes_read = res;
	return 1;
}

i32 bh_file_write_at(bh_file* file, i64 offset, void const* buffer, isize buff_size, isize* bytes_wrote) {
	isize res;
	i64 current_offset = 0;
	bh__file_seek_wrapper(file->fd, offset, BH_FILE_WHENCE_CURRENT, &current_offset);
	if (current_offset == offset) {
		// Standard in and out do like pwrite()
		res = write(file->fd, buffer, buff_size);
	} else {
		res = pwrite(file->fd, buffer, buff_size, offset);
	}
	if (res < 0) return 0;
	if (bytes_wrote) *bytes_wrote = res;

	return 1;
}

static i32 bh__file_seek_wrapper(i32 fd, i64 offset, bh_file_whence whence, i64* new_offset) {
	i64 res = lseek(fd, offset, whence);
	if (res < 0) return 0;
	if (new_offset) *new_offset = res;
	return 1;
}

// Returns new offset
i64 bh_file_seek_to(bh_file* file, i64 offset) {
	i64 new_offset = -1;
	bh__file_seek_wrapper(file->fd, offset, BH_FILE_WHENCE_BEGIN, &new_offset);
	return new_offset;
}

i64 bh_file_seek_to_end(bh_file* file) {
	i64 new_offset = -1;
	bh__file_seek_wrapper(file->fd, 0, BH_FILE_WHENCE_END, &new_offset);
	return new_offset;
}

i64 bh_file_skip(bh_file* file, i64 bytes) {
	i64 new_offset = 0;
	bh__file_seek_wrapper(file->fd, bytes, BH_FILE_WHENCE_CURRENT, &new_offset);
	return new_offset;
}

i64 bh_file_tell(bh_file* file) {
	i64 new_offset = 0;
	bh__file_seek_wrapper(file->fd, 0, BH_FILE_WHENCE_CURRENT, &new_offset);
	return new_offset;
}

bh_file_error bh_file_close(bh_file* file) {
	bh_file_error err = BH_FILE_ERROR_NONE;
	i32 res = close(file->fd);
	if (res < 0)
		err = BH_FILE_ERROR_INVALID;

	return err;
}

i32 bh_file_read(bh_file* file, void* buffer, isize buff_size) {
	return bh_file_read_at(file, bh_file_tell(file), buffer, buff_size, NULL);
}

i32 bh_file_write(bh_file* file, void* buffer, isize buff_size) {
	return bh_file_write_at(file, bh_file_tell(file), buffer, buff_size, NULL);
}

i64 bh_file_size(bh_file* file) {
	i64 size = 0;
	i64 prev = bh_file_tell(file);
	bh_file_seek_to_end(file);
	size = bh_file_tell(file);
	bh_file_seek_to(file, prev);
	return size;
}

bh_file_contents bh_file_read_contents_bh_file(bh_file* file) {
	bh_file_contents fc = { .length = 0, .data = NULL };

	isize size = bh_file_size(file);
	if (size <= 0) return fc;

	fc.data = malloc(size + 1);
	fc.length = size;
	bh_file_read_at(file, 0, fc.data, fc.length, NULL);
	((u8*) fc.data)[fc.length] = '\0';

	return fc;
}

bh_file_contents bh_file_read_contents_direct(const char* filename) {
	bh_file file;
	bh_file_open(&file, filename);
	bh_file_contents fc = bh_file_read_contents(&file);
	bh_file_close(&file);
	return fc;
}

i32 bh_file_contents_delete(bh_file_contents* contents) {
	free(contents->data);
	contents->length = 0;
	return 1;
}
