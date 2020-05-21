#ifndef BH_H
#define BH_H

#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <stdlib.h>
#include <stdarg.h>
#include <string.h> // TODO: Replace with needed functions
#include <assert.h>

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
typedef i32 b32;
typedef void* ptr;




















//-------------------------------------------------------------------------------------
// Better character functions
//-------------------------------------------------------------------------------------
b32 char_is_alpha(const char a);
b32 char_is_num(const char a);
b32 char_is_alphanum(const char a);
b32 char_is_whitespace(const char a);
b32 char_in_range(const char lo, const char hi, const char a);
char charset_contains(const char* charset, char ch);
i64 chars_match(char* ptr1, char* ptr2);







//-------------------------------------------------------------------------------------
// Better math functions
//-------------------------------------------------------------------------------------
#define bh_max(a, b)		((a) > (b) ? (a) : (b))
#define bh_min(a, b)		((a) < (b) ? (a) : (b))
#define bh_clamp(v, a, b)	(bh_min((b), bh_max((a), (v))))
#define bh_abs(x)			((x) < 0 ? -(x) : (x))





//-------------------------------------------------------------------------------------
// Helpful macros
//-------------------------------------------------------------------------------------
#define bh_offset_of(Type, elem)		((isize)&(((Type)*) 0)->elem)
#define bh_aligh_of(Type)				bh_offset_of(struct { char c; Type member; }, member)
#define bh_swap(Type, a, b)				do { Type tmp = (a); (a) = (b); (b) = tmp; } while(0)

#define bh_pointer_add(ptr, amm)		((void *)((u8 *) ptr + amm))
#define BH_BIT(x)						(1 << (x))
#define BH_MASK_SET(var, set, mask) 	((set) ? (var) |= (mask) : (var) &= ~(mask))







//-------------------------------------------------------------------------------------
// Custom allocators
//-------------------------------------------------------------------------------------

typedef enum bh_allocator_actions {
	bh_allocator_action_alloc,
	bh_allocator_action_free,
	bh_allocator_action_resize,
} bh_allocator_actions;

#define BH_ALLOCATOR_PROC(name) \
ptr name(ptr data, bh_allocator_actions action, \
		 isize size, isize alignment, \
		 void* prev_memory, \
		 u64 flags)

typedef BH_ALLOCATOR_PROC(bh__allocator_proc); // NOTE: so bh__allocator_proc can be used instead of that type

typedef struct bh_allocator {
	bh__allocator_proc* proc; // Procedure that can handle bh_allocator_actions
	ptr					data; // Pointer to the other data for the allocator
} bh_allocator;

typedef enum bh_allocator_flags {
	bh_allocator_flag_clear = 1	// Sets all memory to be 0
} bh_allocator_flags;

ptr bh_alloc(bh_allocator a, isize size);
ptr bh_alloc_aligned(bh_allocator a, isize size, isize alignment);
ptr bh_resize(bh_allocator a, ptr data, isize new_size);
ptr bh_resize_aligned(bh_allocator a, ptr data, isize new_size, isize alignment);
void bh_free(bh_allocator a, ptr data);

#define bh_alloc_item(allocator_, T)				(T *) bh_alloc(allocator_, sizeof(T))
#define bh_alloc_array(allocator_, T, n)			(T *) bh_alloc(allocator_, sizeof(T) * (n))

// NOTE: This should get optimized out since alignment should be a power of two
#define bh__align(x, alignment)						((((x) / alignment) + 1) * alignment)




// HEAP ALLOCATOR
// Essentially a wrapper for malloc, free and realloc
bh_allocator bh_heap_allocator(void);
BH_ALLOCATOR_PROC(bh_heap_allocator_proc);





// ARENA ALLOCATOR
typedef struct bh_arena {
	bh_allocator backing;
	ptr first_arena, current_arena;
	isize size, arena_size; // in bytes
} bh_arena;

typedef struct bh__arena_internal {
	ptr next_arena;
	void* data; // Not actually a pointer, just used for the offset
} bh__arena_internal;

void bh_arena_init(bh_arena* alloc, bh_allocator backing, isize arena_size);
void bh_arena_free(bh_arena* alloc);
bh_allocator bh_arena_allocator(bh_arena* alloc);
BH_ALLOCATOR_PROC(bh_arena_allocator_proc);



// SCRATCH ALLOCATOR
typedef struct bh_scratch {
	bh_allocator backing;
	ptr memory, end, curr;
} bh_scratch;

void bh_scratch_init(bh_scratch* scratch, bh_allocator backing, isize scratch_size);
void bh_scratch_free(bh_scratch* scratch);
bh_allocator bh_scratch_allocator(bh_scratch* scratch);
BH_ALLOCATOR_PROC(bh_scratch_allocator_proc);








//-------------------------------------------------------------------------------------
// Better strings
//-------------------------------------------------------------------------------------
#ifndef BH_NO_STRING

typedef struct bh__string {
	u64 length;
	u64 capacity;
} bh__string;

typedef char bh_string;

#define bh__stringhead(x)		(((bh__string *)(x)) - 1)

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
b32 bh_string_delete(bh_string* str);
b32 bh_string_ensure_capacity(bh_string* str, u64 cap);
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

#endif










//-------------------------------------------------------------------------------------
// Better files
//-------------------------------------------------------------------------------------
#ifndef BH_NO_FILE

typedef enum bh_file_error {
	BH_FILE_ERROR_NONE,
	BH_FILE_ERROR_INVALID,
	BH_FILE_ERROR_BAD_FD,
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
	bh_allocator allocator;
	const char *filename;
	isize length;
	void* data;
} bh_file_contents;

bh_file_error bh_file_get_standard(bh_file* file, bh_file_standard stand);

bh_file_error bh_file_create(bh_file* file, char const* filename);
bh_file_error bh_file_open(bh_file* file, char const* filename);
bh_file_error bh_file_open_mode(bh_file* file, bh_file_mode mode, const char* filename);
bh_file_error bh_file_new(bh_file* file, bh_file_descriptor fd, const char* filename);
b32 bh_file_read_at(bh_file* file, i64 offset, void* buffer, isize buff_size, isize* bytes_read);
b32 bh_file_write_at(bh_file* file, i64 offset, void const* buffer, isize buff_size, isize* bytes_wrote);
static b32 bh__file_seek_wrapper(i32 fd, i64 offset, bh_file_whence whence, i64* new_offset);
i64 bh_file_seek_to_end(bh_file* file);
i64 bh_file_skip(bh_file* file, i64 bytes);
i64 bh_file_tell(bh_file* file);
bh_file_error bh_file_close(bh_file* file);
i32 bh_file_read(bh_file* file, void* buffer, isize buff_size);
i32 bh_file_write(bh_file* file, void* buffer, isize buff_size);
i64 bh_file_size(bh_file* file);

#define bh_file_read_contents(allocator_, x) _Generic((x), \
	bh_file*: bh_file_read_contents_bh_file, \
	const char*: bh_file_read_contents_direct, \
	char*: bh_file_read_contents_direct)((allocator_), x)

bh_file_contents bh_file_read_contents_bh_file(bh_allocator alloc, bh_file* file);
bh_file_contents bh_file_read_contents_direct(bh_allocator alloc, const char* filename);
i32 bh_file_contents_delete(bh_file_contents* contents);

#endif










//-------------------------------------------------------------------------------------
// Alternate printing
//-------------------------------------------------------------------------------------
// Barebones implementation of printf. Does not support all format options
// Currently supports:
// 		%c - chars
//		%_(u)d - ints where _ is:
//			nothing - decimal
//			o - octal
//			x - hexadecimal
//		%_(u)l - longs where _ is:
//			nothing - decimal
//			o - octal
//			x - hexadecimal
//		%f - floating points
//		%s - null terminated strings
//		%p - pointers
//		%% - literal %

typedef struct bh__print_format {
	u32 base;
} bh__print_format;

isize bh_printf(char const *fmt, ...);
isize bh_printf_va(char const *fmt, va_list va);
isize bh_printf_err(char const *fmt, ...);
isize bh_printf_err_va(char const *fmt, va_list va);
isize bh_fprintf(bh_file* f, char const *fmt, ...);
isize bh_fprintf_va(bh_file* f, char const *fmt, va_list va);
char* bh_bprintf(char const *fmt, ...);
char* bh_bprintf_va(char const *fmt, va_list va);
isize bh_snprintf(char *str, isize n, char const *fmt, ...);
isize bh_snprintf_va(char *str, isize n, char const *fmt, va_list va);








//-------------------------------------------------------------------------------------
// Better debug functions
//-------------------------------------------------------------------------------------
#ifdef BH_DEBUG

void* bh__debug_malloc(size_t size, const char* file, u64 line);
void* bh__debug_aligned_alloc(size_t size, size_t alignment, const char* file, u64 line);
void  bh__debug_free(void* ptr, const char* file, u64 line);
void* bh__debug_realloc(void* ptr, size_t size, const char* file, u64 line);

#ifdef BH_DEFINE

void* bh__debug_malloc(size_t size, const char* file, u64 line) {
	void* p = malloc(size);
	bh_printf("[DEBUG] %p = malloc(%d) at %s:%d\n", p, size, file, line);
	return p;
}

void* bh__debug_aligned_alloc(size_t size, size_t alignment, const char* file, u64 line) {
	void* p = aligned_alloc(size, alignment);
	bh_printf("[DEBUG] %p = aligned_alloc(%d, %d) at %s:%d\n", p, alignment, size, file, line);
	return p;
}

void bh__debug_free(void* ptr, const char* file, u64 line) {
	bh_printf("[DEBUG] free(%p) at %s:%d\n", ptr, file, line);
	free(ptr);
}

void* bh__debug_realloc(void* ptr, size_t size, const char* file, u64 line) {
	void* p = realloc(ptr, size);
	bh_printf("[DEBUG] %p = realloc(%p, %d) at %s:%d\n", p, ptr, size, file, line);
	return p;
}

#endif

#define malloc(size)					(bh__debug_malloc(size, __FILE__, __LINE__))
#define aligned_alloc(size, alignment)	(bh__debug_aligned_alloc(size, alignment, __FILE__, __LINE__))
#define free(ptr)						(bh__debug_free(ptr, __FILE__, __LINE__))
#define realloc(ptr, size)				(bh__debug_realloc(ptr, size, __FILE__, __LINE__))

#endif


















//-------------------------------------------------------------------------------------
// Better dynamically-sized arrays
//-------------------------------------------------------------------------------------
#ifndef BH_NO_ARRAY

typedef struct bh__arr {
	bh_allocator allocator;
	i32 length, capacity;
} bh__arr;

#ifndef BH_ARR_GROW_FORMULA
#define BH_ARR_GROW_FORMULA(x)		((x) > 0 ? ((x) << 1) : 4)
#endif

#define bh_arr(T)					T*
#define bh__arrhead(arr)			(((bh__arr *)(arr)) - 1)

#define bh_arr_allocator(arr)		(arr ? bh__arrhead(arr)->allocator : bh_heap_allocator())
#define bh_arr_length(arr) 			(arr ? bh__arrhead(arr)->length : 0)
#define bh_arr_capacity(arr) 		(arr ? bh__arrhead(arr)->capacity : 0)
#define bh_arr_size(arr)			(arr ? bh__arrhead(arr)->capacity * sizeof(*(arr)) : 0)
#define bh_arr_valid(arr, i)		(arr ? (i32)(i) < bh__arrhead(arr)->length : 0)

#define bh_arr_pop(arr)				((arr)[--bh__arrhead(arr)->length])
#define bh_arr_last(arr)			((arr)[bh__arrhead(arr)->length - 1])
#define bh_arr_end(arr, i)			((i) >= &(arr)[bh_arr_length(arr)])

#define bh_arr_new(allocator_, arr, cap)	(bh__arr_grow((allocator_), (void**) &(arr), sizeof(*(arr)), cap))
#define bh_arr_free(arr)					(bh__arr_free((void**) &(arr)))
#define bh_arr_copy(allocator_, arr)		(bh__arr_copy((allocator_), (arr), sizeof(*(arr))))

#define bh_arr_grow(arr, cap) 		(bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), cap))
#define bh_arr_shrink(arr, cap)		(bh__arr_shrink((void **) &(arr), sizeof(*(arr)), cap))
#define bh_arr_set_length(arr, n)	( \
	bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), n), \
	bh__arrhead(arr)->length = n)

#define bh_arr_insertn(arr, i, n)	(bh__arr_insertn((void **) &(arr), sizeof(*(arr)), i, n))

#define bh_arr_insert_end(arr, n)	( \
	bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), bh_arr_length(arr) + n), \
	bh__arrhead(arr)->length += n)

#define bh_arr_push(arr, value) 	( \
	bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), bh_arr_length(arr) + 1), \
	arr[bh__arrhead(arr)->length++] = value)

#define bh_arr_is_empty(arr)		(arr ? bh__arrhead(arr)->length == 0 : 1)
#define bh_arr_clear(arr)			(arr ? (bh__arrhead(arr)->length = 0) : 0)

#define bh_arr_deleten(arr, i, n)	(bh__arr_deleten((void **) &(arr), sizeof(*(arr)), i, n))
#define bh_arr_fastdelete(arr, i)	(arr[i] = arr[--bh__arrhead(arr)->length])

b32 bh__arr_grow(bh_allocator alloc, void** arr, i32 elemsize, i32 cap);
b32 bh__arr_shrink(void** arr, i32 elemsize, i32 cap);
b32 bh__arr_free(void **arr);
void* bh__arr_copy(bh_allocator alloc, void *arr, i32 elemsize);
void bh__arr_insertn(void **arr, i32 elemsize, i32 index, i32 numelems);
void bh__arr_deleten(void **arr, i32 elemsize, i32 index, i32 numelems);

#endif












//-------------------------------------------------------------------------------------
// HASH TABLE FUNCTIONS
//-------------------------------------------------------------------------------------
#ifndef BH_NO_HASHTABLE

#define BH__HASH_STORED_KEY_SIZE 64
typedef struct bh__hash_entry {
	char key[BH__HASH_STORED_KEY_SIZE];
	i32 value; // NOTE: Not actually an i32, just used as a placeholder for offset
} bh__hash_entry;

#define BH__HASH_MODULUS 1021
#define BH__HASH_KEYSIZE 64
#ifdef BH_DEFINE
u64 bh__hash_function(const char* str, i32 len) {
	u64 hash = 5381;
	i32 c, l = 0;
	if (len == 0) len = BH__HASH_KEYSIZE;

	while ((c = *str++) && l++ < len) {
		hash = (hash << 5) + hash + c;
	}

	return hash % BH__HASH_MODULUS;
}
#endif

typedef struct bh_hash_iterator {
	ptr *tab, *endtab;
	i32 elemsize, arrlen;
	bh__hash_entry* entry;
} bh_hash_iterator;

typedef struct bh__hash {
	bh_allocator allocator;
	ptr arrs[BH__HASH_MODULUS];
} bh__hash;

#define bh_hash(T)		T*

#ifdef BH_HASH_SIZE_SAFE
	#define bh_hash_init(allocator_, tab)	bh__hash_init(allocator_, (bh__hash **)&(tab))
	#define bh_hash_free(tab)				bh__hash_free((bh__hash **)&(tab))
	#define bh_hash_put(T, tab, key, value) (assert(sizeof(T) == sizeof(*(tab))), (*((T *) bh__hash_put((bh__hash *) tab, sizeof(T), key)) = (T) value))
	#define bh_hash_has(T, tab, key)		(assert(sizeof(T) == sizeof(*(tab))), (bh__hash_has((bh__hash *) tab, sizeof(T), key)))
	#define bh_hash_get(T, tab, key)		(assert(sizeof(T) == sizeof(*(tab))), (*((T *) bh__hash_get((bh__hash *) tab, sizeof(T), key))))
	#define bh_hash_delete(T, tab, key)		(assert(sizeof(T) == sizeof(*(tab))), bh__hash_delete((bh__hash *) tab, sizeof(T), key))

	#define bh_hash_iter_setup(T, tab)			(assert(sizeof(T) == sizeof(*(tab))), bh__hash_iter_setup((bh__hash *) tab, sizeof(T)))
	#define bh_hash_iter_key(it)				(it.entry->key)
	#define bh_hash_iter_value(T, it)			(assert(sizeof(T) == it.elemsize), *(T *)&(it.entry->value))
#else
	#define bh_hash_init(allocator_, tab)	bh__hash_init(allocator_, (bh__hash **)&(tab))
	#define bh_hash_free(tab)				bh__hash_free((bh__hash **)&(tab))
	#define bh_hash_put(T, tab, key, value) (*((T *) bh__hash_put((bh__hash *) tab, sizeof(T), key)) = value)
	#define bh_hash_has(T, tab, key)		(bh__hash_has((bh__hash *) tab, sizeof(T), key))
	#define bh_hash_get(T, tab, key)		(*((T *) bh__hash_get((bh__hash *) tab, sizeof(T), key)))
	#define bh_hash_delete(T, tab, key)		(bh__hash_delete((bh__hash *) tab, sizeof(T), key))

	#define bh_hash_iter_setup(T, tab)			(bh__hash_iter_setup((bh__hash *) tab, sizeof(T)))
	#define bh_hash_iter_key(it)				(it.entry->key)
	#define bh_hash_iter_value(T, it)			(*(T *)&(it.entry->value))
#endif

b32 bh__hash_init(bh_allocator allocator, bh__hash **table);
b32 bh__hash_free(bh__hash **table);
ptr bh__hash_put(bh__hash *table, i32 elemsize, char *key);
b32 bh__hash_has(bh__hash *table, i32 elemsize, char *key);
ptr bh__hash_get(bh__hash *table, i32 elemsize, char *key);
void bh__hash_delete(bh__hash *table, i32 elemsize, char *key);
bh_hash_iterator bh__hash_iter_setup(bh__hash *table, i32 elemsize);
b32 bh_hash_iter_next(bh_hash_iterator* it);

#endif

#ifdef BH_DEFINE
#undef BH_DEFINE








































//-------------------------------------------------------------------------------------
// IMPLEMENTATIONS
//-------------------------------------------------------------------------------------

//-------------------------------------------------------------------------------------
// CHAR FUNCTIONS
//-------------------------------------------------------------------------------------
b32 char_is_alpha(const char a) {
	return ('a' <= a && a <= 'z') || ('A' <= a && a <= 'Z');
}

b32 char_is_num(const char a) {
	return ('0' <= a && a <= '9');
}

b32 char_is_alphanum(const char a) {
	return char_is_alpha(a) || char_is_num(a);
}

b32 char_is_whitespace(const char a) {
	return charset_contains(" \t\r\n", a);
}

b32 char_in_range(const char lo, const char hi, const char a) {
	return lo <= a <= hi;
}

char charset_contains(const char* charset, char ch) {
	while (*charset) {
		if (*charset == ch) return ch;
		charset++;
	}

	return 0;
}

i64 chars_match(char* ptr1, char* ptr2) {
	i64 len = 0;
	while (*ptr2 != '\0' && *ptr1 == *ptr2) ptr1++, ptr2++, len++;
	return *ptr2 == '\0' ? len : 0;
}







//-------------------------------------------------------------------------------------
// CUSTOM ALLOCATORS IMPLEMENTATION
//-------------------------------------------------------------------------------------


ptr bh_alloc(bh_allocator a, isize size) {
	return bh_alloc_aligned(a, size, 16);
}

ptr bh_alloc_aligned(bh_allocator a, isize size, isize alignment) {
	return a.proc(a.data, bh_allocator_action_alloc, size, alignment, NULL,  0);
}

ptr bh_resize(bh_allocator a, ptr data, isize new_size) {
	return bh_resize_aligned(a, data, new_size, 16);
}

ptr bh_resize_aligned(bh_allocator a, ptr data, isize new_size, isize alignment) {
	return a.proc(a.data, bh_allocator_action_resize, new_size, alignment, data, 0);
}

void bh_free(bh_allocator a, ptr data) {
	if (data != NULL) a.proc(a.data, bh_allocator_action_free, 0, 0, data, 0);
}



// HEAP ALLOCATOR IMPLEMENTATION

bh_allocator bh_heap_allocator(void) {
	return (bh_allocator) {
		.proc = bh_heap_allocator_proc,
		.data = NULL
	};
}

BH_ALLOCATOR_PROC(bh_heap_allocator_proc) {
	ptr retval = NULL;

	switch (action) {
	case bh_allocator_action_alloc: {
		retval = aligned_alloc(alignment, size);

		if (flags & bh_allocator_flag_clear && retval != NULL) {
			memset(retval, 0, size);
		}
	} break;

	case bh_allocator_action_resize: {
		// TODO: Maybe replace with better custom function
		retval = realloc(prev_memory, size);
	} break;

	case bh_allocator_action_free: {
		free(prev_memory);
	} break;
	}

	return retval;
}







// ARENA ALLOCATOR IMPLEMENTATION
void bh_arena_init(bh_arena* alloc, bh_allocator backing, isize arena_size) {
	ptr data = bh_alloc(backing, arena_size);

	alloc->backing = backing;
	alloc->arena_size = arena_size;
	alloc->size = sizeof(ptr);
	alloc->first_arena = data;
	alloc->current_arena = data;

	((bh__arena_internal *)(alloc->first_arena))->next_arena = NULL;
}

void bh_arena_free(bh_arena* alloc) {
	bh__arena_internal *walker = (bh__arena_internal *) alloc->first_arena;
	bh__arena_internal *trailer = walker;
	while (walker != NULL) {
		walker = walker->next_arena;
		bh_free(alloc->backing, trailer);
		trailer = walker;
	}

	alloc->first_arena = NULL;
	alloc->current_arena = NULL;
	alloc->arena_size = 0;
	alloc->size = 0;
}

bh_allocator bh_arena_allocator(bh_arena* alloc) {
	return (bh_allocator) {
		.proc = bh_arena_allocator_proc,
		.data = alloc,
	};
}

BH_ALLOCATOR_PROC(bh_arena_allocator_proc) {
	bh_arena* alloc_arena = (bh_arena*) data;

	ptr retval = NULL;

	switch (action) {
	case bh_allocator_action_alloc: {

		// TODO: Do this better because right now bh__align is bad
		// size = bh__align(size, alignment);
		if (size > alloc_arena->arena_size) {
			// Size too large for the arena
			return NULL;
		}

		if (alloc_arena->size + size >= alloc_arena->arena_size) {
			alloc_arena->size = sizeof(ptr);
			bh__arena_internal* new_arena = (bh__arena_internal *) bh_alloc(alloc_arena->backing, alloc_arena->arena_size);

			if (new_arena == NULL) {
				bh_printf_err("Arena Allocator: couldn't allocate new arena");
				return NULL;
			}

			new_arena->next_arena = NULL;
			((bh__arena_internal *)(alloc_arena->current_arena))->next_arena = new_arena;
			alloc_arena->current_arena = new_arena;
		}

		retval = bh_pointer_add(alloc_arena->current_arena, alloc_arena->size);
		alloc_arena->size += size;
	} break;

	case bh_allocator_action_resize: {
		// Do nothing since this is a fixed allocator
	} break;

	case bh_allocator_action_free: {
		// Do nothing since this allocator isn't made for freeing memory
	} break;
	}

	return retval;
}




// SCRATCH ALLOCATOR IMPLEMENTATION
void bh_scratch_init(bh_scratch* scratch, bh_allocator backing, isize scratch_size) {
	ptr memory = bh_alloc(backing, scratch_size);

	scratch->backing = backing;
	scratch->memory = memory;
	scratch->curr = memory;
	scratch->end = memory + scratch_size;
}

void bh_scratch_free(bh_scratch* scratch) {
	bh_free(scratch->backing, scratch->memory);

	scratch->memory = NULL;
	scratch->curr = NULL;
	scratch->end = NULL;
}

bh_allocator bh_scratch_allocator(bh_scratch* scratch) {
	return (bh_allocator) {
		.proc = bh_scratch_allocator_proc,
		.data = scratch,
	};
}

BH_ALLOCATOR_PROC(bh_scratch_allocator_proc) {
	bh_scratch* scratch = (bh_scratch*) data;
	ptr retval = NULL;

	switch (action) {
	case bh_allocator_action_alloc: {
		if (size > scratch->end - scratch->memory) {
			return NULL;
		}

		retval = scratch->curr;
		scratch->curr += size;

		if (scratch->curr >= scratch->end) {
			scratch->curr = scratch->memory;
			retval = scratch->curr;
		}
	} break;

	case bh_allocator_action_free:
	case bh_allocator_action_resize:
		// Do nothing
		break;
	}

	return retval;
}


//-------------------------------------------------------------------------------------
// STRING IMPLEMENTATION (BROKEN)
//-------------------------------------------------------------------------------------
#ifndef BH_NO_STRING

bh_string* bh_string_new_cap(unsigned long cap) {
	bh__string* str;
	str = (bh__string*) malloc(sizeof(*str) + sizeof(char) * cap + 1);
	str[0] = 0;
	return str + 1;
}

bh_string* bh_string_new_str(const char* cstr) {
	const i32 len = strlen(cstr);
	bh__string* str;
	i32 i;

	str = malloc(sizeof(*str) + sizeof(char) * len + 1);
	char* data = (char*) (str + 1);
	for (i = 0; i < len; i++) {
		data[i] = cstr[i];
	}

	data[len] = 0; // Always null terminate the string

	str->length = len;
	str->capacity = len;
	return str + 1;
}

b32 bh_string_delete(bh_string** str) {
	bh__string* strptr = bh__stringhead(*str);
	free(strptr);
	str->length = 0;
	str->capacity = 0;
	return 1;
}

b32 bh_string_grow(bh_string** str, u64 cap) {
	bh__string* strptr = bh__stringhead(*str);
	if (strptr->capacity >= cap) return 1;

	void* p;
	p = realloc(strptr, sizeof(*strptr) + sizeof(char) * cap + 1);

	strptr->capacity = cap;

	return 1;
}

void bh_string_append_bh_string(bh_string** str1, bh_string** str2) {
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
	if (offset > dest->length) return;
	if (!bh_string_ensure_capacity(dest, offset + src->length)) return;

	memcpy(dest->data + offset, src->data, src->length);
	if (offset + src->length > dest->length)
		dest->length = offset + src->length;
}

void bh_string_replace_at_cstr(bh_string* dest, const char* src, u64 offset) {
	if (offset > dest->length) return;
	const i32 srclen = strlen(src);
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
	const i32 srclen = strlen(src);
	if (!bh_string_ensure_capacity(dest, dest->length + srclen)) return;

	// TODO: Use something better. This copies to a seperate buffer first
	memmove(dest->data + offset + srclen, dest->data + offset, dest->length + srclen - offset);
	memcpy(dest->data + offset, src, srclen);
	dest->length += srclen;
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

#endif // ifndef BH_NO_STRING









//-------------------------------------------------------------------------------------
// FILE IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_FILE

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
	default:
		return BH_FILE_ERROR_BAD_FD;
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

b32 bh_file_read_at(bh_file* file, i64 offset, void* buffer, isize buff_size, isize* bytes_read) {
	isize res = pread(file->fd, buffer, buff_size, offset);
	if (res < 0) return 0;
	if (bytes_read) *bytes_read = res;
	return 1;
}

b32 bh_file_write_at(bh_file* file, i64 offset, void const* buffer, isize buff_size, isize* bytes_wrote) {
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

static b32 bh__file_seek_wrapper(i32 fd, i64 offset, bh_file_whence whence, i64* new_offset) {
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

b32 bh_file_read(bh_file* file, void* buffer, isize buff_size) {
	return bh_file_read_at(file, bh_file_tell(file), buffer, buff_size, NULL);
}

b32 bh_file_write(bh_file* file, void* buffer, isize buff_size) {
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

bh_file_contents bh_file_read_contents_bh_file(bh_allocator alloc, bh_file* file) {
	bh_file_contents fc = {
		.allocator = alloc,
		.filename  = file->filename,
		.length = 0, .data = NULL
	};

	isize size = bh_file_size(file);
	if (size <= 0) return fc;

	fc.data = bh_alloc(alloc, size + 1);
	fc.length = size;
	bh_file_read_at(file, 0, fc.data, fc.length, NULL);
	((u8*) fc.data)[fc.length] = '\0';

	return fc;
}

bh_file_contents bh_file_read_contents_direct(bh_allocator alloc, const char* filename) {
	bh_file file;
	bh_file_open(&file, filename);
	bh_file_contents fc = bh_file_read_contents(alloc, &file);
	bh_file_close(&file);
	return fc;
}

b32 bh_file_contents_delete(bh_file_contents* contents) {
	bh_free(contents->allocator, contents->data);
	contents->length = 0;
	return 1;
}

#endif // ifndef BH_NO_FILE


















//-------------------------------------------------------------------------------------
// ALTERNATE PRINTF IMPLEMENTATION
//-------------------------------------------------------------------------------------
isize bh_printf(char const *fmt, ...) {
	isize res;
	va_list va;
	va_start(va, fmt);
	res = bh_printf_va(fmt, va);
	va_end(va);
	return res;
}

isize bh_printf_va(char const *fmt, va_list va) {
	bh_file file;
	bh_file_get_standard(&file, BH_FILE_STANDARD_OUTPUT);
	return bh_fprintf_va(&file, fmt, va);
}

isize bh_printf_err(char const *fmt, ...) {
	isize res;
	va_list va;
	va_start(va, fmt);
	res = bh_printf_err_va(fmt, va);
	va_end(va);
	return res;
}

isize bh_printf_err_va(char const *fmt, va_list va) {
	bh_file file;
	bh_file_get_standard(&file, BH_FILE_STANDARD_ERROR);
	return bh_fprintf_va(&file, fmt, va);
}

isize bh_fprintf(bh_file* f, char const *fmt, ...) {
	isize res;
	va_list va;
	va_start(va, fmt);
	res = bh_fprintf_va(f, fmt, va);
	va_end(va);
	return res;
}

isize bh_fprintf_va(bh_file* f, char const *fmt, va_list va) {
	static char buf[4096];
	isize len = bh_snprintf_va(buf, sizeof(buf), fmt, va);
	bh_file_write(f, buf, len - 1);
	return len;
}

char* bh_bprintf(char const *fmt, ...) {
	char* res;
	va_list va;
	va_start(va, fmt);
	res = bh_bprintf_va(fmt, va);
	va_end(va);
	return res;
}

char* bh_bprintf_va(char const *fmt, va_list va) {
	static char buffer[4096];
	bh_snprintf_va(buffer, sizeof(buffer), fmt, va);
	return buffer;
}

isize bh_snprintf(char *str, isize n, char const *fmt, ...) {
	isize res;
	va_list va;
	va_start(va, fmt);
	res = bh_snprintf_va(str, n, fmt, va);
	va_end(va);
	return res;
}

isize bh__print_string(char* dest, isize n, char* src) {
	isize len = 0;
	while (n-- && (*dest++ = *src++)) len++;
	return len;
}

isize bh__printu64(char* str, isize n, bh__print_format format, u64 value) {
	char buf[128];
	buf[127] = 0;
	char* walker = buf + 127;
	u32 base = format.base ? format.base : 10, tmp;

	while (value > 0) {
		tmp = value % base;
		if (tmp > 9) {
			switch (tmp) {
			case 10: tmp = 'a'; break;
			case 11: tmp = 'b'; break;
			case 12: tmp = 'c'; break;
			case 13: tmp = 'd'; break;
			case 14: tmp = 'e'; break;
			case 15: tmp = 'f'; break;
			}
		} else {
			tmp += '0';
		}

		*--walker = tmp;
		value /= base;
	}

	if (format.base == 16) {
		*--walker = 'x';
		*--walker = '0';
	}

	return bh__print_string(str, n, walker);
}

isize bh__printi64(char* str, isize n, bh__print_format format, i64 value) {
	char buf[128];
	buf[127] = 0;
	char* walker = buf + 127;
	u32 base = format.base ? format.base : 10, tmp;

	b32 negative = value < 0;
	if (negative) value = -value;

	if (value == 0) {
		*--walker = '0';
	} else {
		while (value > 0) {
			tmp = value % base;
			if (tmp > 9) {
				switch (tmp) {
				case 10: tmp = 'a'; break;
				case 11: tmp = 'b'; break;
				case 12: tmp = 'c'; break;
				case 13: tmp = 'd'; break;
				case 14: tmp = 'e'; break;
				case 15: tmp = 'f'; break;
				}
			} else {
				tmp += '0';
			}

			*--walker = tmp;
			value /= base;
		}
	}

	if (negative) {
		*--walker = '-';
	}

	if (format.base == 16) {
		*--walker = 'x';
		*--walker = '0';
	}

	return bh__print_string(str, n, walker);
}

// TODO: This is very hacked together but for now it will work.
isize bh_snprintf_va(char *str, isize n, char const *fmt, va_list va) {
	char const *text_start = str;
	isize res;

	while (*fmt) {
		bh__print_format format = { 0 };
		isize len = 0;

		while (*fmt && *fmt != '%') {
			*(str++) = *(fmt++);
		}

		if (!*fmt) goto end_of_format;

		fmt++;

		switch (*fmt++) {
		case 'o': format.base = 8; break;
		case 'x': format.base = 16; break;
		default: fmt--;
		}

		switch (*fmt) {
		case 'c': {
			char c = (char) va_arg(va, int);
			*(str++) = c;
		} break;

		case 'd': {
			i64 value = (i64) va_arg(va, int);
			len = bh__printi64(str, n, format, value);
		} break;

		case 'l': {
			i64 value = (i64) va_arg(va, long);
			len = bh__printi64(str, n, format, value);
		} break;

		case 'p': {
			u64 value = (u64) va_arg(va, ptr);
			format.base = 16;
			len = bh__printu64(str, n, format, value);
		} break;

		case 's': {
			char* s = va_arg(va, char *);
			len = bh__print_string(str, n, s);
		} break;

		default: fmt--;
		}

		fmt++;

end_of_format:
		str += len;
		n -= len;
	}

	return str - text_start + 1;
}















//-------------------------------------------------------------------------------------
// ARRAY IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_ARRAY

b32 bh__arr_grow(bh_allocator alloc, void** arr, i32 elemsize, i32 cap) {
	bh__arr* arrptr;

	if (*arr == NULL) {
		if (cap == 0 && elemsize == 0) return 1;

		arrptr = (bh__arr *) bh_alloc(alloc, sizeof(*arrptr) + elemsize * cap);
		if (arrptr == NULL) return 0;

		arrptr->allocator = alloc;
		arrptr->capacity = cap;
		arrptr->length = 0;

	} else {
		arrptr = bh__arrhead(*arr);

		if (arrptr->capacity < cap) {
			void* p;
			i32 newcap = arrptr->capacity;
			while (newcap < cap) newcap = BH_ARR_GROW_FORMULA(newcap);

			p = bh_resize(arrptr->allocator, arrptr, sizeof(*arrptr) + elemsize * newcap);

			if (p) {
				arrptr = (bh__arr *) p;
				arrptr->capacity = newcap;
			} else {
				return 0;
			}
		}
	}

	*arr = arrptr + 1;
	return 1;
}

b32 bh__arr_shrink(void** arr, i32 elemsize, i32 cap) {
	if (*arr == NULL) return 0;

	bh__arr* arrptr = bh__arrhead(*arr);
	cap = bh_max(cap, arrptr->length);

	if (arrptr->capacity > cap) {
		void* p = bh_resize(arrptr->allocator, arrptr, sizeof(*arrptr) + elemsize * cap);

		if (p) {
			arrptr = (bh__arr *) p;
			arrptr->capacity = cap;
		} else {
			return 0;
		}
	}

	*arr = arrptr + 1;
	return 1;
}

b32 bh__arr_free(void **arr) {
	bh__arr* arrptr = bh__arrhead(*arr);
	bh_free(arrptr->allocator, arrptr);
	*arr = NULL;
}

void* bh__arr_copy(bh_allocator alloc, void *arr, i32 elemsize) {
	bh__arr* arrptr = bh__arrhead(arr);

	const i32 cap = arrptr->length;

	void* newarr = NULL;
	bh__arr_grow(alloc, &newarr, elemsize, cap);
	bh__arrhead(newarr)->length = cap;
	bh__arrhead(newarr)->capacity = cap;
	memcpy(newarr, arr, elemsize * arrptr->length);

	return newarr;
}

void bh__arr_deleten(void **arr, i32 elemsize, i32 index, i32 numelems) {
	bh__arr* arrptr = bh__arrhead(*arr);

	if (index >= arrptr->length) return; // Can't delete past the end of the array
	if (numelems <= 0) return; // Can't delete nothing

	memmove(
		(char *)(*arr) + elemsize * index,					// Target
		(char *)(*arr) + elemsize * (index + numelems),		// Source
		elemsize * (arrptr->length - (index + numelems)));	// Length
	arrptr->length -= numelems;
}

void bh__arr_insertn(void **arr, i32 elemsize, i32 index, i32 numelems) {
	if (numelems) {
		if (*arr == NULL) {
			bh__arr_grow(bh_arr_allocator(arr), arr, elemsize, numelems); // Making a new array
			return;
		}

		bh__arr* arrptr = bh__arrhead(*arr);
		if (!bh__arr_grow(bh_arr_allocator(arr), arr, elemsize, arrptr->length + numelems)) return; // Fail case
		memmove(
			(char *)(*arr) + elemsize * (index + numelems),
			(char *)(*arr) + elemsize * index,
			elemsize * (arrptr->length - index));
		arrptr->length += numelems;
	}
}

#endif // ifndef BH_NO_ARRAY















//-------------------------------------------------------------------------------------
// HASHTABLE IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_HASHTABLE

b32 bh__hash_init(bh_allocator allocator, bh__hash **table) {
	*table = bh_alloc(allocator, sizeof(bh__hash));
	if (*table == NULL) return 0;

	(*table)->allocator = allocator;

	for (i32 i = 0; i < BH__HASH_MODULUS; i++) {
		(*table)->arrs[i] = NULL;
	}

	return 1;
}

b32 bh__hash_free(bh__hash **table) {
	for (i32 i = 0; i < BH__HASH_MODULUS; i++) {
		if ((*table)->arrs[i] != NULL) {
			bh_arr_free((*table)->arrs[i]);
		}
	}

	bh_free((*table)->allocator, *table);
	*table = NULL;
}

// Assumes NULL terminated string for key
ptr bh__hash_put(bh__hash *table, i32 elemsize, char *key) {
	u64 index = bh__hash_function(key, 0);

	elemsize += BH__HASH_STORED_KEY_SIZE;

	ptr arrptr = table->arrs[index];
	i32 len = bh_arr_length(arrptr);

	while (len--) {
		if (strncmp(key, (char *) arrptr, BH__HASH_STORED_KEY_SIZE) == 0) goto found_matching;
		arrptr = bh_pointer_add(arrptr, elemsize);
	}

	// Didn't find it in the array, make a new one
	arrptr = table->arrs[index];
	len = bh_arr_length(arrptr);
	bh__arr_grow(table->allocator, &arrptr, elemsize, len + 1);
	bh__arrhead(arrptr)->length++;
	table->arrs[index] = arrptr;

	arrptr = bh_pointer_add(arrptr, elemsize * len);
	strncpy(arrptr, key, BH__HASH_STORED_KEY_SIZE);

found_matching:
	return bh_pointer_add(arrptr, BH__HASH_STORED_KEY_SIZE);
}

b32 bh__hash_has(bh__hash *table, i32 elemsize, char *key) {
	u64 index = bh__hash_function(key, 0);

	ptr arrptr = table->arrs[index];
	if (arrptr == NULL) return 0;

	i32 len = bh_arr_length(arrptr);
	i32 stride = elemsize + BH__HASH_STORED_KEY_SIZE;

	while (len--) {
		if (strncmp(key, (char *) arrptr, BH__HASH_STORED_KEY_SIZE) == 0) return 1;
		arrptr = bh_pointer_add(arrptr, stride);
	}

	return 0;
}

ptr bh__hash_get(bh__hash *table, i32 elemsize, char *key) {
	u64 index = bh__hash_function(key, 0);

	ptr arrptr = table->arrs[index];
	if (arrptr == NULL) return NULL;

	i32 stride = elemsize + BH__HASH_STORED_KEY_SIZE;

	i32 len = bh_arr_length(arrptr);
	while (len--) {
		if (strncmp(key, (char *) arrptr, BH__HASH_STORED_KEY_SIZE) == 0) {
			return bh_pointer_add(arrptr, BH__HASH_STORED_KEY_SIZE);
		}

		return bh_pointer_add(arrptr, stride);
	}

	return NULL;
}

void bh__hash_delete(bh__hash *table, i32 elemsize, char *key) {
	u64 index = bh__hash_function(key, 0);

	ptr arrptr = table->arrs[index];
	i32 len = bh_arr_length(arrptr);
	if (arrptr == NULL) return; // Didn't exist

	i32 stride = elemsize + BH__HASH_STORED_KEY_SIZE;
	i32 i = 0;

	while (len && strncmp(key, (char *) arrptr, BH__HASH_STORED_KEY_SIZE) != 0) {
		arrptr = bh_pointer_add(arrptr, stride);
		i++, len--;
	}

	if (len == 0) return; // Didn't exist

	bh__arr_deleten((void **) &arrptr, elemsize, i, 1);
}

bh_hash_iterator bh__hash_iter_setup(bh__hash *table, i32 elemsize) {
	bh_hash_iterator it = {
		.tab = table->arrs,
		.endtab = table->arrs + BH__HASH_MODULUS,
		.elemsize = elemsize,
		.entry = NULL
	};
	return it;
}

b32 bh_hash_iter_next(bh_hash_iterator* it) {
	if (it->tab == NULL) return 0;

	if (it->entry != NULL) {
		it->arrlen--;
		if (it->arrlen <= 0) {
			it->tab++;
			goto step_to_next;
		}

		it->entry = (bh__hash_entry *)bh_pointer_add(it->entry, BH__HASH_STORED_KEY_SIZE + it->elemsize);
		return 1;
	}

step_to_next:
	// Step forward to find next valid
	while (*it->tab == NULL && it->tab != it->endtab) {
		it->tab++;
	}

	if (it->tab == it->endtab) return 0;

	it->entry = *it->tab;
	it->arrlen = bh_arr_length(it->entry);
	return 1;
}

#endif // ifndef BH_NO_HASHTABLE

#endif // ifdef BH_DEFINE

#endif // ifndef BH_H
