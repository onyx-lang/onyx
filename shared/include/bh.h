#ifndef BH_H
#define BH_H

#ifdef BH_STATIC
    #define BH_DEF static
#else
    #ifdef BH_INTERNAL
        #define BH_DEF __attribute__((visibility("hidden")))
    #else
        #define BH_DEF
    #endif
#endif

#ifndef BH_INTERNAL_ALLOCATOR
    #define BH_INTERNAL_ALLOCATOR (bh_heap_allocator())
#endif


// NOTE: For lseek64
#define _LARGEFILE64_SOURCE

#if defined(_WIN32) || defined(_WIN64)
    #define _BH_WINDOWS 1
#endif

#if defined(__unix__)
    #define _BH_LINUX 1
#endif

#if defined(__MACH__) && defined(__APPLE__)
    #define _BH_DARWIN 1
#endif

#include <sys/stat.h>
#include <time.h>

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    #include <errno.h>
    #include <fcntl.h>
    #include <unistd.h>
    #include <dirent.h>
    #include <pthread.h>
    #include <sys/select.h>
#endif

#if defined(_BH_LINUX) || defined(_BH_WINDOWS)
    #include <malloc.h>
#endif

#if defined(_BH_LINUX)
    #include <sys/inotify.h>
#endif

#if defined(_BH_DARWIN)
    #include <sys/malloc.h>
    #include <sys/event.h>
    #include <sys/types.h>
#endif

#include <stdlib.h>
#include <stdarg.h>
#include <string.h> // TODO: Replace with needed functions
#include <stdint.h>
#include <assert.h>
#include <stdio.h>

#if defined(_MSC_VER) && !defined(_WINDOWS_)
    #include "small_windows.h"
#endif

//-------------------------------------------------------------------------------------
// Better types
//-------------------------------------------------------------------------------------
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef int64_t isize;
typedef u32 b32;
typedef void* ptr;
typedef float f32;
typedef double f64;




















//-------------------------------------------------------------------------------------
// Better character functions
//-------------------------------------------------------------------------------------
b32 char_is_alpha(const char a);
b32 char_is_num(const char a);
b32 char_is_alphanum(const char a);
char charset_contains(const char* charset, char ch);
b32 char_is_whitespace(const char a);
b32 char_in_range(const char lo, const char hi, const char a);
i64 chars_match(char* ptr1, char* ptr2);





//-------------------------------------------------------------------------------------
// Better math functions
//-------------------------------------------------------------------------------------
#define bh_max(a, b)         ((a) > (b) ? (a) : (b))
#define bh_min(a, b)         ((a) < (b) ? (a) : (b))
#define bh_clamp(v, a, b)    (bh_min((b), bh_max((a), (v))))
#define bh_abs(x)            ((x) < 0 ? -(x) : (x))
#define size_of(x)           (isize) sizeof(x)

static inline u64 log2_dumb(u64 n) {
    switch (n) {
    case 1 << 0:  return 0;
    case 1 << 1:  return 1;
    case 1 << 2:  return 2;
    case 1 << 3:  return 3;
    case 1 << 4:  return 4;
    case 1 << 5:  return 5;
    case 1 << 6:  return 6;
    case 1 << 7:  return 7;
    case 1 << 8:  return 8;
    case 1 << 9:  return 9;
    case 1 << 10: return 10;
    case 1 << 11: return 11;
    case 1 << 12: return 12;
    case 1 << 13: return 13;
    case 1 << 14: return 14;
    case 1 << 15: return 15;
    case 1 << 16: return 16;
    case 1 << 17: return 17;
    case 1 << 18: return 18;
    case 1 << 19: return 19;
    case 1 << 20: return 20;
    case 1 << 21: return 21;
    case 1 << 22: return 22;
    case 1 << 23: return 23;
    case 1 << 24: return 24;
    case 1 << 25: return 25;
    case 1 << 26: return 26;
    case 1 << 27: return 27;
    case 1 << 28: return 28;
    case 1 << 29: return 29;
    case 1 << 30: return 30;
    case 1 << 31: return 31;

    default:   return 0;
    }
}

static inline const char* bh_num_plural(u64 i) {
    return i != 1 ? "s" : "";
}

static inline const char* bh_num_suffix(u64 i) {
    if (i == 11 || i == 12 || i == 13) return "th";

    switch (i % 10) {
        case 0: return "th";
        case 1: return "st";
        case 2: return "nd";
        case 3: return "rd";
        case 4: return "th";
        case 5: return "th";
        case 6: return "th";
        case 7: return "th";
        case 8: return "th";
        case 9: return "th";

        default: return "";
    }
}

static inline u32 bh_clz(u32 i) {
    #ifdef _BH_WINDOWS
    return __lzcnt(i);
    #else
    return __builtin_clz(i);
    #endif
}




//-------------------------------------------------------------------------------------
// Conversion functions
//-------------------------------------------------------------------------------------

// Converts an unsigned integer to the unsigned LEB128 format
u8* uint_to_uleb128(u64 n, i32* output_length);
u8* int_to_leb128(i64 n, i32* output_length);
u8* float_to_ieee754(f32 f, b32 reverse);
u8* double_to_ieee754(f64 f, b32 reverse);

u64 uleb128_to_uint(u8* bytes, i32 *byte_walker);
BH_DEF i64 leb128_to_int(u8* bytes, i32 *byte_count);



//-------------------------------------------------------------------------------------
// Helpful macros
//-------------------------------------------------------------------------------------
#define bh_offset_of(Type, elem)        ((isize)&(((Type)*) 0)->elem)
#define bh_align_of(Type)                bh_offset_of(struct { char c; Type member; }, member)
#define bh_swap(Type, a, b)                do { Type tmp = (a); (a) = (b); (b) = tmp; } while(0)

#define bh_align(x, a) if ((x) % (a) != 0) (x) += (a) - ((x) % (a));

#define bh_pointer_add(ptr, amm)        ((void *)((u8 *) (ptr) + (amm)))
#define BH_BIT(x)                        (1 << (x))
#define BH_MASK_SET(var, set, mask)     ((set) ? ((var) |= (mask)) : ((var) &= ~(mask)))

#define fori(var, lo, hi)                 for (i64 var = (lo); var < (hi); var++)
#define forir(var, hi, lo)                for (i64 var = (hi); var >= (lo); var--)
#define forll(T, var, start, step)        for (T* var = (start); var != NULL; var = (T *) var->step)

#if defined(BH_DEBUG) && defined(_BH_LINUX) && false
    #define DEBUG_HERE                        __asm("int $3")
#else
    #define DEBUG_HERE
#endif





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
    ptr                    data; // Pointer to the other data for the allocator
} bh_allocator;

typedef enum bh_allocator_flags {
    bh_allocator_flag_clear = 1    // Sets all memory to be 0
} bh_allocator_flags;

ptr bh_alloc(bh_allocator a, isize size);
ptr bh_alloc_aligned(bh_allocator a, isize size, isize alignment);
ptr bh_resize(bh_allocator a, ptr data, isize new_size);
ptr bh_resize_aligned(bh_allocator a, ptr data, isize new_size, isize alignment);
void bh_free(bh_allocator a, ptr data);

#define bh_alloc_item(allocator_, T)                (T *) bh_alloc(allocator_, sizeof(T))
#define bh_alloc_array(allocator_, T, n)            (T *) bh_alloc(allocator_, sizeof(T) * (n))

// NOTE: This should get optimized out since alignment should be a power of two
#define bh__align(x, alignment)                        ((((x) / alignment) + 1) * alignment)




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

BH_DEF void bh_arena_init(bh_arena* alloc, bh_allocator backing, isize arena_size);
BH_DEF void bh_arena_clear(bh_arena* alloc);
BH_DEF void bh_arena_free(bh_arena* alloc);
BH_DEF bh_allocator bh_arena_allocator(bh_arena* alloc);
BH_DEF BH_ALLOCATOR_PROC(bh_arena_allocator_proc);


// ATOMIC ARENA ALLOCATOR
// Currently, this is only available on Linux/MacOS, as it is using pthreads.
#if defined(_BH_LINUX) || defined(_BH_DARWIN)

typedef struct bh_atomic_arena {
    bh_allocator backing;
    ptr first_arena, current_arena;
    isize size, arena_size; // in bytes

    pthread_mutex_t mutex;
} bh_atomic_arena;

typedef struct bh__atomic_arena_internal {
    ptr next_arena;
    void* data; // Not actually a pointer, just used for the offset
} bh__atomic_arena_internal;

BH_DEF void bh_atomic_arena_init(bh_atomic_arena* alloc, bh_allocator backing, isize arena_size);
BH_DEF void bh_atomic_arena_free(bh_atomic_arena* alloc);
BH_DEF bh_allocator bh_atomic_arena_allocator(bh_atomic_arena* alloc);
BH_DEF BH_ALLOCATOR_PROC(bh_atomic_arena_allocator_proc);

#endif





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
// Allocator based string functions
//-------------------------------------------------------------------------------------

b32 bh_str_starts_with(char* str, char* start);
b32 bh_str_ends_with(char* str, char* end);
b32 bh_str_contains(char *str, char *needle);
u32 bh_str_last_index_of(char *str, char needle);
char* bh_strdup(bh_allocator a, char* str);
char* bh_strdup_len(bh_allocator a, char* str, i32 len);













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

#ifdef _BH_WINDOWS
    typedef HANDLE bh_file_descriptor;
#else
    typedef int bh_file_descriptor;
#endif

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
    isize line_count;
} bh_file_contents;


#define BH_FILE_TYPE_DIRECTORY 3
#define BH_FILE_TYPE_FILE      4
#define BH_FILE_TYPE_LINK      5
typedef struct bh_file_stats {
    isize size;
    u32 file_type;
    u64 change_time;
    u64 accessed_time;
    u64 modified_time;
} bh_file_stats;

bh_file_error bh_file_get_standard(bh_file* file, bh_file_standard stand);

bh_file_error bh_file_create(bh_file* file, char const* filename);
bh_file_error bh_file_open(bh_file* file, char const* filename);
bh_file_error bh_file_open_mode(bh_file* file, bh_file_mode mode, const char* filename);
bh_file_error bh_file_new(bh_file* file, bh_file_descriptor fd, const char* filename);
b32 bh_file_read_at(bh_file* file, i64 offset, void* buffer, isize buff_size, isize* bytes_read);
b32 bh_file_write_at(bh_file* file, i64 offset, void const* buffer, isize buff_size, isize* bytes_wrote);
i64 bh_file_seek(bh_file* file, i64 offset, bh_file_whence whence);
i64 bh_file_seek_to(bh_file* file, i64 offset);
i64 bh_file_seek_to_end(bh_file* file);
i64 bh_file_skip(bh_file* file, i64 bytes);
i64 bh_file_tell(bh_file* file);
bh_file_error bh_file_close(bh_file* file);
i32 bh_file_read(bh_file* file, void* buffer, isize buff_size);
i32 bh_file_write(bh_file* file, void* buffer, isize buff_size);
void bh_file_flush(bh_file* file);
i64 bh_file_size(bh_file* file);
b32 bh_file_stat(char const* filename, bh_file_stats* stat);
b32 bh_file_exists(char const* filename);
b32 bh_file_remove(char const* filename);
char* bh_path_get_full_name(char const* filename, bh_allocator a);
char* bh_path_get_parent(char const* filename, bh_allocator a);
char* bh_path_convert_separators(char* path);


typedef struct bh_mapped_folder {
    char *name;
    char *folder;
} bh_mapped_folder;

// This function returns a volatile pointer. Do not store it without copying!
// `included_folders` is bh_arr(const char *).
// 'mapped_folders' is bh_arr(bh_mapped_folder).
char* bh_lookup_file(char* filename, char* relative_to, char *suffix, const char ** included_folders, bh_mapped_folder* mapped_folders, bh_allocator allocator);

char* bh_search_for_mapped_file(char* filename, char* relative_to, char *suffix, bh_mapped_folder* mapped_folders, bh_allocator allocator);

#define bh_file_read_contents(allocator_, x) _Generic((x), \
    bh_file*: bh_file_read_contents_bh_file, \
    const char*: bh_file_read_contents_direct, \
    char*: bh_file_read_contents_direct)((allocator_), x)

bh_file_contents bh_file_read_contents_bh_file(bh_allocator alloc, bh_file* file);
bh_file_contents bh_file_read_contents_direct(bh_allocator alloc, const char* filename);
b32 bh_file_contents_free(bh_file_contents* contents);


#ifdef _BH_WINDOWS
    typedef struct Windows_Directory_Opened {
        HANDLE hndl;
        WIN32_FIND_DATAA found_file;
    } Windows_Directory_Opened;

    typedef Windows_Directory_Opened *bh_dir;
#else
    typedef DIR *bh_dir;
#endif

typedef enum bh_dirent_type {
    BH_DIRENT_UNKNOWN,
    BH_DIRENT_BLOCK,
    BH_DIRENT_CHAR,
    BH_DIRENT_DIRECTORY,
    BH_DIRENT_FILE,
    BH_DIRENT_SYMLINK,
    BH_DIRENT_OTHER,
} bh_dirent_type;

typedef struct bh_dirent {
    bh_dirent_type type;
    u32 id;
    u32 name_length;
    char name[256];
} bh_dirent;

bh_dir bh_dir_open(char* path);
b32    bh_dir_read(bh_dir dir, bh_dirent* out);
void   bh_dir_close(bh_dir dir);



#if defined(_BH_LINUX)
    typedef struct bh_file_watch {
        int inotify_fd;
        int kill_pipe[2];

        fd_set fds;
    } bh_file_watch;
#endif
#if defined(_BH_DARWIN)
    typedef struct bh_file_watch {
        int kqueue_fd;
        struct kevent *listeners;
    } bh_file_watch;
#endif
#if defined(_BH_WINDOWS)
    // TODO: Make these work on Windows
    typedef u32 bh_file_watch;
#endif

bh_file_watch bh_file_watch_new();
void bh_file_watch_free(bh_file_watch *w);
void bh_file_watch_add(bh_file_watch *w, const char *filename);
b32 bh_file_watch_wait(bh_file_watch *w);
void bh_file_watch_stop(bh_file_watch *w);



#endif










//-------------------------------------------------------------------------------------
// Alternate printing
//-------------------------------------------------------------------------------------
// Barebones implementation of printf. Does not support all format options
// Currently supports:
//         %c - chars
//        %_(u)d - ints where _ is:
//            nothing - decimal
//            o - octal
//            x - hexadecimal
//        %_(u)l - longs where _ is:
//            nothing - decimal
//            o - octal
//            x - hexadecimal
//        %f - floating points
//        %s - null terminated strings
//        %p - pointers
//        %% - literal %

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
char* bh_aprintf(bh_allocator alloc, const char* fmt, ...);
char* bh_aprintf_va(bh_allocator alloc, const char* fmt, va_list va);
isize bh_snprintf(char *str, isize n, char const *fmt, ...);
isize bh_snprintf_va(char *str, isize n, char const *fmt, va_list va);



















//-------------------------------------------------------------------------------------
// Flexible buffer
//-------------------------------------------------------------------------------------

typedef struct bh_buffer {
    bh_allocator allocator;
    i32 length, capacity;
    u8* data;
} bh_buffer;

#ifndef BH_BUFFER_GROW_FORMULA
#define BH_BUFFER_GROW_FORMULA(x)            ((x) > 0 ? ((x) << 1) : 16)
#endif

BH_DEF void bh_buffer_init(bh_buffer* buffer, bh_allocator alloc, i32 length);
BH_DEF void bh_buffer_free(bh_buffer* buffer);
BH_DEF void bh_buffer_clear(bh_buffer* buffer);
BH_DEF void bh_buffer_grow(bh_buffer* buffer, i32 length);
BH_DEF void bh_buffer_append(bh_buffer* buffer, const void * data, i32 length);
BH_DEF void bh_buffer_concat(bh_buffer* buffer, bh_buffer other);
BH_DEF void bh_buffer_write_byte(bh_buffer* buffer, u8 byte);
BH_DEF void bh_buffer_write_u32(bh_buffer* buffer, u32 i);
BH_DEF void bh_buffer_write_u64(bh_buffer* buffer, u64 i);
BH_DEF void bh_buffer_write_string(bh_buffer* buffer, char *str);
BH_DEF void bh_buffer_align(bh_buffer* buffer, u32 alignment);














//-------------------------------------------------------------------------------------
// Better dynamically-sized arrays
//-------------------------------------------------------------------------------------
#ifndef BH_NO_ARRAY

typedef struct bh__arr {
    bh_allocator allocator;
    i32 length, capacity;
} bh__arr;

#ifndef BH_ARR_GROW_FORMULA
#define BH_ARR_GROW_FORMULA(x)       ((x) > 0 ? ((x) << 1) : 4)
#endif

#define bh_arr(T)                    T*
#define bh__arrhead(arr)             (((bh__arr *)(arr)) - 1)

#define bh_arr_allocator(arr)        (arr ? bh__arrhead(arr)->allocator : BH_INTERNAL_ALLOCATOR)
#define bh_arr_allocator_assert(arr) (arr ? bh__arrhead(arr)->allocator : (assert(0 && "UNSET ALLOCATOR"), ((bh_allocator) {0})))
#define bh_arr_length(arr)           (arr ? bh__arrhead(arr)->length : 0)
#define bh_arr_capacity(arr)         (arr ? bh__arrhead(arr)->capacity : 0)
#define bh_arr_size(arr)             (arr ? bh__arrhead(arr)->capacity * sizeof(*(arr)) : 0)
#define bh_arr_valid(arr, i)         (arr ? (i32)(i) < bh__arrhead(arr)->length : 0)

#define bh_arr_pop(arr)              ((arr)[--bh__arrhead(arr)->length])
#define bh_arr_last(arr)             ((arr)[bh__arrhead(arr)->length - 1])
#define bh_arr_end(arr, i)           ((i) >= &(arr)[bh_arr_length(arr)])
#define bh_arr_start(arr, i)         ((i) < (arr))

#define bh_arr_new(allocator_, arr, cap)    (bh__arr_grow((allocator_), (void**) &(arr), sizeof(*(arr)), cap))
#define bh_arr_free(arr)                    (bh__arr_free((void**) &(arr)))
#define bh_arr_copy(allocator_, arr)        (bh__arr_copy((allocator_), (arr), sizeof(*(arr))))

#define bh_arr_grow(arr, cap)          (bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), cap))
#define bh_arr_shrink(arr, cap)        (bh__arr_shrink((void **) &(arr), sizeof(*(arr)), cap))
#define bh_arr_set_length(arr, n)      (bh__arrhead(arr)->length = n)

#define bh_arr_insertn(arr, i, n)      (bh__arr_insertn((void **) &(arr), sizeof(*(arr)), i, n))

#define bh_arr_insert_end(arr, n)      ( \
    bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), bh_arr_length(arr) + n), \
    bh__arrhead(arr)->length += n)

#define bh_arr_push(arr, value)       ( \
    bh_arr_length(arr) + 1 > bh_arr_capacity(arr) ? bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), bh_arr_length(arr) + 1) : 0, \
    arr[bh__arrhead(arr)->length++] = value)

#define bh_arr_push_unsafe(arr, value)       ( \
    bh_arr_length(arr) + 1 > bh_arr_capacity(arr) ? bh__arr_grow(bh_arr_allocator_assert(arr), (void **) &(arr), sizeof(*(arr)), bh_arr_length(arr) + 1) : 0, \
    arr[bh__arrhead(arr)->length++] = value)

#define bh_arr_set_at(arr, n, value) ( \
    bh__arr_grow(bh_arr_allocator(arr), (void **) &(arr), sizeof(*(arr)), (n) + 1), \
    bh_arr_set_length((arr), bh_max(bh_arr_length(arr), (i32) (n) + 1)), \
    arr[n] = value)

#define bh_arr_is_empty(arr)          (arr ? bh__arrhead(arr)->length == 0 : 1)
#define bh_arr_clear(arr)             (arr ? (bh__arrhead(arr)->length = 0) : 0)

#define bh_arr_deleten(arr, i, n)     (bh__arr_deleten((void **) &(arr), sizeof(*(arr)), i, n))
#define bh_arr_fastdelete(arr, i)     (arr[i] = arr[--bh__arrhead(arr)->length])
#define bh_arr_fastdeleten(arr, n)    (bh__arrhead(arr)->length -= n)

#define bh_arr_each(T, var, arr)      for (T* var = (arr); !bh_arr_end((arr), var); var++)
#define bh_arr_rev_each(T, var, arr)  for (T* var = &bh_arr_last((arr)); !bh_arr_start((arr), var); var--)

#define bh_arr_zero(arr) memset(arr, 0, bh_arr_length(arr) * sizeof(*(arr)));

b32 bh__arr_grow(bh_allocator alloc, void** arr, i32 elemsize, i32 cap);
b32 bh__arr_shrink(void** arr, i32 elemsize, i32 cap);
b32 bh__arr_free(void **arr);
void* bh__arr_copy(bh_allocator alloc, void *arr, i32 elemsize);
void bh__arr_insertn(void **arr, i32 elemsize, i32 index, i32 numelems);
void bh__arr_deleten(void **arr, i32 elemsize, i32 index, i32 numelems);

#endif












//-------------------------------------------------------------------------------------
// STRING HASH TABLE FUNCTIONS
//-------------------------------------------------------------------------------------
#ifndef BH_NO_TABLE

#ifdef BH_DEFINE
u64 bh__table_hash_function(const char* str, i32 len, i32 mod) {
    u64 hash = 5381;
    i32 c, l = 0;
    if (len == 0) len = ((u32) 1 << 31) - 1;

    while ((c = *str++) && l++ < len) {
        hash = (hash << 5) + hash + c;
    }

    return hash % mod;
}
#endif

typedef struct bh_table_iterator {
    ptr *tab, *endtab;
    i32 elemsize, arrlen;
    ptr entry;
} bh_table_iterator;

typedef struct bh__table {
    bh_allocator allocator;
    u64 table_size; // NOTE: u64 since padding will make it 8-bytes no matter what
    ptr arrs[];
} bh__table;

#define bh_table(T)        T*

#ifdef BH_TABLE_SIZE_SAFE
    #define bh_table_init(allocator_, tab, hs)    bh__table_init(allocator_, (bh__table **)&(tab), hs)
    #define bh_table_free(tab)                    bh__table_free((bh__table **)&(tab))
    #define bh_table_put(T, tab, key, value)      (assert(sizeof(T) == sizeof(*(tab))), (*((T *) bh__table_put((bh__table *) tab, sizeof(T), key)) = (T) value))
    #define bh_table_has(T, tab, key)             (assert(sizeof(T) == sizeof(*(tab))), (bh__table_has((bh__table *) tab, sizeof(T), key)))
    #define bh_table_get(T, tab, key)             (assert(sizeof(T) == sizeof(*(tab))), (*((T *) bh__table_get((bh__table *) tab, sizeof(T), key))))
    #define bh_table_delete(T, tab, key)          (assert(sizeof(T) == sizeof(*(tab))), bh__table_delete((bh__table *) tab, sizeof(T), key))
    #define bh_table_clear(tab)                   (bh__table_clear((bh__table *) tab))

    #define bh_table_iter_setup(T, tab)           (assert(sizeof(T) == sizeof(*(tab))), bh__table_iter_setup((bh__table *) tab, sizeof(T)))
    #define bh_table_iter_key(it)                 ((char *)(bh_pointer_add(it.entry, it.elemsize + sizeof(u16))))
    #define bh_table_iter_value(T, it)            (*(T *)it.entry)
#else
    #define bh_table_init(allocator_, tab, hs)    bh__table_init(allocator_, (bh__table **)&(tab), hs)
    #define bh_table_free(tab)                    bh__table_free((bh__table **)&(tab))
    #define bh_table_put(T, tab, key, value)      (*((T *) bh__table_put((bh__table *) tab, sizeof(T), key)) = value)
    #define bh_table_has(T, tab, key)             (bh__table_has((bh__table *) tab, sizeof(T), key))
    #define bh_table_get(T, tab, key)             (*((T *) bh__table_get((bh__table *) tab, sizeof(T), key)))
    #define bh_table_delete(T, tab, key)          (bh__table_delete((bh__table *) tab, sizeof(T), key))
    #define bh_table_clear(tab)                   (bh__table_clear((bh__table *) tab))

    #define bh_table_iter_setup(T, tab)           (bh__table_iter_setup((bh__table *) tab, sizeof(T)))
    #define bh_table_iter_key(it)                 ((char *)(bh_pointer_add(it.entry, it.elemsize + sizeof(u16))))
    #define bh_table_iter_value(T, it)            (*(T *)it.entry)
#endif

#define bh_table_each_start(T, table) { \
    bh_table_iterator it = bh_table_iter_setup(T, (table)); \
    while (bh_table_iter_next(&it)) { \
        const char* key = bh_table_iter_key(it); \
        T value = bh_table_iter_value(T, it);
#define bh_table_each_end            } }

b32 bh__table_init(bh_allocator allocator, bh__table **table, i32 table_size);
b32 bh__table_free(bh__table **table);
ptr bh__table_put(bh__table *table, i32 elemsize, char *key);
b32 bh__table_has(bh__table *table, i32 elemsize, char *key);
ptr bh__table_get(bh__table *table, i32 elemsize, char *key);
void bh__table_delete(bh__table *table, i32 elemsize, char *key);
void bh__table_clear(bh__table *table);
bh_table_iterator bh__table_iter_setup(bh__table *table, i32 elemsize);
b32 bh_table_iter_next(bh_table_iterator* it);

#endif
// Using stb_ds for tables now because they are better in every single way.
#define Table(T) struct { char *key; T value; } *








//-------------------------------------------------------------------------------
// IMAP (integer to integer map)
//-------------------------------------------------------------------------------
#ifndef BH_NO_IMAP

typedef u64 bh_imap_entry_t;

typedef struct bh__imap_lookup_result {
    i64 hash_index;
    i64 entry_prev;
    i64 entry_index;
} bh__imap_lookup_result;

typedef struct bh__imap_entry {
    bh_imap_entry_t key, value;
    i64 next;
} bh__imap_entry;

typedef struct bh_imap {
    bh_allocator allocator;

    bh_arr(i64)            hashes;
    bh_arr(bh__imap_entry) entries;
} bh_imap;


void bh_imap_init(bh_imap* imap, bh_allocator alloc, i32 hash_count);
void bh_imap_free(bh_imap* imap);
void bh_imap_put(bh_imap* imap, bh_imap_entry_t key, bh_imap_entry_t value);
b32 bh_imap_has(bh_imap* imap, bh_imap_entry_t key);
bh_imap_entry_t bh_imap_get(bh_imap* imap, bh_imap_entry_t key);
void bh_imap_delete(bh_imap* imap, bh_imap_entry_t key);
void bh_imap_clear(bh_imap* imap);

#ifdef BH_DEFINE
#endif // BH_DEFINE


#endif











// MANAGED HEAP ALLOCATOR
static const u64 bh_managed_heap_magic_number = 0x1337cafedeadbeef;

typedef struct bh_managed_heap__link {
    struct bh_managed_heap__link *prev, *next;
    u64 magic_number;
} bh_managed_heap__link;

typedef struct bh_managed_heap {
    bh_managed_heap__link *first;
} bh_managed_heap;

void bh_managed_heap_init(bh_managed_heap* mh);
void bh_managed_heap_free(bh_managed_heap* mh);
bh_allocator bh_managed_heap_allocator(bh_managed_heap* mh);
BH_ALLOCATOR_PROC(bh_managed_heap_allocator_proc);











//-------------------------------------------------------------------------------
// OTHER COMMON DATA STRUCTURES
//-------------------------------------------------------------------------------
#ifndef BH_NO_DATASTRUCTURES
















#endif // BH_NO_DATASTRUCTURES





//------------------------------------------------------------------------------
// TIME / DURATION
//------------------------------------------------------------------------------
u64 bh_time_curr();
u64 bh_time_curr_micro();
u64 bh_time_duration(u64 old);











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

char charset_contains(const char* charset, char ch) {
    while (*charset) {
        if (*charset == ch) return ch;
        charset++;
    }

    return 0;
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
    return lo <= a && a <= hi;
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
    ptr ret = a.proc(a.data, bh_allocator_action_alloc, size, alignment, NULL,  0);
    if (ret != 0) memset(ret, 0, size);
    return ret;
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
#if defined(_BH_WINDOWS)
        retval = _aligned_malloc(size, alignment);
#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
        i32 success = posix_memalign(&retval, alignment, size);
#endif
        if (flags & bh_allocator_flag_clear && retval != NULL) {
            memset(retval, 0, size);
        }
    } break;

    case bh_allocator_action_resize: {
        // TODO: Maybe replace with better custom function
#if defined(_BH_WINDOWS)
        retval = _aligned_realloc(prev_memory, size, alignment);
#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
        retval = realloc(prev_memory, size);
#endif
    } break;

    case bh_allocator_action_free: {
#if defined(_BH_WINDOWS)
        _aligned_free(prev_memory);
#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
        free(prev_memory);
#endif
    } break;
    }

    return retval;
}






// MANAGED HEAP ALLOCATOR IMPLEMENTATION
void bh_managed_heap_init(bh_managed_heap* mh) {
    mh->first = NULL;
}

void bh_managed_heap_free(bh_managed_heap* mh) {
    bh_managed_heap__link *l = mh->first;
    while (l) {
        bh_managed_heap__link *n = l->next;
        if (l->magic_number == bh_managed_heap_magic_number) {
            l->magic_number = 0;
#if defined(_BH_WINDOWS)
            _aligned_free((void *) l);
#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
            free((void *) l);
#endif
        }

        l = n;
    }

    mh->first = NULL;
}

bh_allocator bh_managed_heap_allocator(bh_managed_heap* mh) {
    return (bh_allocator) {
        .proc = bh_managed_heap_allocator_proc,
        .data = mh
    };
}

BH_ALLOCATOR_PROC(bh_managed_heap_allocator_proc) {
    bh_managed_heap* mh = (bh_managed_heap *) data;

    bh_managed_heap__link *old = NULL;
    if (prev_memory) {
        old = ((bh_managed_heap__link *) prev_memory) - 1;

        if (old->magic_number != bh_managed_heap_magic_number) {
            return bh_heap_allocator_proc(NULL, action, size, alignment, prev_memory, flags);
        }
    }

    if (old && (action == bh_allocator_action_resize || action == bh_allocator_action_free)) {
        if (old->prev) {
            old->prev->next = old->next;
        } else {
            mh->first = old->next;
        }

        if (old->next) {
            old->next->prev = old->prev;
        }
    }

    bh_managed_heap__link *newptr = bh_heap_allocator_proc(NULL, action, size + sizeof(*old), alignment, old, flags);

    if (action == bh_allocator_action_alloc || action == bh_allocator_action_resize) {
        if (newptr) {
            newptr->magic_number = bh_managed_heap_magic_number;
            newptr->next = mh->first;
            newptr->prev = NULL;

            if (mh->first != NULL) {
                mh->first->prev = newptr;
            }

            mh->first = newptr;
        }
    }

    return newptr + 1;
}








// ARENA ALLOCATOR IMPLEMENTATION
void bh_arena_init(bh_arena* alloc, bh_allocator backing, isize arena_size) {
    arena_size = bh_max(arena_size, size_of(ptr));
    ptr data = bh_alloc(backing, arena_size);

    alloc->backing = backing;
    alloc->arena_size = arena_size;
    alloc->size = sizeof(ptr);
    alloc->first_arena = data;
    alloc->current_arena = data;

    ((bh__arena_internal *)(alloc->first_arena))->next_arena = NULL;
}

BH_DEF void bh_arena_clear(bh_arena* alloc) {
    bh__arena_internal *walker = (bh__arena_internal *) alloc->first_arena;
    walker = walker->next_arena;
    if (walker != NULL) {
        bh__arena_internal *trailer = walker;
        while (walker != NULL) {
            walker = walker->next_arena;
            bh_free(alloc->backing, trailer);
            trailer = walker;
        }
    }

    alloc->current_arena = alloc->first_arena;
    alloc->size = sizeof(ptr);
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
        bh_align(size, alignment);
        bh_align(alloc_arena->size, alignment);

        if (size > alloc_arena->arena_size - size_of(ptr)) {
            // Size too large for the arena
            return NULL;
        }

        if (alloc_arena->size + size >= alloc_arena->arena_size) {
            bh__arena_internal* new_arena = (bh__arena_internal *) bh_alloc(alloc_arena->backing, alloc_arena->arena_size);

            if (new_arena == NULL) {
                bh_printf_err("Arena Allocator: couldn't allocate new arena");
                return NULL;
            }

            new_arena->next_arena = NULL;
            ((bh__arena_internal *)(alloc_arena->current_arena))->next_arena = new_arena;
            alloc_arena->current_arena = new_arena;
            alloc_arena->size = sizeof(ptr);
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


// ATOMIC ARENA ALLOCATOR IMPLEMENTATION
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
BH_DEF void bh_atomic_arena_init(bh_atomic_arena* alloc, bh_allocator backing, isize arena_size) {
    arena_size = bh_max(arena_size, size_of(ptr));
    ptr data = bh_alloc(backing, arena_size);

    alloc->backing = backing;
    alloc->arena_size = arena_size;
    alloc->size = sizeof(ptr);
    alloc->first_arena = data;
    alloc->current_arena = data;
    pthread_mutex_init(&alloc->mutex, NULL);

    ((bh__arena_internal *)(alloc->first_arena))->next_arena = NULL;
}

BH_DEF void bh_atomic_arena_free(bh_atomic_arena* alloc) {
    bh__atomic_arena_internal *walker = (bh__atomic_arena_internal *) alloc->first_arena;
    bh__atomic_arena_internal *trailer = walker;
    while (walker != NULL) {
        walker = walker->next_arena;
        bh_free(alloc->backing, trailer);
        trailer = walker;
    }

    alloc->first_arena = NULL;
    alloc->current_arena = NULL;
    alloc->arena_size = 0;
    alloc->size = 0;
    pthread_mutex_destroy(&alloc->mutex);
}

BH_DEF bh_allocator bh_atomic_arena_allocator(bh_atomic_arena* alloc) {
    return (bh_allocator) {
        .proc = bh_atomic_arena_allocator_proc,
        .data = alloc,
    };
}

BH_DEF BH_ALLOCATOR_PROC(bh_atomic_arena_allocator_proc) {
    bh_atomic_arena* alloc_arena = (bh_atomic_arena*) data;
    pthread_mutex_lock(&alloc_arena->mutex);

    ptr retval = NULL;

    switch (action) {
    case bh_allocator_action_alloc: {
        bh_align(size, alignment);
        bh_align(alloc_arena->size, alignment);

        if (size > alloc_arena->arena_size - size_of(ptr)) {
            // Size too large for the arena
            break;
        }

        if (alloc_arena->size + size >= alloc_arena->arena_size) {
            bh__arena_internal* new_arena = (bh__arena_internal *) bh_alloc(alloc_arena->backing, alloc_arena->arena_size);

            if (new_arena == NULL) {
                bh_printf_err("Arena Allocator: couldn't allocate new arena");
                break;
            }

            new_arena->next_arena = NULL;
            ((bh__arena_internal *)(alloc_arena->current_arena))->next_arena = new_arena;
            alloc_arena->current_arena = new_arena;
            alloc_arena->size = sizeof(ptr);
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

    pthread_mutex_unlock(&alloc_arena->mutex);
    return retval;
}
#endif





// SCRATCH ALLOCATOR IMPLEMENTATION
void bh_scratch_init(bh_scratch* scratch, bh_allocator backing, isize scratch_size) {
    ptr memory = bh_alloc(backing, scratch_size);

    scratch->backing = backing;
    scratch->memory = memory;
    scratch->curr = memory;
    scratch->end = bh_pointer_add(memory, scratch_size);
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
        if (size > ((u8 *) scratch->end) - ((u8 *) scratch->memory)) {
            return NULL;
        }

        retval = scratch->curr;
        scratch->curr = bh_pointer_add(scratch->curr, size);

        if (scratch->curr >= scratch->end) {
            retval = scratch->memory;
            scratch->curr = bh_pointer_add(scratch->memory, size);
        }
    } break;

    case bh_allocator_action_free: break;

    case bh_allocator_action_resize: {
        if (size > ((u8 *) scratch->end) - ((u8 *) scratch->memory)) {
            return NULL;
        }

        retval = scratch->curr;
        scratch->curr = bh_pointer_add(scratch->curr, size);

        if (scratch->curr >= scratch->end) {
            scratch->curr = scratch->memory;
            retval = scratch->curr;
        }

        // HACK!!!!!: Using size instead of some kind of "old size"
        memcpy(retval, prev_memory, size);
    } break;
    }

    return retval;
}




//-------------------------------------------------------------------------------------
// CONVERSION FUNCTIONS IMPLEMENTATION
//-------------------------------------------------------------------------------------
u8* uint_to_uleb128(u64 n, i32* output_length) {
    static u8 buffer[16];

    *output_length = 0;
    u8* output = buffer;
    u8 byte;
    do {
        byte = n & 0x7f;
        n >>= 7;
        if (n != 0) byte |= (1 << 7);
        *output++ = byte;
        (*output_length)++;
    } while (n != 0);

    return buffer;
}


// Converts a signed integer to the signed LEB128 format
u8* int_to_leb128(i64 n, i32* output_length) {
    static u8 buffer[16];

    *output_length = 0;
    u8* output = buffer;
    b32 more = 1;

    i32 size = 64;

    u8 byte;
    do {
        byte = n & 0x7f;
        n >>= 7;

        more = !(((n == 0) && (byte & 0x40) == 0) || ((n == -1) && (byte & 0x40) != 0));
        if (more)
            byte |= 0x80;
        *output++ = byte;
        (*output_length)++;
    } while (more);

    return buffer;
}

// NOTE: This assumes the underlying implementation of float on the host
// system is already IEEE-754. This is safe to assume in most cases.
u8* float_to_ieee754(f32 f, b32 reverse) {
    static u8 buffer[4];

    u8* fmem = (u8*) &f;
    if (reverse) {
        buffer[0] = fmem[3];
        buffer[1] = fmem[2];
        buffer[2] = fmem[1];
        buffer[3] = fmem[0];
    } else {
        buffer[0] = fmem[0];
        buffer[1] = fmem[1];
        buffer[2] = fmem[2];
        buffer[3] = fmem[3];
    }

    return buffer;
}

u8* double_to_ieee754(f64 f, b32 reverse) {
    static u8 buffer[8];

    u8* fmem = (u8*) &f;
    if (reverse) {
        buffer[0] = fmem[7];
        buffer[1] = fmem[6];
        buffer[2] = fmem[5];
        buffer[3] = fmem[4];
        buffer[4] = fmem[3];
        buffer[5] = fmem[2];
        buffer[6] = fmem[1];
        buffer[7] = fmem[0];
    } else {
        buffer[0] = fmem[0];
        buffer[1] = fmem[1];
        buffer[2] = fmem[2];
        buffer[3] = fmem[3];
        buffer[4] = fmem[4];
        buffer[5] = fmem[5];
        buffer[6] = fmem[6];
        buffer[7] = fmem[7];
    }

    return buffer;
}

u64 uleb128_to_uint(u8* bytes, i32 *byte_count) {
    u64 res = 0;
    u64 shift = 0;

    while (1) {
        u8 byte = bytes[(*byte_count)++];
        res |= (byte & 0x7f) << shift;
        if ((byte & 0x80) == 0) break;
        shift += 7;
    }
    return res;
}

BH_DEF i64 leb128_to_int(u8* bytes, i32 *byte_count) {
    u64 res = 0;
    u64 shift = 0;
    u64 size = 64;

    u8 byte;
    do {
        byte = bytes[(*byte_count)++];
        res |= ((u64) (byte & 0x7f)) << shift;
        shift += 7;
    } while ((byte & 0x80) != 0);

    if ((shift < size) && (byte & 0x40) != 0) {
        u64 zero_shifted = ~ 0x0;
        zero_shifted = zero_shifted << shift;
        return res | zero_shifted;
    }

    return res;
}



//-------------------------------------------------------------------------------------
// STRING IMPLEMENTATION
//-------------------------------------------------------------------------------------
b32 bh_str_starts_with(char* str, char* start) {
    char* s = str;
    char* p = start;

    while (*s != '\0' && *p != '\0' && *s == *p) s++, p++;

    return *p == '\0';
}

b32 bh_str_ends_with(char* str, char* end) {
    i32 slen = strlen(str);
    i32 elen = strlen(end);

    char* s = str + slen - 1;
    char* e = end + elen - 1;

    while (*e == *s && e != end && s != str) e--, s--;

    return *e == *s;
}

b32 bh_str_contains(char *str, char *needle) {
    while (*str) {
        char *walk = needle;
        while (*str == *walk && *walk) walk++, str++;
        if (*walk) return 1;
    }

    return 0;
}

u32 bh_str_last_index_of(char *str, char needle) {
    u32 count = strlen(str);
    char *end = str + count - 1;

    while (end != str) {
        if (*end == needle) break;
        count -= 1;
        end--;
    }

    if (end == str) count = 0;

    return count;
}

char* bh_strdup(bh_allocator a, char* str) {
    u32 len = strlen(str);
    char* buf = bh_alloc(a, len + 1);

    char* t = buf;
    while ((*t++ = *str++));
    return buf;
}

char* bh_strdup_len(bh_allocator a, char* str, i32 len) {
    if (!str) return NULL;

    if (len < 0) {
        len = strlen(str);
    }

    char* buf = bh_alloc(a, len + 1);

    char* t = buf;
    while (len-- > 0) {
        *t++ = *str++;
    }

    *t = '\0';
    return buf;
}





//-------------------------------------------------------------------------------------
// FILE IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_FILE

static b32 bh__file_seek_wrapper(bh_file_descriptor fd, i64 offset, bh_file_whence whence, i64* new_offset);

bh_file_error bh_file_get_standard(bh_file* file, bh_file_standard stand) {
    const char* filename = NULL;

#if defined(_BH_WINDOWS)
    bh_file_descriptor sd_fd;

    switch (stand) {
    case BH_FILE_STANDARD_INPUT:
        sd_fd = GetStdHandle(STD_INPUT_HANDLE);
        filename = "stdin";
        break;
    case BH_FILE_STANDARD_OUTPUT:
        sd_fd = GetStdHandle(STD_OUTPUT_HANDLE);
        filename = "stdout";
        break;
    case BH_FILE_STANDARD_ERROR:
        sd_fd = GetStdHandle(STD_ERROR_HANDLE);
        filename = "stderr";
        break;
    default:
        return BH_FILE_ERROR_BAD_FD;
    }
    file->fd = sd_fd;

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    i32 sd_fd = -1;

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
#endif


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
#if defined(_BH_WINDOWS)
    DWORD desired_access;
    DWORD creation_disposition;

    switch (mode & BH_FILE_MODE_MODES) {
        case BH_FILE_MODE_READ:
            desired_access = GENERIC_READ;
            creation_disposition = OPEN_EXISTING;
            break;
        case BH_FILE_MODE_WRITE:
            desired_access = GENERIC_WRITE;
            creation_disposition = CREATE_ALWAYS;
            break;
        case BH_FILE_MODE_APPEND:
            desired_access = GENERIC_WRITE;
            creation_disposition = OPEN_ALWAYS;
            break;
        case BH_FILE_MODE_READ | BH_FILE_MODE_RW:
            desired_access = GENERIC_READ | GENERIC_WRITE;
            creation_disposition = OPEN_EXISTING;
            break;
        case BH_FILE_MODE_WRITE | BH_FILE_MODE_RW:
            desired_access = GENERIC_READ | GENERIC_WRITE;
            creation_disposition = CREATE_ALWAYS;
            break;
        case BH_FILE_MODE_APPEND | BH_FILE_MODE_RW:
            desired_access = GENERIC_READ | GENERIC_WRITE;
            creation_disposition = OPEN_ALWAYS;
            break;
        default:
            return BH_FILE_ERROR_INVALID;
    }


    file->fd = CreateFileA(filename,
                    desired_access,
                    FILE_SHARE_READ,
                    NULL,
                    creation_disposition,
                    FILE_ATTRIBUTE_NORMAL,
                    NULL);

    if (file->fd == INVALID_HANDLE_VALUE) {
        return BH_FILE_ERROR_INVALID;
    }

    file->filename = filename;
    return BH_FILE_ERROR_NONE;

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    i32 os_mode = 0;

    switch (mode & BH_FILE_MODE_MODES) {
    case BH_FILE_MODE_READ:                       os_mode = O_RDONLY; break;
    case BH_FILE_MODE_WRITE:                      os_mode = O_WRONLY | O_CREAT | O_TRUNC; break;
    case BH_FILE_MODE_APPEND:                     os_mode = O_WRONLY | O_APPEND | O_CREAT; break;
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
#endif
}

bh_file_error bh_file_new(bh_file* file, bh_file_descriptor fd, const char* filename) {
    file->filename = filename; // This may be unsafe
    file->fd = fd;
    return BH_FILE_ERROR_NONE;
}

b32 bh_file_read_at(bh_file* file, i64 offset, void* buffer, isize buff_size, isize* bytes_read) {
#if defined(_BH_WINDOWS)
    bh_file_seek_to(file, offset);
    BOOL res = ReadFile(file->fd, buffer, buff_size, (i32 *) bytes_read, NULL);
    if (res) return 1;
    else     return 0;

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    if (file->fd == 0) {
        isize res = read(file->fd, buffer, buff_size);
        if (res < 0) return 0;
        if (bytes_read) *bytes_read = res;
        return 1;
    }

    isize res = pread(file->fd, buffer, buff_size, offset);
    if (res < 0) return 0;
    if (bytes_read) *bytes_read = res;
    return 1;
#endif
}

b32 bh_file_write_at(bh_file* file, i64 offset, void const* buffer, isize buff_size, isize* bytes_wrote) {
    isize res;
    i64 current_offset = 0;
    bh__file_seek_wrapper(file->fd, 0, BH_FILE_WHENCE_CURRENT, &current_offset);

#if defined(_BH_WINDOWS)
    if (current_offset != offset) bh_file_seek_to(file, offset);
    res = (isize) WriteFile(file->fd, buffer, buff_size, (i32 *) bytes_wrote, NULL);
    return res;

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    if (current_offset == offset || file->fd == 1 || file->fd == 2) {
        // Standard in and out do like pwrite()
        res = write(file->fd, buffer, buff_size);
    } else {
        res = pwrite(file->fd, buffer, buff_size, offset);
    }
    if (res < 0) return 0;
    if (bytes_wrote) *bytes_wrote = res;

    return 1;
#endif
}

static b32 bh__file_seek_wrapper(bh_file_descriptor fd, i64 offset, bh_file_whence whence, i64* new_offset) {
#if defined(_BH_WINDOWS)
    LARGE_INTEGER new_file_pointer;
    LARGE_INTEGER dest;
    dest.QuadPart = offset;

    BOOL res = SetFilePointerEx(fd, dest, &new_file_pointer, whence);
    *new_offset = new_file_pointer.QuadPart;

    return res;

#elif defined(_BH_LINUX)
    i64 res = lseek64(fd, offset, whence);
    if (res < 0) return 0;
    if (new_offset) *new_offset = res;
    return 1;

#elif defined(_BH_DARWIN)
    i64 res = lseek(fd, offset, whence);
    if (res < 0) return 0;
    if (new_offset) *new_offset = res;
    return 1;
#endif
}

// Returns new offset
i64 bh_file_seek(bh_file* file, i64 offset, bh_file_whence whence) {
    i64 new_offset = -1;
    bh__file_seek_wrapper(file->fd, offset, whence, &new_offset);
    return new_offset;
}

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

#if defined(_BH_WINDOWS)
    BOOL success = CloseHandle(file->fd);
    if (!success) err = BH_FILE_ERROR_INVALID;

    return err;

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    i32 res = close(file->fd);
    if (res < 0)
        err = BH_FILE_ERROR_INVALID;

    return err;
#endif
}

i32 bh_file_read(bh_file* file, void* buffer, isize buff_size) {
    return bh_file_read_at(file, bh_file_tell(file), buffer, buff_size, NULL);
}

i32 bh_file_write(bh_file* file, void* buffer, isize buff_size) {
    return bh_file_write_at(file, bh_file_tell(file), buffer, buff_size, NULL);
}

void bh_file_flush(bh_file* file) {
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    fdatasync(file->fd);
    #endif
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
        .filename  = bh_strdup(alloc, (char *) file->filename),
        .length = 0, .data = NULL, .line_count = 0,
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
    bh_file file = {0};
    bh_file_contents fc = {0};
    if (bh_file_open(&file, filename) == BH_FILE_ERROR_NONE) {
        fc = bh_file_read_contents(alloc, &file);
        bh_file_close(&file);
    }
    return fc;
}

b32 bh_file_contents_free(bh_file_contents* contents) {
    bh_free(contents->allocator, contents->data);
    contents->length = 0;
    return 1;
}

static u64 timespec_to_ms(struct timespec t) {
    return t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

b32 bh_file_stat(char const* filename, bh_file_stats* out) {
    struct stat s;
    if (stat(filename, &s) == -1) {
        return 0;
    }

    out->size = s.st_size;

#if defined(_BH_DARWIN)
    // Apple just has to be different.
    out->modified_time = timespec_to_ms(s.st_mtimespec);
    out->accessed_time = timespec_to_ms(s.st_atimespec);
    out->change_time   = timespec_to_ms(s.st_ctimespec);
#elif defined(_BH_WINDOWS)
    out->modified_time = s.st_mtime * 1000;
    out->accessed_time = s.st_atime * 1000;
    out->change_time   = s.st_ctime * 1000;
#else
    out->modified_time = timespec_to_ms(s.st_mtim);
    out->accessed_time = timespec_to_ms(s.st_atim);
    out->change_time   = timespec_to_ms(s.st_ctim);
#endif

    if ((s.st_mode & S_IFMT) == S_IFDIR) out->file_type = BH_FILE_TYPE_DIRECTORY;
    if ((s.st_mode & S_IFMT) == S_IFREG) out->file_type = BH_FILE_TYPE_FILE;

#if defined(_BH_LINUX) || defined (_BH_DARWIN)
    if ((s.st_mode & S_IFMT) == S_IFLNK) out->file_type = BH_FILE_TYPE_LINK;
#endif

    return 1;
}

b32 bh_file_exists(char const* filename) {
    struct stat s;
    return stat(filename, &s) != -1;
}

b32 bh_file_remove(char const* filename) {
#if defined(_BH_WINDOWS)
    return DeleteFileA(filename);

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    return unlink(filename) == 0;
#endif
}

char* bh_path_get_full_name(char const* filename, bh_allocator a) {
#if defined(_BH_WINDOWS)
    char buffer[4096];
    GetFullPathNameA(filename, 4096, buffer, NULL);

    i32 len = strlen(buffer);
    char* result = bh_alloc_array(a, char, len + 1);
    memmove(result, buffer, len);
    result[len] = 0;

    return result;

#elif defined(_BH_LINUX) || defined (_BH_DARWIN)
    char* p = realpath(filename, NULL);

    // Check if the file did not exists.
    // :Cleanup should this return NULL?
    if (p == NULL) return (char *) filename;

    i32 len = strlen(p);
    char* result = bh_alloc_array(a, char, len + 1);
    memmove(result, p, len);
    result[len] = 0;

    free(p);

    return result;
#endif
}

// NOTE: This assumes the filename is the full path, not relative to anything else.
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    #define DIR_SEPARATOR '/'
#elif defined(_BH_WINDOWS)
    #define DIR_SEPARATOR '\\'
#endif
char* bh_path_get_parent(char const* filename, bh_allocator a) {

    char* result = bh_strdup(a, (char *) filename);
    char* end = result + strlen(result);
    while (*end != DIR_SEPARATOR && end != result) *end-- = '\0';

    return result;
}

// This function returns a volatile pointer. Do not store it without copying!
char* bh_lookup_file(
    char* filename,
    char* relative_to,
    char *suffix,
    bh_arr(const char *) included_folders,
    bh_arr(bh_mapped_folder) mapped_folders,
    bh_allocator allocator
) {
    assert(relative_to != NULL);

    static char path[512];
    fori (i, 0, 512) path[i] = 0;

    static char fn[256];
    fori (i, 0, 256) fn[i] = 0;

    if (suffix && !bh_str_ends_with(filename, suffix)) {
        bh_snprintf(fn, 256, "%s%s", filename, suffix);
    } else {
        bh_snprintf(fn, 256, "%s", filename);
    }

    b32 contains_colon = 0;

    fori (i, 0, 256) {
        if (fn[i] == ':') contains_colon = 1;
        if (fn[i] == '/') fn[i] = DIR_SEPARATOR;
    }

    if (bh_str_starts_with(filename, "./")) {
        if (relative_to[strlen(relative_to) - 1] != DIR_SEPARATOR)
            bh_snprintf(path, 512, "%s%c%s", relative_to, DIR_SEPARATOR, fn + 2);
        else
            bh_snprintf(path, 512, "%s%s", relative_to, fn + 2);

        if (bh_file_exists(path)) return bh_path_get_full_name(path, allocator);

        return path;
    }

    if (contains_colon && mapped_folders) {
        char *source_name = fn;
        char *subpath     = NULL;

        fori (i, 0, 256) {
            if (fn[i] == ':') {
                fn[i] = '\0';
                subpath = &fn[i + 1];
                break;
            }
        }

        assert(subpath);

        bh_arr_each(bh_mapped_folder, folder, mapped_folders) {
            if (!strncmp(source_name, folder->name, 256)) {
                if (folder->folder[strlen(folder->folder) - 1] != DIR_SEPARATOR)
                    bh_snprintf(path, 512, "%s%c%s", folder->folder, DIR_SEPARATOR, subpath);
                else
                    bh_snprintf(path, 512, "%s%s", folder->folder, subpath);

                if (bh_file_exists(path))
                    return bh_path_get_full_name(path, allocator);

                break;
            }
        }
    }

    else if (included_folders) {
        bh_arr_each(const char *, folder, included_folders) {
            if ((*folder)[strlen(*folder) - 1] != DIR_SEPARATOR)
                bh_snprintf(path, 512, "%s%c%s", *folder, DIR_SEPARATOR, fn);
            else
                bh_snprintf(path, 512, "%s%s", *folder, fn);

            if (bh_file_exists(path)) return bh_path_get_full_name(path, allocator);
        }
    }

    return bh_path_get_full_name(fn, allocator);
}

char* bh_search_for_mapped_file(char* filename, char* relative_to, char *suffix, bh_mapped_folder* mapped_folders, bh_allocator allocator) {
    assert(relative_to != NULL);

    static char path[512];
    fori (i, 0, 512) path[i] = 0;

    static char fn[256];
    fori (i, 0, 256) fn[i] = 0;

    if (suffix && !bh_str_ends_with(filename, suffix)) {
        bh_snprintf(fn, 256, "%s%s", filename, suffix);
    } else {
        bh_snprintf(fn, 256, "%s", filename);
    }

    b32 contains_colon = 0;

    fori (i, 0, 256) {
        if (fn[i] == ':') contains_colon = 1;
        if (fn[i] == '/') fn[i] = DIR_SEPARATOR;
    }

    // Absolute path
    #ifdef _BH_WINDOWS
    if (contains_colon && fn[1] == ':') { // Handle C:\...
        if (bh_file_exists(fn)) {
            return bh_path_get_full_name(fn, allocator);
        }
    }
    #endif

    if (fn[0] == '/') {
        if (bh_file_exists(fn)) {
            return bh_path_get_full_name(fn, allocator);
        }
    }

    // mapped_folder:filename
    if (contains_colon) {
        char *source_name = fn;
        char *subpath     = NULL;

        fori (i, 0, 256) {
            if (fn[i] == ':') {
                fn[i] = '\0';
                subpath = &fn[i + 1];
                break;
            }
        }

        assert(subpath);

        bh_arr_each(bh_mapped_folder, folder, mapped_folders) {
            if (!strncmp(source_name, folder->name, 256)) {
                if (folder->folder[strlen(folder->folder) - 1] != DIR_SEPARATOR)
                    bh_snprintf(path, 512, "%s%c%s", folder->folder, DIR_SEPARATOR, subpath);
                else
                    bh_snprintf(path, 512, "%s%s", folder->folder, subpath);

                if (bh_file_exists(path))
                    return bh_path_get_full_name(path, allocator);

                break;
            }
        }

        return NULL;
    }

    // Fallback to relative to, "relative_to"
    if (relative_to[strlen(relative_to) - 1] != DIR_SEPARATOR)
        bh_snprintf(path, 512, "%s%c%s", relative_to, DIR_SEPARATOR, fn);
    else
        bh_snprintf(path, 512, "%s%s", relative_to, fn);

    if (bh_file_exists(path))
        return bh_path_get_full_name(path, allocator);

    return NULL;
}

//
// Modifies the path in-place.
char* bh_path_convert_separators(char* path) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    #define DIR_SEPARATOR '/'
    #define OTHER_SEPARATOR '\\'
#elif defined(_BH_WINDOWS)
    #define DIR_SEPARATOR '\\'
    #define OTHER_SEPARATOR '/'
#endif

    fori (i, 0, (i64) strlen(path)) {
        if (path[i] == OTHER_SEPARATOR) {
            path[i] = DIR_SEPARATOR;
        }
    }

    return path;
}


bh_dir bh_dir_open(char* path) {
#ifdef _BH_WINDOWS
    char new_path[512] = { 0 };
    strncpy(new_path, path, 511);
    bh_path_convert_separators(new_path);
    strncat(new_path, "\\*.*", 511);

    Windows_Directory_Opened* dir = malloc(sizeof(Windows_Directory_Opened));
    dir->hndl = FindFirstFileA(new_path, &dir->found_file);
    if (dir->hndl == INVALID_HANDLE_VALUE) {
        return NULL;
    }

    return dir;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    DIR* dir = opendir(path);
    return dir;
#endif
}

b32 bh_dir_read(bh_dir dir, bh_dirent* out) {

#ifdef _BH_WINDOWS
    do {
        BOOL success = FindNextFileA(dir->hndl, &dir->found_file);
        if (!success) return 0;
    } while (!strcmp(dir->found_file.cFileName, ".") || !strcmp(dir->found_file.cFileName, ".."));

    if (out == NULL) return 1;

    out->type = (dir->found_file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
        ? BH_DIRENT_DIRECTORY : BH_DIRENT_FILE;
    out->id = 0;
    out->name_length = strlen(dir->found_file.cFileName);
    strncpy(out->name, dir->found_file.cFileName, 256);

    return 1;
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct dirent *ent;
    while (1) {
        ent = readdir(dir);
        if (ent == NULL) return 0;

        // Skip the current directory and parent directory
        if (strcmp(ent->d_name, ".") && strcmp(ent->d_name, "..")) break;
    }

    bh_dirent_type type = 0;
    switch (ent->d_type) {
        case DT_UNKNOWN: break;
        case DT_BLK: type = BH_DIRENT_BLOCK; break;
        case DT_CHR: type = BH_DIRENT_CHAR; break;
        case DT_DIR: type = BH_DIRENT_DIRECTORY; break;
        case DT_LNK: type = BH_DIRENT_SYMLINK; break;
        case DT_REG: type = BH_DIRENT_FILE; break;
        default: type = BH_DIRENT_OTHER; break;
    }

    if (out == NULL) return 1;

    out->type = type;
    out->id = (u32) ent->d_ino;
    out->name_length = strlen(ent->d_name);
    strncpy(out->name, ent->d_name, 256);

    return 1;
#endif
}

void bh_dir_close(bh_dir dir) {
#ifdef _BH_WINDOWS
    if (dir == NULL) return;

    FindClose(dir->hndl);
    free(dir);
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    if (dir == NULL) return;
    closedir(dir);
#endif
}

#undef DIR_SEPARATOR

#if defined(_BH_LINUX)

bh_file_watch bh_file_watch_new() {
    // TODO: Proper error checking
    bh_file_watch w;
    assert(pipe(w.kill_pipe) != -1);

    assert((w.inotify_fd = inotify_init()) != -1);

    FD_ZERO(&w.fds);
    FD_SET(w.inotify_fd, &w.fds);
    FD_SET(w.kill_pipe[0], &w.fds);

    return w;
}

void bh_file_watch_free(bh_file_watch *w) {
    close(w->inotify_fd);
    close(w->kill_pipe[0]);
    close(w->kill_pipe[1]);
}

void bh_file_watch_add(bh_file_watch *w, const char *filename) {
    inotify_add_watch(w->inotify_fd, filename, IN_MODIFY);
}

b32 bh_file_watch_wait(bh_file_watch *w) {
    select(FD_SETSIZE, &w->fds, NULL, NULL, NULL);

    if (FD_ISSET(w->kill_pipe[0], &w->fds)) {
        char buf;
        (void) read(w->kill_pipe[0], &buf, sizeof(buf));
        return 0;
    }

    FD_ZERO(&w->fds);
    FD_SET(w->inotify_fd, &w->fds);
    FD_SET(w->kill_pipe[0], &w->fds);

    return 1;
}

void bh_file_watch_stop(bh_file_watch *w) {
    char buf = 'a';
    (void) write(w->kill_pipe[1], &buf, 1);
}

#endif // ifdef _BH_LINUX

#if defined(_BH_DARWIN)

bh_file_watch bh_file_watch_new() {
    bh_file_watch w;

    w.kqueue_fd = kqueue();

    w.listeners = NULL;
    bh_arr_new(bh_heap_allocator(), w.listeners, 4);

    return w;
}

void bh_file_watch_free(bh_file_watch *w) {
    bh_arr_each(struct kevent, ev, w->listeners) {
        close(ev->ident);
    }

    bh_arr_free(w->listeners);
    close(w->kqueue_fd);
}

void bh_file_watch_add(bh_file_watch *w, const char *filename) {
    int new_fd = open(filename, O_EVTONLY);

    struct kevent new_event;
    EV_SET(&new_event, new_fd, EVFILT_VNODE, EV_ADD | EV_ENABLE | EV_CLEAR, 
               NOTE_WRITE | NOTE_EXTEND | NOTE_ATTRIB | NOTE_LINK | NOTE_RENAME | NOTE_REVOKE, 0, NULL);

    bh_arr_push(w->listeners, new_event);
}

b32 bh_file_watch_wait(bh_file_watch *w) {
    struct kevent events;

    int nev = kevent(w->kqueue_fd, w->listeners, bh_arr_length(w->listeners), &events, 1, NULL);
    if (nev == -1) return 0;

    return 1;
}

void bh_file_watch_stop(bh_file_watch *w) {
}

#endif // ifdef _BH_DARWIN

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
    static char buffer[4096];
    isize len = bh_snprintf_va(buffer, sizeof(buffer), fmt, va);
    bh_file_write(f, buffer, len - 1);
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
    isize len = bh_snprintf_va(buffer, sizeof(buffer), fmt, va);
    buffer[len - 1] = 0;
    return buffer;
}

char* bh_aprintf(bh_allocator alloc, const char* fmt, ...) {
    char* res;
    va_list va;
    va_start(va, fmt);
    res = bh_aprintf_va(alloc, fmt, va);
    va_end(va);
    return res;
}

char* bh_aprintf_va(bh_allocator alloc, const char* fmt, va_list va) {
    static char buffer[4096];
    isize len = bh_snprintf_va(buffer, sizeof(buffer), fmt, va);
    char* res = bh_alloc(alloc, len);
    memcpy(res, buffer, len);
    res[len - 1] = 0;
    return res;
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

    b32 negative = value < 0 ? 1 : 0;
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

// TODO: This implementation is VERY VERY BAD AND WRONG. Fix it.
isize bh__printf64(char* str, isize n, f64 value) {
    fori (i, 0, 6) value *= 10.0;
    i64 v = (i64) value;

    isize l1 = bh__printi64(str, n, ((bh__print_format) { .base = 10 }), v / 1000000);
    str += l1;
    n -= l1;

    *str = '.';
    str += 1;
    n -= 1;

    isize l2 = bh__printi64(str, n, ((bh__print_format) { .base = 10 }), bh_abs(v) % 1000000);

    return l1 + l2 + 1;
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

        case 'b': { // String with a length (not null terminated)
            char* s = va_arg(va, char *);
            i32 l = va_arg(va, int);
            len = bh__print_string(str, bh_min(l, n), s);
        } break;

        case 'f': {
            f64 f = va_arg(va, f64);
            len = bh__printf64(str, n, f);
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
// FLEXIBLE BUFFER IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_BUFFER

void bh_buffer_init(bh_buffer* buffer, bh_allocator alloc, i32 init_size) {
    buffer->allocator = alloc;
    buffer->length = 0;
    buffer->capacity = init_size;
    buffer->data = bh_alloc(alloc, init_size);
}

void bh_buffer_free(bh_buffer* buffer) {
    bh_free(buffer->allocator, buffer->data);
    buffer->length = 0;
    buffer->capacity = 0;
}

void bh_buffer_clear(bh_buffer* buffer) {
    buffer->length = 0;
}

void bh_buffer_grow(bh_buffer* buffer, i32 length) {
    if (buffer == NULL) return;

    if (buffer->capacity >= length) {
        // NOTE: Already have enough room
        return;
    }

    i32 newcap = buffer->capacity;
    while (newcap < length) newcap = BH_BUFFER_GROW_FORMULA(newcap);

    ptr new_data = bh_resize(buffer->allocator, buffer->data, newcap);
    if (new_data == NULL) return;

    buffer->capacity = newcap;
    buffer->data = new_data;
}

void bh_buffer_append(bh_buffer* buffer, const void * data, i32 length) {
    if (buffer == NULL) return;

    if (buffer->length + length > buffer->capacity) {
        bh_buffer_grow(buffer, buffer->length + length);
    }

    memcpy(buffer->data + buffer->length, data, length);
    buffer->length += length;
}

void bh_buffer_concat(bh_buffer* buffer, bh_buffer other) {
    bh_buffer_append(buffer, other.data, other.length);
}

void bh_buffer_write_byte(bh_buffer* buffer, u8 byte) {
    bh_buffer_grow(buffer, buffer->length + 1);
    buffer->data[buffer->length++] = byte;
}

void bh_buffer_write_u32(bh_buffer* buffer, u32 i) {
    bh_buffer_grow(buffer, buffer->length + 4);
    *((u32 *) bh_pointer_add(buffer->data, buffer->length)) = i;
    buffer->length += 4;
}

void bh_buffer_write_u64(bh_buffer* buffer, u64 i) {
    bh_buffer_grow(buffer, buffer->length + 8);
    *((u64 *) bh_pointer_add(buffer->data, buffer->length)) = i;
    buffer->length += 8;
}

BH_DEF void bh_buffer_write_string(bh_buffer* buffer, char *str) {
    u32 len = strlen(str);
    bh_buffer_append(buffer, (const char *) str, len);
}

void bh_buffer_align(bh_buffer* buffer, u32 alignment) {
    if (buffer->length % alignment != 0) {
        u32 difference = alignment - (buffer->length % alignment);
        buffer->length += difference;

        bh_buffer_grow(buffer, buffer->length);
    }
}


#endif





















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

        memset(arrptr + 1, 0, elemsize * cap);

        arrptr->allocator = alloc;
        arrptr->capacity = cap;
        arrptr->length = 0;

    } else {
        arrptr = bh__arrhead(*arr);

        if (arrptr->capacity < cap) {
            void* p;
            i32 newcap = arrptr->capacity, oldcap = arrptr->capacity;
            while (newcap < cap) newcap = BH_ARR_GROW_FORMULA(newcap);

            p = bh_resize(arrptr->allocator, arrptr, sizeof(*arrptr) + elemsize * newcap);

            if (p) {
                memset(bh_pointer_add(((bh__arr *) p + 1), elemsize * oldcap), 0, elemsize * (newcap - oldcap - 1));

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
    if (*arr == NULL) return 0;

    bh__arr* arrptr = bh__arrhead(*arr);
    bh_free(arrptr->allocator, arrptr);
    *arr = NULL;
    return 1;
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
        (char *)(*arr) + elemsize * index,                    // Target
        (char *)(*arr) + elemsize * (index + numelems),        // Source
        elemsize * (arrptr->length - (index + numelems)));    // Length
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
        arrptr = bh__arrhead(*arr);
        memmove(
            (char *)(*arr) + elemsize * (index + numelems),
            (char *)(*arr) + elemsize * index,
            elemsize * (arrptr->length - index));
        arrptr->length += numelems;
    }
}

#endif // ifndef BH_NO_ARRAY















//-------------------------------------------------------------------------------------
// TABLE IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_TABLE

b32 bh__table_init(bh_allocator allocator, bh__table **table, i32 table_size) {
    *table = bh_alloc(allocator, sizeof(bh__table) + sizeof(ptr) * table_size);
    if (*table == NULL) return 0;

    (*table)->allocator = allocator;
    (*table)->table_size = table_size;

    for (i32 i = 0; i < table_size; i++) {
        (*table)->arrs[i] = NULL;
    }

    return 1;
}

b32 bh__table_free(bh__table **table) {
    if (*table == NULL) return 0;

    for (u64 i = 0; i < (*table)->table_size; i++) {
        if ((*table)->arrs[i] != NULL) {
            bh_arr_free((*table)->arrs[i]);
        }
    }

    bh_free((*table)->allocator, *table);
    *table = NULL;
    return 1;
}

// Assumes NULL terminated string for key
ptr bh__table_put(bh__table *table, i32 elemsize, char *key) {
    elemsize += (elemsize & 1);

    u64 index = bh__table_hash_function(key, 0, table->table_size);
    u8 arr_was_new = 0;

    ptr arrptr = table->arrs[index];
    if (arrptr == NULL) {
        arr_was_new = 1;
        goto add_new_element;
    }
    u64 len = *(u64 *) arrptr;
    arrptr = bh_pointer_add(arrptr, sizeof(u64));

    u16 key_length = 0;
    while (len--) {
        arrptr = bh_pointer_add(arrptr, elemsize);
        key_length = *(u16 *) arrptr;
        arrptr = bh_pointer_add(arrptr, sizeof(u16));
        if (strncmp(key, (char *) arrptr, key_length) == 0) goto found_matching;
        arrptr = bh_pointer_add(arrptr, key_length);
    }

add_new_element:
    arrptr = table->arrs[index];
    i32 byte_len = bh_arr_length(arrptr);
    if (byte_len == 0) byte_len = sizeof(u64);
    key_length = strlen(key) + 1;

    // NOTE: Align to 16 bytes
    if ((key_length + 2) % 16 != 0) {
        key_length = ((((key_length + 2) >> 4) + 1) << 4) - 2;
    }

    bh__arr_grow(table->allocator, &arrptr, 1, byte_len + elemsize + sizeof(u16) + key_length);
    bh__arrhead(arrptr)->length = byte_len + elemsize + sizeof(u16) + key_length;
    table->arrs[index] = arrptr;

    if (arr_was_new) {
        *(u64 *) arrptr = 1;
    } else {
        (*(u64 *) arrptr)++;
    }

    arrptr = bh_pointer_add(arrptr, byte_len + elemsize);
    *(u16 *) arrptr = key_length;
    arrptr = bh_pointer_add(arrptr, sizeof(u16));
    strncpy(arrptr, key, key_length);

found_matching:
    return bh_pointer_add(arrptr, -(sizeof(u16) + elemsize));
}

b32 bh__table_has(bh__table *table, i32 elemsize, char *key) {
    elemsize += (elemsize & 1);

    u64 index = bh__table_hash_function(key, 0, table->table_size);

    ptr arrptr = table->arrs[index];
    if (arrptr == NULL) return 0;

    u64 len = *(u64 *) arrptr;
    arrptr = bh_pointer_add(arrptr, sizeof(u64));

    u16 key_length = 0;
    while (len--) {
        arrptr = bh_pointer_add(arrptr, elemsize);
        key_length = *(u16 *) arrptr;
        arrptr = bh_pointer_add(arrptr, sizeof(u16));
        if (strncmp(key, (char *) arrptr, key_length) == 0) return 1;
        arrptr = bh_pointer_add(arrptr, key_length);
    }

    return 0;
}

ptr bh__table_get(bh__table *table, i32 elemsize, char *key) {
    elemsize += (elemsize & 1);

    u64 index = bh__table_hash_function(key, 0, table->table_size);

    ptr arrptr = table->arrs[index];
    if (arrptr == NULL) return 0;

    u64 len = *(u64 *) arrptr;
    arrptr = bh_pointer_add(arrptr, sizeof(u64));

    u16 key_length = 0;
    while (len--) {
        arrptr = bh_pointer_add(arrptr, elemsize);
        key_length = *(u16 *) arrptr;
        arrptr = bh_pointer_add(arrptr, sizeof(u16));
        if (strncmp(key, (char *) arrptr, key_length) == 0) {
            return bh_pointer_add(arrptr, -(sizeof(u16) + elemsize));
        }
        arrptr = bh_pointer_add(arrptr, key_length);
    }

    return NULL;
}

void bh__table_delete(bh__table *table, i32 elemsize, char *key) {
    elemsize += (elemsize & 1);

    u64 index = bh__table_hash_function(key, 0, table->table_size);

    ptr arrptr = table->arrs[index], walker;
    if (arrptr == NULL) return; // Didn't exist
    walker = arrptr;

    i32 byte_offset = 8;
    i32 delete_len = 0;

    u64 len = *(u64 *) walker;
    walker = bh_pointer_add(walker, sizeof(u64));

    u16 key_length = 0;
    while (len--) {
        walker = bh_pointer_add(walker, elemsize);
        key_length = *(u16 *) walker;
        walker = bh_pointer_add(walker, sizeof(u16));
        if (strncmp(key, (char *) walker, key_length) == 0) {
            delete_len = elemsize + sizeof(u16) + key_length;
            goto found_matching;
        }
        walker = bh_pointer_add(walker, key_length);
        byte_offset += elemsize + sizeof(u16) + key_length;
    }

    // NOTE: Already didn't exist
    return;

found_matching:
    bh__arr_deleten((void **) &arrptr, 1, byte_offset, delete_len);
    table->arrs[index] = arrptr;
    (*(u64 *) arrptr)--;
}

void bh__table_clear(bh__table *table) {
    for (u64 i = 0; i < table->table_size; i++) {
        if (table->arrs[i] != NULL) {
            // NOTE: Set length property to 0
            *((u64 *) table->arrs[i]) = 0;
            bh_arr_set_length(table->arrs[i], 0);
        }
    }
}

bh_table_iterator bh__table_iter_setup(bh__table *table, i32 elemsize) {
    elemsize += (elemsize & 1);

    bh_table_iterator it = {
        .tab = table->arrs,
        .endtab = table->arrs + table->table_size,
        .elemsize = elemsize,
        .entry = NULL
    };
    return it;
}

b32 bh_table_iter_next(bh_table_iterator* it) {
    if (it->tab == NULL) return 0;

    if (it->entry != NULL) {
        it->arrlen--;
        if (it->arrlen <= 0) {
            it->tab++;
            goto step_to_next;
        }

        it->entry = bh_pointer_add(it->entry, it->elemsize);
        it->entry = bh_pointer_add(it->entry, sizeof(u16) + (*(u16 *) it->entry));
        return 1;
    }

step_to_next:
    // Step forward to find next valid
    while (it->tab != it->endtab && *it->tab == NULL) {
        it->tab++;
    }

    if (it->tab == it->endtab) return 0;

    it->entry = *it->tab;
    it->arrlen = *(u64 *) it->entry;
    it->entry = bh_pointer_add(it->entry, sizeof(u64));
    if (it->arrlen <= 0) {
        it->tab++;
        goto step_to_next;
    }
    return 1;
}

#endif // ifndef BH_NO_HASHTABLE



//-------------------------------------------------------------------------------------
// IMAP IMPLEMENTATION
//-------------------------------------------------------------------------------------
#ifndef BH_NO_IMAP
void bh_imap_init(bh_imap* imap, bh_allocator alloc, i32 hash_count) {
    imap->allocator = alloc;

    imap->hashes  = NULL;
    imap->entries = NULL;

    bh_arr_new(alloc, imap->hashes, hash_count);
    bh_arr_new(alloc, imap->entries, 4);

    fori(count, 0, hash_count) bh_arr_push(imap->hashes, -1);
}

void bh_imap_free(bh_imap* imap) {
    bh_arr_free(imap->hashes);
    bh_arr_free(imap->entries);

    imap->hashes = NULL;
    imap->entries = NULL;
}

bh__imap_lookup_result bh__imap_lookup(bh_imap* imap, bh_imap_entry_t key) {
    bh__imap_lookup_result lr = { -1, -1, -1 };

    u64 hash = 0xcbf29ce484222325ull ^ key;
    u64 n = bh_arr_capacity(imap->hashes);

    lr.hash_index  = hash % n;
    lr.entry_index = imap->hashes[lr.hash_index];
    while (lr.entry_index >= 0) {
        if (imap->entries[lr.entry_index].key == key) {
            return lr;
        }

        lr.entry_prev  = lr.entry_index;
        lr.entry_index = imap->entries[lr.entry_index].next;
    }

    return lr;
}

void bh_imap_put(bh_imap* imap, bh_imap_entry_t key, bh_imap_entry_t value) {
    bh__imap_lookup_result lr = bh__imap_lookup(imap, key);

    if (lr.entry_index >= 0) {
        imap->entries[lr.entry_index].value = value;
        return;
    }

    bh__imap_entry entry;
    entry.key = key;
    entry.value = value;
    entry.next = imap->hashes[lr.hash_index];
    bh_arr_push(imap->entries, entry);

    imap->hashes[lr.hash_index] = bh_arr_length(imap->entries) - 1;
}

b32 bh_imap_has(bh_imap* imap, bh_imap_entry_t key) {
    bh__imap_lookup_result lr = bh__imap_lookup(imap, key);
    return lr.entry_index >= 0;
}

bh_imap_entry_t bh_imap_get(bh_imap* imap, bh_imap_entry_t key) {
    bh__imap_lookup_result lr = bh__imap_lookup(imap, key);
    if (lr.entry_index >= 0) {
        return imap->entries[lr.entry_index].value;
    } else {
        return 0;
    }
}

void bh_imap_delete(bh_imap* imap, bh_imap_entry_t key) {
    bh__imap_lookup_result lr = bh__imap_lookup(imap, key);
    if (lr.entry_index < 0) return;

    if (lr.entry_prev < 0) {
        imap->hashes[lr.hash_index] = imap->entries[lr.entry_index].next;
    } else {
        imap->entries[lr.entry_prev].next = imap->entries[lr.entry_index].next;
    }

    // If it's that last thing in the array, just pop off the end
    if (lr.entry_index == bh_arr_length(imap->entries) - 1) {
        bh_arr_pop(imap->entries);
        return;
    }

    bh_arr_fastdelete(imap->entries, lr.entry_index);
    bh__imap_lookup_result last = bh__imap_lookup(imap, imap->entries[lr.entry_index].key);
    if (last.entry_prev >= 0) {
        imap->entries[last.entry_prev].next = lr.entry_index;
    } else {
        imap->hashes[last.hash_index] = lr.entry_index;
    }
}

void bh_imap_clear(bh_imap* imap) {
    // NOTE: Does not clear out an of the data that was in the map
    bh_arr_each(i64, hash, imap->hashes) *hash = -1;
    bh_arr_set_length(imap->entries, 0);
}

#endif // ifndef BH_NO_IMAP




u64 bh_time_curr_micro() {
#if defined(_BH_WINDOWS)
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);

    ULARGE_INTEGER ull;
    ull.LowPart = ft.dwLowDateTime;
    ull.HighPart = ft.dwHighDateTime;
    return (ull.QuadPart - 116444736000000000ULL) / 10;

#elif defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);

    time_t sec = spec.tv_sec;
    u64 ms  = spec.tv_nsec / 1000;
    if (ms > 999999) {
        sec++;
        ms -= 1000000;
    }

    return sec * 1000000 + ms;
#endif
}

u64 bh_time_curr() {
#if defined(_BH_WINDOWS)
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);

    ULARGE_INTEGER ull;
    ull.LowPart = ft.dwLowDateTime;
    ull.HighPart = ft.dwHighDateTime;
    return (ull.QuadPart - 116444736000000000ULL) / 10000;

#elif defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);

    time_t sec = spec.tv_sec;
    u64 ms  = spec.tv_nsec / 1000000;
    if (ms > 999) {
        sec++;
        ms = 0;
    }

    return sec * 1000 + ms;
#endif
}

u64 bh_time_duration(u64 old) {
    u64 curr = bh_time_curr();
    return curr - old;
}

#endif // ifdef BH_DEFINE

#endif // ifndef BH_H
