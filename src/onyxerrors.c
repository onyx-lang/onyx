
#include "onyxerrors.h"
#include "onyxutils.h"

#define MAX_MSGS 100

OnyxErrors errors;

void onyx_errors_init(bh_table(bh_file_contents)* files) {
    errors.file_contents = files;

    bh_arena_init(&errors.msg_arena, global_heap_allocator, 16 * 1024);
    errors.msg_alloc = bh_arena_allocator(&errors.msg_arena);

    bh_arr_new(global_heap_allocator, errors.errors, 4);
}

void onyx_report_error(OnyxFilePos pos, char * format, ...) {

    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .text = bh_strdup(errors.msg_alloc, msg),
    };

    bh_arr_push(errors.errors, err);
}

static void print_detailed_message(OnyxError* err, bh_file_contents* fc) {
    bh_printf("(%s:%l,%l) %s\n", err->pos.filename, err->pos.line, err->pos.column, err->text);

    i32 linelength = 0;
    char* walker = err->pos.line_start;
    while (*walker != '\n') linelength++, walker++;

    i32 numlen = bh_printf(" %d | ", err->pos.line);
    bh_printf("%b\n", err->pos.line_start, linelength);

    char* pointer_str = bh_alloc_array(global_scratch_allocator, char, linelength + numlen);
    memset(pointer_str, ' ', linelength + numlen);
    memset(pointer_str + err->pos.column + numlen - 1, '~', err->pos.length - 1);
    pointer_str[err->pos.column + numlen - 2] = '^';
    pointer_str[err->pos.column + numlen + err->pos.length - 1] = 0;

    bh_printf("%s\n", pointer_str);
}

void onyx_errors_print() {
    // NOTE: If the format of the error messages is ever changed,
    // update onyx_compile.vim and onyx.sublime-build to match
    // the new format. This was editor error highlighting is still
    // supported.
    //
    //                                      - brendanfh   2020/09/03

    bh_arr_each(OnyxError, err, errors.errors) {
        if (err->pos.filename) {
            bh_file_contents* fc = &bh_table_get(bh_file_contents, *errors.file_contents, (char *) err->pos.filename);
            print_detailed_message(err, fc);

        } else {
            bh_printf("(%l,%l) %s\n", err->pos.line, err->pos.column, err->text);
        }
    }
}

b32 onyx_has_errors() {
    return bh_arr_length(errors.errors) > 0;
}
