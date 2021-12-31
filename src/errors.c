#include "errors.h"
#include "utils.h"

OnyxErrors errors;

void onyx_errors_init(bh_arr(bh_file_contents)* files) {
    errors.file_contents = files;

    bh_arena_init(&errors.msg_arena, global_heap_allocator, 16 * 1024);
    errors.msg_alloc = bh_arena_allocator(&errors.msg_arena);

    bh_arr_new(global_heap_allocator, errors.errors, 4);
}

static void print_detailed_message(OnyxError* err, bh_file_contents* fc) {
    bh_printf("(%s:%l,%l) %s\n", err->pos.filename, err->pos.line, err->pos.column, err->text);

    b32 colored_printing = 0;
    #ifdef _BH_LINUX
        colored_printing = !context.options->no_colors;
    #endif

    i32 linelength = 0;
    i32 first_char = 0;
    char* walker = err->pos.line_start;
    while (*walker == ' ' || *walker == '\t') first_char++, linelength++, walker++;
    while (*walker != '\n') linelength++, walker++;

    if (colored_printing) bh_printf("\033[90m");
    i32 numlen = bh_printf(" %d | ", err->pos.line);
    if (colored_printing) bh_printf("\033[94m");
    bh_printf("%b\n", err->pos.line_start, linelength);

    char* pointer_str = bh_alloc_array(global_scratch_allocator, char, linelength + numlen);
    memset(pointer_str, ' ', numlen);
    memcpy(pointer_str + numlen - 1, err->pos.line_start, first_char);
    memset(pointer_str + first_char + numlen - 1, ' ', err->pos.column - first_char);
    memset(pointer_str + err->pos.column + numlen - 1, '~', err->pos.length - 1);
    pointer_str[err->pos.column + numlen - 2] = '^';
    pointer_str[err->pos.column + numlen + err->pos.length - 1] = 0;

    if (colored_printing) bh_printf("\033[91m");
    bh_printf("%s\n", pointer_str);

    if (colored_printing) bh_printf("\033[0m");
}

static i32 errors_sort(const void* v1, const void* v2) {
    OnyxError* e1 = (OnyxError *) v1;
    OnyxError* e2 = (OnyxError *) v2;
    return e2->rank - e1->rank;
}

void onyx_errors_print() {
    // NOTE: If the format of the error messages is ever changed,
    // update onyx_compile.vim and onyx.sublime-build to match
    // the new format. This was editor error highlighting is still
    // supported.
    //
    //                                      - brendanfh   2020/09/03

    qsort(errors.errors, bh_arr_length(errors.errors), sizeof(OnyxError), errors_sort);

    OnyxErrorRank last_rank = errors.errors[0].rank;
    bh_arr_each(OnyxError, err, errors.errors) {
        if (last_rank != err->rank) break;

        if (err->pos.filename) {
            bh_file_contents file_contents = { 0 };
            bh_arr_each(bh_file_contents, fc, *errors.file_contents) {
                if (!strcmp(fc->filename, err->pos.filename)) {
                    file_contents = *fc;
                    break;
                }
            }

            print_detailed_message(err, &file_contents);

        } else {
            bh_printf("(%l,%l) %s\n", err->pos.line, err->pos.column, err->text);
        }

        last_rank = err->rank;
    }
}

b32 onyx_has_errors() {
    return bh_arr_length(errors.errors) > 0;
}

void onyx_clear_errors() {
    if (context.cycle_detected) return;

    bh_arr_set_length(errors.errors, 0);
}

void onyx_submit_error(OnyxError error) {
    bh_arr_push(errors.errors, error);
}

void onyx_report_error(OnyxFilePos pos, OnyxErrorRank rank, char * format, ...) {

    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .rank = rank,
        .text = bh_strdup(errors.msg_alloc, msg),
    };

    bh_arr_push(errors.errors, err);
}

void onyx_submit_warning(OnyxError error) {
    bh_file_contents file_contents = { 0 };
    bh_arr_each(bh_file_contents, fc, *errors.file_contents) {
        if (!strcmp(fc->filename, error.pos.filename)) {
            file_contents = *fc;
            break;
        }
    }

    print_detailed_message(&error, &file_contents);
}

// This definitely doesn't do what I thought it did?
void onyx_report_warning(OnyxFilePos pos, char* format, ...) {
    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .rank = Error_Warning,
        .text = msg,
    };

    bh_arr_push(errors.errors, err);

    /*

    bh_file_contents file_contents = { 0 };
    bh_arr_each(bh_file_contents, fc, *errors.file_contents) {
        if (!strcmp(fc->filename, pos.filename)) {
            file_contents = *fc;
            break;
        }
    }

    print_detailed_message(&err, &file_contents);
    */
}
