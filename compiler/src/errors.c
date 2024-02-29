#include "errors.h"
#include "utils.h"

void onyx_errors_init(bh_arr(bh_file_contents)* files) {
    context.errors.file_contents = files;

    bh_arena_init(&context.errors.msg_arena, global_heap_allocator, 16 * 1024);
    context.errors.msg_alloc = bh_arena_allocator(&context.errors.msg_arena);

    bh_arr_new(global_heap_allocator, context.errors.errors, 4);
}

static void print_error_text(char *text) {
    if (context.options->no_colors) {
        bh_printf("%s", text);
        return;
    }

    char *ch = text;
    b32 in_color = 0;

    while (*ch != '\0') {
        if (*ch == '\'') {
            in_color = !in_color;
            if (in_color) bh_printf("\033[92m");
            else          bh_printf("\033[0m");
        } else {
            bh_printf("%c", *ch);
        }

        ch++;
    }
}

static void print_detailed_message(OnyxError* err, bh_file_contents* fc) {
    b32 colored_printing = 0;
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        colored_printing = !context.options->no_colors;
    #endif

    if (colored_printing) {
        switch (err->rank) {
            case Error_Warning:
                bh_printf("\033[93mwarning\033[0m: ");
                print_error_text(err->text);
                bh_printf("\n\033[90m     at: %s:%l,%l\033[0m\n", err->pos.filename, err->pos.line, err->pos.column);
                break;

            default:
                bh_printf("\033[91merror\033[0m: ");
                print_error_text(err->text);
                bh_printf("\n\033[90m   at: %s:%l,%l\033[0m\n", err->pos.filename, err->pos.line, err->pos.column);
                break;
        }
    } else {
        switch (err->rank) {
            case Error_Warning:
                bh_printf("warning: ");
                print_error_text(err->text);
                bh_printf("\n     at: %s:%l,%l\n", err->pos.filename, err->pos.line, err->pos.column);
                break;

            default:
                bh_printf("error: ");
                print_error_text(err->text);
                bh_printf("\n   at: %s:%l,%l\n", err->pos.filename, err->pos.line, err->pos.column);
                break;
        }
    }

    i32 linelength = 0;
    i32 first_char = 0;
    char* walker = err->pos.line_start;
    while (*walker == ' ' || *walker == '\t') first_char++, linelength++, walker++;
    while (*walker != '\n') linelength++, walker++;


    char numbuf[32];
    i32 numlen = bh_snprintf(numbuf, 31, " %d | ", err->pos.line);

    if (colored_printing) bh_printf("\033[90m");
    fori (i, 0, numlen - 3) bh_printf(" ");
    bh_printf("|\n%s", numbuf);
    if (colored_printing) bh_printf("\033[94m");

    bh_printf("%b\n", err->pos.line_start, linelength);

    if (colored_printing) bh_printf("\033[90m");
    fori (i, 0, numlen - 3) bh_printf(" ");
    bh_printf("|  ");
    if (colored_printing) bh_printf("\033[94m");

    char* pointer_str = bh_alloc_array(global_scratch_allocator, char, linelength);
    memset(pointer_str, ' ', linelength);
    memcpy(pointer_str - 1, err->pos.line_start, first_char);
    memset(pointer_str + first_char - 1, ' ', err->pos.column - first_char);
    memset(pointer_str + err->pos.column - 1, '~', err->pos.length - 1);
    pointer_str[err->pos.column - 2] = '^';
    pointer_str[err->pos.column + err->pos.length - 1] = 0;

    if (colored_printing) bh_printf("\033[91m");
    bh_printf("%s\n", pointer_str);
    if (colored_printing) bh_printf("\033[0m\n");
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

    qsort(context.errors.errors, bh_arr_length(context.errors.errors), sizeof(OnyxError), errors_sort);

    OnyxErrorRank last_rank = context.errors.errors[0].rank;
    bh_arr_each(OnyxError, err, context.errors.errors) {
        if (!context.options->show_all_errors && last_rank != err->rank) break;

        if (err->pos.filename) {
            bh_file_contents file_contents = { 0 };
            bh_arr_each(bh_file_contents, fc, *context.errors.file_contents) {
                if (!strcmp(fc->filename, err->pos.filename)) {
                    file_contents = *fc;
                    break;
                }
            }

            print_detailed_message(err, &file_contents);

        } else {
            bh_printf("\033[91merror\033[0m: ");
            bh_printf("%s\n", err->text);
            bh_printf("\033[90m   at: command line argument\033[0m\n");
        }

        last_rank = err->rank;
    }
}

void onyx_errors_enable() {
    context.errors_enabled = 1;
}

void onyx_errors_disable() {
    if (context.cycle_detected) {
        context.errors_enabled = 1;
        return;
    }
    
    context.errors_enabled = 0;
}

b32 onyx_errors_are_enabled() {
    return context.errors_enabled;
}

b32 onyx_has_errors() {
    bh_arr_each(OnyxError, err, context.errors.errors) {
        if (err->rank >= Error_Waiting_On) return 1;
    }

    return 0;
}

void onyx_clear_errors() {
    if (context.cycle_detected) return;

    bh_arr_set_length(context.errors.errors, 0);
}

void onyx_submit_error(OnyxError error) {
    if (!context.errors_enabled) return;

    bh_arr_push(context.errors.errors, error);
}

void onyx_report_error(OnyxFilePos pos, OnyxErrorRank rank, char * format, ...) {
    if (!context.errors_enabled) return;

    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .rank = rank,
        .text = bh_strdup(context.errors.msg_alloc, msg),
    };

    bh_arr_push(context.errors.errors, err);
}

void onyx_submit_warning(OnyxError error) {
    if (!context.errors_enabled) return;

    bh_file_contents file_contents = { 0 };
    bh_arr_each(bh_file_contents, fc, *context.errors.file_contents) {
        if (!strcmp(fc->filename, error.pos.filename)) {
            file_contents = *fc;
            break;
        }
    }

    print_detailed_message(&error, &file_contents);
}

// This definitely doesn't do what I thought it did?
void onyx_report_warning(OnyxFilePos pos, char* format, ...) {
    if (!context.errors_enabled) return;

    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .rank = Error_Warning,
        .text = bh_strdup(context.errors.msg_alloc, msg),
    };

    bh_arr_push(context.errors.errors, err);

    /*

    bh_file_contents file_contents = { 0 };
    bh_arr_each(bh_file_contents, fc, *context.errors.file_contents) {
        if (!strcmp(fc->filename, pos.filename)) {
            file_contents = *fc;
            break;
        }
    }

    print_detailed_message(&err, &file_contents);
    */
}
