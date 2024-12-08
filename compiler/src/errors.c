#include "errors.h"
#include "utils.h"

void onyx_errors_init(Context *context, bh_arr(bh_file_contents)* files) {
    context->errors.file_contents = files;

    bh_arena_init(&context->errors.msg_arena, context->gp_alloc, 16 * 1024);
    context->errors.msg_alloc = bh_arena_allocator(&context->errors.msg_arena);

    bh_arr_new(context->gp_alloc, context->errors.errors, 4);
}

static i32 errors_sort(const void* v1, const void* v2) {
    OnyxError* e1 = (OnyxError *) v1;
    OnyxError* e2 = (OnyxError *) v2;
    return e2->rank - e1->rank;
}

void onyx_errors_enable(Context *context) {
    context->errors_enabled = 1;
}

void onyx_errors_disable(Context *context) {
    if (context->cycle_detected) {
        context->errors_enabled = 1;
        return;
    }
    
    context->errors_enabled = 0;
}

b32 onyx_errors_are_enabled(Context *context) {
    return context->errors_enabled;
}

b32 onyx_has_errors(Context *context) {
    bh_arr_each(OnyxError, err, context->errors.errors) {
        if (err->rank >= Error_Waiting_On) return 1;
    }

    return 0;
}

void onyx_clear_errors(Context *context) {
    if (context->cycle_detected) return;

    bh_arr_set_length(context->errors.errors, 0);
}

void onyx_submit_error(Context *context, OnyxError error) {
    if (!context->errors_enabled) return;

    bh_arr_push(context->errors.errors, error);
    qsort(context->errors.errors, bh_arr_length(context->errors.errors), sizeof(OnyxError), errors_sort);
}

void onyx_report_error(Context *context, OnyxFilePos pos, OnyxErrorRank rank, char * format, ...) {
    if (!context->errors_enabled) return;

    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .rank = rank,
        .text = bh_strdup(context->errors.msg_alloc, msg),
    };

    bh_arr_push(context->errors.errors, err);
    qsort(context->errors.errors, bh_arr_length(context->errors.errors), sizeof(OnyxError), errors_sort);
}

void onyx_report_warning(Context *context, OnyxFilePos pos, char* format, ...) {
    if (!context->errors_enabled) return;

    va_list vargs;
    va_start(vargs, format);
    char* msg = bh_bprintf_va(format, vargs);
    va_end(vargs);

    OnyxError err = {
        .pos = pos,
        .rank = Error_Warning,
        .text = bh_strdup(context->errors.msg_alloc, msg),
    };

    bh_arr_push(context->errors.errors, err);
    qsort(context->errors.errors, bh_arr_length(context->errors.errors), sizeof(OnyxError), errors_sort);
}
