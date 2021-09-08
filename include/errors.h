#ifndef ONYXERRORS_H
#define ONYXERRORS_H

#include "bh.h"
#include "lex.h"

#include <stdarg.h>

typedef struct OnyxError {
    OnyxFilePos pos;
    char *text;
} OnyxError;

typedef struct OnyxErrors {
    bh_arena     msg_arena;
    bh_allocator msg_alloc;

    // NOTE: Pointer to a array of all the loaded files.
    bh_arr(bh_file_contents)* file_contents;

    bh_arr(OnyxError) errors;
} OnyxErrors;

extern OnyxErrors msgs;

void onyx_errors_init(bh_arr(bh_file_contents)* files);
void onyx_report_error(OnyxFilePos pos, char * format, ...);
void onyx_report_warning(OnyxFilePos pos, char* format, ...);
void onyx_errors_print();
b32  onyx_has_errors();
void onyx_clear_errors();

#endif
