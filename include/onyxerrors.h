#ifndef ONYXERRORS_H
#define ONYXERRORS_H

#include "bh.h"
#include "onyxlex.h"

#include <stdarg.h>

#define ONYX_ERR_BUFFER_SIZE 256

typedef struct OnyxError {
    OnyxFilePos pos;
    char *text;
} OnyxError;

typedef struct OnyxErrors {
    bh_arena     msg_arena;
    bh_allocator msg_alloc;

    // NOTE: Pointer to a table mapping file paths to
    // their file contents. Used for better error messages
    bh_table(bh_file_contents)* file_contents;

    bh_arr(OnyxError) errors;
} OnyxErrors;

extern OnyxErrors msgs;

void onyx_errors_init(bh_table(bh_file_contents)* files);
void onyx_report_error(OnyxFilePos pos, char * format, ...);
void onyx_errors_print();
b32  onyx_has_errors();

#endif
