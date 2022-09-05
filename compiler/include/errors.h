#ifndef ONYXERRORS_H
#define ONYXERRORS_H

#include "bh.h"
#include "lex.h"

#include <stdarg.h>

typedef enum OnyxErrorRank {
    Error_Undefined = 0,
    Error_Other = 1,
    Error_Warning = 2,
    Error_Waiting_On = 3,
    Error_Critical = 4,
} OnyxErrorRank;

typedef struct OnyxError {
    OnyxFilePos pos;
    OnyxErrorRank rank;
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
void onyx_submit_error(OnyxError error);
void onyx_report_error(OnyxFilePos pos, OnyxErrorRank rank, char * format, ...);
void onyx_submit_warning(OnyxError error);
void onyx_report_warning(OnyxFilePos pos, char* format, ...);
void onyx_errors_print();
b32  onyx_has_errors();
void onyx_clear_errors();

#endif
