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
    Error_Command_Line_Arg = 5,
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

struct Context;

void onyx_errors_init(struct Context *context, bh_arr(bh_file_contents)* files);
void onyx_errors_enable(struct Context *context);
void onyx_errors_disable(struct Context *context);
b32 onyx_errors_are_enabled(struct Context *context);
void onyx_submit_error(struct Context *context, OnyxError error);
void onyx_report_error(struct Context *context, OnyxFilePos pos, OnyxErrorRank rank, char * format, ...);
void onyx_report_warning(struct Context *context, OnyxFilePos pos, char* format, ...);
void onyx_errors_print(struct Context *context);
b32  onyx_has_errors(struct Context *context);
void onyx_clear_errors(struct Context *context);

#define ONYX_ERROR(pos, rank, ...) (onyx_report_error(context, (pos), (rank), __VA_ARGS__))
#define ONYX_WARNING(pos, ...) (onyx_report_warning(context, (pos), __VA_ARGS__))

#endif
