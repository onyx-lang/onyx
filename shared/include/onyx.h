#ifndef ONYX_H
#define ONYX_H

#include <stdint.h>

#if defined(_MSC_VER)
    #define API __declspec(dllexport)
#elif defined(__GNUC__)
    #define API __attribute__((visibility("default")))
#else
    #define API
    #pragma warning Unknown dynamic link import/export semantics.
#endif


typedef struct onyx_context_t onyx_context_t;

typedef enum onyx_option_t {
    ONYX_OPTION_NO_OP,
    ONYX_OPTION_CURRENT_DIRECTORY,

    ONYX_OPTION_POST_MVP_FEATURES,
    ONYX_OPTION_MULTI_THREADING,

    ONYX_OPTION_GENERATE_FOREIGN_INFO,
    ONYX_OPTION_GENERATE_TYPE_INFO,
    ONYX_OPTION_GENERATE_METHOD_INFO,
    ONYX_OPTION_GENERATE_DEBUG_INFO,
    ONYX_OPTION_GENERATE_STACK_TRACE,
    ONYX_OPTION_GENERATE_NAME_SECTION,
    ONYX_OPTION_GENERATE_SYMBOL_INFO,
    ONYX_OPTION_GENERATE_LSP_INFO,
    ONYX_OPTION_GENERATE_DOC_INFO,
    ONYX_OPTION_DISABLE_CORE,
    ONYX_OPTION_DISABLE_STALE_CODE,

    ONYX_OPTION_OPTIONAL_SEMICOLONS,

    ONYX_OPTION_DISABLE_FILE_CONTENTS,
    ONYX_OPTION_DISABLE_EXTENSIONS,

    ONYX_OPTION_COLLECT_PERF,

    ONYX_OPTION_PLATFORM,
} onyx_option_t;

typedef enum onyx_pump_t {
    ONYX_PUMP_CONTINUE,
    ONYX_PUMP_DONE,
    ONYX_PUMP_ERRORED,
} onyx_pump_t;

typedef enum onyx_platform_t {
    ONYX_PLATFORM_ONYX   = 1,
    ONYX_PLATFORM_WASI   = 2,
    ONYX_PLATFORM_JS     = 3,
    ONYX_PLATFORM_CUSTOM = 4,
} onyx_platform_t;

typedef enum onyx_error_t {
    ONYX_ERROR_WARNING  = 2,
    ONYX_ERROR_WAITING  = 3,
    ONYX_ERROR_CRITICAL = 4,
    ONYX_ERROR_CLI      = 5,
} onyx_error_t;

typedef enum onyx_output_type_t {
    ONYX_OUTPUT_TYPE_WASM = 0,
    ONYX_OUTPUT_TYPE_JS   = 1,
    ONYX_OUTPUT_TYPE_ODOC = 2,
    ONYX_OUTPUT_TYPE_OSYM = 3,
} onyx_output_type_t;

typedef enum onyx_stat_t {
    ONYX_STAT_FILE_COUNT  = 1,
    ONYX_STAT_LINE_COUNT  = 2,
    ONYX_STAT_TOKEN_COUNT = 3,
} onyx_stat_t;

typedef enum onyx_event_type_t {
    ONYX_EVENT_UNKNOWN           = 0,
    ONYX_EVENT_LOG               = 1,
    ONYX_EVENT_PHASE_START       = 2,
    ONYX_EVENT_SYMBOL_DEFINED    = 3,
    ONYX_EVENT_ALL_TYPES_CHECKED = 4,
} onyx_event_type_t;


//
// Metadata
//

API int32_t onyx_version_major();
API int32_t onyx_version_minor();
API int32_t onyx_version_patch();
API char   *onyx_version_suffix();
API char   *onyx_version_build_time();
API char   *onyx_version_runtime();



//
// Lifecycle
//

API onyx_context_t *onyx_context_create();
API void onyx_context_free(onyx_context_t *ctx);

/// Call after all options have been set and before the first `onyx_pump`.
API void onyx_options_ready(onyx_context_t *ctx);
API onyx_pump_t onyx_pump(onyx_context_t *ctx);


//
// Events
// 

API int32_t           onyx_event_count(onyx_context_t *ctx);
API onyx_event_type_t onyx_event_type(onyx_context_t *ctx, int event_idx);
API int32_t           onyx_event_field_int(onyx_context_t *ctx, int event_idx, char *field);
API const char       *onyx_event_field_str(onyx_context_t *ctx, int event_idx, char *field);


//
// Options
//
API int32_t onyx_set_option_cstr(onyx_context_t *ctx, onyx_option_t opt, char *value);
API int32_t onyx_set_option_bytes(onyx_context_t *ctx, onyx_option_t opt, char *value, int32_t length);
API int32_t onyx_set_option_int(onyx_context_t *ctx, onyx_option_t opt, int32_t value);
API void onyx_add_defined_var(onyx_context_t *ctx, char *variable, int32_t variable_length, char *value, int32_t value_length);

//
// Loading code
//

/// Adds a file to the compilation, following typical `#load` rules.
/// 1. `foo:file.onyx` will search in the `foo` mapped folder.
/// 2. `file.onyx` will search in the current directory for `file.onyx`.
API void onyx_include_file(onyx_context_t *ctx, char *filename, int32_t length);
API void onyx_add_mapped_dir(onyx_context_t *ctx, char *mapped_name, int32_t mapped_length, char *dir, int32_t dir_length);

/// Directly injects Onyx code as a new compilation unit
API void onyx_inject_code(onyx_context_t *ctx, uint8_t *code, int32_t length);

//
// Errors 
//

API int32_t       onyx_error_count(onyx_context_t *ctx);
API const char   *onyx_error_message(onyx_context_t *ctx, int32_t error_idx);
API const char   *onyx_error_filename(onyx_context_t *ctx, int32_t error_idx);
API int32_t       onyx_error_line(onyx_context_t *ctx, int32_t error_idx);
API int32_t       onyx_error_column(onyx_context_t *ctx, int32_t error_idx);
API int32_t       onyx_error_length(onyx_context_t *ctx, int32_t error_idx);
API int32_t       onyx_error_line_text(onyx_context_t *ctx, int32_t error_idx, char *line_buffer, int max_length);
API onyx_error_t  onyx_error_rank(onyx_context_t *ctx, int32_t error_idx);


//
// Code generation
//

API int32_t onyx_output_length(onyx_context_t *ctx, onyx_output_type_t type);
API void    onyx_output_write(onyx_context_t *ctx, onyx_output_type_t type, void *buffer);

//
// Compilation Info
//

API int64_t     onyx_stat(onyx_context_t *ctx, onyx_stat_t stat);
API const char *onyx_stat_filepath(onyx_context_t *ctx, int32_t file_index);


//
// Running WASM
//

API void onyx_run_wasm(void *buffer, int32_t buffer_length, int argc, char **argv);
API void onyx_run_wasm_with_debug(void *buffer, int32_t buffer_length, int argc, char **argv, char *socket_path);

#endif

