//
// THIS FILE WAS AUTOMATICALLY GENERATED.
//

#include "onyx.h"


#define ONYX_LIBRARY_NAME onyx_autocompiler
#include "onyx_library.h"

#define P(i, k) (params->data[i].of.k)

ONYX_DEF(onyx_version_major, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_version_major());
    return NULL;
}

ONYX_DEF(onyx_run_wasm_with_debug, (WASM_I32,WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    onyx_run_wasm_with_debug(ONYX_PTR(P(0, i32)), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(onyx_run_wasm, (WASM_I32,WASM_I32, WASM_I32, WASM_I32), ()) {
    onyx_run_wasm(ONYX_PTR(P(0, i32)), P(1, i32), P(2, i32), ONYX_PTR(P(3, i32)));
    return NULL;
}

ONYX_DEF(onyx_stat_filepath, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_stat_filepath((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_stat, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_stat((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_output_write, (WASM_I64, WASM_I32, WASM_I32), ()) {
    onyx_output_write((onyx_context_t *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(onyx_output_length, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_output_length((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_rank, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_error_rank((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_line_text, (WASM_I64, WASM_I32, WASM_I32,WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_error_line_text((onyx_context_t *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_length, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_error_length((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_column, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_error_column((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_line, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_error_line((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_filename, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_error_filename((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_message, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_error_message((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_error_count, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_error_count((onyx_context_t *) P(0, i64)));
    return NULL;
}

ONYX_DEF(onyx_inject_code, (WASM_I64, WASM_I32,WASM_I32), ()) {
    onyx_inject_code((onyx_context_t *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32));
    return NULL;
}

ONYX_DEF(onyx_add_mapped_dir, (WASM_I64, WASM_I32,WASM_I32, WASM_I32,WASM_I32), ()) {
    onyx_add_mapped_dir((onyx_context_t *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32));
    return NULL;
}

ONYX_DEF(onyx_include_file, (WASM_I64, WASM_I32,WASM_I32), ()) {
    onyx_include_file((onyx_context_t *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32));
    return NULL;
}

ONYX_DEF(onyx_add_defined_var, (WASM_I64, WASM_I32,WASM_I32, WASM_I32,WASM_I32), ()) {
    onyx_add_defined_var((onyx_context_t *) P(0, i64), ONYX_PTR(P(1, i32)), P(2, i32), ONYX_PTR(P(3, i32)), P(4, i32));
    return NULL;
}

ONYX_DEF(onyx_set_option_int, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_set_option_int((onyx_context_t *) P(0, i64), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(onyx_set_option_bytes, (WASM_I64, WASM_I32, WASM_I32,WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_set_option_bytes((onyx_context_t *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32)), P(3, i32)));
    return NULL;
}

ONYX_DEF(onyx_set_option_cstr, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_set_option_cstr((onyx_context_t *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(onyx_event_field_str, (WASM_I64, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_event_field_str((onyx_context_t *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(onyx_event_field_int, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_event_field_int((onyx_context_t *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32))));
    return NULL;
}

ONYX_DEF(onyx_event_type, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_event_type((onyx_context_t *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(onyx_event_count, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_event_count((onyx_context_t *) P(0, i64)));
    return NULL;
}

ONYX_DEF(onyx_pump, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_pump((onyx_context_t *) P(0, i64)));
    return NULL;
}

ONYX_DEF(onyx_options_ready, (WASM_I64), ()) {
    onyx_options_ready((onyx_context_t *) P(0, i64));
    return NULL;
}

ONYX_DEF(onyx_context_free, (WASM_I64), ()) {
    onyx_context_free((onyx_context_t *) P(0, i64));
    return NULL;
}

ONYX_DEF(onyx_context_create, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(onyx_context_create());
    return NULL;
}

ONYX_DEF(onyx_version_runtime, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_version_runtime());
    return NULL;
}

ONYX_DEF(onyx_version_build_time, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_version_build_time());
    return NULL;
}

ONYX_DEF(onyx_version_suffix, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL((int64_t) onyx_version_suffix());
    return NULL;
}

ONYX_DEF(onyx_version_patch, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_version_patch());
    return NULL;
}

ONYX_DEF(onyx_version_minor, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(onyx_version_minor());
    return NULL;
}



ONYX_LIBRARY {
    ONYX_FUNC(onyx_version_major)
    ONYX_FUNC(onyx_run_wasm_with_debug)
    ONYX_FUNC(onyx_run_wasm)
    ONYX_FUNC(onyx_stat_filepath)
    ONYX_FUNC(onyx_stat)
    ONYX_FUNC(onyx_output_write)
    ONYX_FUNC(onyx_output_length)
    ONYX_FUNC(onyx_error_rank)
    ONYX_FUNC(onyx_error_line_text)
    ONYX_FUNC(onyx_error_length)
    ONYX_FUNC(onyx_error_column)
    ONYX_FUNC(onyx_error_line)
    ONYX_FUNC(onyx_error_filename)
    ONYX_FUNC(onyx_error_message)
    ONYX_FUNC(onyx_error_count)
    ONYX_FUNC(onyx_inject_code)
    ONYX_FUNC(onyx_add_mapped_dir)
    ONYX_FUNC(onyx_include_file)
    ONYX_FUNC(onyx_add_defined_var)
    ONYX_FUNC(onyx_set_option_int)
    ONYX_FUNC(onyx_set_option_bytes)
    ONYX_FUNC(onyx_set_option_cstr)
    ONYX_FUNC(onyx_event_field_str)
    ONYX_FUNC(onyx_event_field_int)
    ONYX_FUNC(onyx_event_type)
    ONYX_FUNC(onyx_event_count)
    ONYX_FUNC(onyx_pump)
    ONYX_FUNC(onyx_options_ready)
    ONYX_FUNC(onyx_context_free)
    ONYX_FUNC(onyx_context_create)
    ONYX_FUNC(onyx_version_runtime)
    ONYX_FUNC(onyx_version_build_time)
    ONYX_FUNC(onyx_version_suffix)
    ONYX_FUNC(onyx_version_patch)
    ONYX_FUNC(onyx_version_minor)
    NULL
};