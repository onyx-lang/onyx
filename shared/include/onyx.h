#ifndef ONYX_H
#define ONYX_H

#include <stdint.h>

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
	ONYX_OPTION_DISABLE_CORE,
	ONYX_OPTION_DISABLE_STALE_CODE,

	ONYX_OPTION_OPTIONAL_SEMICOLONS,

	ONYX_OPTION_DISABLE_FILE_CONTENTS,
	ONYX_OPTION_DISABLE_EXTENSIONS,

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


//
// Lifecycle
//

onyx_context_t *onyx_context_create();
void onyx_context_free(onyx_context_t *ctx);

/// Call after all options have been set and before the first `onyx_pump`.
void onyx_options_ready(onyx_context_t *ctx);
onyx_pump_t onyx_pump(onyx_context_t *ctx);

//
// Options
//
int32_t onyx_set_option_cstr(onyx_context_t *ctx, onyx_option_t opt, char *value);
int32_t onyx_set_option_bytes(onyx_context_t *ctx, onyx_option_t opt, char *value, int32_t length);
int32_t onyx_set_option_int(onyx_context_t *ctx, onyx_option_t opt, int32_t value);
void onyx_add_defined_var(onyx_context_t *ctx, char *variable, int32_t variable_length, char *value, int32_t value_length);

//
// Loading code
//

/// Adds a file to the compilation, following typical `#load` rules.
/// 1. `foo:file.onyx` will search in the `foo` mapped folder.
/// 2. `file.onyx` will search in the current directory for `file.onyx`.
void onyx_include_file(onyx_context_t *ctx, char *filename, int32_t length);
void onyx_add_mapped_dir(onyx_context_t *ctx, char *mapped_name, int32_t mapped_length, char *dir, int32_t dir_length);

/// Directly injects Onyx code as a new compilation unit
void onyx_inject_code(onyx_context_t *ctx, uint8_t *code, int32_t length);

//
// Output
//

int32_t     onyx_error_count(onyx_context_t *ctx);
const char *onyx_error_message(onyx_context_t *ctx, int32_t error_idx);
const char *onyx_error_filename(onyx_context_t *ctx, int32_t error_idx);
int32_t     onyx_error_line(onyx_context_t *ctx, int32_t error_idx);
int32_t     onyx_error_column(onyx_context_t *ctx, int32_t error_idx);
int32_t     onyx_error_length(onyx_context_t *ctx, int32_t error_idx);

int32_t onyx_wasm_output_length(onyx_context_t *ctx);
void onyx_wasm_output_write(onyx_context_t *ctx, void *buffer);
#endif



// int main(int argc, char const *argv[]) {
// 	onyx_context_t *ctx = onyx_context_create();
// 
// 	onyx_include_file_cstr(ctx, "hello.onyx");
// 
// 	while (onyx_pump(ctx) == ONYX_PUMP_CONTINUE) {
// 		// Message processing, if enabled
// 	}
// 
// 	int64_t output_length = onyx_wasm_output_length(ctx);
// 	void *output = malloc(output_length);
// 	onyx_wasm_output_write(ctx, output);
// 
// 	FILE* output_fd = fopen("hello.wasm", "wb");
// 	fwrite(output_fd, output, output_length);
// 	fclose(output_fd);
// 
// 	onyx_context_free(ctx);
// }