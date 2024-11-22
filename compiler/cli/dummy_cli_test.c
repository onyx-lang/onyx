#include "onyx.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[]) {
	onyx_context_t *ctx = onyx_context_create();

	onyx_add_mapped_dir(ctx, "core", -1, "/home/brendan/.onyx/core", -1);
	onyx_include_file(ctx, "../tests/hello_world", -1);
	onyx_set_option_int(ctx, ONYX_OPTION_PLATFORM, ONYX_PLATFORM_ONYX);
	onyx_set_option_int(ctx, ONYX_OPTION_MULTI_THREADING, 1);

	onyx_options_ready(ctx);
	while (onyx_pump(ctx) == ONYX_PUMP_CONTINUE) {
		// doing the compilation
	}

	if (onyx_error_count(ctx) > 0) {
		for (int i = 0; i < onyx_error_count(ctx); ++i)
		{
			const char *msg = onyx_error_message(ctx, i);
			const char *file = onyx_error_filename(ctx, i);
			int32_t line = onyx_error_line(ctx, i);

			printf("ERROR %d: %s\n", i, msg);
			printf("  %s:%d\n", file, line);
		}
		return 1;
	}

  	int64_t output_length = onyx_wasm_output_length(ctx);
  	void *output = malloc(output_length);
  	onyx_wasm_output_write(ctx, output);
  
  	FILE* output_fd = fopen("hello.wasm", "wb");
  	fwrite(output, 1, output_length, output_fd);
  	fclose(output_fd);
  
  	onyx_context_free(ctx);
	return 0;
}