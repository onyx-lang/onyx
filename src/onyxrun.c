#define BH_DEFINE
#define BH_NO_TABLE
#define STB_DS_IMPLEMENTATION
#include "bh.h"

#include "wasm_emit.h"

int main(int argc, char *argv[]) {
    i32 wasm_file_idx = 1;
    if (argc < 2) {
        fprintf(stderr, "Expected a WASM file to run.\n");
        return 1;
    }

    b32 debug = 0;
    if (!strcmp(argv[1], "--debug")) {
        debug = 1;
        wasm_file_idx = 2;
    }

    if (argc < 3) {
        fprintf(stderr, "Expected a WASM file to run.\n");
        return 1;
    }

    onyx_run_initialize(debug);

    bh_file wasm_file;
    bh_file_error err = bh_file_open(&wasm_file, argv[wasm_file_idx]);
    if (err != BH_FILE_ERROR_NONE) {
        fprintf(stderr, "Failed to open file %s", argv[wasm_file_idx]);
        return 1;
    }

    bh_file_contents wasm_data = bh_file_read_contents(bh_heap_allocator(), &wasm_file);
    bh_file_close(&wasm_file);

    bh_buffer data;
    data.data = wasm_data.data;
    data.length = wasm_data.length;
    return onyx_run_wasm(data, argc - 1, argv + 1);
}
