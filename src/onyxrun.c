#define BH_DEFINE
#define BH_NO_TABLE
#define STB_DS_IMPLEMENTATION
#include "bh.h"

#include "wasm_emit.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Expected a WASM file to run.\n");
        return 1;
    }

    bh_file wasm_file;
    bh_file_error err = bh_file_open(&wasm_file, argv[1]);
    if (err != BH_FILE_ERROR_NONE) {
        fprintf(stderr, "Failed to open file %s", argv[1]);
        return 1;
    }

    bh_file_contents wasm_data = bh_file_read_contents(bh_heap_allocator(), &wasm_file);
    bh_file_close(&wasm_file);

    bh_buffer data;
    data.data = wasm_data.data;
    data.length = wasm_data.length;
    return onyx_run_wasm(data, argc - 1, argv + 1);
}
