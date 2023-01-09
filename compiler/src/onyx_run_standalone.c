#define BH_DEFINE
#define BH_NO_TABLE
#define STB_DS_IMPLEMENTATION
#include "bh.h"

#include "wasm_emit.h"

extern const char _binary__tmp_out_wasm_start;
extern const char _binary__tmp_out_wasm_end;

void onyx_run_initialize(int debug);
int  onyx_run_wasm(bh_buffer, int argc, char **argv);

int main(int argc, char *argv[]) {
    onyx_run_initialize(0);

    bh_buffer data;
    data.data = (char *) &_binary__tmp_out_wasm_start;
    data.length = &_binary__tmp_out_wasm_end - &_binary__tmp_out_wasm_start;
    return onyx_run_wasm(data, argc - 1, argv + 1);
}
