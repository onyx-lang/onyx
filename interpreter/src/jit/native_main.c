#include "bh.h"
#include "ovm_jit.h"
#include "onyx_library.h"

extern u8  onyx_jit_text[];
extern i32 onyx_func_count;
extern i32 onyx_func_entry_off[];
extern i32 onyx_func_body_off[];
extern u32 onyx_wasm_len;
extern u8  onyx_wasm[];

void onyx_run_initialize(b32 debug_enabled, const char *debug_socket, b32 jit_enabled);
b32  onyx_run_wasm_code(bh_buffer code_buffer, int argc, char *argv[]);
WasmFuncDefinition **onyx_library_onyx_runtime(OnyxRuntime *in_runtime);

int main(int argc, char *argv[]) {
    /* Strong ref so the linker pulls static onyx_runtime out of the archive. */
    if ((void *) onyx_library_onyx_runtime == NULL) return 1;

    static ovm_aot_image_t img;
    img.text = onyx_jit_text;
    img.entry_off = onyx_func_entry_off;
    img.body_off = onyx_func_body_off;
    img.nfuncs = onyx_func_count;
    ovm_jit_set_aot_image(&img);

    onyx_run_initialize(0, NULL, 0);

    bh_buffer data;
    data.data = (char *) onyx_wasm;
    data.length = (i32) onyx_wasm_len;
    return onyx_run_wasm_code(data, argc, argv) ? 0 : 1;
}
