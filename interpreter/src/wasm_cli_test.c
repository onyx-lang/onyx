#define BH_DEFINE
#include "bh.h"
#include "wasm.h"

#include "ovm_wasm.h"
#include "vm.h"


int main(int argc, char *argv[]) {
    wasm_config_t *config = wasm_config_new();

    // TODO: Add this to public header
    void wasm_config_enable_debug(wasm_config_t *, bool);
    wasm_config_enable_debug(config, true);

    wasm_engine_t *engine = wasm_engine_new_with_config(config);

    wasm_store_t *store = wasm_store_new(engine);

    wasm_byte_vec_t wasm_bytes;
    {
        bh_file_contents contents = bh_file_read_contents(bh_heap_allocator(), argv[1]);

        wasm_bytes.size = contents.length;
        wasm_bytes.data = contents.data;
    }

    wasm_module_t *module = wasm_module_new(store, &wasm_bytes);
    assert(module);

    wasm_importtype_vec_t imports;
    wasm_module_imports(module, &imports);

    fori (i, 0, (int) imports.size) {
        const wasm_name_t *module_name = wasm_importtype_module(imports.data[i]);
        const wasm_name_t *import_name = wasm_importtype_name(imports.data[i]);
        bh_printf("imports: %b.%b\n", module_name->data, module_name->size, import_name->data, import_name->size);
    }


    wasm_exporttype_vec_t exports;
    wasm_module_exports(module, &exports);

    fori (i, 0, (int) exports.size) {
        const wasm_name_t *export_name = wasm_exporttype_name(exports.data[i]);
        bh_printf("exports: %b  %d\n", export_name->data, export_name->size, wasm_externtype_kind(wasm_exporttype_type(exports.data[i])));
    }

    ovm_program_print_instructions(module->program, 0, bh_arr_length(module->program->code));
}

