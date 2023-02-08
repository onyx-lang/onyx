#include "vm.h"

#include <math.h>

void print_result(void *data, ovm_value_t *params, ovm_value_t *result) {
    switch (params[0].type) {
        case OVM_TYPE_I32: printf("Result: %d\n", params[0].i32); break;
        case OVM_TYPE_F32: printf("Result: %f\n", params[0].f32); break;
    }
}

void c_call_1f64(void *data, ovm_value_t *params, ovm_value_t *result) {
     result->type = OVM_TYPE_F32;
     result->f32  = (f32) ((f64 (*)(f64)) data)(params[0].f32);
}

int main(int argc, char *argv[]) {

    static ovm_linkable_func_t native_funcs[] = {
        { "dummy", 0, { print_result, NULL } },
        { "print", 1, { print_result, NULL } },
        { "sin",   1, { c_call_1f64, sin } },
        { NULL },
    };

    ovm_store_t *store = ovm_store_new();
    ovm_program_t *prog = ovm_program_new(store);
    ovm_engine_t *engine = ovm_engine_new(store);
    ovm_state_t  *state = ovm_state_new(engine, prog);

    ovm_program_load_from_file(prog, engine, argv[1]);

    static int func_table[] = { 0, 1, 6 };
    ovm_program_register_static_ints(prog, 3, func_table);

    ovm_state_link_external_funcs(prog, state, native_funcs);

    state->pc = 0;
    ovm_value_t values[] = {
        { .type = OVM_TYPE_I32, .i32 = 1 },
        { .type = OVM_TYPE_I32, .i32 = 2 },
    };
    ovm_value_t result = ovm_func_call(engine, state, prog, 5, 0, values);
    // printf("%d %d\n", result.type, result.i32);

    ovm_state_delete(state);
    ovm_engine_delete(engine);
    ovm_program_delete(prog);
    ovm_store_delete(store);

    return 0;
}
