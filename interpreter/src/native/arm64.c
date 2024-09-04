#include "vm.h"

b32 ovm_generate_native_code(ovm_program_t *program, ovm_func_t *func, bh_buffer *code) {
    fori (instr_index, func->start_instr, func->end_instr) {
        ovm_instr_t instr = program->code[instr_index];
    }

    return 1;
}
