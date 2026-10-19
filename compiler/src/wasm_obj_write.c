// Writes a relocatable WASM object (tool-conventions linking v2) so
// wasm-ld can link it with Clang objects. Executable layout is skipped:
// no stack reservation, no _start, no data-segment initializer.
//
// Only functions reachable from a #export, plus #init procedures, are included.
// Executable entry exports are compiled out when runtime.target_type is Object.

#include "wasm_emit.h"
#include "errors.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WASM_TYPE_INT32   0x7F
#define WASM_TYPE_INT64   0x7E
#define WASM_TYPE_FLOAT32 0x7D
#define WASM_TYPE_FLOAT64 0x7C
#define WASM_TYPE_VAR128  0x7B
#define WASM_TYPE_VOID    0x00

#define SYM_FUNCTION 0
#define SYM_DATA     1
#define SYM_GLOBAL   2

#define SYM_BINDING_LOCAL 0x2
#define SYM_UNDEFINED     0x10
#define SYM_EXPORTED      0x20
#define SYM_TLS           0x100

#define RELOC_FUNCTION_INDEX_LEB  0
#define RELOC_MEMORY_ADDR_SLEB    4
#define RELOC_MEMORY_ADDR_I32     5
#define RELOC_GLOBAL_INDEX_LEB    7
#define RELOC_MEMORY_ADDR_TLS_SLEB 21

typedef struct OutSym {
    u8 kind;
    u32 flags;
    u32 index;
    char *name;
    u32 offset;
    u32 size;
} OutSym;

typedef struct OutRel {
    u32 type;
    u32 offset;
    u32 index;
    i32 addend;
} OutRel;

typedef struct ForeignSym {
    AstFunction *fn;
    char *name;
    i32 type_idx;
    i32 sym;
} ForeignSym;

typedef struct ObjImage {
    OnyxWasmModule *module;
    b32 failed;

    i32 nfuncs;
    i32 nglobals;
    i32 ndata;

    u8 *live;
    char **export_name;
    i32 *sym_of_func;

    ForeignSym *foreigns;
    i32 nforeign;

    u8 *global_needed;
    i32 *sym_of_global;
    i32 *defined_globals;
    i32 ndefined_globals;

    u8 *live_data;
    i32 *data_ids;
    i32 ndata_segs;
    i32 *sym_of_datum;

    b32 need_memory;
    b32 need_stack;
    b32 need_tls;
    b32 need_tls_data;
    b32 have_stack;
    b32 have_tls;
    u64 stack_g;
    u64 tls_g;
    i32 stack_sym;
    i32 tls_sym;
    i32 tls_data_sym;
    u32 tls_size;

    i32 *init_syms;
    i32 ninit;

    bh_arr(OutSym) syms;
    bh_arr(OutRel) code_relocs;
    bh_arr(OutRel) data_relocs;
} ObjImage;

static OnyxFilePos fallback_pos(OnyxWasmModule *module) {
    if (module->all_procedures && bh_arr_length(module->all_procedures) > 0) {
        AstFunction *fn = module->all_procedures[0];
        if (fn && fn->token && fn->token->pos.filename && fn->token->pos.line_start)
            return fn->token->pos;
    }
    static char line[] = "\n";
    OnyxFilePos pos;
    memset(&pos, 0, sizeof pos);
    pos.filename = "wasm-object";
    pos.line_start = line;
    pos.line = 1;
    return pos;
}

static void werr(ObjImage *img, OnyxFilePos pos, const char *fmt, ...) {
    img->failed = 1;
    char buf[768];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof buf, fmt, ap);
    va_end(ap);
    if (!pos.filename || !pos.line_start) pos = fallback_pos(img->module);
    onyx_report_error(img->module->context, pos, Error_Critical, "%s", buf);
}

static OnyxFilePos fn_pos(AstFunction *fn) {
    if (fn && fn->token && fn->token->pos.filename && fn->token->pos.line_start)
        return fn->token->pos;
    OnyxFilePos pos;
    memset(&pos, 0, sizeof pos);
    return pos;
}

static char *copy_text(bh_allocator a, const char *text, i32 length) {
    if (length < 0) length = 0;
    char *s = bh_alloc(a, (u32) length + 1);
    if (length && text) memcpy(s, text, (u32) length);
    s[length] = 0;
    return s;
}

static char *copy_token(bh_allocator a, OnyxToken *t) {
    if (!t) return copy_text(a, "", 0);
    return copy_text(a, t->text, t->length);
}

static char *copy_cstr(bh_allocator a, const char *s) {
    if (!s) s = "";
    return copy_text(a, s, (i32) strlen(s));
}

static void buf_uleb(bh_buffer *b, u64 v) {
    i32 n = 0;
    u8 *p = uint_to_uleb128(v, &n);
    bh_buffer_append(b, p, n);
}

static void buf_sleb(bh_buffer *b, i64 v) {
    i32 n = 0;
    u8 *p = int_to_leb128(v, &n);
    bh_buffer_append(b, p, n);
}

static void buf_uleb5(bh_buffer *b, u32 value) {
    u8 p[5];
    for (int i = 0; i < 4; i++) {
        p[i] = (u8) ((value & 0x7f) | 0x80);
        value >>= 7;
    }
    p[4] = (u8) (value & 0x7f);
    bh_buffer_append(b, p, 5);
}

static void buf_sleb5(bh_buffer *b, i32 value) {
    u8 p[5];
    for (int i = 0; i < 4; i++) {
        p[i] = (u8) ((value & 0x7f) | 0x80);
        value >>= 7;
    }
    p[4] = (u8) (value & 0x7f);
    bh_buffer_append(b, p, 5);
}

static void write_wasm_name(bh_buffer *b, const char *s) {
    u32 len = s ? (u32) strlen(s) : 0;
    buf_uleb(b, len);
    if (len) bh_buffer_append(b, s, (i32) len);
}

static void append_section(bh_buffer *out, u8 id, bh_buffer *payload) {
    bh_buffer_write_byte(out, id);
    buf_uleb(out, (u64) payload->length);
    if (payload->length) bh_buffer_append(out, payload->data, payload->length);
}

static void append_custom(bh_buffer *out, const char *name, bh_buffer *body) {
    bh_buffer payload;
    bh_buffer_init(&payload, out->allocator, body->length + (i32) strlen(name) + 8);
    write_wasm_name(&payload, name);
    if (body->length) bh_buffer_append(&payload, body->data, body->length);
    append_section(out, 0, &payload);
    bh_buffer_free(&payload);
}

static u32 align_log2(u32 a) {
    if (a <= 1) return 0;
    u32 l = 0;
    while (l < 31 && (1u << l) < a) l++;
    return l;
}

static b32 function_type_is_c_abi(Type *ft) {
    if (!ft || ft->kind != Type_Kind_Function) return 0;
    if (ft->Function.vararg_arg_pos >= 0) return 0;
    Type *ret = ft->Function.return_type;
    if (ret && type_is_compound(ret)) return 0;
    for (u16 i = 0; i < ft->Function.param_count; i++) {
        Type *p = ft->Function.params[i];
        if (!p) return 0;
        if (type_get_param_pass(p) == Param_Pass_By_Multiple_Values) return 0;
    }
    return 1;
}

static b32 instr_uses_memory(WasmInstructionType t) {
    if (t >= WI_I32_LOAD && t <= WI_MEMORY_GROW) return 1;
    if (t & EXT_INSTR_MASK) return 1;
    if (t & ATOMIC_INSTR_MASK) return 1;
    if ((t & SIMD_INSTR_MASK) && (t & 0xff) <= 0x0b) return 1;
    return 0;
}

static i32 string_length_of(AstNode *node) {
    if (!node) return -1;
    if (node->kind == Ast_Kind_StrLit) return ((AstStrLit *) node)->length;
    if (node->kind == Ast_Kind_File_Contents) return ((AstFileContents *) node)->size;
    return -1;
}

static void resolve_data_id(DatumPatchInfo *patch) {
    if (patch->data_id != 0 || !patch->node_to_use_if_data_id_is_null) return;
    AstNode *node = patch->node_to_use_if_data_id_is_null;
    if (node->kind == Ast_Kind_Memres) patch->data_id = ((AstMemRes *) node)->data_id;
    else if (node->kind == Ast_Kind_StrLit) patch->data_id = ((AstStrLit *) node)->data_id;
    else if (node->kind == Ast_Kind_File_Contents) patch->data_id = ((AstFileContents *) node)->data_id;
}

static void apply_object_fixups(OnyxWasmModule *module) {
    bh_arr_each(CodePatchInfo, patch, module->code_patches) {
        if (patch->kind == Code_Patch_String_Length) {
            i32 length = string_length_of(patch->node_related_to_patch);
            if (length < 0) continue;
            if (patch->func_idx >= (u32) bh_arr_length(module->funcs)) continue;
            WasmFunc *func = &module->funcs[patch->func_idx];
            if (!func->code || patch->instr >= (u32) bh_arr_length(func->code)) continue;
            func->code[patch->instr].data.l = length;
        } else if (patch->kind == Code_Patch_String_Length_In_Data) {
            i32 length = string_length_of(patch->node_related_to_patch);
            if (length < 0) continue;
            if (patch->func_idx == 0 || patch->func_idx > (u32) bh_arr_length(module->data)) continue;
            WasmDatum *datum = &module->data[patch->func_idx - 1];
            if (!datum->data) continue;
            *(i32 *) bh_pointer_add(datum->data, patch->instr) = length;
        }
    }

    bh_arr_each(DatumPatchInfo, patch, module->data_patches) {
        resolve_data_id(patch);
    }
}

static CodePatchInfo *find_code_patch(OnyxWasmModule *module, u32 func, u32 instr, CodePatchInfoKind kind) {
    bh_arr_each(CodePatchInfo, patch, module->code_patches) {
        if (patch->func_idx == func && patch->instr == instr && patch->kind == kind)
            return patch;
    }
    return NULL;
}

static DatumPatchInfo *find_instr_datum_patch(OnyxWasmModule *module, u32 func, u32 instr) {
    bh_arr_each(DatumPatchInfo, patch, module->data_patches) {
        if (patch->kind == Datum_Patch_Instruction && patch->index == func && patch->location == instr)
            return patch;
    }
    return NULL;
}

static i32 foreign_sym_for(ObjImage *img, AstFunction *fn) {
    for (i32 i = 0; i < img->nforeign; i++) {
        if (img->foreigns[i].fn == fn) return img->foreigns[i].sym;
    }
    OnyxToken *tok = fn->foreign.import_name ? fn->foreign.import_name->token : NULL;
    if (!tok) return -1;
    for (i32 i = 0; i < img->nforeign; i++) {
        if (img->foreigns[i].name && tok->length == (i32) strlen(img->foreigns[i].name)
            && memcmp(tok->text, img->foreigns[i].name, (u32) tok->length) == 0)
            return img->foreigns[i].sym;
    }
    return -1;
}

static i32 lookup_foreign_type(OnyxWasmModule *module, const char *name) {
    bh_arr_each(WasmImport, im, module->imports) {
        if (im->kind == WASM_FOREIGN_FUNCTION && im->name && strcmp(im->name, name) == 0)
            return im->idx;
    }
    return -1;
}

static void add_foreign(ObjImage *img, AstFunction *fn) {
    if (foreign_sym_for(img, fn) >= 0) return;

    OnyxToken *tok = fn->foreign.import_name ? fn->foreign.import_name->token : NULL;
    char *name = copy_token(img->module->context->gp_alloc, tok);
    for (i32 i = 0; i < img->nforeign; i++) {
        if (strcmp(img->foreigns[i].name, name) == 0) return;
    }

    i32 type_idx = lookup_foreign_type(img->module, name);
    if (type_idx < 0) {
        werr(img, fn_pos(fn), "Foreign function '%s' was not given a wasm type.", name);
        return;
    }

    if (!function_type_is_c_abi(fn->type)) {
        werr(img, fn_pos(fn),
            "Foreign function '%s' uses a slice, multiple return values, or a variadic parameter, so it cannot be imported by a wasm object.",
            name);
        return;
    }

    img->foreigns = realloc(img->foreigns, (size_t) (img->nforeign + 1) * sizeof(ForeignSym));
    img->foreigns[img->nforeign].fn = fn;
    img->foreigns[img->nforeign].name = name;
    img->foreigns[img->nforeign].type_idx = type_idx;
    img->foreigns[img->nforeign].sym = -1;
    img->nforeign++;
}

static b32 mark_data(ObjImage *img, u32 id, OnyxFilePos pos) {
    if (id == 0 || (i32) id > img->ndata) {
        werr(img, pos, "A wasm object relocation points at a missing data symbol.");
        return 0;
    }
    img->live_data[id] = 1;
    return 1;
}

static void visit_function(ObjImage *img, i32 func_idx, i32 *queue, i32 *qt) {
    if (func_idx < 0 || func_idx >= img->nfuncs) {
        werr(img, fallback_pos(img->module), "A wasm object relocation points at a missing function.");
        return;
    }
    if (img->live[func_idx]) return;

    WasmFunc *func = &img->module->funcs[func_idx];
    if (func->raw_body) {
        werr(img, func->location ? func->location->pos : fallback_pos(img->module),
            "Function '%s' was linked from a wasm object. Object output does not re-link #object files.",
            func->name ? func->name : "unnamed");
        return;
    }
    if (!func->code) {
        werr(img, func->location ? func->location->pos : fallback_pos(img->module),
            "Function '%s' has no body to write into a wasm object.",
            func->name ? func->name : "unnamed");
        return;
    }

    img->live[func_idx] = 1;
    queue[(*qt)++] = func_idx;
}

static void scan_function(ObjImage *img, i32 func_idx, i32 *queue, i32 *qt) {
    OnyxWasmModule *module = img->module;
    WasmFunc *func = &module->funcs[func_idx];
    OnyxFilePos pos = func->location ? func->location->pos : fallback_pos(module);
    const char *fname = func->name ? func->name : "unnamed";

    for (i32 i = 0; i < bh_arr_length(func->code); i++) {
        WasmInstruction *instr = &func->code[i];
        if (instr->type == WI_CALL_INDIRECT) {
            werr(img, pos, "Function '%s' uses an indirect call. Wasm object output does not write table relocations yet.", fname);
            return;
        }
        if (instr_uses_memory(instr->type)) img->need_memory = 1;

        if (instr->type == WI_GLOBAL_GET || instr->type == WI_GLOBAL_SET) {
            u64 g = (u64) instr->data.l;
            if (img->have_stack && g == img->stack_g) img->need_stack = 1;
            else if (img->have_tls && g == img->tls_g) img->need_tls = 1;
            else if (g >= (u64) img->nglobals) {
                werr(img, pos, "Function '%s' references a missing global.", fname);
                return;
            } else {
                img->global_needed[g] = 1;
            }
        }

        if (instr->type != WI_CALL) continue;

        CodePatchInfo *patch = find_code_patch(module, (u32) func_idx, (u32) i, Code_Patch_Callee);
        if (!patch || !patch->node_related_to_patch || patch->node_related_to_patch->kind != Ast_Kind_Function) {
            werr(img, pos, "Function '%s' has a call with no relocation.", fname);
            return;
        }
        AstFunction *callee = (AstFunction *) patch->node_related_to_patch;
        if (callee->is_object_link) {
            werr(img, fn_pos(callee),
                "Function '%s' calls '%s', which comes from a wasm object. Object output does not re-link #object files.",
                fname, get_function_name(module->context, callee));
            return;
        }
        if (callee->is_foreign_dyncall) {
            werr(img, fn_pos(callee),
                "Function '%s' calls dyncall import '%s'. Wasm object output emits C symbols.",
                fname, get_function_name(module->context, callee));
            return;
        }
        if (callee->is_foreign) {
            add_foreign(img, callee);
            continue;
        }
        if (!bh_imap_has(&module->index_map, (u64) callee)) {
            werr(img, fn_pos(callee), "Function '%s' was not generated.", get_function_name(module->context, callee));
            return;
        }
        visit_function(img, (i32) bh_imap_get(&module->index_map, (u64) callee), queue, qt);
    }

    bh_arr_each(DatumPatchInfo, patch, module->data_patches) {
        if (patch->kind != Datum_Patch_Instruction) continue;
        if (patch->index != (u32) func_idx) continue;
        resolve_data_id(patch);
        mark_data(img, patch->data_id, pos);
    }

    bh_arr_each(CodePatchInfo, patch, module->code_patches) {
        if (patch->func_idx != (u32) func_idx) continue;
        if (patch->kind == Code_Patch_Tls_Offset) img->need_tls_data = 1;
    }
}

static void collect_image(ObjImage *img) {
    OnyxWasmModule *module = img->module;
    bh_allocator alloc = module->context->gp_alloc;

    img->nfuncs = bh_arr_length(module->funcs);
    img->nglobals = bh_arr_length(module->globals);
    img->ndata = bh_arr_length(module->data);
    img->have_stack = bh_imap_has(&module->index_map, (u64) &module->context->builtins.stack_top);
    img->have_tls = bh_imap_has(&module->index_map, (u64) &module->context->builtins.tls_base);
    if (img->have_stack) img->stack_g = bh_imap_get(&module->index_map, (u64) &module->context->builtins.stack_top);
    if (img->have_tls) img->tls_g = bh_imap_get(&module->index_map, (u64) &module->context->builtins.tls_base);
    img->stack_sym = -1;
    img->tls_sym = -1;
    img->tls_data_sym = -1;

    if (img->nfuncs > 0) {
        img->live = calloc((size_t) img->nfuncs, 1);
        img->export_name = calloc((size_t) img->nfuncs, sizeof(char *));
        img->sym_of_func = malloc((size_t) img->nfuncs * sizeof(i32));
        for (i32 i = 0; i < img->nfuncs; i++) img->sym_of_func[i] = -1;
    }
    if (img->nglobals > 0) {
        img->global_needed = calloc((size_t) img->nglobals, 1);
        img->sym_of_global = malloc((size_t) img->nglobals * sizeof(i32));
        img->defined_globals = malloc((size_t) img->nglobals * sizeof(i32));
        for (i32 i = 0; i < img->nglobals; i++) img->sym_of_global[i] = -1;
    }
    if (img->ndata > 0) {
        img->live_data = calloc((size_t) img->ndata + 1, 1);
        img->sym_of_datum = malloc(((size_t) img->ndata + 1) * sizeof(i32));
        img->data_ids = malloc((size_t) img->ndata * sizeof(i32));
        for (i32 i = 0; i <= img->ndata; i++) img->sym_of_datum[i] = -1;
    }

    i32 *queue = img->nfuncs ? malloc((size_t) img->nfuncs * sizeof(i32)) : NULL;
    i32 qt = 0;

    b32 any_export = 0;
    bh_arr_each(CodePatchInfo, patch, module->code_patches) {
        if (patch->kind != Code_Patch_Export) continue;

        any_export = 1;
        AstNode *node = patch->node_related_to_patch;
        OnyxFilePos pos = patch->token_related_to_patch ? patch->token_related_to_patch->pos : fallback_pos(module);
        if (!node || node->kind != Ast_Kind_Function) {
            werr(img, pos, "Only functions can be exported from a wasm object.");
            continue;
        }
        AstFunction *fn = (AstFunction *) node;
        if (fn->is_foreign || fn->is_object_link) {
            werr(img, pos, "Export '%s' is not a function defined in this compilation.", copy_token(alloc, patch->token_related_to_patch));
            continue;
        }
        if (!function_type_is_c_abi(fn->type)) {
            werr(img, pos,
                "Export '%s' uses a slice, multiple return values, or a variadic parameter, so it cannot be written to a wasm object.",
                copy_token(alloc, patch->token_related_to_patch));
            continue;
        }
        if (!bh_imap_has(&module->index_map, (u64) fn)) {
            werr(img, pos, "Export '%s' was not generated.", copy_token(alloc, patch->token_related_to_patch));
            continue;
        }
        i32 idx = (i32) bh_imap_get(&module->index_map, (u64) fn);
        if (idx < 0 || idx >= img->nfuncs) {
            werr(img, pos, "Export '%s' was not generated.", copy_token(alloc, patch->token_related_to_patch));
            continue;
        }
        char *name = copy_token(alloc, patch->token_related_to_patch);
        if (img->export_name[idx] && strcmp(img->export_name[idx], name) != 0) {
            werr(img, pos, "Function '%s' is exported under two names.", get_function_name(module->context, fn));
            continue;
        }
        img->export_name[idx] = name;
        visit_function(img, idx, queue, &qt);
    }

    if (module->context->builtins.init_procedures) {
        bh_arr_each(AstFunction *, pinit, module->context->builtins.init_procedures) {
            AstFunction *fn = *pinit;
            if (!fn || fn->is_foreign || fn->is_object_link) continue;
            if (!bh_imap_has(&module->index_map, (u64) fn)) continue;
            visit_function(img, (i32) bh_imap_get(&module->index_map, (u64) fn), queue, &qt);
        }
    }

    if (!any_export && !img->failed) {
        werr(img, fallback_pos(module), "A wasm object needs a #export of a function so the linker has a symbol to keep.");
    }

    for (i32 qh = 0; qh < qt && !img->failed; qh++) {
        scan_function(img, queue[qh], queue, &qt);
    }
    free(queue);

    b32 grew = 1;
    while (grew && !img->failed && img->live_data) {
        grew = 0;
        bh_arr_each(DatumPatchInfo, patch, module->data_patches) {
            if (patch->kind != Datum_Patch_Data && patch->kind != Datum_Patch_Relative) continue;
            if (patch->index == 0 || (i32) patch->index > img->ndata) continue;
            if (!img->live_data[patch->index]) continue;
            resolve_data_id(patch);
            if (patch->data_id == 0 || (i32) patch->data_id > img->ndata) {
                werr(img, fallback_pos(module), "A data relocation points at a missing symbol.");
                break;
            }
            if (!img->live_data[patch->data_id]) {
                img->live_data[patch->data_id] = 1;
                grew = 1;
            }
        }
    }

    if (img->need_tls_data) img->need_tls = 1;
    if (img->need_stack || img->need_tls_data) img->need_memory = 1;
    if (img->live_data) {
        for (i32 i = 1; i <= img->ndata; i++) if (img->live_data[i]) img->need_memory = 1;
    }

    if (img->global_needed) {
        if (img->have_stack && img->stack_g < (u64) img->nglobals) img->global_needed[img->stack_g] = 0;
        if (img->have_tls && img->tls_g < (u64) img->nglobals) img->global_needed[img->tls_g] = 0;
        for (i32 i = 0; i < img->nglobals; i++) {
            if (!img->global_needed[i]) continue;
            WasmGlobal *g = &module->globals[i];
            if (g->type != WASM_TYPE_INT32 && g->type != WASM_TYPE_INT64
                && g->type != WASM_TYPE_FLOAT32 && g->type != WASM_TYPE_FLOAT64) {
                werr(img, fallback_pos(module), "A wasm object global has no initializer.");
                break;
            }
            img->defined_globals[img->ndefined_globals++] = i;
        }
    }
}

static void push_sym(ObjImage *img, OutSym sym) {
    bh_arr_push(img->syms, sym);
}

static void build_symbols(ObjImage *img) {
    OnyxWasmModule *module = img->module;
    bh_allocator alloc = module->context->gp_alloc;
    bh_arr_new(alloc, img->syms, 8);

    i32 pos = 0;
    for (i32 i = 0; i < img->nfuncs; i++) {
        if (!img->live || !img->live[i]) continue;
        WasmFunc *func = &module->funcs[i];
        OutSym sym = { 0 };
        sym.kind = SYM_FUNCTION;
        sym.flags = img->export_name[i] ? SYM_EXPORTED : SYM_BINDING_LOCAL;
        sym.index = (u32) (img->nforeign + pos);
        if (img->export_name[i]) {
            sym.name = img->export_name[i];
        } else {
            char buf[128];
            snprintf(buf, sizeof buf, ".Lfunc.%d", i);
            sym.name = copy_cstr(alloc, func->name && func->name[0] ? func->name : buf);
            if (!img->export_name[i]) {
                char unique[160];
                snprintf(unique, sizeof unique, "%s@%d", sym.name, i);
                sym.name = copy_cstr(alloc, unique);
            }
        }
        img->sym_of_func[i] = bh_arr_length(img->syms);
        push_sym(img, sym);
        pos++;
    }

    for (i32 i = 0; i < img->nforeign; i++) {
        OutSym sym = { 0 };
        sym.kind = SYM_FUNCTION;
        sym.flags = SYM_UNDEFINED;
        sym.index = (u32) i;
        sym.name = NULL;
        img->foreigns[i].sym = bh_arr_length(img->syms);
        push_sym(img, sym);
    }

    u32 imported_globals = 0;
    if (img->need_stack) {
        OutSym sym = { 0 };
        sym.kind = SYM_GLOBAL;
        sym.flags = SYM_UNDEFINED;
        sym.index = imported_globals++;
        img->stack_sym = bh_arr_length(img->syms);
        push_sym(img, sym);
    }
    if (img->need_tls) {
        OutSym sym = { 0 };
        sym.kind = SYM_GLOBAL;
        sym.flags = SYM_UNDEFINED;
        sym.index = imported_globals++;
        img->tls_sym = bh_arr_length(img->syms);
        push_sym(img, sym);
    }

    for (i32 i = 0; i < img->ndefined_globals; i++) {
        i32 g = img->defined_globals[i];
        OutSym sym = { 0 };
        sym.kind = SYM_GLOBAL;
        sym.flags = SYM_BINDING_LOCAL;
        sym.index = imported_globals++;
        char buf[64];
        snprintf(buf, sizeof buf, ".global.%d", g);
        sym.name = copy_cstr(alloc, buf);
        img->sym_of_global[g] = bh_arr_length(img->syms);
        push_sym(img, sym);
    }

    u32 seg = 0;
    if (img->live_data) {
        for (i32 id = 1; id <= img->ndata; id++) {
            if (!img->live_data[id]) continue;
            WasmDatum *datum = &module->data[id - 1];
            OutSym sym = { 0 };
            sym.kind = SYM_DATA;
            sym.flags = SYM_BINDING_LOCAL;
            sym.index = seg;
            sym.offset = 0;
            sym.size = datum->length;
            char buf[64];
            snprintf(buf, sizeof buf, ".data.%d", id);
            sym.name = copy_cstr(alloc, buf);
            img->sym_of_datum[id] = bh_arr_length(img->syms);
            img->data_ids[img->ndata_segs++] = id;
            push_sym(img, sym);
            seg++;
        }
    }

    if (img->need_tls_data) {
        img->tls_size = module->next_tls_offset ? module->next_tls_offset : 1;
        OutSym sym = { 0 };
        sym.kind = SYM_DATA;
        sym.flags = SYM_BINDING_LOCAL | SYM_TLS;
        sym.index = seg;
        sym.offset = 0;
        sym.size = img->tls_size;
        sym.name = copy_cstr(alloc, ".tdata");
        img->tls_data_sym = bh_arr_length(img->syms);
        push_sym(img, sym);
    }

    if (module->context->builtins.init_procedures && img->sym_of_func) {
        img->init_syms = malloc((size_t) bh_arr_length(module->context->builtins.init_procedures) * sizeof(i32));
        bh_arr_each(AstFunction *, pinit, module->context->builtins.init_procedures) {
            AstFunction *fn = *pinit;
            if (!fn || !bh_imap_has(&module->index_map, (u64) fn)) continue;
            if (fn->is_foreign) continue;
            i32 idx = (i32) bh_imap_get(&module->index_map, (u64) fn);
            if (idx < 0 || idx >= img->nfuncs) continue;
            if (img->sym_of_func[idx] < 0) continue;
            img->init_syms[img->ninit++] = img->sym_of_func[idx];
        }
    }
}

static u64 obj_local_idx(LocalAllocator *la, u64 value) {
    u32 idx = (u32) value;
    if (value & 0x100000000ULL) idx += la->allocated[0];
    if (value & 0x200000000ULL) idx += la->allocated[1];
    if (value & 0x400000000ULL) idx += la->allocated[2];
    if (value & 0x800000000ULL) idx += la->allocated[3];
    return idx;
}

static void write_locals(WasmFunc *func, bh_buffer *body) {
    i32 groups =
        (func->locals.allocated[0] != 0) +
        (func->locals.allocated[1] != 0) +
        (func->locals.allocated[2] != 0) +
        (func->locals.allocated[3] != 0) +
        (func->locals.allocated[4] != 0);
    buf_uleb(body, (u64) groups);
    if (func->locals.allocated[0]) { buf_uleb(body, func->locals.allocated[0]); bh_buffer_write_byte(body, WASM_TYPE_INT32); }
    if (func->locals.allocated[1]) { buf_uleb(body, func->locals.allocated[1]); bh_buffer_write_byte(body, WASM_TYPE_INT64); }
    if (func->locals.allocated[2]) { buf_uleb(body, func->locals.allocated[2]); bh_buffer_write_byte(body, WASM_TYPE_FLOAT32); }
    if (func->locals.allocated[3]) { buf_uleb(body, func->locals.allocated[3]); bh_buffer_write_byte(body, WASM_TYPE_FLOAT64); }
    if (func->locals.allocated[4]) { buf_uleb(body, func->locals.allocated[4]); bh_buffer_write_byte(body, WASM_TYPE_VAR128); }
}

static void write_instr_immediates(WasmFunc *func, WasmInstruction *instr, bh_buffer *body) {
    switch (instr->type) {
        case WI_LOCAL_GET:
        case WI_LOCAL_SET:
        case WI_LOCAL_TEE:
            buf_uleb(body, obj_local_idx(&func->locals, (u64) instr->data.l));
            break;

        case WI_GLOBAL_GET:
        case WI_GLOBAL_SET:
        case WI_CALL:
        case WI_BLOCK_START:
        case WI_LOOP_START:
        case WI_JUMP:
        case WI_COND_JUMP:
        case WI_IF_START:
        case WI_MEMORY_SIZE:
        case WI_MEMORY_GROW:
        case WI_MEMORY_FILL:
            buf_uleb(body, (u64) instr->data.i1);
            break;

        case WI_MEMORY_INIT:
        case WI_MEMORY_COPY: {
            buf_uleb(body, (u64) instr->data.i1);
            buf_uleb(body, (u64) instr->data.i2);
            break;
        }

        case WI_JUMP_TABLE: {
            BranchTable *bt = (BranchTable *) instr->data.p;
            buf_uleb(body, bt->count);
            for (u32 i = 0; i < bt->count; i++) buf_uleb(body, bt->cases[i]);
            buf_uleb(body, bt->default_case);
            break;
        }

        case WI_CALL_INDIRECT:
        case WI_I32_STORE: case WI_I32_STORE_8: case WI_I32_STORE_16:
        case WI_I64_STORE: case WI_I64_STORE_8: case WI_I64_STORE_16: case WI_I64_STORE_32:
        case WI_F32_STORE: case WI_F64_STORE:
        case WI_V128_STORE:
        case WI_I32_LOAD:
        case WI_I32_LOAD_8_S: case WI_I32_LOAD_8_U:
        case WI_I32_LOAD_16_S: case WI_I32_LOAD_16_U:
        case WI_I64_LOAD:
        case WI_I64_LOAD_8_S: case WI_I64_LOAD_8_U:
        case WI_I64_LOAD_16_S: case WI_I64_LOAD_16_U:
        case WI_I64_LOAD_32_S: case WI_I64_LOAD_32_U:
        case WI_F32_LOAD: case WI_F64_LOAD:
        case WI_V128_LOAD:
            buf_uleb(body, (u64) instr->data.i1);
            buf_uleb(body, (u64) instr->data.i2);
            break;

        case WI_I32_CONST:
            buf_sleb(body, (i64) instr->data.i1);
            break;
        case WI_I64_CONST:
            buf_sleb(body, instr->data.l);
            break;
        case WI_F32_CONST: {
            float f = instr->data.f;
            bh_buffer_append(body, &f, 4);
            break;
        }
        case WI_F64_CONST: {
            double d = instr->data.d;
            bh_buffer_append(body, &d, 8);
            break;
        }
        case WI_V128_CONST:
        case WI_I8X16_SHUFFLE:
            bh_buffer_append(body, instr->data.p, 16);
            break;

        case WI_I8X16_EXTRACT_LANE_S: case WI_I8X16_EXTRACT_LANE_U: case WI_I8X16_REPLACE_LANE:
        case WI_I16X8_EXTRACT_LANE_S: case WI_I16X8_EXTRACT_LANE_U: case WI_I16X8_REPLACE_LANE:
        case WI_I32X4_EXTRACT_LANE: case WI_I32X4_REPLACE_LANE:
        case WI_I64X2_EXTRACT_LANE: case WI_I64X2_REPLACE_LANE:
        case WI_F32X4_EXTRACT_LANE: case WI_F32X4_REPLACE_LANE:
        case WI_F64X2_EXTRACT_LANE: case WI_F64X2_REPLACE_LANE:
            bh_buffer_write_byte(body, (u8) instr->data.i1);
            break;

        default:
            break;
    }
}

static void record_reloc(ObjImage *img, bh_arr(OutRel) *rels, u32 type, u32 offset, u32 sym, i32 addend) {
    (void) img;
    OutRel rel = { 0 };
    rel.type = type;
    rel.offset = offset;
    rel.index = sym;
    rel.addend = addend;

    // bh_arr_push(*rels, ...) binds the subscript tighter than the
    // dereference, so the push has to be written out by hand.
    i32 n = bh_arr_length(*rels);
    if (n + 1 > bh_arr_capacity(*rels)) {
        bh__arr_grow(bh_arr_allocator(*rels), (void **) rels, sizeof(OutRel), n + 8);
    }
    (*rels)[n] = rel;
    bh__arrhead(*rels)->length = n + 1;
}

static i32 symbol_for_global(ObjImage *img, u64 g) {
    if (img->have_stack && g == img->stack_g) return img->stack_sym;
    if (img->have_tls && g == img->tls_g) return img->tls_sym;
    if (g < (u64) img->nglobals) return img->sym_of_global[g];
    return -1;
}

static void write_instr(ObjImage *img, WasmFunc *func, i32 func_idx, WasmInstruction *instr, i32 instr_index, bh_buffer *body, bh_arr(OutRel) *body_relocs) {
    if (instr->type == WI_NOP) return;

    if (instr->type & SIMD_INSTR_MASK) {
        bh_buffer_write_byte(body, 0xFD);
        buf_uleb(body, (u64) (instr->type & ~SIMD_INSTR_MASK));
    } else if (instr->type & EXT_INSTR_MASK) {
        bh_buffer_write_byte(body, 0xFC);
        buf_uleb(body, (u64) (instr->type & ~EXT_INSTR_MASK));
    } else if (instr->type & ATOMIC_INSTR_MASK) {
        bh_buffer_write_byte(body, 0xFE);
        buf_uleb(body, (u64) (instr->type & ~ATOMIC_INSTR_MASK));
        if (instr->type == WI_ATOMIC_FENCE) {
            bh_buffer_write_byte(body, 0x00);
        } else {
            buf_uleb(body, (u64) instr->data.i1);
            buf_uleb(body, (u64) instr->data.i2);
        }
        return;
    } else {
        bh_buffer_write_byte(body, (u8) instr->type);
    }

    if (instr->type == WI_CALL) {
        CodePatchInfo *patch = find_code_patch(img->module, (u32) func_idx, (u32) instr_index, Code_Patch_Callee);
        AstFunction *callee = (AstFunction *) patch->node_related_to_patch;
        i32 sym = callee->is_foreign
            ? foreign_sym_for(img, callee)
            : img->sym_of_func[(i32) bh_imap_get(&img->module->index_map, (u64) callee)];
        u32 at = (u32) body->length;
        buf_uleb5(body, 0);
        record_reloc(img, body_relocs, RELOC_FUNCTION_INDEX_LEB, at, (u32) sym, 0);
        return;
    }

    if (instr->type == WI_GLOBAL_GET || instr->type == WI_GLOBAL_SET) {
        i32 sym = symbol_for_global(img, (u64) instr->data.l);
        u32 at = (u32) body->length;
        buf_uleb5(body, 0);
        record_reloc(img, body_relocs, RELOC_GLOBAL_INDEX_LEB, at, (u32) sym, 0);
        return;
    }

    if (instr->type == WI_I32_CONST) {
        CodePatchInfo *tls = find_code_patch(img->module, (u32) func_idx, (u32) instr_index, Code_Patch_Tls_Offset);
        if (tls) {
            AstMemRes *memres = (AstMemRes *) tls->node_related_to_patch;
            u32 at = (u32) body->length;
            buf_sleb5(body, 0);
            record_reloc(img, body_relocs, RELOC_MEMORY_ADDR_TLS_SLEB, at, (u32) img->tls_data_sym, (i32) memres->tls_offset);
            return;
        }
        DatumPatchInfo *data = find_instr_datum_patch(img->module, (u32) func_idx, (u32) instr_index);
        if (data) {
            u32 at = (u32) body->length;
            buf_sleb5(body, 0);
            record_reloc(img, body_relocs, RELOC_MEMORY_ADDR_SLEB, at, (u32) img->sym_of_datum[data->data_id], (i32) data->offset);
            return;
        }
    }

    write_instr_immediates(func, instr, body);
}

static void write_global_init(bh_buffer *b, WasmGlobal *global) {
    WasmInstruction instr = { 0 };
    if (global->initial_value && bh_arr_length(global->initial_value) > 0)
        instr = global->initial_value[0];
    else if (global->type == WASM_TYPE_INT64) instr.type = WI_I64_CONST;
    else if (global->type == WASM_TYPE_FLOAT32) instr.type = WI_F32_CONST;
    else if (global->type == WASM_TYPE_FLOAT64) instr.type = WI_F64_CONST;
    else instr.type = WI_I32_CONST;

    bh_buffer_write_byte(b, (u8) instr.type);
    if (instr.type == WI_I32_CONST) buf_sleb(b, instr.data.i1);
    else if (instr.type == WI_I64_CONST) buf_sleb(b, instr.data.l);
    else if (instr.type == WI_F32_CONST) {
        float f = instr.data.f;
        bh_buffer_append(b, &f, 4);
    } else if (instr.type == WI_F64_CONST) {
        double d = instr.data.d;
        bh_buffer_append(b, &d, 8);
    } else {
        buf_sleb(b, 0);
    }
    bh_buffer_write_byte(b, 0x0b);
}

static void encode_object(ObjImage *img, bh_buffer *out) {
    OnyxWasmModule *module = img->module;
    bh_allocator alloc = module->context->gp_alloc;

    bh_buffer types, imports, funcs, globals, code, data, linking, features;
    bh_buffer_init(&types, alloc, 64);
    bh_buffer_init(&imports, alloc, 64);
    bh_buffer_init(&funcs, alloc, 32);
    bh_buffer_init(&globals, alloc, 32);
    bh_buffer_init(&code, alloc, 128);
    bh_buffer_init(&data, alloc, 64);
    bh_buffer_init(&linking, alloc, 64);
    bh_buffer_init(&features, alloc, 64);
    bh_arr_new(alloc, img->code_relocs, 8);
    bh_arr_new(alloc, img->data_relocs, 4);

    i32 ntypes = bh_arr_length(module->types);
    buf_uleb(&types, (u64) ntypes);
    for (i32 i = 0; i < ntypes; i++) {
        WasmFuncType *type = module->types[i];
        bh_buffer_write_byte(&types, 0x60);
        if (!type) {
            bh_buffer_write_byte(&types, 0x00);
            bh_buffer_write_byte(&types, 0x00);
            continue;
        }
        buf_uleb(&types, (u64) type->param_count);
        if (type->param_count)
            bh_buffer_append(&types, type->param_types, type->param_count);
        if (type->return_type != WASM_TYPE_VOID) {
            bh_buffer_write_byte(&types, 0x01);
            bh_buffer_write_byte(&types, type->return_type);
        } else {
            bh_buffer_write_byte(&types, 0x00);
        }
    }

    b32 has_import = img->need_memory || img->nforeign || img->need_stack || img->need_tls;
    if (has_import) {
        i32 count = (img->need_memory ? 1 : 0) + img->nforeign + (img->need_stack ? 1 : 0) + (img->need_tls ? 1 : 0);
        buf_uleb(&imports, (u64) count);
        if (img->need_memory) {
            write_wasm_name(&imports, "env");
            write_wasm_name(&imports, "__linear_memory");
            bh_buffer_write_byte(&imports, 0x02);
            bh_buffer_write_byte(&imports, 0x00);
            buf_uleb(&imports, 0);
        }
        for (i32 i = 0; i < img->nforeign; i++) {
            write_wasm_name(&imports, "env");
            write_wasm_name(&imports, img->foreigns[i].name);
            bh_buffer_write_byte(&imports, 0x00);
            buf_uleb(&imports, (u64) img->foreigns[i].type_idx);
        }
        if (img->need_stack) {
            write_wasm_name(&imports, "env");
            write_wasm_name(&imports, "__stack_pointer");
            bh_buffer_write_byte(&imports, 0x03);
            bh_buffer_write_byte(&imports, WASM_TYPE_INT32);
            bh_buffer_write_byte(&imports, 0x01);
        }
        if (img->need_tls) {
            write_wasm_name(&imports, "env");
            write_wasm_name(&imports, "__tls_base");
            bh_buffer_write_byte(&imports, 0x03);
            bh_buffer_write_byte(&imports, WASM_TYPE_INT32);
            bh_buffer_write_byte(&imports, 0x01);
        }
    }

    i32 ndefined = 0;
    for (i32 i = 0; i < img->nfuncs; i++) if (img->live && img->live[i]) ndefined++;
    buf_uleb(&funcs, (u64) ndefined);
    for (i32 i = 0; i < img->nfuncs; i++) {
        if (!img->live || !img->live[i]) continue;
        buf_uleb(&funcs, (u64) module->funcs[i].type_idx);
    }

    if (img->ndefined_globals) {
        buf_uleb(&globals, (u64) img->ndefined_globals);
        for (i32 i = 0; i < img->ndefined_globals; i++) {
            WasmGlobal *g = &module->globals[img->defined_globals[i]];
            bh_buffer_write_byte(&globals, g->type);
            bh_buffer_write_byte(&globals, g->mutable ? 0x01 : 0x00);
            write_global_init(&globals, g);
        }
    }

    buf_uleb(&code, (u64) ndefined);
    for (i32 fi = 0; fi < img->nfuncs; fi++) {
        if (!img->live || !img->live[fi]) continue;
        WasmFunc *func = &module->funcs[fi];
        bh_buffer body;
        bh_buffer_init(&body, alloc, 64);
        bh_arr(OutRel) body_relocs = NULL;
        bh_arr_new(alloc, body_relocs, 4);

        write_locals(func, &body);
        for (i32 ii = 0; ii < bh_arr_length(func->code); ii++) {
            write_instr(img, func, fi, &func->code[ii], ii, &body, &body_relocs);
        }

        i32 size_len = 0;
        u8 *size = uint_to_uleb128((u64) body.length, &size_len);
        u32 base = (u32) code.length + (u32) size_len;
        bh_arr_each(OutRel, rel, body_relocs) {
            rel->offset += base;
            bh_arr_push(img->code_relocs, *rel);
        }
        bh_buffer_append(&code, size, size_len);
        if (body.length) bh_buffer_append(&code, body.data, body.length);
        bh_arr_free(body_relocs);
        bh_buffer_free(&body);
    }

    i32 nsegments = img->ndata_segs + (img->need_tls_data ? 1 : 0);
    if (nsegments) {
        buf_uleb(&data, (u64) nsegments);
        for (i32 s = 0; s < img->ndata_segs; s++) {
            i32 id = img->data_ids[s];
            WasmDatum *datum = &module->data[id - 1];
            bh_buffer_write_byte(&data, 0x00);
            bh_buffer_write_byte(&data, 0x41);
            bh_buffer_write_byte(&data, 0x00);
            bh_buffer_write_byte(&data, 0x0b);
            buf_uleb(&data, datum->length);

            u8 *bytes = NULL;
            if (datum->length) {
                bytes = calloc(1, datum->length);
                if (datum->data) memcpy(bytes, datum->data, datum->length);
            }
            u32 bytes_at = (u32) data.length;
            bh_arr_each(DatumPatchInfo, patch, module->data_patches) {
                if (patch->kind != Datum_Patch_Data && patch->kind != Datum_Patch_Relative) continue;
                if (patch->index != (u32) id) continue;
                if (patch->location + 4 > datum->length) continue;
                i32 existing = 0;
                if (bytes) memcpy(&existing, bytes + patch->location, 4);
                if (patch->kind == Datum_Patch_Relative && existing == 0 && patch->offset == 0) continue;
                i32 addend = patch->kind == Datum_Patch_Relative ? existing + (i32) patch->offset : (i32) patch->offset;
                if (bytes) memset(bytes + patch->location, 0, 4);
                record_reloc(img, &img->data_relocs, RELOC_MEMORY_ADDR_I32, bytes_at + patch->location, (u32) img->sym_of_datum[patch->data_id], addend);
            }
            if (datum->length) bh_buffer_append(&data, bytes, (i32) datum->length);
            free(bytes);
        }
        if (img->need_tls_data) {
            bh_buffer_write_byte(&data, 0x00);
            bh_buffer_write_byte(&data, 0x41);
            bh_buffer_write_byte(&data, 0x00);
            bh_buffer_write_byte(&data, 0x0b);
            buf_uleb(&data, img->tls_size);
            if (img->tls_size) {
                u8 *zeros = calloc(1, img->tls_size);
                bh_buffer_append(&data, zeros, (i32) img->tls_size);
                free(zeros);
            }
        }
    }

    buf_uleb(&linking, 2);

    if (nsegments) {
        bh_buffer sub;
        bh_buffer_init(&sub, alloc, 32);
        buf_uleb(&sub, (u64) nsegments);
        for (i32 s = 0; s < img->ndata_segs; s++) {
            WasmDatum *datum = &module->data[img->data_ids[s] - 1];
            write_wasm_name(&sub, ".data");
            buf_uleb(&sub, align_log2(datum->alignment ? datum->alignment : 1));
            buf_uleb(&sub, 0);
        }
        if (img->need_tls_data) {
            write_wasm_name(&sub, ".tdata");
            buf_uleb(&sub, 4);
            buf_uleb(&sub, 2);
        }
        bh_buffer_write_byte(&linking, 5);
        buf_uleb(&linking, (u64) sub.length);
        bh_buffer_append(&linking, sub.data, sub.length);
        bh_buffer_free(&sub);
    }

    {
        bh_buffer sub;
        bh_buffer_init(&sub, alloc, 64);
        buf_uleb(&sub, (u64) bh_arr_length(img->syms));
        bh_arr_each(OutSym, sym, img->syms) {
            bh_buffer_write_byte(&sub, sym->kind);
            buf_uleb(&sub, sym->flags);
            if (sym->kind == SYM_FUNCTION || sym->kind == SYM_GLOBAL) {
                buf_uleb(&sub, sym->index);
                if ((sym->flags & SYM_UNDEFINED) == 0) write_wasm_name(&sub, sym->name ? sym->name : "");
            } else if (sym->kind == SYM_DATA) {
                write_wasm_name(&sub, sym->name ? sym->name : "");
                if ((sym->flags & SYM_UNDEFINED) == 0) {
                    buf_uleb(&sub, sym->index);
                    buf_uleb(&sub, sym->offset);
                    buf_uleb(&sub, sym->size);
                }
            }
        }
        bh_buffer_write_byte(&linking, 8);
        buf_uleb(&linking, (u64) sub.length);
        bh_buffer_append(&linking, sub.data, sub.length);
        bh_buffer_free(&sub);
    }

    if (img->ninit) {
        bh_buffer sub;
        bh_buffer_init(&sub, alloc, 16);
        buf_uleb(&sub, (u64) img->ninit);
        for (i32 i = 0; i < img->ninit; i++) {
            buf_uleb(&sub, 0);
            buf_uleb(&sub, (u64) img->init_syms[i]);
        }
        bh_buffer_write_byte(&linking, 6);
        buf_uleb(&linking, (u64) sub.length);
        bh_buffer_append(&linking, sub.data, sub.length);
        bh_buffer_free(&sub);
    }

    static const char *feature_names[] = {
        "mutable-globals",
        "sign-ext",
        "nontrapping-fptoint",
        "bulk-memory",
        "multivalue",
        "reference-types",
    };
    buf_uleb(&features, sizeof feature_names / sizeof feature_names[0]);
    for (u32 i = 0; i < sizeof feature_names / sizeof feature_names[0]; i++) {
        bh_buffer_write_byte(&features, '+');
        write_wasm_name(&features, feature_names[i]);
    }

    u32 next = 0;
    next++;
    if (has_import) next++;
    next++;
    if (img->ndefined_globals) next++;
    u32 code_sec = next++;
    u32 data_sec = 0;
    if (nsegments) data_sec = next++;

    static const u8 magic[] = { 0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00 };
    bh_buffer_append(out, magic, 8);
    append_section(out, 1, &types);
    if (has_import) append_section(out, 2, &imports);
    append_section(out, 3, &funcs);
    if (img->ndefined_globals) append_section(out, 6, &globals);
    append_section(out, 10, &code);
    if (nsegments) append_section(out, 11, &data);
    append_custom(out, "linking", &linking);

    if (bh_arr_length(img->code_relocs)) {
        bh_buffer rel;
        bh_buffer_init(&rel, alloc, 32);
        buf_uleb(&rel, code_sec);
        buf_uleb(&rel, (u64) bh_arr_length(img->code_relocs));
        bh_arr_each(OutRel, r, img->code_relocs) {
            buf_uleb(&rel, r->type);
            buf_uleb(&rel, r->offset);
            buf_uleb(&rel, r->index);
            if (r->type == RELOC_MEMORY_ADDR_SLEB || r->type == RELOC_MEMORY_ADDR_I32 || r->type == RELOC_MEMORY_ADDR_TLS_SLEB
                || r->type == 3)
                buf_sleb(&rel, r->addend);
        }
        append_custom(out, "reloc.CODE", &rel);
        bh_buffer_free(&rel);
    }
    if (bh_arr_length(img->data_relocs)) {
        bh_buffer rel;
        bh_buffer_init(&rel, alloc, 32);
        buf_uleb(&rel, data_sec);
        buf_uleb(&rel, (u64) bh_arr_length(img->data_relocs));
        bh_arr_each(OutRel, r, img->data_relocs) {
            buf_uleb(&rel, r->type);
            buf_uleb(&rel, r->offset);
            buf_uleb(&rel, r->index);
            buf_sleb(&rel, r->addend);
        }
        append_custom(out, "reloc.DATA", &rel);
        bh_buffer_free(&rel);
    }
    append_custom(out, "target_features", &features);

    bh_buffer_free(&types);
    bh_buffer_free(&imports);
    bh_buffer_free(&funcs);
    bh_buffer_free(&globals);
    bh_buffer_free(&code);
    bh_buffer_free(&data);
    bh_buffer_free(&linking);
    bh_buffer_free(&features);
}

static void free_image(ObjImage *img) {
    free(img->live);
    free(img->export_name);
    free(img->sym_of_func);
    free(img->foreigns);
    free(img->global_needed);
    free(img->sym_of_global);
    free(img->defined_globals);
    free(img->live_data);
    free(img->data_ids);
    free(img->sym_of_datum);
    free(img->init_syms);
    bh_arr_free(img->syms);
    bh_arr_free(img->code_relocs);
    bh_arr_free(img->data_relocs);
}

static b32 emit_object(OnyxWasmModule *module, bh_buffer *out) {
    ObjImage img;
    memset(&img, 0, sizeof img);
    img.module = module;
    img.stack_sym = -1;
    img.tls_sym = -1;
    img.tls_data_sym = -1;

    apply_object_fixups(module);
    collect_image(&img);
    if (!img.failed) build_symbols(&img);
    if (!img.failed && out) encode_object(&img, out);

    b32 ok = !img.failed;
    free_image(&img);
    return ok;
}

b32 onyx_wasm_prepare_object(OnyxWasmModule *module) {
    if (!module) return 0;
    return emit_object(module, NULL);
}

void onyx_wasm_module_write_object(OnyxWasmModule *module, bh_buffer *buffer) {
    bh_allocator alloc = module->context->gp_alloc;
    bh_buffer_init(buffer, alloc, 128);
    bh_buffer tmp;
    bh_buffer_init(&tmp, alloc, 256);
    if (emit_object(module, &tmp)) {
        if (tmp.length) bh_buffer_append(buffer, tmp.data, tmp.length);
    }
    bh_buffer_free(&tmp);
}
