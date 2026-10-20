// Links Clang/LLVM WASM object files (tool-conventions linking section v2)
// into an Onyx module. Object code stays as raw function bodies; relocations
// are patched in place. __stack_pointer is Onyx's existing stack global.
// Thread-local symbols are appended to Onyx's TLS block. Their relocations
// store offsets from __tls_base, and a static image restores initializers
// on every thread.

#include "wasm_emit.h"
#include "errors.h"

#define SYM_FUNCTION 0
#define SYM_DATA     1
#define SYM_GLOBAL   2
#define SYM_SECTION  3
#define SYM_TAG      4
#define SYM_TABLE    5

#define SYM_BINDING_MASK  0x3
#define SYM_BINDING_WEAK  0x1
#define SYM_BINDING_LOCAL 0x2
#define SYM_UNDEFINED     0x10
#define SYM_EXPLICIT_NAME 0x40
#define SYM_TLS           0x100
#define SYM_ABSOLUTE      0x200

#define SEG_TLS 2

#define RELOC_FUNCTION_INDEX_LEB 0
#define RELOC_TABLE_INDEX_SLEB   1
#define RELOC_TABLE_INDEX_I32    2
#define RELOC_MEMORY_ADDR_LEB    3
#define RELOC_MEMORY_ADDR_SLEB   4
#define RELOC_MEMORY_ADDR_I32    5
#define RELOC_TYPE_INDEX_LEB     6
#define RELOC_GLOBAL_INDEX_LEB   7
#define RELOC_TABLE_NUMBER_LEB   20
#define RELOC_MEMORY_ADDR_TLS_SLEB 21

#define SEC_CUSTOM   0
#define SEC_TYPE     1
#define SEC_IMPORT   2
#define SEC_FUNCTION 3
#define SEC_GLOBAL   6
#define SEC_CODE     10
#define SEC_DATA     11

typedef struct ByteCursor {
    u8 *p;
    u8 *end;
} ByteCursor;

typedef struct ObjType {
    u8 *params;
    u32 nparams;
    u8 *results;
    u32 nresults;
    i32 onyx_idx;
} ObjType;

typedef struct ObjImport {
    char *module;
    char *field;
    u32 type_idx;
    u8 valtype;
    u8 mut;
} ObjImport;

typedef struct ObjFunc {
    u32 type_idx;
    u8 *body;
    u32 body_len;
    u32 body_off;
    u8 *copy;
    i32 out_func;
    b32 included;
} ObjFunc;

typedef struct ObjGlobal {
    u8 valtype;
    u8 mut;
    u8 *init;
    u32 init_len;
    i32 out_global;
} ObjGlobal;

typedef struct ObjSegment {
    u8 *data;
    u32 len;
    u32 payload_off;
    u32 align;
    u32 flags;
    u32 tls_offset;
    b32 live;
    b32 tls_placed;
    u8 *copy;
    u32 data_id;
} ObjSegment;

typedef struct ObjSymbol ObjSymbol;
typedef struct ObjFile ObjFile;

struct ObjSymbol {
    ObjFile *file;
    u8 kind;
    u32 flags;
    u32 index;
    u32 offset;
    u32 size;
    u32 align;
    char *name;
    b32 live;
    b32 reported;
    ObjSymbol *resolved;
    i32 out_func;
    i32 out_global;
    i32 out_table;
    u32 data_id;
    u32 tls_offset;
};

typedef struct ObjReloc {
    u32 type;
    u32 offset;
    u32 index;
    i32 addend;
    u32 section_id;
} ObjReloc;

typedef struct ObjInit {
    u32 priority;
    u32 symbol_index;
} ObjInit;

typedef struct ObjSection {
    u8 id;
    u8 *payload;
    u32 len;
} ObjSection;

typedef struct ObjProvide {
    char *name;
    AstFunction *func;
    AstMemRes *memres;
} ObjProvide;

struct ObjFile {
    char *path;
    // Set when this file was a member of an ar archive. path is then
    // "archive.a(member.o)" and archive is the path named by #object.
    char *archive;
    OnyxFilePos pos;
    u8 *bytes;
    u32 length;

    bh_arr(ObjType) types;
    bh_arr(ObjImport) func_imports;
    bh_arr(ObjImport) global_imports;
    bh_arr(ObjImport) table_imports;
    bh_arr(ObjFunc) funcs;
    bh_arr(ObjGlobal) globals;
    bh_arr(ObjSegment) segments;
    bh_arr(ObjSymbol) symbols;
    bh_arr(ObjReloc) relocs;
    bh_arr(ObjInit) inits;
    bh_arr(ObjSection) sections;
    bh_arr(ObjProvide) provides;

    b32 failed;
};

typedef struct Linker {
    OnyxWasmModule *module;
    bh_arr(ObjFile *) files;
    bh_arr(ObjSymbol *) work;
    b32 failed;
} Linker;

static b32 read_uleb(ByteCursor *c, u32 *out) {
    u64 result = 0;
    u32 shift = 0;
    for (int i = 0; i < 10; i++) {
        if (c->p >= c->end) return 0;
        u8 b = *c->p++;
        result |= (u64) (b & 0x7f) << shift;
        if ((b & 0x80) == 0) {
            if (result > 0xffffffffull) return 0;
            *out = (u32) result;
            return 1;
        }
        shift += 7;
        if (shift >= 64) return 0;
    }
    return 0;
}

static b32 read_sleb(ByteCursor *c, i32 *out) {
    i32 result = 0;
    u32 shift = 0;
    u8 b = 0;
    for (int i = 0; i < 5; i++) {
        if (c->p >= c->end) return 0;
        b = *c->p++;
        result |= (i32) (b & 0x7f) << shift;
        shift += 7;
        if ((b & 0x80) == 0) break;
        if (i == 4) return 0;
    }
    if (shift < 32 && (b & 0x40)) result |= (i32) (~0u << shift);
    *out = result;
    return 1;
}

static b32 read_bytes(ByteCursor *c, u32 n, u8 **out) {
    if (c->p + n > c->end) return 0;
    *out = c->p;
    c->p += n;
    return 1;
}

static char *read_name(ByteCursor *c, bh_allocator alloc) {
    u32 len = 0;
    if (!read_uleb(c, &len)) return NULL;
    u8 *bytes = NULL;
    if (!read_bytes(c, len, &bytes)) return NULL;
    return bh_strdup_len(alloc, (char *) bytes, (i32) len);
}

static b32 skip_init_expr(ByteCursor *c) {
    while (c->p < c->end) {
        u8 op = *c->p++;
        if (op == 0x0b) return 1;
        if (op == 0x41 || op == 0x42) {
            i32 ignored;
            if (!read_sleb(c, &ignored)) return 0;
            continue;
        }
        if (op == 0x23) {
            u32 ignored;
            if (!read_uleb(c, &ignored)) return 0;
            continue;
        }
        if (op == 0x43) {
            if (c->p + 4 > c->end) return 0;
            c->p += 4;
            continue;
        }
        if (op == 0x44) {
            if (c->p + 8 > c->end) return 0;
            c->p += 8;
            continue;
        }
        return 0;
    }
    return 0;
}

static b32 skip_limits(ByteCursor *c) {
    if (c->p >= c->end) return 0;
    u8 flags = *c->p++;
    u32 minv;
    if (!read_uleb(c, &minv)) return 0;
    if (flags & 1) {
        u32 maxv;
        if (!read_uleb(c, &maxv)) return 0;
    }
    return 1;
}

static void obj_error(Linker *L, ObjFile *file, const char *fmt, ...) {
    L->failed = 1;
    if (file) file->failed = 1;

    OnyxFilePos pos = { 0 };
    if (file) pos = file->pos;
    if (!onyx_errors_are_enabled(L->module->context)) return;

    va_list args;
    va_start(args, fmt);
    char *msg = bh_aprintf_va(L->module->context->errors.msg_alloc, fmt, args);
    va_end(args);

    OnyxError err = { .pos = pos, .rank = Error_Critical, .text = msg };
    onyx_submit_error(L->module->context, err);
}

static u32 reloc_width(u32 type) {
    if (type == RELOC_TABLE_INDEX_I32 || type == RELOC_MEMORY_ADDR_I32) return 4;
    return 5;
}

static b32 reloc_has_addend(u32 type) {
    return type == RELOC_MEMORY_ADDR_LEB
        || type == RELOC_MEMORY_ADDR_SLEB
        || type == RELOC_MEMORY_ADDR_I32
        || type == RELOC_MEMORY_ADDR_TLS_SLEB;
}

static void write_uleb5(u8 *p, u32 value) {
    for (int i = 0; i < 4; i++) {
        p[i] = (u8) ((value & 0x7f) | 0x80);
        value >>= 7;
    }
    p[4] = (u8) (value & 0x7f);
}

static void write_sleb5(u8 *p, i32 value) {
    for (int i = 0; i < 4; i++) {
        p[i] = (u8) ((value & 0x7f) | 0x80);
        value >>= 7;
    }
    p[4] = (u8) (value & 0x7f);
}

static void write_i32(u8 *p, i32 value) {
    u32 u = (u32) value;
    p[0] = (u8) (u);
    p[1] = (u8) (u >> 8);
    p[2] = (u8) (u >> 16);
    p[3] = (u8) (u >> 24);
}

static b32 parse_types(ObjFile *file, u8 *payload, u32 len) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        if (c.p >= c.end || *c.p++ != 0x60) return 0;
        ObjType type = { 0 };
        type.onyx_idx = -1;
        if (!read_uleb(&c, &type.nparams)) return 0;
        if (!read_bytes(&c, type.nparams, &type.params)) return 0;
        if (!read_uleb(&c, &type.nresults)) return 0;
        if (!read_bytes(&c, type.nresults, &type.results)) return 0;
        bh_arr_push(file->types, type);
    }
    return 1;
}

static b32 parse_imports(ObjFile *file, u8 *payload, u32 len, bh_allocator alloc) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        char *module = read_name(&c, alloc);
        char *field = read_name(&c, alloc);
        if (!module || !field || c.p >= c.end) return 0;
        u8 kind = *c.p++;
        ObjImport imp = { 0 };
        imp.module = module;
        imp.field = field;
        if (kind == 0) {
            if (!read_uleb(&c, &imp.type_idx)) return 0;
            bh_arr_push(file->func_imports, imp);
        } else if (kind == 1) {
            if (c.p >= c.end) return 0;
            c.p++;
            if (!skip_limits(&c)) return 0;
            bh_arr_push(file->table_imports, imp);
        } else if (kind == 2) {
            if (!skip_limits(&c)) return 0;
        } else if (kind == 3) {
            if (c.p + 2 > c.end) return 0;
            imp.valtype = *c.p++;
            imp.mut = *c.p++;
            bh_arr_push(file->global_imports, imp);
        } else {
            return 0;
        }
    }
    return 1;
}

static b32 parse_functions(ObjFile *file, u8 *payload, u32 len) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        ObjFunc fn = { 0 };
        fn.out_func = -1;
        if (!read_uleb(&c, &fn.type_idx)) return 0;
        bh_arr_push(file->funcs, fn);
    }
    return 1;
}

static b32 parse_globals(ObjFile *file, u8 *payload, u32 len) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        if (c.p + 2 > c.end) return 0;
        ObjGlobal g = { 0 };
        g.out_global = -1;
        g.valtype = *c.p++;
        g.mut = *c.p++;
        u8 *start = c.p;
        if (!skip_init_expr(&c)) return 0;
        g.init = start;
        g.init_len = (u32) (c.p - start);
        bh_arr_push(file->globals, g);
    }
    return 1;
}

static b32 parse_code(ObjFile *file, u8 *payload, u32 len) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    if (count != (u32) bh_arr_length(file->funcs)) return 0;
    for (u32 i = 0; i < count; i++) {
        u32 size = 0;
        if (!read_uleb(&c, &size)) return 0;
        u8 *body = NULL;
        if (!read_bytes(&c, size, &body)) return 0;
        file->funcs[i].body = body;
        file->funcs[i].body_len = size;
        file->funcs[i].body_off = (u32) (body - payload);
    }
    return 1;
}

static b32 parse_data(ObjFile *file, u8 *payload, u32 len, Linker *L) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        u32 flags = 0;
        if (!read_uleb(&c, &flags)) return 0;
        if (flags & 1) {
            obj_error(L, file, "Passive data segments are not supported in object files.");
            return 0;
        }
        if (flags & 2) {
            u32 mem = 0;
            if (!read_uleb(&c, &mem)) return 0;
        }
        if (!skip_init_expr(&c)) return 0;
        u32 size = 0;
        if (!read_uleb(&c, &size)) return 0;
        u8 *data = NULL;
        if (!read_bytes(&c, size, &data)) return 0;
        ObjSegment seg = { 0 };
        seg.data = data;
        seg.len = size;
        seg.payload_off = (u32) (data - payload);
        seg.align = 1;
        bh_arr_push(file->segments, seg);
    }
    return 1;
}

static b32 parse_segment_info(ObjFile *file, ByteCursor *c) {
    u32 count = 0;
    if (!read_uleb(c, &count)) return 0;
    if (count != (u32) bh_arr_length(file->segments)) return 0;
    for (u32 i = 0; i < count; i++) {
        u32 name_len = 0;
        if (!read_uleb(c, &name_len)) return 0;
        if (c->p + name_len > c->end) return 0;
        c->p += name_len;
        u32 align_log = 0;
        u32 flags = 0;
        if (!read_uleb(c, &align_log) || !read_uleb(c, &flags)) return 0;
        if (align_log >= 31) return 0;
        file->segments[i].align = 1u << align_log;
        file->segments[i].flags = flags;
    }
    return 1;
}

static b32 parse_init_funcs(ObjFile *file, ByteCursor *c) {
    u32 count = 0;
    if (!read_uleb(c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        ObjInit init = { 0 };
        if (!read_uleb(c, &init.priority) || !read_uleb(c, &init.symbol_index)) return 0;
        bh_arr_push(file->inits, init);
    }
    return 1;
}

static b32 parse_symbols(ObjFile *file, ByteCursor *c, bh_allocator alloc, Linker *L) {
    u32 count = 0;
    if (!read_uleb(c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        if (c->p >= c->end) return 0;
        ObjSymbol sym = { 0 };
        sym.file = file;
        sym.out_func = -1;
        sym.out_global = -1;
        sym.out_table = -1;
        sym.kind = *c->p++;
        if (!read_uleb(c, &sym.flags)) return 0;
        b32 defined = (sym.flags & SYM_UNDEFINED) == 0;

        if (sym.kind == SYM_FUNCTION || sym.kind == SYM_GLOBAL || sym.kind == SYM_TABLE || sym.kind == SYM_TAG) {
            if (!read_uleb(c, &sym.index)) return 0;
            if (defined || (sym.flags & SYM_EXPLICIT_NAME)) {
                sym.name = read_name(c, alloc);
                if (!sym.name) return 0;
            } else if (sym.kind == SYM_FUNCTION) {
                if (sym.index >= (u32) bh_arr_length(file->func_imports)) return 0;
                sym.name = file->func_imports[sym.index].field;
            } else if (sym.kind == SYM_GLOBAL) {
                if (sym.index >= (u32) bh_arr_length(file->global_imports)) return 0;
                sym.name = file->global_imports[sym.index].field;
            } else if (sym.kind == SYM_TABLE) {
                if (sym.index >= (u32) bh_arr_length(file->table_imports)) return 0;
                sym.name = file->table_imports[sym.index].field;
            } else {
                obj_error(L, file, "Unsupported undefined symbol kind %d.", sym.kind);
                return 0;
            }
        } else if (sym.kind == SYM_DATA) {
            sym.name = read_name(c, alloc);
            if (!sym.name) return 0;
            if (defined) {
                if ((sym.flags & SYM_BINDING_MASK) == 0x3) {
                    if (!read_uleb(c, &sym.size)) return 0;
                    if (c->p >= c->end) return 0;
                    sym.align = *c->p++;
                } else {
                    if (!read_uleb(c, &sym.index)) return 0;
                    if (!read_uleb(c, &sym.offset)) return 0;
                    if (!read_uleb(c, &sym.size)) return 0;
                }
            }
        } else if (sym.kind == SYM_SECTION) {
            if (!read_uleb(c, &sym.index)) return 0;
            sym.name = "";
        } else {
            obj_error(L, file, "Unknown symbol kind %d in '%s'.", sym.kind, file->path);
            return 0;
        }

        bh_arr_push(file->symbols, sym);
    }
    return 1;
}

static b32 parse_linking(ObjFile *file, u8 *payload, u32 len, bh_allocator alloc, Linker *L) {
    ByteCursor c = { payload, payload + len };
    u32 version = 0;
    if (!read_uleb(&c, &version)) return 0;
    if (version != 2) {
        obj_error(L, file, "Unsupported linking metadata version %d in '%s'.", version, file->path);
        return 0;
    }

    while (c.p < c.end) {
        if (c.p >= c.end) break;
        u8 type = *c.p++;
        u32 size = 0;
        if (!read_uleb(&c, &size)) return 0;
        if (c.p + size > c.end) return 0;
        ByteCursor sub = { c.p, c.p + size };
        c.p += size;

        b32 ok = 1;
        if (type == 5) ok = parse_segment_info(file, &sub);
        else if (type == 6) ok = parse_init_funcs(file, &sub);
        else if (type == 8) ok = parse_symbols(file, &sub, alloc, L);
        if (!ok) return 0;
    }
    return 1;
}

static b32 parse_reloc(ObjFile *file, u8 *payload, u32 len, bh_arr(ObjSection) sections) {
    ByteCursor c = { payload, payload + len };
    u32 section_index = 0;
    u32 count = 0;
    if (!read_uleb(&c, &section_index) || !read_uleb(&c, &count)) return 0;
    if (section_index >= (u32) bh_arr_length(sections)) return 0;
    u32 section_id = sections[section_index].id;

    for (u32 i = 0; i < count; i++) {
        ObjReloc rel = { 0 };
        rel.section_id = section_id;
        if (!read_uleb(&c, &rel.type) || !read_uleb(&c, &rel.offset) || !read_uleb(&c, &rel.index)) return 0;
        if (rel.type > RELOC_GLOBAL_INDEX_LEB
            && rel.type != RELOC_TABLE_NUMBER_LEB
            && rel.type != RELOC_MEMORY_ADDR_TLS_SLEB) return 0;
        if (reloc_has_addend(rel.type)) {
            if (!read_sleb(&c, &rel.addend)) return 0;
        }
        bh_arr_push(file->relocs, rel);
    }
    return 1;
}

static b32 feature_allowed(const char *name) {
    static const char *allowed[] = {
        "mutable-globals",
        "sign-ext",
        "nontrapping-fptoint",
        "bulk-memory",
        "bulk-memory-opt",
        "multivalue",
        "reference-types",
        "call-indirect-overlong",
        "extended-const",
        "atomics",
        NULL,
    };
    for (int i = 0; allowed[i]; i++) {
        if (bh_strcmp(name, allowed[i]) == 0) return 1;
    }
    return 0;
}

static b32 parse_features(ObjFile *file, u8 *payload, u32 len, bh_allocator alloc, Linker *L) {
    ByteCursor c = { payload, payload + len };
    u32 count = 0;
    if (!read_uleb(&c, &count)) return 0;
    for (u32 i = 0; i < count; i++) {
        if (c.p >= c.end) return 0;
        u8 prefix = *c.p++;
        char *name = read_name(&c, alloc);
        if (!name) return 0;
        if (prefix == '+' && !feature_allowed(name)) {
            obj_error(L, file, "Object file '%s' uses unsupported WASM feature '%s'.", file->path, name);
            return 0;
        }
    }
    return 1;
}

static b32 parse_custom(ObjFile *file, u8 *payload, u32 len, bh_allocator alloc, Linker *L) {
    ByteCursor c = { payload, payload + len };
    u32 name_len = 0;
    if (!read_uleb(&c, &name_len)) return 0;
    u8 *name_bytes = NULL;
    if (!read_bytes(&c, name_len, &name_bytes)) return 0;

    if (name_len == 7 && bh_memcmp(name_bytes, "linking", 7) == 0) {
        return parse_linking(file, c.p, (u32) (c.end - c.p), alloc, L);
    }
    if (name_len >= 6 && bh_memcmp(name_bytes, "reloc.", 6) == 0) {
        return parse_reloc(file, c.p, (u32) (c.end - c.p), file->sections);
    }
    if (name_len == 15 && bh_memcmp(name_bytes, "target_features", 15) == 0) {
        return parse_features(file, c.p, (u32) (c.end - c.p), alloc, L);
    }
    return 1;
}

static ObjFile *parse_object_bytes(Linker *L, const char *display, const char *archive, OnyxFilePos pos, u8 *bytes, u32 length) {
    bh_allocator alloc = L->module->allocator;
    ObjFile *file = bh_alloc_item(alloc, ObjFile);
    file->path = bh_strdup(alloc, (char *) display);
    file->archive = archive ? bh_strdup(alloc, (char *) archive) : NULL;
    file->pos = pos;
    file->bytes = bytes;
    file->length = length;

    bh_arr_new(alloc, file->types, 4);
    bh_arr_new(alloc, file->func_imports, 4);
    bh_arr_new(alloc, file->global_imports, 4);
    bh_arr_new(alloc, file->table_imports, 2);
    bh_arr_new(alloc, file->funcs, 4);
    bh_arr_new(alloc, file->globals, 4);
    bh_arr_new(alloc, file->segments, 4);
    bh_arr_new(alloc, file->symbols, 4);
    bh_arr_new(alloc, file->relocs, 4);
    bh_arr_new(alloc, file->inits, 4);
    bh_arr_new(alloc, file->sections, 8);
    bh_arr_new(alloc, file->provides, 4);

    if (!file->bytes || file->length < 8) {
        obj_error(L, file, "Could not read object file '%s'.", display);
        return file;
    }
    if (bh_memcmp(file->bytes, "\0asm", 4) != 0 || file->bytes[4] != 1) {
        obj_error(L, file, "'%s' is not a WASM object file.", display);
        return file;
    }

    ByteCursor c = { file->bytes + 8, file->bytes + file->length };
    b32 saw_linking = 0;
    while (c.p < c.end) {
        if (c.p >= c.end) break;
        u8 id = *c.p++;
        u32 size = 0;
        if (!read_uleb(&c, &size) || c.p + size > c.end) {
            obj_error(L, file, "Malformed WASM object '%s'.", display);
            return file;
        }
        u8 *payload = c.p;
        c.p += size;

        ObjSection sec = { id, payload, size };
        bh_arr_push(file->sections, sec);

        b32 ok = 1;
        if (id == SEC_TYPE) ok = parse_types(file, payload, size);
        else if (id == SEC_IMPORT) ok = parse_imports(file, payload, size, alloc);
        else if (id == SEC_FUNCTION) ok = parse_functions(file, payload, size);
        else if (id == SEC_GLOBAL) ok = parse_globals(file, payload, size);
        else if (id == SEC_CODE) ok = parse_code(file, payload, size);
        else if (id == SEC_DATA) ok = parse_data(file, payload, size, L);
        else if (id == SEC_CUSTOM) {
            if (size >= 8 && payload[0] == 7 && bh_memcmp(payload + 1, "linking", 7) == 0) saw_linking = 1;
            ok = parse_custom(file, payload, size, alloc, L);
        }
        if (!ok) {
            if (!file->failed)
                obj_error(L, file, "Malformed WASM object '%s'.", display);
            return file;
        }
    }

    if (!saw_linking) {
        obj_error(L, file, "'%s' has no WASM linking section.", display);
    }
    return file;
}

static ObjSymbol *find_named(ObjFile *file, const char *name, u8 kind) {
    bh_arr_each(ObjSymbol, sym, file->symbols) {
        if (sym->kind != kind) continue;
        if (sym->flags & SYM_UNDEFINED) continue;
        if (!sym->name || bh_strcmp(sym->name, name) != 0) continue;
        return sym;
    }
    return NULL;
}

static b32 is_stack_pointer(ObjSymbol *sym) {
    return sym->kind == SYM_GLOBAL
        && (sym->flags & SYM_UNDEFINED)
        && sym->name
        && bh_strcmp(sym->name, "__stack_pointer") == 0;
}

static b32 is_tls_base(ObjSymbol *sym) {
    return sym->kind == SYM_GLOBAL
        && (sym->flags & SYM_UNDEFINED)
        && sym->name
        && bh_strcmp(sym->name, "__tls_base") == 0;
}

static b32 is_function_table(ObjSymbol *sym) {
    return sym->kind == SYM_TABLE
        && (sym->flags & SYM_UNDEFINED)
        && sym->name
        && bh_strcmp(sym->name, "__indirect_function_table") == 0;
}

static i32 onyx_stack_global(OnyxWasmModule *mod) {
    AstGlobal *stack = &mod->context->builtins.stack_top;
    if (!bh_imap_has(&mod->index_map, (u64) stack)) return -1;
    return (i32) bh_imap_get(&mod->index_map, (u64) stack);
}

static i32 onyx_tls_global(OnyxWasmModule *mod) {
    AstGlobal *tls = &mod->context->builtins.tls_base;
    if (!bh_imap_has(&mod->index_map, (u64) tls)) return -1;
    return (i32) bh_imap_get(&mod->index_map, (u64) tls);
}

static ObjSymbol *find_definition_in(Linker *L, ObjSymbol *undef, const char *only_archive) {
    ObjSymbol *strong = NULL;
    ObjSymbol *weak = NULL;
    bh_arr_each(ObjFile *, pfile, L->files) {
        ObjFile *file = *pfile;
        if (only_archive && (!file->archive || bh_strcmp(file->archive, only_archive) != 0)) continue;
        bh_arr_each(ObjSymbol, sym, file->symbols) {
            if (sym->flags & SYM_UNDEFINED) continue;
            if ((sym->flags & SYM_BINDING_MASK) == SYM_BINDING_LOCAL) continue;
            if (sym->kind != undef->kind) continue;
            if (!sym->name || !undef->name || bh_strcmp(sym->name, undef->name) != 0) continue;
            if ((sym->flags & SYM_BINDING_MASK) == SYM_BINDING_WEAK) {
                if (!weak) weak = sym;
            } else if (strong) {
                obj_error(L, undef->file, "Duplicate symbol '%s'.", undef->name);
                return NULL;
            } else {
                strong = sym;
            }
        }
    }
    if (strong) return strong;
    return weak;
}

static ObjSymbol *find_definition(Linker *L, ObjSymbol *undef) {
    return find_definition_in(L, undef, NULL);
}

static void mark_symbol(Linker *L, ObjSymbol *sym);
static b32 signature_matches(OnyxWasmModule *mod, AstFunction *fn, ObjType *ty, const char *name);

// Returns 1 when a #provide binding exists for this symbol. The binding may
// still fail the signature check; callers should not also report it as undefined.
static b32 bind_provided_function(Linker *L, ObjSymbol *sym) {
    if (sym->kind != SYM_FUNCTION || !sym->name) return 0;
    if (sym->out_func >= 0) return 1;

    ObjProvide *prov = NULL;
    bh_arr_each(ObjProvide, candidate, sym->file->provides) {
        if (candidate->name && bh_strcmp(candidate->name, sym->name) == 0) {
            prov = candidate;
            break;
        }
    }
    if (!prov) return 0;

    ObjFile *file = sym->file;
    if (sym->index >= (u32) bh_arr_length(file->func_imports)) {
        obj_error(L, file, "Provided symbol '%s' is not a function import.", sym->name);
        return 1;
    }

    u32 type_idx = file->func_imports[sym->index].type_idx;
    if (type_idx >= (u32) bh_arr_length(file->types)
        || !signature_matches(L->module, prov->func, &file->types[type_idx], sym->name)) {
        L->failed = 1;
        sym->reported = 1;
        return 1;
    }

    if (!bh_imap_has(&L->module->index_map, (u64) prov->func)) {
        obj_error(L, file, "Provided procedure for '%s' was not compiled.", sym->name);
        return 1;
    }

    sym->live = 1;
    sym->out_func = (i32) bh_imap_get(&L->module->index_map, (u64) prov->func);
    return 1;
}

// An undefined data symbol is a memory reservation in the object. Point it at
// the Onyx global from #provide so load/store relocations use that address.
static b32 bind_provided_data(Linker *L, ObjSymbol *sym) {
    if (sym->kind != SYM_DATA || !sym->name) return 0;
    if (sym->data_id != 0) return 1;

    ObjProvide *prov = NULL;
    bh_arr_each(ObjProvide, candidate, sym->file->provides) {
        if (candidate->name && strcmp(candidate->name, sym->name) == 0) {
            prov = candidate;
            break;
        }
    }
    if (!prov || !prov->memres) return 0;

    AstMemRes *memres = prov->memres;
    b32 sym_tls = (sym->flags & SYM_TLS) != 0;
    if (sym_tls != (memres->threadlocal != 0)) {
        obj_error(L, sym->file, sym_tls
            ? "Symbol '%s' is thread-local, but the provided global is not."
            : "Symbol '%s' is not thread-local, but the provided global is.",
            sym->name);
        sym->reported = 1;
        return 1;
    }

    if (!memres->type) {
        obj_error(L, sym->file, "Provided global for '%s' has no type.", sym->name);
        sym->reported = 1;
        return 1;
    }
    u32 have = (u32) type_size_of(memres->type);
    if (sym->size && have < sym->size) {
        obj_error(L, sym->file, "Provided global for '%s' is %d bytes; the object symbol is %d bytes.",
            sym->name, have, sym->size);
        sym->reported = 1;
        return 1;
    }

    if (memres->threadlocal) {
        sym->tls_offset = memres->tls_offset;
    } else if (memres->data_id == 0) {
        obj_error(L, sym->file, "Provided global for '%s' was not placed in memory.", sym->name);
        sym->reported = 1;
        return 1;
    } else {
        sym->data_id = memres->data_id;
    }
    sym->live = 1;
    return 1;
}

static void queue_symbol(Linker *L, ObjSymbol *sym) {
    if (!sym || sym->live) return;
    sym->live = 1;
    bh_arr_push(L->work, sym);
}

static void mark_symbol(Linker *L, ObjSymbol *sym) {
    if (!sym || L->failed) return;
    if (is_stack_pointer(sym)) {
        sym->live = 1;
        sym->out_global = onyx_stack_global(L->module);
        if (sym->out_global < 0)
            obj_error(L, sym->file, "Internal error: __stack_pointer has no global index.");
        return;
    }

    if (is_tls_base(sym)) {
        sym->live = 1;
        sym->out_global = onyx_tls_global(L->module);
        if (sym->out_global < 0)
            obj_error(L, sym->file, "Internal error: __tls_base has no global index.");
        return;
    }

    if (is_function_table(sym)) {
        sym->live = 1;
        return;
    }

    if (sym->flags & SYM_UNDEFINED) {
        if (sym->resolved) {
            mark_symbol(L, sym->resolved);
            return;
        }
        if (sym->reported) return;
        if (sym->kind == SYM_FUNCTION && sym->out_func >= 0) return;
        // Members of one archive share a namespace: a definition in any
        // member wins over this block's #provide. A plain object still lets
        // its own #provide win over a definition in a different object.
        if (sym->file->archive) {
            ObjSymbol *local = find_definition_in(L, sym, sym->file->archive);
            if (L->failed) return;
            if (local) {
                sym->resolved = local;
                mark_symbol(L, local);
                return;
            }
        }
        if (sym->kind == SYM_DATA && bind_provided_data(L, sym)) return;
        if (bind_provided_function(L, sym)) return;
        ObjSymbol *def = find_definition(L, sym);
        if (!def) {
            sym->reported = 1;
            sym->live = 1;
            obj_error(L, sym->file, "Undefined symbol '%s' while linking '%s'.", sym->name ? sym->name : "", sym->file->path);
            return;
        }
        sym->resolved = def;
        mark_symbol(L, def);
        return;
    }

    queue_symbol(L, sym);
}

static ObjSymbol *definition(ObjSymbol *sym) {
    if (sym && sym->resolved) return sym->resolved;
    return sym;
}

static b32 offset_in_range(u32 off, u32 start, u32 len) {
    return off >= start && off < start + len;
}

static void scan_symbol(Linker *L, ObjSymbol *sym) {
    ObjFile *file = sym->file;
    if (sym->kind == SYM_FUNCTION) {
        u32 imports = (u32) bh_arr_length(file->func_imports);
        if (sym->index < imports || sym->index - imports >= (u32) bh_arr_length(file->funcs)) {
            obj_error(L, file, "Function symbol '%s' has an invalid index.", sym->name ? sym->name : "");
            return;
        }
        ObjFunc *fn = &file->funcs[sym->index - imports];
        bh_arr_each(ObjReloc, rel, file->relocs) {
            if (rel->section_id != SEC_CODE) continue;
            if (!offset_in_range(rel->offset, fn->body_off, fn->body_len)) continue;
            if (rel->type == RELOC_TYPE_INDEX_LEB) continue;
            if (rel->index >= (u32) bh_arr_length(file->symbols)) {
                obj_error(L, file, "Relocation has an invalid symbol index.");
                return;
            }
            mark_symbol(L, &file->symbols[rel->index]);
        }
    } else if (sym->kind == SYM_DATA && (sym->flags & SYM_ABSOLUTE) == 0 && (sym->flags & SYM_BINDING_MASK) != 0x3) {
        if (sym->index >= (u32) bh_arr_length(file->segments)) {
            obj_error(L, file, "Data symbol '%s' has an invalid segment.", sym->name ? sym->name : "");
            return;
        }
        ObjSegment *seg = &file->segments[sym->index];
        seg->live = 1;
        bh_arr_each(ObjReloc, rel, file->relocs) {
            if (rel->section_id != SEC_DATA) continue;
            if (!offset_in_range(rel->offset, seg->payload_off, seg->len)) continue;
            if (rel->type == RELOC_TYPE_INDEX_LEB) continue;
            if (rel->index >= (u32) bh_arr_length(file->symbols)) {
                obj_error(L, file, "Relocation has an invalid symbol index.");
                return;
            }
            mark_symbol(L, &file->symbols[rel->index]);
        }
    }
}

static void drain(Linker *L) {
    while (!L->failed && bh_arr_length(L->work) > 0) {
        ObjSymbol *sym = L->work[bh_arr_length(L->work) - 1];
        bh_arr_pop(L->work);
        scan_symbol(L, sym);
    }
}

static b32 file_is_included(ObjFile *file) {
    bh_arr_each(ObjSymbol, sym, file->symbols) {
        if ((sym->flags & SYM_UNDEFINED) == 0 && sym->live) return 1;
    }
    return 0;
}

static void mark_constructors(Linker *L) {
    b32 grew = 1;
    while (grew && !L->failed) {
        grew = 0;
        bh_arr_each(ObjFile *, pfile, L->files) {
            ObjFile *file = *pfile;
            if (!file_is_included(file)) continue;
            bh_arr_each(ObjInit, init, file->inits) {
                if (init->symbol_index >= (u32) bh_arr_length(file->symbols)) {
                    obj_error(L, file, "Constructor references an invalid symbol.");
                    return;
                }
                ObjSymbol *sym = definition(&file->symbols[init->symbol_index]);
                if (sym && !sym->live) {
                    mark_symbol(L, &file->symbols[init->symbol_index]);
                    grew = 1;
                }
            }
        }
        drain(L);
    }
}

static Type *unwrap_type(Type *type) {
    while (type && type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;
    if (type && type->kind == Type_Kind_Enum) type = type->Enum.backing;
    return type;
}

static b32 c_wasm_type(Type *type, u8 *out) {
    type = unwrap_type(type);
    if (!type) return 0;
    if (type_get_param_pass(type) != Param_Pass_By_Value) return 0;
    if (type->kind == Type_Kind_Pointer || type->kind == Type_Kind_MultiPointer) {
        *out = 0x7f;
        return 1;
    }
    if (type->kind != Type_Kind_Basic) return 0;
    if (type->Basic.flags & Basic_Flag_SIMD) return 0;
    if (type->Basic.flags & (Basic_Flag_Integer | Basic_Flag_Boolean | Basic_Flag_Pointer | Basic_Flag_Type_Index)) {
        if (type->Basic.size <= 4) { *out = 0x7f; return 1; }
        if (type->Basic.size <= 8) { *out = 0x7e; return 1; }
    }
    if (type->Basic.flags & Basic_Flag_Float) {
        if (type->Basic.size <= 4) { *out = 0x7d; return 1; }
        if (type->Basic.size <= 8) { *out = 0x7c; return 1; }
    }
    return 0;
}

static b32 lower_c_param(Type *type, u8 *out) {
    if (type_get_param_pass(type) == Param_Pass_By_Implicit_Pointer) {
        *out = 0x7f;
        return 1;
    }
    if (type_get_param_pass(type) == Param_Pass_By_Multiple_Values) return 0;
    Type *one = type_struct_is_just_one_basic_value(unwrap_type(type));
    if (one) type = one;
    return c_wasm_type(type, out);
}

static b32 signature_matches(OnyxWasmModule *mod, AstFunction *fn, ObjType *ty, const char *name) {
    Type *ft = fn->type;
    if (!ft || ft->kind != Type_Kind_Function) {
        onyx_report_error(mod->context, fn->token->pos, Error_Critical,
            "Object function '%s' was not typechecked.", name);
        return 0;
    }

    b32 sret = type_function_get_cc(ft) == CC_Return_Stack;
    u32 base = 0;
    if (sret) {
        if (ty->nparams < 1 || ty->params[0] != 0x7f) {
            onyx_report_error(mod->context, fn->token->pos, Error_Critical,
                "'%s' returns an aggregate, but the object file has no leading return pointer.",
                name);
            return 0;
        }
        base = 1;
    }

    if (ty->nparams != base + (u32) ft->Function.param_count) {
        onyx_report_error(mod->context, fn->token->pos, Error_Critical,
            "Declaration of '%s' has %d parameter(s); the object file has %d.",
            name, ft->Function.param_count, ty->nparams - base);
        return 0;
    }
    for (u32 i = 0; i < (u32) ft->Function.param_count; i++) {
        u8 expect = 0;
        if (!lower_c_param(ft->Function.params[i], &expect)) {
            onyx_report_error(mod->context, fn->token->pos, Error_Critical,
                "Parameter %d of '%s' is not a C-compatible type.",
                i + 1, name);
            return 0;
        }
        if (expect != ty->params[base + i]) {
            onyx_report_error(mod->context, fn->token->pos, Error_Critical,
                "Parameter %d of '%s' does not match the object file.",
                i + 1, name);
            return 0;
        }
    }

    Type *ret = unwrap_type(ft->Function.return_type);
    b32 is_void = ret && ret->kind == Type_Kind_Basic && ret->Basic.kind == Basic_Kind_Void;
    if (sret) {
        if (ty->nresults != 0) {
            onyx_report_error(mod->context, fn->token->pos, Error_Critical,
                "'%s' returns an aggregate, but the object file also returns a wasm value.",
                name);
            return 0;
        }
        return 1;
    }
    if (is_void) {
        if (ty->nresults != 0) {
            onyx_report_error(mod->context, fn->token->pos, Error_Critical,
                "'%s' is declared void, but the object file returns a value.", name);
            return 0;
        }
        return 1;
    }
    if (ty->nresults != 1) {
        onyx_report_error(mod->context, fn->token->pos, Error_Critical,
            "'%s' must have exactly one WASM result.", name);
        return 0;
    }
    u8 expect = 0;
    if (!c_wasm_type(ret, &expect) || expect != ty->results[0]) {
        onyx_report_error(mod->context, fn->token->pos, Error_Critical,
            "The return type of '%s' does not match the object file.", name);
        return 0;
    }
    return 1;
}

static i32 intern_obj_type(OnyxWasmModule *mod, ObjType *type) {
    if (type->onyx_idx >= 0) return type->onyx_idx;
    if (type->nresults > 1 || type->nparams > 64) return -1;

    char buf[80];
    char *t = buf;
    for (u32 i = 0; i < type->nparams; i++) *(t++) = (char) type->params[i];
    *(t++) = ':';
    u8 ret = type->nresults == 0 ? 0x00 : type->results[0];
    *(t++) = (char) ret;
    *t = 0;

    i32 index = shgeti(mod->type_map, buf);
    if (index != -1) {
        type->onyx_idx = mod->type_map[index].value;
        return type->onyx_idx;
    }

    WasmFuncType *wasm_type = bh_alloc(mod->allocator, sizeof(WasmFuncType) + sizeof(WasmType) * type->nparams);
    wasm_type->return_type = ret;
    wasm_type->param_count = type->nparams;
    for (u32 i = 0; i < type->nparams; i++) wasm_type->param_types[i] = type->params[i];
    bh_arr_push(mod->types, wasm_type);
    shput(mod->type_map, buf, mod->next_type_idx);
    type->onyx_idx = mod->next_type_idx++;
    return type->onyx_idx;
}

static i32 alloc_defined_func(OnyxWasmModule *mod, WasmFunc func) {
    i32 idx = (i32) mod->next_func_idx++;
    bh_arr_set_at(mod->funcs, idx, func);
    return idx;
}

static b32 decode_global_init(OnyxWasmModule *mod, ObjGlobal *global, bh_arr(WasmInstruction) *out) {
    ByteCursor c = { global->init, global->init + global->init_len };
    bh_arr_new(mod->allocator, *out, 2);
    while (c.p < c.end) {
        u8 op = *c.p++;
        if (op == 0x0b) return 1;
        WasmInstruction in = { 0 };
        if (op == 0x41 || op == 0x42) {
            i32 v = 0;
            if (!read_sleb(&c, &v)) return 0;
            in.type = op == 0x41 ? WI_I32_CONST : WI_I64_CONST;
            in.data.l = v;
            bh_arr_push(*out, in);
            continue;
        }
        if (op == 0x43 || op == 0x44) {
            u32 n = op == 0x43 ? 4 : 8;
            if (c.p + n > c.end) return 0;
            in.type = op == 0x43 ? WI_F32_CONST : WI_F64_CONST;
            if (op == 0x43) bh_memcpy(&in.data.f, c.p, 4);
            else            bh_memcpy(&in.data.d, c.p, 8);
            c.p += n;
            bh_arr_push(*out, in);
            continue;
        }
        return 0;
    }
    return 0;
}

static u32 align_tls(u32 value, u32 align) {
    if (align <= 1) return value;
    u32 mask = align - 1;
    return (value + mask) & ~mask;
}

static void note_tls(OnyxWasmModule *mod, u32 at, u32 align) {
    if (!mod->tls_span || at < mod->tls_image_begin) mod->tls_image_begin = at;
    mod->tls_span = 1;
    if (align > mod->tls_align) mod->tls_align = align;
}

static void place_tls_segment(OnyxWasmModule *mod, ObjSegment *seg) {
    if (seg->tls_placed) return;
    u32 align = seg->align ? seg->align : 1;
    u32 at = align_tls(mod->next_tls_offset, align);
    seg->tls_offset = at;
    seg->tls_placed = 1;
    note_tls(mod, at, align);
    mod->next_tls_offset = at + seg->len;

    u32 len = seg->len ? seg->len : 1;
    u8 *copy = bh_alloc_array(mod->allocator, u8, len);
    if (seg->len && seg->data) bh_memcpy(copy, seg->data, seg->len);
    seg->copy = copy;
}

static b32 segment_is_tls(ObjFile *file, u32 seg_index) {
    if (seg_index >= (u32) bh_arr_length(file->segments)) return 0;
    if (file->segments[seg_index].flags & SEG_TLS) return 1;
    bh_arr_each(ObjSymbol, sym, file->symbols) {
        if (!(sym->flags & SYM_TLS) || sym->kind != SYM_DATA) continue;
        if (sym->flags & SYM_UNDEFINED) continue;
        if ((sym->flags & SYM_BINDING_MASK) == 0x3) continue;
        if (sym->index == seg_index) return 1;
    }
    return 0;
}

static u32 place_bytes(OnyxWasmModule *mod, u8 *bytes, u32 len, u32 align) {
    if (align == 0) align = 1;
    WasmDatum datum = { 0 };
    datum.data = bytes;
    datum.length = len;
    datum.alignment = align;
    datum.id = (u32) bh_arr_length(mod->data) + 1;
    bh_arr_push(mod->data, datum);
    return datum.id;
}

static void assign_outputs(Linker *L) {
    OnyxWasmModule *mod = L->module;
    bh_arr_each(ObjFile *, pfile, L->files) {
        ObjFile *file = *pfile;
        for (u32 i = 0; i < (u32) bh_arr_length(file->segments); i++) {
            ObjSegment *seg = &file->segments[i];
            if (!seg->live || !segment_is_tls(file, i)) continue;
            place_tls_segment(mod, seg);
        }
    }
    bh_arr_each(ObjFile *, pfile, L->files) {
        ObjFile *file = *pfile;
        bh_arr_each(ObjSymbol, sym, file->symbols) {
            if (!sym->live || (sym->flags & SYM_UNDEFINED)) continue;
            if (sym->kind == SYM_FUNCTION) {
                u32 imports = (u32) bh_arr_length(file->func_imports);
                if (sym->index < imports) continue;
                ObjFunc *fn = &file->funcs[sym->index - imports];
                if (fn->included) {
                    sym->out_func = fn->out_func;
                    continue;
                }
                if (!fn->body || fn->type_idx >= (u32) bh_arr_length(file->types)) {
                    obj_error(L, file, "Function '%s' has an invalid type index or no body.", sym->name ? sym->name : "");
                    return;
                }
                i32 type_idx = intern_obj_type(mod, &file->types[fn->type_idx]);
                if (type_idx < 0) {
                    obj_error(L, file, "Function '%s' has an unsupported WASM type.", sym->name);
                    return;
                }
                u8 *copy = bh_alloc_array(mod->allocator, u8, fn->body_len ? fn->body_len : 1);
                if (fn->body_len) bh_memcpy(copy, fn->body, fn->body_len);
                WasmFunc wasm_func = { 0 };
                wasm_func.type_idx = type_idx;
                wasm_func.raw_body = copy;
                wasm_func.raw_body_len = fn->body_len;
                wasm_func.name = sym->name;
                fn->out_func = alloc_defined_func(mod, wasm_func);
                fn->copy = copy;
                fn->included = 1;
                sym->out_func = fn->out_func;
            } else if (sym->kind == SYM_GLOBAL) {
                u32 imports = (u32) bh_arr_length(file->global_imports);
                if (sym->index < imports || sym->index - imports >= (u32) bh_arr_length(file->globals)) {
                    obj_error(L, file, "Global symbol '%s' has an invalid index.", sym->name);
                    return;
                }
                ObjGlobal *global = &file->globals[sym->index - imports];
                if (global->out_global >= 0) {
                    sym->out_global = global->out_global;
                    continue;
                }
                bh_arr(WasmInstruction) init = NULL;
                if (!decode_global_init(mod, global, &init)) {
                    obj_error(L, file, "Global '%s' has an initializer this linker cannot copy.", sym->name);
                    return;
                }
                WasmGlobal wg = { 0 };
                wg.type = global->valtype;
                wg.mutable = global->mut ? 1 : 0;
                wg.initial_value = init;
                i32 gidx = (i32) mod->next_global_idx++;
                bh_arr_set_at(mod->globals, gidx, wg);
                global->out_global = gidx;
                sym->out_global = gidx;
            } else if (sym->kind == SYM_DATA) {
                if (sym->flags & SYM_ABSOLUTE) continue;
                b32 tls = (sym->flags & SYM_TLS) != 0;
                if ((sym->flags & SYM_BINDING_MASK) == 0x3) {
                    u32 align = sym->align < 31 ? (1u << sym->align) : 1;
                    if (tls) {
                        u32 at = align_tls(mod->next_tls_offset, align);
                        sym->tls_offset = at;
                        note_tls(mod, at, align);
                        mod->next_tls_offset = at + sym->size;
                    } else {
                        u32 len = sym->size ? sym->size : 1;
                        u8 *zeros = bh_alloc_array(mod->allocator, u8, len);
                        sym->data_id = place_bytes(mod, zeros, sym->size, align);
                    }
                } else if (sym->index < (u32) bh_arr_length(file->segments)) {
                    ObjSegment *seg = &file->segments[sym->index];
                    if (seg->tls_placed) {
                        sym->tls_offset = seg->tls_offset + sym->offset;
                    } else {
                        if (!seg->live) seg->live = 1;
                        if (seg->data_id == 0) {
                            u32 len = seg->len ? seg->len : 1;
                            u8 *copy = bh_alloc_array(mod->allocator, u8, len);
                            if (seg->len && seg->data) bh_memcpy(copy, seg->data, seg->len);
                            seg->copy = copy;
                            seg->data_id = place_bytes(mod, copy, seg->len, seg->align);
                        }
                        sym->data_id = seg->data_id;
                    }
                }
            }
        }
    }
}

typedef struct PatchSite {
    u8 *bytes;
    u32 at;
    u32 len;
    b32 found;
    b32 live;
} PatchSite;

static PatchSite locate_reloc(ObjFile *file, ObjReloc *rel) {
    PatchSite site = { 0 };
    if (rel->section_id == SEC_CODE) {
        bh_arr_each(ObjFunc, fn, file->funcs) {
            if (!offset_in_range(rel->offset, fn->body_off, fn->body_len)) continue;
            site.found = 1;
            site.live = fn->included;
            site.bytes = fn->copy;
            site.at = rel->offset - fn->body_off;
            site.len = fn->body_len;
            return site;
        }
    } else if (rel->section_id == SEC_DATA) {
        bh_arr_each(ObjSegment, seg, file->segments) {
            if (!offset_in_range(rel->offset, seg->payload_off, seg->len)) continue;
            site.found = 1;
            site.live = seg->live;
            site.bytes = seg->copy;
            site.at = rel->offset - seg->payload_off;
            site.len = seg->len;
            return site;
        }
    }
    return site;
}

static void patch_immediate(Linker *L, ObjFile *file, u8 *bytes, u32 at, u32 len, u32 kind, i32 value) {
    u32 width = kind == ONYX_OBJ_PATCH_I32 ? 4 : 5;
    if (at + width > len) {
        obj_error(L, file, "Relocation does not fit in its section.");
        return;
    }
    if (kind == ONYX_OBJ_PATCH_ULEB5) write_uleb5(bytes + at, (u32) value);
    else if (kind == ONYX_OBJ_PATCH_SLEB5) write_sleb5(bytes + at, value);
    else write_i32(bytes + at, value);
}

static void apply_relocs(Linker *L) {
    OnyxWasmModule *mod = L->module;
    b32 need_table = 0;
    bh_arr_each(ObjFile *, pfile, L->files) {
        bh_arr_each(ObjReloc, rel, (*pfile)->relocs) {
            if (rel->type == RELOC_TABLE_INDEX_SLEB || rel->type == RELOC_TABLE_INDEX_I32) need_table = 1;
        }
    }
    if (need_table && bh_arr_length(mod->elems) == 0) {
        ObjType void_type = { 0 };
        i32 type_idx = intern_obj_type(mod, &void_type);
        u8 *body = bh_alloc_array(mod->allocator, u8, 3);
        body[0] = 0x00;
        body[1] = 0x00;
        body[2] = 0x0b;
        WasmFunc trap = { 0 };
        trap.type_idx = type_idx;
        trap.raw_body = body;
        trap.raw_body_len = 3;
        trap.name = "__object_null_trap";
        i32 idx = alloc_defined_func(mod, trap);
        bh_arr_push(mod->elems, (i32) (idx + mod->next_foreign_func_idx));
    }

    bh_arr_each(ObjFile *, pfile, L->files) {
        ObjFile *file = *pfile;
        if (!file_is_included(file)) continue;
        bh_arr_each(ObjReloc, rel, file->relocs) {
            PatchSite site = locate_reloc(file, rel);
            if (!site.found || !site.live) continue;

            if (rel->type == RELOC_TABLE_NUMBER_LEB) {
                // Onyx has a single function table. Its table number is 0.
                patch_immediate(L, file, site.bytes, site.at, site.len, ONYX_OBJ_PATCH_ULEB5, 0);
                continue;
            }

            if (rel->type == RELOC_TYPE_INDEX_LEB) {
                if (rel->index >= (u32) bh_arr_length(file->types)) {
                    obj_error(L, file, "Type relocation is out of range.");
                    return;
                }
                i32 type_idx = intern_obj_type(mod, &file->types[rel->index]);
                if (type_idx < 0) {
                    obj_error(L, file, "Unsupported WASM type in a relocation.");
                    return;
                }
                patch_immediate(L, file, site.bytes, site.at, site.len, ONYX_OBJ_PATCH_ULEB5, type_idx);
                continue;
            }

            if (rel->index >= (u32) bh_arr_length(file->symbols)) {
                obj_error(L, file, "Relocation has an invalid symbol index.");
                return;
            }
            ObjSymbol *sym = definition(&file->symbols[rel->index]);
            if (!sym) continue;

            if (rel->type == RELOC_FUNCTION_INDEX_LEB) {
                if (sym->out_func < 0) {
                    obj_error(L, file, "Function relocation against '%s' was not linked.", sym->name ? sym->name : "");
                    return;
                }
                i32 final = sym->out_func + (i32) mod->next_foreign_func_idx;
                patch_immediate(L, file, site.bytes, site.at, site.len, ONYX_OBJ_PATCH_ULEB5, final);
            } else if (rel->type == RELOC_GLOBAL_INDEX_LEB) {
                if (sym->kind != SYM_GLOBAL || sym->out_global < 0) {
                    obj_error(L, file, "Global relocation against '%s' is not supported.", sym->name ? sym->name : "");
                    return;
                }
                patch_immediate(L, file, site.bytes, site.at, site.len, ONYX_OBJ_PATCH_ULEB5, sym->out_global);
            } else if (rel->type == RELOC_TABLE_INDEX_SLEB || rel->type == RELOC_TABLE_INDEX_I32) {
                if (sym->out_func < 0) {
                    obj_error(L, file, "Table relocation against '%s' was not linked.", sym->name ? sym->name : "");
                    return;
                }
                if (sym->out_table < 0) {
                    sym->out_table = bh_arr_length(mod->elems);
                    i32 final = sym->out_func + (i32) mod->next_foreign_func_idx;
                    bh_arr_push(mod->elems, final);
                }
                u32 kind = rel->type == RELOC_TABLE_INDEX_I32 ? ONYX_OBJ_PATCH_I32 : ONYX_OBJ_PATCH_SLEB5;
                patch_immediate(L, file, site.bytes, site.at, site.len, kind, sym->out_table);
            } else if (reloc_has_addend(rel->type)) {
                OnyxObjectMemPatch patch = { 0 };
                patch.bytes = site.bytes;
                patch.at = site.at;
                patch.addend = rel->addend;
                if (rel->type == RELOC_MEMORY_ADDR_LEB) patch.kind = ONYX_OBJ_PATCH_ULEB5;
                else if (rel->type == RELOC_MEMORY_ADDR_SLEB || rel->type == RELOC_MEMORY_ADDR_TLS_SLEB) patch.kind = ONYX_OBJ_PATCH_SLEB5;
                else patch.kind = ONYX_OBJ_PATCH_I32;
                b32 in_tls = (sym->flags & SYM_TLS) != 0;
                if (!in_tls && sym->kind == SYM_DATA && sym->file
                    && (sym->flags & SYM_BINDING_MASK) != 0x3
                    && sym->index < (u32) bh_arr_length(sym->file->segments)
                    && sym->file->segments[sym->index].tls_placed) {
                    in_tls = 1;
                }
                if (sym->flags & SYM_ABSOLUTE) {
                    patch.data_id = 0;
                    patch.extra = sym->offset;
                } else if (in_tls) {
                    // Clang addresses TLS as global.get __tls_base; i32.const offset; i32.add.
                    patch.data_id = 0;
                    patch.extra = sym->tls_offset;
                } else {
                    if (sym->data_id == 0) {
                        obj_error(L, file, "Memory relocation against '%s' has no address.", sym->name ? sym->name : "");
                        return;
                    }
                    patch.data_id = sym->data_id;
                    patch.extra = (sym->flags & SYM_BINDING_MASK) == 0x3 ? 0 : sym->offset;
                }
                if (patch.at + reloc_width(rel->type) > site.len) {
                    obj_error(L, file, "Relocation does not fit in its section.");
                    return;
                }
                bh_arr_push(mod->object_mem_patches, patch);
            } else {
                obj_error(L, file, "Unsupported relocation type %d.", rel->type);
                return;
            }
        }
    }
}

typedef struct LinkedCtor {
    u32 priority;
    ObjSymbol *sym;
} LinkedCtor;

static void emit_constructors(Linker *L) {
    bh_arr(LinkedCtor) ctors = NULL;
    bh_arr_new(L->module->allocator, ctors, 4);
    bh_arr_each(ObjFile *, pfile, L->files) {
        ObjFile *file = *pfile;
        if (!file_is_included(file)) continue;
        bh_arr_each(ObjInit, init, file->inits) {
            if (init->symbol_index >= (u32) bh_arr_length(file->symbols)) continue;
            LinkedCtor ctor;
            ctor.priority = init->priority;
            ctor.sym = definition(&file->symbols[init->symbol_index]);
            bh_arr_push(ctors, ctor);
        }
    }
    if (bh_arr_length(ctors) == 0) return;

    i32 ctor_count = bh_arr_length(ctors);
    for (i32 i = 1; i < ctor_count; i++) {
        LinkedCtor key = ctors[i];
        i32 j = i - 1;
        while (j >= 0 && ctors[j].priority > key.priority) {
            ctors[j + 1] = ctors[j];
            j--;
        }
        ctors[j + 1] = key;
    }

    AstFunction *runner = L->module->context->builtins.run_init_procedures;
    if (!runner || !bh_imap_has(&L->module->index_map, (u64) runner)) {
        obj_error(L, L->files[0], "Object constructors need __run_init_procedures, which was not compiled.");
        return;
    }
    i32 runner_idx = (i32) bh_imap_get(&L->module->index_map, (u64) runner);
    WasmFunc *wf = &L->module->funcs[runner_idx];
    if (!wf->code || bh_arr_length(wf->code) == 0 || bh_arr_last(wf->code).type != WI_BLOCK_END) {
        obj_error(L, L->files[0], "Could not insert object constructors into __run_init_procedures.");
        return;
    }

    WasmInstruction end = bh_arr_pop(wf->code);
    bh_arr_each(LinkedCtor, ctor, ctors) {
        if (!ctor->sym || ctor->sym->out_func < 0) {
            obj_error(L, L->files[0], "Object constructor was not linked.");
            return;
        }
        WasmInstruction call = { 0 };
        call.type = WI_CALL;
        call.data.l = ctor->sym->out_func + (i32) L->module->next_foreign_func_idx;
        bh_arr_push(wf->code, call);
    }
    bh_arr_push(wf->code, end);
}

static void source_error(Linker *L, OnyxFilePos pos, const char *fmt, ...) {
    L->failed = 1;
    if (!onyx_errors_are_enabled(L->module->context)) return;
    va_list args;
    va_start(args, fmt);
    char *msg = bh_aprintf_va(L->module->context->errors.msg_alloc, fmt, args);
    va_end(args);
    OnyxError err = { .pos = pos, .rank = Error_Critical, .text = msg };
    onyx_submit_error(L->module->context, err);
}

static b32 file_is_source(ObjFile *file, const char *source) {
    if (!file || !source) return 0;
    if (file->archive) return bh_strcmp(file->archive, source) == 0;
    return file->path && bh_strcmp(file->path, source) == 0;
}

static b32 source_is_loaded(Linker *L, const char *path) {
    bh_arr_each(ObjFile *, pfile, L->files) {
        if (file_is_source(*pfile, path)) return 1;
    }
    return 0;
}

static i32 ar_field_int(const char *s, int n) {
    i32 value = 0;
    b32 saw = 0;
    for (int i = 0; i < n; i++) {
        if (s[i] == ' ') {
            if (saw) break;
            continue;
        }
        if (s[i] < '0' || s[i] > '9') return -1;
        saw = 1;
        value = value * 10 + (s[i] - '0');
    }
    return saw ? value : -1;
}

static char *ar_copy_name(bh_allocator alloc, const u8 *s, u32 n) {
    while (n > 0 && (s[n - 1] == 0 || s[n - 1] == '\n' || s[n - 1] == '/')) n--;
    return bh_strdup_len(alloc, (char *) s, (i32) n);
}

static b32 ar_is_symbol_index(const char *name) {
    return name
        && (bh_strcmp(name, "/") == 0
            || bh_strcmp(name, "/SYM64") == 0
            || bh_strcmp(name, "__.SYMDEF") == 0
            || bh_strcmp(name, "__.SYMDEF SORTED") == 0
            || bh_strcmp(name, "__.SYMDEF_64") == 0
            || bh_strcmp(name, "__.SYMDEF SORTED_64") == 0);
}

static char *ar_strtab_name(bh_allocator alloc, const u8 *tab, u32 tab_len, i32 offset) {
    if (!tab || offset < 0 || (u32) offset >= tab_len) return NULL;
    u32 i = (u32) offset;
    u32 start = i;
    while (i < tab_len && tab[i] != 0 && tab[i] != '\n' && tab[i] != '/') i++;
    return ar_copy_name(alloc, tab + start, i - start);
}

static void load_archive(Linker *L, const char *path, OnyxFilePos pos, u8 *bytes, u32 length) {
    bh_allocator alloc = L->module->allocator;
    if (length >= 8 && bh_memcmp(bytes, "!<thin>\n", 8) == 0) {
        source_error(L, pos, "Thin archive '%s' is not supported. Create a normal archive.", path);
        return;
    }
    if (length < 8 || bh_memcmp(bytes, "!<arch>\n", 8) != 0) {
        source_error(L, pos, "'%s' is not a WASM object or an archive.", path);
        return;
    }

    const u8 *strtab = NULL;
    u32 strtab_len = 0;
    i32 wasm_members = 0;
    u32 cursor = 8;
    while (cursor < length && !L->failed) {
        if (length - cursor < 60) {
            if (length - cursor <= 1) break;
            source_error(L, pos, "Malformed archive '%s'.", path);
            return;
        }
        const u8 *hdr = bytes + cursor;
        if (hdr[58] != '`' || hdr[59] != '\n') {
            source_error(L, pos, "Malformed archive '%s'.", path);
            return;
        }
        i32 member_size = ar_field_int((const char *) hdr + 48, 10);
        if (member_size < 0 || cursor + 60 + (u32) member_size > length) {
            source_error(L, pos, "Malformed archive '%s'.", path);
            return;
        }
        u8 *payload = bytes + cursor + 60;
        u32 payload_len = (u32) member_size;
        cursor += 60 + (u32) member_size;
        if (member_size & 1) {
            if (cursor < length) cursor++;
        }

        char raw[17];
        bh_memcpy(raw, hdr, 16);
        int namelen = 16;
        while (namelen > 0 && raw[namelen - 1] == ' ') namelen--;
        raw[namelen] = 0;

        char *member = NULL;
        u8 *body = payload;
        u32 body_len = payload_len;
        if (ar_is_symbol_index(raw)) continue;
        if (bh_strcmp(raw, "//") == 0) {
            strtab = payload;
            strtab_len = payload_len;
            continue;
        }
        if (raw[0] == '/' && raw[1] >= '0' && raw[1] <= '9') {
            member = ar_strtab_name(alloc, strtab, strtab_len, ar_field_int(raw + 1, namelen - 1));
            if (!member) {
                source_error(L, pos, "Archive '%s' has a member name the string table does not contain.", path);
                return;
            }
        } else if (namelen >= 3 && bh_memcmp(raw, "#1/", 3) == 0) {
            i32 n = ar_field_int(raw + 3, namelen - 3);
            if (n < 0 || (u32) n > payload_len) {
                source_error(L, pos, "Malformed archive '%s'.", path);
                return;
            }
            member = ar_copy_name(alloc, payload, (u32) n);
            body = payload + n;
            body_len = payload_len - (u32) n;
        } else {
            if (namelen > 0 && raw[namelen - 1] == '/') raw[--namelen] = 0;
            member = ar_copy_name(alloc, (const u8 *) raw, (u32) namelen);
        }

        // macOS ar names its symbol index __.SYMDEF or __.SYMDEF SORTED.
        // The name is often longer than 15 bytes, so it arrives through #1/.
        if (ar_is_symbol_index(member)) continue;

        if (body_len < 4 || bh_memcmp(body, "\0asm", 4) != 0) {
            source_error(L, pos, "Archive member '%s' in '%s' is not a WASM object.", member ? member : "", path);
            return;
        }
        char *display = bh_aprintf(alloc, "%s(%s)", path, member ? member : "");
        ObjFile *file = parse_object_bytes(L, display, path, pos, body, body_len);
        bh_arr_push(L->files, file);
        wasm_members++;
        if (file->failed) return;
    }

    if (!L->failed && wasm_members == 0)
        source_error(L, pos, "Archive '%s' contains no WASM objects.", path);
}

static void load_source(Linker *L, const char *path, OnyxFilePos pos) {
    if (source_is_loaded(L, path) || L->failed) return;

    bh_allocator alloc = L->module->allocator;
    bh_file_contents contents = bh_file_read_contents(alloc, path);
    u8 *bytes = (u8 *) contents.data;
    u32 length = (u32) contents.length;
    if (bytes && length >= 8 && (bh_memcmp(bytes, "!<arch>\n", 8) == 0 || bh_memcmp(bytes, "!<thin>\n", 8) == 0)) {
        load_archive(L, path, pos, bytes, length);
        return;
    }

    ObjFile *file = parse_object_bytes(L, path, NULL, pos, bytes, length);
    bh_arr_push(L->files, file);
}

static i32 collect_source_function(Linker *L, const char *source, const char *name, ObjSymbol **out) {
    i32 count = 0;
    *out = NULL;
    bh_arr_each(ObjFile *, pfile, L->files) {
        if (!file_is_source(*pfile, source)) continue;
        ObjSymbol *sym = find_named(*pfile, name, SYM_FUNCTION);
        if (!sym) continue;
        count++;
        *out = sym;
    }
    return count;
}

static void finish_tls_image(Linker *L) {
    OnyxWasmModule *mod = L->module;
    if (!mod->tls_span || L->failed) return;
    if (mod->next_tls_offset < mod->tls_image_begin) return;
    u32 size = mod->next_tls_offset - mod->tls_image_begin;
    if (size == 0) return;

    u32 begin = mod->tls_image_begin;
    u32 blob_len = size + 8;
    u8 *blob = bh_alloc_array(mod->allocator, u8, blob_len);
    blob[0] = (u8) begin;
    blob[1] = (u8) (begin >> 8);
    blob[2] = (u8) (begin >> 16);
    blob[3] = (u8) (begin >> 24);
    blob[4] = (u8) size;
    blob[5] = (u8) (size >> 8);
    blob[6] = (u8) (size >> 16);
    blob[7] = (u8) (size >> 24);

    bh_arr_each(ObjFile *, pfile, L->files) {
        bh_arr_each(ObjSegment, seg, (*pfile)->segments) {
            if (!seg->tls_placed || !seg->len || !seg->copy) continue;
            u32 dest = 8 + (seg->tls_offset - begin);
            if (dest + seg->len > blob_len) continue;
            bh_memcpy(blob + dest, seg->copy, seg->len);
        }
    }

    b32 patched = 0;
    bh_arr_each(OnyxObjectMemPatch, patch, mod->object_mem_patches) {
        b32 found = 0;
        bh_arr_each(ObjFile *, pfile, L->files) {
            bh_arr_each(ObjSegment, seg, (*pfile)->segments) {
                if (!seg->tls_placed || patch->bytes != seg->copy) continue;
                patch->bytes = blob;
                patch->at = 8 + (seg->tls_offset - begin) + patch->at;
                patched = 1;
                found = 1;
                break;
            }
            if (found) break;
        }
    }

    b32 nonzero = 0;
    for (u32 i = 0; i < size; i++) {
        if (blob[8 + i]) {
            nonzero = 1;
            break;
        }
    }
    if (!nonzero && !patched) return;
    mod->tls_image_data_id = place_bytes(mod, blob, blob_len, 4);
}

static void add_file_provides(Linker *L, ObjFile *file, AstObjectBlock *block) {
    bh_arr_each(ObjectProvide, prov, block->provides) {
        if (!prov->name || (!prov->func && !prov->memres)) continue;
        b32 seen = 0;
        bh_arr_each(ObjProvide, existing, file->provides) {
            if (bh_strcmp(existing->name, prov->name) != 0) continue;
            if (existing->func != prov->func || existing->memres != prov->memres) {
                onyx_report_error(L->module->context, prov->symbol->pos, Error_Critical,
                    "Symbol '%s' is provided by two different definitions for '%s'.",
                    prov->name, file->path);
                L->failed = 1;
            }
            seen = 1;
            break;
        }
        if (seen) continue;
        ObjProvide entry;
        entry.name = prov->name;
        entry.func = prov->func;
        entry.memres = prov->memres;
        bh_arr_push(file->provides, entry);
    }
}

void onyx_wasm_link_object_files(OnyxWasmModule *module) {
    if (bh_arr_length(module->object_functions) == 0) return;

    Linker linker = { 0 };
    linker.module = module;
    bh_arr_new(module->allocator, linker.files, 4);
    bh_arr_new(module->allocator, linker.work, 8);

    bh_arr_each(AstObjectBlock *, pblock, module->object_blocks) {
        AstObjectBlock *block = *pblock;
        if (!block->filename) continue;
        load_source(&linker, block->filename, block->token->pos);
        if (linker.failed) return;
        bh_arr_each(ObjFile *, pfile, linker.files) {
            if (file_is_source(*pfile, block->filename))
                add_file_provides(&linker, *pfile, block);
        }
    }
    if (linker.failed) return;
    bh_arr_each(AstFunction *, pfunc, module->object_functions) {
        AstFunction *fn = *pfunc;
        if (!fn->object_filename) continue;
        load_source(&linker, fn->object_filename, fn->token->pos);
    }
    if (linker.failed) return;

    bh_arr_each(AstFunction *, pfunc, module->object_functions) {
        AstFunction *fn = *pfunc;
        ObjSymbol *sym = NULL;
        i32 matches = collect_source_function(&linker, fn->object_filename, fn->object_symbol, &sym);
        if (matches > 1) {
            onyx_report_error(module->context, fn->token->pos, Error_Critical,
                "Symbol '%s' is defined more than once in '%s'.", fn->object_symbol, fn->object_filename);
            linker.failed = 1;
            continue;
        }
        if (!sym) {
            b32 loaded = 0;
            bh_arr_each(ObjFile *, pfile, linker.files) {
                if (file_is_source(*pfile, fn->object_filename)) loaded = 1;
            }
            if (!loaded) {
                onyx_report_error(module->context, fn->token->pos, Error_Critical,
                    "Object file '%s' was not loaded.", fn->object_filename);
                return;
            }
            onyx_report_error(module->context, fn->token->pos, Error_Critical,
                "Symbol '%s' was not found in '%s'.", fn->object_symbol, fn->object_filename);
            linker.failed = 1;
            continue;
        }
        ObjFile *file = sym->file;
        u32 imports = (u32) bh_arr_length(file->func_imports);
        if (sym->index < imports || sym->index - imports >= (u32) bh_arr_length(file->funcs)) {
            onyx_report_error(module->context, fn->token->pos, Error_Critical,
                "Symbol '%s' is not a defined function.", fn->object_symbol);
            linker.failed = 1;
            continue;
        }
        ObjFunc *ofn = &file->funcs[sym->index - imports];
        if (ofn->type_idx >= (u32) bh_arr_length(file->types)
            || !signature_matches(module, fn, &file->types[ofn->type_idx], fn->object_symbol)) {
            linker.failed = 1;
            continue;
        }
        sym->live = 0;
        mark_symbol(&linker, sym);
    }
    if (linker.failed) return;
    drain(&linker);
    mark_constructors(&linker);
    if (linker.failed) return;

    bh_arr_each(ObjFile *, pfile, linker.files) {
        bh_arr_each(ObjSymbol, sym, (*pfile)->symbols) {
            if (!sym->live || (sym->flags & SYM_UNDEFINED)) continue;
            if ((sym->flags & SYM_BINDING_MASK) == SYM_BINDING_LOCAL) continue;
            if ((sym->flags & SYM_BINDING_MASK) == SYM_BINDING_WEAK) continue;
            bh_arr_each(ObjFile *, other_file, linker.files) {
                bh_arr_each(ObjSymbol, other, (*other_file)->symbols) {
                    if (other == sym || !other->live || (other->flags & SYM_UNDEFINED)) continue;
                    if ((other->flags & SYM_BINDING_MASK) == SYM_BINDING_LOCAL) continue;
                    if ((other->flags & SYM_BINDING_MASK) == SYM_BINDING_WEAK) continue;
                    if (other->kind == sym->kind && other->name && sym->name && bh_strcmp(other->name, sym->name) == 0) {
                        obj_error(&linker, sym->file, "Duplicate symbol '%s'.", sym->name);
                        return;
                    }
                }
            }
        }
    }

    assign_outputs(&linker);
    if (linker.failed) return;

    bh_arr_each(AstFunction *, pfunc, module->object_functions) {
        AstFunction *fn = *pfunc;
        ObjSymbol *sym = NULL;
        i32 matches = collect_source_function(&linker, fn->object_filename, fn->object_symbol, &sym);
        if (matches == 1 && sym && sym->out_func >= 0)
            bh_imap_put(&module->index_map, (u64) fn, (u64) sym->out_func);
    }

    apply_relocs(&linker);
    if (linker.failed) return;
    finish_tls_image(&linker);
    if (linker.failed) return;
    emit_constructors(&linker);
}

void onyx_wasm_apply_object_memory_patches(OnyxWasmModule *module) {
    bh_arr_each(OnyxObjectMemPatch, patch, module->object_mem_patches) {
        u32 base = patch->extra;
        if (patch->data_id) {
            WasmDatum *datum = &module->data[patch->data_id - 1];
            base = datum->offset_ + patch->extra;
        }
        i32 value = (i32) base + patch->addend;
        if (patch->kind == ONYX_OBJ_PATCH_ULEB5) write_uleb5(patch->bytes + patch->at, (u32) value);
        else if (patch->kind == ONYX_OBJ_PATCH_SLEB5) write_sleb5(patch->bytes + patch->at, value);
        else write_i32(patch->bytes + patch->at, value);
    }
}
