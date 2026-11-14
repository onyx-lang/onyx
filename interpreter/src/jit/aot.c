#include "emit.h"
#include "ovm_wasm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *helper_c_names[OVM_JIT_H_COUNT] = {
    "ovm_jit_return",
    "ovm_jit_call_fast",
    "ovm_jit_calli_fast",
    "ovm_jit_slow_idx",
    "memmove",
    "memset",
};

typedef struct {
    u8 *data;
    i32 len;
    i32 cap;
} objbuf_t;

static void ob_init(objbuf_t *b) { memset(b, 0, sizeof(*b)); }

static void ob_free(objbuf_t *b) {
    free(b->data);
    memset(b, 0, sizeof(*b));
}

static void ob_need(objbuf_t *b, i32 extra) {
    i32 need = b->len + extra;
    if (need <= b->cap) return;
    i32 cap = b->cap ? b->cap : 4096;
    while (cap < need) cap *= 2;
    b->data = realloc(b->data, (size_t) cap);
    b->cap = cap;
}

static void ob_bytes(objbuf_t *b, const void *p, i32 n) {
    ob_need(b, n);
    memcpy(b->data + b->len, p, (size_t) n);
    b->len += n;
}

static void ob_u8(objbuf_t *b, u8 v) { ob_bytes(b, &v, 1); }
static void ob_u16(objbuf_t *b, u16 v) { ob_bytes(b, &v, 2); }
static void ob_u32(objbuf_t *b, u32 v) { ob_bytes(b, &v, 4); }
static void ob_u64(objbuf_t *b, u64 v) { ob_bytes(b, &v, 8); }
static void ob_i32(objbuf_t *b, i32 v) { ob_bytes(b, &v, 4); }
static void ob_i64(objbuf_t *b, i64 v) { ob_bytes(b, &v, 8); }

static void ob_align(objbuf_t *b, i32 align) {
    while (b->len & (align - 1)) ob_u8(b, 0);
}

static void ob_zeros(objbuf_t *b, i32 n) {
    ob_need(b, n);
    memset(b->data + b->len, 0, (size_t) n);
    b->len += n;
}

static i32 ob_str(objbuf_t *b, const char *s) {
    i32 at = b->len;
    ob_bytes(b, s, (i32) strlen(s) + 1);
    return at;
}

typedef struct {
    ovm_emit_buf_t *buf;
    i32 *entry_off;
    i32 *body_abs;
    i32 nfuncs;
    const u8 *wasm;
    u32 wasm_len;
    i32 ro_len_off;
    i32 ro_wasm_off;
    i32 ro_count_off;
    i32 ro_entry_off;
    i32 ro_body_off;
    i32 ro_size;
} aot_img_t;

static void aot_pack_rodata(aot_img_t *img, objbuf_t *ro) {
    img->ro_len_off = ro->len;
    ob_u32(ro, img->wasm_len);
    img->ro_wasm_off = ro->len;
    ob_bytes(ro, img->wasm, (i32) img->wasm_len);
    ob_align(ro, 4);
    img->ro_count_off = ro->len;
    ob_i32(ro, img->nfuncs);
    img->ro_entry_off = ro->len;
    for (i32 i = 0; i < img->nfuncs; i++) ob_i32(ro, img->entry_off[i]);
    img->ro_body_off = ro->len;
    for (i32 i = 0; i < img->nfuncs; i++) ob_i32(ro, img->body_abs[i]);
    img->ro_size = ro->len;
}

#if !defined(__APPLE__)

#define ELF64_ST_INFO(b, t) (((b) << 4) + ((t) & 0xf))
#define ELF64_R_INFO(s, t)  (((u64) (s) << 32) + (u64) (t))
#define STB_GLOBAL 1
#define STT_NOTYPE 0
#define STT_FUNC   2
#define STT_OBJECT 1
#define SHN_UNDEF  0
#define SHT_NULL   0
#define SHT_PROGBITS 1
#define SHT_SYMTAB 2
#define SHT_STRTAB 3
#define SHT_RELA   4
#define SHF_WRITE  1
#define SHF_ALLOC  2
#define SHF_EXECINSTR 4
#define SHF_INFO_LINK 0x40
#define ET_REL 1
#define EM_X86_64 62
#define EM_AARCH64 183
#define EV_CURRENT 1
#define ELFCLASS64 2
#define ELFDATA2LSB 1
#define ELFOSABI_NONE 0
#define R_X86_64_64   1
#define R_X86_64_PC32 2
#define R_AARCH64_ABS64 257
#define R_AARCH64_ADR_PREL_PG_HI21 275
#define R_AARCH64_LDST64_ABS_LO12_NC 286

static int aot_write_elf64(aot_img_t *img, const char *path) {
    objbuf_t ro;
    ob_init(&ro);
    aot_pack_rodata(img, &ro);

    objbuf_t str;
    ob_init(&str);
    ob_u8(&str, 0);
    i32 n_text = ob_str(&str, "onyx_jit_text");
    i32 n_help = ob_str(&str, "onyx_helpers");
    i32 n_wlen = ob_str(&str, "onyx_wasm_len");
    i32 n_wasm = ob_str(&str, "onyx_wasm");
    i32 n_fcnt = ob_str(&str, "onyx_func_count");
    i32 n_fent = ob_str(&str, "onyx_func_entry_off");
    i32 n_fbod = ob_str(&str, "onyx_func_body_off");
    i32 n_help_sym[OVM_JIT_H_COUNT];
    for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) n_help_sym[i] = ob_str(&str, helper_c_names[i]);

    objbuf_t shstr;
    ob_init(&shstr);
    ob_u8(&shstr, 0);
    i32 ns_text = ob_str(&shstr, ".text");
    i32 ns_data = ob_str(&shstr, ".data");
    i32 ns_ro = ob_str(&shstr, ".rodata");
    i32 ns_rt = ob_str(&shstr, ".rela.text");
    i32 ns_rd = ob_str(&shstr, ".rela.data");
    i32 ns_sym = ob_str(&shstr, ".symtab");
    i32 ns_str = ob_str(&shstr, ".strtab");
    i32 ns_shs = ob_str(&shstr, ".shstrtab");

    /* symbol 0 undef; 1 text; 2 helpers; 3-7 rodata; 8+ helpers undef */
    enum { SYM_TEXT = 1, SYM_HELP = 2, SYM_H0 = 8 };

    objbuf_t rel_text;
    ob_init(&rel_text);
    i32 nh = bh_arr_length(img->buf->helper_relocs);
    for (i32 i = 0; i < nh; i++) {
        ovm_helper_reloc_t r = img->buf->helper_relocs[i];
        u32 type = 0;
        i64 addend = 0;
#if defined(__x86_64__)
        type = R_X86_64_PC32;
        addend = (i64) r.helper * 8 - 4;
#elif defined(__aarch64__) || defined(__arm64__)
        if (r.kind == OVM_HRELOC_A64_ADRP) {
            type = R_AARCH64_ADR_PREL_PG_HI21;
            addend = (i64) r.helper * 8;
        } else {
            type = R_AARCH64_LDST64_ABS_LO12_NC;
            addend = (i64) r.helper * 8;
        }
#endif
        ob_u64(&rel_text, (u64) r.at);
        ob_u64(&rel_text, ELF64_R_INFO(SYM_HELP, type));
        ob_i64(&rel_text, addend);
    }

    objbuf_t rel_data;
    ob_init(&rel_data);
#if defined(__x86_64__)
    u32 abs64 = R_X86_64_64;
#else
    u32 abs64 = R_AARCH64_ABS64;
#endif
    for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) {
        ob_u64(&rel_data, (u64) i * 8);
        ob_u64(&rel_data, ELF64_R_INFO(SYM_H0 + i, abs64));
        ob_i64(&rel_data, 0);
    }

    objbuf_t sym;
    ob_init(&sym);
    ob_zeros(&sym, 24); /* undef */

    #define SYM(name, val, sz, info, sh) do { \
        ob_u32(&sym, (u32) (name)); \
        ob_u8(&sym, (u8) (info)); \
        ob_u8(&sym, 0); \
        ob_u16(&sym, (u16) (sh)); \
        ob_u64(&sym, (u64) (val)); \
        ob_u64(&sym, (u64) (sz)); \
    } while (0)

    SYM(n_text, 0, img->buf->len, ELF64_ST_INFO(STB_GLOBAL, STT_FUNC), 1);
    SYM(n_help, 0, OVM_JIT_H_COUNT * 8, ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT), 2);
    SYM(n_wlen, img->ro_len_off, 4, ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT), 3);
    SYM(n_wasm, img->ro_wasm_off, img->wasm_len, ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT), 3);
    SYM(n_fcnt, img->ro_count_off, 4, ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT), 3);
    SYM(n_fent, img->ro_entry_off, img->nfuncs * 4, ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT), 3);
    SYM(n_fbod, img->ro_body_off, img->nfuncs * 4, ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT), 3);
    for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) {
        SYM(n_help_sym[i], 0, 0, ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE), SHN_UNDEF);
    }
    #undef SYM

    u16 nsh = 9;
    i32 ehsize = 64;

    objbuf_t out;
    ob_init(&out);
    ob_zeros(&out, ehsize);

    i32 off_text = out.len;
    ob_bytes(&out, img->buf->data, img->buf->len);
    ob_align(&out, 8);

    i32 off_data = out.len;
    ob_zeros(&out, OVM_JIT_H_COUNT * 8);

    i32 off_ro = out.len;
    ob_bytes(&out, ro.data, ro.len);
    ob_align(&out, 8);

    i32 off_rt = out.len;
    ob_bytes(&out, rel_text.data, rel_text.len);
    ob_align(&out, 8);

    i32 off_rd = out.len;
    ob_bytes(&out, rel_data.data, rel_data.len);
    ob_align(&out, 8);

    i32 off_sym = out.len;
    ob_bytes(&out, sym.data, sym.len);
    ob_align(&out, 8);

    i32 off_str = out.len;
    ob_bytes(&out, str.data, str.len);
    ob_align(&out, 8);

    i32 off_shs = out.len;
    ob_bytes(&out, shstr.data, shstr.len);
    ob_align(&out, 8);

    i32 off_sh = out.len;

    #define SHDR(name, type, flags, addr, off, sz, link, info, align, entsz) do { \
        ob_u32(&out, (u32) (name)); \
        ob_u32(&out, (u32) (type)); \
        ob_u64(&out, (u64) (flags)); \
        ob_u64(&out, (u64) (addr)); \
        ob_u64(&out, (u64) (off)); \
        ob_u64(&out, (u64) (sz)); \
        ob_u32(&out, (u32) (link)); \
        ob_u32(&out, (u32) (info)); \
        ob_u64(&out, (u64) (align)); \
        ob_u64(&out, (u64) (entsz)); \
    } while (0)

    SHDR(0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);
    SHDR(ns_text, SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 0, off_text, img->buf->len, 0, 0, 16, 0);
    SHDR(ns_data, SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, 0, off_data, OVM_JIT_H_COUNT * 8, 0, 0, 8, 0);
    SHDR(ns_ro, SHT_PROGBITS, SHF_ALLOC, 0, off_ro, ro.len, 0, 0, 4, 0);
    SHDR(ns_rt, SHT_RELA, SHF_INFO_LINK, 0, off_rt, rel_text.len, 6, 1, 8, 24);
    SHDR(ns_rd, SHT_RELA, SHF_INFO_LINK, 0, off_rd, rel_data.len, 6, 2, 8, 24);
    SHDR(ns_sym, SHT_SYMTAB, 0, 0, off_sym, sym.len, 7, 1, 8, 24);
    SHDR(ns_str, SHT_STRTAB, 0, 0, off_str, str.len, 0, 0, 1, 0);
    SHDR(ns_shs, SHT_STRTAB, 0, 0, off_shs, shstr.len, 0, 0, 1, 0);
    #undef SHDR

    u16 machine;
#if defined(__x86_64__)
    machine = EM_X86_64;
#else
    machine = EM_AARCH64;
#endif

    /* ELF header */
    u8 *eh = out.data;
    eh[0] = 0x7f; eh[1] = 'E'; eh[2] = 'L'; eh[3] = 'F';
    eh[4] = ELFCLASS64;
    eh[5] = ELFDATA2LSB;
    eh[6] = EV_CURRENT;
    eh[7] = ELFOSABI_NONE;
    memcpy(eh + 16, &(u16){ ET_REL }, 2);
    memcpy(eh + 18, &machine, 2);
    memcpy(eh + 20, &(u32){ EV_CURRENT }, 4);
    memset(eh + 24, 0, 16); /* entry, phoff */
    memcpy(eh + 40, &(u64){ (u64) off_sh }, 8);
    memset(eh + 48, 0, 4); /* flags */
    memcpy(eh + 52, &(u16){ 64 }, 2);
    memcpy(eh + 54, &(u16){ 0 }, 2);
    memcpy(eh + 56, &(u16){ 0 }, 2);
    memcpy(eh + 58, &(u16){ 64 }, 2);
    memcpy(eh + 60, &nsh, 2);
    memcpy(eh + 62, &(u16){ 8 }, 2);

    FILE *f = fopen(path, "wb");
    int ok = 0;
    if (f) {
        ok = fwrite(out.data, 1, (size_t) out.len, f) == (size_t) out.len;
        fclose(f);
    }

    ob_free(&ro);
    ob_free(&str);
    ob_free(&shstr);
    ob_free(&rel_text);
    ob_free(&rel_data);
    ob_free(&sym);
    ob_free(&out);
    return ok;
}

#else /* Apple Mach-O */

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#if defined(__x86_64__)
#include <mach-o/x86_64/reloc.h>
#endif
#if defined(__aarch64__) || defined(__arm64__)
#include <mach-o/arm64/reloc.h>
#endif

static int aot_write_macho64(aot_img_t *img, const char *path) {
    objbuf_t ro;
    ob_init(&ro);
    aot_pack_rodata(img, &ro);

    objbuf_t str;
    ob_init(&str);
    ob_u8(&str, 0);
    #define MSTR(lit) ob_str(&str, "_" lit)
    i32 n_text = MSTR("onyx_jit_text");
    i32 n_help = MSTR("onyx_helpers");
    i32 n_wlen = MSTR("onyx_wasm_len");
    i32 n_wasm = MSTR("onyx_wasm");
    i32 n_fcnt = MSTR("onyx_func_count");
    i32 n_fent = MSTR("onyx_func_entry_off");
    i32 n_fbod = MSTR("onyx_func_body_off");
    i32 n_h[OVM_JIT_H_COUNT];
    for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) {
        char tmp[64];
        snprintf(tmp, sizeof tmp, "_%s", helper_c_names[i]);
        n_h[i] = ob_str(&str, tmp);
    }
    #undef MSTR

    objbuf_t trel;
    ob_init(&trel);
    i32 nh = bh_arr_length(img->buf->helper_relocs);
    for (i32 i = 0; i < nh; i++) {
        ovm_helper_reloc_t r = img->buf->helper_relocs[i];
#if defined(__x86_64__)
        i32 addend = r.helper * 8;
        memcpy(img->buf->data + r.at, &addend, 4);
        struct relocation_info rel;
        memset(&rel, 0, sizeof rel);
        rel.r_address = r.at;
        rel.r_symbolnum = 1; /* patched to onyx_helpers */
        rel.r_pcrel = 1;
        rel.r_length = 2;
        rel.r_extern = 1;
        rel.r_type = X86_64_RELOC_SIGNED;
        ob_bytes(&trel, &rel, (i32) sizeof rel);
#elif defined(__aarch64__) || defined(__arm64__)
        /* ADDEND then PAGE21 / PAGEOFF12. symbol = onyx_helpers (index 1) */
        struct relocation_info add, rel;
        memset(&add, 0, sizeof add);
        add.r_address = r.at;
        add.r_symbolnum = (unsigned) (r.helper * 8);
        add.r_pcrel = 0;
        add.r_length = 2;
        add.r_extern = 0;
        add.r_type = ARM64_RELOC_ADDEND;
        memset(&rel, 0, sizeof rel);
        rel.r_address = r.at;
        rel.r_symbolnum = 1; /* patched to onyx_helpers index */
        rel.r_length = 2;
        rel.r_extern = 1;
        if (r.kind == OVM_HRELOC_A64_ADRP) {
            rel.r_pcrel = 1;
            rel.r_type = ARM64_RELOC_PAGE21;
        } else {
            rel.r_pcrel = 0;
            rel.r_type = ARM64_RELOC_PAGEOFF12;
        }
        ob_bytes(&trel, &add, (i32) sizeof add);
        ob_bytes(&trel, &rel, (i32) sizeof rel);
#endif
    }

    objbuf_t drel;
    ob_init(&drel);
    for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) {
        struct relocation_info rel;
        memset(&rel, 0, sizeof rel);
        rel.r_address = i * 8;
        rel.r_symbolnum = 7 + i; /* first undef after 7 defined */
        rel.r_pcrel = 0;
        rel.r_length = 3;
        rel.r_extern = 1;
#if defined(__x86_64__)
        rel.r_type = X86_64_RELOC_UNSIGNED;
#else
        rel.r_type = ARM64_RELOC_UNSIGNED;
#endif
        ob_bytes(&drel, &rel, (i32) sizeof rel);
    }

    /* Defined ext: 0 text, 1 helpers, 2-6 rodata. Undef: 7+ helpers */
    enum { MO_HELP = 1, MO_H0 = 7 };

    /* patch text reloc symbol numbers to MO_HELP */
    {
        i32 nrel = trel.len / (i32) sizeof(struct relocation_info);
        struct relocation_info *rs = (struct relocation_info *) trel.data;
        for (i32 i = 0; i < nrel; i++) {
            if (rs[i].r_extern) rs[i].r_symbolnum = MO_HELP;
        }
    }
    {
        struct relocation_info *rs = (struct relocation_info *) drel.data;
        for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) rs[i].r_symbolnum = MO_H0 + i;
    }

    i32 vm_text = 0;
    i32 vm_data = (img->buf->len + 7) & ~7;
    i32 vm_ro = (vm_data + OVM_JIT_H_COUNT * 8 + 3) & ~3;
    i32 vm_end = (vm_ro + ro.len + 7) & ~7;

    objbuf_t nlist;
    ob_init(&nlist);
    #define NL(strx, type, sect, val) do { \
        struct nlist_64 nl; \
        memset(&nl, 0, sizeof nl); \
        nl.n_un.n_strx = (uint32_t) (strx); \
        nl.n_type = (uint8_t) (type); \
        nl.n_sect = (uint8_t) (sect); \
        nl.n_value = (uint64_t) (val); \
        ob_bytes(&nlist, &nl, (i32) sizeof nl); \
    } while (0)

    NL(n_text, N_EXT | N_SECT, 1, vm_text);
    NL(n_help, N_EXT | N_SECT, 2, vm_data);
    NL(n_wlen, N_EXT | N_SECT, 3, vm_ro + img->ro_len_off);
    NL(n_wasm, N_EXT | N_SECT, 3, vm_ro + img->ro_wasm_off);
    NL(n_fcnt, N_EXT | N_SECT, 3, vm_ro + img->ro_count_off);
    NL(n_fent, N_EXT | N_SECT, 3, vm_ro + img->ro_entry_off);
    NL(n_fbod, N_EXT | N_SECT, 3, vm_ro + img->ro_body_off);
    for (i32 i = 0; i < OVM_JIT_H_COUNT; i++) NL(n_h[i], N_EXT | N_UNDF, 0, 0);
    #undef NL

    i32 nsyms = 7 + OVM_JIT_H_COUNT;

    /* Layout: header + load commands, then section data, relocs, nlist, strtab */
    u32 ncmds = 4;
    u32 sizeofcmds = (u32) (sizeof(struct segment_command_64) + 3 * sizeof(struct section_64)
                            + sizeof(struct symtab_command)
                            + sizeof(struct dysymtab_command)
                            + sizeof(struct build_version_command));

    i32 hdr_sz = (i32) (sizeof(struct mach_header_64) + sizeofcmds);
    i32 off_text = hdr_sz;
    i32 off_data = off_text + img->buf->len;
    while (off_data & 7) off_data++;
    i32 off_ro = off_data + OVM_JIT_H_COUNT * 8;
    i32 off_trel = off_ro + ro.len;
    while (off_trel & 3) off_trel++;
    i32 off_drel = off_trel + trel.len;
    i32 off_sym = off_drel + drel.len;
    while (off_sym & 7) off_sym++;
    i32 off_str = off_sym + nlist.len;

    objbuf_t out;
    ob_init(&out);

    struct mach_header_64 mh;
    memset(&mh, 0, sizeof mh);
    mh.magic = MH_MAGIC_64;
#if defined(__x86_64__)
    mh.cputype = CPU_TYPE_X86_64;
    mh.cpusubtype = CPU_SUBTYPE_X86_64_ALL;
#else
    mh.cputype = CPU_TYPE_ARM64;
    mh.cpusubtype = CPU_SUBTYPE_ARM64_ALL;
#endif
    mh.filetype = MH_OBJECT;
    mh.ncmds = ncmds;
    mh.sizeofcmds = sizeofcmds;
    mh.flags = 0;
    ob_bytes(&out, &mh, (i32) sizeof mh);

    struct segment_command_64 sg;
    memset(&sg, 0, sizeof sg);
    sg.cmd = LC_SEGMENT_64;
    sg.cmdsize = (uint32_t) (sizeof(sg) + 3 * sizeof(struct section_64));
    sg.nsects = 3;
    sg.vmaddr = 0;
    sg.vmsize = (uint64_t) vm_end;
    sg.fileoff = (uint64_t) off_text;
    sg.filesize = (uint64_t) (off_trel - off_text);
    sg.maxprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
    sg.initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE;
    ob_bytes(&out, &sg, (i32) sizeof sg);

    struct section_64 s_text, s_data, s_ro;
    memset(&s_text, 0, sizeof s_text);
    memcpy(s_text.sectname, "__text", 6);
    memcpy(s_text.segname, "__TEXT", 6);
    s_text.addr = (uint64_t) vm_text;
    s_text.size = (uint64_t) img->buf->len;
    s_text.offset = (uint32_t) off_text;
    s_text.align = 4;
    s_text.reloff = (uint32_t) off_trel;
    s_text.nreloc = (uint32_t) (trel.len / (i32) sizeof(struct relocation_info));
    s_text.flags = S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS;

    memset(&s_data, 0, sizeof s_data);
    memcpy(s_data.sectname, "__data", 6);
    memcpy(s_data.segname, "__DATA", 6);
    s_data.addr = (uint64_t) vm_data;
    s_data.size = (uint64_t) (OVM_JIT_H_COUNT * 8);
    s_data.offset = (uint32_t) off_data;
    s_data.align = 3;
    s_data.reloff = (uint32_t) off_drel;
    s_data.nreloc = (uint32_t) OVM_JIT_H_COUNT;

    memset(&s_ro, 0, sizeof s_ro);
    memcpy(s_ro.sectname, "__const", 7);
    memcpy(s_ro.segname, "__TEXT", 6);
    s_ro.addr = (uint64_t) vm_ro;
    s_ro.size = (uint64_t) ro.len;
    s_ro.offset = (uint32_t) off_ro;
    s_ro.align = 2;

    ob_bytes(&out, &s_text, (i32) sizeof s_text);
    ob_bytes(&out, &s_data, (i32) sizeof s_data);
    ob_bytes(&out, &s_ro, (i32) sizeof s_ro);

    struct symtab_command st;
    memset(&st, 0, sizeof st);
    st.cmd = LC_SYMTAB;
    st.cmdsize = (uint32_t) sizeof st;
    st.symoff = (uint32_t) off_sym;
    st.nsyms = (uint32_t) nsyms;
    st.stroff = (uint32_t) off_str;
    st.strsize = (uint32_t) str.len;
    ob_bytes(&out, &st, (i32) sizeof st);

    struct dysymtab_command dy;
    memset(&dy, 0, sizeof dy);
    dy.cmd = LC_DYSYMTAB;
    dy.cmdsize = (uint32_t) sizeof dy;
    dy.iextdefsym = 0;
    dy.nextdefsym = 7;
    dy.iundefsym = 7;
    dy.nundefsym = (uint32_t) OVM_JIT_H_COUNT;
    ob_bytes(&out, &dy, (i32) sizeof dy);

    struct build_version_command bv;
    memset(&bv, 0, sizeof bv);
    bv.cmd = LC_BUILD_VERSION;
    bv.cmdsize = (uint32_t) sizeof bv;
    bv.platform = PLATFORM_MACOS;
    bv.minos = 0x000b0000; /* 11.0 */
    bv.sdk = 0x000b0000;
    bv.ntools = 0;
    ob_bytes(&out, &bv, (i32) sizeof bv);

    while (out.len < off_text) ob_u8(&out, 0);
    ob_bytes(&out, img->buf->data, img->buf->len);
    while (out.len < off_data) ob_u8(&out, 0);
    ob_zeros(&out, OVM_JIT_H_COUNT * 8);
    while (out.len < off_ro) ob_u8(&out, 0);
    ob_bytes(&out, ro.data, ro.len);
    while (out.len < off_trel) ob_u8(&out, 0);
    ob_bytes(&out, trel.data, trel.len);
    while (out.len < off_drel) ob_u8(&out, 0);
    ob_bytes(&out, drel.data, drel.len);
    while (out.len < off_sym) ob_u8(&out, 0);
    ob_bytes(&out, nlist.data, nlist.len);
    while (out.len < off_str) ob_u8(&out, 0);
    ob_bytes(&out, str.data, str.len);

    FILE *f = fopen(path, "wb");
    int ok = 0;
    if (f) {
        ok = fwrite(out.data, 1, (size_t) out.len, f) == (size_t) out.len;
        fclose(f);
    }

    ob_free(&ro);
    ob_free(&str);
    ob_free(&trel);
    ob_free(&drel);
    ob_free(&nlist);
    ob_free(&out);
    return ok;
}

#endif

static int ovm_jit_aot_write_object(ovm_program_t *program, const u8 *wasm, u32 wasm_len, const char *path) {
    i32 nfuncs = bh_arr_length(program->funcs);
    if (nfuncs <= 0) return 0;

    ovm_emit_buf_t buf;
    ovm_emit_init(&buf);
    buf.aot = 1;

    i32 *entry = calloc((size_t) nfuncs, sizeof(i32));
    i32 *body = calloc((size_t) nfuncs, sizeof(i32));
    if (!entry || !body) {
        free(entry);
        free(body);
        ovm_emit_free(&buf);
        return 0;
    }

    if (!ovm_jit_emit_functions(program, &buf, entry, body)) {
        free(entry);
        free(body);
        ovm_emit_free(&buf);
        return 0;
    }

    i32 *body_abs = calloc((size_t) nfuncs, sizeof(i32));
    if (!body_abs) {
        free(entry);
        free(body);
        ovm_emit_free(&buf);
        return 0;
    }
    for (i32 i = 0; i < nfuncs; i++) {
        if (entry[i] < 0) body_abs[i] = -1;
        else body_abs[i] = entry[i] + body[i];
    }

    aot_img_t img;
    memset(&img, 0, sizeof img);
    img.buf = &buf;
    img.entry_off = entry;
    img.body_abs = body_abs;
    img.nfuncs = nfuncs;
    img.wasm = wasm;
    img.wasm_len = wasm_len;

#if defined(__APPLE__)
    int ok = aot_write_macho64(&img, path);
#else
    int ok = aot_write_elf64(&img, path);
#endif

    free(entry);
    free(body);
    free(body_abs);
    ovm_emit_free(&buf);
    return ok;
}

int ovm_native_compile_wasm(const void *wasm, u32 wasm_len, const char *obj_path) {
    if (!wasm || wasm_len == 0 || !obj_path) return 0;

#if !defined(__aarch64__) && !defined(__arm64__) && !defined(__x86_64__)
    return 0;
#endif

    wasm_engine_t *engine = wasm_engine_new();
    if (!engine) return 0;
    wasm_store_t *store = wasm_store_new(engine);
    if (!store) {
        wasm_engine_delete(engine);
        return 0;
    }

    wasm_byte_vec_t bytes;
    bytes.size = wasm_len;
    bytes.data = (wasm_byte_t *) wasm;
    wasm_module_t *mod = wasm_module_new(store, &bytes);
    int ok = 0;
    if (mod && mod->program) {
        ok = ovm_jit_aot_write_object(mod->program, (const u8 *) wasm, wasm_len, obj_path);
    }
    if (mod) wasm_module_delete(mod);
    wasm_store_delete(store);
    wasm_engine_delete(engine);
    return ok;
}
