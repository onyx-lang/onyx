#include "doc.h"
#include "utils.h"
#include "types.h"

static i32 sort_tags(const void* a, const void* b) {
    AstNode *n1 = *(AstNode **) a;
    AstNode *n2 = *(AstNode **) b;

    i32 diff;
    if ((diff = strncmp(n1->token->text, n2->token->text, n2->token->length))) {
        return diff;
    }

    return n1->token->length - n2->token->length;
}


void onyx_docs_emit_tags(char *dest) {
    bh_file tags_file;
    if (bh_file_create(&tags_file, dest) != BH_FILE_ERROR_NONE) {
        bh_printf("Cannot create '%s'.\n", dest);
        return;
    }

    bh_fprintf(&tags_file, "!_TAG_FILE_FORMAT\t2\n");
    bh_fprintf(&tags_file, "!_TAG_FILE_SORTED\t1\n");
    bh_fprintf(&tags_file, "!_TAG_OUTPUT_FILESEP\tslash\n");
    bh_fprintf(&tags_file, "!_TAG_OUTPUT_MODE\tu-ctags\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_AUTHOR\tOnyx Compiler\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_NAME\tOnyx Compiler\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_URL\thttps://github.com/onyx-lang/onyx\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_VERSION\t0.1.0\n");

    qsort(context.tag_locations, bh_arr_length(context.tag_locations), sizeof(AstNode *), sort_tags);

    bh_arr_each(AstNode *, pnode, context.tag_locations) {
        AstBinding *node = (AstBinding *) *pnode;
        assert(node->kind == Ast_Kind_Binding);

        i32 line_len = 0;
        char *c = node->token->pos.line_start;
        while (*c++ != '\n') line_len++;

        bh_fprintf(&tags_file, "%b\t%s\t/^%b$/\n",
                node->token->text, node->token->length,
                node->token->pos.filename,
                node->token->pos.line_start, line_len);

    }

    bh_file_close(&tags_file);
}

static i32 sort_symbol_resolutions(const SymbolResolution *a, const SymbolResolution *b) {
    if (a->file_id != b->file_id) {
        return a->file_id > b->file_id ? 1 : -1;
    }

    if (a->line != b->line) {
        return a->line > b->line ? 1 : -1;
    }

    return 0;
}

void onyx_docs_emit_symbol_info(const char *dest) {
    bh_file sym_file;
    if (bh_file_create(&sym_file, dest) != BH_FILE_ERROR_NONE) {
        bh_printf("Cannot create '%s'.\n", dest);
        return;
    }

    SymbolInfoTable *syminfo = context.symbol_info;

    qsort(syminfo->symbols_resolutions,
            bh_arr_length(syminfo->symbols_resolutions),
            sizeof(SymbolResolution),
            (int (*)(const void *, const void*)) sort_symbol_resolutions);

    bh_buffer file_section;
    bh_buffer_init(&file_section, global_heap_allocator, 2048);
    fori (i, 0, shlen(syminfo->files)) {
        char *filename = syminfo->files[i].key;
        u32   file_id  = syminfo->files[i].value;
        bh_buffer_write_u32(&file_section, file_id);
        bh_buffer_write_u32(&file_section, strlen(filename));
        bh_buffer_write_string(&file_section, filename);
    }

    bh_buffer sym_def_section;
    bh_buffer_init(&sym_def_section, global_heap_allocator, 2048);
    bh_arr_each(SymbolInfo, sym, syminfo->symbols) {
        bh_buffer_write_u32(&sym_def_section, sym->id);
        bh_buffer_write_u32(&sym_def_section, sym->file_id);
        bh_buffer_write_u32(&sym_def_section, sym->line);
        bh_buffer_write_u32(&sym_def_section, sym->column);
    }

    bh_buffer sym_res_section;
    bh_buffer_init(&sym_res_section, global_heap_allocator, 2048);
    bh_arr_each(SymbolResolution, sym, syminfo->symbols_resolutions) {
        bh_buffer_write_u32(&sym_res_section, sym->file_id);
        bh_buffer_write_u32(&sym_res_section, sym->line);
        bh_buffer_write_u32(&sym_res_section, sym->column);
        bh_buffer_write_u32(&sym_res_section, sym->length);
        bh_buffer_write_u32(&sym_res_section, sym->symbol_id);
    }

    bh_buffer header_section;
    bh_buffer_init(&header_section, global_heap_allocator, 16);
    bh_buffer_append(&header_section, "OSYM", 4);
    bh_buffer_write_u32(&header_section, 1);
    bh_buffer_write_u32(&header_section, 32);
    bh_buffer_write_u32(&header_section, shlenu(syminfo->files));
    bh_buffer_write_u32(&header_section, 32 + file_section.length);
    bh_buffer_write_u32(&header_section, bh_arr_length(syminfo->symbols));
    bh_buffer_write_u32(&header_section, 32 + file_section.length + sym_def_section.length);
    bh_buffer_write_u32(&header_section, bh_arr_length(syminfo->symbols_resolutions));

    bh_file_write(&sym_file, header_section.data, header_section.length);
    bh_file_write(&sym_file, file_section.data, file_section.length);
    bh_file_write(&sym_file, sym_def_section.data, sym_def_section.length);
    bh_file_write(&sym_file, sym_res_section.data, sym_res_section.length);

    bh_file_close(&sym_file);

    bh_buffer_free(&header_section);
    bh_buffer_free(&file_section);
    bh_buffer_free(&sym_def_section);
    bh_buffer_free(&sym_res_section);


    bh_arr_free(syminfo->symbols);
    bh_arr_free(syminfo->symbols_resolutions);
    shfree(syminfo->files);
    bh_imap_free(&syminfo->node_to_id);
}



//
// Onyx Documentation Format
//

#define Doc_Magic_Bytes "ODOC"


struct Doc_String {
    u32 offset;
    u32 length;
};

struct Doc_Array {
    u32 offset;
    u32 length;
};

struct Doc_Header {
    u8  magic_bytes[4];
    u32 version;

    struct Doc_String program_name;
    u32 build_time;
};

struct Doc_File {
    u32 package_id;
    struct Doc_String name;
};

struct Doc_Location {
    u32 file_id;
    i32 line;
    i32 col;
};

struct Doc_Package {
    struct Doc_String name;
    struct Doc_String qualified_name;
    
    struct Doc_Array subpackages;
};

enum Doc_Entity_Kind {
    Entity_Kind_Procedure = 1,
};

struct Doc_Entity {
    enum Doc_Entity_Kind kind;

    u32 package_id;
    struct Doc_Location location;

    struct Doc_String name;
    struct Doc_String notes;
};

struct Doc {
    struct Doc_Header header;

    struct Doc_Array packages;
    struct Doc_Array entities;
    struct Doc_Array files;
};


typedef struct DocGenerator {
    bh_buffer string_buffer;
} DocGenerator;

void doc_gen_init(DocGenerator *gen) {
    bh_buffer_init(&gen->string_buffer, global_heap_allocator, 256);
}

void doc_gen_deinit(DocGenerator *gen) {
    bh_buffer_free(&gen->string_buffer);
}

u32 doc_gen_add_string(DocGenerator *gen, char *data, u32 len) {
    u32 offset = gen->string_buffer.length;

    bh_buffer_append(&gen->string_buffer, data, len);

    return offset;
}

void onyx_docs_emit_odoc(const char *dest) {
    bh_file doc_file;
    if (bh_file_create(&doc_file, dest) != BH_FILE_ERROR_NONE) {
        bh_printf("Cannot create '%s'.\n", dest);
        return;
    }

    DocGenerator gen;
    doc_gen_init(&gen);

    struct Doc final_doc;

    memcpy(final_doc.header.magic_bytes, Doc_Magic_Bytes, 4);
    final_doc.header.version = 1;

    char *program_name = context.options->target_file;
    u32 program_name_len = strlen(program_name);
    final_doc.header.program_name.offset = doc_gen_add_string(&gen, program_name, program_name_len);
    final_doc.header.program_name.length = program_name_len;

    final_doc.header.build_time = bh_time_curr() / 1000;

    final_doc.packages.offset = 0;
    final_doc.packages.length = 0;

    final_doc.entities.offset = 0;
    final_doc.entities.length = 0;

    final_doc.files.offset = 0;
    final_doc.files.length = 0;

    bh_file_write(&doc_file, &final_doc, sizeof(struct Doc));

    doc_gen_deinit(&gen);
    bh_file_close(&doc_file);
}



