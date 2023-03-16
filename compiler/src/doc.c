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

static void write_cstring(bh_buffer *buffer, const char *data) {
    i32 len = strlen(data);
    bh_buffer_write_u32(buffer, len);
    bh_buffer_append(buffer, data, len);
}

static void write_string(bh_buffer *buffer, i32 len, char *data) {
    bh_buffer_write_u32(buffer, len);
    bh_buffer_append(buffer, data, len);
}

void onyx_docs_emit_odoc(const char *dest) {
    bh_file doc_file;
    if (bh_file_create(&doc_file, dest) != BH_FILE_ERROR_NONE) {
        bh_printf("Cannot create '%s'.\n", dest);
        return;
    }


    bh_buffer doc_buffer;
    bh_buffer_init(&doc_buffer, global_heap_allocator, 16 * 1024);

    bh_buffer_append(&doc_buffer, Doc_Magic_Bytes, 4);
    bh_buffer_write_u32(&doc_buffer, 1);

    const char *program_name = context.options->target_file;
    write_cstring(&doc_buffer, program_name);

    bh_buffer_write_u32(&doc_buffer, bh_time_curr() / 1000);

    int offset_table_index = doc_buffer.length;
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);

    //
    // Package Info
    // 
    *((u32 *) bh_pointer_add(doc_buffer.data, offset_table_index + 0)) = doc_buffer.length;

    Table(Package *) packages = (void *) context.packages;
    bh_buffer_write_u32(&doc_buffer, shlenu(packages));
    fori (i, 0, shlen(packages)) {
        char *package_qualified_name = packages[i].key;
        Package *p = packages[i].value;

        // We want 0-based package id's, because they are going
        // to serve as indicies into the array.
        bh_buffer_write_u32(&doc_buffer, p->id - 1);

        write_cstring(&doc_buffer, p->name);
        write_cstring(&doc_buffer, package_qualified_name);

        bh_buffer_write_u32(&doc_buffer, bh_arr_length(p->sub_packages));
        fori (j, 0, bh_arr_length(p->sub_packages)) {
            bh_buffer_write_u32(&doc_buffer, (u32) p->sub_packages[j]);
        }
    }

    //
    // Entity Info
    //
    *((u32 *) bh_pointer_add(doc_buffer.data, offset_table_index + 4)) = doc_buffer.length;

    bh_buffer_write_u32(&doc_buffer, 0);


    //
    // File Info
    //
    *((u32 *) bh_pointer_add(doc_buffer.data, offset_table_index + 8)) = doc_buffer.length;

    bh_buffer_write_u32(&doc_buffer, 0);


    bh_file_write(&doc_file, doc_buffer.data, doc_buffer.length);
    bh_file_close(&doc_file);
}



