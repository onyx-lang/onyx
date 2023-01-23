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

void onyx_docs_emit_symbol_info(const char *dest) {
    bh_file sym_file;
    if (bh_file_create(&sym_file, dest) != BH_FILE_ERROR_NONE) {
        bh_printf("Cannot create '%s'.\n", dest);
        return;
    }

    SymbolInfoTable *syminfo = context.symbol_info;

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

// static i32 cmp_doc_entry(const void * a, const void * b) {
//     DocEntry* d1 = (DocEntry *) a;
//     DocEntry* d2 = (DocEntry *) b;
// 
//     return strncmp(d1->def, d2->def, 1024);
// }
// 
// static i32 cmp_doc_package(const void * a, const void * b) {
//     DocPackage* d1 = (DocPackage *) a;
//     DocPackage* d2 = (DocPackage *) b;
// 
//     return strncmp(d1->name, d2->name, 1024);
// }
// 
// static char* node_to_doc_def(const char* sym, AstNode *node, bh_allocator a) {
//     static char buf[1024];
//     memset(buf, 0, 1023);
// 
//     strncat(buf, sym, 1023);
//     strncat(buf, " :: ", 1023);
// 
//     switch (node->kind) {
//         case Ast_Kind_Function: {
//             strncat(buf, "(", 1023);
// 
//             AstFunction *func = (AstFunction *) node;
//             bh_arr_each(AstParam, param, func->params) {
//                 if (param != func->params)
//                     strncat(buf, ", ", 1023);
// 
//                 token_toggle_end(param->local->token);
//                 strncat(buf, param->local->token->text, 1023);
//                 token_toggle_end(param->local->token);
// 
//                 strncat(buf, ": ", 1023);
// 
//                 strncat(buf, type_get_name(param->local->type), 1023);
// 
//                 if (param->default_value != NULL) {
//                     strncat(buf, " = <default>", 1023);
//                 }
//             }
// 
//             strncat(buf, ") -> ", 1023);
//             strncat(buf, type_get_name(func->type->Function.return_type), 1023);
// 
//             break;
//         }
// 
//         case Ast_Kind_Struct_Type: {
//             strncat(buf, "struct { ", 1023);
// 
//             AstStructType* st = (AstStructType *) node;
//             bh_arr_each(AstStructMember *, smem, st->members) {
// 
//                 token_toggle_end((*smem)->token);
//                 strncat(buf, (*smem)->token->text, 1023);
//                 token_toggle_end((*smem)->token);
// 
//                 strncat(buf, ": ", 1023);
// 
//                 strncat(buf, type_get_name((*smem)->type), 1023);
// 
//                 strncat(buf, "; ", 1023);
//             }
// 
//             strncat(buf, "}", 1023);
//             break;
//         }
// 
//         case Ast_Kind_Basic_Type:
//         case Ast_Kind_Array_Type:
//         case Ast_Kind_Type_Alias:
//         case Ast_Kind_Slice_Type:
//         case Ast_Kind_DynArr_Type: {
//             strncat(buf, type_get_name(type_build_from_ast(global_heap_allocator, (AstType *) node)), 1023);
//             break;
//         }
// 
//         default: {
//             strncat(buf, "<unimplemented printing>", 1023);
//         }
//     }
// 
//     return bh_strdup(a, buf);
// }
// 
// static DocPackage doc_package_create(Package* p, bh_allocator a) {
//     DocPackage dp;
//     dp.name = p->name;
//     dp.public_entries = NULL;
//     dp.private_entries = NULL;
// 
//     bh_arr_new(global_heap_allocator, dp.public_entries, 16);
//     bh_arr_new(global_heap_allocator, dp.private_entries, 16);
// 
//     fori (i, 0, shlen(p->scope->symbols)) {
//         char *key = p->scope->symbols[i].key;
//         AstNode *value = p->scope->symbols[i].value;
//         if (!key || !value) continue;
// 
//         DocEntry de;
//         if (value->token) de.pos = value->token->pos;
//         de.def = node_to_doc_def(key, value, a);
//         de.sym = (char *) key;
//         de.additional = NULL;
// 
//         bh_arr_push(dp.public_entries, de);
//     }
// 
//     fori (i, 0, shlen(p->private_scope->symbols)) {
//         char *key = p->scope->symbols[i].key;
//         AstNode *value = p->scope->symbols[i].value;
//         if (!key || !value) continue;
// 
//         DocEntry de;
//         if (value->token) de.pos = value->token->pos;
//         de.def = node_to_doc_def(key, value, a);
//         de.sym = (char *) key;
//         de.additional = NULL;
// 
//         bh_arr_push(dp.private_entries, de);
//     }
// 
//     qsort(dp.public_entries,  bh_arr_length(dp.public_entries),  sizeof(DocEntry), cmp_doc_entry);
//     qsort(dp.private_entries, bh_arr_length(dp.private_entries), sizeof(DocEntry), cmp_doc_entry);
// 
//     return dp;
// }
// 
// OnyxDocumentation onyx_docs_generate() {
//     OnyxDocumentation doc;
// 
//     bh_arena_init(&doc.doc_arena, global_heap_allocator, 16 * 1024);
//     bh_allocator a = bh_arena_allocator(&doc.doc_arena);
// 
//     doc.package_docs = NULL;
//     bh_arr_new(global_heap_allocator, doc.package_docs, 16);
// 
//     fori (i, 0, shlen(context.packages)) {
//         DocPackage dp = doc_package_create(context.packages[i].value, a);
//         bh_arr_push(doc.package_docs, dp);
//     }
// 
//     qsort(doc.package_docs, bh_arr_length(doc.package_docs), sizeof(DocPackage), cmp_doc_package);
// 
//     return doc;
// }
// 
// static void onyx_docs_emit_human(OnyxDocumentation* doc, bh_file* file) {
//     // NOTE: Disabling fancy line printing until I can make it better
//     #if 0
//     bh_arr_each(DocPackage, dp, doc->package_docs) {
//         bh_printf("Package '%s'\n", dp->name);
// 
//         if (bh_arr_length(dp->public_entries) > 0) {
//             bh_printf("\xe2\x94\x9c\xe2\x94\x80 Public symbols\n");
//             bh_arr_each(DocEntry, de, dp->public_entries) {
//                 bh_printf("\xe2\x94\x82  \xe2\x94\x9c\xe2\x94\x80 %s\n", de->def);
// 
//                 if (de->pos.filename != NULL)
//                     bh_printf("\xe2\x94\x82  \xe2\x94\x82    at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
//                 else
//                     bh_printf("\xe2\x94\x82  \xe2\x94\x82    compiler built-in\n");
// 
//                 bh_printf("\xe2\x94\x82  \xe2\x94\x82\n");
//             }
//         }
// 
//         if (bh_arr_length(dp->private_entries) > 0) {
//             bh_printf("\xe2\x94\x9c\xe2\x94\x80 Private symbols\n");
//             bh_arr_each(DocEntry, de, dp->private_entries) {
//                 bh_printf("\xe2\x94\x82  \xe2\x94\x9c\xe2\x94\x80 %s\n", de->def);
//                 if (de->pos.filename != NULL)
//                     bh_printf("\xe2\x94\x82  \xe2\x94\x82    at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
//                 else
//                     bh_printf("\xe2\x94\x82  \xe2\x94\x82    compiler built-in\n");
// 
//                 bh_printf("\xe2\x94\x82  \xe2\x94\x82\n");
//             }
// 
//             bh_printf("\n");
//         }
//     }
//     #else
//     bh_arr_each(DocPackage, dp, doc->package_docs) {
//         bh_fprintf(file, "Package '%s'\n", dp->name);
// 
//         if (bh_arr_length(dp->public_entries) > 0) {
//             bh_fprintf(file, "   Public symbols\n");
//             bh_arr_each(DocEntry, de, dp->public_entries) {
//                 bh_fprintf(file, "     %s\n", de->def);
// 
//                 if (de->pos.filename != NULL)
//                     bh_fprintf(file, "        at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
//                 else
//                     bh_fprintf(file, "        compiler built-in\n");
// 
//                 bh_fprintf(file, "    \n");
//             }
//         }
// 
//         if (bh_arr_length(dp->private_entries) > 0) {
//             bh_fprintf(file, "   Private symbols\n");
//             bh_arr_each(DocEntry, de, dp->private_entries) {
//                 bh_fprintf(file, "      %s\n", de->def);
//                 if (de->pos.filename != NULL)
//                     bh_fprintf(file, "        at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
//                 else
//                     bh_fprintf(file, "        compiler built-in\n");
// 
//                 bh_fprintf(file, "    \n");
//             }
// 
//             bh_fprintf(file, "\n");
//         }
//     }
//     #endif
// }
// 
// 
// 
// static void onyx_docs_emit_html(OnyxDocumentation* doc, bh_file* file) {
//     bh_fprintf(file, "HTML documentation output not supported yet.\n");
//     return;
// }
// 
// void onyx_docs_emit(OnyxDocumentation* doc, const char* filename) {
//     bh_file file;
//     if (bh_file_create(&file, filename) != BH_FILE_ERROR_NONE) {
//         bh_printf("ERROR: Failed to open file '%s' for writing documentation.\n", filename);
//         return;
//     }
// 
//     switch (doc->format) {
//         case Doc_Format_Human: onyx_docs_emit_human(doc, &file); break;
//         case Doc_Format_Html: onyx_docs_emit_html(doc, &file); break;
//     }
// 
//     bh_file_close(&file);
// }

