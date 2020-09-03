#include "onyxdoc.h"
#include "onyxutils.h"
#include "onyxtypes.h"

static i32 cmp_doc_entry(const void * a, const void * b) {
    DocEntry* d1 = (DocEntry *) a;
    DocEntry* d2 = (DocEntry *) b;

    return strncmp(d1->def, d2->def, 1024);
}

static i32 cmp_doc_package(const void * a, const void * b) {
    DocPackage* d1 = (DocPackage *) a;
    DocPackage* d2 = (DocPackage *) b;

    return strncmp(d1->name, d2->name, 1024);
}

static char* node_to_doc_def(const char* sym, AstNode *node, bh_allocator a) {
    static char buf[1024];
    memset(buf, 0, 1023);

    strncat(buf, sym, 1023);
    strncat(buf, " :: ", 1023);

    switch (node->kind) {
        case Ast_Kind_Function: {
            strncat(buf, "proc (", 1023);

            AstFunction *func = (AstFunction *) node;
            bh_arr_each(AstParam, param, func->params) {
                if (param != func->params)
                    strncat(buf, ", ", 1023);

                token_toggle_end(param->local->token);
                strncat(buf, param->local->token->text, 1023);
                token_toggle_end(param->local->token);

                strncat(buf, ": ", 1023);

                strncat(buf, type_get_name(param->local->type), 1023);

                if (param->default_value != NULL) {
                    strncat(buf, " = <default>", 1023);
                }
            }

            strncat(buf, ")", 1023);

            if (func->type->Function.return_type != &basic_types[Basic_Kind_Void]) {
                strncat(buf, " -> ", 1023);
                strncat(buf, type_get_name(func->type->Function.return_type), 1023);
            }

            break;
        }

        case Ast_Kind_Struct_Type: {
            strncat(buf, "struct { ", 1023);

            AstStructType* st = (AstStructType *) node;
            bh_arr_each(AstStructMember *, smem, st->members) {

                token_toggle_end((*smem)->token);
                strncat(buf, (*smem)->token->text, 1023);
                token_toggle_end((*smem)->token);

                strncat(buf, ": ", 1023);

                strncat(buf, type_get_name((*smem)->type), 1023);

                strncat(buf, "; ", 1023);
            }

            strncat(buf, "}", 1023);
            break;
        }

        case Ast_Kind_Basic_Type:
        case Ast_Kind_Array_Type:
        case Ast_Kind_Type_Alias:
        case Ast_Kind_Slice_Type:
        case Ast_Kind_DynArr_Type: {
            strncat(buf, type_get_name(type_build_from_ast(global_heap_allocator, (AstType *) node)), 1023);
            break;
        }

        default: {
            strncat(buf, "<unimplemented printing>", 1023);
        }
    }

    return bh_strdup(a, buf);
}

static DocPackage doc_package_create(Package* p, bh_allocator a) {
    DocPackage dp;
    dp.name = p->name;
    dp.public_entries = NULL;
    dp.private_entries = NULL;

    bh_arr_new(global_heap_allocator, dp.public_entries, 16);
    bh_arr_new(global_heap_allocator, dp.private_entries, 16);

    bh_table_each_start(AstNode *, p->scope->symbols)
        DocEntry de;
        de.pos = value->token->pos;
        de.def = node_to_doc_def(key, value, a);
        de.sym = (char *) key;
        de.additional = NULL;

        bh_arr_push(dp.public_entries, de);
    bh_table_each_end;

    bh_table_each_start(AstNode *, p->private_scope->symbols)
        DocEntry de;
        de.pos = value->token->pos;
        de.def = node_to_doc_def(key, value, a);
        de.sym = (char *) key;
        de.additional = NULL;

        bh_arr_push(dp.private_entries, de);
    bh_table_each_end;

    qsort(dp.public_entries,  bh_arr_length(dp.public_entries),  sizeof(DocEntry), cmp_doc_entry);
    qsort(dp.private_entries, bh_arr_length(dp.private_entries), sizeof(DocEntry), cmp_doc_entry);

    return dp;
}

OnyxDocumentation onyx_docs_generate(ProgramInfo* prog) {
    OnyxDocumentation doc;

    bh_arena_init(&doc.doc_arena, global_heap_allocator, 16 * 1024);
    bh_allocator a = bh_arena_allocator(&doc.doc_arena);

    doc.package_docs = NULL;
    bh_arr_new(global_heap_allocator, doc.package_docs, 16);

    bh_table_each_start(Package *, prog->packages);
        DocPackage dp = doc_package_create(value, a);
        bh_arr_push(doc.package_docs, dp);
    bh_table_each_end;

    qsort(doc.package_docs, bh_arr_length(doc.package_docs), sizeof(DocPackage), cmp_doc_package);

    return doc;
}

static void onyx_docs_emit_human(OnyxDocumentation* doc) {
    // NOTE: Disabling fancy line printing until I can make it better
    #if 0
    bh_arr_each(DocPackage, dp, doc->package_docs) {
        bh_printf("Package '%s'\n", dp->name);

        if (bh_arr_length(dp->public_entries) > 0) {
            bh_printf("\xe2\x94\x9c\xe2\x94\x80 Public symbols\n");
            bh_arr_each(DocEntry, de, dp->public_entries) {
                bh_printf("\xe2\x94\x82  \xe2\x94\x9c\xe2\x94\x80 %s\n", de->def);

                if (de->pos.filename != NULL)
                    bh_printf("\xe2\x94\x82  \xe2\x94\x82    at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
                else
                    bh_printf("\xe2\x94\x82  \xe2\x94\x82    compiler built-in\n");

                bh_printf("\xe2\x94\x82  \xe2\x94\x82\n");
            }
        }

        if (bh_arr_length(dp->private_entries) > 0) {
            bh_printf("\xe2\x94\x9c\xe2\x94\x80 Private symbols\n");
            bh_arr_each(DocEntry, de, dp->private_entries) {
                bh_printf("\xe2\x94\x82  \xe2\x94\x9c\xe2\x94\x80 %s\n", de->def);
                if (de->pos.filename != NULL)
                    bh_printf("\xe2\x94\x82  \xe2\x94\x82    at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
                else
                    bh_printf("\xe2\x94\x82  \xe2\x94\x82    compiler built-in\n");

                bh_printf("\xe2\x94\x82  \xe2\x94\x82\n");
            }

            bh_printf("\n");
        }
    }
    #else
    bh_arr_each(DocPackage, dp, doc->package_docs) {
        bh_printf("Package '%s'\n", dp->name);

        if (bh_arr_length(dp->public_entries) > 0) {
            bh_printf("   Public symbols\n");
            bh_arr_each(DocEntry, de, dp->public_entries) {
                bh_printf("     %s\n", de->def);

                if (de->pos.filename != NULL)
                    bh_printf("        at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
                else
                    bh_printf("        compiler built-in\n");

                bh_printf("    \n");
            }
        }

        if (bh_arr_length(dp->private_entries) > 0) {
            bh_printf("   Private symbols\n");
            bh_arr_each(DocEntry, de, dp->private_entries) {
                bh_printf("      %s\n", de->def);
                if (de->pos.filename != NULL)
                    bh_printf("        at %s:%d,%d\n", de->pos.filename, de->pos.line, de->pos.column);
                else
                    bh_printf("        compiler built-in\n");

                bh_printf("    \n");
            }

            bh_printf("\n");
        }
    }
    #endif
}


static i32 sort_tags(const void* a, const void* b) {
    DocEntry *da = *((DocEntry **) a);
    DocEntry *db = *((DocEntry **) b);

    return strcmp(da->sym, db->sym);
}

static void onyx_docs_emit_tags(OnyxDocumentation* doc) {
    bh_file tags_file;
    if (bh_file_create(&tags_file, "./tags") != BH_FILE_ERROR_NONE) {
        bh_printf("Cannot create 'tags'.\n");
        return;
    }

    bh_fprintf(&tags_file, "!_TAG_FILE_FORMAT\t2\n");
    bh_fprintf(&tags_file, "!_TAG_FILE_SORTED\t1\n");
    bh_fprintf(&tags_file, "!_TAG_OUTPUT_FILESEP\tslash\n");
    bh_fprintf(&tags_file, "!_TAG_OUTPUT_MODE\tu-ctags\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_AUTHOR\tOnyx Compiler\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_NAME\tOnyx Compiler\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_URL\thttps://github.com/brendanfh/onyx\n");
    bh_fprintf(&tags_file, "!_TAG_PROGRAM_VERSION\t0.0.1\n");

    bh_arr(DocEntry *) tags = NULL;
    bh_arr_new(global_heap_allocator, tags, 256);

    bh_arr_each(DocPackage, dp, doc->package_docs) {
        bh_arr_each(DocEntry, de, dp->public_entries) bh_arr_push(tags, de);
        bh_arr_each(DocEntry, de, dp->private_entries) bh_arr_push(tags, de);
    }

    qsort(tags, bh_arr_length(tags), sizeof(DocEntry *), sort_tags);

    bh_arr_each(DocEntry *, tag, tags) {
        if ((*tag)->pos.filename == NULL) continue;

        i32 line_len = 0;
        char *c = (*tag)->pos.line_start;
        while (*c++ != '\n') line_len++;

        bh_fprintf(&tags_file, "%s\t%s\t/^%b$/\n",
                (*tag)->sym, (*tag)->pos.filename,
                (*tag)->pos.line_start, line_len);
    }

    bh_file_close(&tags_file);
}

static void onyx_docs_emit_html(OnyxDocumentation* doc) {
    bh_printf("HTML documentation output not supported yet.\n");
    return;
}

void onyx_docs_emit(OnyxDocumentation* doc) {
    switch (doc->format) {
        case Doc_Format_Human: onyx_docs_emit_human(doc); break;
        case Doc_Format_Tags: onyx_docs_emit_tags(doc); break;
        case Doc_Format_Html: onyx_docs_emit_html(doc); break;
    }
}

