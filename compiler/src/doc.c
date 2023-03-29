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

void onyx_docs_submit(OnyxDocInfo *docs, AstBinding *binding) {
    if (!binding->entity || !binding->entity->package) return;

    AstNode *node = binding->node;
    if (!(binding->flags & Ast_Flag_Binding_Isnt_Captured)) {
        if (node->kind == Ast_Kind_Function) {
            ((AstFunction *) node)->original_binding_to_node = binding;
        }

        if (node->kind == Ast_Kind_Macro) {
            ((AstMacro *) node)->original_binding_to_node = binding;
        }

        if (node->kind == Ast_Kind_Polymorphic_Proc) {
            ((AstFunction *) node)->original_binding_to_node = binding;
        }

        if (node->kind == Ast_Kind_Overloaded_Function) {
            ((AstOverloadedFunction *) node)->original_binding_to_node = binding;
        }

        return;   
    }

    if (node->kind == Ast_Kind_Function) {
        AstFunction *func = (void *) node;
        if (!func->generated_from) {
            func->original_binding_to_node = binding;
            bh_arr_push(docs->procedures, binding);
        }
    }

    if (node->kind == Ast_Kind_Macro) {
        ((AstMacro *) node)->original_binding_to_node = binding;
        bh_arr_push(docs->procedures, binding);
    }

    if (node->kind == Ast_Kind_Polymorphic_Proc) {
        ((AstFunction *) node)->original_binding_to_node = binding;
        bh_arr_push(docs->procedures, binding);
    }

    if (node->kind == Ast_Kind_Overloaded_Function) {
        ((AstOverloadedFunction *) node)->original_binding_to_node = binding;
        bh_arr_push(docs->procedures, binding);
    }

    if (node->kind == Ast_Kind_Struct_Type) {
        bh_arr_push(docs->structures, binding);
    }

    if (node->kind == Ast_Kind_Poly_Struct_Type) {
        bh_arr_push(docs->structures, binding);
    }

    if (node->kind == Ast_Kind_Enum_Type) {
        bh_arr_push(docs->enumerations, binding);
    }
}

#define Doc_Magic_Bytes "ODOC"

#define Doc_Procedure_Flag_Macro 1
#define Doc_Procedure_Flag_Foreign 2
#define Doc_Procedure_Flag_Overloaded 4

static void write_cstring(bh_buffer *buffer, const char *data) {
    i32 len = strlen(data);
    bh_buffer_write_u32(buffer, len);
    bh_buffer_append(buffer, data, len);
}

static void write_string(bh_buffer *buffer, i32 len, char *data) {
    bh_buffer_write_u32(buffer, len);
    bh_buffer_append(buffer, data, len);
}

static void write_location(bh_buffer *buffer, OnyxFilePos location) {
    if (shgeti(context.doc_info->file_ids, location.filename) == -1) {
        shput(context.doc_info->file_ids, location.filename, context.doc_info->next_file_id);
        context.doc_info->next_file_id++;
    }

    bh_buffer_write_u32(buffer, context.doc_info->file_ids[shgeti(context.doc_info->file_ids, location.filename)].value);
    bh_buffer_write_u32(buffer, location.line);
    bh_buffer_write_u32(buffer, location.column);
}

static void write_type_node(bh_buffer *buffer, void *vnode) {
    AstNode *node = vnode;
    if (!node) goto unknown_case;

    node = strip_aliases(node);

    switch (node->kind) {
        case Ast_Kind_Basic_Type:
            if (((AstBasicType *) node)->basic_type == &type_auto_return) {
                bh_buffer_write_string(buffer, "#auto");
            } else {
                bh_buffer_write_string(buffer, (char *) ((AstBasicType *) node)->basic_type->Basic.name);
            }
            return;

        case Ast_Kind_Address_Of:
            bh_buffer_write_string(buffer, "&");
            write_type_node(buffer, ((AstAddressOf *) node)->expr);
            return;

        case Ast_Kind_Pointer_Type:
            bh_buffer_write_string(buffer, "&");
            write_type_node(buffer, ((AstPointerType *) node)->elem);
            return;

        case Ast_Kind_Slice_Type:
            bh_buffer_write_string(buffer, "[] ");
            write_type_node(buffer, ((AstSliceType *) node)->elem);
            return;

        case Ast_Kind_VarArg_Type:
            bh_buffer_write_string(buffer, "..");
            write_type_node(buffer, ((AstVarArgType *) node)->elem);
            return;

        case Ast_Kind_DynArr_Type:
            bh_buffer_write_string(buffer, "[..] ");
            write_type_node(buffer, ((AstDynArrType *) node)->elem);
            return;

        case Ast_Kind_Struct_Type:
            bh_buffer_write_string(buffer, ((AstStructType *) node)->name);
            return;

        case Ast_Kind_Poly_Struct_Type:
            bh_buffer_write_string(buffer, ((AstPolyStructType *) node)->name);
            return;

        case Ast_Kind_Poly_Call_Type:
            if (((AstPolyCallType *) node)->callee == (AstType *) builtin_optional_type) {
                bh_buffer_write_string(buffer, "? ");
                write_type_node(buffer, ((AstPolyCallType *) node)->params[0]);
                return;
            }

            write_type_node(buffer, ((AstPolyCallType *) node)->callee);
            if (node->flags & Ast_Flag_Poly_Call_From_Auto) return;

            bh_buffer_write_byte(buffer, '(');

            bh_arr_each(AstNode *, param, ((AstPolyCallType *) node)->params) {
                if (param != ((AstPolyCallType *) node)->params) {
                    bh_buffer_write_string(buffer, ", ");
                }

                write_type_node(buffer, *param);
            }

            bh_buffer_write_byte(buffer, ')');
            return;

        case Ast_Kind_Type_Compound:
            bh_buffer_write_byte(buffer, '(');

            bh_arr_each(AstType *, type, ((AstCompoundType *) node)->types) {
                if (type != ((AstCompoundType *) node)->types) {
                    bh_buffer_write_string(buffer, ", ");
                }

                write_type_node(buffer, *type);
            }

            bh_buffer_write_byte(buffer, ')');
            return;

        case Ast_Kind_Function_Type:
            bh_buffer_write_byte(buffer, '(');

            fori (i, 0, (i64) ((AstFunctionType *) node)->param_count) {
                if (i != 0) {
                    bh_buffer_write_string(buffer, ", ");
                }

                write_type_node(buffer, ((AstFunctionType *) node)->params[i]);
            }

            bh_buffer_write_string(buffer, ") -> ");

            write_type_node(buffer, ((AstFunctionType *) node)->return_type);
            return;

        case Ast_Kind_Field_Access:
            write_type_node(buffer, ((AstFieldAccess *) node)->expr);
            bh_buffer_write_byte(buffer, '.');
            bh_buffer_append(buffer, node->token->text, node->token->length);
            return;

        case Ast_Kind_Typeof:
            bh_buffer_write_string(buffer, "_TYPEOF_");
            return;

        case Ast_Kind_Symbol:
        case Ast_Kind_Param:
            if (node->flags & Ast_Flag_Symbol_Is_PolyVar)
                bh_buffer_write_byte(buffer, '$');
                
            bh_buffer_append(buffer, node->token->text, node->token->length);
            return;
    }

  unknown_case:
    if (node) bh_buffer_write_string(buffer, (char *) onyx_ast_node_kind_string(node->kind));
}

static void write_doc_notes(bh_buffer *buffer, AstBinding *binding) {
    if (!binding || !binding->documentation) {
        write_cstring(buffer, "");
    } else {
        write_string(buffer, binding->documentation->length, binding->documentation->text);
    }
}

static void write_entity_header(bh_buffer *buffer, AstBinding *binding, OnyxFilePos location) {
    if (!binding) {
        bh_buffer_write_u32(buffer, 0);
        bh_buffer_write_u32(buffer, 1);
        bh_buffer_write_u32(buffer, 0);

    } else {
        // Name
        write_string(buffer, binding->token->length, binding->token->text);
        
        // Visibility
        u32 visibility = 1;
        if (binding->flags & Ast_Flag_Private_Package) visibility = 2;
        if (binding->flags & Ast_Flag_Private_File)    visibility = 3;
        bh_buffer_write_u32(buffer, visibility);

        // Package ID
        bh_buffer_write_u32(buffer, binding->entity->package->id - 1);
    }

    // Location
    write_location(buffer, location);

    // Notes
    write_doc_notes(buffer, binding);
}

static b32 write_doc_procedure(bh_buffer *buffer, AstBinding *binding, AstNode *proc);

static b32 write_doc_function(bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    AstFunction *func = (void *) proc;
    if (func->kind == Ast_Kind_Macro) {
        func = (void *) ((AstMacro *) proc)->body;
    }

    write_entity_header(buffer, binding, func->token->pos);

    // Flags
    bh_buffer_write_u32(buffer, proc->kind == Ast_Kind_Macro ? Doc_Procedure_Flag_Macro : 0);

    // Parameter types
    bh_buffer_write_u32(buffer, bh_arr_length(func->params));
    bh_arr_each(AstParam, param, func->params) {
        write_string(buffer, param->local->token->length, param->local->token->text);
        write_cstring(buffer, type_get_name(param->local->type));
        write_cstring(buffer, "");
    }

    // Return type
    write_cstring(buffer, type_get_name(func->type->Function.return_type));

    // Overload procs
    bh_buffer_write_u32(buffer, 0);

    return 1;
}

static b32 write_doc_overloaded_function(bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    AstOverloadedFunction *ofunc = (void *) proc;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, global_heap_allocator, bh_arr_length(ofunc->overloads) * 2);
    build_all_overload_options(ofunc->overloads, &all_overloads);

    write_entity_header(buffer, binding, ofunc->token->pos);

    // Flags
    bh_buffer_write_u32(buffer, Doc_Procedure_Flag_Overloaded);

    // Empty Parameter and Return types
    bh_buffer_write_u32(buffer, 0);
    bh_buffer_write_u32(buffer, 0);

    u32 proc_count_patch = buffer->length;
    bh_buffer_write_u32(buffer, 0);

    u32 proc_count = 0;
    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstNode* node = strip_aliases((AstNode *) entry->key);

        if (write_doc_procedure(buffer, NULL, node)) {
            proc_count += 1;
        }
    }

    *((u32 *) bh_pointer_add(buffer->data, proc_count_patch)) = proc_count;

    bh_imap_free(&all_overloads);
    return 1;
}

static b32 write_doc_polymorphic_proc(bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    AstFunction *func = (void *) proc;
    if (func->kind == Ast_Kind_Macro) {
        func = (void *) ((AstMacro *) proc)->body;
    }

    write_entity_header(buffer, binding, func->token->pos);

    // Flags
    bh_buffer_write_u32(buffer, proc->kind == Ast_Kind_Macro ? Doc_Procedure_Flag_Macro : 0);

    // Parameter types

    bh_buffer param_type_buf;
    bh_buffer_init(&param_type_buf, global_scratch_allocator, 256);

    bh_buffer_write_u32(buffer, bh_arr_length(func->params));
    bh_arr_each(AstParam, param, func->params) {
        bh_buffer_clear(&param_type_buf);

        if (param->is_baked)
            write_cstring(buffer, bh_bprintf("$%b", param->local->token->text, param->local->token->length));
        else 
            write_string(buffer, param->local->token->length, param->local->token->text);

        write_type_node(&param_type_buf, param->local->type_node);
        write_string(buffer, param_type_buf.length, param_type_buf.data);
        write_cstring(buffer, "");
    }


    // Return type
    bh_buffer_clear(&param_type_buf);
    write_type_node(&param_type_buf, func->return_type);
    write_string(buffer, param_type_buf.length, param_type_buf.data);
    bh_buffer_free(&param_type_buf);

    // Overload procs
    bh_buffer_write_u32(buffer, 0);

    return 1;
}

static b32 write_doc_procedure(bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    if (proc->kind == Ast_Kind_Function) {
        return write_doc_function(buffer, binding, proc);

    } else if (proc->kind == Ast_Kind_Macro) {
        AstMacro *macro = (void *) proc;

        if (macro->body->kind == Ast_Kind_Function)
            return write_doc_function(buffer, binding, proc);
        else
            return write_doc_polymorphic_proc(buffer, binding, proc);

    } else if (proc->kind == Ast_Kind_Overloaded_Function) {
        return write_doc_overloaded_function(buffer, binding, proc);

    } else if (proc->kind == Ast_Kind_Polymorphic_Proc) {
        return write_doc_polymorphic_proc(buffer, binding, proc);
    }

    return 0;
}

static b32 write_doc_structure(bh_buffer *buffer, AstBinding *binding, AstNode *node) {
    Scope *method_scope = NULL;

    if (node->kind == Ast_Kind_Struct_Type) {
        AstStructType *struct_node = (void *) node;
        method_scope = get_scope_from_node((AstNode *) struct_node);

        write_entity_header(buffer, binding, node->token->pos);

        Type *struct_type = struct_node->stcache;
        assert(struct_type);

        bh_buffer_write_u32(buffer, bh_arr_length(struct_type->Struct.memarr));
        bh_arr_each(StructMember*, pmem, struct_type->Struct.memarr) {
            StructMember* mem = *pmem;

            write_cstring(buffer, mem->name);
            write_cstring(buffer, type_get_name(mem->type));
            write_cstring(buffer, "");

            bh_buffer_write_u32(buffer, 0);
        }

        // Polymorph parameters
        bh_buffer_write_u32(buffer, 0);
    }
    else if (node->kind == Ast_Kind_Poly_Struct_Type) {
        AstPolyStructType *poly_struct_node = (void *) node;
        method_scope = get_scope_from_node((AstNode *) poly_struct_node);

        AstStructType *struct_node = poly_struct_node->base_struct;

        write_entity_header(buffer, binding, node->token->pos);

        bh_buffer type_buf;
        bh_buffer_init(&type_buf, global_scratch_allocator, 256);

        bh_buffer_write_u32(buffer, bh_arr_length(struct_node->members));
        bh_arr_each(AstStructMember *, psmem, struct_node->members) {
            AstStructMember *smem = *psmem;

            bh_buffer_clear(&type_buf);
            write_type_node(&type_buf, smem->type_node);

            write_string(buffer, smem->token->length, smem->token->text);
            write_string(buffer, type_buf.length, type_buf.data);
            write_cstring(buffer, "");

            bh_buffer_write_u32(buffer, smem->is_used ? 1 : 0);
        }

        // Polymorph parameters
        bh_buffer_write_u32(buffer, bh_arr_length(poly_struct_node->poly_params));
        bh_arr_each(AstPolyStructParam, param, poly_struct_node->poly_params) {
            bh_buffer_clear(&type_buf);
            write_type_node(&type_buf, param->type_node);

            write_string(buffer, param->token->length, param->token->text);
            write_string(buffer, type_buf.length, type_buf.data);
            write_cstring(buffer, "");
        }

        bh_buffer_free(&type_buf);
    }

    u32 count_patch = buffer->length;
    bh_buffer_write_u32(buffer, 0);

    u32 method_count = 0;
    fori (i, 0, shlen(method_scope->symbols)) {
        AstFunction* node = (AstFunction *) strip_aliases(method_scope->symbols[i].value);
        if (node->kind != Ast_Kind_Function
            && node->kind != Ast_Kind_Polymorphic_Proc
            && node->kind != Ast_Kind_Overloaded_Function
            && node->kind != Ast_Kind_Macro)
            continue;

        assert(node->entity);
        assert(node->entity->function == node);

        AstBinding *binding = NULL;
        switch (node->kind) {
            case Ast_Kind_Polymorphic_Proc:
            case Ast_Kind_Function:            binding = node->original_binding_to_node; break;
            case Ast_Kind_Macro:               binding = ((AstMacro *) node)->original_binding_to_node; break;
            case Ast_Kind_Overloaded_Function: binding = ((AstOverloadedFunction *) node)->original_binding_to_node; break;
        }

        method_count++;
        write_doc_procedure(buffer, binding, (AstNode *) node);
    }

    *((u32 *) bh_pointer_add(buffer->data, count_patch)) = method_count;

    return 1;
}

static b32 write_doc_enum(bh_buffer *buffer, AstBinding *binding, AstNode *node) {
    AstEnumType *enum_node = (void *) node;

    write_entity_header(buffer, binding, node->token->pos);

    bh_buffer_write_u32(buffer, bh_arr_length(enum_node->values));
    bh_arr_each(AstEnumValue *, pvalue, enum_node->values) {
        AstEnumValue * value = *pvalue;

        write_string(buffer, value->token->length, value->token->text);

        assert(value->value->kind == Ast_Kind_NumLit);
        AstNumLit *num = (AstNumLit *) value->value;
        bh_buffer_write_u64(buffer, num->value.l);
    }

    bh_buffer_write_u32(buffer, enum_node->is_flags ? 1 : 0);

    return 1;
}

static void write_doc_entity_array(bh_buffer *buffer, bh_arr(AstBinding *) arr,
    b32 (*write_doc)(bh_buffer *buffer, AstBinding *, AstNode*),
    u32 offset_write_location) {
    *((u32 *) bh_pointer_add(buffer->data, offset_write_location)) = buffer->length;

    u32 count_patch = buffer->length;
    bh_buffer_write_u32(buffer, 0);

    u32 count = 0;
    bh_arr_each(AstBinding *, pbind, arr) {
        if (write_doc(buffer, *pbind, (*pbind)->node)) {
            count++;
        }
    }

    *((u32 *) bh_pointer_add(buffer->data, count_patch)) = count;
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

        write_cstring(&doc_buffer, p->unqualified_name);
        write_cstring(&doc_buffer, package_qualified_name);

        bh_buffer_write_u32(&doc_buffer, p->parent_id);

        bh_buffer_write_u32(&doc_buffer, bh_arr_length(p->sub_packages));
        fori (j, 0, bh_arr_length(p->sub_packages)) {
            bh_buffer_write_u32(&doc_buffer, (u32) p->sub_packages[j] - 1);
        }
    }

    //
    // Procedure Info
    //
    write_doc_entity_array(&doc_buffer, context.doc_info->procedures, write_doc_procedure, offset_table_index + 4);


    //
    // Structure Info
    //
    write_doc_entity_array(&doc_buffer, context.doc_info->structures, write_doc_structure, offset_table_index + 8);


    //
    // Enum Info
    //
    write_doc_entity_array(&doc_buffer, context.doc_info->enumerations, write_doc_enum, offset_table_index + 12);


    //
    // File Info
    //
    *((u32 *) bh_pointer_add(doc_buffer.data, offset_table_index + 16)) = doc_buffer.length;

    bh_buffer_write_u32(&doc_buffer, shlenu(context.doc_info->file_ids));
    fori (i, 0, shlen(context.doc_info->file_ids)) {
        const char *key = context.doc_info->file_ids[i].key;
        
        bh_buffer_write_u32(&doc_buffer, 0);
        write_cstring(&doc_buffer, key);
    }


    bh_file_write(&doc_file, doc_buffer.data, doc_buffer.length);
    bh_file_close(&doc_file);
}



