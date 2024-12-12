#include "doc.h"
#include "utils.h"
#include "types.h"

static i32 sort_symbol_resolutions(const SymbolResolution *a, const SymbolResolution *b) {
    if (a->file_id != b->file_id) {
        return a->file_id > b->file_id ? 1 : -1;
    }

    if (a->line != b->line) {
        return a->line > b->line ? 1 : -1;
    }

    return a->column > b->column ? 1 : -1;
}

void onyx_docs_emit_symbol_info(Context *context, bh_buffer *out_buffer) {
    SymbolInfoTable *syminfo = context->symbol_info;
    if (!syminfo) return;

    qsort(syminfo->symbols_resolutions,
            bh_arr_length(syminfo->symbols_resolutions),
            sizeof(SymbolResolution),
            (int (*)(const void *, const void*)) sort_symbol_resolutions);

    bh_buffer file_section;
    bh_buffer_init(&file_section, context->gp_alloc, 2048);
    fori (i, 0, shlen(syminfo->files)) {
        char *filename = syminfo->files[i].key;
        u32   file_id  = syminfo->files[i].value;
        bh_buffer_write_u32(&file_section, file_id);
        bh_buffer_write_u32(&file_section, strlen(filename));
        bh_buffer_write_string(&file_section, filename);
    }

    bh_buffer sym_def_section;
    bh_buffer_init(&sym_def_section, context->gp_alloc, 2048);

    bh_buffer docs_section;
    bh_buffer_init(&docs_section, context->gp_alloc, 4096);

    bh_arr_each(SymbolInfo, sym, syminfo->symbols) {
        bh_buffer_write_u32(&sym_def_section, sym->id);
        bh_buffer_write_u32(&sym_def_section, sym->file_id);
        bh_buffer_write_u32(&sym_def_section, sym->line);
        bh_buffer_write_u32(&sym_def_section, sym->column);

        if (context->options->generate_lsp_info_file) {
            if (sym->documentation_length > 0) {
                bh_buffer_write_u32(&sym_def_section, docs_section.length);
                bh_buffer_write_u32(&sym_def_section, sym->documentation_length);

                bh_buffer_append(&docs_section, sym->documentation, sym->documentation_length);
            } else {
                bh_buffer_write_u32(&sym_def_section, 0);
                bh_buffer_write_u32(&sym_def_section, 0);
            }
        }
    }

    bh_buffer sym_res_section;
    bh_buffer_init(&sym_res_section, context->gp_alloc, 2048);
    bh_arr_each(SymbolResolution, sym, syminfo->symbols_resolutions) {
        bh_buffer_write_u32(&sym_res_section, sym->file_id);
        bh_buffer_write_u32(&sym_res_section, sym->line);
        bh_buffer_write_u32(&sym_res_section, sym->column);
        bh_buffer_write_u32(&sym_res_section, sym->length);
        bh_buffer_write_u32(&sym_res_section, sym->symbol_id);
    }

    bh_buffer header_section;
    bh_buffer_init(&header_section, context->gp_alloc, 16);
    bh_buffer_append(&header_section, "OSYM", 4);

    u32 header_size = 32;
    if (context->options->generate_lsp_info_file) {
        bh_buffer_write_u32(&header_section, 2);
        header_size = 40;
    } else {
        bh_buffer_write_u32(&header_section, 1);
    }

    bh_buffer_write_u32(&header_section, header_size);
    bh_buffer_write_u32(&header_section, shlenu(syminfo->files));
    bh_buffer_write_u32(&header_section, header_size + file_section.length);
    bh_buffer_write_u32(&header_section, bh_arr_length(syminfo->symbols));
    bh_buffer_write_u32(&header_section, header_size + file_section.length + sym_def_section.length);
    bh_buffer_write_u32(&header_section, bh_arr_length(syminfo->symbols_resolutions));

    if (context->options->generate_lsp_info_file) {
        bh_buffer_write_u32(&header_section, header_size + file_section.length + sym_def_section.length + sym_res_section.length);
        bh_buffer_write_u32(&header_section, docs_section.length);
    }

    bh_buffer_init(out_buffer, context->gp_alloc, header_section.length + file_section.length + sym_def_section.length + sym_res_section.length);
    bh_buffer_append(out_buffer, header_section.data, header_section.length);
    bh_buffer_append(out_buffer, file_section.data, file_section.length);
    bh_buffer_append(out_buffer, sym_def_section.data, sym_def_section.length);
    bh_buffer_append(out_buffer, sym_res_section.data, sym_res_section.length);

    if (context->options->generate_lsp_info_file) {
        bh_buffer_append(out_buffer, docs_section.data, docs_section.length);
    }

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
    if (!docs) return;
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

    if (node->kind == Ast_Kind_Distinct_Type) {
        bh_arr_push(docs->distinct_types, binding);
    }

    if (node->kind == Ast_Kind_Union_Type) {
        bh_arr_push(docs->unions, binding);
    }

    if (node->kind == Ast_Kind_Poly_Union_Type) {
        bh_arr_push(docs->unions, binding);
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

static void write_location(Context *context, bh_buffer *buffer, OnyxFilePos location) {
    if (shgeti(context->doc_info->file_ids, location.filename) == -1) {
        shput(context->doc_info->file_ids, location.filename, context->doc_info->next_file_id);
        context->doc_info->next_file_id++;
    }

    bh_buffer_write_u32(buffer, context->doc_info->file_ids[shgeti(context->doc_info->file_ids, location.filename)].value);
    bh_buffer_write_u32(buffer, location.line);
    bh_buffer_write_u32(buffer, location.column);
}

static void write_type_node(Context *context, bh_buffer *buffer, void *vnode) {
    AstNode *node = vnode;
    if (!node) goto unknown_case;

    node = strip_aliases(node);

    switch (node->kind) {
        case Ast_Kind_Basic_Type:
            if (((AstBasicType *) node)->basic_type == context->types.auto_return) {
                bh_buffer_write_string(buffer, "#auto");
            } else {
                bh_buffer_write_string(buffer, (char *) ((AstBasicType *) node)->basic_type->Basic.name);
            }
            return;

        case Ast_Kind_Address_Of:
            bh_buffer_write_string(buffer, "&");
            write_type_node(context, buffer, ((AstAddressOf *) node)->expr);
            return;

        case Ast_Kind_Pointer_Type:
            bh_buffer_write_string(buffer, "&");
            write_type_node(context, buffer, ((AstPointerType *) node)->elem);
            return;

        case Ast_Kind_Multi_Pointer_Type:
            bh_buffer_write_string(buffer, "[&] ");
            write_type_node(context, buffer, ((AstPointerType *) node)->elem);
            return;

        case Ast_Kind_Slice_Type:
            bh_buffer_write_string(buffer, "[] ");
            write_type_node(context, buffer, ((AstSliceType *) node)->elem);
            return;

        case Ast_Kind_VarArg_Type:
            bh_buffer_write_string(buffer, "..");
            write_type_node(context, buffer, ((AstVarArgType *) node)->elem);
            return;

        case Ast_Kind_DynArr_Type:
            bh_buffer_write_string(buffer, "[..] ");
            write_type_node(context, buffer, ((AstDynArrType *) node)->elem);
            return;

        case Ast_Kind_Struct_Type:
            bh_buffer_write_string(buffer, ((AstStructType *) node)->name);
            return;

        case Ast_Kind_Union_Type:
            bh_buffer_write_string(buffer, ((AstUnionType *) node)->name);
            return;

        case Ast_Kind_Poly_Struct_Type:
            bh_buffer_write_string(buffer, ((AstPolyStructType *) node)->name);
            return;

        case Ast_Kind_Poly_Union_Type:
            bh_buffer_write_string(buffer, ((AstPolyUnionType *) node)->name);
            return;

        case Ast_Kind_Poly_Call_Type:
            if (((AstPolyCallType *) node)->callee == (AstType *) context->builtins.optional_type) {
                bh_buffer_write_string(buffer, "? ");
                write_type_node(context, buffer, ((AstPolyCallType *) node)->params[0]);
                return;
            }

            write_type_node(context, buffer, ((AstPolyCallType *) node)->callee);
            if (node->flags & Ast_Flag_Poly_Call_From_Auto) return;

            bh_buffer_write_byte(buffer, '(');

            bh_arr_each(AstNode *, param, ((AstPolyCallType *) node)->params) {
                if (param != ((AstPolyCallType *) node)->params) {
                    bh_buffer_write_string(buffer, ", ");
                }

                write_type_node(context, buffer, *param);
            }

            bh_buffer_write_byte(buffer, ')');
            return;

        case Ast_Kind_Type_Compound:
            bh_buffer_write_byte(buffer, '(');

            bh_arr_each(AstType *, type, ((AstCompoundType *) node)->types) {
                if (type != ((AstCompoundType *) node)->types) {
                    bh_buffer_write_string(buffer, ", ");
                }

                write_type_node(context, buffer, *type);
            }

            bh_buffer_write_byte(buffer, ')');
            return;

        case Ast_Kind_Function_Type:
            bh_buffer_write_byte(buffer, '(');

            fori (i, 0, (i64) ((AstFunctionType *) node)->param_count) {
                if (i != 0) {
                    bh_buffer_write_string(buffer, ", ");
                }

                write_type_node(context, buffer, ((AstFunctionType *) node)->params[i]);
            }

            bh_buffer_write_string(buffer, ") -> ");

            write_type_node(context, buffer, ((AstFunctionType *) node)->return_type);
            return;

        case Ast_Kind_Field_Access:
            write_type_node(context, buffer, ((AstFieldAccess *) node)->expr);
            bh_buffer_write_byte(buffer, '.');
            bh_buffer_append(buffer, node->token->text, node->token->length);
            return;

        case Ast_Kind_Typeof:
            bh_buffer_write_string(buffer, (char *) type_get_name(
                context,
                type_build_from_ast(context, (AstType *) node)
            ));
            return;

        case Ast_Kind_Alias:
            write_type_node(context, buffer, ((AstAlias *) node)->alias);
            return;

        case Ast_Kind_Type_Alias:
            write_type_node(context, buffer, ((AstTypeAlias *) node)->to);
            return;

        case Ast_Kind_Symbol:
        case Ast_Kind_Param:
            if (node->flags & Ast_Flag_Symbol_Is_PolyVar)
                bh_buffer_write_byte(buffer, '$');
                
            bh_buffer_append(buffer, node->token->text, node->token->length);
            return;

        default: break;
    }

  unknown_case:
    if (node) bh_buffer_write_string(buffer, (char *) onyx_ast_node_kind_string(node->kind));
}

static void write_doc_notes(bh_buffer *buffer, AstBinding *binding) {
    if (binding) {
        if (binding->documentation_token_old) {
            write_string(buffer, binding->documentation_token_old->length, binding->documentation_token_old->text);
            return;
        }

        if (binding->documentation_string) {
            write_string(buffer, strlen(binding->documentation_string), (char *) binding->documentation_string);
            return;
        }
    }

    write_cstring(buffer, "");
}

static void write_entity_header(Context *context, bh_buffer *buffer, AstBinding *binding, OnyxFilePos location) {
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
    write_location(context, buffer, location);

    // Notes
    write_doc_notes(buffer, binding);
}

static b32 write_doc_procedure(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *proc);

static void write_doc_constraints(Context *context, bh_buffer *buffer, ConstraintContext *constraints, bh_arr(AstPolyParam) poly_params) {
    bh_buffer tmp_buffer;
    bh_buffer_init(&tmp_buffer, context->scratch_alloc, 256);

    u32 constraint_count_patch = buffer->length;
    bh_buffer_write_u32(buffer, 0);

    u32 constraint_count = 0;
    bh_arr_each(AstConstraint *, pconstraint, constraints->constraints) {
        AstConstraint *constraint = *pconstraint;

        bh_buffer_clear(&tmp_buffer);
        write_type_node(context, &tmp_buffer, constraint->interface);
        bh_buffer_write_string(&tmp_buffer, "(");

        bh_arr_each(AstTyped *, ptype_arg, constraint->args) {
            if (ptype_arg != constraint->args) {
                bh_buffer_write_string(&tmp_buffer, ", ");
            }

            AstTyped *type_arg = *ptype_arg;
            write_type_node(context, &tmp_buffer, type_arg);
        }

        bh_buffer_write_string(&tmp_buffer, ")");
        write_string(buffer, tmp_buffer.length, (char *) tmp_buffer.data);

        constraint_count += 1;
    }

    if (poly_params) {
        bh_arr_each(AstPolyParam, poly_param, poly_params) {
            if (!poly_param->implicit_interface) continue;

            bh_buffer_clear(&tmp_buffer);
            write_type_node(context, &tmp_buffer, poly_param->implicit_interface);
            bh_buffer_write_string(&tmp_buffer, "(");

            poly_param->poly_sym->flags &= ~Ast_Flag_Symbol_Is_PolyVar;
            write_type_node(context, &tmp_buffer, poly_param->poly_sym);
            poly_param->poly_sym->flags |= Ast_Flag_Symbol_Is_PolyVar;

            bh_buffer_write_string(&tmp_buffer, ")");
            write_string(buffer, tmp_buffer.length, (char *) tmp_buffer.data);

            constraint_count += 1;
        }
    }

    *((u32 *) bh_pointer_add(buffer->data, constraint_count_patch)) = constraint_count;

    bh_buffer_free(&tmp_buffer);
}

static void write_doc_methods(Context *context, bh_buffer *buffer, Scope *method_scope) {
    u32 count_patch = buffer->length;
    bh_buffer_write_u32(buffer, 0);

    if (method_scope == NULL) {
        return;
    }

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
            default: break;
        }

        OnyxToken tmp_name_token;
        tmp_name_token.pos = binding->token->pos;
        tmp_name_token.text = method_scope->symbols[i].key;
        tmp_name_token.length = strlen(tmp_name_token.text);

        OnyxToken *old_token = binding->token;
        binding->token = &tmp_name_token;

        method_count++;
        write_doc_procedure(context, buffer, binding, (AstNode *) node);

        binding->token = old_token;
    }

    *((u32 *) bh_pointer_add(buffer->data, count_patch)) = method_count;
}

static b32 write_doc_function(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    AstFunction *func = (void *) proc;
    if (func->kind == Ast_Kind_Macro) {
        func = (void *) ((AstMacro *) proc)->body;
    }

    write_entity_header(context, buffer, binding, func->token->pos);

    // Flags
    bh_buffer_write_u32(buffer, proc->kind == Ast_Kind_Macro ? Doc_Procedure_Flag_Macro : 0);

    // Parameter types
    bh_buffer_write_u32(buffer, bh_arr_length(func->params));
    bh_arr_each(AstParam, param, func->params) {
        write_string(buffer, param->local->token->length, param->local->token->text);
        write_cstring(buffer, type_get_name(context, param->local->type));
        write_cstring(buffer, "");
    }

    // Return type
    write_cstring(buffer, type_get_name(context, func->type->Function.return_type));

    // Overload procs
    bh_buffer_write_u32(buffer, 0);

    // Constraints
    bh_buffer_write_u32(buffer, 0);

    return 1;
}

static b32 write_doc_overloaded_function(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    AstOverloadedFunction *ofunc = (void *) proc;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, context->gp_alloc, bh_arr_length(ofunc->overloads) * 2);
    build_all_overload_options(ofunc->overloads, &all_overloads);

    write_entity_header(context, buffer, binding, ofunc->token->pos);

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

        if (write_doc_procedure(context, buffer, NULL, node)) {
            proc_count += 1;
        }
    }

    *((u32 *) bh_pointer_add(buffer->data, proc_count_patch)) = proc_count;

    // Constraints
    bh_buffer_write_u32(buffer, 0);

    bh_imap_free(&all_overloads);
    return 1;
}

static b32 write_doc_polymorphic_proc(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    AstFunction *func = (void *) proc;
    if (func->kind == Ast_Kind_Macro) {
        func = (void *) ((AstMacro *) proc)->body;
    }

    write_entity_header(context, buffer, binding, func->token->pos);

    // Flags
    bh_buffer_write_u32(buffer, proc->kind == Ast_Kind_Macro ? Doc_Procedure_Flag_Macro : 0);

    // Parameter types

    bh_buffer param_type_buf;
    bh_buffer_init(&param_type_buf, context->scratch_alloc, 256);

    bh_buffer_write_u32(buffer, bh_arr_length(func->params));
    bh_arr_each(AstParam, param, func->params) {
        bh_buffer_clear(&param_type_buf);

        if (param->is_baked)
            write_cstring(buffer, bh_bprintf("$%b", param->local->token->text, param->local->token->length));
        else 
            write_string(buffer, param->local->token->length, param->local->token->text);

        write_type_node(context, &param_type_buf, param->local->type_node);
        write_string(buffer, param_type_buf.length, (char *) param_type_buf.data);
        write_cstring(buffer, "");
    }


    // Return type
    bh_buffer_clear(&param_type_buf);
    write_type_node(context, &param_type_buf, func->return_type);
    write_string(buffer, param_type_buf.length, (char *) param_type_buf.data);
    bh_buffer_free(&param_type_buf);

    // Overload procs
    bh_buffer_write_u32(buffer, 0);

    // Constraints
    write_doc_constraints(context, buffer, &func->constraints, func->poly_params);
    return 1;
}

static b32 write_doc_procedure(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *proc) {
    if (proc->kind == Ast_Kind_Function) {
        return write_doc_function(context, buffer, binding, proc);

    } else if (proc->kind == Ast_Kind_Macro) {
        AstMacro *macro = (void *) proc;

        if (macro->body->kind == Ast_Kind_Function)
            return write_doc_function(context, buffer, binding, proc);
        else
            return write_doc_polymorphic_proc(context, buffer, binding, proc);

    } else if (proc->kind == Ast_Kind_Overloaded_Function) {
        return write_doc_overloaded_function(context, buffer, binding, proc);

    } else if (proc->kind == Ast_Kind_Polymorphic_Proc) {
        return write_doc_polymorphic_proc(context, buffer, binding, proc);
    }

    return 0;
}

static b32 write_doc_structure(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *node) {
    Scope *method_scope = NULL;

    if (node->kind == Ast_Kind_Struct_Type) {
        AstStructType *struct_node = (void *) node;
        method_scope = get_scope_from_node(context, (AstNode *) struct_node);

        write_entity_header(context, buffer, binding, node->token->pos);

        Type *struct_type = struct_node->stcache;
        assert(struct_type);

        bh_buffer_write_u32(buffer, bh_arr_length(struct_type->Struct.memarr));
        bh_arr_each(StructMember*, pmem, struct_type->Struct.memarr) {
            StructMember* mem = *pmem;

            write_cstring(buffer, mem->name);
            write_cstring(buffer, type_get_name(context, mem->type));
            write_cstring(buffer, "");

            bh_buffer_write_u32(buffer, 0);
        }

        // Polymorph parameters
        bh_buffer_write_u32(buffer, 0);

        // Constraints
        write_doc_constraints(context, buffer, &struct_node->constraints, NULL);
    }
    else if (node->kind == Ast_Kind_Poly_Struct_Type) {
        AstPolyStructType *poly_struct_node = (void *) node;
        method_scope = get_scope_from_node(context, (AstNode *) poly_struct_node);

        AstStructType *struct_node = poly_struct_node->base_struct;

        write_entity_header(context, buffer, binding, node->token->pos);

        bh_buffer type_buf;
        bh_buffer_init(&type_buf, context->scratch_alloc, 256);

        bh_buffer_write_u32(buffer, bh_arr_length(struct_node->members));
        bh_arr_each(AstStructMember *, psmem, struct_node->members) {
            AstStructMember *smem = *psmem;

            bh_buffer_clear(&type_buf);
            write_type_node(context, &type_buf, smem->type_node);

            write_string(buffer, smem->token->length, smem->token->text);
            write_string(buffer, type_buf.length, (char *) type_buf.data);
            write_cstring(buffer, "");

            bh_buffer_write_u32(buffer, smem->is_used ? 1 : 0);
        }

        // Polymorph parameters
        bh_buffer_write_u32(buffer, bh_arr_length(poly_struct_node->poly_params));
        bh_arr_each(AstPolyStructParam, param, poly_struct_node->poly_params) {
            bh_buffer_clear(&type_buf);
            write_type_node(context, &type_buf, param->type_node);

            write_string(buffer, param->token->length, param->token->text);
            write_string(buffer, type_buf.length, (char *) type_buf.data);
            write_cstring(buffer, "");
        }

        // Constraints
        write_doc_constraints(context, buffer, &struct_node->constraints, NULL);

        bh_buffer_free(&type_buf);
    }

    write_doc_methods(context, buffer, method_scope);

    return 1;
}

static b32 write_doc_union_type(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *node) {
    Scope *method_scope = NULL;

    if (node->kind == Ast_Kind_Union_Type) {
        AstUnionType *union_node = (void *) node;
        method_scope = get_scope_from_node(context, (AstNode *) union_node);

        write_entity_header(context, buffer, binding, node->token->pos);

        Type *union_type = union_node->utcache;
        assert(union_type);

        bh_buffer_write_u32(buffer, bh_arr_length(union_type->Union.variants_ordered));
        bh_arr_each(UnionVariant*, puv, union_type->Union.variants_ordered) {
            UnionVariant* uv = *puv;

            write_cstring(buffer, uv->name);
            write_cstring(buffer, type_get_name(context, uv->type));
        }

        // Polymorph parameters
        bh_buffer_write_u32(buffer, 0);

        // Constraints
        write_doc_constraints(context, buffer, &union_node->constraints, NULL);
    }
    else if (node->kind == Ast_Kind_Poly_Union_Type) {
        AstPolyUnionType *poly_union_node = (void *) node;
        method_scope = get_scope_from_node(context, (AstNode *) poly_union_node);

        AstUnionType *union_node = poly_union_node->base_union;

        write_entity_header(context, buffer, binding, node->token->pos);

        bh_buffer type_buf;
        bh_buffer_init(&type_buf, context->scratch_alloc, 256);

        bh_buffer_write_u32(buffer, bh_arr_length(union_node->variants));
        bh_arr_each(AstUnionVariant*, puv, union_node->variants) {
            AstUnionVariant* uv = *puv;

            bh_buffer_clear(&type_buf);
            write_type_node(context, &type_buf, uv->type_node);

            write_string(buffer, uv->token->length, uv->token->text);
            write_string(buffer, type_buf.length, (char *) type_buf.data);
        }

        // Polymorph parameters
        bh_buffer_write_u32(buffer, bh_arr_length(poly_union_node->poly_params));
        bh_arr_each(AstPolyStructParam, param, poly_union_node->poly_params) {
            bh_buffer_clear(&type_buf);
            write_type_node(context, &type_buf, param->type_node);

            write_string(buffer, param->token->length, param->token->text);
            write_string(buffer, type_buf.length, (char *) type_buf.data);
            write_cstring(buffer, "");
        }

        // Constraints
        write_doc_constraints(context, buffer, &union_node->constraints, NULL);

        bh_buffer_free(&type_buf);
    }

    write_doc_methods(context, buffer, method_scope);

    return 1;
}

static b32 write_doc_enum(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *node) {
    AstEnumType *enum_node = (void *) node;

    write_entity_header(context, buffer, binding, node->token->pos);

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

static b32 write_doc_distinct_type(Context *context, bh_buffer *buffer, AstBinding *binding, AstNode *node) {
    AstDistinctType *distinct_node = (void *) node;

    write_entity_header(context, buffer, binding, node->token->pos);

    bh_buffer type_buf;
    bh_buffer_init(&type_buf, context->scratch_alloc, 256);
    write_type_node(context, &type_buf, distinct_node->base_type);
    write_string(buffer, type_buf.length, (char *) type_buf.data);
    bh_buffer_free(&type_buf);

    write_doc_methods(context, buffer, distinct_node->scope);

    return 1;
}

static void write_doc_entity_array(Context *context, bh_buffer *buffer, bh_arr(AstBinding *) arr,
    b32 (*write_doc)(Context *context, bh_buffer *buffer, AstBinding *, AstNode*),
    u32 offset_write_location) {
    *((u32 *) bh_pointer_add(buffer->data, offset_write_location)) = buffer->length;

    u32 count_patch = buffer->length;
    bh_buffer_write_u32(buffer, 0);

    u32 count = 0;
    bh_arr_each(AstBinding *, pbind, arr) {
        if (write_doc(context, buffer, *pbind, (*pbind)->node)) {
            count++;
        }
    }

    *((u32 *) bh_pointer_add(buffer->data, count_patch)) = count;
}

void onyx_docs_generate_odoc(Context *context, bh_buffer *out_buffer) {
    if (!context->doc_info) {
        out_buffer->data = NULL;
        out_buffer->length = 0;
        return;
    }

    bh_buffer doc_buffer;
    bh_buffer_init(&doc_buffer, context->gp_alloc, 16 * 1024);

    bh_buffer_append(&doc_buffer, Doc_Magic_Bytes, 4);
    bh_buffer_write_u32(&doc_buffer, 1);

    const char *program_name = "out.wasm";
    write_cstring(&doc_buffer, program_name);

    bh_buffer_write_u32(&doc_buffer, bh_time_curr() / 1000);

    int offset_table_index = doc_buffer.length;
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);
    bh_buffer_write_u32(&doc_buffer, 0);

    //
    // Package Info
    // 
    *((u32 *) bh_pointer_add(doc_buffer.data, offset_table_index + 0)) = doc_buffer.length;

    Table(Package *) packages = (void *) context->packages;
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

        bh_buffer_write_u32(&doc_buffer, bh_arr_length(p->doc_strings) + bh_arr_length(p->doc_string_tokens));
        bh_arr_each(OnyxToken *, ptkn, p->doc_string_tokens) {
            OnyxToken *tkn = *ptkn;
            write_string(&doc_buffer, tkn->length, tkn->text);
        }
        bh_arr_each(const char *, pstr, p->doc_strings) {
            write_cstring(&doc_buffer, *pstr);
        }
    }

    //
    // Procedure Info
    //
    write_doc_entity_array(context, &doc_buffer, context->doc_info->procedures, write_doc_procedure, offset_table_index + 4);


    //
    // Structure Info
    //
    write_doc_entity_array(context, &doc_buffer, context->doc_info->structures, write_doc_structure, offset_table_index + 8);


    //
    // Enum Info
    //
    write_doc_entity_array(context, &doc_buffer, context->doc_info->enumerations, write_doc_enum, offset_table_index + 12);


    //
    // Distinct Types Info
    //
    write_doc_entity_array(context, &doc_buffer, context->doc_info->distinct_types, write_doc_distinct_type, offset_table_index + 16);


    //
    // Union Info
    //
    write_doc_entity_array(context, &doc_buffer, context->doc_info->unions, write_doc_union_type, offset_table_index + 20);


    //
    // File Info
    //
    *((u32 *) bh_pointer_add(doc_buffer.data, offset_table_index + 24)) = doc_buffer.length;

    bh_buffer_write_u32(&doc_buffer, shlenu(context->doc_info->file_ids));
    fori (i, 0, shlen(context->doc_info->file_ids)) {
        const char *key = context->doc_info->file_ids[i].key;
        
        bh_buffer_write_u32(&doc_buffer, 0);
        write_cstring(&doc_buffer, key);
    }

    *out_buffer = doc_buffer;
}



