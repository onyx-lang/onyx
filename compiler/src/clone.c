#include "astnodes.h"
#include "parser.h"
#include "utils.h"

static inline b32 should_clone(Context *context, AstNode* node) {
    if (node->flags & Ast_Flag_No_Clone) return 0;

    if (context->cloner.dont_copy_structs) {
        if (node->kind == Ast_Kind_Struct_Type) return 0;
        if (node->kind == Ast_Kind_Function)    return 0;
        if (node->kind == Ast_Kind_Polymorphic_Proc) return 0;
    }

    switch (node->kind) {
        // List of nodes that should not be copied
        case Ast_Kind_Global:
        case Ast_Kind_Memres:
        case Ast_Kind_StrLit:
        case Ast_Kind_Package:
        case Ast_Kind_Overloaded_Function:
        case Ast_Kind_Alias:
        case Ast_Kind_Code_Block:
        case Ast_Kind_Macro:
        case Ast_Kind_Symbol:
        case Ast_Kind_Poly_Struct_Type:
        case Ast_Kind_Poly_Union_Type:
        case Ast_Kind_Basic_Type:
        case Ast_Kind_Enum_Type:
        case Ast_Kind_Enum_Value:
        case Ast_Kind_Directive_This_Package: // Not copied, because it should depend on where it was written, not where a macro/polymorphic function was expanded.
            return 0;

        default: return 1;
    }
}

static inline i32 ast_kind_to_size(AstNode* node) {
    switch (node->kind) {
        case Ast_Kind_Error: return sizeof(AstNode);
        case Ast_Kind_Package: return sizeof(AstPackage);
        case Ast_Kind_Load_File: return sizeof(AstInclude);
        case Ast_Kind_Load_Path: return sizeof(AstInclude);
        case Ast_Kind_Load_All: return sizeof(AstInclude);
        case Ast_Kind_Memres: return sizeof(AstMemRes);
        case Ast_Kind_Binding: return sizeof(AstBinding);
        case Ast_Kind_Function: return sizeof(AstFunction);
        case Ast_Kind_Overloaded_Function: return sizeof(AstOverloadedFunction);
        case Ast_Kind_Polymorphic_Proc: return sizeof(AstFunction);
        case Ast_Kind_Block: return sizeof(AstBlock);
        case Ast_Kind_Local: return sizeof(AstLocal);
        case Ast_Kind_Global: return sizeof(AstGlobal);
        case Ast_Kind_Symbol: return sizeof(AstNode);
        case Ast_Kind_Unary_Op: return sizeof(AstUnaryOp);
        case Ast_Kind_Binary_Op: return sizeof(AstBinaryOp);
        case Ast_Kind_Type_Start: return 0;
        case Ast_Kind_Type: return sizeof(AstType);
        case Ast_Kind_Basic_Type: return sizeof(AstBasicType);
        case Ast_Kind_Pointer_Type: return sizeof(AstPointerType);
        case Ast_Kind_Multi_Pointer_Type: return sizeof(AstMultiPointerType);
        case Ast_Kind_Function_Type: return sizeof(AstFunctionType) + ((AstFunctionType *) node)->param_count * sizeof(AstType *);
        case Ast_Kind_Array_Type: return sizeof(AstArrayType);
        case Ast_Kind_Slice_Type: return sizeof(AstSliceType);
        case Ast_Kind_DynArr_Type: return sizeof(AstDynArrType);
        case Ast_Kind_VarArg_Type: return sizeof(AstVarArgType);
        case Ast_Kind_Struct_Type: return sizeof(AstStructType);
        case Ast_Kind_Poly_Struct_Type: return sizeof(AstPolyStructType);
        case Ast_Kind_Poly_Call_Type: return sizeof(AstPolyCallType);
        case Ast_Kind_Enum_Type: return sizeof(AstEnumType);
        case Ast_Kind_Type_Alias: return sizeof(AstTypeAlias);
        case Ast_Kind_Type_Raw_Alias: return sizeof(AstTypeRawAlias);
        case Ast_Kind_Type_Compound: return sizeof(AstCompoundType);
        case Ast_Kind_Typeof: return sizeof(AstTypeOf);
        case Ast_Kind_Type_End: return 0;
        case Ast_Kind_Struct_Member: return sizeof(AstStructMember);
        case Ast_Kind_Enum_Value: return sizeof(AstEnumValue);
        case Ast_Kind_NumLit: return sizeof(AstNumLit);
        case Ast_Kind_StrLit: return sizeof(AstStrLit);
        case Ast_Kind_Param: return sizeof(AstLocal);
        case Ast_Kind_Argument: return sizeof(AstArgument);
        case Ast_Kind_Call: return sizeof(AstCall);
        case Ast_Kind_Intrinsic_Call: return sizeof(AstCall);
        case Ast_Kind_Return: return sizeof(AstReturn);
        case Ast_Kind_Address_Of: return sizeof(AstAddressOf);
        case Ast_Kind_Dereference: return sizeof(AstDereference);
        case Ast_Kind_Subscript: return sizeof(AstSubscript);
        case Ast_Kind_Slice: return sizeof(AstSubscript);
        case Ast_Kind_Field_Access: return sizeof(AstFieldAccess);
        case Ast_Kind_Unary_Field_Access: return sizeof(AstUnaryFieldAccess);
        case Ast_Kind_Pipe: return sizeof(AstBinaryOp);
        case Ast_Kind_Range_Literal: return sizeof(AstRangeLiteral);
        case Ast_Kind_Method_Call: return sizeof(AstBinaryOp);
        case Ast_Kind_Size_Of: return sizeof(AstSizeOf);
        case Ast_Kind_Align_Of: return sizeof(AstAlignOf);
        case Ast_Kind_File_Contents: return sizeof(AstFileContents);
        case Ast_Kind_Struct_Literal: return sizeof(AstStructLiteral);
        case Ast_Kind_Array_Literal: return sizeof(AstArrayLiteral);
        case Ast_Kind_If: return sizeof(AstIfWhile);
        case Ast_Kind_For: return sizeof(AstFor);
        case Ast_Kind_While: return sizeof(AstIfWhile);
        case Ast_Kind_Jump: return sizeof(AstJump);
        case Ast_Kind_Defer: return sizeof(AstDefer);
        case Ast_Kind_Switch: return sizeof(AstSwitch);
        case Ast_Kind_Switch_Case: return sizeof(AstSwitchCase);
        case Ast_Kind_Directive_Solidify: return sizeof(AstDirectiveSolidify);
        case Ast_Kind_Compound: return sizeof(AstCompound);
        case Ast_Kind_Named_Value: return sizeof(AstNamedValue);
        case Ast_Kind_Call_Site: return sizeof(AstCallSite);
        case Ast_Kind_Static_If: return sizeof(AstIfWhile);
        case Ast_Kind_If_Expression: return sizeof(AstIfExpression);
        case Ast_Kind_Directive_Insert: return sizeof(AstDirectiveInsert);
        case Ast_Kind_Directive_Defined: return sizeof(AstDirectiveDefined);
        case Ast_Kind_Do_Block: return sizeof(AstDoBlock);
        case Ast_Kind_Constraint: return sizeof(AstConstraint);
        case Ast_Kind_Directive_Remove: return sizeof(AstDirectiveRemove);
        case Ast_Kind_Directive_First: return sizeof(AstDirectiveFirst);
        case Ast_Kind_Directive_Export_Name: return sizeof(AstDirectiveExportName);
        case Ast_Kind_Import: return sizeof(AstImport);
        case Ast_Kind_Capture_Block: return sizeof(AstCaptureBlock);
        case Ast_Kind_Capture_Local: return sizeof(AstCaptureLocal);
        case Ast_Kind_Union_Type: return sizeof(AstUnionType);
        case Ast_Kind_Union_Variant: return sizeof(AstUnionVariant);
        case Ast_Kind_Procedural_Expansion: return sizeof(AstProceduralExpansion);

        default: break;
    }

    return 0;
}

AstNode* ast_clone_with_captured_entities(Context *context, void* n, bh_arr(AstNode *)* ents) {
    context->cloner.captured_entities = *ents;

    AstNode* cloned = ast_clone(context, n);

    *ents = context->cloner.captured_entities;
    context->cloner.captured_entities = NULL;
    return cloned;
}

AstNode* ast_clone_list(Context *context, void* n) {
    AstNode* node = (AstNode *) n;
    if (node == NULL) return NULL;

    AstNode* root = ast_clone(context, node);
    AstNode* curr = root->next;
    AstNode** insertion = &root->next;

    while (curr != NULL) {
        curr = ast_clone(context, curr);
        *insertion = curr;
        insertion = &curr->next;
        curr = curr->next;
    }

    return root;
}

#define E(ent) do { \
    assert(context->cloner.captured_entities); \
    ent->entity = NULL; \
    bh_arr_push(context->cloner.captured_entities, (AstNode *) ent); \
    } while (0);
    

#define C(nt, mname) ((nt *) nn)->mname = (void *) ast_clone(context, ((nt *) node)->mname);

// NOTE: Using void* to avoid a lot of unnecessary casting
AstNode* ast_clone(Context *context, void* n) {
    AstNode* node = (AstNode *) n;

    if (node == NULL) return NULL;
    if (!should_clone(context, node)) return node;

    context->cloner.clone_depth++;

    i32 node_size = ast_kind_to_size(node);
    // bh_printf("Cloning %s with size %d\n", onyx_ast_node_kind_string(node->kind), node_size);

    AstNode* nn = onyx_ast_node_new(context->ast_alloc, node_size, node->kind);
    memmove(nn, node, node_size);

    nn->flags &= ~(Ast_Flag_Has_Been_Checked | Ast_Flag_Has_Been_Symres);

    switch ((u16) node->kind) {
        case Ast_Kind_Binary_Op:
        case Ast_Kind_Pipe:
        case Ast_Kind_Method_Call:
            C(AstBinaryOp, left);
            C(AstBinaryOp, right);
            break;

        case Ast_Kind_Unary_Op:
            C(AstUnaryOp, expr);
            C(AstUnaryOp, type_node);
            break;

        case Ast_Kind_Param:
        case Ast_Kind_Local:
            C(AstLocal, type_node);
            break;

        case Ast_Kind_Call:
            C(AstCall, callee);
            arguments_deep_clone(context, &((AstCall *) nn)->args, &((AstCall *) node)->args);
            break;

        case Ast_Kind_Argument:
            C(AstArgument, value);
            break;

        case Ast_Kind_Address_Of:
            C(AstAddressOf, expr);
            break;

        case Ast_Kind_Dereference:
            C(AstDereference, expr);
            break;

        case Ast_Kind_Slice:
        case Ast_Kind_Subscript:
            C(AstSubscript, addr);
            C(AstSubscript, expr);
            break;

        case Ast_Kind_Field_Access:
            C(AstFieldAccess, expr);
            break;

        case Ast_Kind_Size_Of:
            C(AstSizeOf, so_ast_type);
            break;

        case Ast_Kind_Align_Of:
            C(AstAlignOf, ao_ast_type);
            break;

        case Ast_Kind_Struct_Literal: {
            AstStructLiteral* st = (AstStructLiteral *) node;
            AstStructLiteral* dt = (AstStructLiteral *) nn;

            dt->stnode = (AstTyped *) ast_clone(context, st->stnode);

            arguments_deep_clone(context, &dt->args, &st->args);
            break;
        }

        case Ast_Kind_Array_Literal: {
            AstArrayLiteral* st = (AstArrayLiteral *) node;
            AstArrayLiteral* dt = (AstArrayLiteral *) nn;

            dt->atnode = (AstTyped *) ast_clone(context, st->atnode);

            dt->values = NULL;
            bh_arr_new(context->gp_alloc, dt->values, bh_arr_length(st->values));
            bh_arr_each(AstTyped *, val, st->values)
                bh_arr_push(dt->values, (AstTyped *) ast_clone(context, *val));

            break;
        }

        case Ast_Kind_Range_Literal:
            C(AstRangeLiteral, low);
            C(AstRangeLiteral, high);
            C(AstRangeLiteral, step);
            break;

        case Ast_Kind_Return:
            C(AstReturn, expr);
            break;

        case Ast_Kind_Block:
            ((AstBlock *) nn)->body = ast_clone_list(context, ((AstBlock *) node)->body);
            ((AstBlock *) nn)->quoted_block_capture_scope = NULL;
            break;

        case Ast_Kind_Defer:
            C(AstDefer, stmt);
            break;

        case Ast_Kind_For: {
            AstFor* sf = (AstFor *) node;
            AstFor* df = (AstFor *) nn;

            df->indexing_variables = NULL;
            bh_arr_new(context->gp_alloc, df->indexing_variables, bh_arr_length(sf->indexing_variables));
            bh_arr_each(AstLocal *, iv, sf->indexing_variables)
                bh_arr_push(df->indexing_variables, (AstLocal *) ast_clone(context, (AstTyped *) *iv));

            C(AstFor, iter);
            break;
        }

        case Ast_Kind_If:
        case Ast_Kind_While:
            ((AstIfWhile *) nn)->initialization = ast_clone_list(context, ((AstIfWhile *) node)->initialization);
            //fallthrough

        case Ast_Kind_Static_If:
            C(AstIfWhile, cond);

            C(AstIfWhile, true_stmt);
            C(AstIfWhile, false_stmt);

            if (nn->kind == Ast_Kind_Static_If) {
                ((AstIfWhile *) node)->flags |= Ast_Flag_Dead;
                ((AstIfWhile *) node)->flags |= Ast_Flag_Static_If_Resolved;

                ((AstIfWhile *) nn)->flags &= ~Ast_Flag_Dead;
                ((AstIfWhile *) nn)->flags &= ~Ast_Flag_Static_If_Resolved;
                E(nn);
            }

            break;

        case Ast_Kind_Switch_Case: {
            C(AstSwitchCase, capture);
            C(AstSwitchCase, block);

            AstSwitchCase *dw = (AstSwitchCase *) nn;
            AstSwitchCase *sw = (AstSwitchCase *) node;

            dw->values = NULL;
            bh_arr_new(context->gp_alloc, dw->values, bh_arr_length(sw->values));
            bh_arr_each(AstTyped *, value, sw->values)
                bh_arr_push(dw->values, (AstTyped *) ast_clone(context, *value));

            break;
        }

        case Ast_Kind_Switch: {
            AstSwitch* dw = (AstSwitch *) nn;
            AstSwitch* sw = (AstSwitch *) node;

            dw->initialization = ast_clone_list(context, sw->initialization);
            C(AstSwitch, expr);


            dw->cases = NULL;
            C(AstSwitch, case_block);
            break;
        }

        case Ast_Kind_Pointer_Type:
            C(AstPointerType, elem);
            break;

        case Ast_Kind_Multi_Pointer_Type:
            C(AstMultiPointerType, elem);
            break;

        case Ast_Kind_Array_Type:
            C(AstArrayType, count_expr);
            C(AstArrayType, elem);
            break;

        case Ast_Kind_Slice_Type:
            C(AstSliceType, elem);
            break;

        case Ast_Kind_DynArr_Type:
            C(AstDynArrType, elem);
            break;

        case Ast_Kind_VarArg_Type:
            C(AstVarArgType, elem);
            break;

        case Ast_Kind_Type_Alias:
            C(AstTypeAlias, to);
            break;

        case Ast_Kind_Struct_Type: {
            AstStructType* ds = (AstStructType *) nn;
            AstStructType* ss = (AstStructType *) node;

            ds->members = NULL;
            bh_arr_new(context->gp_alloc, ds->members, bh_arr_length(ss->members));

            bh_arr_each(AstStructMember *, smem, ss->members) {
                bh_arr_push(ds->members, (AstStructMember *) ast_clone(context, *smem));
            }

            ds->meta_tags = NULL;
            bh_arr_new(context->gp_alloc, ds->meta_tags, bh_arr_length(ss->meta_tags));
            bh_arr_each(AstTyped *, tag, ss->meta_tags) {
                bh_arr_push(ds->meta_tags, (AstTyped *) ast_clone(context, *tag));
            }
            
            if (ss->constraints.constraints) {
                memset(&ds->constraints, 0, sizeof(ConstraintContext));
                bh_arr_new(context->gp_alloc, ds->constraints.constraints, bh_arr_length(ss->constraints.constraints));

                bh_arr_each(AstConstraint *, constraint, ss->constraints.constraints) {
                    bh_arr_push(ds->constraints.constraints, (AstConstraint *) ast_clone(context, (AstNode *) *constraint));
                }
            }

            ds->stcache = NULL;
            break;
        }

        case Ast_Kind_Struct_Member: {
            C(AstStructMember, type_node);
            C(AstStructMember, initial_value);

            AstStructMember *ds = (AstStructMember *) nn;
            AstStructMember *ss = (AstStructMember *) node;

            ds->meta_tags = NULL;
            bh_arr_new(context->gp_alloc, ds->meta_tags, bh_arr_length(ss->meta_tags));
            bh_arr_each(AstTyped *, tag, ss->meta_tags) {
                bh_arr_push(ds->meta_tags, (AstTyped *) ast_clone(context, *tag));
            }

            break;
        }

        case Ast_Kind_Union_Type: {
            AstUnionType* du = (AstUnionType *) nn;
            AstUnionType* su = (AstUnionType *) node;

            du->variants = NULL;
            bh_arr_new(context->gp_alloc, du->variants, bh_arr_length(su->variants));

            bh_arr_each(AstUnionVariant *, uv, su->variants) {
                bh_arr_push(du->variants, (AstUnionVariant *) ast_clone(context, *uv));
            }

            du->meta_tags = NULL;
            bh_arr_new(context->gp_alloc, du->meta_tags, bh_arr_length(su->meta_tags));
            bh_arr_each(AstTyped *, tag, su->meta_tags) {
                bh_arr_push(du->meta_tags, (AstTyped *) ast_clone(context, *tag));
            }
            
            if (su->constraints.constraints) {
                memset(&du->constraints, 0, sizeof(ConstraintContext));
                bh_arr_new(context->gp_alloc, du->constraints.constraints, bh_arr_length(su->constraints.constraints));

                bh_arr_each(AstConstraint *, constraint, su->constraints.constraints) {
                    bh_arr_push(du->constraints.constraints, (AstConstraint *) ast_clone(context, (AstNode *) *constraint));
                }
            }

            du->utcache = NULL;
            break;
        }

        case Ast_Kind_Union_Variant: {
            C(AstUnionVariant, type_node);

            AstUnionVariant *du = (AstUnionVariant *) nn;
            AstUnionVariant *su = (AstUnionVariant *) node;

            du->meta_tags = NULL;
            bh_arr_new(context->gp_alloc, du->meta_tags, bh_arr_length(su->meta_tags));
            bh_arr_each(AstTyped *, tag, su->meta_tags) {
                bh_arr_push(du->meta_tags, (AstTyped *) ast_clone(context, *tag));
            }

            break;
        }

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pcd = (AstPolyCallType *) nn;
            AstPolyCallType* pcs = (AstPolyCallType *) node;

            pcd->callee = (AstType *) ast_clone(context, pcs->callee);
            pcd->params = NULL;
            bh_arr_new(context->gp_alloc, pcd->params, bh_arr_length(pcs->params));

            bh_arr_each(AstNode *, param, pcs->params) {
                bh_arr_push(pcd->params, ast_clone(context, *param));
            }

            break;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* cd = (AstCompoundType *) nn;
            AstCompoundType* cs = (AstCompoundType *) node;

            cd->types = NULL;
            bh_arr_new(context->gp_alloc, cd->types, bh_arr_length(cs->types));

            bh_arr_each(AstType *, type, cs->types) {
                bh_arr_push(cd->types, (AstType *) ast_clone(context, (AstNode *) *type));
            }
            break;
        }

        case Ast_Kind_Function_Type:
            C(AstFunctionType, return_type);
            ((AstFunctionType *) nn)->param_count = ((AstFunctionType *) node)->param_count;
            fori (i, 0, (i64) ((AstFunctionType *) nn)->param_count) {
                ((AstFunctionType *) nn)->params[i] = (AstType *) ast_clone(context, ((AstFunctionType *) node)->params[i]);
            }
            break;

        case Ast_Kind_Binding:
            bh_printf("Cloning binding: %b\n", node->token->text, node->token->length);
            C(AstTyped, type_node);
            C(AstBinding, node);
            break;

        case Ast_Kind_Function:
        case Ast_Kind_Polymorphic_Proc: {
            AstFunction* df = (AstFunction *) nn;
            AstFunction* sf = (AstFunction *) node;

            // Check if we are cloning a function inside of a function.
            if (context->cloner.clone_depth > 1) {
                // If we are, and the inner function has a scope, this means that
                // the inner function does not capture anything, and is not polymorphic.
                // Therefore, it should be treated as a normal function and not cloned
                // inside of this function.
                // 
                // If the inner function does not have a scope, that means that it is
                // either polymorphic and/or it has captures. In either case, we have
                // to clone the function internally below.
                if (df->scope != NULL) {
                    context->cloner.clone_depth--;
                    return node;
                }
            }
            else {
                convert_polyproc_to_function(df);
            }

            if (sf->is_foreign) return node;
            assert(df->scope == NULL);

            df->nodes_that_need_entities_after_clone = NULL;
            bh_arr_new(context->gp_alloc, df->nodes_that_need_entities_after_clone, 1);

            bh_arr(AstNode *) old_captured_entities = context->cloner.captured_entities;
            context->cloner.captured_entities = df->nodes_that_need_entities_after_clone;

            df->return_type = (AstType *) ast_clone(context, sf->return_type);
            df->body = (AstBlock *) ast_clone(context, sf->body);
            df->captures = (AstCaptureBlock *) ast_clone(context, sf->captures);

            df->nodes_that_need_entities_after_clone = context->cloner.captured_entities;
            context->cloner.captured_entities = old_captured_entities;

            df->params = NULL;
            bh_arr_new(context->ast_alloc, df->params, bh_arr_length(sf->params));

            bh_arr_each(AstParam, param, sf->params) {
                AstParam new_param = { 0 };

                context->cloner.dont_copy_structs = 1;
                new_param.local = (AstLocal *) ast_clone(context, param->local);
                new_param.local->flags &= ~Ast_Flag_Param_Symbol_Dirty;
                new_param.default_value = (AstTyped *) ast_clone(context, param->default_value);
                new_param.use_processed = 0;
                context->cloner.dont_copy_structs = 0;

                new_param.vararg_kind = param->vararg_kind;
                new_param.is_used = param->is_used;
                bh_arr_push(df->params, new_param);
            }

            df->named_return_locals = NULL;
            if (sf->named_return_locals) {
                bh_arr_new(context->ast_alloc, df->named_return_locals, bh_arr_length(sf->named_return_locals));
                bh_arr_each(AstLocal *, named_return, sf->named_return_locals) {
                    bh_arr_push(df->named_return_locals, (AstLocal *) ast_clone(context, (AstNode *) *named_return));
                }
            }

            if (sf->constraints.constraints) {
                memset(&df->constraints, 0, sizeof(ConstraintContext));
                bh_arr_new(context->ast_alloc, df->constraints.constraints, bh_arr_length(sf->constraints.constraints));

                bh_arr_each(AstConstraint *, constraint, sf->constraints.constraints) {
                    bh_arr_push(df->constraints.constraints, (AstConstraint *) ast_clone(context, (AstNode *) *constraint));
                }
            }

            if (sf->tags) {
                bh_arr_new(context->ast_alloc, df->tags, bh_arr_length(sf->tags));
                bh_arr_each(AstTyped *, pexpr, sf->tags) {
                    bh_arr_push(df->tags, (AstTyped *) ast_clone(context, (AstNode *) *pexpr));
                }    
            }

            if (df->kind == Ast_Kind_Polymorphic_Proc) {
                df->scope_to_lookup_captured_values = NULL;
            }

            if (context->cloner.clone_depth > 1 && context->cloner.captured_entities) {
                sf->flags |= Ast_Flag_Function_Is_Lambda_Inside_PolyProc;
                df->flags &= ~Ast_Flag_Function_Is_Lambda_Inside_PolyProc;
                E(df);
            }

            break;
        }

        case Ast_Kind_Constraint: {
            C(AstConstraint, interface);

            AstConstraint* dc = (AstConstraint *) nn;
            AstConstraint* sc = (AstConstraint *) node;

            dc->args = NULL;
            bh_arr_new(context->gp_alloc, dc->args, bh_arr_length(sc->args));

            bh_arr_each(AstTyped *, arg, sc->args) {
                bh_arr_push(dc->args, (AstTyped *) ast_clone(context, (AstNode *) *arg));
            }

            dc->phase = Constraint_Phase_Waiting_To_Be_Queued;
            break;
        }

        case Ast_Kind_Directive_Solidify: {
            AstDirectiveSolidify* dd = (AstDirectiveSolidify *) nn;
            AstDirectiveSolidify* sd = (AstDirectiveSolidify *) node;

            dd->poly_proc = (AstFunction *) ast_clone(context, (AstNode *) sd->poly_proc);
            dd->resolved_proc = NULL;

            dd->known_polyvars = NULL;
            bh_arr_new(context->gp_alloc, dd->known_polyvars, bh_arr_length(sd->known_polyvars));

            bh_arr_each(AstPolySolution, sln, sd->known_polyvars) {
                AstPolySolution new_sln;
                new_sln.kind     = sln->kind;
                new_sln.poly_sym = (AstNode *) ast_clone(context, (AstNode *) sln->poly_sym);
                new_sln.ast_type = (AstType *) ast_clone(context, (AstNode *) sln->ast_type);
                bh_arr_push(dd->known_polyvars, new_sln);
            }

            break;
        }

        case Ast_Kind_Compound: {
            AstCompound* cd = (AstCompound *) nn;
            AstCompound* cs = (AstCompound *) node;

            cd->exprs = NULL;
            bh_arr_new(context->gp_alloc, cd->exprs, bh_arr_length(cs->exprs));

            bh_arr_each(AstTyped *, expr, cs->exprs) {
                bh_arr_push(cd->exprs, (AstTyped *) ast_clone(context, (AstNode *) *expr));
            }
            break;
        }

        case Ast_Kind_Named_Value:
            C(AstNamedValue, value);
            break;

        case Ast_Kind_If_Expression:
            C(AstIfExpression, cond);
            C(AstIfExpression, true_expr);
            C(AstIfExpression, false_expr);
            break;

        case Ast_Kind_Directive_Insert:
            C(AstDirectiveInsert, code_expr);

            AstDirectiveInsert* id = (AstDirectiveInsert *) nn;
            AstDirectiveInsert* is = (AstDirectiveInsert *) node;
            id->binding_exprs = NULL;
            bh_arr_new(context->gp_alloc, id->binding_exprs, bh_arr_length(is->binding_exprs));

            bh_arr_each(AstTyped *, expr, is->binding_exprs) {
                bh_arr_push(id->binding_exprs, (AstTyped *) ast_clone(context, (AstNode *) *expr));
            }
            break;

        case Ast_Kind_Directive_Defined:
            C(AstDirectiveDefined, expr);
            ((AstDirectiveDefined *) nn)->is_defined = 0;
            break;

        case Ast_Kind_Typeof:
            C(AstTypeOf, expr);
            ((AstTypeOf *) nn)->resolved_type = NULL;
            break;

        case Ast_Kind_Do_Block:
            C(AstDoBlock, block);
            C(AstDoBlock, type_node);
            break;

        case Ast_Kind_File_Contents:
            C(AstFileContents, filename_expr);
            E(nn);
            break;

        case Ast_Kind_Directive_Export_Name:
            C(AstDirectiveExportName, func);
            break;

        case Ast_Kind_Capture_Block: {
            AstCaptureBlock* cd = (AstCaptureBlock *) nn;
            AstCaptureBlock* cs = (AstCaptureBlock *) node;

            cd->captures = NULL;
            bh_arr_new(context->gp_alloc, cd->captures, bh_arr_length(cs->captures));

            bh_arr_each(AstCaptureLocal *, expr, cs->captures) {
                bh_arr_push(cd->captures, (AstCaptureLocal *) ast_clone(context, (AstNode *) *expr));
            }
            break;
        }

        case Ast_Kind_Capture_Local:
            C(AstCaptureLocal, type_node);
            break;

        case Ast_Kind_Procedural_Expansion:
            C(AstProceduralExpansion, proc_macro);
            break;
    }

    context->cloner.clone_depth--;
    return nn;
}

#undef C

AstFunction* clone_function_header(Context *context, AstFunction* func) {
    if (func->kind != Ast_Kind_Function && func->kind != Ast_Kind_Polymorphic_Proc) return NULL;

    if (func->is_foreign) return func;

    AstFunction* new_func = onyx_ast_node_new(context->ast_alloc, sizeof(AstFunction), func->kind);
    memmove(new_func, func, sizeof(AstFunction));
    assert(new_func->scope == NULL);

    convert_polyproc_to_function(new_func);

    new_func->return_type = (AstType *) ast_clone(context, func->return_type);

    new_func->params = NULL;
    bh_arr_new(context->gp_alloc, new_func->params, bh_arr_length(func->params));
    bh_arr_each(AstParam, param, func->params) {
        AstParam new_param;

        context->cloner.dont_copy_structs = 1;
        new_param.local = (AstLocal *) ast_clone(context, param->local);
        new_param.local->flags &= ~Ast_Flag_Param_Symbol_Dirty;
        new_param.default_value = (AstTyped *) ast_clone(context, param->default_value);
        new_param.use_processed = 0;
        context->cloner.dont_copy_structs = 0;

        new_param.vararg_kind = param->vararg_kind;
        new_param.is_used = param->is_used;
        bh_arr_push(new_func->params, new_param);
    }

    if (func->constraints.constraints) {
        memset(&new_func->constraints, 0, sizeof(ConstraintContext));
        bh_arr_new(context->gp_alloc, new_func->constraints.constraints, bh_arr_length(func->constraints.constraints));

        bh_arr_each(AstConstraint *, constraint, func->constraints.constraints) {
            bh_arr_push(new_func->constraints.constraints, (AstConstraint *) ast_clone(context, (AstNode *) *constraint));
        }
    }

    return new_func;
}

// Clones a function body from a given function. It is assumed that `dest` is
// a function from `clone_function_header`.
void clone_function_body(Context *context, AstFunction* dest, AstFunction* source) {
    if (dest->kind != Ast_Kind_Function) return;
    if (source->kind != Ast_Kind_Polymorphic_Proc && source->kind != Ast_Kind_Function) return;

    dest->nodes_that_need_entities_after_clone = NULL;
    bh_arr_new(context->gp_alloc, dest->nodes_that_need_entities_after_clone, 1);
    context->cloner.captured_entities = dest->nodes_that_need_entities_after_clone;

    dest->body = (AstBlock *) ast_clone(context, source->body);
    
    dest->nodes_that_need_entities_after_clone = context->cloner.captured_entities;
    context->cloner.captured_entities = NULL;
}
