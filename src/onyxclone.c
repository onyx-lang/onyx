#include "onyxastnodes.h"
#include "onyxparser.h"
#include "onyxutils.h"

static inline b32 should_clone(AstNode* node) {
    if (node->flags & Ast_Flag_No_Clone) return 0;

    switch (node->kind) {
        // List of nodes that should not be copied
        case Ast_Kind_Global:
        case Ast_Kind_Memres:
        case Ast_Kind_StrLit:
        case Ast_Kind_Package:
        case Ast_Kind_Enum_Type:
        case Ast_Kind_Enum_Value:
        case Ast_Kind_Overloaded_Function:
        case Ast_Kind_Polymorphic_Proc:
        case Ast_Kind_Alias:
        case Ast_Kind_Code_Block:
        case Ast_Kind_Macro:
        case Ast_Kind_File_Contents:
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
        case Ast_Kind_Memres: return sizeof(AstMemRes);
        case Ast_Kind_Binding: return sizeof(AstBinding);
        case Ast_Kind_Function: return sizeof(AstFunction);
        case Ast_Kind_Overloaded_Function: return sizeof(AstOverloadedFunction);
        case Ast_Kind_Polymorphic_Proc: return sizeof(AstPolyProc);
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
        case Ast_Kind_Use: return sizeof(AstUse);
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
        case Ast_Kind_Do_Block: return sizeof(AstDoBlock);
        case Ast_Kind_Count: return 0;
    }

    return 0;
}

AstNode* ast_clone_list(bh_allocator a, void* n) {
    AstNode* node = (AstNode *) n;
    if (node == NULL) return NULL;

    AstNode* root = ast_clone(a, node);
    AstNode* curr = root->next;
    AstNode** insertion = &root->next;

    while (curr != NULL) {
        curr = ast_clone(a, curr);
        *insertion = curr;
        insertion = &curr->next;
        curr = curr->next;
    }

    return root;
}

#define C(nt, mname) ((nt *) nn)->mname = (void *) ast_clone(a, ((nt *) node)->mname);

// NOTE: Using void* to avoid a lot of unnecessary casting
AstNode* ast_clone(bh_allocator a, void* n) {
    AstNode* node = (AstNode *) n;

    if (node == NULL) return NULL;
    if (!should_clone(node)) return node;

    static int clone_depth = 0;
    clone_depth++;

    i32 node_size = ast_kind_to_size(node);
    // bh_printf("Cloning %s with size %d\n", onyx_ast_node_kind_string(node->kind), node_size);

    AstNode* nn = onyx_ast_node_new(a, node_size, node->kind);
    memmove(nn, node, node_size);

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
            arguments_deep_clone(a, &((AstCall *) nn)->args, &((AstCall *) node)->args);
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

            dt->stnode = (AstTyped *) ast_clone(a, st->stnode);

            arguments_deep_clone(a, &dt->args, &st->args);
            break;
        }

        case Ast_Kind_Array_Literal: {
            AstArrayLiteral* st = (AstArrayLiteral *) node;
            AstArrayLiteral* dt = (AstArrayLiteral *) nn;

            dt->atnode = (AstTyped *) ast_clone(a, st->atnode);

            dt->values = NULL;
            bh_arr_new(global_heap_allocator, dt->values, bh_arr_length(st->values));
            bh_arr_each(AstTyped *, val, st->values)
                bh_arr_push(dt->values, (AstTyped *) ast_clone(a, *val));

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
            ((AstBlock *) nn)->body = ast_clone_list(a, ((AstBlock *) node)->body);
            break;

        case Ast_Kind_Defer:
            C(AstDefer, stmt);
            break;

        case Ast_Kind_For:
            C(AstFor, var);
            C(AstFor, iter);
            C(AstFor, stmt);
            break;

        case Ast_Kind_If:
        case Ast_Kind_While:
            C(AstIfWhile, local);
            C(AstIfWhile, assignment);

            if (((AstIfWhile *) nn)->assignment)
                ((AstIfWhile *) nn)->assignment->left = (AstTyped *) ((AstIfWhile *) nn)->local;

            C(AstIfWhile, cond);
            //fallthrough

        case Ast_Kind_Static_If:
            C(AstIfWhile, true_stmt);
            C(AstIfWhile, false_stmt);
            break;

        case Ast_Kind_Switch: {
            AstSwitch* dw = (AstSwitch *) nn;
            AstSwitch* sw = (AstSwitch *) node;

            dw->local = (AstLocal *) ast_clone(a, sw->local);
            dw->assignment = (AstBinaryOp *) ast_clone(a, sw->assignment);
            if (dw->assignment)
                dw->assignment->left = (AstTyped *) sw->local;
            dw->expr = (AstTyped *) ast_clone(a, sw->expr);

            dw->default_case = (AstBlock *) ast_clone(a, sw->default_case);

            dw->cases = NULL;
            bh_arr_new(global_heap_allocator, dw->cases, bh_arr_length(sw->cases));

            bh_arr_each(AstSwitchCase, c, sw->cases) {
                bh_arr(AstTyped *) new_values = NULL;
                bh_arr_new(global_heap_allocator, new_values, bh_arr_length(c->values));
                bh_arr_each(AstTyped *, value, c->values)
                    bh_arr_push(new_values, (AstTyped *) ast_clone(a, *value));

                AstSwitchCase sc;
                sc.values = new_values; 
                sc.block = (AstBlock *) ast_clone(a, c->block);
                bh_arr_push(dw->cases, sc);
            }
            break;
        }

        case Ast_Kind_Pointer_Type:
            C(AstPointerType, elem);
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
            bh_arr_new(global_heap_allocator, ds->members, bh_arr_length(ss->members));

            bh_arr_each(AstStructMember *, smem, ss->members) {
                bh_arr_push(ds->members, (AstStructMember *) ast_clone(a, *smem));
            }

            ds->stcache = NULL;
            break;
        }

        case Ast_Kind_Struct_Member:
            C(AstStructMember, type_node);
            C(AstStructMember, initial_value);
            break;

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pcd = (AstPolyCallType *) nn;
            AstPolyCallType* pcs = (AstPolyCallType *) node;

            pcd->callee = (AstType *) ast_clone(a, pcs->callee);
            pcd->params = NULL;
            bh_arr_new(global_heap_allocator, pcd->params, bh_arr_length(pcs->params));

            bh_arr_each(AstNode *, param, pcs->params) {
                bh_arr_push(pcd->params, ast_clone(a, *param));
            }

            break;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* cd = (AstCompoundType *) nn;
            AstCompoundType* cs = (AstCompoundType *) node;

            cd->types = NULL;
            bh_arr_new(global_heap_allocator, cd->types, bh_arr_length(cs->types));

            bh_arr_each(AstType *, type, cs->types) {
                bh_arr_push(cd->types, (AstType *) ast_clone(a, (AstNode *) *type));
            }
            break;
        }

        case Ast_Kind_Function_Type:
            C(AstFunctionType, return_type);
            ((AstFunctionType *) nn)->param_count = ((AstFunctionType *) node)->param_count;
            fori (i, 0, (i64) ((AstFunctionType *) nn)->param_count) {
                ((AstFunctionType *) nn)->params[i] = (AstType *) ast_clone(a, ((AstFunctionType *) node)->params[i]);
            }
            break;

        case Ast_Kind_Binding:
            bh_printf("Cloning binding: %b\n", node->token->text, node->token->length);
            C(AstTyped, type_node);
            C(AstBinding, node);
            break;

        case Ast_Kind_Function: {
            if (clone_depth > 1) {
                clone_depth--;
                return node;
            }

            AstFunction* df = (AstFunction *) nn;
            AstFunction* sf = (AstFunction *) node;

            if (sf->flags & Ast_Flag_Foreign) return node;

            df->return_type = (AstType *) ast_clone(a, sf->return_type);
            df->body = (AstBlock *) ast_clone(a, sf->body);

            df->params = NULL;
            bh_arr_new(global_heap_allocator, df->params, bh_arr_length(sf->params));

            bh_arr_each(AstParam, param, sf->params) {
                AstParam new_param = { 0 };
                new_param.local = (AstLocal *) ast_clone(a, param->local);
                new_param.default_value = (AstTyped *) ast_clone(a, param->default_value);
                new_param.vararg_kind = param->vararg_kind;
                bh_arr_push(df->params, new_param);
            }

            break;
        }

        case Ast_Kind_Use:
            C(AstUse, expr);
            break;

        case Ast_Kind_Directive_Solidify: {
            AstDirectiveSolidify* dd = (AstDirectiveSolidify *) nn;
            AstDirectiveSolidify* sd = (AstDirectiveSolidify *) node;

            dd->poly_proc = (AstPolyProc *) ast_clone(a, (AstNode *) sd->poly_proc);
            dd->resolved_proc = NULL;

            dd->known_polyvars = NULL;
            bh_arr_new(global_heap_allocator, dd->known_polyvars, bh_arr_length(sd->known_polyvars));

            bh_arr_each(AstPolySolution, sln, sd->known_polyvars) {
                AstPolySolution new_sln;
                new_sln.kind     = sln->kind;
                new_sln.poly_sym = (AstNode *) ast_clone(a, (AstNode *) sln->poly_sym);
                new_sln.ast_type = (AstType *) ast_clone(a, (AstNode *) sln->ast_type);
                bh_arr_push(dd->known_polyvars, new_sln);
            }

            break;
        }

        case Ast_Kind_Compound: {
            AstCompound* cd = (AstCompound *) nn;
            AstCompound* cs = (AstCompound *) node;

            cd->exprs = NULL;
            bh_arr_new(global_heap_allocator, cd->exprs, bh_arr_length(cs->exprs));

            bh_arr_each(AstTyped *, expr, cs->exprs) {
                bh_arr_push(cd->exprs, (AstTyped *) ast_clone(a, (AstNode *) *expr));
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
            break;

        case Ast_Kind_Typeof:
            C(AstTypeOf, expr);
            ((AstTypeOf *) nn)->resolved_type = NULL;
            break;

        case Ast_Kind_Do_Block:
            C(AstDoBlock, block);
            ((AstDoBlock *) nn)->type_node = (AstType *) &basic_type_auto_return;
            break;
    }

    clone_depth--;
    return nn;
}

#undef C

AstFunction* clone_function_header(bh_allocator a, AstFunction* func) {
    if (func->kind != Ast_Kind_Function) return NULL;

    if (func->flags & Ast_Flag_Foreign) return func;

    AstFunction* new_func = onyx_ast_node_new(a, sizeof(AstFunction), func->kind);
    memmove(new_func, func, sizeof(AstFunction));

    new_func->return_type = (AstType *) ast_clone(a, func->return_type);

    new_func->params = NULL;
    bh_arr_new(global_heap_allocator, new_func->params, bh_arr_length(func->params));
    bh_arr_each(AstParam, param, func->params) {
        AstParam new_param;
        new_param.local = (AstLocal *) ast_clone(a, param->local);
        new_param.default_value = (AstTyped *) ast_clone(a, param->default_value);
        new_param.vararg_kind = param->vararg_kind;
        bh_arr_push(new_func->params, new_param);
    }

    return new_func;
}

// Clones a function body from a given function. It is assumed that `dest` is
// a function from `clone_function_header`.
void clone_function_body(bh_allocator a, AstFunction* dest, AstFunction* source) {
    if (dest->kind != Ast_Kind_Function) return;
    if (source->kind != Ast_Kind_Function) return;

    dest->body = (AstBlock *) ast_clone(a, source->body);
}
