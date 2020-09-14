#include "onyxastnodes.h"
#include "onyxparser.h"
#include "onyxutils.h"

static inline b32 should_clone(AstNode* node) {
	if (node->flags & Ast_Flag_No_Clone) return 0;

	switch (node->kind) {
		// List of nodes that should not be copied
		case Ast_Kind_Global:
		case Ast_Kind_Memres:
		case Ast_Kind_NumLit:
		case Ast_Kind_StrLit:
		case Ast_Kind_Package:
		case Ast_Kind_Enum_Type:
		case Ast_Kind_Enum_Value:
		case Ast_Kind_Overloaded_Function:
		case Ast_Kind_Polymorphic_Proc:
			return 0;

		default: return 1;
	}
}

static inline i32 ast_kind_to_size(AstNode* node) {
	switch (node->kind) {
        case Ast_Kind_Error: return sizeof(AstNode);
        case Ast_Kind_Program: return sizeof(AstNode);
        case Ast_Kind_Package: return sizeof(AstPackage);
        case Ast_Kind_Include_File: return sizeof(AstInclude);
        case Ast_Kind_Include_Folder: return sizeof(AstInclude);
        case Ast_Kind_Use_Package: return sizeof(AstUsePackage);
        case Ast_Kind_Alias: return sizeof(AstAlias);
        case Ast_Kind_Memres: return sizeof(AstMemRes);
        case Ast_Kind_Binding: return sizeof(AstBinding);
        case Ast_Kind_Function: return sizeof(AstFunction);
        case Ast_Kind_Overloaded_Function: return sizeof(AstOverloadedFunction);
        case Ast_Kind_Polymorphic_Proc: return sizeof(AstPolyProc);
        case Ast_Kind_Block: return sizeof(AstBlock);
        case Ast_Kind_Local_Group: return sizeof(AstNode);
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
        case Ast_Kind_Type_End: return 0;
        case Ast_Kind_Struct_Member: return sizeof(AstStructMember);
        case Ast_Kind_Enum_Value: return sizeof(AstEnumValue);
        case Ast_Kind_NumLit: return sizeof(AstNumLit);
        case Ast_Kind_StrLit: return sizeof(AstStrLit);
        case Ast_Kind_Param: return sizeof(AstLocal);
        case Ast_Kind_Argument: return sizeof(AstArgument);
        case Ast_Kind_Call: return sizeof(AstCall);
        case Ast_Kind_Intrinsic_Call: return sizeof(AstIntrinsicCall);
        case Ast_Kind_Return: return sizeof(AstReturn);
        case Ast_Kind_Address_Of: return sizeof(AstAddressOf);
        case Ast_Kind_Dereference: return sizeof(AstDereference);
        case Ast_Kind_Array_Access: return sizeof(AstArrayAccess);
        case Ast_Kind_Slice: return sizeof(AstArrayAccess);
        case Ast_Kind_Field_Access: return sizeof(AstFieldAccess);
        case Ast_Kind_Pipe: return sizeof(AstBinaryOp);
        case Ast_Kind_Range: return sizeof(AstBinaryOp);
        case Ast_Kind_Size_Of: return sizeof(AstSizeOf);
        case Ast_Kind_Align_Of: return sizeof(AstAlignOf);
        case Ast_Kind_File_Contents: return sizeof(AstFileContents);
        case Ast_Kind_Struct_Literal: return sizeof(AstStructLiteral);
        case Ast_Kind_If: return sizeof(AstIfWhile);
        case Ast_Kind_For: return sizeof(AstFor);
        case Ast_Kind_While: return sizeof(AstIfWhile);
        case Ast_Kind_Jump: return sizeof(AstJump);
        case Ast_Kind_Defer: return sizeof(AstDefer);
        case Ast_Kind_Switch: return sizeof(AstSwitch);
        case Ast_Kind_Switch_Case: return sizeof(AstSwitchCase);
        case Ast_Kind_Count: return 0;
	}
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

// NOTE: Using void* to avoid a lot of unnecessary casting
AstNode* ast_clone(bh_allocator a, void* n) {
	AstNode* node = (AstNode *) n;

	if (node == NULL) return NULL;
	if (!should_clone(node)) return node;

	i32 node_size = ast_kind_to_size(node);
	// bh_printf("Cloning %s with size %d\n", onyx_ast_node_kind_string(node->kind), node_size);

	AstNode* nn = onyx_ast_node_new(a, node_size, node->kind);
	memmove(nn, node, node_size);

	switch ((u16) node->kind) {
		case Ast_Kind_Binary_Op:
        case Ast_Kind_Range:
			((AstBinaryOp *) nn)->left  = (AstTyped *) ast_clone(a, ((AstBinaryOp *) node)->left);
			((AstBinaryOp *) nn)->right = (AstTyped *) ast_clone(a, ((AstBinaryOp *) node)->right);
			break;

		case Ast_Kind_Unary_Op:
			((AstUnaryOp *) nn)->expr = (AstTyped *) ast_clone(a, ((AstUnaryOp *) node)->expr);
			((AstUnaryOp *) nn)->type_node = (AstType *) ast_clone(a, ((AstUnaryOp *) node)->type_node);
			break;

		case Ast_Kind_Param:
		case Ast_Kind_Local:
			((AstLocal *) nn)->type_node = (AstType *) ast_clone(a, ((AstLocal *) node)->type_node);
			break;

		case Ast_Kind_Call:
			((AstCall *) nn)->arguments = (AstArgument *) ast_clone_list(a, ((AstCall *) node)->arguments);
			break;

		case Ast_Kind_Argument:
			((AstArgument *) nn)->value = (AstTyped *) ast_clone(a, ((AstArgument *) node)->value);
			break;

		case Ast_Kind_Address_Of:
			((AstAddressOf *) nn)->expr = (AstTyped *) ast_clone(a, ((AstAddressOf *) node)->expr);
			break;

		case Ast_Kind_Dereference:
			((AstDereference *) nn)->expr = (AstTyped *) ast_clone(a, ((AstDereference *) node)->expr);
			break;

		case Ast_Kind_Slice:
		case Ast_Kind_Array_Access:
			((AstArrayAccess *) nn)->addr = (AstTyped *) ast_clone(a, ((AstArrayAccess *) node)->addr);
			((AstArrayAccess *) nn)->expr = (AstTyped *) ast_clone(a, ((AstArrayAccess *) node)->expr);
			break;

		case Ast_Kind_Field_Access:
			((AstFieldAccess *) nn)->expr = (AstTyped *) ast_clone(a, ((AstFieldAccess *) node)->expr);
			break;

		case Ast_Kind_Size_Of:
			((AstSizeOf *) nn)->so_type = (AstType *) ast_clone(a, ((AstSizeOf *) node)->so_type);
			break;

		case Ast_Kind_Align_Of:
			((AstAlignOf *) nn)->ao_type = (AstType *) ast_clone(a, ((AstAlignOf *) node)->ao_type);
			break;

		case Ast_Kind_Struct_Literal: {
			AstStructLiteral* st = (AstStructLiteral *) node;
			AstStructLiteral* dt = (AstStructLiteral *) nn;

			dt->stnode = (AstTyped *) ast_clone(a, st->stnode);

			dt->named_values = NULL;
			dt->values = NULL;
			bh_arr_new(global_heap_allocator, dt->named_values, bh_arr_length(st->named_values));
			bh_arr_new(global_heap_allocator, dt->values, bh_arr_length(st->values));

			bh_arr_each(AstStructMember *, smem, st->named_values)
				bh_arr_push(dt->named_values, (AstStructMember *) ast_clone(a, *smem));

			bh_arr_each(AstTyped *, val, st->values)
				bh_arr_push(dt->values, (AstTyped *) ast_clone(a, *val));

			break;
		}

		case Ast_Kind_Return:
			((AstReturn *) nn)->expr = (AstTyped *) ast_clone(a, ((AstReturn *) node)->expr);
			break;

		case Ast_Kind_Block:
			((AstBlock *) nn)->body = ast_clone_list(a, ((AstBlock *) node)->body);
			((AstBlock *) nn)->locals = NULL;
			bh_arr_new(global_heap_allocator, ((AstBlock *) nn)->locals, 4);
			break;

		case Ast_Kind_Defer:
			((AstDefer *) nn)->stmt = ast_clone(a, ((AstDefer *) node)->stmt);
			break;

		case Ast_Kind_For:
			((AstFor *) nn)->var = (AstLocal *) ast_clone(a, ((AstFor *) node)->var);
			((AstFor *) nn)->iter = (AstTyped *) ast_clone(a, ((AstFor *) node)->iter);
			((AstFor *) nn)->stmt = (AstBlock *) ast_clone(a, ((AstFor *) node)->stmt);
			break;

		case Ast_Kind_If:
		case Ast_Kind_While:
			((AstIfWhile *) nn)->local = (AstLocal *) ast_clone(a, ((AstIfWhile *) node)->local);
			((AstIfWhile *) nn)->assignment = (AstBinaryOp *) ast_clone(a, ((AstIfWhile *) node)->assignment);

			if (((AstIfWhile *) nn)->assignment)
				((AstIfWhile *) nn)->assignment->left = (AstTyped *) ((AstIfWhile *) nn)->local;

			((AstIfWhile *) nn)->cond = (AstTyped *) ast_clone(a, ((AstIfWhile *) node)->cond);
			((AstIfWhile *) nn)->true_stmt = (AstBlock *) ast_clone(a, ((AstIfWhile *) node)->true_stmt);
			((AstIfWhile *) nn)->false_stmt = (AstBlock *) ast_clone(a, ((AstIfWhile *) node)->false_stmt);
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
				AstSwitchCase sc;
				sc.value = (AstTyped *) ast_clone(a, c->value);
				sc.block = (AstBlock *) ast_clone(a, c->block);
				bh_arr_push(dw->cases, sc);
			}
			break;
		}

		case Ast_Kind_Pointer_Type:
			((AstPointerType *) nn)->elem = (AstType *) ast_clone(a, ((AstPointerType *) node)->elem);
			break;

		case Ast_Kind_Array_Type:
			((AstArrayType *) nn)->count_expr = (AstTyped *) ast_clone(a, ((AstArrayType *) node)->count_expr);
			((AstArrayType *) nn)->elem = (AstType *) ast_clone(a, ((AstArrayType *) node)->elem);
			break;

		case Ast_Kind_Slice_Type:
			((AstSliceType *) nn)->elem = (AstType *) ast_clone(a, ((AstSliceType *) node)->elem);
			break;

		case Ast_Kind_DynArr_Type:
			((AstDynArrType *) nn)->elem = (AstType *) ast_clone(a, ((AstDynArrType *) node)->elem);
			break;

		case Ast_Kind_VarArg_Type:
			((AstVarArgType *) nn)->elem = (AstType *) ast_clone(a, ((AstVarArgType *) node)->elem);
			break;

		case Ast_Kind_Type_Alias:
			((AstTypeAlias *) nn)->to = (AstType *) ast_clone(a, ((AstTypeAlias *) node)->to);
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
            ((AstStructMember *) nn)->initial_value = (AstTyped *) ast_clone(a, ((AstStructMember *) node)->initial_value);
            break;

		case Ast_Kind_Function_Type:
			((AstFunctionType *) nn)->return_type = (AstType *) ast_clone(a, ((AstFunctionType *) node)->return_type);
			((AstFunctionType *) nn)->param_count = ((AstFunctionType *) node)->param_count;
			fori (i, 0, ((AstFunctionType *) nn)->param_count) {
				((AstFunctionType *) nn)->params[i] = (AstType *) ast_clone(a, ((AstFunctionType *) node)->params[i]);
			}
			break;

		case Ast_Kind_Binding:
			bh_printf("Cloning binding: %b\n", node->token->text, node->token->length);
			((AstTyped *) nn)->type_node = (AstType *) ast_clone(a, ((AstTyped *) node)->type_node);
			((AstBinding *) nn)->node = ast_clone(a, ((AstBinding *) node)->node);
			break;

		case Ast_Kind_Function: {
			AstFunction* df = (AstFunction *) nn;
			AstFunction* sf = (AstFunction *) node;

			if (sf->flags & Ast_Flag_Foreign) return node;

			df->return_type = (AstType *) ast_clone(a, sf->return_type);
			df->body = (AstBlock *) ast_clone(a, sf->body);

			df->locals = NULL;
		    bh_arr_new(global_heap_allocator, df->locals, 4);

			df->params = NULL;
			bh_arr_new(global_heap_allocator, df->params, bh_arr_length(sf->params));

			bh_arr_each(AstParam, param, sf->params) {
				AstParam new_param;
				new_param.local = (AstLocal *) ast_clone(a, param->local);
				new_param.default_value = (AstTyped *) ast_clone(a, param->default_value);
                new_param.is_vararg = param->is_vararg;
				bh_arr_push(df->params, new_param);
			}

			break;
		}
	}

	return nn;
}
