#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxutils.h"

// NOTE: The one weird define you need to know before read the code below
#define make_node(nclass, kind) onyx_ast_node_new(parser->allocator, sizeof(nclass), kind)

static AstNode error_node = { Ast_Kind_Error, 0, NULL, NULL };

// NOTE: Forward declarations
static void consume_token(OnyxParser* parser);
static void unconsume_token(OnyxParser* parser);
static b32 is_terminating_token(TokenType token_type);
static OnyxToken* expect_token(OnyxParser* parser, TokenType token_type);

static AstNumLit*     parse_numeric_literal(OnyxParser* parser);
static AstTyped*      parse_factor(OnyxParser* parser);
static AstTyped*      parse_expression(OnyxParser* parser);
static AstIf*         parse_if_stmt(OnyxParser* parser);
static AstWhile*      parse_while_stmt(OnyxParser* parser);
static AstFor*        parse_for_stmt(OnyxParser* parser);
static b32            parse_symbol_declaration(OnyxParser* parser, AstNode** ret);
static AstReturn*     parse_return_statement(OnyxParser* parser);
static AstBlock*      parse_block(OnyxParser* parser);
static AstNode*       parse_statement(OnyxParser* parser);
static AstType*       parse_type(OnyxParser* parser);
static AstStructType* parse_struct(OnyxParser* parser);
static AstLocal*      parse_function_params(OnyxParser* parser);
static AstFunction*   parse_function_definition(OnyxParser* parser);
static AstTyped*      parse_global_declaration(OnyxParser* parser);
static AstEnumType*   parse_enum_declaration(OnyxParser* parser);
static AstTyped*      parse_top_level_expression(OnyxParser* parser);
static AstNode*       parse_top_level_statement(OnyxParser* parser);

static void consume_token(OnyxParser* parser) {
    parser->prev = parser->curr;
    parser->curr++;
    while (parser->curr->type == Token_Type_Comment) parser->curr++;
}

static void unconsume_token(OnyxParser* parser) {
    // TODO: This is probably wrong
    while (parser->prev->type == Token_Type_Comment) parser->prev--;
    parser->curr = parser->prev;
    parser->prev--;
}

static b32 is_terminating_token(TokenType token_type) {
    return (token_type == ';')
        || (token_type == '}')
        || (token_type == '{')
        || (token_type == Token_Type_End_Stream);
}

static void find_token(OnyxParser* parser, TokenType token_type) {
    while (parser->curr->type != token_type && !is_terminating_token(parser->curr->type)) {
        consume_token(parser);
    }
}

// Advances to next token no matter what
static OnyxToken* expect_token(OnyxParser* parser, TokenType token_type) {
    OnyxToken* token = parser->curr;
    consume_token(parser);

    if (token->type != token_type) {
        onyx_message_add(Msg_Type_Expected_Token,
                         token->pos,
                         token_name(token_type), token_name(token->type));
        return NULL;
    }

    return token;
}

static void add_node_to_process(OnyxParser* parser, AstNode* node) {
    bh_arr_push(parser->results.nodes_to_process, ((NodeToProcess) {
        .package = parser->package,
        .scope = parser->package->scope,
        .node = node,
    }));
}



static AstNumLit* parse_numeric_literal(OnyxParser* parser) {
    AstNumLit* lit_node = make_node(AstNumLit, Ast_Kind_NumLit);
    lit_node->token = expect_token(parser, Token_Type_Literal_Numeric);
    lit_node->flags |= Ast_Flag_Comptime;
    lit_node->value.l = 0ll;

    AstType* type;
    token_toggle_end(lit_node->token);
    char* tok = lit_node->token->text;

    // NOTE: charset_contains() behaves more like string_contains()
    // so I'm using it in this case
    if (charset_contains(tok, '.')) {
        if (tok[lit_node->token->length - 1] == 'f') {
            type = (AstType *) &basic_type_f32;
            lit_node->value.f = strtof(tok, NULL);
        } else {
            type = (AstType *) &basic_type_f64;
            lit_node->value.d = strtod(tok, NULL);
        }
    } else {
        i64 value = strtoll(tok, NULL, 0);
        if (bh_abs(value) < ((u64) 1 << 32)) {
            type = (AstType *) &basic_type_i32;
        } else {
            type = (AstType *) &basic_type_i64;
        }

        lit_node->value.l = value;
    }

    lit_node->type_node = type;
    token_toggle_end(lit_node->token);
    return lit_node;
}

// ( <expr> )
// - <factor>
// ! <factor>
// <symbol> ( '(' <exprlist> ')' )?
// <numlit>
// 'true'
// 'false'
// All of these could be followed by a cast
static AstTyped* parse_factor(OnyxParser* parser) {
    AstTyped* retval = NULL;

    switch ((u16) parser->curr->type) {
        case '(': {
            consume_token(parser);
            AstTyped* expr = parse_expression(parser);
            expect_token(parser, ')');
            retval = expr;
            break;
        }

        case '-': {
            consume_token(parser);
            AstTyped* factor = parse_factor(parser);

            AstUnaryOp* negate_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            negate_node->operation = Unary_Op_Negate;
            negate_node->expr = factor;

            if ((factor->flags & Ast_Flag_Comptime) != 0) {
                negate_node->flags |= Ast_Flag_Comptime;
            }

            retval = (AstTyped *) negate_node;
            break;
        }

        case '!': {
            AstUnaryOp* not_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
            not_node->operation = Unary_Op_Not;
            not_node->token = expect_token(parser, '!');
            not_node->expr = parse_factor(parser);

            if ((not_node->expr->flags & Ast_Flag_Comptime) != 0) {
                not_node->flags |= Ast_Flag_Comptime;
            }

            retval = (AstTyped *) not_node;
            break;
        }

        case '*': {
            AstDereference* deref_node = make_node(AstDereference, Ast_Kind_Dereference);
            deref_node->token = expect_token(parser, '*');
            deref_node->expr  = parse_factor(parser);

            retval = (AstTyped *) deref_node;
            break;
        }

        case '^': {
            AstAddressOf* aof_node = make_node(AstAddressOf, Ast_Kind_Address_Of);
            aof_node->token = expect_token(parser, '^');
            aof_node->expr  = parse_factor(parser);

            retval = (AstTyped *) aof_node;
            break;
        }

        case Token_Type_Keyword_Sizeof: {
            AstSizeOf* so_node = make_node(AstSizeOf, Ast_Kind_Size_Of);
            so_node->token = expect_token(parser, Token_Type_Keyword_Sizeof);
            so_node->so_type = (AstType *) parse_type(parser);
            so_node->type_node = (AstType *) &basic_type_i32;

            retval = (AstTyped *) so_node;
            break;
        }

        case Token_Type_Keyword_Alignof: {
            AstAlignOf* ao_node = make_node(AstAlignOf, Ast_Kind_Align_Of);
            ao_node->token = expect_token(parser, Token_Type_Keyword_Alignof);
            ao_node->ao_type = (AstType *) parse_type(parser);
            ao_node->type_node = (AstType *) &basic_type_i32;

            retval = (AstTyped *) ao_node;
            break;
        }

        case Token_Type_Symbol: {
            OnyxToken* sym_token = expect_token(parser, Token_Type_Symbol);
            AstTyped* sym_node = make_node(AstTyped, Ast_Kind_Symbol);
            sym_node->token = sym_token;

            retval = sym_node;
            break;
        }

        case Token_Type_Literal_Numeric:
            retval = (AstTyped *) parse_numeric_literal(parser);
            break;

        case Token_Type_Literal_String: {
            AstPointerType* str_type = make_node(AstPointerType, Ast_Kind_Pointer_Type);
            str_type->flags |= Basic_Flag_Pointer;
            str_type->elem = (AstType *) &basic_type_u8;

            AstStrLit* str_node = make_node(AstStrLit, Ast_Kind_StrLit);
            str_node->token     = expect_token(parser, Token_Type_Literal_String);
            str_node->type_node = (AstType *) str_type;
            str_node->addr      = 0;

            add_node_to_process(parser, (AstNode *) str_node);

            retval = (AstTyped *) str_node;
            break;
        }

        case Token_Type_Literal_True: {
            AstNumLit* bool_node = make_node(AstNumLit, Ast_Kind_NumLit);
            bool_node->type_node = (AstType *) &basic_type_bool;
            bool_node->token = expect_token(parser, Token_Type_Literal_True);
            bool_node->value.i = 1;
            retval = (AstTyped *) bool_node;
            break;
        }

        case Token_Type_Literal_False: {
            AstNumLit* bool_node = make_node(AstNumLit, Ast_Kind_NumLit);
            bool_node->type_node = (AstType *) &basic_type_bool;
            bool_node->token = expect_token(parser, Token_Type_Literal_False);
            bool_node->value.i = 0;
            retval = (AstTyped *) bool_node;
            break;
        }

        default:
            onyx_message_add(Msg_Type_Unexpected_Token,
                    parser->curr->pos,
                    token_name(parser->curr->type));
            return NULL;
    }

    while (parser->curr->type == '[' || parser->curr->type == Token_Type_Keyword_Cast
        || parser->curr->type == '.' || parser->curr->type == '(') {

        switch ((u16) parser->curr->type) {
            case '[': {
                AstArrayAccess* aa_node = make_node(AstArrayAccess, Ast_Kind_Array_Access);
                aa_node->token = expect_token(parser, '[');
                aa_node->addr  = retval;
                aa_node->expr  = parse_expression(parser);

                expect_token(parser, ']');

                retval = (AstTyped *) aa_node;
                break;
            }

            case '.': {
                consume_token(parser);
                AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                field->token = expect_token(parser, Token_Type_Symbol);
                field->expr  = retval;

                retval = (AstTyped *) field;
                break;
            }

            case Token_Type_Keyword_Cast: {
                AstUnaryOp* cast_node = make_node(AstUnaryOp, Ast_Kind_Unary_Op);
                cast_node->token = expect_token(parser, Token_Type_Keyword_Cast);
                cast_node->type_node = parse_type(parser);
                cast_node->operation = Unary_Op_Cast;
                cast_node->expr = retval;

                retval = (AstTyped *) cast_node;
                break;
            }

            case '(': {
                AstCall* call_node = make_node(AstCall, Ast_Kind_Call);
                call_node->token = expect_token(parser, '(');
                call_node->callee = (AstNode *) retval;
                call_node->arg_count = 0;

                AstArgument** prev = &call_node->arguments;
                AstArgument* curr = NULL;
                while (parser->curr->type != ')') {
                    curr = make_node(AstArgument, Ast_Kind_Argument);
                    curr->token = parser->curr;
                    curr->value = parse_expression(parser);

                    if (curr != NULL && curr->kind != Ast_Kind_Error) {
                        *prev = curr;
                        prev = (AstArgument **) &curr->next;

                        call_node->arg_count++;
                    }

                    if (parser->curr->type == ')')
                        break;

                    if (parser->curr->type != ',') {
                        onyx_message_add(Msg_Type_Expected_Token,
                                parser->curr->pos,
                                token_name(','),
                                token_name(parser->curr->type));
                        return (AstTyped *) &error_node;
                    }

                    consume_token(parser);
                }
                consume_token(parser);

                retval = (AstTyped *) call_node;
                break;
            }
        }
    }

    return retval;
}

static inline i32 get_precedence(BinaryOp kind) {
    switch (kind) {
        case Binary_Op_Assign:          return 1;
        case Binary_Op_Assign_Add:      return 1;
        case Binary_Op_Assign_Minus:    return 1;
        case Binary_Op_Assign_Multiply: return 1;
        case Binary_Op_Assign_Divide:   return 1;
        case Binary_Op_Assign_Modulus:  return 1;
        case Binary_Op_Assign_And:      return 1;
        case Binary_Op_Assign_Or:       return 1;
        case Binary_Op_Assign_Xor:      return 1;
        case Binary_Op_Assign_Shl:      return 1;
        case Binary_Op_Assign_Shr:      return 1;
        case Binary_Op_Assign_Sar:      return 1;

        case Binary_Op_Bool_And:        return 2;
        case Binary_Op_Bool_Or:         return 2;

        case Binary_Op_Equal:           return 3;
        case Binary_Op_Not_Equal:       return 3;

        case Binary_Op_Less_Equal:      return 4;
        case Binary_Op_Less:            return 4;
        case Binary_Op_Greater_Equal:   return 4;
        case Binary_Op_Greater:         return 4;

        case Binary_Op_And:             return 5;
        case Binary_Op_Or:              return 5;
        case Binary_Op_Xor:             return 5;

        case Binary_Op_Add:             return 6;
        case Binary_Op_Minus:           return 6;

        case Binary_Op_Multiply:        return 7;
        case Binary_Op_Divide:          return 7;

        case Binary_Op_Modulus:         return 8;

        default:                        return -1;
    }
}

// <expr> +  <expr>
// <expr> -  <expr>
// <expr> *  <expr>
// <expr> /  <expr>
// <expr> %  <expr>
// <expr> == <expr>
// <expr> != <expr>
// <expr> <= <expr>
// <expr> >= <expr>
// <expr> <  <expr>
// <expr> >  <expr>
// <expr> =  <expr>
// <expr> += <expr>
// <expr> -= <expr>
// <expr> *= <expr>
// <expr> /= <expr>
// <expr> %= <expr>
// <factor>
// With expected precedence rules
static AstTyped* parse_expression(OnyxParser* parser) {
    bh_arr(AstBinaryOp*) tree_stack = NULL;
    bh_arr_new(global_scratch_allocator, tree_stack, 4);
    bh_arr_set_length(tree_stack, 0);

    AstTyped* left = parse_factor(parser);
    AstTyped* right;
    AstTyped* root = left;

    BinaryOp bin_op_kind;
    OnyxToken* bin_op_tok;

    while (1) {
        bin_op_kind = -1;
        switch ((u16) parser->curr->type) {
            case Token_Type_Equal_Equal:       bin_op_kind = Binary_Op_Equal; break;
            case Token_Type_Not_Equal:         bin_op_kind = Binary_Op_Not_Equal; break;
            case Token_Type_Less_Equal:        bin_op_kind = Binary_Op_Less_Equal; break;
            case Token_Type_Greater_Equal:     bin_op_kind = Binary_Op_Greater_Equal; break;
            case '<':                          bin_op_kind = Binary_Op_Less; break;
            case '>':                          bin_op_kind = Binary_Op_Greater; break;

            case '+':                          bin_op_kind = Binary_Op_Add; break;
            case '-':                          bin_op_kind = Binary_Op_Minus; break;
            case '*':                          bin_op_kind = Binary_Op_Multiply; break;
            case '/':                          bin_op_kind = Binary_Op_Divide; break;
            case '%':                          bin_op_kind = Binary_Op_Modulus; break;

            case '&':                          bin_op_kind = Binary_Op_And; break;
            case '|':                          bin_op_kind = Binary_Op_Or; break;
            case '^':                          bin_op_kind = Binary_Op_Xor; break;
            case Token_Type_Shift_Left:        bin_op_kind = Binary_Op_Shl; break;
            case Token_Type_Shift_Right:       bin_op_kind = Binary_Op_Shr; break;
            case Token_Type_Shift_Arith_Right: bin_op_kind = Binary_Op_Sar; break;

            case Token_Type_And_And:           bin_op_kind = Binary_Op_Bool_And; break;
            case Token_Type_Or_Or:             bin_op_kind = Binary_Op_Bool_Or; break;

            case '=':                          bin_op_kind = Binary_Op_Assign; break;
            case Token_Type_Plus_Equal:        bin_op_kind = Binary_Op_Assign_Add; break;
            case Token_Type_Minus_Equal:       bin_op_kind = Binary_Op_Assign_Minus; break;
            case Token_Type_Star_Equal:        bin_op_kind = Binary_Op_Assign_Multiply; break;
            case Token_Type_Fslash_Equal:      bin_op_kind = Binary_Op_Assign_Divide; break;
            case Token_Type_Percent_Equal:     bin_op_kind = Binary_Op_Assign_Modulus; break;
            case Token_Type_And_Equal:         bin_op_kind = Binary_Op_Assign_And; break;
            case Token_Type_Or_Equal:          bin_op_kind = Binary_Op_Assign_Or; break;
            case Token_Type_Xor_Equal:         bin_op_kind = Binary_Op_Assign_Xor; break;
            case Token_Type_Shl_Equal:         bin_op_kind = Binary_Op_Assign_Shl; break;
            case Token_Type_Shr_Equal:         bin_op_kind = Binary_Op_Assign_Shr; break;
            case Token_Type_Sar_Equal:         bin_op_kind = Binary_Op_Assign_Sar; break;
            default: goto expression_done;
        }

        if (bin_op_kind != -1) {
            bin_op_tok = parser->curr;
            consume_token(parser);

            AstBinaryOp* bin_op = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
            bin_op->operation = bin_op_kind;
            bin_op->token = bin_op_tok;

            while ( !bh_arr_is_empty(tree_stack) &&
                    get_precedence(bh_arr_last(tree_stack)->operation) >= get_precedence(bin_op_kind))
                bh_arr_pop(tree_stack);

            if (bh_arr_is_empty(tree_stack)) {
                // NOTE: new is now the root node
                bin_op->left = root;
                root = (AstTyped *) bin_op;
            } else {
                bin_op->left = bh_arr_last(tree_stack)->right;
                bh_arr_last(tree_stack)->right = (AstTyped *) bin_op;
            }

            bh_arr_push(tree_stack, bin_op);

            right = parse_factor(parser);
            bin_op->right = right;

            if ((left->flags & Ast_Flag_Comptime) != 0 && (right->flags & Ast_Flag_Comptime) != 0) {
                bin_op->flags |= Ast_Flag_Comptime;
            }
        }
    }

expression_done:
    return root;
}

// 'if' <expr> <stmt> ('elseif' <cond> <stmt>)* ('else' <block>)?
static AstIf* parse_if_stmt(OnyxParser* parser) {
    expect_token(parser, Token_Type_Keyword_If);

    AstTyped* cond = parse_expression(parser);
    AstNode*  true_stmt = parse_statement(parser);

    AstIf* if_node = make_node(AstIf, Ast_Kind_If);
    AstIf* root_if = if_node;

    if_node->cond = cond;
    if (true_stmt != NULL)
        if_node->true_stmt = true_stmt;

    while (parser->curr->type == Token_Type_Keyword_Elseif) {
        consume_token(parser);
        AstIf* elseif_node = make_node(AstIf, Ast_Kind_If);

        cond = parse_expression(parser);
        true_stmt = parse_statement(parser);

        elseif_node->cond = cond;
        if (true_stmt != NULL)
            elseif_node->true_stmt = true_stmt;

        if_node->false_stmt = (AstNode *) elseif_node;
        if_node = elseif_node;
    }

    if (parser->curr->type == Token_Type_Keyword_Else) {
        consume_token(parser);

        AstNode* false_stmt = parse_statement(parser);
        if (false_stmt != NULL)
            if_node->false_stmt = false_stmt;
    }

    return root_if;
}

// 'while' <expr> <block>
static AstWhile* parse_while_stmt(OnyxParser* parser) {
    OnyxToken* while_token = expect_token(parser, Token_Type_Keyword_While);

    AstTyped* cond = parse_expression(parser);
    AstNode*  stmt = parse_statement(parser);

    AstWhile* while_node = make_node(AstWhile, Ast_Kind_While);
    while_node->token = while_token;
    while_node->cond = cond;
    while_node->stmt = stmt;

    return while_node;
}

static AstFor* parse_for_stmt(OnyxParser* parser) {
    AstFor* for_node = make_node(AstFor, Ast_Kind_For);
    for_node->token = expect_token(parser, Token_Type_Keyword_For);

    AstLocal* var_node = make_node(AstLocal, Ast_Kind_Local);
    var_node->token = expect_token(parser, Token_Type_Symbol);
    var_node->type_node = (AstType *) &basic_type_i32;

    for_node->var = var_node;

    expect_token(parser, ':');
    for_node->start = parse_expression(parser);
    expect_token(parser, ',');
    for_node->end = parse_expression(parser);

    if (parser->curr->type == ',') {
        consume_token(parser);
        for_node->step = parse_expression(parser);
    }

    for_node->stmt = parse_statement(parser);

    return for_node;
}

// Returns 1 if the symbol was consumed. Returns 0 otherwise
// ret is set to the statement to insert
// <symbol> : <type> = <expr>
// <symbol> : <type> : <expr>
// <symbol> := <expr>
// <symbol> :: <expr>
static b32 parse_symbol_declaration(OnyxParser* parser, AstNode** ret) {
    if (parser->curr->type != Token_Type_Symbol) return 0;
    if ((parser->curr + 1)->type != ':')         return 0;

    OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);
    consume_token(parser);
    AstType* type_node = NULL;

    // NOTE: var: type
    if (parser->curr->type != ':'
            && parser->curr->type != '=') {
        type_node = parse_type(parser);
    }

    AstLocal* local = make_node(AstLocal, Ast_Kind_Local);
    local->token = symbol;
    local->type_node = type_node;
    *ret = (AstNode *) local;

    if (parser->curr->type == '=' || parser->curr->type == ':') {
        if (parser->curr->type == ':') {
            local->flags |= Ast_Flag_Const;
        }

        AstBinaryOp* assignment = make_node(AstBinaryOp, Ast_Kind_Binary_Op);
        assignment->operation = Binary_Op_Assign;
        local->next = (AstNode *) assignment;
        assignment->token = parser->curr;
        consume_token(parser);

        AstTyped* expr = parse_expression(parser);
        if (expr == NULL) {
            token_toggle_end(parser->curr);
            onyx_message_add(Msg_Type_Expected_Expression,
                    assignment->token->pos,
                    parser->curr->text);
            token_toggle_end(parser->curr);
            return 1;
        }
        assignment->right = expr;

        AstNode* left_symbol = make_node(AstNode, Ast_Kind_Symbol);
        left_symbol->token = symbol;
        assignment->left = (AstTyped *) left_symbol;
    }

    return 1;
}

// 'return' <expr>?
static AstReturn* parse_return_statement(OnyxParser* parser) {
    AstReturn* return_node = make_node(AstReturn, Ast_Kind_Return);
    return_node->token = expect_token(parser, Token_Type_Keyword_Return);

    AstTyped* expr = NULL;

    if (parser->curr->type != ';') {
        expr = parse_expression(parser);

        if (expr == NULL || expr == (AstTyped *) &error_node) {
            return (AstReturn *) &error_node;
        } else {
            return_node->expr = expr;
        }
    }

    return return_node;
}

// <return> ;
// <block>
// <symbol_statement> ;
// <expr> ;
// <if>
// <while>
// 'break' ;
// 'continue' ;
static AstNode* parse_statement(OnyxParser* parser) {
    b32 needs_semicolon = 1;
    AstNode* retval = NULL;

    switch ((u16) parser->curr->type) {
        case Token_Type_Keyword_Return:
            retval = (AstNode *) parse_return_statement(parser);
            break;

        case '{':
        case Token_Type_Empty_Block:
            needs_semicolon = 0;
            retval = (AstNode *) parse_block(parser);
            break;

        case Token_Type_Symbol:
            if (parse_symbol_declaration(parser, &retval)) break;
            // fallthrough

        case '(':
        case '+':
        case '-':
        case '!':
        case '*':
        case Token_Type_Literal_Numeric:
        case Token_Type_Literal_String:
            retval = (AstNode *) parse_expression(parser);
            break;

        case Token_Type_Keyword_If:
            needs_semicolon = 0;
            retval = (AstNode *) parse_if_stmt(parser);
            break;

        case Token_Type_Keyword_While:
            needs_semicolon = 0;
            retval = (AstNode *) parse_while_stmt(parser);
            break;

        case Token_Type_Keyword_For:
            needs_semicolon = 0;
            retval = (AstNode *) parse_for_stmt(parser);
            break;

        case Token_Type_Keyword_Break: {
            AstBreak* bnode = make_node(AstBreak, Ast_Kind_Break);
            bnode->token = expect_token(parser, Token_Type_Keyword_Break);

            u64 count = 1;
            while (parser->curr->type == Token_Type_Keyword_Break) {
                consume_token(parser);
                count++;
            }
            bnode->count = count;

            retval = (AstNode *) bnode;
            break;
        }

        case Token_Type_Keyword_Continue: {
            AstContinue* cnode = make_node(AstBreak, Ast_Kind_Continue);
            cnode->token = expect_token(parser, Token_Type_Keyword_Continue);

            u64 count = 1;
            while (parser->curr->type == Token_Type_Keyword_Continue) {
                consume_token(parser);
                count++;
            }
            cnode->count = count;

            retval = (AstNode *) cnode;
            break;
        }

        default:
            break;
    }

    if (needs_semicolon) {
        if (parser->curr->type != ';') {
            onyx_message_add(Msg_Type_Expected_Token,
                parser->curr->pos,
                token_name(';'),
                token_name(parser->curr->type));

            find_token(parser, ';');
        }
        consume_token(parser);
    }

    return retval;
}

// '---'
// '{' <stmtlist> '}'
static AstBlock* parse_block(OnyxParser* parser) {
    AstBlock* block = make_node(AstBlock, Ast_Kind_Block);

    // NOTE: --- is for an empty block
    if (parser->curr->type == Token_Type_Empty_Block) {
        expect_token(parser, Token_Type_Empty_Block);
        return block;
    }

    expect_token(parser, '{');

    AstNode** next = &block->body;
    AstNode* stmt = NULL;
    while (parser->curr->type != '}') {
        stmt = parse_statement(parser);

        if (stmt != NULL && stmt->kind != Ast_Kind_Error) {
            *next = stmt;

            while (stmt->next != NULL) stmt = stmt->next;
            next = &stmt->next;
        }
    }

    expect_token(parser, '}');

    return block;
}

// <symbol>
// '^' <type>
static AstType* parse_type(OnyxParser* parser) {
    AstType* root = NULL;
    AstType** next_insertion = &root;

    while (1) {
        if (parser->curr->type == '^') {
            AstPointerType* new = make_node(AstPointerType, Ast_Kind_Pointer_Type);
            new->flags |= Basic_Flag_Pointer;
            new->token = expect_token(parser, '^');
            *next_insertion = (AstType *) new;
            next_insertion = &new->elem;
        }

        else if (parser->curr->type == '[') {
           AstArrayType* new = make_node(AstArrayType, Ast_Kind_Array_Type);
           new->token = expect_token(parser, '[');

           if (parser->curr->type != ']')
               new->count_expr = parse_expression(parser);

           expect_token(parser, ']');
           *next_insertion = (AstType *) new;
           next_insertion = &new->elem;
        }

        else if (parser->curr->type == Token_Type_Symbol) {
            AstNode* symbol_node = make_node(AstNode, Ast_Kind_Symbol);
            symbol_node->token = expect_token(parser, Token_Type_Symbol);

            if (parser->curr->type == '.') {
                consume_token(parser);
                AstFieldAccess* field = make_node(AstFieldAccess, Ast_Kind_Field_Access);
                field->token = expect_token(parser, Token_Type_Symbol);
                field->expr  = (AstTyped *) symbol_node;

                *next_insertion = (AstType *) field;
            } else {
                *next_insertion = (AstType *) symbol_node;
            }

            next_insertion = NULL;
        }

        else if (parser->curr->type == Token_Type_Keyword_Struct) {
            AstStructType* s_node = parse_struct(parser);
            *next_insertion = (AstType *) s_node;
            next_insertion = NULL;
        }

        else {
            token_toggle_end(parser->curr);
            onyx_message_add(Msg_Type_Unexpected_Token,
                    parser->curr->pos,
                    parser->curr->text);
            token_toggle_end(parser->curr);

            consume_token(parser);
            break;
        }

        if (next_insertion == NULL) break;
    }

    return root;
}

static AstStructType* parse_struct(OnyxParser* parser) {
    AstStructType* s_node = make_node(AstStructType, Ast_Kind_Struct_Type);
    s_node->token = expect_token(parser, Token_Type_Keyword_Struct);

    bh_arr_new(global_heap_allocator, s_node->members, 4);

    expect_token(parser, '{');
    while (parser->curr->type != '}') {
        AstStructMember* mem = make_node(AstStructMember, Ast_Kind_Struct_Member);
        mem->offset = 0;

        mem->token = expect_token(parser, Token_Type_Symbol);
        expect_token(parser, ':');
        mem->type_node = parse_type(parser);
        expect_token(parser, ';');

        bh_arr_push(s_node->members, mem);
    }

    expect_token(parser, '}');

    return s_node;
}

// e
// '(' (<symbol>: <type>,?)* ')'
static AstLocal* parse_function_params(OnyxParser* parser) {
    if (parser->curr->type != '(')
        return NULL;

    expect_token(parser, '(');

    if (parser->curr->type == ')') {
        consume_token(parser);
        return NULL;
    }

    AstLocal* first_param = NULL;
    AstLocal* curr_param = NULL;
    AstLocal* trailer = NULL;

    OnyxToken* symbol;
    while (parser->curr->type != ')') {
        if (parser->curr->type == ',') consume_token(parser);

        symbol = expect_token(parser, Token_Type_Symbol);
        expect_token(parser, ':');

        curr_param = make_node(AstLocal, Ast_Kind_Param);
        curr_param->token = symbol;
        curr_param->flags |= Ast_Flag_Const;
        curr_param->type_node = parse_type(parser);

        if (first_param == NULL) first_param = curr_param;

        curr_param->next = NULL;
        if (trailer) trailer->next = (AstNode *) curr_param;

        trailer = curr_param;
    }

    consume_token(parser); // Skip the )
    return first_param;
}

// e
// '#' <symbol>
static b32 parse_possible_directive(OnyxParser* parser, const char* dir) {
    if (parser->curr->type != '#') return 0;

    expect_token(parser, '#');
    OnyxToken* sym = expect_token(parser, Token_Type_Symbol);

    b32 match = (strlen(dir) == sym->length) && (strncmp(dir, sym->text, sym->length) == 0);
    if (!match) {
        unconsume_token(parser);
        unconsume_token(parser);
    }
    return match;
}

// 'proc' <directive>* <func_params> ('->' <type>)? <block>
static AstFunction* parse_function_definition(OnyxParser* parser) {
    AstFunction* func_def = make_node(AstFunction, Ast_Kind_Function);
    bh_arr_new(global_heap_allocator, func_def->locals, 4);
    func_def->token = expect_token(parser, Token_Type_Keyword_Proc);

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "overloaded")) {
            AstOverloadedFunction* ofunc = make_node(AstOverloadedFunction, Ast_Kind_Overloaded_Function);
            ofunc->token = func_def->token;

            bh_arr_new(global_heap_allocator, ofunc->overloads, 4);

            expect_token(parser, '{');
            while (parser->curr->type != '}') {
                AstTyped* sym_node = make_node(AstTyped, Ast_Kind_Symbol);
                sym_node->token = expect_token(parser, Token_Type_Symbol);

                bh_arr_push(ofunc->overloads, sym_node);

                if (parser->curr->type != '}')
                    expect_token(parser, ',');
            }

            consume_token(parser);
            return (AstFunction *) ofunc;
        }

        if (parse_possible_directive(parser, "intrinsic")) {
            func_def->flags |= Ast_Flag_Intrinsic;

            if (parser->curr->type == Token_Type_Literal_String) {
                OnyxToken* str_token = expect_token(parser, Token_Type_Literal_String);
                func_def->intrinsic_name = str_token;
            }
        }

        else if (parse_possible_directive(parser, "inline")) {
            func_def->flags |= Ast_Flag_Inline;
        }

        else if (parse_possible_directive(parser, "foreign")) {
            func_def->foreign_module = expect_token(parser, Token_Type_Literal_String);
            func_def->foreign_name   = expect_token(parser, Token_Type_Literal_String);

            func_def->flags |= Ast_Flag_Foreign;
        }

        else if (parse_possible_directive(parser, "export")) {
            func_def->flags |= Ast_Flag_Exported;

            if (parser->curr->type == Token_Type_Literal_String) {
                OnyxToken* str_token = expect_token(parser, Token_Type_Literal_String);
                func_def->exported_name = str_token;
            }
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_message_add(Msg_Type_Unknown_Directive,
                    directive_token->pos,
                    symbol_token->text, symbol_token->length);
        }
    }

    AstLocal* params = parse_function_params(parser);
    func_def->params = params;

    AstType* return_type = (AstType *) &basic_type_void;
    if (parser->curr->type == Token_Type_Right_Arrow) {
        expect_token(parser, Token_Type_Right_Arrow);

        return_type = parse_type(parser);
    }

    u64 param_count = 0;
    for (AstLocal* param = params;
            param != NULL;
            param = (AstLocal *) param->next)
        param_count++;

    AstFunctionType* type_node = bh_alloc(parser->allocator, sizeof(AstFunctionType) + param_count * sizeof(AstType *));
    type_node->kind = Ast_Kind_Function_Type;
    type_node->param_count = param_count;
    type_node->return_type = return_type;

    u32 i = 0;
    for (AstLocal* param = params;
            param != NULL;
            param = (AstLocal *) param->next) {
        type_node->params[i] = param->type_node;
        i++;
    }

    func_def->type_node = (AstType *) type_node;

    func_def->body = parse_block(parser);

    return func_def;
}

// 'global' <type>
static AstTyped* parse_global_declaration(OnyxParser* parser) {
    AstGlobal* global_node = make_node(AstGlobal, Ast_Kind_Global);
    global_node->token = expect_token(parser, Token_Type_Keyword_Global);

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "foreign")) {
            global_node->foreign_module = expect_token(parser, Token_Type_Literal_String);
            global_node->foreign_name   = expect_token(parser, Token_Type_Literal_String);

            global_node->flags |= Ast_Flag_Foreign;
        }

        else if (parse_possible_directive(parser, "export")) {
            global_node->flags |= Ast_Flag_Exported;

            if (parser->curr->type == Token_Type_Literal_String) {
                OnyxToken* str_token = expect_token(parser, Token_Type_Literal_String);
                global_node->exported_name = str_token;
            }
        }

        else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_message_add(Msg_Type_Unknown_Directive,
                    directive_token->pos,
                    symbol_token->text, symbol_token->length);
        }
    }

    global_node->type_node = parse_type(parser);

    add_node_to_process(parser, (AstNode *) global_node);

    return (AstTyped *) global_node;
}

static AstEnumType* parse_enum_declaration(OnyxParser* parser) {
    AstEnumType* enum_node = make_node(AstEnumType, Ast_Kind_Enum_Type);
    enum_node->token = expect_token(parser, Token_Type_Keyword_Enum);

    bh_arr_new(global_heap_allocator, enum_node->values, 4);

    while (parser->curr->type == '#') {
        if (parse_possible_directive(parser, "flags")) {
            enum_node->flags |= Ast_Flag_Enum_Is_Flags;
        } else {
            OnyxToken* directive_token = expect_token(parser, '#');
            OnyxToken* symbol_token = expect_token(parser, Token_Type_Symbol);

            onyx_message_add(Msg_Type_Unknown_Directive,
                    directive_token->pos,
                    symbol_token->text, symbol_token->length);
        }
    }

    AstType* backing = (AstType *) &basic_type_u32;
    if (parser->curr->type == '(') {
        consume_token(parser);

        AstNode* backing_sym = make_node(AstNode, Ast_Kind_Symbol);
        backing_sym->token = expect_token(parser, Token_Type_Symbol);
        backing = (AstType *) backing_sym;

        expect_token(parser, ')');
    }
    enum_node->backing = backing;

    expect_token(parser, '{');

    while (parser->curr->type != '}') {
        AstEnumValue* evalue = make_node(AstEnumValue, Ast_Kind_Enum_Value);
        evalue->token = expect_token(parser, Token_Type_Symbol);
        evalue->type_node = (AstType *) enum_node;

        if (parser->curr->type == ':') {
            consume_token(parser);
            expect_token(parser, ':');
            
            evalue->value = parse_numeric_literal(parser);
        }

        expect_token(parser, ';');

        bh_arr_push(enum_node->values, evalue);
    }

    expect_token(parser, '}');

    return enum_node;
}

// <proc>
// <global>
// <expr>
static AstTyped* parse_top_level_expression(OnyxParser* parser) {
    if (parser->curr->type == Token_Type_Keyword_Proc) {
        AstFunction* func_node = parse_function_definition(parser);

        add_node_to_process(parser, (AstNode *) func_node);

        return (AstTyped *) func_node;
    }
    else if (parser->curr->type == Token_Type_Keyword_Global) {
        return parse_global_declaration(parser);
    }
    else if (parser->curr->type == Token_Type_Keyword_Struct) {
        return (AstTyped *) parse_struct(parser);
    }
    else if (parser->curr->type == Token_Type_Keyword_Enum) {
        return (AstTyped *) parse_enum_declaration(parser);
    }
    else {
        return parse_expression(parser);
    }
}

// 'use' <string>
// <symbol> :: <expr>
static AstNode* parse_top_level_statement(OnyxParser* parser) {
    switch (parser->curr->type) {
        case Token_Type_Keyword_Use: {
            OnyxToken* use_token = expect_token(parser, Token_Type_Keyword_Use);

            if (parser->curr->type == Token_Type_Keyword_Package) {
                consume_token(parser);

                AstUsePackage* upack = make_node(AstUsePackage, Ast_Kind_Use_Package);
                upack->token = use_token;

                AstNode* pack_symbol = make_node(AstNode, Ast_Kind_Symbol);
                pack_symbol->token = expect_token(parser, Token_Type_Symbol);
                upack->package = (AstPackage *) pack_symbol;

                // followed by 'as'
                if (parser->curr->type == Token_Type_Keyword_Cast) {
                    consume_token(parser);
                    upack->alias = expect_token(parser, Token_Type_Symbol);
                }

                if (parser->curr->type == '{') {
                    consume_token(parser);

                    bh_arr_new(global_heap_allocator, upack->only, 4);

                    while (parser->curr->type != '}') {
                        AstAlias* alias = make_node(AstAlias, Ast_Kind_Alias);
                        alias->token = expect_token(parser, Token_Type_Symbol);

                        if (parser->curr->type == Token_Type_Keyword_Cast) {
                            consume_token(parser);
                            alias->alias = expect_token(parser, Token_Type_Symbol);
                        } else {
                            alias->alias = alias->token;
                        }

                        bh_arr_push(upack->only, alias);

                        if (parser->curr->type != '}')
                            expect_token(parser, ',');
                    }

                    consume_token(parser);
                }

                add_node_to_process(parser, (AstNode *) upack);
                return NULL;
                
            } else {
                AstIncludeFile* include = make_node(AstIncludeFile, Ast_Kind_Include_File);
                include->token = use_token;
                include->filename = expect_token(parser, Token_Type_Literal_String);

                return (AstNode *) include;
            }
        }

        case Token_Type_Keyword_Proc:
            parse_top_level_expression(parser);
            return NULL;

        case Token_Type_Symbol: {
            OnyxToken* symbol = parser->curr;
            consume_token(parser);

            expect_token(parser, ':');

            if (parser->curr->type == ':') {
                expect_token(parser, ':');

                AstTyped* node = parse_top_level_expression(parser);

                if (node->kind == Ast_Kind_Function) {
                    AstFunction* func = (AstFunction *) node;

                    if (func->exported_name == NULL)
                        func->exported_name = symbol;

                } else if (node->kind == Ast_Kind_Global) {
                    AstGlobal* global = (AstGlobal *) node;

                    if (global->exported_name == NULL)
                        global->exported_name = symbol;

                } else if (node->kind != Ast_Kind_Overloaded_Function
                        && node->kind != Ast_Kind_StrLit) {

                    if (node->kind == Ast_Kind_Struct_Type || node->kind == Ast_Kind_Enum_Type) {
                        ((AstStructType *)node)->name = bh_aprintf(global_heap_allocator,
                            "%b", symbol->text, symbol->length);
                    }

                    // HACK
                    add_node_to_process(parser, (AstNode *) node);
                }

                AstBinding* binding = make_node(AstBinding, Ast_Kind_Binding);
                binding->token = symbol;
                binding->node = (AstNode *) node;

                return (AstNode *) binding;
            } else {
                if (parser->curr->type == '=') {
                    onyx_message_add(Msg_Type_Literal,
                            parser->curr->pos,
                            "assigning initial values to memory reservations isn't allowed yet.");
                    break;
                }

                AstMemRes* memres = make_node(AstMemRes, Ast_Kind_Memres);
                memres->token = symbol;
                memres->type_node = parse_type(parser);

                add_node_to_process(parser, (AstNode *) memres);

                AstBinding* binding = make_node(AstBinding, Ast_Kind_Binding);
                binding->token = symbol;
                binding->node = (AstNode *) memres;

                return (AstNode *) binding;
            }
        }

        default: break;
    }

    consume_token(parser);
    return NULL;
}





// NOTE: This returns a void* so I don't need to cast it everytime I use it
void* onyx_ast_node_new(bh_allocator alloc, i32 size, AstKind kind) {
    void* node = bh_alloc(alloc, size);

    memset(node, 0, size);
    *(AstKind *) node = kind;

    return node;
}

OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, ProgramInfo* program) {
    OnyxParser parser;

    parser.allocator = alloc;
    parser.tokenizer = tokenizer;
    parser.curr = tokenizer->tokens;
    parser.prev = NULL;
    parser.program = program;

    parser.results = (ParseResults) {
        .allocator = global_heap_allocator,

        .files = NULL,
        .nodes_to_process = NULL,
    };

    bh_arr_new(parser.results.allocator, parser.results.files, 4);
    bh_arr_new(parser.results.allocator, parser.results.nodes_to_process, 4);

    return parser;
}

void onyx_parser_free(OnyxParser* parser) {
}

ParseResults onyx_parse(OnyxParser *parser) {
    if (parser->curr->type == Token_Type_Keyword_Package) {
        consume_token(parser);

        OnyxToken* symbol = expect_token(parser, Token_Type_Symbol);

        token_toggle_end(symbol);
        Package *package = program_info_package_lookup_or_create(
            parser->program,
            symbol->text,
            parser->program->global_scope,
            parser->allocator);
        token_toggle_end(symbol);

        parser->package = package;

    } else {
        Package *package = program_info_package_lookup_or_create(
            parser->program,
            "main",
            parser->program->global_scope,
            parser->allocator);

        parser->package = package;
    }

    while (parser->curr->type != Token_Type_End_Stream) {
        AstNode* curr_stmt = parse_top_level_statement(parser);

        if (curr_stmt != NULL && curr_stmt != &error_node) {
            while (curr_stmt != NULL) {

                switch (curr_stmt->kind) {
                    case Ast_Kind_Include_File: bh_arr_push(parser->results.files, (AstIncludeFile *) curr_stmt); break;
                    case Ast_Kind_Binding: {
                        symbol_introduce(parser->package->scope,
                            ((AstBinding *) curr_stmt)->token,
                            ((AstBinding *) curr_stmt)->node);
                        break;
                    }

                    default: assert(("Invalid top level node", 0));
                }

                curr_stmt = curr_stmt->next;
            }
        }
    }

    return parser->results;
}
