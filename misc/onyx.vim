" Vim syntax file
" Language: Onyx
" Maintainer: Brendan Hansen <brendan.f.hansen@gmail.com>
" Last Change: 2020 June 20

if exists("b:current_syntax")
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword onyxKeyword package struct enum proc use global
syn keyword onyxKeyword if elseif else
syn keyword onyxKeyword for while do
syn keyword onyxKeyword switch case
syn keyword onyxKeyword break continue return defer fallthrough syn keyword onyxKeyword as cast sizeof alignof
syn keyword onyxType bool void
syn keyword onyxType i8 u8
syn keyword onyxType i16 u16
syn keyword onyxType i32 u32
syn keyword onyxType i64 u64
syn keyword onyxType f32 f64
syn keyword onyxType rawptr
syn keyword onyxType str cstr
syn keyword onyxType i8x16 i16x8 i32x4 i64x2 f32x4 f64x2 v128
syn keyword onyxType type_expr

syn keyword onyxConstant        true false null null_proc

syn match onyxNumber            "\<0x[a-fA-F0-9]\+\>"
syn match onyxNumber            "\<\d\+[lf]\=\>"
syn match onyxNumber            "\<\d\+\.\d*f\=\>"
syn match onyxNumber            "\.\d\+f\=\>"

syn keyword onyxCommentStart    contained TODO NOTE BUG HACK

syn region onyxComment          start="//" end="$" keepend contains=onyxCommentStart
syn region onyxComment          start="/\*" end="\*/" contains=onyxCommentStart

syn match onyxDefinitionGroup   "\<[a-zA-Z_][a-zA-Z0-9_]*\> *::" contains=onyxDefinition
syn match onyxDefinition        "\<[a-zA-Z_][a-zA-Z0-9_]*\>" contained

syn match onyxCallGroup         "\<[a-zA-Z_][a-zA-Z0-9_\.]*\> *(" contains=onyxCall
syn match onyxCall              "\<[a-zA-Z_][a-zA-Z0-9_\.]*\>" contained

syn match onyxDirective         "\#[a-zA-Z_]\+"

syn region onyxString		    display start=+"+ skip=+\\\\\|\\"+ end=+"+ keepend

hi def link onyxKeyword          Statement
hi def link onyxType             Type
hi def link onyxComment          Comment
hi def link onyxCommentStart     Todo
hi def link onyxConstant         Constant
hi def link onyxDirective        Constant
hi def link onyxString           String
hi def link onyxNumber           Number
hi def link onyxDefinition       Identifier
hi def link onyxCall             Function
hi def link onyxOperator         Operator

let b:current_syntax = "onyx"
let &cpo = s:cpo_save
unlet s:cpo_save
