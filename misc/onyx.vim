" Vim syntax file
" Language: Onyx
" Maintainer: Brendan Hansen <brendan.f.hansen@gmail.com>
" Last Change: 2020 June 20

if exists("b:current_syntax")
	finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword onyxKeyword struct proc use export foreign global
syn keyword onyxKeyword if elseif else
syn keyword onyxKeyword for while loop return do
syn keyword onyxKeyword return
syn keyword onyxKeyword as

syn keyword onyxType i32
syn keyword onyxType i64
syn keyword onyxType f32
syn keyword onyxType f64

syn keyword onyxConstant        true false

syn keyword onyxCommentStart	contained TODO NOTE BUG HACK

syn region onyxComment start="//" end="$" keepend contains=onyxCommentStart

hi def link onyxKeyword		    Statement
hi def link onyxType 		    Type
hi def link onyxComment		    Comment
hi def link onyxCommentStart 	Todo
hi def link onyxConstant        Constant

let b:current_syntax = "onyx"
let &cpo = s:cpo_save
unlet s:cpo_save
