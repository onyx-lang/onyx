" Vim syntax file
" Language: Onyx
" Maintainer: Brendan Hansen <brendan.f.hansen@gmail.com>
" Last Change: 2020 June 20

if exists("b:current_syntax")
	finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword onyxKeyword struct
syn keyword onyxKeyword use
syn keyword onyxKeyword if
syn keyword onyxKeyword elseif
syn keyword onyxKeyword else
syn keyword onyxKeyword export
syn keyword onyxKeyword proc
syn keyword onyxKeyword foreign
syn keyword onyxKeyword for
syn keyword onyxKeyword return
syn keyword onyxKeyword do
syn keyword onyxKeyword global
syn keyword onyxKeyword as

syn keyword onyxType i32
syn keyword onyxType i64
syn keyword onyxType f32
syn keyword onyxType f64

syn keyword onyxCommentStart	contained TODO NOTE BUG HACK

syn region onyxComment start="//" end="$" keepend contains=onyxCommentStart

hi def link onyxKeyword		Statement
hi def link onyxType 		Type
hi def link onyxComment		Comment
hi def link onyxCommentStart 	Todo

let b:current_syntax = "onyx"
let &cpo = s:cpo_save
unlet s:cpo_save
