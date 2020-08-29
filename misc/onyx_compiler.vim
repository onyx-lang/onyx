" Vim compiler file
" Compiler:             Onyx
" Previous Maintainer:  Brendan Hansen <brendan.f.hansen@gmail.com>
" Latest Revision:      2020-08-29

if exists("current_compiler")
  finish
endif
let current_compiler = "onyx"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=%E\(%f\:%l\\,%c\)\ \%m,%C%.%#

set makeprg=onyx\ %

let &cpo = s:cpo_save
unlet s:cpo_save
