" lisp.vim:
"               Lisp indent plugin for Slimv
" Version:      0.8.3
" Last Change:  14 May 2011
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:did_indent")
   finish
endif

setlocal nolisp
setlocal autoindent
setlocal indentexpr=SlimvIndent(v:lnum)

