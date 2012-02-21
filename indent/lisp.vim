" lisp.vim:
"               Lisp indent plugin for Slimv
" Version:      0.9.5
" Last Change:  21 Feb 2012
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

" Handle cases when lisp dialects explicitly use the lisp indent plugins
if &ft == "clojure" && exists("g:slimv_disable_clojure")
    finish
endif

if &ft == "scheme" && exists("g:slimv_disable_scheme")
    finish
endif 

setlocal nolisp
setlocal autoindent
setlocal expandtab
setlocal indentexpr=SlimvIndent(v:lnum)

