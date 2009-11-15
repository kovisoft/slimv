" clojure.vim:
"               Clojure indent plugin for Slimv
" Version:      0.5.4
" Last Change:  15 Nov 2009
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

runtime indent/**/lisp.vim

set lisp

