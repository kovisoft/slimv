" slimv-scheme.vim:
"               Scheme filetype plugin for Slimv
" Version:      0.8.2
" Last Change:  27 Apr 2011
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if &cp || exists( 'g:slimv_scheme_loaded' )
    finish
endif

let g:slimv_scheme_loaded = 1

" Try to autodetect Scheme executable
" Returns list [Scheme executable, Scheme implementation]
function! b:SlimvAutodetect()
    " Currently only MIT Scheme on Linux
    if executable( 'scheme' )
        " MIT Scheme
        return ['scheme', 'mit']
    endif

    return ['', '']
endfunction

" Try to find out the Scheme implementation
function! b:SlimvImplementation()
    if exists( 'g:slimv_impl' ) && g:slimv_impl != ''
        " Return Lisp implementation if defined
        return tolower( g:slimv_impl )
    endif

    return 'mit'
endfunction

" Filename for the REPL buffer file
function! b:SlimvREPLFile()
    return 'Slimv.REPL.scm'
endfunction

" Lookup symbol in the Hyperspec
function! b:SlimvHyperspecLookup( word, exact, all )
    " No Hyperspec support for Scheme at the moment
    let symbol = []
    return symbol
endfunction

" Source Slimv general part
runtime ftplugin/**/slimv.vim

