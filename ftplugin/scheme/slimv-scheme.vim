" slimv-scheme.vim:
"               Scheme filetype plugin for Slimv
" Version:      0.8.4
" Last Change:  28 May 2011
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if exists("b:did_ftplugin")
    finish
endif

" ---------- Begin part loaded once ----------
if !exists( 'g:slimv_scheme_loaded' )

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

endif "!exists( 'g:slimv_lisp_loaded' )
" ---------- End of part loaded once ----------

runtime ftplugin/**/lisp.vim

" Must be called for each lisp buffer
call SlimvInitBuffer()

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

