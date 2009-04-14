" slimv-lisp.vim:
"               Lisp filetype plugin for Slimv
" Version:      0.5.0
" Last Change:  14 Apr 2009
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if &cp || exists( 'g:slimv_lisp_loaded' )
    finish
endif

let g:slimv_lisp_loaded = 1

runtime ftplugin/**/slimv.vim

" Lookup symbol in the list of Lisp Hyperspec symbol databases
function! b:HyperspecLookup( word, exact )
    if !exists( 'g:slimv_clhs_loaded' )
        runtime ftplugin/**/slimv-clhs.vim
    endif

    let symbol = ['', '']
    if exists( 'g:slimv_clhs_loaded' )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_clhs,          g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_issues,        g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_chapters,      g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_control_chars, g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_macro_chars,   g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_loop,          g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_arguments,     g:slimv_clhs_root, symbol )
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_glossary,      g:slimv_clhs_root, symbol )
    endif
    if exists( 'g:slimv_clhs_user_db' )
	" Give a choice for the user to extend the symbol database
        if exists( 'g:slimv_clhs_user_root' )
            let user_root = g:slimv_clhs_user_root
        else
            let user_root = ''
        endif
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_clhs_user_db, user_root, symbol )
    endif
    return symbol
endfunction

