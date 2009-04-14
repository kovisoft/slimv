" slimv-clojure.vim:
"               Clojure filetype plugin for Slimv
" Version:      0.5.0
" Last Change:  31 Mar 2009
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if &cp || exists( 'g:slimv_clojure_loaded' )
    finish
endif

let g:slimv_clojure_loaded = 1

runtime ftplugin/**/slimv.vim

" Lookup symbol in the list of Clojure Hyperspec symbol databases
function! b:HyperspecLookup( word, exact )
    if !exists( 'g:slimv_cljapi_loaded' )
        runtime ftplugin/**/slimv-cljapi.vim
"        if !exists( 'g:slimv_cljapi_loaded' )
"            call SlimvError( "Clojure API database not found." )
"            return
"        endif
    endif

    if !exists( 'g:slimv_javadoc_loaded' )
        runtime ftplugin/**/slimv-javadoc.vim
"        if !exists( 'g:slimv_javadoc_loaded' )
"            call SlimvError( "JavaDoc database not found." )
"            return
"        endif
    endif

    let symbol = ['', '']
    let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_cljapi_db,  g:slimv_cljapi_root,  symbol )
    let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_javadoc_db, g:slimv_javadoc_root, symbol )
    if exists( 'g:slimv_cljapi_user_db' )
	" Give a choice for the user to extend the symbol database
        if exists( 'g:slimv_cljapi_user_root' )
            let user_root = g:slimv_cljapi_user_root
        else
            let user_root = ''
        endif
        let symbol = SlimvFindSymbol( a:word, a:exact, g:slimv_cljapi_user_db, user_root, symbol )
    endif
    return symbol
endfunction

