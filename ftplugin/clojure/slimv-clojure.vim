" slimv-clojure.vim:
"               Clojure filetype plugin for Slimv
" Version:      0.5.2
" Last Change:  04 May 2009
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

" Try to autodetect Clojure executable
" Returns list [Clojure executable, Clojure implementation]
function! b:SlimvAutodetect()
    if executable( 'clojure.jar' )
        " Clojure
        return ['"java -cp clojure.jar clojure.lang.Repl"', 'clojure']
    endif

    " Try to find Clojure in the PATH
    let path = substitute( $PATH, ';', ',', 'g' )
    let lisps = split( globpath( path, 'clojure*.jar' ), '\n' )
    if len( lisps ) > 0
        return ['"java -cp ' . lisps[0] . ' clojure.lang.Repl"', 'clojure']
    endif

    if g:slimv_windows
        " Try to find Clojure on the standard installation places
        let lisps = split( globpath( 'c:/*clojure*', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return ['"java -cp ' . lisps[0] . ' clojure.lang.Repl"', 'clojure']
        endif
    endif

    return ['', '']
endfunction

" Try to find out the Clojure implementation
function! b:SlimvImplementation()
    if exists( 'g:slimv_impl' ) && g:slimv_impl != ''
        " Return Lisp implementation if defined
        return tolower( g:slimv_impl )
    endif

    return 'clojure'
endfunction

" Filename for the REPL buffer file
function! b:SlimvREPLFile()
    return 'Slimv.REPL.clj'
endfunction

" Lookup symbol in the list of Clojure Hyperspec symbol databases
function! b:SlimvHyperspecLookup( word, exact, all )
    if !exists( 'g:slimv_cljapi_loaded' )
        runtime ftplugin/**/slimv-cljapi.vim
    endif

    if !exists( 'g:slimv_javadoc_loaded' )
        runtime ftplugin/**/slimv-javadoc.vim
    endif

    let symbol = []
    if exists( 'g:slimv_cljapi_db' )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_cljapi_db,  g:slimv_cljapi_root,  symbol )
    endif
    if exists( 'g:slimv_javadoc_db' )
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_javadoc_db, g:slimv_javadoc_root, symbol )
    endif
    if exists( 'g:slimv_cljapi_user_db' )
        " Give a choice for the user to extend the symbol database
        if exists( 'g:slimv_cljapi_user_root' )
            let user_root = g:slimv_cljapi_user_root
        else
            let user_root = ''
        endif
        let symbol = SlimvFindSymbol( a:word, a:exact, a:all, g:slimv_cljapi_user_db, user_root, symbol )
    endif
    return symbol
endfunction

" Source Slimv general part
runtime ftplugin/**/slimv.vim

