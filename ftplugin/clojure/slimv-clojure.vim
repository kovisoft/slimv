" slimv-clojure.vim:
"               Clojure filetype plugin for Slimv
" Version:      0.8.0
" Last Change:  10 Apr 2011
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

" Transform filename so that it will not contain spaces
function! s:TransformFilename( name )
    if match( a:name, ' ' ) >= 0
        return fnamemodify( a:name , ':8' )
    else
        return a:name
    endif
endfunction

" Build a Clojure startup command by adding
" all clojure*.jar files found to the classpath
function! b:SlimvBuildStartCmd( lisps )
    let cp = s:TransformFilename( a:lisps[0] )
    let i = 1
    while i < len( a:lisps )
        let cp = cp . ';' . s:TransformFilename( a:lisps[i] )
        let i = i + 1
    endwhile
    if g:slimv_swank
        " Try to find swank-clojure and add it to classpath
        let swanks = split( globpath( &runtimepath, 'swank-clojure'), '\n' )
        if len( swanks ) > 0
            let cp = cp . ';' . s:TransformFilename( swanks[0] )
        endif
    endif
    return ['java -cp ' . cp . ' clojure.main', 'clojure']
endfunction

" Try to autodetect Clojure executable
" Returns list [Clojure executable, Clojure implementation]
function! b:SlimvAutodetect()
    " Firts try the most basic setup: everything in the path
    if executable( 'lein' )
        return ['"lein repl"', 'clojure']
    endif
    if executable( 'clojure' )
        return ['clojure', 'clojure']
    endif
    let lisps = []
    if executable( 'clojure.jar' )
        let lisps = ['clojure.jar']
    endif
    if executable( 'clojure-contrib.jar' )
        let lisps = lisps + 'clojure-contrib.jar'
    endif
    if len( lisps ) > 0
        return b:SlimvBuildStartCmd( lisps )
    endif

    if g:slimv_swank
        " Check if Clojure is bundled with Slimv
        let lisps = split( globpath( &runtimepath, 'swank-clojure/clojure*.jar'), '\n' )
        if len( lisps ) > 0
            return b:SlimvBuildStartCmd( lisps )
        endif
    endif

    " Try to find Clojure in the PATH
    let path = substitute( $PATH, ';', ',', 'g' )
    let lisps = split( globpath( path, 'clojure*.jar' ), '\n' )
    if len( lisps ) > 0
        return b:SlimvBuildStartCmd( lisps )
    endif

    if g:slimv_windows
        " Try to find Clojure on the standard installation places
        let lisps = split( globpath( 'c:/*clojure*,c:/*clojure*/lib', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return b:SlimvBuildStartCmd( lisps )
        endif
    else
        " Try to find Clojure in the home directory
        let lisps = split( globpath( '/usr/local/bin/*clojure*', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return b:SlimvBuildStartCmd( lisps )
        endif
        let lisps = split( globpath( '~/*clojure*', 'clojure*.jar' ), '\n' )
        if len( lisps ) > 0
            return b:SlimvBuildStartCmd( lisps )
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
runtime ftplugin/**/lisp.vim
runtime ftplugin/**/slimv.vim

