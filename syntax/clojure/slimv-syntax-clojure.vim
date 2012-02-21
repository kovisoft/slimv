" slimv-syntax-clojure.vim:
"               Clojure syntax plugin for Slimv
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
if exists("b:current_syntax") || exists("g:slimv_disable_clojure")
  finish
endif

" Clojure keywords not defined by lisp.vim
syn keyword lispFunc def defmulti defn defonce defprotocol doall dorun doseq dosync doto
syn keyword lispFunc filter fn for future in-ns letfn ns range str take try

runtime syntax/**/lisp.vim

" Add [] and {} to the lisp_rainbow handling
syn match			 lispSymbol			  contained			   ![^()\[\]{}'`,"; \t]\+!
syn match			 lispBarSymbol			  contained			   !|..\{-}|!
syn match			 lispAtom			  "'[^ \t()\[\]{}]\+"		   contains=lispAtomMark
if exists("g:lisp_rainbow") && g:lisp_rainbow != 0
    syn region lispParen0           matchgroup=hlLevel0 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen1 
    syn region lispParen1 contained matchgroup=hlLevel1 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen2 
    syn region lispParen2 contained matchgroup=hlLevel2 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen3 
    syn region lispParen3 contained matchgroup=hlLevel3 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen4 
    syn region lispParen4 contained matchgroup=hlLevel4 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen5 
    syn region lispParen5 contained matchgroup=hlLevel5 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen6 
    syn region lispParen6 contained matchgroup=hlLevel6 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen7 
    syn region lispParen7 contained matchgroup=hlLevel7 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen8 
    syn region lispParen8 contained matchgroup=hlLevel8 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen9 
    syn region lispParen9 contained matchgroup=hlLevel9 start="`\=\[" end="\]" skip="|.\{-}|" contains=@lispListCluster,lispParen0

    syn region lispParen0           matchgroup=hlLevel0 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen1 
    syn region lispParen1 contained matchgroup=hlLevel1 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen2 
    syn region lispParen2 contained matchgroup=hlLevel2 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen3 
    syn region lispParen3 contained matchgroup=hlLevel3 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen4 
    syn region lispParen4 contained matchgroup=hlLevel4 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen5 
    syn region lispParen5 contained matchgroup=hlLevel5 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen6 
    syn region lispParen6 contained matchgroup=hlLevel6 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen7 
    syn region lispParen7 contained matchgroup=hlLevel7 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen8 
    syn region lispParen8 contained matchgroup=hlLevel8 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen9 
    syn region lispParen9 contained matchgroup=hlLevel9 start="`\={"  end="}"  skip="|.\{-}|" contains=@lispListCluster,lispParen0
endif

