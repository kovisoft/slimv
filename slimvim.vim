" slimvim.vim:  The Superior Lisp Interaction Mode for VIM
" Last Change:	2008 Feb 24
" Maintainer:	Tamas Kovacs <kovisoft@gmail.com>
" License:	This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("g:slimvim_loaded")
    finish
endif
let g:slimvim_loaded        = 1
let g:slimvim_loaded_python = 0

if !exists("g:slimvim_path")
"    let g:slimvim_path = $VIMRUNTIME . "/plugin/slimvim.py"
    let g:slimvim_path = $VIM . "/vimfiles/plugin/slimvim.py"
endif

if !exists("g:slimvim_python")
    let g:slimvim_python    = "c:/python24/python.exe"
endif

if !exists("g:slimvim_lisp")
    let g:slimvim_lisp      = "c:/lispbox/clisp-2.37/clisp.exe"
endif

"py import vim
"py import sys
"
"vim.command( 'let user_input = input( "Enter something" )' )
"user_input = vim.eval( "user_input" )


function! SlimvimLoad()
    if g:slimvim_loaded_python == 0
        py import vim
        py import sys
        py import os
        let g:slimvim_loaded_python = 1
"	let g:slimvim_path = $VIMRUNTIME . '/plugin/slimvim.py'
"	echo g:slimvim_path
    endif
endfunction

function! SlimvimEval(args)
"    echo a:args
    call SlimvimLoad()
"    py sys.argv=['slimvim.py', '-c'] + vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    py print '>>>', os.environ
"    py sys.argv = [os.environ.get('VIMRUNTIME')+'/plugin/slimvim.py', '-c'] + 
"                  \ vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    let s:xxx = py vim.current.buffer[vim.current.range.start]
    py sys.argv = [vim.eval("g:slimvim_path"),
                  \ '-p', vim.eval("g:slimvim_python"),
                  \ '-l', vim.eval("g:slimvim_lisp"), '-c'] + 
                  \ vim.eval("a:args")
"                  \ [vim.eval("a:args")]
    execute ":pyfile " . g:slimvim_path
endfunction

function! SlimvimEvalDefun()
endfunction

function! SlimvimEvalLastExp()
endfunction

function! SlimvimPprintEvalLastExp()
"                 (dolist (o list)
"                   (pprint o))
endfunction

function! SlimvimEvalRegion()
"    call SlimvimLoad()

"    py sys.argv=['slimvim.py', '-c'] + vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    py print '>>>', os.environ
"    py sys.argv = [os.environ.get('VIMRUNTIME')+'/plugin/slimvim.py', '-c'] + 
"                  \ vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    let s:xxx = py vim.current.buffer[vim.current.range.start]
"    let lines = getline(".")
"TODO: In visual mode this is called in a loop for all lines
    let lines = getline("'<", "'>")
    call SlimvimEval(lines)
"    py sys.argv = [vim.eval("g:slimvim_path"),
"                  \ '-p', vim.eval("g:slimvim_python"),
"                  \ '-l', vim.eval("g:slimvim_lisp"), '-c'] + 
"                  \ vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    execute ":pyfile " . g:slimvim_path
endfunction

function! SlimvimEvalBuffer()
    let lines = getline(1, '$')
    call SlimvimEval(lines)

"    call SlimvimLoad()
""    py sys.argv=[os.environ.get('VIMRUNTIME')+'/plugin/slimvim.py', '-c'] + vim.current.buffer[0:]
"    py sys.argv = [vim.eval("g:slimvim_path"),
"                  \ '-p', vim.eval("g:slimvim_python"),
"                  \ '-l', vim.eval("g:slimvim_lisp"), '-c'] + 
"                  \ vim.current.buffer[0:]
""    pyfile c:\python24\slimvim.py
""    pyfile c:\Program Files\Vim\vim71\plugin\slimvim.py
""    pyfile $VIMRUNTIME/plugin/slimvim.py
"    execute ":pyfile " . g:slimvim_path
endfunction

"map <A-F5> :py import vim<CR>:py import sys<CR>
"map <C-F5> :py sys.argv=['']+vim.current.buffer[vim.current.range.start:vim.current.range.end+1]<CR>:pyfile c:\python24\slimvim.py<CR>
" SLIME: <C-A-x>
map <C-F1> :call SlimvimEvalDefun()<CR>
" SLIME: <C-x> <C-e>
map <C-F2> :call SlimvimEvalLastExp()<CR>
" SLIME: 
map <C-F3> :call SlimvimPprintEvalLastExp()<CR>
" SLIME: <C-c> <C-r>
map <C-F5> :call SlimvimEvalRegion()<CR>
" SLIME: ???
map <C-F6> :call SlimvimEvalBuffer()<CR>

