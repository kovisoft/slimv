" Slimvim:      The Superior Lisp Interaction Mode for VIM
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
    let g:slimvim_python    = "c:\python24"
endif

if !exists("g:slimvim_lisp")
    let g:slimvim_lisp      = "c:\lispbox\clisp-2.37\clisp.exe"
endif

"py import vim
"py import sys

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

function! SlimvimSendRange()
    call SlimvimLoad()
"    py sys.argv=['slimvim.py', '-c'] + vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    py print '>>>', os.environ
"    py sys.argv = [os.environ.get('VIMRUNTIME')+'/plugin/slimvim.py', '-c'] + 
"                  \ vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
    py sys.argv = [vim.eval("g:slimvim_path"), '-c'] + 
                  \ vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
"    pyfile c:\python24\slimvim.py
"    pyfile c:\Program Files\Vim\vim71\plugin\slimvim.py
"    pyfile $VIMRUNTIME/plugin/slimvim.py
    execute ":pyfile " . g:slimvim_path
endfunction

function! SlimvimSendAll()
    call SlimvimLoad()
"    py sys.argv=[os.environ.get('VIMRUNTIME')+'/plugin/slimvim.py', '-c'] + vim.current.buffer[0:]
    py sys.argv=[vim.eval("g:slimvim_path"), '-c'] + vim.current.buffer[0:]
"    pyfile c:\python24\slimvim.py
"    pyfile $VIMRUNTIME/plugin/slimvim.py
    execute ":pyfile " . g:slimvim_path
endfunction

"map <A-F5> :py import vim<CR>:py import sys<CR>
"map <C-F5> :py sys.argv=['']+vim.current.buffer[vim.current.range.start:vim.current.range.end+1]<CR>:pyfile c:\python24\slimvim.py<CR>
map <C-F5> :call SlimvimSendRange()<CR>
map <C-F6> :call SlimvimSendAll()<CR>

