" slimvim : The Superior Lisp Interaction Mode for VIM
" Author: Tamas Kovacs, 2008
"            No warranty, express or implied.
"  *** ***   Use At-Your-Own-Risk!   *** ***
"
" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("g:slimvim_loaded")
 finish
endif
let g:slimvim_loaded        = 1
let g:slimvim_loaded_python = 0

"py import vim
"py import sys

function! SlimvimLoad()
    if g:slimvim_loaded_python == 0
        py import vim
        py import sys
        let g:slimvim_loaded_python = 1
    endif
endfunction

function! SlimvimSendRange()
    call SlimvimLoad()
    py sys.argv=['slimvim.py', '-c'] + vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
    pyfile c:\python24\slimvim.py
endfunction

function! SlimvimSendAll()
    call SlimvimLoad()
    py sys.argv=['']+vim.current.buffer[0:]
    pyfile c:\python24\slimvim.py
endfunction

"map <A-F5> :py import vim<CR>:py import sys<CR>
"map <C-F5> :py sys.argv=['']+vim.current.buffer[vim.current.range.start:vim.current.range.end+1]<CR>:pyfile c:\python24\slimvim.py<CR>
map <C-F5> :call SlimvimSendRange()<CR>
map <C-F6> :call SlimvimSendAll()<CR>

