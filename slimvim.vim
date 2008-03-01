" slimvim.vim:  The Superior Lisp Interaction Mode for VIM
" Last Change:	2008 Feb 24
" Maintainer:	Tamas Kovacs <kovisoft@gmail.com>
" License:	This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" ---------------------------------------------------------------------
"  Issues:
"  - register s is used
"
"  TODO: swank server
"  TODO: make it work on Linux
"  TODO: handle not whole line selection
"  TODO: is it possible to redirect output to VIM buffer?
"  TODO: handle '(...) and #'(,,,), etc type s-expressions
"  TODO: compile related functions and keybindings
"  TODO: documentation commands
"  TODO: possibility to use cmd frontend (like Console: console "/k <command>")
"  Load Once:
if &cp || exists("g:slimvim_loaded")
    finish
endif
let g:slimvim_loaded        = 1
let g:slimvim_loaded_python = 0

" ---------------------------------------------------------------------
"  Global variable definitions

if !exists("g:slimvim_path")
"    let g:slimvim_path = $VIMRUNTIME . "/plugin/slimvim.py"
    let g:slimvim_path = $VIM . "/vimfiles/plugin/slimvim.py"
endif

if !exists("g:slimvim_python")
    let g:slimvim_python    = "c:/python24/python.exe"
"    let g:slimvim_python    = 'console -r "/k c:/python24/python.exe"'
endif

if !exists("g:slimvim_lisp")
    let g:slimvim_lisp      = "c:/lispbox/clisp-2.37/clisp.exe"
endif

"py import vim
"py import sys
"
"vim.command( 'let user_input = input( "Enter something" )' )
"user_input = vim.eval( "user_input" )

" ---------------------------------------------------------------------
"  General utility functions

function! SlimvimLoad()
    " 
    " Load Python library and necessary modules
    "
    if g:slimvim_loaded_python == 0
        py import vim
        py import sys
        py import os
        let g:slimvim_loaded_python = 1
"	let g:slimvim_path = $VIMRUNTIME . '/plugin/slimvim.py'
"	echo g:slimvim_path
    endif
endfunction

function! SlimvimSelectSymbol()
    normal viw"sy
endfunction

function! SlimvimSelectForm()
    normal va("sy
endfunction

function! SlimvimSelectToplevelForm()
    normal 99[(
    call SlimvimSelectForm()
endfunction

function! SlimvimEval(args)
    "
    " Send argument to Lisp server for evaluation
    "
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

function! SlimvimEvalRegion() range
    "
    " Eval buffer lines in the given range
    "
    if mode() == "v" || mode() == "V"
        let lines = getline(a:firstline, a:lastline)
    else
        let lines = getline("'<", "'>")
    endif
    call SlimvimEval(lines)
endfunction

function! SlimvimEvalSelection()
    "
    " Eval contents of the 's' register
    "
    let lines = []
    call add(lines, getreg('"s'))
    call SlimvimEval(lines)
endfunction

function! SlimvimEvalForm(template)
    "
    " Eval Lisp form.
    " Form given in the template is passed to Lisp without modification.
    "
    let lines = [a:template]
    call SlimvimEval(lines)
endfunction

function! SlimvimEvalForm1(template, par1)
    "
    " Eval Lisp form, with the given parameter substituted in the template.
    " %par1% string is substituted with par1
    "
    let temp1 = substitute(a:template, '%par1%', a:par1, "g")
    let lines = [temp1]
    call SlimvimEval(lines)
endfunction

function! SlimvimEvalForm2(template, par1, par2)
    "
    " Eval Lisp form, with the given parameters substituted in the template.
    " %par1% string is substituted with par1
    " %par2% string is substituted with par2
    "
    let temp1 = substitute(a:template, '%par1%', a:par1, "g")
    let temp2 = substitute(temp1,      '%par2%', a:par2, "g")
    let lines = [temp2]
    call SlimvimEval(lines)
endfunction

" ---------------------------------------------------------------------
"  Special functions

function! SlimvimEvalBuffer()
    "
    " Evaluate the whole buffer
    "
    let lines = getline(1, '$')
    call SlimvimEval(lines)
endfunction

function! SlimvimEvalLastExp()
    call SlimvimSelectForm()
    call SlimvimEvalSelection()
endfunction

function! SlimvimEvalDefun()
    call SlimvimSelectToplevelForm()
    call SlimvimEvalSelection()
endfunction

function! SlimvimPprintEvalLastExp()
    "normal va(v
    call SlimvimSelectForm()
    "normal va("sy
    "let lines = ["(dolist (o"] + getline("'<", "'>") + [")(pprint o))"]
"    let lines = ["(dolist (o" . getreg('"s') . ")(pprint o))"]
"    call SlimvimEval(lines)
"                 (dolist (o list)
"                   (pprint o))
    call SlimvimEvalForm1('(dolist (o %par1%)(pprint o))', getreg('"s'))
endfunction

function! SlimvimUndefineFunction()
    "normal viw"sy
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1('(fmakunbound (read-from-string %par1%))', '"' . getreg('"s') . '"')
endfunction

" ---------------------------------------------------------------------
"  Slimvim keybindings

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

