" slimvim.vim:  The Superior Lisp Interaction Mode for VIM
" Last Change:	2008 Mar 01
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
"  TODO: is it possible to redirect output to VIM buffer?
"  TODO: handle '(...) and #'(,,,), etc type s-expressions
"  TODO: compile related functions and keybindings
"  TODO: documentation commands
"  TODO: possibility to use cmd frontend (like Console: console "/k <command>")
" ---------------------------------------------------------------------
"  Load Once:
if &cp || exists("g:slimvim_loaded")
    finish
endif
let g:slimvim_loaded        = 1
let g:slimvim_loaded_python = 0

" ---------------------------------------------------------------------
"  Global variable definitions
" ---------------------------------------------------------------------

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

let g:slimvim_command = 'console -r "/k ' . g:slimvim_python . ' \"' . g:slimvim_path . '\" -l ' . g:slimvim_lisp . ' -s"'
"let g:slimvim_command = g:slimvim_python . ' "' . g:slimvim_path . '" -l ' . g:slimvim_lisp . ' -s'

if !exists("g:slimvim_template_pprint")
    let g:slimvim_template_pprint = '(dolist (o %par1%)(pprint o))'
endif

if !exists("g:slimvim_template_undefine")
    let g:slimvim_template_undefine = '(fmakunbound (read-from-string %par1%))'
endif

"vim.command( 'let user_input = input( "Enter something" )' )
"user_input = vim.eval( "user_input" )

" ---------------------------------------------------------------------
"  General utility functions
" ---------------------------------------------------------------------

" Load Python library and necessary modules
function! SlimvimLoad()
    if g:slimvim_loaded_python == 0
        py import vim
        py import sys
        py import os
        let g:slimvim_loaded_python = 1
    endif
endfunction

" Select symbol under cursor and copy it to register 's'
function! SlimvimSelectSymbol()
    normal viw"sy
endfunction

" Select bottom level form the cursor is inside and copy it to register 's'
function! SlimvimSelectForm()
    normal va("sy
endfunction

" Select top level form the cursor is inside and copy it to register 's'
function! SlimvimSelectToplevelForm()
    normal 99[(
    call SlimvimSelectForm()
endfunction

" Return the contents of register 's'
function! SlimvimGetSelection()
    return getreg('"s')
endfunction

" Send argument to Lisp server for evaluation
function! SlimvimEval(args)
    call SlimvimLoad()
    if exists("g:slimvim_command")
        py sys.argv = [vim.eval("g:slimvim_path"),
                      \ '-r', vim.eval("g:slimvim_command"), '-c'] + 
                      \ vim.eval("a:args")
    else
        py sys.argv = [vim.eval("g:slimvim_path"),
                      \ '-p', vim.eval("g:slimvim_python"),
                      \ '-l', vim.eval("g:slimvim_lisp"), '-c'] + 
                      \ vim.eval("a:args")
    endif
    execute ":pyfile " . g:slimvim_path
endfunction

" Eval buffer lines in the given range
function! SlimvimEvalRegion() range
    if mode() == "v" || mode() == "V"
        let lines = getline(a:firstline, a:lastline)
    else
        let lines = getline("'<", "'>")
    endif
    call SlimvimEval(lines)
endfunction

" Eval contents of the 's' register
function! SlimvimEvalSelection()
    let lines = []
    call add(lines, SlimvimGetSelection())
    call SlimvimEval(lines)
endfunction

" Eval Lisp form.
" Form given in the template is passed to Lisp without modification.
function! SlimvimEvalForm(template)
    let lines = [a:template]
    call SlimvimEval(lines)
endfunction

" Eval Lisp form, with the given parameter substituted in the template.
" %par1% string is substituted with par1
function! SlimvimEvalForm1(template, par1)
    let temp1 = substitute(a:template, '%par1%', a:par1, "g")
    let lines = [temp1]
    call SlimvimEval(lines)
endfunction

" Eval Lisp form, with the given parameters substituted in the template.
" %par1% string is substituted with par1
" %par2% string is substituted with par2
function! SlimvimEvalForm2(template, par1, par2)
    let temp1 = substitute(a:template, '%par1%', a:par1, "g")
    let temp2 = substitute(temp1,      '%par2%', a:par2, "g")
    let lines = [temp2]
    call SlimvimEval(lines)
endfunction

" ---------------------------------------------------------------------
"  Special functions
" ---------------------------------------------------------------------

" Evaluate the whole buffer
function! SlimvimEvalBuffer()
    let lines = getline(1, '$')
    call SlimvimEval(lines)
endfunction

function! SlimvimEvalLastExp()
    call SlimvimSelectForm()
    call SlimvimEvalSelection()
endfunction

function! SlimvimPprintEvalLastExp()
    call SlimvimSelectForm()
    call SlimvimEvalForm1(g:slimvim_template_pprint, SlimvimGetSelection())
endfunction

function! SlimvimEvalDefun()
    call SlimvimSelectToplevelForm()
    call SlimvimEvalSelection()
endfunction

function! SlimvimUndefineFunction()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_undefine, '"' . SlimvimGetSelection() . '"')
endfunction

" ---------------------------------------------------------------------
"  Slimvim keybindings
" ---------------------------------------------------------------------

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

