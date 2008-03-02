" slimvim.vim:  The Superior Lisp Interaction Mode for VIM
" Last Change:	2008 Mar 02
" Maintainer:	Tamas Kovacs <kovisoft@gmail.com>
" License:	This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"  Issues:
"  - register s is used
"
"  TODO: make it work on Linux
"  TODO: is it possible to redirect output to VIM buffer?
"  TODO: compile related functions and keybindings
"  TODO: documentation commands
"  TODO: possibility to use cmd frontend (like Console: console "/k <command>")
" ---------------------------------------------------------------------
"  Mini-FAQ:
"  - Q: Why is this plugin called 'Slimvim'?
"  - A: Because it is trying to mimic the popular Emacs extension 'SLIME'.
"       In SLIME 'E' stands for 'Emacs', so here it is replaced with 'vim'.
"
"  - Q: Why another 'Superior Lisp Mode' if there is already one?
"  - A: Because many programmers prefer VIM as a program text editor
"       over Emacs, including me. I don't want to start a holy war or
"       whatever, I'm just happy if someone else finds this plugin useful.
"
"  - Q: How does Slimvim work?
"  - A: Slimvim consists of three parts: VIM plugin, client and server.
"       The Slimvim server is a swank server that embeds a Lisp REPL.
"       The Slimvim client interfaces with the server and is responsible
"       for sending Lisp commands to the Lisp REPL.
"       The VIM plugin is translating editor commands to Lisp commands to be
"       sent to the server by the client.
"       So the dataflow is like this:
"       VIM -> VIM plugin -> Slimvim client -> Slimvim server -> Lisp REPL
"       The plugin resides in 'slimvim.vim', the client and the server both
"       reside in 'slimvim.py'.
"
"  - Q: Why is the default port number 5151?
"  - A: Hint: what roman numbers are 5,1,5,1? Bingo: V,I,V,I or double VI.
"
"  - Q: Are you a Lisp expert?
"  - A: No, not at all. I'm just learning Lisp.
" =====================================================================
"  Load Once:
if &cp || exists("g:slimvim_loaded")
    finish
endif
let g:slimvim_loaded        = 1
let g:slimvim_loaded_python = 0

" =====================================================================
"  Global variable definitions
" =====================================================================

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
    let g:slimvim_template_undefine = '(fmakunbound (read-from-string "%par1%"))'
endif

if !exists("g:slimvim_template_describe")
    let g:slimvim_template_describe = '(describe (read-from-string "%par1%"))'
endif

if !exists("g:slimvim_template_disassemble")
    let g:slimvim_template_disassemble = "(disassemble #'%par1%)"
endif

if !exists("g:slimvim_template_apropos")
    let g:slimvim_template_apropos = '(apropos "%par1%")'
endif

if !exists("g:slimvim_template_macroexpand")
    let g:slimvim_template_macroexpand = '(pprint %par1%)'
endif

if !exists("g:slimvim_template_macroexpand_all")
    let g:slimvim_template_macroexpand_all = '(pprint %par1%)'
endif

if !exists("g:slimvim_template_compile_file")
    let g:slimvim_template_compile_file = '(compile-file "%par1%")'
endif

if !exists("mapleader")
    let mapleader = ','
endif

"vim.command( 'let user_input = input( "Enter something" )' )
"user_input = vim.eval( "user_input" )

" =====================================================================
"  General utility functions
" =====================================================================

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
    "normal va("sy
    normal va(o
    " Handle '() or #'() etc. type special syntax forms
    " TODO: what to do with ` operator?
    let c = col(".") - 2
    while c > 0 && match(' \t()', getline(".")[c]) < 0
        normal h
	let c = c - 1
    endwhile
    normal "sy
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
    "TODO: handle continuous (not whole line) selection case
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
    let p1 = substitute(a:par1, '&', '\\&', "g")  " & -> \&
    let temp1 = substitute(a:template, '%par1%', p1, "g")
    let lines = [temp1]
    call SlimvimEval(lines)
endfunction

" Eval Lisp form, with the given parameters substituted in the template.
" %par1% string is substituted with par1
" %par2% string is substituted with par2
function! SlimvimEvalForm2(template, par1, par2)
    let p1 = substitute(a:par1, '&', '\\&', "g")  " & -> \&
    let p2 = substitute(a:par2, '&', '\\&', "g")  " & -> \&
    let temp1 = substitute(a:template, '%par1%', p1, "g")
    let temp2 = substitute(temp1,      '%par2%', p2, "g")
    let lines = [temp2]
    call SlimvimEval(lines)
endfunction

" =====================================================================
"  Special functions
" =====================================================================

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
    call SlimvimEvalForm1(g:slimvim_template_undefine, SlimvimGetSelection())
endfunction

function! SlimvimDescribeSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_describe, SlimvimGetSelection())
endfunction

function! SlimvimDisassemble()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_disassemble, SlimvimGetSelection())
endfunction

function! SlimvimApropos()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_apropos, SlimvimGetSelection())
endfunction

function! SlimvimMacroexpand()
    normal 99[(vt(%"sy
    let m = SlimvimGetSelection() . "))"
    let m = substitute(m, "defmacro\\s*", "macroexpand-1 '(", "g")
    call SlimvimEvalForm1(g:slimvim_template_macroexpand, m)
endfunction

function! SlimvimMacroexpandAll()
    normal 99[(vt(%"sy
    let m = SlimvimGetSelection() . "))"
    let m = substitute(m, "defmacro\\s*", "macroexpand '(", "g")
    call SlimvimEvalForm1(g:slimvim_template_macroexpand_all, m)
endfunction

function! SlimvimCompileFile()
    let f = input( "Compile filename: " )
    call SlimvimEvalForm1(g:slimvim_template_compile_file, f)
endfunction

" =====================================================================
"  Slimvim keybindings
" =====================================================================

" <Leader> can be set in .vimrc, it defaults here to ','
" <Leader> timeouts in 1000 msec by default, if this is too short,
" then increase 'timeoutlen'
" SLIME: <C-A-x>
map <Leader>d :call SlimvimEvalDefun()<CR>
" SLIME: <C-x> <C-e>
map <Leader>e :call SlimvimEvalLastExp()<CR>
" SLIME: ???
map <Leader>p :call SlimvimPprintEvalLastExp()<CR>
" SLIME: <C-c> <C-r>
map <Leader>r :call SlimvimEvalRegion()<CR>
" SLIME: ???
map <Leader>b :call SlimvimEvalBuffer()<CR>
" SLIME: ???
map <Leader>u :call SlimvimUndefineFunction()<CR>
" SLIME: ???
map <Leader>s :call SlimvimDescribeSymbol()<CR>
" SLIME: ???
map <Leader>i :call SlimvimDisassemble()<CR>
" SLIME: ???
map <Leader>a :call SlimvimApropos()<CR>
" SLIME: ???
map <Leader>1 :call SlimvimMacroexpand()<CR>
" SLIME: ???
map <Leader>m :call SlimvimMacroexpandAll()<CR>
" SLIME: ???
map <Leader>f :call SlimvimCompileFile()<CR>

