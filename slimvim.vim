" slimvim.vim:  The Superior Lisp Interaction Mode for VIM
" Last Change:	2008 Mar 04
" Maintainer:	Tamas Kovacs <kovisoft@gmail.com>
" License:	This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"  Issues:
"  - VIM register 's' is used
"  - (un)profile does not work
"  - needs Python 2.4 or higher
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
"  - Q: Why is SLIME functionality XYZ missing from Slimvim?
"  - A: There are two possible reasons:
"       1. The dataflow of Slimvim is one-directional: from client to server.
"          There is no data sent back from the server to the client, so if a
"          functionality requires that Slimvim reads data from REPL, then
"          currently it is not possible to implement it.
"       2. It is possible to implement it, but I did not (yet) do it.
"          Maybe future releases will contain it.
"
"  - Q: Why is the default port number 5151?
"  - A: Hint: what roman numbers are 5,1,5,1? Bingo: V,I,V,I or double VI.
"
"  - Q: Are you a Lisp expert?
"  - A: No, not at all. I'm just learning Lisp. Also just learning VIM
"       scripting. And I'm not a Python expert too, however (at the moment)
"       I have more experience with Python than with Lisp.
"
"  - Q: Why using Python for the client/server code? Why not Lisp?
"  - A: This is for historical reasons and may change in the future.
"       Preliminary versions used VIM's built-in Python support.
"       Later on the client/server code was separated from VIM but still
"       remained written in Python. On Linux this should not be a problem,
"       most Linux distributions contain a Python interpreter.
"       On Windows this means, you need to install Python, if you don't have
"       one (at least version 2.4). Anyway, Python is a nice language and
"       also a perfect replacement for calculator.exe :-)
" =====================================================================
"  Load Once:
if &cp || exists("g:slimvim_loaded")
    finish
endif

let g:slimvim_loaded        = 1
let g:slimvim_loaded_python = 0

if has("win32") || has("win95") || has("win64") || has("win16")
    let g:slimvim_windows   = 1
else
    let g:slimvim_windows   = 0
endif

" =====================================================================
"  Global variable definitions
" =====================================================================

if !exists('g:slimvim_port')
    "TODO: pass this to the client
    let g:slimvim_port = 5151
endif

if !exists('g:slimvim_path')
    if g:slimvim_windows
	"let g:slimvim_path = $VIMRUNTIME . "/plugin/slimvim.py"
	let g:slimvim_path = $VIM . '/vimfiles/plugin/slimvim.py'
    else
	let g:slimvim_path = $HOME . '/.vim/plugin/slimvim.py'
    endif
endif

if !exists('g:slimvim_python')
    if g:slimvim_windows
	"let g:slimvim_python    = 'python.exe'
	let g:slimvim_python    = 'c:/python24/python.exe'
    else
	let g:slimvim_python    = 'python'
    endif
endif

if !exists('g:slimvim_lisp')
    if g:slimvim_windows
	"let g:slimvim_lisp      = 'clisp.exe'
	"let g:slimvim_lisp      = 'c:/lispbox/clisp-2.37/clisp.exe'
"	let g:slimvim_lisp      = '\"c:/lispbox/clisp-2.37/clisp.exe -ansi\"'
	let g:slimvim_lisp      = '"c:/lispbox/clisp-2.37/clisp.exe -ansi"'
    else
	let g:slimvim_lisp      = 'clisp'
    endif
endif

if !exists('g:slimvim_server')
    if g:slimvim_windows
	let g:slimvim_command = g:slimvim_python . ' \"' . g:slimvim_path . '\"'
	"let g:slimvim_server = 'console -r "/k ' . g:slimvim_python . ' \"' . g:slimvim_path . '\" -l ' . g:slimvim_lisp . ' -s"'
	let g:slimvim_server = ':!start console -r "/k ' . g:slimvim_python . ' \"' . g:slimvim_path . '\" -l ' . g:slimvim_lisp . ' -s"'
	"let g:slimvim_server = 'console -r "/k c:/python24/python.exe \"c:/Program Files/Vim/vimfiles/plugin/slimvim.py\" -l \"c:/lispbox/clisp-2.37/clisp.exe -ansi\" -s"'
	"let g:slimvim_server = g:slimvim_python . ' "' . g:slimvim_path . '" -l ' . g:slimvim_lisp . ' -s'
    else
	let g:slimvim_server = ':!xterm -e ' . g:slimvim_python . ' ' . g:slimvim_path . ' -l ' . g:slimvim_lisp . ' -s &'
    endif
endif

if !exists('g:slimvim_client')
    if g:slimvim_windows
	"let g:slimvim_client = g:slimvim_path . ' -r ' . g:slimvim_server . ' -c '
	"let g:slimvim_client = ':!' . g:slimvim_python . ' "' . g:slimvim_path . '" -c '
"	let g:slimvim_client = g:slimvim_python . ' "' . g:slimvim_path . '" -c '
	let g:slimvim_client = g:slimvim_python . ' "' . g:slimvim_path . '" -l ' . g:slimvim_lisp . ' -c '
    else
	let g:slimvim_client = g:slimvim_python . ' ' . g:slimvim_path . ' -l ' . g:slimvim_lisp . ' -c '
    endif
endif


"let g:term = 'console -r \"/k %p \\"%s\\" -l %l -s\"'
"let g:term1 = substitute( g:term,  '%p', g:slimvim_python, 'g' )
"let g:term2 = substitute( g:term1, '%s', g:slimvim_path, 'g' )
"let g:term3 = substitute( g:term2, '%l', g:slimvim_lisp, 'g' )
"let g:client = '%p %s -r \"console -r \\"/k %p \\\"%s\\\" -l %l -s\\"\" -c'
"let g:client = '%p %s -r \"%p \\"%s\\" -l %l -s\" -c'


" ---------------------------------------------------------------------

if !exists("g:slimvim_template_pprint")
    let g:slimvim_template_pprint = '(dolist (o %1)(pprint o))'
endif

if !exists("g:slimvim_template_undefine")
    let g:slimvim_template_undefine = '(fmakunbound (read-from-string "%1"))'
endif

if !exists("g:slimvim_template_describe")
    let g:slimvim_template_describe = '(describe (read-from-string "%1"))'
endif

if !exists("g:slimvim_template_trace")
    let g:slimvim_template_trace = "(trace %1)"
endif

if !exists("g:slimvim_template_untrace")
    let g:slimvim_template_untrace = "(untrace %1)"
endif

if !exists("g:slimvim_template_profile")
    "TODO: support different Lisp implementations
    let g:slimvim_template_profile = "(mon:monitor %1)"
endif

if !exists("g:slimvim_template_unprofile")
    "TODO: support different Lisp implementations
    let g:slimvim_template_unprofile = "(mon:unmonitor %1)"
endif

if !exists("g:slimvim_template_disassemble")
    let g:slimvim_template_disassemble = "(disassemble #'%1)"
endif

if !exists("g:slimvim_template_apropos")
    let g:slimvim_template_apropos = '(apropos "%1")'
endif

if !exists("g:slimvim_template_macroexpand")
    let g:slimvim_template_macroexpand = '(pprint %1)'
endif

if !exists("g:slimvim_template_macroexpand_all")
    let g:slimvim_template_macroexpand_all = '(pprint %1)'
endif

if !exists("g:slimvim_template_compile_file")
    let g:slimvim_template_compile_file = '(compile-file "%1")'
endif

if !exists("mapleader")
    let mapleader = ','
endif

"vim.command( 'let user_input = input( "Enter something" )' )
"user_input = vim.eval( "user_input" )

" =====================================================================
"  General utility functions
" =====================================================================

function! SlimvimServerRunning()
    "TODO: make this work on Linux
    let netstat = system( 'netstat -a' )
    "let netstat = execute '!netstat -a'
    if match( netstat, printf( '%d', g:slimvim_port ) ) >= 0
	return 1
    else
	return 0
endfunction

function! SlimvimConnectServer()
    "TODO: make this work on Linux
    "TODO: handle if called again after server already started
    "silent execute ":!start " . g:slimvim_server
    silent execute g:slimvim_server
    " Wait for server + Lisp startup
    sleep 1
endfunction

" Load Python library and necessary modules
function! SlimvimLoad()
""echo 'console -r "/k %p \"%s\" -l %l -s"'
    if g:slimvim_loaded_python == 0
        "py import vim
        "py import sys
        "py import os
        let g:slimvim_loaded_python = 1
"	call SlimvimConnectServer()
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

function SlimvimMakeArgs(args)
    "echo a:args
    let a = '"' . join(a:args, '" "') . '" '
    let a = substitute(a, '\n', '\\n', 'g')
    "let a = ''
    ""let a = a . '"' . substitute(a:args[0], '\n', '\\n" "', 'g') . '" '
    "let a = a . '"' . substitute(a:args[0], '\n', '\\n', 'g') . '" '
    "echo a
    return a
endfunction

" Send argument to Lisp server for evaluation
function! SlimvimEval(args)
    "TODO: overcome command line argument length limitations
    "TODO: eliminate the use of python in VIM
    "TODO: in visual mode and not called from EvalRegion do not call this in a
    "      loop for all lines in the selection
    call SlimvimLoad()
" start client with server command given
"    py sys.argv = [vim.eval("g:slimvim_path"),
"                  \ '-r', vim.eval("g:slimvim_server"), '-c'] + 
"                  \ vim.eval("a:args")
    "call SlimvimMakeArgs(a:args)
    "py sys.argv = [vim.eval("g:slimvim_path"), '-c'] + vim.eval("a:args")
    "execute ":pyfile " . g:slimvim_path
"    silent execute '!' . g:slimvim_python . ' "' . g:slimvim_path . '" -c "(+ 1 2)"'
"    echo '!' . g:slimvim_python . ' "' . g:slimvim_path . '" -c ' . SlimvimMakeArgs(a:args)

    "silent execute '!' . g:slimvim_python . ' "' . g:slimvim_path . '" -c ' . SlimvimMakeArgs(a:args)
"    silent execute g:slimvim_client . SlimvimMakeArgs(a:args)
    "TODO: why does the followign give an E371: Command not found error on Windows?
    "silent execute ':!start /WAIT /B ' . g:slimvim_python . ' "' . g:slimvim_path . '" -c ' . SlimvimMakeArgs(a:args)
    "silent execute '!cmd /c /q ' . g:slimvim_python . ' "' . g:slimvim_path . '" -c ' . SlimvimMakeArgs(a:args)
    "execute '!' . g:slimvim_python . ' "' . g:slimvim_path . '" -c ' . SlimvimMakeArgs(a:args)

    "let result = system( g:slimvim_client . SlimvimMakeArgs(a:args) )
    "let result = system( g:slimvim_python . ' "' . g:slimvim_path . '" -c ' . SlimvimMakeArgs(a:args) )
"    echo g:slimvim_client . SlimvimMakeArgs(a:args)
    let result = system( g:slimvim_client . SlimvimMakeArgs(a:args) )
"    execute '!' . g:slimvim_client . SlimvimMakeArgs(a:args)
    "echo result
endfunction

" Eval buffer lines in the given range
function! SlimvimEvalRegion() range
    "TODO: handle continuous (not whole line) selection case
    "TODO: getline has only one argument in VIM 6.x
    if mode() == "v" || mode() == "V"
        let lines = getline(a:firstline, a:lastline)
    else
        let lines = getline("'<", "'>")
    endif
    call SlimvimEval(lines)
endfunction

" Eval contents of the 's' register
function! SlimvimEvalSelection()
    "TODO: VIM 6.x does not have lists. What to do?
    let lines = [SlimvimGetSelection()]
    "let lines = []
    "call add(lines, SlimvimGetSelection())
    call SlimvimEval(lines)
endfunction

" Eval Lisp form.
" Form given in the template is passed to Lisp without modification.
function! SlimvimEvalForm(template)
    let lines = [a:template]
    call SlimvimEval(lines)
endfunction

" Eval Lisp form, with the given parameter substituted in the template.
" %1 string is substituted with par1
function! SlimvimEvalForm1(template, par1)
    "let p1 = substitute(a:par1, '&', '\\&', "g")  " & -> \&
    let p1 = escape(a:par1, '&')
    let temp1 = substitute(a:template, '%1', p1, "g")
    let lines = [temp1]
    call SlimvimEval(lines)
endfunction

" Eval Lisp form, with the given parameters substituted in the template.
" %1 string is substituted with par1
" %2 string is substituted with par2
function! SlimvimEvalForm2(template, par1, par2)
    "let p1 = substitute(a:par1, '&', '\\&', "g")  " & -> \&
    "let p2 = substitute(a:par2, '&', '\\&', "g")  " & -> \&
    let p1 = escape(a:par1, '&')
    let p2 = escape(a:par2, '&')
    let temp1 = substitute(a:template, '%1', p1, "g")
    let temp2 = substitute(temp1,      '%2', p2, "g")
    let lines = [temp2]
    call SlimvimEval(lines)
endfunction

" =====================================================================
"  Special functions
" =====================================================================

function! SlimvimEvalDefun()
    call SlimvimSelectToplevelForm()
    call SlimvimEvalSelection()
endfunction

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

function! SlimvimInteractiveEval()
    let e = input( "Eval: " )
    if e
        call SlimvimEval([e])
    endif
endfunction

function! SlimvimUndefineFunction()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_undefine, SlimvimGetSelection())
endfunction

" ---------------------------------------------------------------------

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

function! SlimvimTrace()
    let s = input( "Trace: " )
    if s
        call SlimvimEvalForm1(g:slimvim_template_trace, s)
    endif
endfunction

function! SlimvimTraceSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_trace, SlimvimGetSelection())
endfunction

function! SlimvimUntrace()
    let s = input( "Untrace: " )
    if s
        call SlimvimEvalForm1(g:slimvim_template_untrace, s)
    endif
endfunction

function! SlimvimUntraceSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_untrace, SlimvimGetSelection())
endfunction

function! SlimvimDisassemble()
    let s = input( "Disassemble: " )
    if s
        call SlimvimEvalForm1(g:slimvim_template_disassemble, s)
    endif
endfunction

function! SlimvimDisassembleSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_disassemble, SlimvimGetSelection())
endfunction

function! SlimvimProfile()
    let s = input( "Profile: " )
    if s
        call SlimvimEvalForm1(g:slimvim_template_profile, s)
    endif
endfunction

function! SlimvimProfileSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_profile, SlimvimGetSelection())
endfunction

function! SlimvimUnProfile()
    let s = input( "Unprofile: " )
    if s
        call SlimvimEvalForm1(g:slimvim_template_unprofile, s)
    endif
endfunction

function! SlimvimUnprofileSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_unprofile, SlimvimGetSelection())
endfunction

" ---------------------------------------------------------------------

" compile-string
"      (funcall (compile nil (read-from-string
"                             (format nil "(~S () ~A)" 'lambda string)


function! SlimvimCompileFile()
    call SlimvimEvalForm1(g:slimvim_template_compile_file, fnamemodify(bufname(""), ":p"))
endfunction

function! SlimvimDescribeSymbol()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_describe, SlimvimGetSelection())
endfunction

" ---------------------------------------------------------------------

function! SlimvimApropos()
    call SlimvimSelectSymbol()
    call SlimvimEvalForm1(g:slimvim_template_apropos, SlimvimGetSelection())
endfunction

" =====================================================================
"  Slimvim keybindings
" =====================================================================

" <Leader> can be set in .vimrc, it defaults here to ','
" <Leader> timeouts in 1000 msec by default, if this is too short,
" then increase 'timeoutlen'
map <Leader>c  :call SlimvimConnectServer()<CR>

map <Leader>d  :call SlimvimEvalDefun()<CR>
map <Leader>e  :call SlimvimEvalLastExp()<CR>
map <Leader>E  :call SlimvimPprintEvalLastExp()<CR>
map <Leader>r  :call SlimvimEvalRegion()<CR>
map <Leader>b  :call SlimvimEvalBuffer()<CR>
map <Leader>i  :call SlimvimInteractiveEval()<CR>
map <Leader>u  :call SlimvimUndefineFunction()<CR>

map <Leader>1  :call SlimvimMacroexpand()<CR>
map <Leader>m  :call SlimvimMacroexpandAll()<CR>
map <Leader>t  :call SlimvimTrace()<CR>
map <Leader>gt :call SlimvimTraceSymbol()<CR>
map <Leader>T  :call SlimvimUntrace()<CR>
map <Leader>gT :call SlimvimUntraceSymbol()<CR>
map <Leader>l  :call SlimvimDisassemble()<CR>

map <Leader>f  :call SlimvimCompileFile()<CR>

map <Leader>p  :call SlimvimProfile()<CR>
map <Leader>gp :call SlimvimProfileSymbol()<CR>
map <Leader>P  :call SlimvimUnprofile()<CR>
map <Leader>gP :call SlimvimUnprofileSymbol()<CR>

map <Leader>s  :call SlimvimDescribeSymbol()<CR>
map <Leader>a  :call SlimvimApropos()<CR>

" =====================================================================
"  Slimvim menu
" =====================================================================

" Works only if 'wildcharm' is <Tab>
":map <Leader>, :emenu Slimvim.<Tab>
if &wildcharm != 0
    execute ":map <Leader>, :emenu Slimvim." . nr2char(&wildcharm)
endif

menu &Slimvim.&Evaluation.Eval-&Defun              :call SlimvimEvalDefun()<CR>
menu &Slimvim.&Evaluation.Eval-Last-&Exp           :call SlimvimEvalLastExp()<CR>
menu &Slimvim.&Evaluation.&Pprint-Eval-Last        :call SlimvimPprintEvalLastExp()<CR>
menu &Slimvim.&Evaluation.Eval-&Region             :call SlimvimEvalRegion()<CR>
menu &Slimvim.&Evaluation.Eval-&Buffer             :call SlimvimEvalBuffer()<CR>
menu &Slimvim.&Evaluation.&Interactive-Eval\.\.\.  :call SlimvimInteractiveEval()<CR>
menu &Slimvim.&Evaluation.&Undefine-Function       :call SlimvimUndefineFunction()<CR>

menu &Slimvim.De&bugging.Macroexpand-&1            :call SlimvimMacroexpand()<CR>
menu &Slimvim.De&bugging.&Macroexpand-All          :call SlimvimMacroexpandAll()<CR>
menu &Slimvim.De&bugging.&Trace\.\.\.              :call SlimvimTrace()<CR>
menu &Slimvim.De&bugging.U&ntrace\.\.\.            :call SlimvimUntrace()<CR>
menu &Slimvim.De&bugging.Disassemb&le\.\.\.        :call SlimvimDisassemble()<CR>

menu &Slimvim.&Compilation.Compile-&File           :call SlimvimCompileFile()<CR>

menu &Slimvim.&Profiling.&Profile\.\.\.            :call SlimvimProfile()<CR>
menu &Slimvim.&Profiling.&Unprofile\.\.\.          :call SlimvimUnprofile()<CR>

menu &Slimvim.&Documentation.Describe-&Symbol      :call SlimvimDescribeSymbol()<CR>
menu &Slimvim.&Documentation.&Apropos              :call SlimvimApropos()<CR>

