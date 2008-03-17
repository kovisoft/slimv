" slimvim.vim:  The Superior Lisp Interaction Mode for VIM
" Last Change:	2008 Mar 17
" Maintainer:	Tamas Kovacs <kovisoft@gmail.com>
" License:	This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  TODO: make it work on Linux
"  TODO: is it possible to redirect output to VIM buffer?
"  TODO: compile related functions and keybindings
"  TODO: documentation commands
"  TODO: possibility to use cmd frontend (like Console: console "/k <command>")
"  TODO: find slimvim.vim, if not in vimfiles, but still in the VIM runtimepath
"  TODO: autodetect Python and Lisp installation directory
" You should look at (HKEY_LOCAL_MACHINE,HKEY_CURRENT_USER)/Software/Python. 
"  TODO: find slimvim.py in the VIM search path
" globpath(&rtp, "**/slimvim.py")
"  TODO: handle double quotes in forms (or ;; comment) sent to server
"
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

function! SlimvimAutodetectPython()
    if executable( 'python' )
	return 'python'
    endif

    if g:slimvim_windows
	" Try to find Python on the standard installation places
	let pythons = split( globpath( 'c:/python*,c:/Program Files/python*', 'python.exe' ), '\n' )
	if len( pythons ) > 0
	    return pythons[0]
	endif
	" Go deeper in subdirectories
	let pythons = split( globpath( 'c:/python*/**,c:/Program Files/python*/**', 'python.exe' ), '\n' )
	if len( pythons ) > 0
	    return pythons[0]
	endif
	return ''
    else
	return ''
    endif
endfunction

function! SlimvimAutodetectLisp()
    if executable( 'clisp' )
	" Common Lisp
	return 'clisp'
    endif
    if executable( 'gcl' )
	" GNU Common Lisp
	return 'gcl'
    endif
    if executable( 'cmucl' )
	" Carnegie Mellon University Common Lisp
	return 'cmucl'
    endif
    if executable( 'sbcl' )
	" Steel Bank Common Lisp
	return 'sbcl'
    endif
    if executable( 'ecl' )
	" Embeddable Common Lisp
	return 'ecl'
    endif
    if executable( 'acl' )
	" Allegro Common Lisp
	return 'acl'
    endif
    if executable( 'lwl' )
	" LispWorks
	return 'lwl'
    endif

    if g:slimvim_windows
	" Try to find Python on the standard installation places
	let lisps = split( globpath( 'c:/*lisp*,c:/Program Files/*lisp*', '*lisp.exe' ), '\n' )
	if len( lisps ) > 0
	    return lisps[0]
	endif
	let lisps = split( globpath( 'c:/*lisp*/**,c:/Program Files/*lisp*/**', '*lisp.exe' ), '\n' )
	if len( lisps ) > 0
	    return lisps[0]
	endif
	let lisps = split( globpath( 'c:/gcl*,c:/Program Files/gcl*', 'gcl.exe' ), '\n' )
	if len( lisps ) > 0
	    return lisps[0]
	endif
	let lisps = split( globpath( 'c:/cmucl*,c:/Program Files/cmucl*', 'cmucl.exe' ), '\n' )
	if len( lisps ) > 0
	    return lisps[0]
	endif
	let lisps = split( globpath( 'c:/sbcl*,c:/Program Files/sbcl*', 'sbcl.exe' ), '\n' )
	if len( lisps ) > 0
	    return lisps[0]
	endif
	let lisps = split( globpath( 'c:/ecl*,c:/Program Files/ecl*', 'ecl.exe' ), '\n' )
	if len( lisps ) > 0
	    return lisps[0]
	endif
	"return 'clisp.exe'
	"return 'c:/lispbox/clisp-2.37/clisp.exe'
	"return '\"c:/lispbox/clisp-2.37/clisp.exe -ansi\"'
	"TODO: remove this hack
	"return '"c:/lispbox/clisp-2.37/clisp.exe -ansi"'
	return ''
    else
	return ''
    endif
endfunction

function! SlimvimClientCommand()
    if g:slimvim_python == '' || g:slimvim_lisp == ''
	" We don't have enough information to build the command to start the client
	return ''
    endif
    if g:slimvim_port == 5151
	let port = ''
    else
	" Using port number other than default, must pass it to client
	let port = ' -p ' . g:slimvim_port
    endif
    if g:slimvim_windows
	"let g:slimvim_client = g:slimvim_path . ' -r ' . g:slimvim_server . ' -c '
	"let g:slimvim_client = ':!' . g:slimvim_python . ' "' . g:slimvim_path . '" -c '
"	let g:slimvim_client = g:slimvim_python . ' "' . g:slimvim_path . '" -c '

	"return g:slimvim_python . ' "' . g:slimvim_path . '"' . port  . ' -l ' . g:slimvim_lisp . ' -c '
	return g:slimvim_python . ' "' . g:slimvim_path . '"' . port . ' -r ' .
	       \ '"console -w Slimvim -r \"/k @p @s -l @l -s\""' . ' -l ' . g:slimvim_lisp . ' -c '
    else
	return g:slimvim_python . ' ' . g:slimvim_path . port . ' -l ' . g:slimvim_lisp . ' -c '
    endif
endfunction

if !exists('g:slimvim_path')
"    if g:slimvim_windows
"	"let g:slimvim_path = $VIMRUNTIME . "/plugin/slimvim.py"
"	let g:slimvim_path = $VIM . '/vimfiles/plugin/slimvim.py'
"    else
"	let g:slimvim_path = $HOME . '/.vim/plugin/slimvim.py'
"    endif
    "let g:slimvim_path = globpath(&runtimepath, '**/slimvim.py')
    "let g:slimvim_path = globpath(&runtimepath, '*/slimvim.py')
    let plugins = split( globpath( &runtimepath, 'plugin/**/slimvim.py'), '\n' )
    if len( plugins ) > 0
	let g:slimvim_path = plugins[0]
    else
	let g:slimvim_path = 'slimvim.py'
    endif
endif

if !exists('g:slimvim_python')
    let g:slimvim_python = SlimvimAutodetectPython()
endif

if !exists('g:slimvim_lisp')
    let g:slimvim_lisp = SlimvimAutodetectLisp()
endif

"if !exists('g:slimvim_server')
"    if g:slimvim_windows
"	let g:slimvim_command = g:slimvim_python . ' \"' . g:slimvim_path . '\"'
"	"let g:slimvim_server = 'console -r "/k ' . g:slimvim_python . ' \"' . g:slimvim_path . '\" -l ' . g:slimvim_lisp . ' -s"'
"	let g:slimvim_server = ':!start console -r "/k ' . g:slimvim_python . ' \"' . g:slimvim_path . '\" -l ' . g:slimvim_lisp . ' -s"'
"	"let g:slimvim_server = 'console -r "/k c:/python24/python.exe \"c:/Program Files/Vim/vimfiles/plugin/slimvim.py\" -l \"c:/lispbox/clisp-2.37/clisp.exe -ansi\" -s"'
"	"let g:slimvim_server = g:slimvim_python . ' "' . g:slimvim_path . '" -l ' . g:slimvim_lisp . ' -s'
"    else
"	let g:slimvim_server = ':!xterm -e ' . g:slimvim_python . ' ' . g:slimvim_path . ' -l ' . g:slimvim_lisp . ' -s &'
"    endif
"endif

if !exists('g:slimvim_client')
    let g:slimvim_client = SlimvimClientCommand()
    "let g:slimvim_client = ''
endif


"let g:term = 'console -r \"/k %p \\"%s\\" -l %l -s\"'
"let g:term1 = substitute( g:term,  '%p', g:slimvim_python, 'g' )
"let g:term2 = substitute( g:term1, '%s', g:slimvim_path, 'g' )
"let g:term3 = substitute( g:term2, '%l', g:slimvim_lisp, 'g' )
"let g:client = '%p %s -r \"console -r \\"/k %p \\\"%s\\\" -l %l -s\\"\" -c'
"let g:client = '%p %s -r \"%p \\"%s\\" -l %l -s\" -c'


" ---------------------------------------------------------------------

"TODO: change %1 to @1 to be conform with @p, @s, @l above (or just leave it alone?)
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


" =====================================================================
"  General utility functions
" =====================================================================

"function! SlimvimServerRunning()
"    "TODO: make this work on Linux
"    let netstat = system( 'netstat -a' )
"    "let netstat = execute '!netstat -a'
"    if match( netstat, printf( '%d', g:slimvim_port ) ) >= 0
"	return 1
"    else
"	return 0
"endfunction

"function! SlimvimConnectServer()
"    "TODO: make this work on Linux
"    "TODO: handle if called again after server already started
"    "silent execute ":!start " . g:slimvim_server
"    silent execute g:slimvim_server
"    " Wait for server + Lisp startup
"    sleep 1
"endfunction

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
    "TODO: can we use expand('<cWORD>') here?
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
    "TODO: in visual mode and not called from EvalRegion do not call this in a
    "      loop for all lines in the selection
    call SlimvimLoad()

    if g:slimvim_client == ''
	" No command to start client, we are clueless, ask user for assistance
	if g:slimvim_python == ''
	    let g:slimvim_python = input( "Enter Python path (or fill g:slimvim_python in your vimrc): ", "", "file" )
	endif
	if g:slimvim_lisp == ''
	    let g:slimvim_lisp = input( "Enter Lisp path (or fill g:slimvim_lisp in your vimrc): ", "", "file" )
	endif
	let g:slimvim_client = SlimvimClientCommand()
    endif

    if g:slimvim_client != ''
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
    endif
endfunction

function! SlimvimConnectServer()
    call SlimvimEval([])
endfunction

" Eval buffer lines in the given range
function! SlimvimEvalRegion() range
    "TODO: handle continuous (not whole line) selection case
    "TODO: getline has only one argument in VIM 6.x
    if mode() == "v" || mode() == "V"
        let lines = getline(a:firstline, a:lastline)
	let firstcol = col(a:firstline) - 1
	let lastcol  = col(a:lastline ) - 2
    else
        let lines = getline("'<", "'>")
	let firstcol = col("'<") - 1
	let lastcol  = col("'>") - 2
    endif
    if lastcol >= 0
	let lines[len(lines)-1] = lines[len(lines)-1][ : lastcol]
    else
	let lines[len(lines)-1] = ''
    endif
    let lines[0] = lines[0][firstcol : ]
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
    if e != ""
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
    call SlimvimSelectSymbol()
    let s = input( "Trace: ", SlimvimGetSelection() )
    echo s
    if s != ""
        call SlimvimEvalForm1(g:slimvim_template_trace, s)
    endif
endfunction

function! SlimvimUntrace()
    call SlimvimSelectSymbol()
    let s = input( "Untrace: ", SlimvimGetSelection() )
    if s != ""
        call SlimvimEvalForm1(g:slimvim_template_untrace, s)
    endif
endfunction

function! SlimvimDisassemble()
    call SlimvimSelectSymbol()
    let s = input( "Disassemble: ", SlimvimGetSelection() )
    if s != ""
        call SlimvimEvalForm1(g:slimvim_template_disassemble, s)
    endif
endfunction

function! SlimvimProfile()
    call SlimvimSelectSymbol()
    let s = input( "Profile: ", SlimvimGetSelection() )
    if s != ""
        call SlimvimEvalForm1(g:slimvim_template_profile, s)
    endif
endfunction

function! SlimvimUnProfile()
    call SlimvimSelectSymbol()
    let s = input( "Unprofile: ", SlimvimGetSelection() )
    if s != ""
        call SlimvimEvalForm1(g:slimvim_template_unprofile, s)
    endif
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
map <Leader>S  :call SlimvimConnectServer()<CR>

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
map <Leader>T  :call SlimvimUntrace()<CR>
map <Leader>l  :call SlimvimDisassemble()<CR>

map <Leader>f  :call SlimvimCompileFile()<CR>

map <Leader>p  :call SlimvimProfile()<CR>
map <Leader>P  :call SlimvimUnprofile()<CR>

map <Leader>s  :call SlimvimDescribeSymbol()<CR>
map <Leader>a  :call SlimvimApropos()<CR>

" =====================================================================
"  Slimvim menu
" =====================================================================

" Works only if 'wildcharm' is <Tab>
":map <Leader>, :emenu Slimvim.<Tab>
if &wildcharm == 0
    set wildcharm=<Tab>
endif
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

