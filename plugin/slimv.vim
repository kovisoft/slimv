" slimv.vim:    The Superior Lisp Interaction Mode for VIM
" Version:      0.1.2
" Last Change:  11 Feb 2009
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if &cp || exists( 'g:slimv_loaded' )
    finish
endif

let g:slimv_loaded = 1

if has( 'win32' ) || has( 'win95' ) || has( 'win64' ) || has( 'win16' )
    let g:slimv_windows = 1
else
    " This means Linux only at the moment
    let g:slimv_windows = 0
endif


" =====================================================================
"  Functions used by global variable definitions
" =====================================================================

" Write debug message to logile (message must be a list)
function! SlimvWriteLog( level, message )
    if exists( 'g:slimv_debug' ) && exists( 'g:slimv_logfile' ) && g:slimv_debug >= a:level
        " We need to make a hack: write things into a temporary file
        " then append temp file contents to the logfile
        let tmp = tempname()
        try
            call writefile( a:message, tmp )
        finally
            if g:slimv_windows
                silent execute '!type ' . tmp . ' >> ' . g:slimv_logfile
            else
                silent execute '!cat ' . tmp . ' >> ' . g:slimv_logfile
            endif
            call delete(tmp)
        endtry
        " Unfortunately I know no way to tell writefile to append the text
        "call writefile( a:message, g:slimv_logfile )
    endif
endfunction

" Write debug message to logile with a timestamp
function! SlimvLog( level, message )
    if exists( '*strftime' )
        let time = strftime( '%Y %b %d %X' )
    else
        let time = localtime()
    endif
    call SlimvWriteLog( a:level, ['***** ' . time] + a:message + [''] )
endfunction

" Try to autodetect Python executable
function! SlimvAutodetectPython()
    if executable( 'python' )
        return 'python'
    endif

    if g:slimv_windows
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

" Try to autodetect Lisp executable
function! SlimvAutodetectLisp()
    " Check the easy cases
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

    if g:slimv_windows
        " Try to find Python on the standard installation places
        let lisps = split( globpath( 'c:/*lisp*,c:/Program Files/*lisp*', '*lisp.exe' ), '\n' )
        if len( lisps ) > 0
            return lisps[0]
        endif
        let lisps = split( globpath( 'c:/*lisp*/*,c:/Program Files/*lisp*/*', '*lisp.exe' ), '\n' )
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
        return ''
    else
        return ''
    endif
endfunction

" Build the command to start the client
function! SlimvClientCommand()
    if g:slimv_python == '' || g:slimv_lisp == ''
        " We don't have enough information to build client command
        return ''
    endif
    if g:slimv_port == 5151
        let port = ''
    else
        " Using port number other than default, must pass it to client
        let port = ' -p ' . g:slimv_port
    endif
    if g:slimv_windows
        return g:slimv_python . ' "' . g:slimv_path . '"' . port  . ' -l ' . g:slimv_lisp
        " This one can be used to start Lisp in a 'Console' window
        " instead of the default DOS box
        "return g:slimv_python . ' "' . g:slimv_path . '"' . port . ' -r ' .
        "       \ '"console -w Slimv -r \"/k @p @s -l ' . g:slimv_lisp . ' -s\""'
    else
        return g:slimv_python . ' ' . g:slimv_path . port . ' -l ' . g:slimv_lisp
    endif
endfunction

" Find slimv.py in the Vim plugin directory (if not given in vimrc)
if !exists( 'g:slimv_path' )
    let plugins = split( globpath( &runtimepath, 'plugin/**/slimv.py'), '\n' )
    if len( plugins ) > 0
        let g:slimv_path = plugins[0]
    else
        let g:slimv_path = 'slimv.py'
    endif
endif

" Log global variables to logfile (if debug log set)
function! SlimvLogGlobals()
    let info = [ 'Loaded file: ' . fnamemodify( bufname(''), ':p' ) ]
    call add( info,  printf( 'g:slimv_debug = %d',   g:slimv_debug ) )
    call add( info,  printf( 'g:slimv_logfile = %s', g:slimv_logfile ) )
    call add( info,  printf( 'g:slimv_port = %d',    g:slimv_port ) )
    call add( info,  printf( 'g:slimv_python = %s',  g:slimv_python ) )
    call add( info,  printf( 'g:slimv_lisp = %s',    g:slimv_lisp ) )
    call add( info,  printf( 'g:slimv_client = %s',  g:slimv_client ) )
    call SlimvLog( g:slimv_debug, info )
endfunction

au BufNewFile,BufRead *.lisp call SlimvLogGlobals()


" =====================================================================
"  Global variable definitions
" =====================================================================

" Debug level (0 = no debug messages)
if !exists( 'g:slimv_debug' )
    let g:slimv_debug = 0
endif

" Leave client window open for debugging purposes
" (works only on Windows at the moment)
if !exists( 'g:slimv_debug_client' )
    let g:slimv_debug_client = 0
endif

" Logfile name for debug messages
if !exists( 'g:slimv_logfile' )
    let g:slimv_logfile = 'slimv.log'
endif

" TCP port number to use
if !exists( 'g:slimv_port' )
    let g:slimv_port = 5151
endif

" Find Python (if not given in vimrc)
if !exists( 'g:slimv_python' )
    let g:slimv_python = SlimvAutodetectPython()
endif

" Find Lisp (if not given in vimrc)
if !exists( 'g:slimv_lisp' )
    let g:slimv_lisp = SlimvAutodetectLisp()
endif

" Build client command (if not given in vimrc)
if !exists( 'g:slimv_client' )
    let g:slimv_client = SlimvClientCommand()
endif

" Slimv keybinding set (0 = no keybindings)
if !exists( 'g:slimv_keybindings' )
    let g:slimv_keybindings = 1
endif

" Append Slimv menu to the global menu (0 = no menu)
if !exists( 'g:slimv_menu' )
    let g:slimv_menu = 1
endif

" Name of the REPL buffer inside Vim
if !exists( 'g:slimv_bufname' )
    let g:slimv_bufname = 'Slimv.REPL'
endif


" =====================================================================
"  Template definitions
" =====================================================================

if !exists( 'g:slimv_template_pprint' )
    let g:slimv_template_pprint = '(dolist (o %1)(pprint o))'
endif

if !exists( 'g:slimv_template_undefine' )
    let g:slimv_template_undefine = '(fmakunbound (read-from-string "%1"))'
endif

if !exists( 'g:slimv_template_describe' )
    let g:slimv_template_describe = '(describe (read-from-string "%1"))'
endif

if !exists( 'g:slimv_template_trace' )
    let g:slimv_template_trace = '(trace %1)'
endif

if !exists( 'g:slimv_template_untrace' )
    let g:slimv_template_untrace = '(untrace %1)'
endif

if !exists( 'g:slimv_template_profile' )
    "TODO: support different Lisp implementations
    let g:slimv_template_profile = '(mon:monitor %1)'
endif

if !exists( 'g:slimv_template_unprofile' )
    "TODO: support different Lisp implementations
    let g:slimv_template_unprofile = '(mon:unmonitor %1)'
endif

if !exists( 'g:slimv_template_disassemble' )
    let g:slimv_template_disassemble = "(disassemble #'%1)"
endif

if !exists( 'g:slimv_template_inspect' )
    let g:slimv_template_inspect = '(inspect %1)'
endif

if !exists( 'g:slimv_template_apropos' )
    let g:slimv_template_apropos = '(apropos "%1")'
endif

if !exists( 'g:slimv_template_macroexpand' )
    let g:slimv_template_macroexpand = '(pprint %1)'
endif

if !exists( 'g:slimv_template_macroexpand_all' )
    let g:slimv_template_macroexpand_all = '(pprint %1)'
endif

if !exists( 'g:slimv_template_compile_file' )
"    let g:slimv_template_compile_file = '(compile-file "%1")'
    let g:slimv_template_compile_file =
    \ '(let ((fasl-file (compile-file "%1")))' .
    \ '  (when (and %2 fasl-file) (load fasl-file)))'
endif

if !exists( 'g:slimv_template_compile_string' )
    let g:slimv_template_compile_string = 
    \ '(funcall (compile nil (read-from-string (format nil "(~S () ~A)" ' . "'" . 'lambda "%1"))))'
endif

if !exists( 'mapleader' )
    let mapleader = ','
endif


" =====================================================================
"  General utility functions
" =====================================================================

" Open a new REPL buffer or switch to the existing one
function! SlimvOpenReplBuffer()
    "TODO: check if this works without 'set hidden'
    "TODO: add option for split window
    let repl_buf = bufnr( g:slimv_bufname )
    if repl_buf == -1
        " Create a new REPL buffer
        exe "edit " . g:slimv_bufname
    else
        " REPL buffer is already created. Check if it is open in a window
        let repl_win = bufwinnr( repl_buf )
        if repl_win != -1
            " Switch to the REPL window
            if winnr() != repl_win
                exe repl_win . "wincmd w"
            endif
        else
            " Switch to the REPL buffer
            exe "buffer " . repl_buf
        endif
    endif
    
    " This buffer will not have an associated file
    set buftype=nofile
endfunction

" Select symbol under cursor and copy it to register 's'
function! SlimvSelectSymbol()
    "TODO: can we use expand('<cWORD>') here?
    normal viw"sy
endfunction

" Select bottom level form the cursor is inside and copy it to register 's'
function! SlimvSelectForm()
    normal va(o
    " Handle '() or #'() etc. type special syntax forms
    " TODO: what to do with ` operator?
    let c = col( '.' ) - 2
    while c > 0 && match( ' \t()', getline( '.' )[c] ) < 0
        normal h
        let c = c - 1
    endwhile
    normal "sy
endfunction

" Select top level form the cursor is inside and copy it to register 's'
function! SlimvSelectToplevelForm()
    normal 99[(
    call SlimvSelectForm()
endfunction

" Return the contents of register 's'
function! SlimvGetSelection()
    return getreg( '"s' )
endfunction

" Prepare argument list to be sent to the client
function SlimvMakeArgs( args )
    let ar = a:args
    let i = 0
    while i < len(ar)
        let ar[i] = substitute( ar[i], '"',  '\\"', 'g' )
        let i = i + 1
    endwhile
    let a = join( ar, '" "' )
    "let a = substitute( a, '"',  '\\"', 'g' )
    let a = substitute( a, '\n', '\\n', 'g' )
    let a = '"' . a . '" '
    return a
endfunction

" Send text to the client
function! SlimvSendToClient( args )
    if g:slimv_debug_client == 0
        let result = system( g:slimv_client . ' -c ' . SlimvMakeArgs(a:args) )
    else
        execute '!' . g:slimv_client . ' -c ' . SlimvMakeArgs(a:args)
    endif
endfunction

" Send argument to Lisp server for evaluation
function! SlimvEval( args )
    if g:slimv_client == ''
        " No command to start client, we are clueless, ask user for assistance
        if g:slimv_python == ''
            let g:slimv_python = input( 'Enter Python path (or fill g:slimv_python in your vimrc): ', '', 'file' )
        endif
        if g:slimv_lisp == ''
            let g:slimv_lisp = input( 'Enter Lisp path (or fill g:slimv_lisp in your vimrc): ', '', 'file' )
        endif
        let g:slimv_client = SlimvClientCommand()
    endif

    if g:slimv_client == ''
        return
    endif

    " Hardcoded to use temporary file for passing text to the client
    let use_temp_file = 1
    if use_temp_file
        let tmp = tempname()
        try
            let ar = []
            let i = 0
            while i < len( a:args )
                call extend( ar, split( a:args[i], '\n' ) )
                let i = i + 1
            endwhile
            call SlimvLog( 1, a:args )
            call writefile( ar, tmp )
            if g:slimv_debug_client == 0
                let result = system( g:slimv_client . ' -f ' . tmp )
            else
                execute '!' . g:slimv_client . ' -f ' . tmp
            endif
        finally
            call delete(tmp)
        endtry
    else
        " Send text to the client via command line arguments
        " This is problematic due to command line argument size limitations
        " So currently it is not used
        let total = 0
        let i = 0
        let j = 0
        while j < len( a:args )
            let l = len( a:args[j] )
            if l >= 1000
                " Check the length of each line
                echo 'Line #' . j . ' too long'
                break
            endif
            if total + l < 1000
                " Limit also total length to be passed to the client
                " in command line args
                let total = total + l
            else
                " Total length would be too large, pass lines collected previously
                " and start over collecting lines
                call SlimvSendToClient( a:args[i : j-1] )
                let i = j
                let total = 0
            endif
            let j = j + 1
        endwhile
        if i < j
            " There are some lines left unsent, send them now
            call SlimvSendToClient( a:args[i : j-1] )
        endif
    endif
endfunction

" Start and connect slimv server
" This is a quite dummy function that just evaluates a comment
function! SlimvConnectServer()
    call SlimvEval([';;; Slimv client connected successfully'])
endfunction

" Get the last region (visual block)
function! SlimvGetRegion() range
    if mode() == 'v' || mode() == 'V'
        let lines = getline( a:firstline, a:lastline )
        let firstcol = col( a:firstline ) - 1
        let lastcol  = col( a:lastline  ) - 2
    else
        let lines = getline( "'<", "'>" )
        let firstcol = col( "'<" ) - 1
        let lastcol  = col( "'>" ) - 2
    endif
    if lastcol >= 0
        let lines[len(lines)-1] = lines[len(lines)-1][ : lastcol]
    else
        let lines[len(lines)-1] = ''
    endif
    let lines[0] = lines[0][firstcol : ]
    return lines
endfunction

" Eval buffer lines in the given range
function! SlimvEvalRegion() range
    let lines = SlimvGetRegion()
    call SlimvEval( lines )
endfunction

" Eval contents of the 's' register
function! SlimvEvalSelection()
    let lines = [SlimvGetSelection()]
    call SlimvEval( lines )
endfunction

" Eval Lisp form.
" Form given in the template is passed to Lisp without modification.
function! SlimvEvalForm( template )
    let lines = [a:template]
    call SlimvEval( lines )
endfunction

" Eval Lisp form, with the given parameter substituted in the template.
" %1 string is substituted with par1
function! SlimvEvalForm1( template, par1 )
    let p1 = escape( a:par1, '&' )
    let p1 = escape( p1, '\\' )
    let temp1 = substitute( a:template, '%1', p1, 'g' )
    let lines = [temp1]
    call SlimvEval( lines )
endfunction

" Eval Lisp form, with the given parameters substituted in the template.
" %1 string is substituted with par1
" %2 string is substituted with par2
function! SlimvEvalForm2( template, par1, par2 )
    let p1 = escape( a:par1, '&' )
    let p2 = escape( a:par2, '&' )
    let p1 = escape( p1, '\\' )
    let p2 = escape( p2, '\\' )
    let temp1 = substitute( a:template, '%1', p1, 'g' )
    let temp2 = substitute( temp1,      '%2', p2, 'g' )
    let lines = [temp2]
    call SlimvEval( lines )
endfunction


" =====================================================================
"  Special functions
" =====================================================================

" Evaluate top level form at the cursor pos
function! SlimvEvalDefun()
    call SlimvSelectToplevelForm()
    call SlimvEvalSelection()
endfunction

" Evaluate the whole buffer
function! SlimvEvalBuffer()
    let lines = getline( 1, '$' )
    call SlimvEval( lines )
endfunction

" Evaluate last expression
function! SlimvEvalLastExp()
    call SlimvSelectForm()
    call SlimvEvalSelection()
endfunction

" Evaluate and pretty print last expression
function! SlimvPprintEvalLastExp()
    call SlimvSelectForm()
    call SlimvEvalForm1( g:slimv_template_pprint, SlimvGetSelection() )
endfunction

" Evaluate expression entered interactively
function! SlimvInteractiveEval()
    let e = input( 'Eval: ' )
    if e != ''
        call SlimvEval([e])
    endif
endfunction

" Undefine function
function! SlimvUndefineFunction()
    call SlimvSelectSymbol()
    call SlimvEvalForm1( g:slimv_template_undefine, SlimvGetSelection() )
endfunction

" ---------------------------------------------------------------------

" Macroexpand-1 the current top level form
function! SlimvMacroexpand()
    normal 99[(vt(%"sy
    let m = SlimvGetSelection() . '))'
    let m = substitute( m, "defmacro\\s*", "macroexpand-1 '(", 'g' )
    call SlimvEvalForm1( g:slimv_template_macroexpand, m )
endfunction

" Macroexpand the current top level form
function! SlimvMacroexpandAll()
    normal 99[(vt(%"sy
    let m = SlimvGetSelection() . '))'
    let m = substitute( m, "defmacro\\s*", "macroexpand '(", 'g' )
    call SlimvEvalForm1( g:slimv_template_macroexpand_all, m )
endfunction

" Switch trace on for the selected function
function! SlimvTrace()
    call SlimvSelectSymbol()
    let s = input( 'Trace: ', SlimvGetSelection() )
    echo s
    if s != ''
        call SlimvEvalForm1( g:slimv_template_trace, s )
    endif
endfunction

" Switch trace off for the selected function
function! SlimvUntrace()
    call SlimvSelectSymbol()
    let s = input( 'Untrace: ', SlimvGetSelection() )
    if s != ''
        call SlimvEvalForm1( g:slimv_template_untrace, s )
    endif
endfunction

" Disassemble the selected function
function! SlimvDisassemble()
    call SlimvSelectSymbol()
    let s = input( 'Disassemble: ', SlimvGetSelection() )
    if s != ''
        call SlimvEvalForm1( g:slimv_template_disassemble, s )
    endif
endfunction

" Inspect symbol
function! SlimvInspect()
    call SlimvSelectSymbol()
    let s = input( 'Inspect: ', SlimvGetSelection() )
    if s != ''
        call SlimvEvalForm1( g:slimv_template_inspect, s )
    endif
endfunction

" Switch profiling on for the selected function
function! SlimvProfile()
    call SlimvSelectSymbol()
    let s = input( 'Profile: ', SlimvGetSelection() )
    if s != ''
        call SlimvEvalForm1( g:slimv_template_profile, s )
    endif
endfunction

" Switch profiling off for the selected function
function! SlimvUnProfile()
    call SlimvSelectSymbol()
    let s = input( 'Unprofile: ', SlimvGetSelection() )
    if s != ''
        call SlimvEvalForm1( g:slimv_template_unprofile, s )
    endif
endfunction

" ---------------------------------------------------------------------

" Compile the current top-level form
function! SlimvCompileDefun()
    "TODO: handle double quote characters in form
    call SlimvSelectToplevelForm()
    call SlimvEvalForm1( g:slimv_template_compile_string, SlimvGetSelection() )
endfunction

" Compile and load whole file
function! SlimvCompileLoadFile()
    let filename = fnamemodify( bufname(''), ':p' )
    let filename = escape( filename, '\\' )
    call SlimvEvalForm2( g:slimv_template_compile_file, filename, 'T' )
endfunction

" Compile whole file
function! SlimvCompileFile()
    let filename = fnamemodify( bufname(''), ':p' )
    let filename = escape( filename, '\\' )
    call SlimvEvalForm2( g:slimv_template_compile_file, filename, 'NIL' )
endfunction

function! SlimvCompileRegion() range
    "TODO: handle double quote characters in form
    let lines = SlimvGetRegion()
    let region = join( lines, ' ' )
    call SlimvEvalForm1( g:slimv_template_compile_string, region )
endfunction

" Describe the selected symbol
function! SlimvDescribeSymbol()
    call SlimvSelectSymbol()
    call SlimvEvalForm1( g:slimv_template_describe, SlimvGetSelection() )
endfunction

" ---------------------------------------------------------------------

" Apropos of the selected symbol
function! SlimvApropos()
    call SlimvSelectSymbol()
    call SlimvEvalForm1( g:slimv_template_apropos, SlimvGetSelection() )
endfunction

" =====================================================================
"  Slimv keybindings
" =====================================================================

" <Leader> can be set in .vimrc, it defaults here to ','
" <Leader> timeouts in 1000 msec by default, if this is too short,
" then increase 'timeoutlen'

if g:slimv_keybindings == 1
    " Short (one-key) keybinding set

    noremap <Leader>S  :call SlimvConnectServer()<CR>
    
    noremap <Leader>d  :<C-U>call SlimvEvalDefun()<CR>
    noremap <Leader>e  :<C-U>call SlimvEvalLastExp()<CR>
    noremap <Leader>E  :<C-U>call SlimvPprintEvalLastExp()<CR>
    noremap <Leader>r  :call SlimvEvalRegion()<CR>
    noremap <Leader>b  :<C-U>call SlimvEvalBuffer()<CR>
    noremap <Leader>v  :call SlimvInteractiveEval()<CR>
    noremap <Leader>u  :call SlimvUndefineFunction()<CR>
    
    noremap <Leader>1  :<C-U>call SlimvMacroexpand()<CR>
    noremap <Leader>m  :<C-U>call SlimvMacroexpandAll()<CR>
    noremap <Leader>t  :call SlimvTrace()<CR>
    noremap <Leader>T  :call SlimvUntrace()<CR>
    noremap <Leader>l  :call SlimvDisassemble()<CR>
    noremap <Leader>i  :call SlimvInspect()<CR>
    
    noremap <Leader>D  :<C-U>call SlimvCompileDefun()<CR>
    noremap <Leader>L  :<C-U>call SlimvCompileLoadFile()<CR>
    noremap <Leader>F  :<C-U>call SlimvCompileFile()<CR>
    noremap <Leader>R  :call SlimvCompileRegion()<CR>
    
    noremap <Leader>p  :call SlimvProfile()<CR>
    noremap <Leader>P  :call SlimvUnprofile()<CR>
    
    noremap <Leader>s  :call SlimvDescribeSymbol()<CR>
    noremap <Leader>a  :call SlimvApropos()<CR>

elseif g:slimv_keybindings == 2
    " Easy to remember (two-key) keybinding set

    " Connection commands
    noremap <Leader>cs  :call SlimvConnectServer()<CR>
    
    " Evaluation commands
    noremap <Leader>ed  :<C-U>call SlimvEvalDefun()<CR>
    noremap <Leader>ee  :<C-U>call SlimvEvalLastExp()<CR>
    noremap <Leader>ep  :<C-U>call SlimvPprintEvalLastExp()<CR>
    noremap <Leader>er  :call SlimvEvalRegion()<CR>
    noremap <Leader>eb  :<C-U>call SlimvEvalBuffer()<CR>
    noremap <Leader>ei  :call SlimvInteractiveEval()<CR>
    noremap <Leader>eu  :call SlimvUndefineFunction()<CR>
    
    " Debug commands
    noremap <Leader>m1  :<C-U>call SlimvMacroexpand()<CR>
    noremap <Leader>ma  :<C-U>call SlimvMacroexpandAll()<CR>
    noremap <Leader>dt  :call SlimvTrace()<CR>
    noremap <Leader>du  :call SlimvUntrace()<CR>
    noremap <Leader>dd  :call SlimvDisassemble()<CR>
    noremap <Leader>di  :call SlimvInspect()<CR>
    
    " Compile commands
    noremap <Leader>cd  :<C-U>call SlimvCompileDefun()<CR>
    noremap <Leader>cl  :<C-U>call SlimvCompileLoadFile()<CR>
    noremap <Leader>cf  :<C-U>call SlimvCompileFile()<CR>
    noremap <Leader>cr  :call SlimvCompileRegion()<CR>
    
    " Profile commands
    noremap <Leader>pp  :call SlimvProfile()<CR>
    noremap <Leader>pu  :call SlimvUnprofile()<CR>
    
    " Documentation commands
    noremap <Leader>ds  :call SlimvDescribeSymbol()<CR>
    noremap <Leader>da  :call SlimvApropos()<CR>

endif

" =====================================================================
"  Slimv menu
" =====================================================================

if g:slimv_menu == 1
    " Works only if 'wildcharm' is <Tab>
    ":map <Leader>, :emenu Slimv.<Tab>
    if &wildcharm == 0
        set wildcharm=<Tab>
    endif
    if &wildcharm != 0
        execute ':map <Leader>, :emenu Slimv.' . nr2char( &wildcharm )
    endif

    menu &Slimv.&Evaluation.Eval-&Defun                :<C-U>call SlimvEvalDefun()<CR>
    menu &Slimv.&Evaluation.Eval-Last-&Exp             :<C-U>call SlimvEvalLastExp()<CR>
    menu &Slimv.&Evaluation.&Pprint-Eval-Last          :<C-U>call SlimvPprintEvalLastExp()<CR>
    menu &Slimv.&Evaluation.Eval-&Region               :call SlimvEvalRegion()<CR>
    menu &Slimv.&Evaluation.Eval-&Buffer               :<C-U>call SlimvEvalBuffer()<CR>
    menu &Slimv.&Evaluation.Interacti&ve-Eval\.\.\.    :call SlimvInteractiveEval()<CR>
    menu &Slimv.&Evaluation.&Undefine-Function         :call SlimvUndefineFunction()<CR>
    
    menu &Slimv.De&bugging.Macroexpand-&1              :<C-U>call SlimvMacroexpand()<CR>
    menu &Slimv.De&bugging.&Macroexpand-All            :<C-U>call SlimvMacroexpandAll()<CR>
    menu &Slimv.De&bugging.&Trace\.\.\.                :call SlimvTrace()<CR>
    menu &Slimv.De&bugging.U&ntrace\.\.\.              :call SlimvUntrace()<CR>
    menu &Slimv.De&bugging.Disassemb&le\.\.\.          :call SlimvDisassemble()<CR>
    menu &Slimv.De&bugging.&Inspect\.\.\.              :call SlimvInspect()<CR>
    
    menu &Slimv.&Compilation.Compile-&Defun            :<C-U>call SlimvCompileDefun()<CR>
    menu &Slimv.&Compilation.Compile-&Load-File        :<C-U>call SlimvCompileLoadFile()<CR>
    menu &Slimv.&Compilation.Compile-&File             :<C-U>call SlimvCompileFile()<CR>
    menu &Slimv.&Compilation.Compile-&Region           :call SlimvCompileRegion()<CR>
    
    menu &Slimv.&Profiling.&Profile\.\.\.              :call SlimvProfile()<CR>
    menu &Slimv.&Profiling.&Unprofile\.\.\.            :call SlimvUnprofile()<CR>
    
    menu &Slimv.&Documentation.Describe-&Symbol        :call SlimvDescribeSymbol()<CR>
    menu &Slimv.&Documentation.&Apropos                :call SlimvApropos()<CR>
endif

