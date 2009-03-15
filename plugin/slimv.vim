" slimv.vim:    The Superior Lisp Interaction Mode for VIM
" Version:      0.3.0
" Last Change:  15 Mar 2009
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
        " Try to find Lisp on the standard installation places
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
    endif

    if executable( 'clojure.jar' )
        " Clojure
        return '"java -cp clojure.jar clojure.lang.Repl"'
    endif

    if g:slimv_windows
        " Try to find Clojure on the standard installation places
        let lisps = split( globpath( 'c:/*clojure*', 'clojure.jar' ), '\n' )
        if len( lisps ) > 0
            return '"java -cp ' . lisps[0] . ' clojure.lang.Repl"'
        endif
    endif

    return ''
endfunction

" Build the command to start the client
function! SlimvMakeClientCommand()
    if g:slimv_python == '' || g:slimv_lisp == ''
        " We don't have enough information to build client command
        return ''
    endif

    " Start with the Python path
    let cmd = g:slimv_python

    " Add path of Slimv script, on Windows enclose in double quotes
    if g:slimv_windows
        let cmd = cmd . ' "' . g:slimv_path . '"'
    else
        let cmd = cmd . ' ' . g:slimv_path
    endif

    " Add port number if different from default
    if g:slimv_port != 5151
        let cmd = cmd . ' -p ' . g:slimv_port
    endif

    " Add Lisp path
    let cmd = cmd . ' -l ' . g:slimv_lisp

    return cmd
endfunction

function! SlimvClientCommand()
    if g:slimv_client == ''
        " No command to start client, we are clueless, ask user for assistance
        if g:slimv_python == ''
            let g:slimv_python = input( 'Enter Python path (or fill g:slimv_python in your vimrc): ', '', 'file' )
        endif
        if g:slimv_lisp == ''
            let g:slimv_lisp = input( 'Enter Lisp path (or fill g:slimv_lisp in your vimrc): ', '', 'file' )
        endif
        let g:slimv_client = SlimvMakeClientCommand()
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

" Get the filetype (Lisp dialect) used by Slimv
function! SlimvGetFiletype()
    if exists( 'g:slimv_filetype' )
        " Return Slimv filetype if defined
        return g:slimv_filetype
    endif

    if &ft != ''
        " Return Vim filetype if defined
        return &ft
    endif

    " We have no clue, guess its lisp
    return 'lisp'
endfunction

" Log global variables to logfile (if debug log set)
function! SlimvLogGlobals()
    let info = [ 'Loaded file: ' . fnamemodify( bufname(''), ':p' ) ]
    call add( info,  printf( 'g:slimv_debug         = %d',    g:slimv_debug ) )
    call add( info,  printf( 'g:slimv_debug_client  = %d',    g:slimv_debug_client ) )
    call add( info,  printf( 'g:slimv_logfile       = %s',    g:slimv_logfile ) )
    call add( info,  printf( 'g:slimv_port          = %d',    g:slimv_port ) )
    call add( info,  printf( 'g:slimv_python        = %s',    g:slimv_python ) )
    call add( info,  printf( 'g:slimv_lisp          = %s',    g:slimv_lisp ) )
    call add( info,  printf( 'g:slimv_client        = %s',    g:slimv_client ) )
    call add( info,  printf( 'g:slimv_repl_open     = %d',    g:slimv_repl_open ) )
    call add( info,  printf( 'g:slimv_repl_dir      = %s',    g:slimv_repl_dir ) )
    call add( info,  printf( 'g:slimv_repl_file     = %s',    g:slimv_repl_file ) )
    call add( info,  printf( 'g:slimv_repl_split    = %d',    g:slimv_repl_split ) )
    call add( info,  printf( 'g:slimv_repl_wait     = %d',    g:slimv_repl_wait ) )
    call add( info,  printf( 'g:slimv_keybindings   = %d',    g:slimv_keybindings ) )
    call add( info,  printf( 'g:slimv_menu          = %d',    g:slimv_menu ) )
    call SlimvLog( 1, info )
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

" Try to find out if the Lisp dialect used is actually Clojure
if !exists( 'g:slimv_filetype' )
    if match( g:slimv_lisp, 'clojure' ) >= 0
        let g:slimv_filetype = 'clojure'
    endif
endif

" Open a REPL buffer inside Vim?
if !exists( 'g:slimv_repl_open' )
    let g:slimv_repl_open = 1
endif

" Directory name for the REPL buffer file
if !exists( 'g:slimv_repl_dir' )
    if g:slimv_windows
        let g:slimv_repl_dir = matchstr( tempname(), '.*\\' )
    else
        let g:slimv_repl_dir = '/tmp/'
    endif
endif

" Filename for the REPL buffer file
if !exists( 'g:slimv_repl_file' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_repl_file = 'Slimv.REPL.clj'
    else
        let g:slimv_repl_file = 'Slimv.REPL.lisp'
    endif
endif

" Shall we open REPL buffer in split window?
if !exists( 'g:slimv_repl_split' )
    let g:slimv_repl_split = 1
endif

" How many seconds to wait for the REPL output to finish?
if !exists( 'g:slimv_repl_wait' )
    let g:slimv_repl_wait = 10
endif

" Build client command (if not given in vimrc)
if !exists( 'g:slimv_client' )
    let g:slimv_client = SlimvMakeClientCommand()
endif

" Slimv keybinding set (0 = no keybindings)
if !exists( 'g:slimv_keybindings' )
    let g:slimv_keybindings = 1
endif

" Append Slimv menu to the global menu (0 = no menu)
if !exists( 'g:slimv_menu' )
    let g:slimv_menu = 1
endif


" =====================================================================
"  Template definitions
" =====================================================================

if !exists( 'g:slimv_template_pprint' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_pprint = '(doseq [o %1] (println o))'
    else
        let g:slimv_template_pprint = '(dolist (o %1)(pprint o))'
    endif
endif

if !exists( 'g:slimv_template_undefine' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_undefine = "(ns-unmap 'user '" . "%1)"
    else
        let g:slimv_template_undefine = '(fmakunbound (read-from-string "%1"))'
    endif
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
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_inspect = "(print-doc #'" . "%1)"
    else
        let g:slimv_template_inspect = '(inspect %1)'
    endif
endif

if !exists( 'g:slimv_template_apropos' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_apropos = '(find-doc "%1")'
    else
        let g:slimv_template_apropos = '(apropos "%1")'
    endif
endif

if !exists( 'g:slimv_template_macroexpand' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_macroexpand = '%1'
    else
        let g:slimv_template_macroexpand = '(pprint %1)'
    endif
endif

if !exists( 'g:slimv_template_macroexpand_all' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_macroexpand_all = '%1'
    else
        let g:slimv_template_macroexpand_all = '(pprint %1)'
    endif
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
"  Other non-global script variables
" =====================================================================

" Name of the REPL buffer inside Vim
let s:repl_name = g:slimv_repl_dir . g:slimv_repl_file

" Lisp prompt in the last line
let s:prompt = ''

" The current mode when REPL refresh was started
let s:insertmode = 0


" =====================================================================
"  General utility functions
" =====================================================================

" Position the cursor at the end of the REPL buffer
" Optionally mark this position in Vim mark 's'
function! SlimvEndOfReplBuffer( markit )
    if !g:slimv_repl_open
        " User does not want to display REPL in Vim
        return
    endif
    normal G$
    if a:markit
        " Remember the end of the buffer: user may enter commands here
        " Also remember the prompt, because the user may overwrite it
        call setpos( "'s'", [0, line('$'), col('$'), 0] )
        let s:prompt = getline( "'s'" )
        if s:insertmode
            " Hacking: we add a space at the end of the last line
            " so that the cursor remains in correct position after insertmode eval
            "call setline( "'s", s:prompt . " " )
        endif
    endif
    set nomodified
endfunction

" Reload the contents of the REPL buffer from the output file immediately
function! SlimvRefreshReplBufferNow()
    if !g:slimv_repl_open
        " User does not want to display REPL in Vim
        return
    endif

    if bufnr( s:repl_name ) != bufnr( "%" )
        " REPL is not the actual buffer
        return
    endif

    try
        execute "silent edit! " . s:repl_name
    catch /.*/
        " Oops, something went wrong, the buffer will not be refreshed this time
    endtry
    syntax on
    "TODO: use :read instead and keep only the delta in the readout file
    if &endofline == 1
        " Handle the situation when the last line is an empty line in REPL
        " but Vim rejects to handle it as a separate line
        try
            call append( '$', "" )
        catch /.*/
            " OK, we cannot append right now, the server is probably busy with
            " updating the REPL file. Just go on, it's not that important.
        endtry
    endif
    call SlimvEndOfReplBuffer( 1 )
endfunction

" Send interrupt command to REPL
function! SlimvInterrupt()
    call SlimvSend( ['SLIMV::INTERRUPT'], 0 )
endfunction

" Refresh REPL buffer continuously until no change is detected
function! SlimvRefreshReplBuffer()
    if !g:slimv_repl_open
        " User does not want to display REPL in Vim
        return
    endif

    " Refresh REPL buffer for a while until no change is detected
    let ftime = getftime( s:repl_name )
    let lastftime = ftime
    sleep 200m
    call SlimvRefreshReplBufferNow()

    let save_ve = &ve
    if s:insertmode
        " We are in insert mode, let's fake a movement to the right
        " in order to display the cursor at the right place.
        " For this we need to set the virtualedit=all option temporarily
        echon '-- INSERT --'
        set ve=all
        normal l
    else
        " Inform user that we are in running mode (waiting for REPL output)
        echon '-- RUNNING --'
    endif
    let interrupt = 0
    let wait = g:slimv_repl_wait * 10   " number of cycles to wait for refreshing the REPL buffer
    try
        while wait > 0 || g:slimv_repl_wait == 0
            let m = '/\%#/'
            silent! execute 'match Cursor ' . m
            match Cursor /\%#/
            redraw
            if getchar(1)
                break
            endif
            sleep 100m
            let lastftime = ftime
            let ftime = getftime( s:repl_name )
            if ftime != lastftime || ftime == localtime()
                " REPL buffer file changed recently, reload it
                call SlimvRefreshReplBufferNow()
            endif
            if g:slimv_repl_wait != 0
                let wait = wait - 1
            endif
        endwhile
    catch /^Vim:Interrupt$/
        if getchar(1)
            " Swallow interrupt key
            let c = getchar(0)
            if c == 3
                " Yes, this was the Ctrl-C, propagate it to the server
                let interrupt = 1
                call SlimvHandleInterrupt()
            endif
        endif
    endtry

    " Restore everything
    silent! execute 'match None ' . m
    if !interrupt
        let s:insertmode = 0
    endif
    echon '            '
    let &ve = save_ve

    if wait == 0 && ftime != lastftime
        " Time is up and Lisp REPL still did not finish output
        " Inform user about this and about the non-blocking and blocking refresh keys
        if g:slimv_keybindings == 1
            let refresh = "<Leader>z or <Leader>Z"
        elseif g:slimv_keybindings == 2
            let refresh = "<Leader>rw or <Leader>rr"
        else
            let refresh = ":call SlimvRefreshReplBuffer()"
        endif
        call append( '$', "Slimv warning: REPL is busy, refresh display with " . refresh )
        call SlimvEndOfReplBuffer( 1 )
    endif
endfunction

" Open a new REPL buffer or switch to the existing one
function! SlimvOpenReplBuffer()
    "TODO: check if this works without 'set hidden'
    let repl_buf = bufnr( s:repl_name )
    if repl_buf == -1
        " Create a new REPL buffer
        if g:slimv_repl_split
            execute "split " . s:repl_name
        else
            execute "edit " . s:repl_name
        endif
    else
        if g:slimv_repl_split
            " REPL buffer is already created. Check if it is open in a window
            let repl_win = bufwinnr( repl_buf )
            if repl_win == -1
                " Create windows
                execute "split +buffer " . repl_buf
            else
                " Switch to the REPL window
                if winnr() != repl_win
                    execute repl_win . "wincmd w"
                endif
            endif
        else
            execute "buffer " . repl_buf
        endif
    endif

    " This buffer will not have an associated file
    inoremap <buffer> <silent> <CR> <End><CR><C-O>:call SlimvHandleCR()<CR>
    inoremap <buffer> <silent> <expr> <BS> SlimvHandleBS()
    inoremap <buffer> <silent> <Up> <C-O>:call SlimvHandleUp()<CR>
    inoremap <buffer> <silent> <Down> <C-O>:call SlimvHandleDown()<CR>
    execute "au FileChangedShell " . g:slimv_repl_file . " :call SlimvRefreshReplBufferNow()"
    execute "au FocusGained "      . g:slimv_repl_file . " :call SlimvRefreshReplBufferNow()"
    execute "au BufEnter "         . g:slimv_repl_file . " :call SlimvRefreshReplBufferNow()"

    filetype on
    redraw

    call SlimvSend( ['SLIMV::OUTPUT::' . s:repl_name ], 0 )
    call SlimvEndOfReplBuffer( 0 )
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

" Send argument to Lisp server for evaluation
function! SlimvSend( args, open_buffer )
    call SlimvClientCommand()
    if g:slimv_client == ''
        return
    endif

    if a:open_buffer
        call SlimvOpenReplBuffer()
    endif

    " Build a temporary file from the form to be evaluated
    let ar = []
    let i = 0
    while i < len( a:args )
        call extend( ar, split( a:args[i], '\n' ) )
        let i = i + 1
    endwhile

    let tmp = tempname()
    try
        call SlimvLog( 2, a:args )
        call writefile( ar, tmp )

        " Send the file to the client for evaluation
        if g:slimv_debug_client == 0
            let result = system( g:slimv_client . ' -f ' . tmp )
        else
            execute '!' . g:slimv_client . ' -f ' . tmp
        endif
    finally
        call delete(tmp)
    endtry

    if a:open_buffer
        call SlimvRefreshReplBuffer()
    endif
endfunction

" Eval arguments in Lisp REPL
function! SlimvEval( args )
    call SlimvSend( a:args, g:slimv_repl_open )
endfunction

" Set command line after the prompt
function! SlimvSetCommandLine( cmd )
    let line = getline( "." )
    if line( "." ) == line( "'s" )
        " The prompt is in the line marked with 's
        let promptlen = len( s:prompt )
    else
        let promptlen = 0
    endif
    if len( line ) > promptlen
        let line = strpart( line, 0, promptlen )
    endif
    let line = line . a:cmd
    call setline( ".", line )
    call SlimvEndOfReplBuffer( 0 )
endfunction

" Add command list to the command history
function! SlimvAddHistory( cmd )
    if !exists( 'g:slimv_cmdhistory' )
        let g:slimv_cmdhistory = []
    endif
    let i = 0
    while i < len( a:cmd )
        " Trim trailing whitespaces from the command
        let command = substitute( a:cmd[i], "\\(.*[^ ]\\)\\s*", "\\1", "g" )
        call add( g:slimv_cmdhistory, command )
        let i = i + 1
    endwhile
    let g:slimv_cmdhistorypos = len( g:slimv_cmdhistory )
endfunction

" Recall command from the command history at the marked position
function! SlimvRecallHistory()
    if g:slimv_cmdhistorypos >= 0 && g:slimv_cmdhistorypos < len( g:slimv_cmdhistory )
        call SlimvSetCommandLine( g:slimv_cmdhistory[g:slimv_cmdhistorypos] )
    else
        call SlimvSetCommandLine( "" )
    endif
endfunction

" Count the opening and closing parens or brackets to determine if they match
function! s:GetParenCount( lines )
    let paren = 0
    let inside_string = 0
    let i = 0
    while i < len( a:lines )
        let inside_comment = 0
        let j = 0
        while j < len( a:lines[i] )
            if inside_string
                " We are inside a string, skip parens, wait for closing '"'
                if a:lines[i][j] == '"'
                    let inside_string = 0
                endif
            elseif inside_comment
                " We are inside a comment, skip parens, wait for end of line
            else
                " We are outside of strings and comments, now we shall count parens
                if a:lines[i][j] == '"'
                    let inside_string = 1
                endif
                if a:lines[i][j] == ';'
                    let inside_comment = 1
                endif
                if a:lines[i][j] == '(' || a:lines[i][j] == '['
                    let paren = paren + 1
                endif
                if a:lines[i][j] == ')' || a:lines[i][j] == ']'
                    let paren = paren - 1
                    if paren < 0
                        " Oops, too many closing parens in the middle
                        return paren
                    endif
                endif
            endif
            let j = j + 1
        endwhile
        let i = i + 1
    endwhile
    return paren
endfunction

" Handle insert mode 'Enter' keypress in the REPL buffer
function! SlimvHandleCR()
    let lastline = line( "'s" )
    let lastcol  =  col( "'s" )
    if lastline > 0
        if line( "." ) >= lastline
            " Trim the prompt from the beginning of the command line
            " The user might have overwritten some parts of the prompt
            let cmdline = getline( lastline )
            let c = 0
            while c < lastcol - 1 && cmdline[c] == s:prompt[c]
                let c = c + 1
            endwhile
            let cmd = [ strpart( getline( lastline ), c ) ]

            " Build a possible multi-line command
            let l = lastline + 1
            while l <= line("$") - 1
                call add( cmd, strpart( getline( l ), 0) )
                let l = l + 1
            endwhile

            " Count the number of opening and closing braces
            let paren = s:GetParenCount( cmd )
            if paren == 0
                " Expression finished, let's evaluate it
                " but first add it to the history
                let s:insertmode = 1
                call SlimvAddHistory( cmd )
                call SlimvEval( cmd )
            elseif paren < 0
                " Too many closing braces
                let dummy = input( "Too many closing parens found. Press ENTER to continue." )
            else
                " Expression is not finished yet, indent properly and wait for completion
                " Indentation works only if lisp indentation is switched on
                let indent = ''
                let i = lispindent( '.' )
                while i > 0
                    let indent = indent . ' '
                    let i = i - 1
                endwhile
                call setline( ".", indent )
                call SlimvEndOfReplBuffer( 0 )
            endif
        endif
    else
        call append( '$', "Slimv error: previous EOF mark not found, re-enter last form:" )
        call append( '$', "" )
        call SlimvEndOfReplBuffer( 1 )
    endif
endfunction

" Handle insert mode 'Backspace' keypress in the REPL buffer
function! SlimvHandleBS()
    if line( "." ) == line( "'s" ) && col( "." ) <= col( "'s" )
        " No BS allowed before the previous EOF mark
        return ""
    else
        return "\<BS>"
    endif
endfunction

" Handle insert mode 'Up' keypress in the REPL buffer
function! SlimvHandleUp()
    if line( "." ) >= line( "'s" )
        if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos > 0
            let g:slimv_cmdhistorypos = g:slimv_cmdhistorypos - 1
            call SlimvRecallHistory()
        endif
    else
        normal k
    endif
endfunction

" Handle insert mode 'Down' keypress in the REPL buffer
function! SlimvHandleDown()
    if line( "." ) >= line( "'s" )
        if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos < len( g:slimv_cmdhistory )
            let g:slimv_cmdhistorypos = g:slimv_cmdhistorypos + 1
            call SlimvRecallHistory()
        else
            call SlimvSetCommandLine( "" )
        endif
    else
        normal j
    endif
endfunction

" Handle insert mode 'Down' keypress in the REPL buffer
function! SlimvHandleInterrupt()
    call SlimvSend( ['SLIMV::INTERRUPT'], 0 )
    call SlimvRefreshReplBuffer()
endfunction

" Start and connect slimv server
" This is a quite dummy function that just evaluates a comment
function! SlimvConnectServer()
    call SlimvEval( [';;; Slimv client connected successfully'] )
endfunction

" Refresh REPL buffer continuously
function! SlimvRefresh()
    if bufnr( s:repl_name ) == -1
        " REPL not opened, no need to refresh
        return
    endif
    if bufnr( s:repl_name ) != bufnr( "%" )
        " REPL is not the current window, activate it
        call SlimvOpenReplBuffer()
    endif
    call SlimvRefreshReplBuffer()
endfunction

" Refresh REPL buffer continuously
function! SlimvRefreshNow()
    if bufnr( s:repl_name ) == -1
        " REPL not opened, no need to refresh
        return
    endif
    if bufnr( s:repl_name ) != bufnr( "%" )
        " REPL is not the current window, activate it
        call SlimvOpenReplBuffer()
    endif
    call SlimvRefreshReplBufferNow()
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
    "let p1 = escape( p1, '\\' )
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
    "let p1 = escape( p1, '\\' )
    "let p2 = escape( p2, '\\' )
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

" General part of the various macroexpand functions
function! SlimvMacroexpandGeneral( command )
    normal 99[(
    let line = getline( "." )
    if match( line, 'defmacro' ) < 0
        " The form does not contain 'defmacro', put it in a macroexpand block
        call SlimvSelectForm()
        let m = "(" . a:command . " '" . SlimvGetSelection() . ")"
    else
        " The form is a 'defmacro', so do a macroexpand from the macro name and parameters
        if SlimvGetFiletype() == 'clojure'
            " Some Vim configs (e.g. matchit.vim) include the trailing ']' after '%' in Visual mode
            normal vt[%ht]"sy
        else
            normal vt(])"sy
        endif
        let m = SlimvGetSelection() . '))'
        let m = substitute( m, "defmacro\\s*", a:command . " '(", 'g' )
        if SlimvGetFiletype() == 'clojure'
            " Remove opening bracket from the parameter list
            " TODO: fix this for multi-line macro header
            let m = substitute( m, "\\[\\(.*\\)", "\\1", 'g' )
        else
            " Remove opening brace from the parameter list
            " The nice regular expression below says: remove the third '('
            " ( + something + ( + something + ( + something -> ( + something + ( + something + something
            " TODO: fix this for multi-line macro header
            let m = substitute( m, "\\(([^()]*([^()]*\\)(\\(.*\\)", "\\1\\2", 'g' )
        endif
    endif
    return m
endfunction

" Macroexpand-1 the current top level form
function! SlimvMacroexpand()
    let m = SlimvMacroexpandGeneral( "macroexpand-1" )
    call SlimvEvalForm1( g:slimv_template_macroexpand, m )
endfunction

" Macroexpand the current top level form
function! SlimvMacroexpandAll()
    let m = SlimvMacroexpandGeneral( "macroexpand" )
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

    noremap <Leader>S  :call SlimvConnectServer()<CR>
    noremap <Leader>z  :call SlimvRefresh()<CR>
    noremap <Leader>Z  :call SlimvRefreshNow()<CR>

elseif g:slimv_keybindings == 2
    " Easy to remember (two-key) keybinding set

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

    " REPL commands
    noremap <Leader>rc  :call SlimvConnectServer()<CR>
    noremap <Leader>rr  :call SlimvRefresh()<CR>
    noremap <Leader>rn  :call SlimvRefreshNow()<CR>

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

    menu &Slimv.&REPL.&Connect-Server                  :call SlimvConnectServer()<CR>
    menu &Slimv.&REPL.&Refresh                         :call SlimvRefresh()<CR>
    menu &Slimv.&REPL.&Refresh-Now                     :call SlimvRefreshNow()<CR>
endif

