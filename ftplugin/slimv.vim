" slimv.vim:    The Superior Lisp Interaction Mode for VIM
" Version:      0.8.3
" Last Change:  17 May 2011
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

let g:slimv_windows = 0
let g:slimv_cygwin  = 0

if has( 'win32' ) || has( 'win95' ) || has( 'win64' ) || has( 'win16' )
    let g:slimv_windows = 1
elseif has( 'win32unix' )
    let g:slimv_cygwin = 1
endif


" =====================================================================
"  Functions used by global variable definitions
" =====================================================================

" Try to autodetect Python executable
function! SlimvAutodetectPython()
    if !g:slimv_cygwin && executable( 'python' )
        return 'python'
    endif

    if g:slimv_windows || g:slimv_cygwin
        " Try to find Python on the standard installation places
        " For Cygwin we need to use the Windows Python instead of the Cygwin Python
        let pythons = split( globpath( 'c:/python*,c:/Program Files/python*', 'python.exe' ), '\n' )
        if len( pythons ) == 0
            " Go deeper in subdirectories
            let pythons = split( globpath( 'c:/python*/**,c:/Program Files/python*/**', 'python.exe' ), '\n' )
            if len( pythons ) == 0
                return ''
            endif
        endif
        let pycmd = pythons[0]
        if match( pycmd, ' ' ) >= 0
            " Convert Python command to short 8.3 format if path contains spaces
            let pycmd = fnamemodify( pycmd, ':8' )
        endif
        return pycmd
    else
        return ''
    endif
endfunction

" Build the command to start the client
function! SlimvMakeClientCommand()
    if g:slimv_python == '' || g:slimv_lisp == ''
        " We don't have enough information to build client command
        return ''
    endif

    " Start with the Python path
    let cmd = g:slimv_python

    " Add path of Slimv script, enclose it in double quotes if path contains spaces
    if g:slimv_path[0] != '"' && match( g:slimv_path, ' ' ) >= 0
        let cmd = cmd . ' "' . g:slimv_path . '"'
    else
        let cmd = cmd . ' ' . g:slimv_path
    endif

    " Add port number if different from default
    if g:slimv_port != 5151
        let cmd = cmd . ' -p ' . g:slimv_port
    endif

    " Add Lisp path
    if g:slimv_lisp[0] != '"' && match( g:slimv_lisp, ' ' ) >= 0
        let cmd = cmd . ' -l "' . g:slimv_lisp . '"'
    else
        let cmd = cmd . ' -l ' . g:slimv_lisp
    endif

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

" Convert Cygwin path to Windows path, if needed
function! s:Cygpath( path )
    let path = a:path
    if g:slimv_cygwin
        let path = system( 'cygpath -w ' . path )
        let path = substitute( path, "\n", "", "g" )
        let path = substitute( path, "\\", "/", "g" )
    endif
    return path
endfunction

" Find slimv.py in the Vim ftplugin directory (if not given in vimrc)
if !exists( 'g:slimv_path' )
    let plugins = split( globpath( &runtimepath, 'ftplugin/**/slimv.py'), '\n' )
    if len( plugins ) > 0
        let g:slimv_path = s:Cygpath( plugins[0] )
    else
        let g:slimv_path = 'slimv.py'
    endif
endif

" Get the filetype (Lisp dialect) used by Slimv
function! SlimvGetFiletype()
    if &ft != ''
        " Return Vim filetype if defined
        return &ft
    endif

    if match( tolower( g:slimv_lisp ), 'clojure' ) >= 0 || match( tolower( g:slimv_lisp ), 'clj' ) >= 0
        " Must be Clojure
        return 'clojure'
    endif

    " We have no clue, guess its lisp
    return 'lisp'
endfunction

" Try to autodetect SWANK and build the command to start the SWANK server
function! SlimvSwankCommand()
    if exists( 'g:slimv_swank_clojure' ) && SlimvGetFiletype() == 'clojure'
        return g:slimv_swank_clojure
    endif
    if exists( 'g:slimv_swank_scheme' ) && SlimvGetFiletype() == 'scheme'
        return g:slimv_swank_scheme
    endif
    if exists( 'g:slimv_swank_cmd' )
        return g:slimv_swank_cmd
    endif

    let cmd = ''
    if SlimvGetFiletype() == 'clojure'
        " First autodetect 'lein swank'
        if executable( 'lein' )
            let cmd = '"lein swank"'
        else
            " Check if swank-clojure is bundled with Slimv
            let swanks = split( globpath( &runtimepath, 'swank-clojure/swank/swank.clj'), '\n' )
            if len( swanks ) == 0
                return ''
            endif
            let sclj = substitute( swanks[0], '\', '/', "g" )
            let cmd = g:slimv_lisp . ' -e "(load-file \"' . sclj . '\") (swank.swank/start-repl)" -r'
        endif
    elseif SlimvGetFiletype() == 'scheme'
        let swanks = split( globpath( &runtimepath, 'slime/contrib/swank-mit-scheme.scm'), '\n' )
        if len( swanks ) == 0
            return ''
        endif
        if b:SlimvImplementation() == 'mit'
            let cmd = '"' . g:slimv_lisp . '" --load "' . swanks[0] . '"'
        endif
    else
        " First check if SWANK is bundled with Slimv
        let swanks = split( globpath( &runtimepath, 'slime/start-swank.lisp'), '\n' )
        if len( swanks ) == 0
            " Try to find SWANK in the standard SLIME installation locations
            if g:slimv_windows || g:slimv_cygwin
                let swanks = split( globpath( 'c:/slime/,c:/*lisp*/slime/,c:/*lisp*/site/lisp/slime/,c:/Program Files/*lisp*/site/lisp/slime/', 'start-swank.lisp' ), '\n' )
            else
                let swanks = split( globpath( '/usr/share/common-lisp/source/slime/', 'start-swank.lisp' ), '\n' )
            endif
        endif
        if len( swanks ) == 0
            return ''
        endif

        " Build proper SWANK start command for the Lisp implementation used
        if b:SlimvImplementation() == 'sbcl'
            let cmd = '"' . g:slimv_lisp . '" --load "' . swanks[0] . '"'
        elseif b:SlimvImplementation() == 'clisp'
            let cmd = '"' . g:slimv_lisp . '" -i "' . swanks[0] . '"'
        elseif b:SlimvImplementation() == 'allegro'
            let cmd = '"' . g:slimv_lisp . '" -L "' . swanks[0] . '"'
        elseif b:SlimvImplementation() == 'cmu'
            let cmd = '"' . g:slimv_lisp . '" -load "' . swanks[0] . '"'
        else
            let cmd = '"' . g:slimv_lisp . '" -l "' . swanks[0] . '"'
        endif
    endif
    if cmd != ''
        if g:slimv_windows || g:slimv_cygwin
            return '!start /MIN ' . cmd
        else
            return '! xterm -iconic -e ' . cmd . ' &'
        endif
    endif
    return ''
endfunction

" =====================================================================
"  Global variable definitions
" =====================================================================

" Use SWANK server
if !exists( 'g:slimv_swank' )
    let g:slimv_swank = 1
endif

" TCP port number to use for the SWANK server
if !exists( 'g:swank_port' )
    let g:swank_port = 4005
endif

" TCP port number to use for the Slimv server
if !exists( 'g:slimv_port' )
    let g:slimv_port = 5151
endif

" Find Python (if not given in vimrc)
if !exists( 'g:slimv_python' )
    let g:slimv_python = SlimvAutodetectPython()
endif

" Find Lisp (if not given in vimrc)
if !exists( 'g:slimv_lisp' )
    let lisp = b:SlimvAutodetect()
    let g:slimv_lisp = lisp[0]
    if !exists( 'g:slimv_impl' )
        let g:slimv_impl = lisp[1]
    endif
endif

" Try to find out the Lisp implementation
" if not autodetected and not given in vimrc
if !exists( 'g:slimv_impl' )
    let g:slimv_impl = b:SlimvImplementation()
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
        let g:slimv_repl_dir = s:Cygpath( '/tmp/' )
    endif
endif

" Filename for the REPL buffer file
if !exists( 'g:slimv_repl_file' )
    let g:slimv_repl_file = b:SlimvREPLFile()
endif

" Shall we open REPL buffer in split window?
if !exists( 'g:slimv_repl_split' )
    let g:slimv_repl_split = 1
endif

" Wrap long lines in REPL buffer
if !exists( 'g:slimv_repl_wrap' )
    let g:slimv_repl_wrap = 1
endif

" Maximum number of lines echoed from the evaluated form
if !exists( 'g:slimv_echolines' )
    let g:slimv_echolines = 4
endif

" Syntax highlighting for the REPL buffer
if !exists( 'g:slimv_repl_syntax' )
    let g:slimv_repl_syntax = 0
endif

" Alternative value (in msec) for 'updatetime' while the REPL buffer is changing
if !exists( 'g:slimv_updatetime' )
    let g:slimv_updatetime = 500
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

" Build the ctags command capable of generating lisp tags file
" The command can be run with execute 'silent !' . g:slimv_ctags
if !exists( 'g:slimv_ctags' )
    let ctags = split( globpath( '$vim,$vimruntime', 'ctags.exe' ), '\n' )
    if len( ctags ) > 0
        " Remove -a option to regenerate every time
        let g:slimv_ctags = '"' . ctags[0] . '" -a --language-force=lisp *.lisp *.clj'
    endif
endif

" Package/namespace handling
if !exists( 'g:slimv_package' )
    let g:slimv_package = 1
endif

" General timeout for various startup and connection events (seconds)
if !exists( 'g:slimv_timeout' )
    let g:slimv_timeout = 20
endif

" Use balloonexpr to display symbol description
if !exists( 'g:slimv_balloon' )
    let g:slimv_balloon = 1
endif

" Shall we use simple or fuzzy completion?
if !exists( 'g:slimv_simple_compl' )
    let g:slimv_simple_compl = 0
endif

" Custom <Leader> for the Slimv plugin
if !exists( 'g:slimv_leader' )
    if exists( 'mapleader' )
        let g:slimv_leader = mapleader
    else
        let g:slimv_leader = ','
    endif
endif


" =====================================================================
"  Template definitions
" =====================================================================

if !exists( 'g:slimv_template_pprint' )
    if SlimvGetFiletype() == 'clojure'
        let g:slimv_template_pprint = '(doseq [o %1] (println o))'
    else
        let g:slimv_template_pprint = '(dolist (o %1) (pprint o))'
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
    if b:SlimvImplementation() == 'sbcl'
        let g:slimv_template_profile = '(sb-profile:profile %1)'
    else
        let g:slimv_template_profile = '(mon:monitor %1)'
    endif
endif

if !exists( 'g:slimv_template_unprofile' )
    if b:SlimvImplementation() == 'sbcl'
        let g:slimv_template_unprofile = '(sb-profile:unprofile %1)'
    else
        let g:slimv_template_unprofile = '(mon:unmonitor %1)'
    endif
endif

if !exists( 'g:slimv_template_unprofile_all' )
    if b:SlimvImplementation() == 'sbcl'
        let g:slimv_template_unprofile_all = '(sb-profile:unprofile)'
    else
        let g:slimv_template_unprofile_all = '(mon:unmonitor)'
    endif
endif

if !exists( 'g:slimv_template_show_profiled' )
    if b:SlimvImplementation() == 'sbcl'
        let g:slimv_template_show_profiled = '(sb-profile:profile)'
    else
        let g:slimv_template_show_profiled = '(pprint mon:*monitored-functions*)'
    endif
endif

if !exists( 'g:slimv_template_profile_report' )
    if b:SlimvImplementation() == 'sbcl'
        let g:slimv_template_profile_report = '(sb-profile:report)'
    else
        let g:slimv_template_profile_report = '(mon:report-monitoring)'
    endif
endif

if !exists( 'g:slimv_template_profile_reset' )
    if b:SlimvImplementation() == 'sbcl'
        let g:slimv_template_profile_reset = '(sb-profile:reset)'
    else
        let g:slimv_template_profile_reset = '(mon:reset-all-monitoring)'
    endif
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


" =====================================================================
"  Other non-global script variables
" =====================================================================

let s:repl_name = g:slimv_repl_dir . g:slimv_repl_file    " Name of the REPL buffer inside Vim
let s:prompt = ''                                         " Lisp prompt in the last line
let s:indent = ''                                         " Most recent indentation info
let s:last_update = 0                                     " The last update time for the REPL buffer
let s:last_size = 0                                       " The last size of the REPL buffer
let s:save_updatetime = &updatetime                       " The original value for 'updatetime'
let s:save_showmode = &showmode                           " The original value for 'showmode'
let s:python_initialized = 0                              " Is the embedded Python initialized?
let s:swank_connected = 0                                 " Is the SWANK server connected?
let s:swank_package = ''                                  " Package to use at the next SWANK eval
let s:swank_form = ''                                     " Form to send to SWANK
let s:refresh_disabled = 0                                " Set this variable temporarily to avoid recursive REPL rehresh calls
let s:debug_activated = 0                                 " Are we in the SWANK debugger?
let s:debug_move_cursor = 0                               " Move cursor to Restarts when debug activated
let s:compiled_file = ''                                  " Name of the compiled file
let s:au_curhold_set = 0                                  " Whether the autocommand has been set
let s:skip_sc = 'synIDattr(synID(line("."), col("."), 0), "name") =~ "[Ss]tring\\|[Cc]omment"'
                                                          " Skip matches inside string or comment 

" =====================================================================
"  General utility functions
" =====================================================================

" Display an error message
function SlimvError( msg )
    echohl ErrorMsg
    echo a:msg
    echohl None
endfunction 

" Display an error message and a question, return user response
function SlimvErrorAsk( msg, question )
    echohl ErrorMsg
    let answer = input( a:msg . a:question )
    echo ""
    echohl None
    return answer
endfunction 

" Display an error message and wait for ENTER
function SlimvErrorWait( msg )
    call SlimvErrorAsk( a:msg, " Press ENTER to continue." )
endfunction 

" Position the cursor at the end of the REPL buffer
" Optionally mark this position in Vim mark 's'
function! SlimvEndOfReplBuffer()
    if !g:slimv_repl_open
        " User does not want to display REPL in Vim
        return
    endif
    normal! G$
endfunction

" Remember the end of the REPL buffer: user may enter commands here
" Also remember the prompt, because the user may overwrite it
function! SlimvMarkBufferEnd()
    call setpos( "'s", [0, line('$'), col('$'), 0] )
    let s:prompt = getline( "'s" )
endfunction

" Handle response coming from the SWANK listener
function! SlimvSwankResponse()
    let s:refresh_disabled = 1
    call SlimvCommand( 'python swank_output()' )
    let s:refresh_disabled = 0
    let msg = ''
    redir => msg
    silent execute 'python swank_response("")'
    redir END

    if s:swank_action != '' && msg != ''
        if s:swank_action == ':describe-symbol'
            echo msg
            echo input('Press ENTER to continue.')
        endif
    endif
    if s:swank_actions_pending == ''
        " All SWANK output handled
        let &updatetime = s:save_updatetime
    endif
endfunction

" Execute the given command and write its output at the end of the REPL buffer
function! SlimvCommand( cmd )
    " Execute the command with output redirected to variable
    let msg = ''
    redir => msg
    silent execute a:cmd
    redir END

    let repl_buf = bufnr( g:slimv_repl_file )
    if repl_buf == -1
        " REPL buffer not loaded
        return
    endif
    let repl_win = bufwinnr( repl_buf )
    let this_win = winnr()

    if msg == ''
        " No new REPL output since the last refresh
        if g:slimv_updatetime > 0 && s:last_update < localtime() - 1
            let &updatetime = s:save_updatetime
        endif
        return
    endif
    let this_buf = bufnr( "%" )
    if repl_buf != this_buf
        " Switch to the REPL buffer/window
        try
            if g:slimv_repl_split && repl_win != -1
                if this_win != repl_win
                    execute repl_win . "wincmd w"
                endif
            else
                execute "buf " . repl_buf
            endif
        catch /.*/
            " Some Vim versions give an E303 error here
            " but we don't need a swapfile for the REPL buffer anyway
        endtry
    endif

    if g:slimv_updatetime > 0
        let &updatetime = g:slimv_updatetime
    endif

    let lines = split( msg, '\n' )
    set noreadonly
    call append( '$', lines )
    set readonly
    set nomodified
    let s:last_update = localtime()

    if !g:slimv_repl_syntax
        set syntax=
    endif
    if g:slimv_swank
        setlocal buftype=nofile
        setlocal noswapfile
    else
        setlocal autoread
    endif
    call SlimvEndOfReplBuffer()
    call SlimvMarkBufferEnd()
    if s:debug_activated && s:debug_move_cursor
        call search( '^Restarts:', 'bW' )
        let s:debug_move_cursor = 0
        stopinsert
    endif

    if repl_buf != this_buf && repl_win != -1 && !s:debug_activated
        " Switch back to the caller buffer/window
        if g:slimv_repl_split
            if this_win != repl_win
                execute this_win . "wincmd w"
            endif
        else
            execute "buf " . this_buf
        endif
    endif
endfunction

" Execute the given SWANK command, wait for and return the response
function! SlimvCommandGetResponse( name, cmd )
    let s:refresh_disabled = 1
    call SlimvCommand( a:cmd )
    let msg = ''
    let s:swank_action = ''
    let starttime = localtime()
    let cmd_timeout = 3
    while s:swank_action == '' && localtime()-starttime < cmd_timeout
        python swank_listen()
        redir => msg
        silent execute 'python swank_response("' . a:name . '")'
        redir END
    endwhile
    let s:refresh_disabled = 0
    return msg
endfunction

" Reload the contents of the REPL buffer from the output file if changed
function! SlimvRefreshReplBuffer()
    if s:refresh_disabled
        " Refresh is unwanted at the moment, probably another refresh is going on
        return
    endif

"    if !g:slimv_repl_open || !g:slimv_repl_split
    if !g:slimv_repl_open
        " User does not want to display REPL in Vim
        " or does not display it in a split window
        return
    endif

    let repl_buf = bufnr( g:slimv_repl_file )
    if repl_buf == -1
        " REPL buffer not loaded
        return
    endif
    let repl_win = bufwinnr( repl_buf )
    let this_win = winnr()

    if g:slimv_swank && s:swank_connected
        call SlimvSwankResponse()
        return
    endif

    let size = getfsize( s:repl_name )
    if size == s:last_size
        " REPL output file did not change since the last refresh
        if g:slimv_updatetime > 0 && s:last_update < localtime() - 1
            let &updatetime = s:save_updatetime
        endif
        return
    endif
    let this_buf = bufnr( "%" )
    if repl_buf != this_buf
        " Switch to the REPL buffer/window
        try
            if g:slimv_repl_split && repl_win != -1
                if this_win != repl_win
                    execute repl_win . "wincmd w"
                endif
            else
                execute "buf " . repl_buf
            endif
        catch /.*/
            " Some Vim versions give an E303 error here
            " but we don't need a swapfile for the REPL buffer anyway
        endtry
    endif

    if g:slimv_updatetime > 0
        let &updatetime = g:slimv_updatetime
    endif

    try
        execute "silent view! " . s:repl_name
        let s:last_size = size
        let s:last_update = localtime()
    catch /.*/
        " Oops, something went wrong, the buffer will not be refreshed this time
    endtry
    syntax on
    if !g:slimv_repl_syntax
        set syntax=
    endif
    if g:slimv_swank
        setlocal buftype=nofile
        setlocal noswapfile
    else
        setlocal autoread
    endif
    call SlimvEndOfReplBuffer()
    call SlimvMarkBufferEnd()
    set nomodified

    if repl_buf != this_buf && repl_win != -1 && !s:debug_activated
        " Switch back to the caller buffer/window
        if g:slimv_repl_split
            if this_win != repl_win
                execute this_win . "wincmd w"
            endif
        else
            execute "buf " . this_buf
        endif
    endif
endfunction

" This function re-triggers the CursorHold event
" after refreshing the REPL buffer
function! SlimvTimer()
    call SlimvRefreshReplBuffer()
    if g:slimv_repl_open
        if mode() == 'i' || mode() == 'I'
            " Put '<Insert>' twice into the typeahead buffer, which should not do anything
            " just switch to overwrite mode then back to insert mode
            call feedkeys("\<insert>\<insert>")
        else
            " Put an incomplete 'f' command and an Esc into the typeahead buffer
            call feedkeys("f\e")
        endif
    endif
endfunction

" Switch refresh mode on:
" refresh REPL buffer on frequent Vim events
function! SlimvRefreshModeOn()
    set readonly
    if g:slimv_swank
        setlocal buftype=nofile
        setlocal noswapfile
    else
        setlocal autoread
    endif
    if ! g:slimv_swank
        execute "au CursorMoved  * :call SlimvRefreshReplBuffer()"
        execute "au CursorMovedI * :call SlimvRefreshReplBuffer()"
    endif
    if ! s:au_curhold_set
        let s:au_curhold_set = 1
        execute "au CursorHold   * :call SlimvTimer()"
        execute "au CursorHoldI  * :call SlimvTimer()"
    endif
    call SlimvRefreshReplBuffer()
endfunction

" Switch refresh mode off
function! SlimvRefreshModeOff()
    if ! g:slimv_swank
        execute "au! CursorMoved"
        execute "au! CursorMovedI"
    endif
    execute "au! CursorHold"
    execute "au! CursorHoldI"
    let s:au_curhold_set = 0
    set noreadonly
endfunction

" Called when entering REPL buffer
function! SlimvReplEnter()
    call SlimvAddReplMenu()
    execute "au FileChangedRO " . g:slimv_repl_file . " :call SlimvRefreshModeOff()"
    call SlimvRefreshModeOn()
endfunction

" Called when leaving REPL buffer
function! SlimvReplLeave()
    try
        " Check if REPL menu exists, then remove it
        aunmenu REPL
        execute ':unmap ' . g:slimv_leader . '\'
    catch /.*/
        " REPL menu not found, we cannot remove it
    endtry
    if g:slimv_repl_split
        call SlimvRefreshModeOn()
    else
        call SlimvRefreshModeOff()
    endif
endfunction

" View the given file in a top/bottom/left/right split window
function! s:SplitView( filename )
    if g:slimv_repl_split == 1
        execute "silent topleft sview! " . a:filename
    elseif g:slimv_repl_split == 2
        execute "silent botright sview! " . a:filename
    elseif g:slimv_repl_split == 3
        execute "silent topleft vertical sview! " . a:filename
    elseif g:slimv_repl_split == 4
        execute "silent botright vertical sview! " . a:filename
    else
        execute "silent view! " . a:filename
    endif
endfunction

" Open a new REPL buffer or switch to the existing one
function! SlimvOpenReplBuffer()
    let repl_buf = bufnr( g:slimv_repl_file )
    if repl_buf == -1
        " Create a new REPL buffer
        call s:SplitView( s:repl_name )
    else
        if g:slimv_repl_split
            " REPL buffer is already created. Check if it is open in a window
            let repl_win = bufwinnr( repl_buf )
            if repl_win == -1
                " Create windows
                call s:SplitView( s:repl_name )
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

    " Add keybindings valid only for the REPL buffer
    if g:slimv_swank
        inoremap <buffer> <silent>    <CR>   <C-R>=pumvisible() ? "\<lt>CR>" : "\<lt>End>\<lt>C-O>:call SlimvSendCommand(0)\<lt>CR>"<CR>
        inoremap <buffer> <silent>    <C-CR> <End><C-O>:call SlimvSendCommand(1)<CR>
    else
        inoremap <buffer> <silent>    <CR>   <C-R>=pumvisible() ? "\<lt>CR>" : "\<lt>End>\<lt>CR>\<lt>C-O>:call SlimvSendCommand(0)\<lt>CR>"<CR>
        inoremap <buffer> <silent>    <C-CR> <End><CR><C-O>:call SlimvSendCommand(1)<CR>
    endif
    inoremap <buffer> <silent>        <Up>   <C-R>=pumvisible() ? "\<lt>Up>" : "\<lt>C-O>:call SlimvHandleUp()\<lt>CR>"<CR>
    inoremap <buffer> <silent>        <Down> <C-R>=pumvisible() ? "\<lt>Down>" : "\<lt>C-O>:call SlimvHandleDown()\<lt>CR>"<CR>
    noremap  <buffer> <silent>        <CR>   :call SlimvHandleEnter()<CR>
    inoremap <buffer> <silent>        <C-C>  <C-O>:call SlimvInterrupt()<CR>

    if exists( 'g:paredit_loaded' )
        inoremap <buffer> <silent> <expr> <BS>   PareditBackspace(1)
    else
        inoremap <buffer> <silent> <expr> <BS>   SlimvHandleBS()
    endif

    if g:slimv_keybindings == 1
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'.      :call SlimvSendCommand(0)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'/      :call SlimvSendCommand(1)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'<Up>   :call SlimvPreviousCommand()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'<Down> :call SlimvNextCommand()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'z      :call SlimvRefresh()<CR>'
    elseif g:slimv_keybindings == 2
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rs     :call SlimvSendCommand(0)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'ro     :call SlimvSendCommand(1)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rp     :call SlimvPreviousCommand()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rn     :call SlimvNextCommand()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rr     :call SlimvRefresh()<CR>'
    endif

    if g:slimv_repl_wrap
        inoremap <buffer> <silent>        <Home> <C-O>g<Home>
        inoremap <buffer> <silent>        <End>  <C-O>g<End>
        noremap  <buffer> <silent>        <Up>   gk
        noremap  <buffer> <silent>        <Down> gj
        noremap  <buffer> <silent>        <Home> g<Home>
        noremap  <buffer> <silent>        <End>  g<End>
        noremap  <buffer> <silent>        k      gk
        noremap  <buffer> <silent>        j      gj
        noremap  <buffer> <silent>        0      g0
        noremap  <buffer> <silent>        $      g$
        set wrap
    endif

    hi SlimvNormal term=none cterm=none gui=none
    hi SlimvCursor term=reverse cterm=reverse gui=reverse

    " Add autocommands specific to the REPL buffer
    execute "au FileChangedShell " . g:slimv_repl_file . " :call SlimvRefreshReplBuffer()"
    execute "au FocusGained "      . g:slimv_repl_file . " :call SlimvRefreshReplBuffer()"
    execute "au BufEnter "         . g:slimv_repl_file . " :call SlimvReplEnter()"
    execute "au BufLeave "         . g:slimv_repl_file . " :call SlimvReplLeave()"

    filetype on
    if g:slimv_swank
        setlocal buftype=nofile
        setlocal noswapfile
    else
        setlocal autoread
    endif
    redraw
    let s:last_size = 0

    call SlimvRefreshReplBuffer()
endfunction

" Select symbol under cursor and return it
function! SlimvSelectSymbol()
    if SlimvGetFiletype() == 'clojure'
        setlocal iskeyword+=~,#,&,\|,{,},!,?
    else
        setlocal iskeyword+=~,#,&,\|,{,},[,],!,?
    endif
    let symbol = expand('<cword>')
    return symbol
endfunction

" Select symbol with possible prefixes under cursor and return it
function! SlimvSelectSymbolExt()
    let save_iskeyword = &iskeyword
    if SlimvGetFiletype() == 'clojure'
        setlocal iskeyword+=~,#,&,\|,{,},!,?,'
    else
        setlocal iskeyword+=~,#,&,\|,{,},[,],!,?,'
    endif
    let symbol = expand('<cword>')
    let &iskeyword = save_iskeyword
    return symbol
endfunction

" Select bottom level form the cursor is inside and copy it to register 's'
function! SlimvSelectForm()
    " Search the opening '(' if we are standing on a special form prefix character
    let c = col( '.' ) - 1
    while c < len( getline( '.' ) ) && match( "'`#", getline( '.' )[c] ) >= 0
        normal! l
        let c = c + 1
    endwhile
    normal! va(o
    " Handle '() or #'() etc. type special syntax forms
    let c = col( '.' ) - 2
    while c >= 0 && match( ' \t()', getline( '.' )[c] ) < 0
        normal! h
        let c = c - 1
    endwhile
    silent normal! "sy
    let sel = SlimvGetSelection()
    if sel == ''
        call SlimvError( "Form is empty." )
        return 0
    elseif sel == '(' || sel == '['
        call SlimvError( "Form is unbalanced." )
        return 0
    else
        return 1
    endif
endfunction

" Find starting '(' of a top level form
function SlimvFindDefunStart()
    let l = line( '.' )
    let matchb = max( [l-100, 1] )
    while searchpair( '(', '', ')', 'bW', s:skip_sc, matchb )
    endwhile
endfunction

" Select top level form the cursor is inside and copy it to register 's'
function! SlimvSelectDefun()
    call SlimvFindDefunStart()
    return SlimvSelectForm()
endfunction

" Return the contents of register 's'
function! SlimvGetSelection()
    return getreg( '"s' )
endfunction

" Find the given string backwards and put it in front of the current selection
" if it is a valid Lisp form (i.e. not inside comment or string)
function! SlimvFindAddSel( string )
    let found = 0
    let searching = search( '(\s*' . a:string . '\s', 'bcW' )
    while searching
        " Search for the previos occurrence
        if synIDattr( synID( line('.'), col('.'), 0), 'name' ) !~ '[Ss]tring\|[Cc]omment'
            " It is not inside a comment or string
            let found = 1
            break
        endif
        let searching = search( '(\s*' . a:string . '\s', 'bW' )
    endwhile
    if found
        if g:slimv_swank
            silent normal! ww
            let s:swank_package = expand('<cword>')
        else
            " Put the form just found at the beginning of the selection
            let sel = SlimvGetSelection()
            silent normal! v%"sy
            call setreg( '"s', SlimvGetSelection() . "\n" . sel )
        endif
    endif
endfunction

" Find and add language specific package/namespace definition before the
" cursor position and if exists then add it in front of the current selection
function! SlimvFindPackage()
    if !g:slimv_package || s:debug_activated || SlimvGetFiletype() == 'scheme'
        return
    endif
    if SlimvGetFiletype() == 'clojure'
        call SlimvFindAddSel( 'in-ns' )
    else
        call SlimvFindAddSel( 'in-package' )
    endif
endfunction

" Execute the given SWANK command with current package defined
function! SlimvCommandUsePackage( cmd )
    let oldpos = getpos( '.' ) 
    call SlimvFindPackage()
    let s:refresh_disabled = 1
    call SlimvCommand( a:cmd )
    let s:swank_package = ''
    call setpos( '.', oldpos ) 
    let s:refresh_disabled = 0
    call SlimvRefreshReplBuffer()
endfunction

" Initialize embedded Python and connect to SWANK server
function! SlimvConnectSwank()
    if !s:python_initialized
        if ! has('python')
            call SlimvErrorWait( 'Vim is compiled without the Python feature. Switching off SWANK client.' )
            let g:slimv_swank = 0
            return 0
        endif
        if g:slimv_windows || g:slimv_cygwin
            " Verify that Vim is compiled with Python and Python is properly installed
            let v = ''
            redir => v
            silent ver
            redir END
            let pydll = matchstr( v, '\cpython..\.dll' )
            if ! executable( pydll )
                call SlimvErrorWait( pydll . ' not found. Switching off SWANK client.' )
                let g:slimv_swank = 0
                return 0
            endif
        endif
        python import vim
        execute 'pyfile ' . substitute( g:slimv_path, "slimv.py", "swank.py", "g" )
        let s:python_initialized = 1
    endif

    if !s:swank_connected
        let s:swank_version = ''
        python swank_connect( "g:swank_port", "result" )
        if result != ''
            " SWANK server is not running, start server if possible
            let swank = SlimvSwankCommand()
            if swank != ''
                redraw
                echon "\rStarting SWANK server..."
                silent execute swank
                let starttime = localtime()
                while result != '' && localtime()-starttime < g:slimv_timeout
                    sleep 500m
                    python swank_connect( "g:swank_port", "result" )
                endwhile
                redraw!
            endif
        endif
        if result == ''
            " Connected to SWANK server
            redraw
            echon "\rGetting SWANK connection info..."
            let starttime = localtime()
            while s:swank_version == '' && localtime()-starttime < g:slimv_timeout
                call SlimvSwankResponse()
            endwhile
            if s:swank_version >= '2008-12-23'
                python swank_create_repl()
                call SlimvSwankResponse()
            endif
            let s:swank_connected = 1
            if g:slimv_simple_compl == 0
                python swank_require('swank-fuzzy')
                call SlimvSwankResponse()
            endif
            redraw
            echon "\rConnected to SWANK server on port " . g:swank_port . "."
        else
            " Display connection error message
            let answer = SlimvErrorAsk( result, " Switch off SWANK client [Y/n]?" )
            if answer[0] != 'n' && answer[0] != 'N'
                let g:slimv_swank = 0
            endif
        endif
    endif
    return s:swank_connected
endfunction

" Send argument to Lisp server for evaluation
function! SlimvSend( args, open_buffer, echoing )
    call SlimvClientCommand()
    if g:slimv_client == ''
        return
    endif

    let repl_buf = bufnr( g:slimv_repl_file )
    let repl_win = bufwinnr( repl_buf )

    if a:open_buffer && ( repl_buf == -1 || ( g:slimv_repl_split && repl_win == -1 ) )
        call SlimvOpenReplBuffer()
    endif

    if g:slimv_swank
        call SlimvConnectSwank()
    endif
    if g:slimv_swank && !s:swank_connected
        return
    endif

    " Send the lines to the client for evaluation
    let text = join( a:args, "\n" ) . "\n"

    if g:slimv_swank
        let s:refresh_disabled = 1
        let s:swank_form = text
        if a:echoing
            if g:slimv_echolines > 0
                let nlpos = match( s:swank_form, "\n", 0, g:slimv_echolines )
                if nlpos > 0
                    " Echo only the first g:slimv_echolines number of lines
                    let s:swank_form = strpart( s:swank_form, 0, nlpos ) . " ..."
                    let end = s:CloseForm( [s:swank_form] )
                    if end != 'ERROR'
                        let s:swank_form = s:swank_form . end
                    endif
                endif
            endif
            call SlimvCommand( 'echo s:swank_form' )
            let s:swank_form = text
        endif
        call SlimvCommand( 'python swank_input("s:swank_form")' )
        let s:swank_package = ''
        let s:refresh_disabled = 0
        call SlimvRefreshReplBuffer()
    else
        let result = system( g:slimv_client . ' -o ' . s:repl_name, text )
        if result != ''
            " Treat any output as error message
            call SlimvErrorWait( result )
        endif
    endif

    if a:open_buffer
        " Refresh REPL buffer then return to the caller buffer/window
        call SlimvRefreshReplBuffer()
        if g:slimv_repl_split && repl_win == -1
            execute "normal! \<C-w>p"
        endif
    endif
endfunction

" Eval arguments in Lisp REPL
function! SlimvEval( args )
    call SlimvSend( a:args, g:slimv_repl_open, 1 )
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
    call SlimvEndOfReplBuffer()
    set nomodified
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
        if len( a:cmd ) > 1 || len( g:slimv_cmdhistory ) == 0 || command != g:slimv_cmdhistory[-1]
            " Add command only if differs from the last one
            call add( g:slimv_cmdhistory, command )
        endif
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

" Return missing parens, double quotes, etc to properly close form
function! s:CloseForm( lines )
    let form = join( a:lines, "\n" )
    let end = ''
    let i = 0
    while i < len( form )
        if form[i] == '"'
            " Inside a string
            let end = '"' . end
            let i += 1
            while i < len( form )
                if form[i] == '\'
                    " Ignore next character
                    let i += 2
                elseif form[i] == '"'
                    let end = end[1:]
                    break
                else
                    let i += 1
                endif
            endwhile
        elseif form[i] == ';'
            " Inside a comment
            let end = "\n" . end
            let cend = match(form, "\n", i)
            if cend == -1
                break
            endif
            let i = cend
            let end = end[1:]
        else
            " We are outside of strings and comments, now we shall count parens
            if form[i] == '('
                let end = ')' . end
            elseif form[i] == '['
                let end = ']' . end
            elseif form[i] == ')' || form[i] == ']'
                if len( end ) == 0 || end[0] != form[i]
                    " Oops, too many closing parens or invalid closing paren
                    return 'ERROR'
                endif
                let end = end[1:]
            endif
        endif
        let i += 1
    endwhile
    return end
endfunction

" Return Lisp source code indentation at the given line
function! SlimvIndent( lnum )
    if a:lnum <= 1
        " Start of the file
        return 0
    endif
    let pnum = prevnonblank(a:lnum - 1)
    if pnum == 0
        " Hit the start of the file, use zero indent.
        return 0
    endif
    " Use custom indentation only if default indenting is >2
    let li = lispindent(a:lnum)
    if li > 2
        " Find start of current form
        let [l, c] = searchpairpos( '(', '', ')', 'nbW', s:skip_sc, pnum )
        " Use custom indentation only if default indenting is >2 from the opening paren in the previous line
        if l == pnum && li > c + 1
            let line = getline( l )
            let parent = strpart( line, 0, c )
            if match( parent, '\c(\s*\(flet\|labels\|macrolet\)\s*(\s*(\s*$' ) >= 0
                " Handle special indentation style for flet, labels, etc.
                return c + 1
            endif
            " Found opening paren in the previous line, let's find out the function name
            let func = matchstr( line, '\<\k*\>', c )
            if func != '' && g:slimv_swank && s:swank_connected
                let s:indent = ''
                silent execute 'python get_indent_info("' . func . '")'
                if s:indent >= '0' && s:indent <= '9'
                    " Function has &body argument, so indent by 2 spaces from the opening '('
                    return c + 1
                endif
            endif
        endif
    endif

    " Use default Lisp indening
    return li
endfunction 

" Send command line to REPL buffer
" Arguments: close = add missing closing parens
function! SlimvSendCommand( close )
    call SlimvRefreshModeOn()
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
            while l <= line("$")
                call add( cmd, strpart( getline( l ), 0) )
                let l = l + 1
            endwhile

            " Count the number of opening and closing braces
            let end = s:CloseForm( cmd )
            if end == 'ERROR'
                " Too many closing parens
                call SlimvErrorWait( "Too many or invalid closing parens found." )
                return
            endif
            let echoing = 0
            if a:close && end != ''
                " Close form if necessary and evaluate it
                let cmd[len(cmd)-1] = cmd[len(cmd)-1] . end
                let end = ''
                let echoing = 1
            endif
            if end == ''
                " Expression finished, let's evaluate it
                " but first add it to the history
                call SlimvAddHistory( cmd )
                " Evaluate, but echo only when form is actually closed here
                call SlimvSend( cmd, g:slimv_repl_open, echoing )
            else
                " Expression is not finished yet, indent properly and wait for completion
                " Indentation works only if lisp indentation is switched on
                let l = line('.') + 1
                call append( '.', '' )
                call setline( l, repeat( ' ', SlimvIndent(l) ) )
                normal! j$
            endif
        endif
    else
        call append( '$', "Slimv error: previous EOF mark not found, re-enter last form:" )
        call append( '$', "" )
        call SlimvEndOfReplBuffer()
        call SlimvMarkBufferEnd()
        set nomodified
    endif
endfunction

" Close current top level form by adding the missing parens
function! SlimvCloseForm()
    let l2 = line( '.' )
    call SlimvFindDefunStart()
    let l1 = line( '.' )
    let form = []
    let l = l1
    while l <= l2
        call add( form, getline( l ) )
        let l = l + 1
    endwhile
    let end = s:CloseForm( form )
    if end == 'ERROR'
        " Too many closing parens
        call SlimvErrorWait( "Too many or invalid closing parens found." )
    elseif end != ''
        " Add missing parens
        if end[0] == "\n"
            call append( l2, end[1:] )
        else
            call setline( l2, getline( l2 ) . end )
        endif
    endif
    normal! %
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

" Recall previous command from command history
function! s:PreviousCommand()
    if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos > 0
        let g:slimv_cmdhistorypos = g:slimv_cmdhistorypos - 1
        call SlimvRecallHistory()
    endif
endfunction

" Recall next command from command history
function! s:NextCommand()
    if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos < len( g:slimv_cmdhistory )
        let g:slimv_cmdhistorypos = g:slimv_cmdhistorypos + 1
        call SlimvRecallHistory()
    else
        call SlimvSetCommandLine( "" )
    endif
endfunction

" Handle insert mode 'Up' keypress in the REPL buffer
function! SlimvHandleUp()
    if line( "." ) >= line( "'s" )
        if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos == len( g:slimv_cmdhistory )
            call SlimvRefresh()
            call SlimvEndOfReplBuffer()
            call SlimvMarkBufferEnd()
            startinsert!
        endif
        call s:PreviousCommand()
    else
        normal! gk
    endif
endfunction

" Handle insert mode 'Down' keypress in the REPL buffer
function! SlimvHandleDown()
    if line( "." ) >= line( "'s" )
        call s:NextCommand()
    else
        normal! gj
    endif
endfunction

" Handle normal mode 'Enter' keypress in the REPL buffer
function! SlimvHandleEnter()
    let line = getline('.')
    if s:debug_activated
        " Check if Enter was pressed in a section printed by the SWANK debugger
        let item = matchstr( line, '\d\+' )
        if item != ''
            let section = getline( line('.') - item - 1 )
            if section[0:8] == 'Restarts:'
                " Apply item-th restart
                call SlimvEval( [item] )
                return
            endif
            if section[0:9] == 'Backtrace:'
                " Display item-th frame, we signal frames by prefixing with '#'
                call SlimvEval( ['#' . item] )
                return
            endif
        endif
    endif
    if line[0:9] == 'Inspecting'
        " Reload inspected item
        call SlimvEval( ['[0]'] )
        return
    endif

    if line[0] == '['
        if line[0:3] == '[<<]'
            " Pop back up in the inspector
            let item = '-1'
        else
            " Inspect n-th part
            let item = matchstr( line, '\d\+' )
        endif
        if item != ''
            call SlimvEval( ['[' . item . ']'] )
            return
        endif
    endif

    if line[0] == '<'
        " Inspector n-th action
        let item = matchstr( line, '\d\+' )
        if item != ''
            call SlimvEval( ['<' . item . '>'] )
            return
        endif
    endif

    " No special treatment, perform the original function
    execute "normal! \<CR>"
endfunction

" Go to command line and recall previous command from command history
function! SlimvPreviousCommand()
    call SlimvEndOfReplBuffer()
    if line( "." ) >= line( "'s" )
        call s:PreviousCommand()
    endif
endfunction

" Go to command line and recall next command from command history
function! SlimvNextCommand()
    call SlimvEndOfReplBuffer()
    if line( "." ) >= line( "'s" )
        call s:NextCommand()
    endif
endfunction

" Handle interrupt (Ctrl-C) keypress in the REPL buffer
function! SlimvInterrupt()
    if g:slimv_swank
        call SlimvCommand( 'python swank_interrupt()' )
    else
        call SlimvSend( ['SLIMV::INTERRUPT'], 0, 1 )
    endif
    call SlimvRefreshReplBuffer()
endfunction

" Display function argument list
function! SlimvArglist()
    let l = line('.')
    let c = col('.') - 1
    let line = getline('.')
    if SlimvGetFiletype() == 'clojure'
        setlocal iskeyword+=~,#,&,\|,{,},!,?
    else
        setlocal iskeyword+=~,#,&,\|,{,},[,],!,?
    endif
    if s:swank_connected && c > 1 && line[c-2] =~ '\k'
        " Display only if entering the first space after a keyword
        let matchb = max( [l-100, 1] )
        let [l0, c0] = searchpairpos( '(', '', ')', 'nbW', s:skip_sc, matchb )
        if l0 > 0
            " Found opening paren, let's find out the function name
            let arg = matchstr( line, '\<\k*\>', c0 )
            if arg != ''
                " Ask function argument list from SWANK
                let msg = SlimvCommandGetResponse( ':operator-arglist', 'python swank_op_arglist("' . arg . '")' )
                if msg != ''
                    " Print argument list in status line with newlines removed.
                    " Disable showmode until the next ESC to prevent
                    " immeditate overwriting by the "-- INSERT --" text.
                    let s:save_showmode = &showmode
                    set noshowmode
                    let msg = substitute( msg, "\n", "", "g" )
                    if match( msg, arg ) != 1
                        " Function name is not received from REPL
                        echo "\r(" . arg . ' ' . msg[1:]
                    else
                        echo "\r" . msg
                    endif
                endif
            endif
        endif
    endif

    " Return empty string because this function is called from an insert mode mapping
    return ''
endfunction

" Start and connect slimv server
" This is a quite dummy function that just evaluates the empty string
function! SlimvConnectServer()
    if g:slimv_swank
        let repl_buf = bufnr( g:slimv_repl_file )
        let repl_win = bufwinnr( repl_buf )
        if g:slimv_repl_open && ( repl_buf == -1 || ( g:slimv_repl_split && repl_win == -1 ) )
            call SlimvOpenReplBuffer()
        endif 
        if s:swank_connected
            python swank_disconnect()
            let s:swank_connected = 0
        endif 
        call SlimvConnectSwank()
    endif
    if !g:slimv_swank
        call SlimvSend( ['SLIMV::OUTPUT::' . s:repl_name ], g:slimv_repl_open, 1 )
    endif
endfunction

" Refresh REPL buffer continuously
function! SlimvRefresh()
    if bufnr( g:slimv_repl_file ) == -1 || g:slimv_swank
        " REPL not opened, no need to refresh
        return
    endif
    if bufnr( g:slimv_repl_file ) != bufnr( "%" )
        " REPL is not the current window, activate it
        call SlimvOpenReplBuffer()
    else
        try
            execute "silent view! " . s:repl_name
            let s:last_size = getfsize( s:repl_name )
            let s:last_update = localtime()
        catch /.*/
            " Oops, something went wrong, the buffer will not be refreshed this time
        endtry
    endif
endfunction

" Get the last region (visual block)
function! SlimvGetRegion() range
    let oldpos = getpos( '.' ) 
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

    " Find and add package/namespace definition in front of the region
    if g:slimv_package
        call setreg( '"s', '' )
        call SlimvFindPackage()
        let sel = SlimvGetSelection()
        if sel != ''
            let lines = [sel] + lines
        endif
    endif
    call setpos( '.', oldpos ) 
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
    let oldpos = getpos( '.' ) 
    if !SlimvSelectDefun()
        return
    endif
    call SlimvFindPackage()
    call setpos( '.', oldpos ) 
    call SlimvEvalSelection()
endfunction

" Evaluate the whole buffer
function! SlimvEvalBuffer()
    let lines = getline( 1, '$' )
    call SlimvEval( lines )
endfunction

" Evaluate current s-expression at the cursor pos
function! SlimvEvalExp()
    let oldpos = getpos( '.' ) 
    if !SlimvSelectForm()
        return
    endif
    call SlimvFindPackage()
    call setpos( '.', oldpos ) 
    call SlimvEvalSelection()
endfunction

" Evaluate and pretty print current s-expression
function! SlimvPprintEvalExp()
    let oldpos = getpos( '.' ) 
    if !SlimvSelectForm()
        return
    endif
    call SlimvFindPackage()
    call setpos( '.', oldpos ) 
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
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommand( 'python swank_undefine_function("' . SlimvSelectSymbol() . '")' )
            call SlimvRefreshReplBuffer()
        endif
    else
        call SlimvEvalForm1( g:slimv_template_undefine, SlimvSelectSymbol() )
    endif
endfunction

" ---------------------------------------------------------------------

" General part of the various macroexpand functions
function! SlimvMacroexpandGeneral( command )
    call SlimvFindDefunStart()
    let line = getline( "." )
    if match( line, '(\s*defmacro\s' ) < 0
        " The form does not contain 'defmacro', put it in a macroexpand block
        if !SlimvSelectForm()
            return
        endif
        let m = "(" . a:command . " '" . SlimvGetSelection() . ")"
    else
        " The form is a 'defmacro', so do a macroexpand from the macro name and parameters
        if SlimvGetFiletype() == 'clojure'
            " Some Vim configs (e.g. matchit.vim) include the trailing ']' after '%' in Visual mode
            silent normal! vt[%ht]"sy
        else
            silent normal! vt(])"sy
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
    if g:slimv_swank
        if s:swank_connected
            if !SlimvSelectForm()
                return
            endif
            let s:swank_form = SlimvGetSelection()
            call SlimvCommandUsePackage( 'python swank_macroexpand("s:swank_form")' )
        endif
    else
        let oldpos = getpos( '.' ) 
        let m = SlimvMacroexpandGeneral( "macroexpand-1" )
        call SlimvEvalForm1( g:slimv_template_macroexpand, m )
        call setpos( '.', oldpos ) 
    endif
endfunction

" Macroexpand the current top level form
function! SlimvMacroexpandAll()
    if g:slimv_swank
        if s:swank_connected
            if !SlimvSelectForm()
                return
            endif
            let s:swank_form = SlimvGetSelection()
            call SlimvCommandUsePackage( 'python swank_macroexpand_all("s:swank_form")' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        let oldpos = getpos( '.' ) 
        let m = SlimvMacroexpandGeneral( "macroexpand" )
        call SlimvEvalForm1( g:slimv_template_macroexpand_all, m )
        call setpos( '.', oldpos ) 
    endif
endfunction

" Switch trace on for the selected function (toggle for swank)
function! SlimvTrace()
    if g:slimv_swank
        if s:swank_connected
            let s = input( '(Un)trace: ', SlimvSelectSymbol() )
            if s != ''
                call SlimvCommandUsePackage( 'python swank_toggle_trace("' . s . '")' )
                redraw!
            endif
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        let s = input( 'Trace: ', SlimvSelectSymbol() )
        echo s
        if s != ''
            call SlimvEvalForm1( g:slimv_template_trace, s )
        endif
    endif
endfunction

" Switch trace off for the selected function (or all functions for swank)
function! SlimvUntrace()
    if g:slimv_swank
        if s:swank_connected
            let s:refresh_disabled = 1
            call SlimvCommand( 'python swank_untrace_all()' )
            let s:refresh_disabled = 0
            call SlimvRefreshReplBuffer()
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        let s = input( 'Untrace: ', SlimvSelectSymbol() )
        if s != ''
            call SlimvEvalForm1( g:slimv_template_untrace, s )
        endif
    endif
endfunction

" Disassemble the selected function
function! SlimvDisassemble()
    let s = input( 'Disassemble: ', SlimvSelectSymbol() )
    if s != ''
        if g:slimv_swank
            if s:swank_connected
                call SlimvCommandUsePackage( 'python swank_disassemble("' . s . '")' )
            else
                call SlimvError( "Not connected to SWANK server." )
            endif
        else
            call SlimvEvalForm1( g:slimv_template_disassemble, s )
        endif
    endif
endfunction

" Inspect symbol under cursor
function! SlimvInspect()
    let s = input( 'Inspect: ', SlimvSelectSymbolExt() )
    if s != ''
        if g:slimv_swank
            if s:swank_connected
                call SlimvCommandUsePackage( 'python swank_inspect("' . s . '")' )
            else
                call SlimvError( "Not connected to SWANK server." )
            endif
        else
            call SlimvEvalForm1( g:slimv_template_inspect, s )
        endif
    endif
endfunction

" Cross reference: who calls
function! SlimvXrefBase( text, cmd )
    if g:slimv_swank
        if s:swank_connected
            let s = input( a:text, SlimvSelectSymbol() )
            if s != ''
                call SlimvCommandUsePackage( 'python swank_xref("' . s . '", "' . a:cmd . '")' )
            endif
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        call SlimvError( "SWANK is switched off." )
    endif
endfunction

" Cross reference: who calls
function! SlimvXrefCalls()
    call SlimvXrefBase( 'Who calls: ', ':calls' )
endfunction

" Cross reference: who references
function! SlimvXrefReferences()
    call SlimvXrefBase( 'Who references: ', ':references' )
endfunction

" Cross reference: who sets
function! SlimvXrefSets()
    call SlimvXrefBase( 'Who sets: ', ':sets' )
endfunction

" Cross reference: who binds
function! SlimvXrefBinds()
    call SlimvXrefBase( 'Who binds: ', ':binds' )
endfunction

" Cross reference: who macroexpands
function! SlimvXrefMacroexpands()
    call SlimvXrefBase( 'Who macroexpands: ', ':macroexpands' )
endfunction

" Cross reference: who specializes
function! SlimvXrefSpecializes()
    call SlimvXrefBase( 'Who specializes: ', ':specializes' )
endfunction

" Cross reference: list callers
function! SlimvXrefCallers()
    call SlimvXrefBase( 'List callers: ', ':callers' )
endfunction

" Cross reference: list callees
function! SlimvXrefCallees()
    call SlimvXrefBase( 'List callees: ', ':callees' )
endfunction

" ---------------------------------------------------------------------

" Compile and load profiler
function! SlimvLoadProfiler()
    if SlimvGetFiletype() == 'clojure'
        call SlimvError( "No profiler support for Clojure." )
    elseif b:SlimvImplementation() == 'sbcl'
        call SlimvError( "SBCL has a built-in profiler, no need to load it." )
    else
        let profiler = split( globpath( &runtimepath, 'slime/metering.lisp'), '\n' )
        if len( profiler ) == 0
            let profiler = split( globpath( &runtimepath, 'ftplugin/**/metering.lisp'), '\n' )
        endif
        if len( profiler ) > 0
            let filename = profiler[0]
            let filename = substitute( filename, '\\', '/', 'g' )
            call SlimvEvalForm2( g:slimv_template_compile_file, filename, 'T' )
        else
            call SlimvError( "metering.lisp is not found in the Vim ftplugin directory or below." )
        endif
    endif
endfunction

" Switch or toggle profiling on for the selected function
function! SlimvProfile()
    if g:slimv_swank
        if s:swank_connected
            let s = input( '(Un)profile: ', SlimvSelectSymbol() )
            if s != ''
                call SlimvCommandUsePackage( 'python swank_toggle_profile("' . s . '")' )
                redraw!
            endif
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        if SlimvGetFiletype() == 'clojure'
            call SlimvError( "No profiler support for Clojure." )
        else
            let s = input( 'Profile: ', SlimvSelectSymbol() )
            if s != ''
                call SlimvEvalForm1( g:slimv_template_profile, s )
            endif
        endif
    endif
endfunction

" Switch profiling on based on substring
function! SlimvProfileSubstring()
    if s:swank_connected
        let s = input( 'Profile by matching substring: ', SlimvSelectSymbol() )
        if s != ''
            let p = input( 'Package (RET for all packages): ' )
            call SlimvCommandUsePackage( 'python swank_profile_substring("' . s . '","' . p . '")' )
            redraw!
        endif
    else
        call SlimvError( "Not connected to SWANK server." )
    endif
endfunction

" Switch profiling off for the selected function
function! SlimvUnprofile()
    if SlimvGetFiletype() == 'clojure'
        call SlimvError( "No profiler support for Clojure." )
    else
        let s = input( 'Unprofile: ', SlimvSelectSymbol() )
        if s != ''
            call SlimvEvalForm1( g:slimv_template_unprofile, s )
        endif
    endif
endfunction

" Switch profiling completely off
function! SlimvUnprofileAll()
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommandUsePackage( 'python swank_unprofile_all()' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        if SlimvGetFiletype() == 'clojure'
            call SlimvError( "No profiler support for Clojure." )
        else
            call SlimvEvalForm( g:slimv_template_unprofile_all )
        endif
    endif
endfunction

" Display list of profiled functions
function! SlimvShowProfiled()
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommandUsePackage( 'python swank_profiled_functions()' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        if SlimvGetFiletype() == 'clojure'
            call SlimvError( "No profiler support for Clojure." )
        else
            call SlimvEvalForm( g:slimv_template_show_profiled )
        endif
    endif
endfunction

" Report profiling results
function! SlimvProfileReport()
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommandUsePackage( 'python swank_profile_report()' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        if SlimvGetFiletype() == 'clojure'
            call SlimvError( "No profiler support for Clojure." )
        else
            call SlimvEvalForm( g:slimv_template_profile_report )
        endif
    endif
endfunction

" Reset profiling counters
function! SlimvProfileReset()
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommandUsePackage( 'python swank_profile_reset()' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        if SlimvGetFiletype() == 'clojure'
            call SlimvError( "No profiler support for Clojure." )
        else
            call SlimvEvalForm( g:slimv_template_profile_reset )
        endif
    endif
endfunction

" ---------------------------------------------------------------------

" Compile the current top-level form
function! SlimvCompileDefun()
    let oldpos = getpos( '.' ) 
    if !SlimvSelectDefun()
        call setpos( '.', oldpos ) 
        return
    endif
    if g:slimv_swank
        if s:swank_connected
            let s:swank_form = SlimvGetSelection()
            call SlimvCommandUsePackage( 'python swank_compile_string("s:swank_form")' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        call SlimvFindPackage()
        let form = SlimvGetSelection()
        let form = substitute( form, '"', '\\\\"', 'g' )
        call SlimvEvalForm1( g:slimv_template_compile_string, form )
        call setpos( '.', oldpos ) 
    endif
endfunction

" Compile and load whole file
function! SlimvCompileLoadFile()
    let filename = fnamemodify( bufname(''), ':p' )
    let filename = substitute( filename, '\\', '/', 'g' )
    if &modified
        let answer = SlimvErrorAsk( '', "Save file before compiling [Y/n]?" )
        if answer[0] != 'n' && answer[0] != 'N'
            write
        endif
    endif
    if g:slimv_swank
        if s:swank_connected
            let s:compiled_file = ''
            call SlimvCommandUsePackage( 'python swank_compile_file("' . filename . '")' )
            let starttime = localtime()
            while s:compiled_file == '' && localtime()-starttime < g:slimv_timeout
                call SlimvSwankResponse()
            endwhile
            if s:compiled_file != ''
                call SlimvCommandUsePackage( 'python swank_load_file("' . s:compiled_file . '")' )
                let s:compiled_file = ''
            endif
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        call SlimvEvalForm2( g:slimv_template_compile_file, filename, 'T' )
    endif
endfunction

" Compile whole file
function! SlimvCompileFile()
    let filename = fnamemodify( bufname(''), ':p' )
    let filename = substitute( filename, '\\', '/', 'g' )
    if &modified
        let answer = SlimvErrorAsk( '', "Save file before compiling [Y/n]?" )
        if answer[0] != 'n' && answer[0] != 'N'
            write
        endif
    endif
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommandUsePackage( 'python swank_compile_file("' . filename . '")' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        call SlimvEvalForm2( g:slimv_template_compile_file, filename, 'NIL' )
    endif
endfunction

function! SlimvCompileRegion() range
    let oldpos = getpos( '.' ) 
    let lines = SlimvGetRegion()
    let region = join( lines, "\n" )
    if g:slimv_swank
        if s:swank_connected
            let s:swank_form = region
            call SlimvCommandUsePackage( 'python swank_compile_string("s:swank_form")' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        call SlimvFindPackage()
        let region = substitute( region, '"', '\\\\"', 'g' )
        call SlimvEvalForm1( g:slimv_template_compile_string, region )
        call setpos( '.', oldpos ) 
    endif
endfunction

" ---------------------------------------------------------------------

" Describe the selected symbol
function! SlimvDescribeSymbol()
    if g:slimv_swank
        if s:swank_connected
            call SlimvCommandUsePackage( 'python swank_describe_symbol("' . SlimvSelectSymbol() . '")' )
        else
            call SlimvError( "Not connected to SWANK server." )
        endif
    else
        call SlimvEvalForm1( g:slimv_template_describe, SlimvSelectSymbol() )
    endif
endfunction

" Display symbol description in balloonexpr
function! SlimvDescribe(arg)
    let arg=a:arg
    if a:arg == ''
        let arg = expand('<cword>')
    endif
    if !s:swank_connected
        return ''
    endif
    let arglist = SlimvCommandGetResponse( ':operator-arglist', 'python swank_op_arglist("' . arg . '")' )
    if arglist == ''
        " Not able to fetch arglist, assuming function is not defined
        " Skip calling describe, otherwise SWANK goes into the debugger
        return ''
    endif
    let msg = SlimvCommandGetResponse( ':describe-function', 'python swank_describe_function("' . arg . '")' )
    if msg == ''
        " No describe info, display arglist
        if match( arglist, arg ) != 1
            " Function name is not received from REPL
            return "(" . arg . ' ' . arglist[1:]
        else
            return arglist
        endif
    else
        return msg
    endif
endfunction

" Setup balloonexp to display symbol description
if g:slimv_swank && g:slimv_balloon && has( 'balloon_eval' )
    "setlocal balloondelay=100
    setlocal ballooneval
    setlocal balloonexpr=SlimvDescribe(v:beval_text)
endif

" Apropos of the selected symbol
function! SlimvApropos()
    call SlimvEvalForm1( g:slimv_template_apropos, SlimvSelectSymbol() )
endfunction

" Generate tags file using ctags
function! SlimvGenerateTags()
    if exists( 'g:slimv_ctags' ) && g:slimv_ctags != ''
        execute 'silent !' . g:slimv_ctags
    else
        call SlimvError( "Copy ctags to the Vim path or define g:slimv_ctags." )
    endif
endfunction

" ---------------------------------------------------------------------

" Find word in the CLHS symbol database, with exact or partial match.
" Return either the first symbol found with the associated URL,
" or the list of all symbols found without the associated URL.
function! SlimvFindSymbol( word, exact, all, db, root, init )
    if a:word == ''
        return []
    endif
    if !a:all && a:init != []
        " Found something already at a previous db lookup, no need to search this db
        return a:init
    endif
    let lst = a:init
    let i = 0
    let w = tolower( a:word )
    if a:exact
        while i < len( a:db )
            " Try to find an exact match
            if a:db[i][0] == w
                " No reason to check a:all here
                return [a:db[i][0], a:root . a:db[i][1]]
            endif
            let i = i + 1
        endwhile
    else
        while i < len( a:db )
            " Try to find the symbol starting with the given word
            let w2 = escape( w, '~' )
            if match( a:db[i][0], w2 ) == 0
                if a:all
                    call add( lst, a:db[i][0] )
                else
                    return [a:db[i][0], a:root . a:db[i][1]]
                endif
            endif
            let i = i + 1
        endwhile
    endif

    " Return whatever found so far
    return lst
endfunction

" Lookup word in Common Lisp Hyperspec
function! SlimvLookup( word )
    " First try an exact match
    let w = a:word
    let symbol = []
    while symbol == []
        let symbol = b:SlimvHyperspecLookup( w, 1, 0 )
        if symbol == []
            " Symbol not found, try a match on beginning of symbol name
            let symbol = b:SlimvHyperspecLookup( w, 0, 0 )
            if symbol == []
                " We are out of luck, can't find anything
                let msg = 'Symbol ' . w . ' not found. Hyperspec lookup word: '
                let val = ''
            else
                let msg = 'Hyperspec lookup word: '
                let val = symbol[0]
            endif
            " Ask user if this is that he/she meant
            let w = input( msg, val )
            if w == ''
                " OK, user does not want to continue
                return
            endif
            let symbol = []
        endif
    endwhile
    if symbol != []
        " Symbol found, open HS page in browser
        if match( symbol[1], ':' ) < 0 && exists( g:slimv_hs_root )
            let page = g:slimv_hs_root . symbol[1]
        else
            " URL is already a fully qualified address
            let page = symbol[1]
        endif
        if exists( "g:slimv_browser_cmd" )
            " We have an given command to start the browser
            silent execute '! ' . g:slimv_browser_cmd . ' ' . page
        else
            if g:slimv_windows
                " Run the program associated with the .html extension
                silent execute '! start ' . page
            else
                " On Linux it's not easy to determine the default browser
                " Ask help from Python webbrowser package
                let pycmd = "import webbrowser; webbrowser.open('" . page . "')"
                silent execute '! ' . g:slimv_python . ' -c "' . pycmd . '"'
            endif
        endif
        " This is needed especially when using text browsers
        redraw!
    endif
endfunction

" Lookup current symbol in the Common Lisp Hyperspec
function! SlimvHyperspec()
    call SlimvLookup( SlimvSelectSymbol() )
endfunction

" Complete symbol name starting with 'base'
function! SlimvComplete( base )
    " Find all symbols starting with "a:base"
    if g:slimv_swank && s:swank_connected
        if g:slimv_simple_compl
            let msg = SlimvCommandGetResponse( ':simple-completions', 'python swank_completions("' . a:base . '")' )
        else
            let msg = SlimvCommandGetResponse( ':fuzzy-completions', 'python swank_fuzzy_completions("' . a:base . '")' )
        endif
        if msg != ''
            " We have a completion list from SWANK
            let res = split( msg, '\n' )
            return res
        endif
    endif

    " No completion yet, try to fetch it from the Hyperspec database
    let res = []
    let symbol = b:SlimvHyperspecLookup( a:base, 0, 1 )
    call sort( symbol )
    for m in symbol
        if m =~ '^' . a:base
            call add( res, m )
        endif
    endfor
    return res
endfunction

" Complete function that uses the Hyperspec database
function! SlimvOmniComplete( findstart, base )
    if a:findstart
        " Locate the start of the symbol name
        if SlimvGetFiletype() == 'clojure'
            setlocal iskeyword+=~,#,&,\|,{,},!,?
        else
            setlocal iskeyword+=~,#,&,\|,{,},[,],!,?
        endif
        let upto = strpart( getline( '.' ), 0, col( '.' ) - 1)
        let p = match(upto, '\k\+$')
        return p 
    else
        return SlimvComplete( a:base )
    endif
endfunction

" Define complete function only if none is defined yet
if &omnifunc == ''
    set omnifunc=SlimvOmniComplete
endif

" Complete function for user-defined commands
function! SlimvCommandComplete( arglead, cmdline, cursorpos )
    " Locate the start of the symbol name
    if SlimvGetFiletype() == 'clojure'
        setlocal iskeyword+=~,#,&,\|,{,},!,?
    else
        setlocal iskeyword+=~,#,&,\|,{,},[,],!,?
    endif
    let upto = strpart( a:cmdline, 0, a:cursorpos )
    let base = matchstr(upto, '\k\+$')
    let ext  = matchstr(upto, '\S*\k\+$')
    let compl = SlimvComplete( base )
    if len(compl) > 0 && base != ext
        " Command completion replaces whole word between spaces, so we
        " need to add any prefix present in front of the keyword, like '('
        let prefix = strpart( ext, 0, len(ext) - len(base) )
        let i = 0
        while i < len(compl)
            let compl[i] = prefix . compl[i]
            let i = i + 1
        endwhile
    endif
    return compl
endfunction

" Set current package
function! SlimvSetPackage()
    if s:swank_connected
        let oldpos = getpos( '.' )
        call SlimvFindPackage()
        call setpos( '.', oldpos )
        let pkg = input( 'Package: ', s:swank_package )
        if pkg != ''
            let s:refresh_disabled = 1
            call SlimvCommand( 'python swank_set_package("' . pkg . '")' )
            let s:refresh_disabled = 0
            call SlimvRefreshReplBuffer()
        endif
    else
        call SlimvError( "Not connected to SWANK server." )
    endif
endfunction

" =====================================================================
"  Slimv keybindings
" =====================================================================

" <Leader> timeouts in 1000 msec by default, if this is too short,
" then increase 'timeoutlen'

" Map keyboard keyset dependant shortcut to command and also add it to menu
function! s:MenuMap( name, shortcut1, shortcut2, command )
    if g:slimv_keybindings == 1
        " Short (one-key) keybinding set
        let shortcut = a:shortcut1
    elseif g:slimv_keybindings == 2
        " Easy to remember (two-key) keybinding set
        let shortcut = a:shortcut2
    endif

    if shortcut != ''
        execute "noremap <silent> " . shortcut . " " . a:command
        if a:name != '' && g:slimv_menu == 1
            silent execute "amenu " . a:name . "<Tab>" . shortcut . " " . a:command
        endif
    elseif a:name != '' && g:slimv_menu == 1
        silent execute "amenu " . a:name . " " . a:command
    endif
endfunction

if g:slimv_swank
    " Map space to display function argument list in status line
    inoremap <silent> <Space>    <Space><C-R>=SlimvArglist()<CR>
    "noremap  <silent> <C-C>      :call SlimvInterrupt()<CR>
    au InsertLeave * :let &showmode=s:save_showmode
endif

" Edit commands
inoremap <silent> <C-X>0     <C-O>:call SlimvCloseForm()<CR>
inoremap <silent> <Tab>      <C-R>=pumvisible() ? "\<lt>C-N>" : "\<lt>C-X>\<lt>C-O>"<CR>
call s:MenuMap( 'Slim&v.Edi&t.Close-&Form',                     g:slimv_leader.')',  g:slimv_leader.'tc',  ':<C-U>call SlimvCloseForm()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.&Complete-Symbol<Tab>Tab',        '',                  '',                   '<Ins><C-X><C-O>' )
call s:MenuMap( 'Slim&v.Edi&t.&Paredit-Toggle',                 g:slimv_leader.'(',  g:slimv_leader.'(t',  ':<C-U>call PareditToggle()<CR>' )

" Evaluation commands
call s:MenuMap( 'Slim&v.&Evaluation.Eval-&Defun',               g:slimv_leader.'d',  g:slimv_leader.'ed',  ':<C-U>call SlimvEvalDefun()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Eval-Current-&Exp',         g:slimv_leader.'e',  g:slimv_leader.'ee',  ':<C-U>call SlimvEvalExp()<CR>' )
if !g:slimv_swank
call s:MenuMap( 'Slim&v.&Evaluation.&Pprint-Eval-Exp',          g:slimv_leader.'E',  g:slimv_leader.'ep',  ':<C-U>call SlimvPprintEvalExp()<CR>' )
endif
call s:MenuMap( 'Slim&v.&Evaluation.Eval-&Region',              g:slimv_leader.'r',  g:slimv_leader.'er',  ':call SlimvEvalRegion()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Eval-&Buffer',              g:slimv_leader.'b',  g:slimv_leader.'eb',  ':<C-U>call SlimvEvalBuffer()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Interacti&ve-Eval\.\.\.',   g:slimv_leader.'v',  g:slimv_leader.'ei',  ':call SlimvInteractiveEval()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.&Undefine-Function',        g:slimv_leader.'u',  g:slimv_leader.'eu',  ':call SlimvUndefineFunction()<CR>' )

" Debug commands
call s:MenuMap( 'Slim&v.De&bugging.Macroexpand-&1',             g:slimv_leader.'1',  g:slimv_leader.'m1',  ':<C-U>call SlimvMacroexpand()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Macroexpand-All',           g:slimv_leader.'m',  g:slimv_leader.'ma',  ':<C-U>call SlimvMacroexpandAll()<CR>' )

if g:slimv_swank
call s:MenuMap( 'Slim&v.De&bugging.Toggle-&Trace\.\.\.',        g:slimv_leader.'t',  g:slimv_leader.'dt',  ':call SlimvTrace()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.U&ntrace-All',               g:slimv_leader.'T',  g:slimv_leader.'du',  ':call SlimvUntrace()<CR>' )
else
call s:MenuMap( 'Slim&v.De&bugging.&Trace\.\.\.',               g:slimv_leader.'t',  g:slimv_leader.'dt',  ':call SlimvTrace()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.U&ntrace\.\.\.',             g:slimv_leader.'T',  g:slimv_leader.'du',  ':call SlimvUntrace()<CR>' )
endif

call s:MenuMap( 'Slim&v.De&bugging.Disassemb&le\.\.\.',         g:slimv_leader.'l',  g:slimv_leader.'dd',  ':call SlimvDisassemble()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Inspect\.\.\.',             g:slimv_leader.'i',  g:slimv_leader.'di',  ':call SlimvInspect()<CR>' )

" Compile commands
call s:MenuMap( 'Slim&v.&Compilation.Compile-&Defun',           g:slimv_leader.'D',  g:slimv_leader.'cd',  ':<C-U>call SlimvCompileDefun()<CR>' )
call s:MenuMap( 'Slim&v.&Compilation.Compile-&Load-File',       g:slimv_leader.'L',  g:slimv_leader.'cl',  ':<C-U>call SlimvCompileLoadFile()<CR>' )
call s:MenuMap( 'Slim&v.&Compilation.Compile-&File',            g:slimv_leader.'F',  g:slimv_leader.'cf',  ':<C-U>call SlimvCompileFile()<CR>' )
call s:MenuMap( 'Slim&v.&Compilation.Compile-&Region',          g:slimv_leader.'R',  g:slimv_leader.'cr',  ':call SlimvCompileRegion()<CR>' )

" Xref commands
call s:MenuMap( 'Slim&v.&Xref.Who-&Calls',                      g:slimv_leader.'xc', g:slimv_leader.'xc',  ':call SlimvXrefCalls()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&References',                 g:slimv_leader.'xr', g:slimv_leader.'xr',  ':call SlimvXrefReferences()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&Sets',                       g:slimv_leader.'xs', g:slimv_leader.'xs',  ':call SlimvXrefSets()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&Binds',                      g:slimv_leader.'xb', g:slimv_leader.'xb',  ':call SlimvXrefBinds()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&Macroexpands',               g:slimv_leader.'xm', g:slimv_leader.'xm',  ':call SlimvXrefMacroexpands()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-S&pecializes',                g:slimv_leader.'xp', g:slimv_leader.'xp',  ':call SlimvXrefSpecializes()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.&List-Callers',                   g:slimv_leader.'xl', g:slimv_leader.'xl',  ':call SlimvXrefCallers()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.List-Call&ees',                   g:slimv_leader.'xe', g:slimv_leader.'xe',  ':call SlimvXrefCallees()<CR>' )

" Profile commands
if g:slimv_swank
call s:MenuMap( 'Slim&v.&Profiling.Toggle-&Profile\.\.\.',      g:slimv_leader.'p',  g:slimv_leader.'pp',  ':<C-U>call SlimvProfile()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.Profile-&By-Substring\.\.\.',g:slimv_leader.'B',  g:slimv_leader.'pb',  ':<C-U>call SlimvProfileSubstring()<CR>' )
else
call s:MenuMap( 'Slim&v.&Profiling.&Load-Profiler',             g:slimv_leader.'O',  g:slimv_leader.'pl',  ':<C-U>call SlimvLoadProfiler()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.&Profile\.\.\.',             g:slimv_leader.'p',  g:slimv_leader.'pp',  ':<C-U>call SlimvProfile()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.&Unprofile\.\.\.',           g:slimv_leader.'P',  g:slimv_leader.'pu',  ':<C-U>call SlimvUnprofile()<CR>' )
endif
call s:MenuMap( 'Slim&v.&Profiling.Unprofile-&All',             g:slimv_leader.'U',  g:slimv_leader.'pa',  ':<C-U>call SlimvUnprofileAll()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.&Show-Profiled',             g:slimv_leader.'?',  g:slimv_leader.'ps',  ':<C-U>call SlimvShowProfiled()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.-ProfilingSep-',             '',                  '',                   ':' )
call s:MenuMap( 'Slim&v.&Profiling.Profile-Rep&ort',            g:slimv_leader.'o',  g:slimv_leader.'pr',  ':<C-U>call SlimvProfileReport()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.Profile-&Reset',             g:slimv_leader.'X',  g:slimv_leader.'px',  ':<C-U>call SlimvProfileReset()<CR>' )

" Documentation commands
call s:MenuMap( 'Slim&v.&Documentation.Describe-&Symbol',       g:slimv_leader.'s',  g:slimv_leader.'ds',  ':call SlimvDescribeSymbol()<CR>' )
call s:MenuMap( 'Slim&v.&Documentation.&Apropos',               g:slimv_leader.'a',  g:slimv_leader.'da',  ':call SlimvApropos()<CR>' )
call s:MenuMap( 'Slim&v.&Documentation.&Hyperspec',             g:slimv_leader.'h',  g:slimv_leader.'dh',  ':call SlimvHyperspec()<CR>' )
call s:MenuMap( 'Slim&v.&Documentation.Generate-&Tags',         g:slimv_leader.']',  g:slimv_leader.'dg',  ':call SlimvGenerateTags()<CR>' )

" REPL commands
call s:MenuMap( 'Slim&v.&Repl.&Connect-Server',                 g:slimv_leader.'c',  g:slimv_leader.'rc',  ':call SlimvConnectServer()<CR>' )
if g:slimv_swank
call s:MenuMap( '',                                             g:slimv_leader.'g',  g:slimv_leader.'rp',  ':call SlimvSetPackage()<CR>' )
endif
call s:MenuMap( 'Slim&v.&Repl.Interrup&t-Lisp-Process',         g:slimv_leader.'y',  g:slimv_leader.'ri',  ':call SlimvInterrupt()<CR>' )


" =====================================================================
"  Slimv menu
" =====================================================================

if g:slimv_menu == 1
    " Works only if 'wildcharm' is <Tab>
    if &wildcharm == 0
        set wildcharm=<Tab>
    endif
    if &wildcharm != 0
        execute ':map ' . g:slimv_leader.', :emenu Slimv.' . nr2char( &wildcharm )
    endif
endif

" Add REPL menu. This menu exist only for the REPL buffer.
function SlimvAddReplMenu()
    if &wildcharm != 0
        execute ':map ' . g:slimv_leader.'\ :emenu REPL.' . nr2char( &wildcharm )
    endif

    amenu &REPL.Send-&Input                            :call SlimvSendCommand(0)<CR>
    amenu &REPL.Cl&ose-Send-Input                      :call SlimvSendCommand(1)<CR>
    amenu &REPL.Set-Packa&ge                           :call SlimvSetPackage()<CR>
    amenu &REPL.Interrup&t-Lisp-Process                <Esc>:<C-U>call SlimvInterrupt()<CR>
    amenu &REPL.-REPLSep-                              :
    amenu &REPL.&Previous-Input                        :call SlimvPreviousCommand()<CR>
    amenu &REPL.&Next-Input                            :call SlimvNextCommand()<CR>
    amenu &REPL.&Refresh                               :call SlimvRefresh()<CR>
endfunction

" =====================================================================
"  Slimv commands
" =====================================================================

command! -complete=customlist,SlimvCommandComplete -nargs=* Lisp call SlimvEval([<q-args>])
command! -complete=customlist,SlimvCommandComplete -nargs=* Eval call SlimvEval([<q-args>])

" Switch on syntax highlighting
syntax on

