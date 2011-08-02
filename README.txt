--------------------------------------------------------------------------------
slimv.vim
--------------------------------------------------------------------------------
Superior Lisp Interaction Mode for Vim ("SLIME for Vim")

Vim script

created by
Tamas Kovacs
 
--------------------------------------------------------------------------------
Description
--------------------------------------------------------------------------------
Slimv is a SWANK client for Vim, similarly to SLIME for Emacs. SWANK is a TCP server for Emacs, which runs a Common Lisp, Clojure or Scheme REPL and provides a socket interface for evaluating, compiling, debugging, profiling lisp code. The SWANK server is embedded in Slimv, but you can also use your own SWANK installation.

Slimv opens the lisp REPL (Read-Eval-Print Loop) inside a Vim buffer. Lisp commands may be entered and executed in the REPL buffer, just as in a regular REPL.

Slimv supports SLIME's debugger, profiler, cross reference, arglist, indentation, symbol name completion functions. The script also has a Common Lisp Hyperspec lookup feature and it is able to lookup symbols in the Clojure API, as well as in JavaDoc.

Slimv comes with Paredit Mode, which is similar to the functionality of paredit.el in Emacs. Paredit Mode tries to maintain the balanced state of matched characters (parenthesis marks, square brackets, double quotes). Matched characters are inserted and removed in pairs, also when working with a block of text (well, mostly). Slimv also implements many paredit.el s-expression handling functions, like Split/Join/Wrap/Splice. Slurpage and Barfage known from Emacs is also possible but in a different fashion: you don't move the list element in or out of the list, rather you move the opening or closing parenthesis over the element or sub-list.

Please visit the Slimv Tutorial for a more complete introduction:
http://kovisoft.bitbucket.org/tutorial.html

Here follows a list of Slimv commands, any similarity with SLIME's menu is not coincidental. :)

Edit commands:
    *  Close Form
    *  Complete Symbol
    *  Function Arglist
    *  Paredit Toggle

Evaluation commands:
    *  Eval Defun
    *  Eval Current Expression
    *  Eval Region
    *  Eval Buffer
    *  Interactive Eval
    *  Undefine Function

Debug commands:
    *  Macroexpand-1
    *  Macroexpand All
    *  Toggle Trace
    *  Untrace All
    *  Disassemble
    *  Inspect
    *  Abort
    *  Quit to Toplevel
    *  Continue
    *  List Threads
    *  Kill Thread
    *  Debug Thread

Compile commands:
    *  Compile Defun
    *  Compile and Load File
    *  Compile File
    *  Compile Region

Cross Reference commands
    *  Who Calls
    *  Who References
    *  Who Sets
    *  Who Binds
    *  Who Macroexpands
    *  Who Specializes
    *  List Callers
    *  List Callees

Profile commands:
    *  Toggle Profile
    *  Profile by Substring
    *  Unprofile All
    *  Show Profiled
    *  Profile Report
    *  Profile Reset

Documentation commands:
    *  Describe Symbol
    *  Apropos
    *  Hyperspec
    *  Generate Tags

REPL commands:
    *  Connect to Server
    *  Interrupt Lisp Process
    *  Send Input
    *  Close and Send Input
    *  Set Package
    *  Previous Input
    *  Next Input

For more information see the included documentation.
 
---------------------------------------------------------------------------------------------
Installation details
---------------------------------------------------------------------------------------------

Extract the zip archive into your vimfiles or runtime directory.

Slimv works on Windows, Linux and Mac OS X (via Terminal.app), Cygwin is supported. The script requires the following programs installed on your system:
    *  Vim with Python feature enabled
    *  Python (must be the same Python version that was Vim compiled against)
    *  Lisp (any Common Lisp with SLIME support) or Clojure or MIT Scheme (Linux only)

Vim's Python version can be identified with the :ver command, look for the -DDYNAMIC_PYTHON_DLL=\"pythonXX\" string.

Slimv tries to autodetect your Lisp/Clojure/Python/Slime installation directories. If it fails to determine the correct directories, then you need to enter the command to start the SWANK server into your vimrc file.

Linux example:
    let g:slimv_swank_cmd = '! xterm -e sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &'

Windows example:
    let g:slimv_swank_cmd = '!start "c:/Program Files/Lisp Cabinet/bin/ccl/wx86cl.exe" -l "c:/Program Files/Lisp Cabinet/site/lisp/slime/start-swank.lisp"'

For Clojure use the g:slimv_swank_clojure option, e.g.:
    let g:slimv_swank_clojure = '! xterm -e lein swank &' 


Important notice to pre-0.8.0 users:
If you want the old functionality, please set g:slimv_swank to 0 in your vimrc file. Please note however, that the development focuses on the SWANK client.

See the included documentation for more complete installation and customization instructions.

--------------------------------------------------------------------------------
Script versions
--------------------------------------------------------------------------------

0.8.5: Switch on indent plugins, do not complete empty string on <Tab>, added Clojure keywords to syntax plugin, use -i option to load swank-clojure, implementation specific REPL initialization, for Clojure it imports source, apropos, javadoc, etc. (thanks to Ömer Sinan Agacan), print Lisp version at REPL startup, added List-Threads, Kill-Thread, Debug-Thread (thanks to Philipp Marek), write prompt after Toggle-Trace, display list of untraced functions for Untrace-All, when in SLDB, Interactive-Eval evaluates expressions in the frame, Inspect inspects objects in the frame, changed g:slimv_echolines logic: set 0 for no lines, -1 for all lines, bugfixes: removed extra linebreak between chunks of long output, indentation problems for symbols with package specification (thanks to Philipp Marek), indentation of Clojure's defn, plist indentation (thanks to Philipp Marek), occasional few seconds delay in swank response, running Swank server on Mac OS X (on behalf of Tobias Pflug).

0.8.4: Added handling for Unicode characters, truncate arglist output to fit in the status line, added debugger keybindings: ,a for abort ,q for quit ,n for continue, changed keybinding for apropos to ,A, added compiler error messages to quickfix list, map insert mode <Space> and <Tab> only for lisp (and dialects) buffers, bugfixes: wait for the response to :create-repl before calling :swank-require (thanks to Philipp Marek), indentation problems with unbalanced parens in comment, arglist ate the <Space> when virtualedit was off.

0.8.3: Added top/bottom/left/right directions to g:slimv_repl_split, added :Lisp (and an equivalent :Eval) command with completion, added g:slimv_leader and g:paredit_leader options, added g:slimv_echolines to echo only the first few lines of the form being evaluated, added fuzzy completion and option g:slimv_simple_compl (by Philipp Marek), indent macros with &body argument by two spaces when connected to swank (thanks to Philipp Marek and Andreas Fredriksson), special indentation for flet, labels and macrolet, default for Set-Package is current package (thanks to Philipp Marek), bugfixes: REPL output ordering problems, problem with inserting Space into visual block, blinking when g:slimv_repl_syntax is on, entering incomplete form in REPL command line, close form when inside comment, string, or with mixed ([. 

0.8.2: Added Paredit and g:lisp_rainbow support for Scheme files, added SWANK support for MIT Scheme on Linux, added frame call information to SLDB (thanks to Philipp Marek), check for unbalanced form before evaluation, reconnect SWANK server in Connect-Server if already connected (thanks to Philipp Marek), select current form instead of top level form in Macroexpand, bugfixes: Paredit handling of escaped matched characters (e.g. \"), cursor positioning problems when debugger activated, print prompt after Describe.

0.8.1: Added action handling to Inspector, fixed Inspector output, bugfixes: read-string mode was stuck, buffer corruption with two source windows (thanks to Philipp Marek), eliminate multiple CursorHold autocommands (thanks to Philipp Marek), completion with special characters in symbol name (thanks to Philipp Marek), sometimes cursor went to the start of line in insert mode, syntax error in Untrace All (thanks to Philipp Marek), removed ' prefix from symbol selection (except for Inspect), keep cursor position in Describe and Compile-Region.

0.8.0: Major update: added SWANK client (many thanks to Philipp Marek), additional changes: split documentation into three parts, added keymapping hints to GUI menu items, renamed Eval-Last-Expression to Eval-Current-Expression, REPL buffer is not syntax highlighted anymore, switch on filetype plugins, autodetection for Allegro CL, Lisp Cabinet and Leiningen, ask for save before compiling file, map <Tab> for completion, bugfixes: finding start of keyword in completion, deleting escaped " inside string, Up/Down/Enter handling in popup menu.

0.7.7: Find next closing paren when using ,< or ,> in Paredit and not standing on a paren, open REPL buffer upon connecting server, bugfixes: REPL buffer prompt identification was sometimes missing, switch off REPL refresh mode when REPL buffer is not visible (thanks to Philipp Marek), convert Python path on Windows to short 8.3 filename format if it contains space (thanks to Razvan Rotaru).

0.7.6: Cursor position is kept during evaluation, most Slimv commands made silent, bugfixes: find defun start when cursor is on a comment, keep newlines in Compile-Region, infinite loop when selecting form in empty buffer, error when opening REPL buffer with g:slimv_repl_split=0, REPL blinking in insert mode when visualbell is on, added the comma to the list of macro prefix characters (thanks to John Obbele), long/short Windows filename problem for REPL buffer. 

0.7.5: Added Cygwin compatibility using the Windows Python (thanks to Jerome Baum), display client error message when eval was not successful, form is passed to client via stdin instead of temp file, bugfixes: automatic reconnection when server closed and reopened, delete and yank also via named registers in paredit.vim, handle double quotes in Compile-Defun and Compile-Region.

0.7.4: Added autodetection for simple 'clojure' command on Linux, removed duplicates from history of commands entered in REPL buffer (those recallable with <Up>), bugfixes: infinite loop during eval when 'in-package' or 'in-ns' was in comment, Lisp prompt identification problems in REPL buffer, input line duplication in SBCL on Linux (assigned "*debug-io*" to stdin), Eval Defun sometimes missed last ")".

0.7.3: Added compatibility with Python 3.x, bugfixes: input lines for REPL were doubled on Linux (thanks to Andrew Hills), however not yet fixed for SBCL, enclose Slimv path in double quotes if contains space, select form when standing on prefix character (e.g. ' or `).

0.7.2: Added autodetection for /usr/local/bin/clojure on Linux, added special characters to Lisp keyword selection (iskeyword), run Vim's original ftplugin/lisp.vim for Clojure filetype, bugfixes: PareditWrap error when g:paredit_shortmaps=1 (thanks to Jon Thacker), buffer selection problems in case of three of more buffers (thanks to Philipp Marek), conflicting keybindings for SlimvGenerateTags, unmap error messages when g:paredit_mode=0.

0.7.1: Added option g:slimv_browser_cmd for opening hyperspec in a custom webbrowser (on behalf of Andreas Salwasser), added paredit handling for d<motion>, c<motion>, p and P commands: keep paren balance when deleting and pasting text, Paredit Toggle function removes and re-adds paredit keybindings, bugfix: D and C deleted till beginning of line if () or [] found, handle escaped \" characters inside string.

0.7.0: Added package/namespace support, new way of refreshing the REPL buffer via autocommands, removed 'RUNNING' mode, cursor stays in the current buffer at evaluation, added option g:slimv_updatetime, removed options related to the old way of refreshing (g:slimv_repl_return and g:slimv_repl_wait), removed debug logging, updated Clojure API to version 1.2, extended keyword definition when selecting symbols, bugfix: defmacro detection problem (again).

0.6.3: Added option to return cursor to the editor window from REPL buffer after evaluating an s-expression, Wrap: if standing on a paren then wrap the whole s-expression, Wrap selection: exit visual mode after command, Bugfixes: inserting double quotes in paredit mode (like "\""), dd in paredit mode when unbalanced form is inside comment, reopen REPL buffer after closing it via ":q", comment and string detection error with noignorecase setting, wrong positioning when moving parenthesis to the right, defmacro detection problem, paredit wrap selection missed last character when 'selection' was not "exclusive".

0.6.2: Added support for Mac OS X via Terminal.app (on behalf of Vlad Hanciuta), added string "clj" as a detector for Clojure (by Vlad Hanciuta), bugfixes: paredit wrap function missed last character when 'selection' was not "exclusive" (thanks to Marcin Fatyga), input was stuck inside SBCL debugger (on behalf of Philipp Marek and Dmitry Petukhov), occasional error messages during REPL buffer update, REPL menu was sometimes missing, occasional command line color problems.

0.6.1: Added Split/Join/Wrap/Splice functions to Paredit Mode, bugfixes: delete commands put erased characters into yank buffer, D does not delete whole line.

0.6.0: Added paredit mode, set wrap for REPL buffer.

0.5.6: Improved REPL buffer response time, added debug log flushing frequency, bugfix: early exit of REPL refresh mode on some machines.

0.5.5: Updated Clojure API to 1.1, expand tilde-prefix to home directory on Linux, autodetect Clojure in the user home directory on Linux. 

0.5.4: Added autodetection for clojure-contrib.jar and Clozure CL, applied lisp_rainbow to Clojure's [], renamed Clojure indent plugin to clojure.vim, switched on lisp indentation for Clojure files. 

0.5.3: Added Interrupt-Lisp-Process command, added mapping for the REPL menu, added special forms to Clojre API lookup, bugfixes: put cursor after the last character in insert mode when refreshing REPL buffer, fixed some Ctrl-C handling problems.

0.5.2: Adapted Clojure API lookup and autodetection to version 1.0, Complete-Symbol moved to separate Edit submenu, added Close-Form command.

0.5.1: Added symbol name completion based on the Hyperspec database.

0.5.0: Major project reorganization: Slimv is now a Lisp and Clojure filetype plugin, added Common Lisp Hyperspec, Clojure API and JavaDoc lookup, separate menu for REPL buffer, menu items work in every Vim mode, fixed mark 's usage bug.

0.4.1: Added profiler support for SBCL, new commands: Show Profiled/Send Input/Close and Send Input/Previous Input/Next Input, highlight error messages.

0.4.0: Added SLIME's profiling tool with support from Slimv, added command to generate tags file, fixed eval problems of large buffers, fixed Compile And Load filename problems with '\' on Windows, recycle old REPL temporary file at next server startup.

0.3.0: REPL buffer enhancements: added syntax highlighting and automatic indentation, it is possible to enter a multi-line command, Ctrl-C is working; server REPL window performance enhancement on Linux.

0.2.2: Fixed REPL input and output mix-up problems, evaluation performance enhancement, corrected some more macroexpand problems. 

0.2.1: Added basic Clojure support, fixed some macroexpand and REPL refresh problems.

0.2.0: Major update: Lisp REPL inside a Vim buffer.

0.1.4: Corrected the delayed display of last line in REPL window on Linux.
       Ctrl+C is propagated to Lisp REPL, so it is possible to interrupt a running program.

0.1.3: Handle DOS and Unix style line endings on Windows, don't write logfile when debug level not set

vim:et:wrap:
