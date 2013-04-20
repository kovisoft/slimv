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

Slimv supports SLIME's debugger, inspector, profiler, cross reference, arglist, indentation, symbol name completion functions. The script also has a Common Lisp Hyperspec lookup feature and it is able to lookup symbols in the Clojure API, as well as in JavaDoc.

Slimv comes with Paredit Mode, which is similar to the functionality of paredit.el in Emacs. Paredit Mode tries to maintain the balanced state of matched characters (parenthesis marks, square and curly braces, double quotes). Matched characters are inserted and removed in pairs, also when working with a block of text (well, mostly). Slimv also implements many paredit.el s-expression handling functions, like Split/Join/Wrap/Splice/Raise. Slurpage and Barfage known from Emacs is also possible but in a different fashion: you don't move the list element in or out of the list, rather you move the opening or closing parenthesis over the element or sub-list.

Please visit the Slimv Tutorial for a more complete introduction:
http://kovisoft.bitbucket.org/tutorial.html

Please find the most recent development version in the repository:
https://bitbucket.org/kovisoft/slimv

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
    *  Set Breakpoint
    *  Break on Exception
    *  Inspect
    *  Abort
    *  Quit to Toplevel
    *  Continue
    *  Restart Frame
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
    *  Clear REPL

For more information see the included documentation.
 
---------------------------------------------------------------------------------------------
Installation details
---------------------------------------------------------------------------------------------

Extract the zip archive into your vimfiles or runtime directory.

Slimv works on Windows, Linux and Mac OS X (via Terminal.app), Cygwin is supported. The script requires the following programs installed on your system:
    *  Vim with Python feature enabled
    *  Python (must be the same Python version that was Vim compiled against)
    *  Lisp (any Common Lisp with SLIME support) or Clojure or MIT Scheme (Linux only)

Vim's Python version can be identified with the :ver command, look for the -DDYNAMIC_PYTHON_DLL=\"pythonXX\" string (if you have it). Another way of determining Vim's Python version:

:python import sys; print(sys.version)

Slimv tries to autodetect your Lisp/Clojure/Slime installation directories. If it fails to determine the correct directories, then you need to enter the command to start the SWANK server into your vimrc file.

Linux example:
    let g:slimv_swank_cmd = '! xterm -e sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &'

Windows example:
    let g:slimv_swank_cmd = '!start "c:/Program Files/Lisp Cabinet/bin/ccl/wx86cl.exe" -l "c:/Program Files/Lisp Cabinet/site/lisp/slime/start-swank.lisp"'

Mac OS X example:
    let g:slimv_swank_cmd = '!osascript -e "tell application \"Terminal\" to do script \"sbcl --load ~/.vim/slime/start-swank.lisp\""'

For Clojure use the g:slimv_swank_clojure option, e.g.:
    let g:slimv_swank_clojure = '! xterm -e lein swank &' 


See the included documentation for more complete installation and customization instructions.

--------------------------------------------------------------------------------
Script versions
--------------------------------------------------------------------------------

0.9.10: Replaced 'readonly' flag with 'nomodifiable' for SLDB, Inspect, Threads buffers. Restore window and buffer if SLDB is activated during completion. Allow using Slimv and VimClojure (or other clojure filetype plugin) together. Added Restart-Frame command (thanks to Philipp Marek). Added defn- to clojure keywords (thanks to David Soria Parra). Paredit initialization on filetype instead of filename extension. Do not permanently set 'iskeyword' in paredit.vim. Paredit: added '^' macro prefix, treat #_(...) type of clojure comments as regular forms, handle VimClojure's #"" regexp syntax definition. It is now possible to remove plugin/paredit.vim. Bugfixes: Fixed missing variable error message. REPL prompt position corruption problems. Cursor positioning problems when displaying arglist. Paredit 'cc', 'Vc' did not delete line. Paredit 'd', 'c', 'vc', 'cW' cursor positioning problems. Paredit 'C' extra whitespace and trailing ". Reset indent after paredit 'cc'. Paredit 'dd', ',&gt;' EOL problem. paredit indentation when 'indentexpr' takes no argument (thanks to Tim Pope). Keep cursor inside "" when deleting over trailing " via paredit 'cW', 'C', etc. Use &amp;ft instead of SlimvGetFiletype() in paredit.vim.

0.9.9: Added Paredit functions to Slimv menu (thanks to Conrad Schuler). Inspector: Use the same package when pressing [--more--], speeding up multi-part object processing, save/restore cursor position for previously visited pages. ,rc was doubly mapped, use ,- for REPL Clear. Autodetect ritz-swank. Added Break-on-Exception function for ritz-swank. Added minibuffer operations, this enables [set value] in Inspector. Added smartjumping for Clojure (thanks to dgrnbrg on bitbucket.org). Disable indenting on "set noautoindent". Pass python output to Vim script in variable instead of redirecting stdout (hopefully solves 64-bit MacVim problems). Handle [] and {} delimiters in Scheme like in Clojure. Paredit: ignore (, [, or { when preceded by \. Bugfixes: Arglist on Space after pressing 'I' in visual block mode. Indentation after multi-line subform. Problems with finding function name for arglist. Corruption when pasting large text into console Vim. Cursor positioning in REPL buffer when virtualedit=all. Multi-line entry name parsing in Inspector.

0.9.8: Added autodetection for 'ccl'. Delete empty lines when re-gathering Electric Returns. Inspector: put multiple items in one line (like in Slime), highlight selectable parts and actions, hide item id-s, display "path" of inspected object. Don't extend s-expression with prefix when macroexpanding. Don't evaluate or compile the REPL buffer. Added device to the path when loading pretty printer patches for SBCL (thanks to Andrew Lyon). Added option g:slimv_repl_simple_eval and Electric Return for REPL buffer. Print arglist when pressing Space after closing parens or double quotes, also when pressing Enter. Added "Clear REPL" entry to the REPL menu (mapped to <Leader>-). Paredit: special handling of cw, cb, ciw, caw supporting repeat ('.'). Do not describe empty symbol. Prefer selecting symbol to the left when cursor is on whitespace. Added "." character to iskeyword for Lisp. Removed "." when selecting symbol for completion for Clojure. Increased fuzzy completion limit. Bugfixes: Find package/namespace when current form is in a fold. PareditToggle ckecks if buffer was registered for Paredit. Electric Return re-gathering at end of line with no virtualedit. Extra character at the end of selection using 'v('. Garbage upon pressing ')' or Enter in completion popup. Paredit 'x' at end of line when 'whichwrap' includes h,l. Arglist sometimes not displayed. Paredit Wrap when line ends in a multibyte character (thanks to Sung Pae).

0.9.7: Keep cursor position on expanding [--more--] in the Inspector. Added [--all---] to Inspector for fetching all parts. Don't explicitly check for pythonXX.dll, rely on has('python'). Require 'swank-repl' for slime version above 2011-12-04 in case contribs are disabled. Identify VimClojure REPL prompt position for paredit (thanks to David Greenberg). Paredit: added <leader><Up> for Splice-killing-backward, <leader><Down> for Splice-killing-forward, <leader>I for Raise. Paredit: added 'Electric Return' feature and new option g:paredit_electric_return. Increased the distance to search for the defun start to 200 lines. Bugfixes: Positioning the cursor at the end of REPL prompt in insert mode. Handle restart/frame number above 999 (thanks to Philipp Marek). Form selection when cursor is next to the opening paren. Indentation of multi-line strings. Indentation of lines with multi-byte characters. Cursor movement while indenting.

0.9.6: Perform indenting when Tab pressed on whitespace. Added support for newer swank message length counting in bytes. Updated Lisp swank server to version 2012-03-06. Updated Clojure API reference to version 1.3. Identify .cljs files as Clojure type. Enable g:slimv_repl_syntax by default. Restart parenthesis highlighting at each REPL prompt. Scheme: eval buffer and region in a (begin ...) block. Added option g:scheme_builtin_swank for enabling MIT scheme's built-in swank server. Added syntax highlight for Scheme quoted symbol. Keep SLDB buffer if swank stays in debugger upon selecting a restart. When reconnecting the swank server wait for disconnection first. Fixed REPL buffer slowdown caused by re-assigning autocommands. Fixed detection of string and comment when no syntax loaded. Fixed Scheme syntax highlight problems in the REPL buffer. Call :frame-source-location and :frame-call only if swank supports them.

0.9.5: Use correct SLDB level when invoking restart. Autodetect tmux session on Linux (thanks to Brett Kosinski). Enable syntax only once to avoid reload of syntax plugins. Added option g:slimv_browser_cmd_suffix. Skip syntax and indent file for disabled filetypes. Check the presence of X on Linux. Indentation fixes: keywords, gap after '(', defsystem, defmacro, symbol-macrolet. Use winsaveview()/winrestview() for remembering current view when moving around (e.g. searching for package). Find package for arglist and completion. Ignore mapleader when it's <Space>. Print SLDB error description also into the REPL buffer. Evaluate keyword if using Eval-Defun outside of s-expression. Disable unsupported swank features for Scheme. Bugfixes: Paredit 'cw' at the end of line. Omit REPL prompt from Eval-Defun and Eval-Expression. Printing of '\n' and other escaped characters. Paredit delete and put corrupted the "0 register.

0.9.4: Added highlighting of square and curly brackets for Clojure. Added options to disable Slimv for specific filetypes: g:slimv_disable_clojure, g:slimv_disable_lisp, g:slimv_disable_scheme. Added option g:slimv_indent_keylists (thanks to Andrew Smirnoff). Added "set hidden" for safe switching of modified buffers. Added Help to Inspect and Threads buffers. Evaluate register contents if Eval-Region prefixed by ["x]. Store form in register if Eval-Defun or Eval-Exp prefixed by ["x]. Increased timeout for :create-repl. Stay in REPL buffer if Macroexpand performed in REPL. Search for either (in-ns) or (ns) for Clojure, remove quote character from namespace name. Added SlimvEvalTestDefun() and SlimvEvalTestExp() for immediate testing of the form(s) being evaluated. Bugfixes: Various indentation issues (function name is a subform, let, let*, do, defpackage, defsystem, and [] for Clojure). Eval-Range problem in visual mode. SLDB parsing problem with newlines in description of restarts. REPL autoscroll incidentally stopping (thanks to Andrew Lyon). Added some index out of range checks (thanks to Philipp Marek).

0.9.3: Start Swank server in virtual terminal when running in GNU screen on Linux (thanks to Oleg Terenchuk). Reuse a window for slimv also when there are three or more windows open. Don't go to end of REPL buffer if user moved the cursor away from EOF. Use xdg-open for default browser in Linux. Removed option g:slimv_python. Added option g:slimv_repl_max_len for limiting the number of lines in the REPL buffer. Added option g:slimv_preferred to choose preferred lisp implementation. Query additional parts of big inspected object upon pressing Enter on [--more--]. Thread List is displayed and handled in a separate Threads buffer. Bugfixes: Window navigation problems between source/REPL/SLDB/etc. Error messages when Swank server window is closed. Return control to vim after starting browser defined by g:slimv_browser_cmd. Fixed indentation of arguments before &body argument. Autocomplete for dotted package/namespace names. Indentation of aif.

0.9.2: Added option g:swank_log to enable swank debug log. Added options g:slimv_repl_name, g:slimv_sldb_name, g:slimv_inspect_name. Added option g:slimv_indent_maxlines. Changed Debug-Thread mapping to <leader>dT (g:slimv_keybindings=2) due to conflict with Generate-Tags. Label thread ID in thread list (by Philipp Marek). Set balloonexpr for all buffers (thanks to Philipp Marek). Connect swank server when needed instead of printing an error message (by Philipp Marek). Set expandtab for lisp and clojure files. Kill-Thread kills all threads in the selected range (by Philipp Marek). Bugfixes: Added missing parts of Set-Breakpoint introduced in 0.9.1. Test source lookup (upon pressing Enter) before testing fold toggle in SLDB (by Philipp Marek). Indentation of flet, labels, macrolet. Kill-Thread now really kills thread (by Philipp Marek). Inspect gensyms in frame (by Philipp Marek).

0.9.1: Improved frame number identification in SLDB buffer. Moved frame source location above frame locals in SLDB. Fold frame source location if more than 2 lines. Inspect-In-Frame: preselect symbol under cursor only in variable lines, open Inspector in the other window. Improved XRef file location parsing. Use current paragraph when no range set for Eval-Region and Compile-Region. Added option g:slimv_sldb_wrap, do not set wrap for source buffers. Added Set-Breakpoint command mapped to <leader>B (thanks to Philipp Marek), changed Profile-By-Substring mapping to <leader>P. Set Lisp keyword characters also in SLDB buffer. Bugfixes: Error messages at Connect-Server. Error message for frame source location without filename. XRef output sometimes cut.

0.9.0: Separate buffers for SLDB and Inspector, toggle frame information in SLDB buffer by pressing Enter, look up source when pressing Enter on filename with location in SLDB, added option g:swank_block_size to override Swank output buffer size (thanks to stassats on #lisp and Philipp Marek), removed old non-swank functionality, removed option g:slimv_repl_open, paredit: new mappings [[ and ]] for previous and next defun, bugfixes: various refresh problems (thanks to Philipp Marek), disable debug mode when reconnecting Swank (by Philipp Marek), display multi-line debug condition and frame source location, quote characters in compile (by Philipp Marek), use proper SLDB level when invoking restart (by Philipp Marek), restore all lisp keyword characters in iskeyword, indentation of defgeneric, use proper filename and location when compiling defun or region, buffer corruption when re-triggering timer in insert mode, <End> moved cursor to the right edge of screen in REPL buffer when virtualmode=all.

0.8.6: Handle cl:in-package, common-lisp:in-package (thanks to Philipp Marek), added option g:swank_host to allow connecting to remote Swank server, autodetection of Cake for Clojure (thanks to Chris Cahoon), set paredit mode also for .cl and .rkt files, recognise domain reversed package names in form com.gigamonkeys.pathnames (thanks to has2k1), added curly braces rainbow parenthesis for Clojure, added paredit handling of curly braces for Clojure, use SlimvIndent also for Clojure, handle line number returned in :compilation-result, bugfixes: removed double newline in :read-string (text input), when editing with cw in paredit mode, keep ending whitespaces (thanks to Mats Rauhala), compilation error when Swank does not return file name, skip dot character when Swank returns a dotted pair (a . b).

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
