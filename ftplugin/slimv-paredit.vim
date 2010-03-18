" slimv-paredit.vim:
"               Paredit mode for Slimv
" Version:      0.6.0
" Last Change:  18 Mar 2010
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if &cp || exists( 'g:paredit_loaded' )
    finish
endif

let g:paredit_loaded = 1

" =====================================================================
"  Global variable definitions
" =====================================================================

" Paredit mode selector
if !exists( 'g:paredit_mode' )
    let g:paredit_mode = 1
endif

if !exists( 'g:paredit_matchlines' )
    let g:paredit_matchlines = 100
endif

" =====================================================================
"  General utility functions
" =====================================================================

" Is the current cursor position inside a comment?
function! PareditInsideComment()
    let line = line('.')
    let col  = col('.')
    if col > len( getline( line ) )
        let col = col - 1
    endif
    return synIDattr( synID( line, col, 0), 'name' ) =~ "comment"
endfunction

" Is the current cursor position inside a string?
function! PareditInsideString()
    "TODO: detect only if we are really inside, i.e. not on the double quote at the edge
    let line = line('.')
    let col  = col('.')
    if col > len( getline( line ) )
        let col = col - 1
    endif
    return synIDattr( synID( line, col, 0), 'name' ) =~ "string"
endfunction

" Is the current cursor position inside a comment or string?
function! PareditInsideCommentOrString()
    let line = line('.')
    let col  = col('.')
    if col > len( getline( line ) )
        let col = col - 1
    endif
    return synIDattr( synID( line, col, 0), 'name' ) =~ "string\\|comment"
endfunction

function! PareditIsBalanced()
    let paren = 0
    let bracket = 0
    let inside_string = 0
    let current_form_closed = 0
    let l1 = line( '.' )
    let c1 = col ( '.' ) - 1
    "let l = 1
    "let start = searchpairpos( '(', '', ')', 'brW', 'getline(".")=~".*;.*"' )
    let [lstart, cstart] = searchpairpos( '(', '', ')', 'brnW', '', l1-g:paredit_matchlines )
    if lstart == 0 && cstart == 0
        let l = l1
    else
        let l = lstart
    endif
    let [lend, cend] = searchpairpos( '(', '', ')', 'rnW', '', l1+g:paredit_matchlines )
    if lend == 0 && cend == 0
        let lend = line( '$' )
    endif
    while l <= lend
        let inside_comment = 0
        let line = getline( l )
        let c = 0
        while c < len( line )
            if inside_string
                " We are inside a string, skip parens, wait for closing '"'
                if line[c] == '"'
                    let inside_string = 0
                endif
            elseif inside_comment
                " We are inside a comment, skip to the end of line
                let c = len( line ) - 1
            else
                " We are outside of strings and comments, now we shall count parens
                if line[c] == '"'
                    let inside_string = 1
                elseif line[c] == ';'
                    let inside_comment = 1
                elseif line[c] == '('
                    let paren = paren + 1
                    if current_form_closed
                        " Current form closed (balanced) and another one is starting
                        return 1
                    endif
                elseif line[c] == ')'
                    let paren = paren - 1
                    if paren == 0 && ( l > l1 || ( l == l1 && c >= c1 ))
                        let current_form_closed = 1
                    elseif paren < 0
                        " Oops, too many closing parens
                        return 0
                    endif
                elseif line[c] == '['
                    let bracket = bracket + 1
                elseif line[c] == ']'
                    let bracket = bracket - 1
                    if bracket < 0
                        " Oops, too many closing brackets
                        return 0
                    endif
                endif
            endif
            let c = c + 1
        endwhile
        let l = l + 1
    endwhile

    " No subsequent form found, paren/bracket must be 0 for balanced forms
    if paren != 0 || bracket != 0 || inside_string
        return 0
    endif
    return 1
endfunction

" Insert opening type of a paired character, like ( or [.
function! PareditInsertOpening( open, close )
    if !g:paredit_mode || PareditInsideCommentOrString() || !PareditIsBalanced()
        return a:open
    endif
    let retval = a:open . a:close . "\<Left>"
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if line[pos] != ' ' && line[pos] != '\t' && line[pos] != '(' && line[pos] != ')' && line[pos] != '[' && line[pos] != ']'
        let retval = a:open . a:close . " \<Left>\<Left>"
    else
        let retval = a:open . a:close . "\<Left>"
    endif
    if pos > 0 && line[pos-1] != ' ' && line[pos-1] != '\t' && line[pos-1] != '(' && line[pos-1] != '['
        let retval = " " . retval
    endif
    return retval
endfunction

" Insert closing type of a paired character, like ) or ].
function! PareditInsertClosing( open, close )
    if !g:paredit_mode || PareditInsideCommentOrString() || !PareditIsBalanced()
        return a:close
    endif
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if line[pos] == a:close
        return "\<Right>"
    else
        let open  = escape( a:open , '[]' )
        let close = escape( a:close, '[]' )
        return "\<C-O>:call searchpair('" . open . "','','" . close . "','W')\<CR>\<Right>"
    endif
endfunction

" Insert an (opening or closing) double quote
function! PareditInsertQuotes()
    if !g:paredit_mode || PareditInsideComment()
        return '"'
    endif
    if PareditInsideString()
        let line = getline( '.' )
        let pos = col( '.' ) - 1
        if line[pos] == '"'
            return "\<Right>"
        elseif search('"', 'W') == 0
            return '"'
        else
            "return "\<C-O>f" . '"'
            "return "\<C-O>:call search('" . '"' . "','W')\<CR>\<Right>"
            "call search('"', 'W')
            return "\<Right>"
        endif
    else
        return '""' . "\<Left>"
    endif
endfunction

" Handle <BS> keypress
function! PareditBackspace( repl_mode )
    if a:repl_mode && line( "." ) == line( "'s" ) && col( "." ) <= col( "'s" )
        " No BS allowed before the previous EOF mark in the REPL
        " i.e. don't delete Lisp prompt
        return ""
    endif

    if !g:paredit_mode || PareditInsideComment()
        return "\<BS>"
    endif

    let line = getline( '.' )
    let pos = col( '.' ) - 1

    if pos == 0
        " We are at the beginning of the line
        return "\<BS>"
    elseif line[pos-1] != '(' && line[pos-1] != ')' && line[pos-1] != '[' && line[pos-1] != ']' && line[pos-1] != '"'
        " Deleting a non-special character
        return "\<BS>"
    elseif line[pos-1] != '"' && !PareditIsBalanced()
        " Current top-form is unbalanced, can't retain paredit mode
        return "\<BS>"
    endif

    if (line[pos-1] == '(' && line[pos] == ')') || (line[pos-1] == '[' && line[pos] == ']') || (line[pos-1] == '"' && line[pos] == '"')
        " Deleting an empty character-pair
        return "\<Right>\<BS>\<BS>"
    else
        " Character-pair is not empty, don't delete just move inside
        return "\<Left>"
    endif
endfunction

" Handle <Del> keypress
function! PareditDel()
    if !g:paredit_mode || PareditInsideComment()
        return "\<Del>"
    endif

    let line = getline( '.' )
    let pos = col( '.' ) - 1

    if pos == len(line)
        " We are at the end of the line
        return "\<Del>"
    elseif line[pos] != '(' && line[pos] != ')' && line[pos] != '[' && line[pos] != ']' && line[pos] != '"'
        " Erasing a non-special character
        return "\<Del>"
    elseif line[pos] != '"' && !PareditIsBalanced()
        " Current top-form is unbalanced, can't retain paredit mode
        return "\<Del>"
    elseif pos == 0
        return "\<Right>"
    endif

    if (line[pos-1] == '(' && line[pos] == ')') || (line[pos-1] == '[' && line[pos] == ']') || (line[pos-1] == '"' && line[pos] == '"')
        " Erasing an empty character-pair
        return "\<Left>\<Del>\<Del>"
    else
        " Character-pair is not empty, don't erase just move inside
        return "\<Right>"
    endif
endfunction

" Forward erasing a character in normal mode, do not check if current form balanced
function! s:EraseFwd( count )
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    let c = a:count
    while c > 0
        if PareditInsideComment()
            let line = strpart( line, 0, pos ) . strpart( line, pos+1 )
        elseif pos == len(line)
            " We are at the end of the line
            let line = strpart( line, 0, pos-1 )
        elseif pos > 0 && ((line[pos-1] == '(' && line[pos] == ')') || (line[pos-1] == '[' && line[pos] == ']') || (line[pos-1] == '"' && line[pos] == '"'))
            " Erasing an empty character-pair
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos+1 )
            let pos = pos - 1
            normal! h
        elseif line[pos] == '(' || line[pos] == ')' || line[pos] == '[' || line[pos] == ']' || line[pos] == '"'
            " Character-pair is not empty, don't erase just move inside
            let pos = pos + 1
            normal! l
        else
            " Erasing a non-special character
            let line = strpart( line, 0, pos ) . strpart( line, pos+1 )
        endif
        let c = c - 1
    endwhile
    call setline( '.', line )
endfunction

" Backward erasing a character in normal mode, do not check if current form balanced
function! s:EraseBck( count )
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    let c = a:count
    while c > 0 && pos > 0
        if PareditInsideComment()
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos )
        elseif (line[pos-1] == '(' && line[pos] == ')') || (line[pos-1] == '[' && line[pos] == ']') || (line[pos-1] == '"' && line[pos] == '"')
            " Erasing an empty character-pair
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos+1 )
        elseif line[pos-1] != '(' && line[pos-1] != ')' && line[pos-1] != '[' && line[pos-1] != ']' && line[pos-1] != '"'
            " Erasing a non-special character
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos )
        endif
        normal! h
        let pos = pos - 1
        let c = c - 1
    endwhile
    call setline( '.', line )
endfunction

" Forward erasing a character in normal mode
function! PareditEraseFwd()
    if !g:paredit_mode || !PareditIsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'x'
        else
            normal! x
        endif
        return
    endif

    call s:EraseFwd( v:count1 )
endfunction

" Backward erasing a character in normal mode
function! PareditEraseBck()
    if !g:paredit_mode || !PareditIsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'X'
        else
            normal! X
        endif
        return
    endif

    call s:EraseBck( v:count1 )
endfunction

" Forward erasing character till the end of line in normal mode
" Keeping the balanced state
function! PareditEraseFwdLine()
    if !g:paredit_mode || !PareditIsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'D'
        else
            normal! D
        endif
        return
    endif

    let lastcol = -1
    let lastlen = -1
    while col( '.' ) != lastcol || len( getline( '.' ) ) != lastlen
        let lastcol = col( '.' )
        let lastlen = len( getline( '.' ) )
        call s:EraseFwd( 1 )
    endwhile
endfunction


" Erasing all characters in the line in normal mode
" Keeping the balanced state
function! PareditEraseLine()
    if !g:paredit_mode || !PareditIsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'dd'
        else
            normal! dd
        endif
        return
    endif

    let c = v:count1
    while c > 0
        call PareditEraseFwdLine()

        let lastcol = -1
        let lastlen = -1
        while col( '.' ) != lastcol || len( getline( '.' ) ) != lastlen
            let lastcol = col( '.' )
            let lastlen = len( getline( '.' ) )
            call s:EraseBck( 1 )
        endwhile

        if len( getline( '.' ) ) == 0
            normal! dd
        elseif c > 1
            normal! J
        endif
        let c = c - 1
    endwhile
endfunction

" =====================================================================
"  Keybindings
" =====================================================================

inoremap <expr> (     PareditInsertOpening('(',')')
inoremap <expr> )     PareditInsertClosing('(',')')
inoremap <expr> [     PareditInsertOpening('[',']')
inoremap <expr> ]     PareditInsertClosing('[',']')

inoremap <expr> "     PareditInsertQuotes()
inoremap <expr> <BS>  PareditBackspace(0)
inoremap <expr> <Del> PareditDel()
noremap         x     :<C-U>call PareditEraseFwd()<CR>
noremap         X     :<C-U>call PareditEraseBck()<CR>
noremap         s     :<C-U>call PareditEraseFwd()<CR>i
noremap         D     :<C-U>call PareditEraseFwdLine()<CR>
noremap         C     :<C-U>call PareditEraseFwdLine()<CR>A
noremap         S     0:<C-U>call PareditEraseFwdLine()<CR>A
noremap         dd    :<C-U>call PareditEraseLine()<CR>

