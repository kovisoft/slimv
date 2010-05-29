" paredit.vim:
"               Paredit mode for Slimv
" Version:      0.6.2
" Last Change:  26 May 2010
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

"TODO: automatic indentation
" Automatic indentation after some editing commands
"if !exists( 'g:paredit_autoindent' )
"    let g:paredit_autoindent = 1
"endif

" Match delimiter this number of lines before and after cursor position
if !exists( 'g:paredit_matchlines' )
    let g:paredit_matchlines = 100
endif

" Use short keymaps, i.e. J instead of <Leader>J
if !exists( 'g:paredit_shortmaps' )
    let g:paredit_shortmaps = 0
endif

" =====================================================================
"  Other variable definitions
" =====================================================================

" Skip matches inside string or comment
let s:skip_c  = 'synIDattr(synID(line("."), col("."), 0), "name") =~ "comment"'
let s:skip_sc = 'synIDattr(synID(line("."), col("."), 0), "name") =~ "string\\|comment"'

" Regular expressions to identify special characters combinations used by paredit
"TODO: add curly brace
let s:any_matched_char   = '(\|)\|\[\|\]\|\"'
let s:any_matched_pair   = '()\|\[\]\|\"\"'
let s:any_opening_char   = '(\|\['
let s:any_closing_char   = ')\|\]'
let s:any_openclose_char = '(\|\[\|)\|\]'
let s:any_wsopen_char    = '\s\|(\|\['
let s:any_wsclose_char   = '\s\|)\|\]'
let s:any_macro_prefix   = "'" . '\|`\|#\|@\|\~'

let s:yank_pos           = []

" =====================================================================
"  General utility functions
" =====================================================================

" Buffer specific initialization
function! PareditInitBuffer()
    "  Buffer specific keybindings
    inoremap <buffer> <expr>   (            PareditInsertOpening('(',')')
    inoremap <buffer> <expr>   )            PareditInsertClosing('(',')')
    inoremap <buffer> <expr>   [            PareditInsertOpening('[',']')
    inoremap <buffer> <expr>   ]            PareditInsertClosing('[',']')
    inoremap <buffer> <expr>   "            PareditInsertQuotes()
    inoremap <buffer> <expr>   <BS>         PareditBackspace(0)
    inoremap <buffer> <expr>   <Del>        PareditDel()
    nnoremap <buffer> <silent> (            :<C-U>call PareditFindOpening('(',')',0)<CR>
    nnoremap <buffer> <silent> )            :<C-U>call PareditFindClosing('(',')',0)<CR>
    vnoremap <buffer> <silent> (            <Esc>:<C-U>call PareditFindOpening('(',')',1)<CR>
    vnoremap <buffer> <silent> )            <Esc>:<C-U>call PareditFindClosing('(',')',1)<CR>
    nnoremap <buffer> <silent> x            :<C-U>call PareditEraseFwd()<CR>
    nnoremap <buffer> <silent> <Del>        :<C-U>call PareditEraseFwd()<CR>
    nnoremap <buffer> <silent> X            :<C-U>call PareditEraseBck()<CR>
    nnoremap <buffer> <silent> s            :<C-U>call PareditEraseFwd()<CR>i
    nnoremap <buffer> <silent> D            :<C-U>call PareditEraseFwdLine()<CR>
    nnoremap <buffer> <silent> C            :<C-U>call PareditEraseFwdLine()<CR>A
    nnoremap <buffer> <silent> dd           :<C-U>call PareditEraseLine()<CR>
    nnoremap <buffer> <silent> cc           0:<C-U>call PareditEraseFwdLine()<CR>A
    nnoremap <buffer> <silent> <Leader>w(   :<C-U>call PareditWrap('(',')')<CR>
    vnoremap <buffer> <silent> <Leader>w(   :<C-U>call PareditWrapSelection('(',')')<CR>
    nnoremap <buffer> <silent> <Leader>w[   :<C-U>call PareditWrap('[',']')<CR>
    vnoremap <buffer> <silent> <Leader>w[   :<C-U>call PareditWrapSelection('[',']')<CR>
    nnoremap <buffer> <silent> <Leader>w"   :<C-U>call PareditWrap('"','"')<CR>
    vnoremap <buffer> <silent> <Leader>w"   :<C-U>call PareditWrapSelection('"','"')<CR>

    if g:paredit_shortmaps
        " Shorter keymaps: old functionality of KEY is remapped to <Leader>KEY
        nnoremap <buffer> <silent> <            :<C-U>call PareditMoveLeft()<CR>
        nnoremap <buffer> <silent> >            :<C-U>call PareditMoveRight()<CR>
        nnoremap <buffer> <silent> O            :<C-U>call PareditSplit()<CR>
        nnoremap <buffer> <silent> J            :<C-U>call PareditJoin()<CR>
        nnoremap <buffer> <silent> W            :<C-U>call PareditWrap()<CR>
        vnoremap <buffer> <silent> W            :<C-U>call PareditWrapSelection()<CR>
        nnoremap <buffer> <silent> S            :<C-U>call PareditSplice()<CR>
        nnoremap <buffer> <silent> <Leader><    :<C-U>normal! <<CR>
        nnoremap <buffer> <silent> <Leader>>    :<C-U>normal! ><CR>
        nnoremap <buffer> <silent> <Leader>O    :<C-U>normal! O<CR>
        nnoremap <buffer> <silent> <Leader>J    :<C-U>normal! J<CR>
        nnoremap <buffer> <silent> <Leader>W    :<C-U>normal! W<CR>
        vnoremap <buffer> <silent> <Leader>W    :<C-U>normal! W<CR>
        nnoremap <buffer> <silent> <Leader>S    :<C-U>normal! S<CR>
    else
        " Longer keymaps with <Leader> prefix
        nnoremap <buffer> <silent> S            0:<C-U>call PareditEraseFwdLine()<CR>A
        nnoremap <buffer> <silent> <Leader><    :<C-U>call PareditMoveLeft()<CR>
        nnoremap <buffer> <silent> <Leader>>    :<C-U>call PareditMoveRight()<CR>
        nnoremap <buffer> <silent> <Leader>O    :<C-U>call PareditSplit()<CR>
        nnoremap <buffer> <silent> <Leader>J    :<C-U>call PareditJoin()<CR>
        nnoremap <buffer> <silent> <Leader>W    :<C-U>call PareditWrap('(',')')<CR>
        vnoremap <buffer> <silent> <Leader>W    :<C-U>call PareditWrapSelection('(',')')<CR>
        nnoremap <buffer> <silent> <Leader>S    :<C-U>call PareditSplice()<CR>
    endif
endfunction

" Toggle paredit mode
function! PareditToggle()
    let g:paredit_mode = 1 - g:paredit_mode
    echo g:paredit_mode ? 'Paredit mode on' : 'Paredit mode off'
endfunction

" Does the current syntax item match the given regular expression?
function! s:SynIDMatch( regexp )
    let line = line('.')
    let col  = col('.')
    if col > len( getline( line ) )
        let col = col - 1
    endif
    return synIDattr( synID( line, col, 0), 'name' ) =~ a:regexp
endfunction

" Is the current cursor position inside a comment?
function! s:InsideComment()
    return s:SynIDMatch( 'comment' )
endfunction

" Is the current cursor position inside a string?
function! s:InsideString()
    return s:SynIDMatch( 'string' )
endfunction

" Is the current cursor position inside a comment or string?
function! s:InsideCommentOrString()
    return s:SynIDMatch( 'string\|comment' )
endfunction

" Autoindent current top level form
function! PareditIndentTopLevelForm( level )
    if a:level < g:paredit_autoindent
        return
    endif
    let l = line( '.' )
    let c =  col( '.' )
    normal! ms
    let matchb = max( [l-g:paredit_matchlines, 1] )
    let [l0, c0] = searchpairpos( '(', '', ')', 'brmW', s:skip_sc, matchb )
    "let save_exp = &expandtab
    "set expandtab
    normal! v%=`s
    "let &expandtab = save_exp
endfunction

" Is this a Slimv REPL buffer?
function! s:IsReplBuffer()
    if exists( 'g:slimv_repl_dir' ) && exists( 'g:slimv_repl_file' )
        let repl_name = g:slimv_repl_dir . g:slimv_repl_file
        return bufnr( repl_name ) == bufnr( '%' )
    else
        return bufname( '%' ) =~ '.*\.repl\..*'
    endif
endfunction

" Get Slimv REPL buffer last command prompt position
" Return [0, 0] if this is not the REPL buffer
function! s:GetReplPromptPos()
    if !s:IsReplBuffer()
        return [0, 0]
    endif
    return [ line( "'s" ), col( "'s" ) ]
endfunction

" Is the current top level form balanced, i.e all opening delimiters
" have a matching closing delimiter
function! s:IsBalanced()
    let l = line( '.' )
    let c =  col( '.' )
    let line = getline( '.' )
    let matchb = max( [l-g:paredit_matchlines, 1] )
    let matchf = min( [l+g:paredit_matchlines, line('$')] )
    let prompt = line( "'s" )
    if s:IsReplBuffer() && l >= prompt && matchb < prompt
        " Do not go before the last command prompt in the REPL buffer
        let matchb = prompt
    endif
    let p1 = searchpair( '(', '', ')', 'brnmW', s:skip_sc, matchb )
    let p2 = searchpair( '(', '', ')',  'rnmW', s:skip_sc, matchf )
    if !(p1 == p2) && !(p1 == p2 - 1 && line[c-1] == '(') && !(p1 == p2 + 1 && line[c-1] == ')')
        " Number of opening and closing parens differ
        return 0
    endif
    let b1 = searchpair( '\[', '', '\]', 'brnmW', s:skip_sc, matchb )
    if b1 == 0
        " Outside of all bracket-pairs
        return 1
    endif
    let b2 = searchpair( '\[', '', '\]',  'rnmW', s:skip_sc, matchf )
    if !(b1 == b2) && !(b1 == b2 - 1 && line[c-1] == '[') && !(b1 == b2 + 1 && line[c-1] == ']')
        " Number of opening and closing brackets differ
        return 0
    endif
    return 1
endfunction

" Find opening matched character
function! PareditFindOpening( open, close, select )
    let open  = escape( a:open , '[]' )
    let close = escape( a:close, '[]' )
    call searchpair( open, '', close, 'bW', s:skip_sc )
    if a:select
        call searchpair( open, '', close, 'W', s:skip_sc )
        let save_ve = &ve
        set ve=all 
        normal! lvh
        let &ve = save_ve
        call searchpair( open, '', close, 'bW', s:skip_sc )
    endif
endfunction

" Find closing matched character
function! PareditFindClosing( open, close, select )
    let open  = escape( a:open , '[]' )
    let close = escape( a:close, '[]' )
    if a:select
        let line = getline( '.' )
        if line[col('.')-1] != a:open
            normal! h
        endif
        call searchpair( open, '', close, 'W', s:skip_sc )
        call searchpair( open, '', close, 'bW', s:skip_sc )
        normal! v
        call searchpair( open, '', close, 'W', s:skip_sc )
        normal! l
    else
        call searchpair( open, '', close, 'W', s:skip_sc )
    endif
endfunction

" Insert opening type of a paired character, like ( or [.
function! PareditInsertOpening( open, close )
    if !g:paredit_mode || s:InsideCommentOrString() || !s:IsBalanced()
        return a:open
    endif
    let retval = a:open . a:close . "\<Left>"
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if line[pos] !~ s:any_wsclose_char
        " Add a space after if needed
        let retval = a:open . a:close . " \<Left>\<Left>"
    else
        let retval = a:open . a:close . "\<Left>"
    endif
    if pos > 0 && line[pos-1] !~ s:any_wsopen_char && line[pos-1] !~ s:any_macro_prefix
        " Add a space before if needed
        let retval = " " . retval
    endif
    return retval
endfunction

" Insert closing type of a paired character, like ) or ].
function! PareditInsertClosing( open, close )
    if !g:paredit_mode || s:InsideCommentOrString() || !s:IsBalanced()
        return a:close
    endif
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if line[pos] == a:close
        return "\<Right>"
    else
        let open  = escape( a:open , '[]' )
        let close = escape( a:close, '[]' )
        return "\<C-O>:call searchpair('" . open . "','','" . close . "','W','" . s:skip_sc . "')\<CR>\<Right>"
        "TODO: indent after going to closing character
    endif
endfunction

" Insert an (opening or closing) double quote
function! PareditInsertQuotes()
    if !g:paredit_mode || s:InsideComment()
        return '"'
    endif
    if s:InsideString()
        let line = getline( '.' )
        let pos = col( '.' ) - 1
        "TODO: skip comments in search(...)
        if line[pos] == '"'
            " Standing on a ", just move to the right
            return "\<Right>"
        elseif (pos > 0 && line[pos-1] == '\') || search('[^\\]"\|^"', 'nW') == 0
            " We don't have any closing ", insert one
            return '"'
        else
            " Move to the closing "
            return "\<C-O>:call search('" . '[^\\]"\|^"' . "','eW')\<CR>\<Right>"
        endif
    else
        " Outside of string: insert a pair of ""
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

    if !g:paredit_mode || s:InsideComment()
        return "\<BS>"
    endif

    let line = getline( '.' )
    let pos = col( '.' ) - 1

    if pos == 0
        " We are at the beginning of the line
        return "\<BS>"
    elseif line[pos-1] !~ s:any_matched_char
        " Deleting a non-special character
        return "\<BS>"
    elseif line[pos-1] != '"' && !s:IsBalanced()
        " Current top-form is unbalanced, can't retain paredit mode
        return "\<BS>"
    endif

    if line[pos-1:pos] =~ s:any_matched_pair
        " Deleting an empty character-pair
        return "\<Right>\<BS>\<BS>"
    else
        " Character-pair is not empty, don't delete just move inside
        return "\<Left>"
    endif
endfunction

" Handle <Del> keypress
function! PareditDel()
    if !g:paredit_mode || s:InsideComment()
        return "\<Del>"
    endif

    let line = getline( '.' )
    let pos = col( '.' ) - 1

    if pos == len(line)
        " We are at the end of the line
        return "\<Del>"
    elseif line[pos] !~ s:any_matched_char
        " Erasing a non-special character
        return "\<Del>"
    elseif line[pos] != '"' && !s:IsBalanced()
        " Current top-form is unbalanced, can't retain paredit mode
        return "\<Del>"
    elseif pos == 0
        return "\<Right>"
    endif

    if line[pos-1:pos] =~ s:any_matched_pair
        " Erasing an empty character-pair
        return "\<Left>\<Del>\<Del>"
    else
        " Character-pair is not empty, don't erase just move inside
        return "\<Right>"
    endif
endfunction

" Initialize yank position list
function! s:InitYankPos()
    let @" = ''
    let s:yank_pos = []
endfunction

" Add position to the yank list
function! s:AddYankPos( pos )
"echo input('111 '.a:pos)
    let s:yank_pos = [a:pos] + s:yank_pos
endfunction

" Remove the head of yank position list and return it
function! s:RemoveYankPos()
    if len(s:yank_pos) > 0
        let pos = s:yank_pos[0]
        let s:yank_pos = s:yank_pos[1:]
        return pos
    else
        return 0
    endif
endfunction

" Forward erasing a character in normal mode, do not check if current form balanced
function! s:EraseFwd( count, startcol )
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    let reg = @"
    let c = a:count
    while c > 0
        if s:InsideString() && line[pos : pos+1] == '\"'
            " Erasing a \" inside string
            let reg = reg . line[pos : pos+1]
            let line = strpart( line, 0, pos ) . strpart( line, pos+2 )
        elseif s:InsideComment() && line[pos] == ';' && a:startcol >= 0
            " Erasing the whole comment, only when erasing a block of characters
            let reg = reg . strpart( line, pos )
            let line = strpart( line, 0, pos )
        elseif s:InsideComment() || ( s:InsideString() && line[pos] != '"' )
            " Erasing any character inside string or comment
            let reg = reg . line[pos]
            let line = strpart( line, 0, pos ) . strpart( line, pos+1 )
        elseif pos > 0 && line[pos-1:pos] =~ s:any_matched_pair
            if pos > a:startcol
                " Erasing an empty character-pair
                let p2 = s:RemoveYankPos()
                let reg = strpart( reg, 0, p2 ) . line[pos-1] . strpart( reg, p2 )
                let reg = reg . line[pos]
                let line = strpart( line, 0, pos-1 ) . strpart( line, pos+1 )
                let pos = pos - 1
                normal! h
            else
                " Can't erase character-pair: it would move the cursor before startcol
                let pos = pos + 1
                normal! l
            endif
        elseif line[pos] =~ s:any_matched_char
            " Character-pair is not empty, don't erase just move inside
            call s:AddYankPos( len(reg) )
            let pos = pos + 1
            normal! l
        elseif pos < len(line)
            " Erasing a non-special character
            let reg = reg . line[pos]
            let line = strpart( line, 0, pos ) . strpart( line, pos+1 )
        endif
        let c = c - 1
    endwhile
    call setline( '.', line )
    let @" = reg
endfunction

" Backward erasing a character in normal mode, do not check if current form balanced
function! s:EraseBck( count )
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    let reg = @"
    let c = a:count
    while c > 0 && pos > 0
        if s:InsideString() && pos > 1 && line[pos-2:pos-1] == '\"'
            let reg = reg . line[pos-2 : pos-1]
            let line = strpart( line, 0, pos-2 ) . strpart( line, pos )
            normal! h
            let pos = pos - 1
        elseif s:InsideComment() || ( s:InsideString() && line[pos-1] != '"' )
            let reg = reg . line[pos-1]
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos )
        elseif line[pos-1:pos] =~ s:any_matched_pair
            " Erasing an empty character-pair
            let p2 = s:RemoveYankPos()
            let reg = strpart( reg, 0, p2 ) . line[pos-1] . strpart( reg, p2 )
            let reg = reg . line[pos]
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos+1 )
        elseif line[pos-1] =~ s:any_matched_char
            " Character-pair is not empty, don't erase
            call s:AddYankPos( len(reg) )
        else
            " Erasing a non-special character
            let reg = reg . line[pos-1]
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos )
        endif
        normal! h
        let pos = pos - 1
        let c = c - 1
    endwhile
    call setline( '.', line )
    let @" = reg
endfunction

" Forward erasing a character in normal mode
function! PareditEraseFwd()
    if !g:paredit_mode || !s:IsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'x'
        else
            normal! x
        endif
        return
    endif

    call s:InitYankPos()
    call s:EraseFwd( v:count1, -1 )
endfunction

" Backward erasing a character in normal mode
function! PareditEraseBck()
    if !g:paredit_mode || !s:IsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'X'
        else
            normal! X
        endif
        return
    endif

    call s:InitYankPos()
    call s:EraseBck( v:count1 )
endfunction

" Forward erasing character till the end of line in normal mode
" Keeping the balanced state
function! s:EraseFwdLine( startcol )
    let lastcol = -1
    let lastlen = -1
    while col( '.' ) != lastcol || len( getline( '.' ) ) != lastlen
        let lastcol = col( '.' )
        let lastlen = len( getline( '.' ) )
        call s:EraseFwd( 1, a:startcol )
    endwhile
endfunction

" Forward erasing character till the end of line in normal mode
" But first check if we are allowed to do it in paredit way
function! PareditEraseFwdLine()
    if !g:paredit_mode || !s:IsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'D'
        else
            normal! D
        endif
        return
    endif

    call s:InitYankPos()
    call s:EraseFwdLine( col( '.' ) - 1 )
endfunction

" Erasing all characters in the line in normal mode
" Keeping the balanced state
function! PareditEraseLine()
    if !g:paredit_mode || !s:IsBalanced()
        if v:count > 0
            silent execute 'normal! ' . v:count . 'dd'
        else
            normal! dd
        endif
        return
    endif

    normal! 0
    call s:InitYankPos()
    let c = v:count1
    while c > 0
        call s:EraseFwdLine( -1 )
        if len( getline( '.' ) ) == 0
            let reg = @"
            normal! dd
            let @" = reg . "\n"
        elseif c > 1
            normal! J
            let @" = @" . "\n"
        endif
        let c = c - 1
    endwhile

    normal! ==
endfunction

" Find beginning of previous element (atom or sub-expression) in a form
" skip_whitespc: skip whitespaces before the previous element
function! s:PrevElement( skip_whitespc )
    let [l0, c0] = [line( '.' ), col( '.' )]
    let symbol_pos = [0, 0]
    let symbol_end = [0, 0]

    " Move to the beginning of the prefix if any
    let line = getline( '.' )
    let c = col('.') - 1
    if c > 0 && line[c-1] =~ s:any_macro_prefix
        normal! h
    endif

    let moved = 0
    while 1
        " Go to previous character
        if !moved
            let [l1, c1] = [line( '.' ), col( '.' )]
            normal! h
        endif
        let moved = 0
        let [l, c] = [line( '.' ), col( '.' )]

        if [l, c] == [l1, c1]
            " Beginning of line reached
            if symbol_pos != [0, 0]
                let symbol_end = [l, c]
                if !a:skip_whitespc && !s:InsideString()
                    " Newline before previous symbol
                    call setpos( '.', [0, l0, c0, 0] )
                    return [l, c]
                endif
            endif
            normal! k$
            let [l, c] = [line( '.' ), col( '.' )]
            if [l, c] == [l1, c1]
                " Beginning of file reached: stop
                call setpos( '.', [0, l0, c0, 0] )
                return [0, 0]
            endif
            let moved = 1
        elseif s:InsideComment()
            " Skip comments
        else
            let line = getline( '.' )
            if s:InsideString()
                let symbol_pos = [l, c]
            elseif symbol_pos == [0, 0]
                if line[c-1] =~ s:any_closing_char
                    " Skip to the beginning of this sub-expression
                    let symbol_pos = [l, c]
                    normal! %
                    let line2 = getline( '.' )
                    let c2 = col('.') - 1
                    if c2 > 0 && line2[c2-1] =~ s:any_macro_prefix
                        normal! h
                    endif
                elseif line[c-1] =~ s:any_opening_char
                    " Opening delimiter found: stop
                    call setpos( '.', [0, l0, c0, 0] )
                    return [0, 0]
                elseif line[c-1] =~ '\S'
                    " Previous symbol starting
                    let symbol_pos = [l, c]
                endif
            else
                if line[c-1] =~ s:any_opening_char || (a:skip_whitespc && line[c-1] =~ '\S' && symbol_end != [0, 0])
                    " Previous symbol beginning reached, opening delimiter or second previous symbol starting
                    call setpos( '.', [0, l0, c0, 0] )
                    return [l, c+1]
                elseif line[c-1] =~ '\s' || symbol_pos[0] != l
                    " Whitespace before previous symbol
                    let symbol_end = [l, c]
                    if !a:skip_whitespc
                        call setpos( '.', [0, l0, c0, 0] )
                        return [l, c+1]
                    endif
                endif
            endif
        endif
    endwhile
endfunction

" Find end of next element (atom or sub-expression) in a form
" skip_whitespc: skip whitespaces after the next element
function! s:NextElement( skip_whitespc )
    let [l0, c0] = [line( '.' ), col( '.' )]
    let symbol_pos = [0, 0]
    let symbol_end = [0, 0]

    while 1
        " Go to next character
        let [l1, c1] = [line( '.' ), col( '.' )]
        normal! l
        let [l, c] = [line( '.' ), col( '.' )]

        " Skip comments
        while [l, c] == [l1, c1] || s:InsideComment()
            if symbol_pos != [0, 0]
                let symbol_end = [l, c]
                if !a:skip_whitespc && !s:InsideString()
                    " Next symbol ended with comment
                    call setpos( '.', [0, l0, c0, 0] )
                    return [l, c]
                endif
            endif
            normal! 0j0
            let [l, c] = [line( '.' ), col( '.' )]
            if [l, c] == [l1, c1]
                " End of file reached: stop
                call setpos( '.', [0, l0, c0, 0] )
                return [0, 0]
            endif
        endwhile

        let line = getline( '.' )
        if s:InsideString()
            let symbol_pos = [l, c]
        elseif symbol_pos == [0, 0]
            if line[c-1] =~ s:any_macro_prefix && line[c] =~ s:any_opening_char
                " Skip to the end of this prefixed sub-expression
                let symbol_pos = [l, c]
                normal! l%
            elseif line[c-1] =~ s:any_opening_char
                " Skip to the end of this sub-expression
                let symbol_pos = [l, c]
                normal! %
            elseif line[c-1] =~ s:any_closing_char
                " Closing delimiter found: stop
                call setpos( '.', [0, l0, c0, 0] )
                return [0, 0]
            elseif line[c-1] =~ '\S'
                " Next symbol starting
                let symbol_pos = [l, c]
            endif
        else
            if line[c-1] =~ s:any_closing_char || (a:skip_whitespc && line[c-1] =~ '\S' && symbol_end != [0, 0])
                " Next symbol ended, closing delimiter or second next symbol starting
                call setpos( '.', [0, l0, c0, 0] )
                return [l, c]
            elseif line[c-1] =~ '\s' || symbol_pos[0] != l
                " Next symbol ending with whitespace
                let symbol_end = [l, c]
                if !a:skip_whitespc
                    call setpos( '.', [0, l0, c0, 0] )
                    return [l, c]
                endif
            endif
        endif
    endwhile
endfunction

" Move character from [l0, c0] to [l1, c1]
" Set position to [l1, c1]
function! s:MoveChar( l0, c0, l1, c1 )
    let line = getline( a:l0 )
    let c = line[a:c0-1]
    if a:l1 == a:l0
        " Move character inside line
        if a:c1 > a:c0
            let line = strpart( line, 0, a:c0-1 ) . strpart( line, a:c0, a:c1-a:c0-1 ) . c . strpart( line, a:c1-1 )
            call setline( a:l0, line )
            call setpos( '.', [0, a:l1, a:c1-1, 0] ) 
        else
            let line = strpart( line, 0, a:c1-1 ) . c . strpart( line, a:c1-1, a:c0-a:c1 ) . strpart( line, a:c0 )
            call setline( a:l0, line )
            call setpos( '.', [0, a:l1, a:c1, 0] ) 
        endif
    else
        " Move character to another line
        let line = strpart( line, 0, a:c0-1 ) . strpart( line, a:c0 )
        call setline( a:l0, line )
        let line1 = getline( a:l1 )
        if a:c1 > 1
            let line1 = strpart( line1, 0, a:c1-1 ) . c . strpart( line1, a:c1-1 )
            call setline( a:l1, line1 )
            call setpos( '.', [0, a:l1, a:c1, 0] )
        else
            let line1 = c . line1
            call setline( a:l1, line1 )
            call setpos( '.', [0, a:l1, 1, 0] ) 
        endif
    endif
endfunction

" Find a paren nearby to move
function! s:FindParenNearby()
    let line = getline( '.' )
    let c0 =  col( '.' )
    if line[c0-1] !~ s:any_openclose_char
        " OK, we are not standing on a paren to move, but check if there is one nearby
        if (c0 < 2 || line[c0-2] !~ s:any_openclose_char) && line[c0] =~ s:any_openclose_char
            normal! l
        elseif c0 > 1 && line[c0-2] =~ s:any_openclose_char && line[c0] !~ s:any_openclose_char
            normal! h
        endif
    endif

    " Skip macro prefix character    
    let c0 =  col( '.' )
    if line[c0-1] =~ s:any_macro_prefix && line[c0] =~ s:any_opening_char
        normal! l
    endif
endfunction

" Move delimiter one atom or s-expression to the left
function! PareditMoveLeft()
    call s:FindParenNearby()

    let line = getline( '.' )
    let l0 = line( '.' )
    let c0 =  col( '.' )

    if line[c0-1] =~ s:any_opening_char
        let closing = 0
    elseif line[c0-1] =~ s:any_closing_char
        let closing = 1
    else
        " Can move only delimiters
        return
    endif

    let [lp, cp] = s:GetReplPromptPos()
    let [l1, c1] = s:PrevElement( closing )
    if [l1, c1] == [0, 0]
        " No previous element found
        return
    elseif [lp, cp] != [0, 0] && l0 >= lp && (l1 < lp || (l1 == lp && c1 < cp))
        " Do not go before the last command prompt in the REPL buffer
        return
    endif
    if !closing && c0 > 0 && line[c0-2] =~ s:any_macro_prefix
        call s:MoveChar( l0, c0-1, l1, c1 )
        call s:MoveChar( l0, c0 - (l0 != l1), l1, c1+1 )
        let len = 2
    else
        call s:MoveChar( l0, c0, l1, c1 )
        let len = 1
    endif
    let line = getline( '.' )
    let c =  col( '.' ) - 1
    if closing && line[c+1] !~ s:any_wsclose_char
        " Insert a space after if needed
        execute "normal! a "
        normal! h
    endif
    if !closing && c > 0 && line[c-len] !~ s:any_wsopen_char
        " Insert a space before if needed
        if len > 1
            execute "normal! hi "
            normal! ll
        else
            execute "normal! i "
            normal! l
        endif
    endif
    return
endfunction

" Move delimiter one atom or s-expression to the right
function! PareditMoveRight()
    call s:FindParenNearby()

    "TODO: move ')' in '() xxx' leaves space
    let line = getline( '.' )
    let l0 = line( '.' )
    let c0 =  col( '.' )

    if line[c0-1] =~ s:any_opening_char
        let opening = 1
    elseif line[c0-1] =~ s:any_closing_char
        let opening = 0
    else
        " Can move only delimiters
        return
    endif

    let [lp, cp] = s:GetReplPromptPos()
    let [l1, c1] = s:NextElement( opening )
    if [l1, c1] == [0, 0]
        " No next element found
        return
    elseif [lp, cp] != [0, 0] && l0 < lp && l1 >= lp
        " Do not go after the last command prompt in the REPL buffer
        return
    endif
    if opening && c0 > 0 && line[c0-2] =~ s:any_macro_prefix
        call s:MoveChar( l0, c0-1, l1, c1 )
        call s:MoveChar( l0, c0-1, l1, c1 + (l0 != l1) )
        let len = 2
    else
        call s:MoveChar( l0, c0, l1, c1 )
        let len = 1
    endif
    let line = getline( '.' )
    let c =  col( '.' ) - 1
    if opening && c > 0 && line[c-len] !~ s:any_wsopen_char
        " Insert a space before if needed
        if len > 1
            execute "normal! hi "
            normal! ll
        else
            execute "normal! i "
            normal! l
        endif
    endif
    if !opening && line[c+1] !~ s:any_wsclose_char
        " Insert a space after if needed
        execute "normal! a "
        normal! h
    endif
    return
endfunction

" Find closing of the innermost structure: (...) or [...]
" Return a list where first element is the closing character,
" second and third is its position (line, column)
function! s:FindClosing()
    let l = line( '.' )
    let c = col( '.' )
    call PareditFindClosing( '(', ')', 0 )
    let lp = line( '.' )
    let cp = col( '.' )
    call setpos( '.', [0, l, c, 0] )
    call PareditFindClosing( '[', ']', 0 )
    let lb = line( '.' )
    let cb = col( '.' )
    call setpos( '.', [0, l, c, 0] )
    if [lp, cp] == [l, c] && [lb, cb] == [l, c]
        " Not found any kind of paren
        return ['', 0, 0]
    elseif [lb, cb] == [l, c] || lp < lb || (lp == lb && cp < cb)
        " The innermost structure is a (...)
        return [')', lp, cp]
    else
        " The innermost structure is a [...]
        return [']', lb, cb]
    endif
endfunction

" Split list or string at the cursor position
" Current symbol will be split into the second part
function! PareditSplit()
    if !g:paredit_mode || s:InsideComment()
        return
    endif

    if s:InsideString()
        normal! i" "
    else
        " Go back to the beginning of the current symbol
        let c = col('.') - 1
        if getline('.')[c] =~ '\S'
            if c == 0 || (c > 0 && getline('.')[c-1] =~ s:any_wsopen_char)
                " OK, we are standing on the first character of the symbol
            else
                normal! b
            endif
        endif

        " First find which kind of paren is the innermost
        let [p, l, c] = s:FindClosing()
        if p !~ s:any_closing_char
            " Not found any kind of parens
            return
        endif

        " Delete all whitespaces around cursor position
        while getline('.')[col('.')-1] =~ '\s'
            normal! x
        endwhile
        while col('.') > 1 && getline('.')[col('.')-2] =~ '\s'
            normal! X
        endwhile

        if p == ')'
            normal! i) (
        else
            normal! i] [
        endif
    endif
endfunction

" Join two neighboring lists or strings
function! PareditJoin()
    if !g:paredit_mode || s:InsideCommentOrString()
        return
    endif

    "TODO: skip parens in comments
    let [l0, c0] = searchpos(s:any_matched_char, 'nbW')
    let [l1, c1] = searchpos(s:any_matched_char, 'ncW')
    if [l0, c0] == [0, 0] || [l1, c1] == [0, 0]
        return
    endif
    let line0 = getline( l0 )
    let line1 = getline( l1 )
    if (line0[c0-1] == ')' && line1[c1-1] == '(') || (line0[c0-1] == ']' && line1[c1-1] == '[') || (line0[c0-1] == '"' && line1[c1-1] == '"')
        if l0 == l1
            " First list ends on the same line where the second list begins
            let line0 = strpart( line0, 0, c0-1 ) . ' ' . strpart( line0, c1 )
            call setline( l0, line0 )
        else
            " First list ends on a line different from where the second list begins
            let line0 = strpart( line0, 0, c0-1 )
            let line1 = strpart( line1, 0, c1-1 ) . strpart( line1, c1 )
            call setline( l0, line0 )
            call setline( l1, line1 )
        endif
    endif
endfunction

" Wrap current visual block in parens of the given kind
function! s:WrapSelection( open, close )
    let l0 = line( "'<" )
    let l1 = line( "'>" )
    let c0 = col( "'<" )
    let c1 = col( "'>" )
    if [l0, c0] == [0, 0] || [l1, c1] == [0, 0]
        " No selection
        return
    endif
    if l0 > l1 || (l0 == l1 && c0 > c1)
        " Swap both ends of selection to make [l0, c0] < [l1, c1]
        let [ltmp, ctmp] = [l0, c0]
        let [l0, c0] = [l1, c1]
        let [l1, c1] = [ltmp, ctmp]
    endif
    let save_ve = &ve
    set ve=all 
    call setpos( '.', [0, l0, c0, 0] )
    execute "normal! i" . a:open
    call setpos( '.', [0, l1, c1 + (l0 == l1), 0] )
    execute "normal! i" . a:close
    let &ve = save_ve
endfunction

" Wrap current visual block in parens of the given kind
" Keep visual mode
function! PareditWrapSelection( open, close )
    call s:WrapSelection( a:open, a:close )
    if line( "'<" ) == line( "'>" )
        normal! gvolol
    else
        normal! gvolo
    endif
endfunction

" Wrap current symbol in parens of the given kind
" Stand on the opening paren (if not wrapping in "")
function! PareditWrap( open, close )
    let sel = &selection
    let &selection = 'exclusive'
    execute "normal! " . "viw\<Esc>"
    call s:WrapSelection( a:open, a:close )
    if a:open != '"'
        normal! %
    endif
    let &selection = sel
endfunction

" Splice current list into the containing list
function! PareditSplice()
    if !g:paredit_mode
        return
    endif

    " First find which kind of paren is the innermost
    let [p, l, c] = s:FindClosing()
    if p !~ s:any_closing_char
        " Not found any kind of parens
        return
    endif

    call setpos( '.', [0, l, c, 0] )
    normal! %
    let l = line( '.' )
    let c = col( '.' )
    normal! %x
    call setpos( '.', [0, l, c, 0] )
    normal! x
    if c > 1 && getline('.')[c-2] =~ s:any_macro_prefix
        normal! X
    endif
endfunction


" =====================================================================
"  Autocommands
" =====================================================================

au BufNewFile,BufRead *.lisp call PareditInitBuffer()
au BufNewFile,BufRead *.clj  call PareditInitBuffer()

