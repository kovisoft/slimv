" paredit.vim:
"               Paredit mode for Slimv
" Version:      0.9.9
" Last Change:  10 Nov 2012
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

" Needed to load filetype and indent plugins
filetype plugin on
filetype indent on

" =====================================================================
"  Global variable definitions
" =====================================================================

" Paredit mode selector
if !exists( 'g:paredit_mode' )
    let g:paredit_mode = 1
endif

" Match delimiter this number of lines before and after cursor position
if !exists( 'g:paredit_matchlines' )
    let g:paredit_matchlines = 100
endif

" Use short keymaps, i.e. J instead of <Leader>J
if !exists( 'g:paredit_shortmaps' )
    let g:paredit_shortmaps = 0
endif

" Use smart jumping to the nearest paren, curly brace, or square bracket in
" clojure
if !exists( 'g:paredit_smartjump' )
    let g:paredit_smartjump = 0
endif

" Custom <Leader> for the Paredit plugin
if !exists( 'g:paredit_leader' )
    if exists( 'mapleader' )
        let g:paredit_leader = mapleader
    else
        let g:paredit_leader = ','
    endif
endif

" Use 'Electric Return', i.e. add double newlines if enter pressed before a closing paren
if !exists( 'g:paredit_electric_return' )
    let g:paredit_electric_return = 1
endif

" =====================================================================
"  Other variable definitions
" =====================================================================

" Skip matches inside string or comment or after '\'
let s:skip_sc = '(synIDattr(synID(line("."), col("."), 0), "name") =~ "[Ss]tring\\|[Cc]omment\\|[Ss]pecial" || getline(line("."))[col(".")-2] == "\\")'

" Valid macro prefix characters
let s:any_macro_prefix   = "'" . '\|`\|#\|@\|\~\|,'

" Repeat count for some remapped edit functions (like 'd')
let s:repeat             = 0

let s:yank_pos           = []

" =====================================================================
"  General utility functions
" =====================================================================

" Buffer specific initialization
function! PareditInitBuffer()
    let b:paredit_init = 1
    " Make sure to include special characters in 'iskeyword'
    " in case they are accidentally removed
    " Also define regular expressions to identify special characters used by paredit
    if &ft =~ '.*\(clojure\|scheme\).*'
        setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&
        let b:any_matched_char   = '(\|)\|\[\|\]\|{\|}\|\"'
        let b:any_matched_pair   = '()\|\[\]\|{}\|\"\"'
        let b:any_opening_char   = '(\|\[\|{'
        let b:any_closing_char   = ')\|\]\|}'
        let b:any_openclose_char = '(\|)\|\[\|\]\|{\|}'
        let b:any_wsopen_char    = '\s\|(\|\[\|{'
        let b:any_wsclose_char   = '\s\|)\|\]\|}'
    else
        setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&,{,},[,]
        let b:any_matched_char   = '(\|)\|\"'
        let b:any_matched_pair   = '()\|\"\"'
        let b:any_opening_char   = '('
        let b:any_closing_char   = ')'
        let b:any_openclose_char = '(\|)'
        let b:any_wsopen_char    = '\s\|('
        let b:any_wsclose_char   = '\s\|)'
    endif

    if g:paredit_mode
        " Paredit mode is on: add buffer specific keybindings
        inoremap <buffer> <expr>   (            PareditInsertOpening('(',')')
        inoremap <buffer> <silent> )            <C-R>=(pumvisible() ? "\<lt>C-Y>" : "")<CR><C-O>:let save_ve=&ve<CR><C-O>:set ve=all<CR><C-O>:<C-U>call PareditInsertClosing('(',')')<CR><C-O>:let &ve=save_ve<CR>
        inoremap <buffer> <expr>   "            PareditInsertQuotes()
        inoremap <buffer> <expr>   <BS>         PareditBackspace(0)
        inoremap <buffer> <expr>   <Del>        PareditDel()
        if &ft =~ '.*\(clojure\|scheme\).*' && g:paredit_smartjump
            nnoremap <buffer> <silent> (            :<C-U>call PareditSmartJumpOpening(0)<CR>
            nnoremap <buffer> <silent> )            :<C-U>call PareditSmartJumpClosing(0)<CR>
            vnoremap <buffer> <silent> (            <Esc>:<C-U>call PareditSmartJumpOpening(1)<CR>
            vnoremap <buffer> <silent> )            <Esc>:<C-U>call PareditSmartJumpClosing(1)<CR>
        else
            nnoremap <buffer> <silent> (            :<C-U>call PareditFindOpening('(',')',0)<CR>
            nnoremap <buffer> <silent> )            :<C-U>call PareditFindClosing('(',')',0)<CR>
            vnoremap <buffer> <silent> (            <Esc>:<C-U>call PareditFindOpening('(',')',1)<CR>
            vnoremap <buffer> <silent> )            <Esc>:<C-U>call PareditFindClosing('(',')',1)<CR>
        endif
        nnoremap <buffer> <silent> [[           :<C-U>call PareditFindDefunBck()<CR>
        nnoremap <buffer> <silent> ]]           :<C-U>call PareditFindDefunFwd()<CR>
        nnoremap <buffer> <silent> x            :<C-U>call PareditEraseFwd()<CR>
        nnoremap <buffer> <silent> <Del>        :<C-U>call PareditEraseFwd()<CR>
        nnoremap <buffer> <silent> X            :<C-U>call PareditEraseBck()<CR>
        nnoremap <buffer> <silent> s            :<C-U>call PareditEraseFwd()<CR>i
        nnoremap <buffer> <silent> D            v$:<C-U>call PareditDelete(visualmode(),1)<CR>
        nnoremap <buffer> <silent> C            v$:<C-U>call PareditChange(visualmode(),1)<CR>
        nnoremap <buffer> <silent> d            :<C-U>call PareditSetDelete(v:count)<CR>g@
        vnoremap <buffer> <silent> d            :<C-U>call PareditDelete(visualmode(),1)<CR>
        vnoremap <buffer> <silent> x            :<C-U>call PareditDelete(visualmode(),1)<CR>
        vnoremap <buffer> <silent> <Del>        :<C-U>call PareditDelete(visualmode(),1)<CR>
        nnoremap <buffer> <silent> c            :set opfunc=PareditChange<CR>g@
        vnoremap <buffer> <silent> c            :<C-U>call PareditChange(visualmode(),1)<CR>
        nnoremap <buffer> <silent> dd           :<C-U>call PareditDeleteLines()<CR>
        nnoremap <buffer> <silent> cc           :<C-U>call PareditChangeLines()<CR>
        nnoremap <buffer> <silent> cw           :<C-U>call PareditChangeSpec('cw',1)<CR>
        nnoremap <buffer> <silent> cb           :<C-U>call PareditChangeSpec('cb',0)<CR>
        nnoremap <buffer> <silent> ciw          :<C-U>call PareditChangeSpec('ciw',1)<CR>
        nnoremap <buffer> <silent> caw          :<C-U>call PareditChangeSpec('caw',1)<CR>
        nnoremap <buffer> <silent> p            :<C-U>call PareditPut('p')<CR>
        nnoremap <buffer> <silent> P            :<C-U>call PareditPut('P')<CR>
        execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'w(  :<C-U>call PareditWrap("(",")")<CR>'
        execute 'vnoremap <buffer> <silent> ' . g:paredit_leader.'w(  :<C-U>call PareditWrapSelection("(",")")<CR>'
        execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'w"  :<C-U>call PareditWrap('."'".'"'."','".'"'."')<CR>"
        execute 'vnoremap <buffer> <silent> ' . g:paredit_leader.'w"  :<C-U>call PareditWrapSelection('."'".'"'."','".'"'."')<CR>"
        " Spliec s-expression killing backward/forward
        execute 'nmap     <buffer> <silent> ' . g:paredit_leader.'<Up>    d[(,S'
        execute 'nmap     <buffer> <silent> ' . g:paredit_leader.'<Down>  d])%,S'
        execute 'nmap     <buffer> <silent> ' . g:paredit_leader.'I   :<C-U>call PareditRaise()<CR>'
        if &ft =~ '.*\(clojure\|scheme\).*'
            inoremap <buffer> <expr>   [            PareditInsertOpening('[',']')
            inoremap <buffer> <silent> ]            <C-R>=(pumvisible() ? "\<lt>C-Y>" : "")<CR><C-O>:let save_ve=&ve<CR><C-O>:set ve=all<CR><C-O>:<C-U>call PareditInsertClosing('[',']')<CR><C-O>:let &ve=save_ve<CR>
            inoremap <buffer> <expr>   {            PareditInsertOpening('{','}')
            inoremap <buffer> <silent> }            <C-R>=(pumvisible() ? "\<lt>C-Y>" : "")<CR><C-O>:let save_ve=&ve<CR><C-O>:set ve=all<CR><C-O>:<C-U>call PareditInsertClosing('{','}')<CR><C-O>:let &ve=save_ve<CR>
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'w[  :<C-U>call PareditWrap("[","]")<CR>'
            execute 'vnoremap <buffer> <silent> ' . g:paredit_leader.'w[  :<C-U>call PareditWrapSelection("[","]")<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'w{  :<C-U>call PareditWrap("{","}")<CR>'
            execute 'vnoremap <buffer> <silent> ' . g:paredit_leader.'w{  :<C-U>call PareditWrapSelection("{","}")<CR>'
        endif

        if g:paredit_shortmaps
            " Shorter keymaps: old functionality of KEY is remapped to <Leader>KEY
            nnoremap <buffer> <silent> <            :<C-U>call PareditMoveLeft()<CR>
            nnoremap <buffer> <silent> >            :<C-U>call PareditMoveRight()<CR>
            nnoremap <buffer> <silent> O            :<C-U>call PareditSplit()<CR>
            nnoremap <buffer> <silent> J            :<C-U>call PareditJoin()<CR>
            nnoremap <buffer> <silent> W            :<C-U>call PareditWrap('(',')')<CR>
            vnoremap <buffer> <silent> W            :<C-U>call PareditWrapSelection('(',')')<CR>
            nnoremap <buffer> <silent> S            :<C-U>call PareditSplice()<CR>
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'<  :<C-U>normal! <<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'>  :<C-U>normal! ><CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'O  :<C-U>normal! O<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'J  :<C-U>normal! J<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'W  :<C-U>normal! W<CR>'
            execute 'vnoremap <buffer> <silent> ' . g:paredit_leader.'W  :<C-U>normal! W<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'S  :<C-U>normal! S<CR>'
        else
            " Longer keymaps with <Leader> prefix
            nnoremap <buffer> <silent> S            V:<C-U>call PareditChange(visualmode(),1)<CR>
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'<  :<C-U>call PareditMoveLeft()<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'>  :<C-U>call PareditMoveRight()<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'O  :<C-U>call PareditSplit()<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'J  :<C-U>call PareditJoin()<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'W  :<C-U>call PareditWrap("(",")")<CR>'
            execute 'vnoremap <buffer> <silent> ' . g:paredit_leader.'W  :<C-U>call PareditWrapSelection("(",")")<CR>'
            execute 'nnoremap <buffer> <silent> ' . g:paredit_leader.'S  :<C-U>call PareditSplice()<CR>'
        endif

        if g:paredit_electric_return && mapcheck( "<CR>", "i" ) == ""
            " Do not override any possible mapping for <Enter>
            inoremap <buffer> <expr>   <CR>         PareditEnter()
        endif
    else
        " Paredit mode is off: remove keybindings
        silent! iunmap <buffer> (
        silent! iunmap <buffer> )
        silent! iunmap <buffer> "
        silent! iunmap <buffer> <BS>
        silent! iunmap <buffer> <Del>
        silent! unmap  <buffer> (
        silent! unmap  <buffer> )
        silent! unmap  <buffer> [[
        silent! unmap  <buffer> ]]
        silent! unmap  <buffer> x
        silent! unmap  <buffer> <Del>
        silent! unmap  <buffer> X
        silent! unmap  <buffer> s
        silent! unmap  <buffer> D
        silent! unmap  <buffer> C
        silent! unmap  <buffer> d
        silent! unmap  <buffer> c
        silent! unmap  <buffer> dd
        silent! unmap  <buffer> cc
        silent! unmap  <buffer> cw
        silent! unmap  <buffer> cb
        silent! unmap  <buffer> ciw
        silent! unmap  <buffer> caw
        if &ft =~ '.*\(clojure\|scheme\).*'
            silent! iunmap <buffer> [
            silent! iunmap <buffer> ]
            silent! iunmap <buffer> {
            silent! iunmap <buffer> }
        endif
        if mapcheck( "<CR>", "i" ) == "PareditEnter()"
            " Remove only if we have added this mapping
            silent! iunmap <buffer> <CR>
        endif
    endif
endfunction

" General Paredit operator function
function! PareditOpfunc( func, type, visualmode )
    let sel_save = &selection
    let ve_save = &virtualedit
    set virtualedit=all
    let regname = v:register
    let save_0 = getreg( '0' )

    if a:visualmode  " Invoked from Visual mode, use '< and '> marks.
        silent exe "normal! `<" . a:type . "`>"
    elseif a:type == 'line'
        let &selection = "inclusive"
        silent exe "normal! '[V']"
    elseif a:type == 'block'
        let &selection = "inclusive"
        silent exe "normal! `[\<C-V>`]"
    else
        let &selection = "inclusive"
        silent exe "normal! `[v`]"
    endif

    if !g:paredit_mode || a:visualmode && a:type == 'block' || a:type == "\<C-V>"
        " Block mode is too difficult to handle at the moment
        silent exe "normal! d"
        let putreg = getreg( '"' )
    else
        silent exe "normal! y"
        let putreg = getreg( '"' )
        if a:func == 'd'
            " Register "0 is corrupted by the above 'y' command
            call setreg( '0', save_0 ) 
        endif

        " Find and keep unbalanced matched characters in the region
        let endingwhitespace = matchstr(putreg, "\\s*$")
        let instring = s:InsideString( line("'<"), col("'<") )
        if col("'>") > 1 && !s:InsideString( line("'<"), col("'<") - 1 )
            " We are at the beginning of the string
            let instring = 0
        endif
        let matched = s:GetMatchedChars( putreg, instring, s:InsideComment( line("'<"), col("'<") ) )
        let matched = s:Unbalanced( matched )
        let matched = substitute( matched, '\s', '', 'g' )
        if a:func == 'c'
            let matched = matched . endingwhitespace
        endif

        if matched == ''
            silent exe "normal! gvd"
        else
            silent exe "normal! gvc" . matched
            silent exe "normal! l"
            if a:func == 'c'
                silent exe "normal! " . string(len(endingwhitespace)) . "h"
            endif
        endif
    endif

    let &selection = sel_save
    let &virtualedit = ve_save
    if a:func == 'd' && regname == '"'
        " Do not currupt the '"' register and hence the "0 register
        call setreg( '1', putreg ) 
    else
        call setreg( regname, putreg ) 
    endif
endfunction

" Set delete mode also saving repeat count
function! PareditSetDelete( count )
    let s:repeat = a:count
    set opfunc=PareditDelete
endfunction

" General delete operator handling
function! PareditDelete( type, ... )
    call PareditOpfunc( 'd', a:type, a:0 )
    if s:repeat > 1
        call feedkeys( (s:repeat-1) . "." )
    endif
    let s:repeat = 0
endfunction

" General change operator handling
function! PareditChange( type, ... )
    let ve_save = &virtualedit
    set virtualedit=all
    call PareditOpfunc( 'c', a:type, a:0 )
    startinsert
    let &virtualedit = ve_save
endfunction

" Delete v:count number of lines
function! PareditDeleteLines()
    if v:count > 1
        silent exe "normal! V" . (v:count-1) . "j\<Esc>"
    else
        silent exe "normal! V\<Esc>"
    endif
    call PareditDelete(visualmode(),1)
endfunction

" Change v:count number of lines
function! PareditChangeLines()
    if v:count > 1
        silent exe "normal! V" . (v:count-1) . "j\<Esc>"
    else
        silent exe "normal! V\<Esc>"
    endif
    call PareditChange(visualmode(),1)
endfunction

" Handle special change command, e.g. cw
" Check if we may revert to its original Vim function
" This way '.' can be used to repeat the command
function! PareditChangeSpec( cmd, dir )
    let line = getline( '.' )
    if a:dir == 0
        " Changing backwards
        let c =  col( '.' ) - 2
        while c >= 0 && line[c] =~ b:any_matched_char
            " Shouldn't delete a matched character, just move left
            call feedkeys( 'h', 'n')
            let c = c - 1
        endwhile
        if c < 0 && line[0] =~ b:any_matched_char
            " Can't help, still on matched character, insert instead
            call feedkeys( 'i', 'n')
            return
        endif
    else
        " Changing forward
        let c =  col( '.' ) - 1
        while c < len(line) && line[c] =~ b:any_matched_char
            " Shouldn't delete a matched character, just move right
            call feedkeys( 'l', 'n')
            let c = c + 1
        endwhile
        if c == len(line)
            " Can't help, still on matched character, append instead
            call feedkeys( 'a', 'n')
            return
        endif
    endif
    
    " Safe to use Vim's built-in change function
    call feedkeys( a:cmd, 'n')
endfunction

" Paste text from put register in a balanced way
function! PareditPut( cmd )
    let regname = v:register
    let reg_save = getreg( regname )
    let putreg = reg_save

    " Find unpaired matched characters by eliminating paired ones
    let matched = s:GetMatchedChars( putreg, s:InsideString(), s:InsideComment() )
    let matched = s:Unbalanced( matched )

    if matched !~ '\S\+'
        " Register contents is balanced, perform default put function
        silent exe "normal! " . (v:count>1 ? v:count : '') . (regname=='"' ? '' : '"'.regname) . a:cmd
        return
    endif

    " Replace all unpaired matched characters with a space in order to keep balance
    let i = 0
    while i < len( putreg )
        if matched[i] !~ '\s'
            let putreg = strpart( putreg, 0, i ) . ' ' . strpart( putreg, i+1 )
        endif
        let i = i + 1
    endwhile

    " Store balanced text in put register and call the appropriate put command
    call setreg( regname, putreg ) 
    silent exe "normal! " . (v:count>1 ? v:count : '') . (regname=='"' ? '' : '"'.regname) . a:cmd
    call setreg( regname, reg_save ) 
endfunction

" Toggle paredit mode
function! PareditToggle()
    " Don't disable paredit if it was not initialized yet for the current buffer
    if exists( 'b:paredit_init') || g:paredit_mode == 0
        let g:paredit_mode = 1 - g:paredit_mode
    endif
    echo g:paredit_mode ? 'Paredit mode on' : 'Paredit mode off'
    call PareditInitBuffer()
endfunction

" Does the current syntax item match the given regular expression?
function! s:SynIDMatch( regexp, line, col, match_eol )
    let col  = a:col
    if a:match_eol && col > len( getline( a:line ) )
        let col = col - 1
    endif
    return synIDattr( synID( a:line, col, 0), 'name' ) =~ a:regexp
endfunction

" Is the current cursor position inside a comment?
function! s:InsideComment( ... )
    let l = a:0 ? a:1 : line('.')
    let c = a:0 ? a:2 : col('.')
    if &syntax == ''
        " No help from syntax engine,
        " remove strings and search for ';' up to the cursor position
        let line = strpart( getline(l), 0, c - 1 )
        let line = substitute( line, '\\"', '', 'g' )
        let line = substitute( line, '"[^"]*"', '', 'g' )
        return match( line, ';' ) >= 0
    endif
    return s:SynIDMatch( '[Cc]omment', l, c, 1 )
endfunction

" Is the current cursor position inside a string?
function! s:InsideString( ... )
    let l = a:0 ? a:1 : line('.')
    let c = a:0 ? a:2 : col('.')
    if &syntax == ''
        " No help from syntax engine,
        " count quote characters up to the cursor position
        let line = strpart( getline(l), 0, c - 1 )
        let line = substitute( line, '\\"', '', 'g' )
        let quotes = substitute( line, '[^"]', '', 'g' )
        return len(quotes) % 2
    endif
    return s:SynIDMatch( '[Ss]tring', l, c, 0 )
endfunction

" Is this a Slimv or VimClojure REPL buffer?
function! s:IsReplBuffer()
    if exists( 'g:slimv_repl_name' )
        return bufnr( g:slimv_repl_name ) == bufnr( '%' )
    elseif exists( 'b:vimclojure_repl' )
        return 1
    else
        return 0
    endif
endfunction

" Get Slimv or VimClojure REPL buffer last command prompt position
" Return [0, 0] if this is not the REPL buffer
function! s:GetReplPromptPos()
    if !s:IsReplBuffer()
        return [0, 0]
    endif
    if exists( 'b:vimclojure_repl')
        let cur_pos = getpos( '.' )
        call cursor( line( '$' ), 1)
        call cursor( line( '.' ), col( '$') )
        call search( b:vimclojure_namespace . '=>', 'bcW' )
        let target_pos = getpos( '.' )[1:2]
        call setpos( '.', cur_pos )
        return target_pos
    else
        return [ b:repl_prompt_line, b:repl_prompt_col ]
    endif
endfunction

" Is the current top level form balanced, i.e all opening delimiters
" have a matching closing delimiter
function! s:IsBalanced()
    let l = line( '.' )
    let c =  col( '.' )
    let line = getline( '.' )
    if c > len(line)
        let c = len(line)
    endif
    let matchb = max( [l-g:paredit_matchlines, 1] )
    let matchf = min( [l+g:paredit_matchlines, line('$')] )
    let [prompt, cp] = s:GetReplPromptPos()
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

    if &ft =~ '.*\(clojure\|scheme\).*'
        let b1 = searchpair( '\[', '', '\]', 'brnmW', s:skip_sc, matchb )
        let b2 = searchpair( '\[', '', '\]',  'rnmW', s:skip_sc, matchf )
        if !(b1 == b2) && !(b1 == b2 - 1 && line[c-1] == '[') && !(b1 == b2 + 1 && line[c-1] == ']')
            " Number of opening and closing brackets differ
            return 0
        endif
        let b1 = searchpair( '{', '', '}', 'brnmW', s:skip_sc, matchb )
        let b2 = searchpair( '{', '', '}',  'rnmW', s:skip_sc, matchf )
        if !(b1 == b2) && !(b1 == b2 - 1 && line[c-1] == '{') && !(b1 == b2 + 1 && line[c-1] == '}')
            " Number of opening and closing curly braces differ
            return 0
        endif
    endif
    return 1
endfunction

" Filter out all non-matched characters from the region
function! s:GetMatchedChars( lines, start_in_string, start_in_comment )
    let inside_string  = a:start_in_string
    let inside_comment = a:start_in_comment
    let matched = repeat( ' ', len( a:lines ) )
    let i = 0
    while i < len( a:lines )
        if inside_string
            " We are inside a string, skip parens, wait for closing '"'
            " but skip escaped \" characters
            if a:lines[i] == '"' && a:lines[i-1] != '\'
                let matched = strpart( matched, 0, i ) . a:lines[i] . strpart( matched, i+1 )
                let inside_string = 0
            endif
        elseif inside_comment
            " We are inside a comment, skip parens, wait for end of line
            if a:lines[i] == "\n"
                let inside_comment = 0
            endif
        elseif i > 0 && a:lines[i-1] == '\' && (i < 2 || a:lines[i-2] != '\')
            " This is an escaped character, ignore it
        else
            " We are outside of strings and comments, now we shall count parens
            if a:lines[i] == '"'
                let matched = strpart( matched, 0, i ) . a:lines[i] . strpart( matched, i+1 )
                let inside_string = 1
            endif
            if a:lines[i] == ';'
                let inside_comment = 1
            endif
            if a:lines[i] =~ b:any_openclose_char
                let matched = strpart( matched, 0, i ) . a:lines[i] . strpart( matched, i+1 )
            endif
        endif
        let i = i + 1
    endwhile
    return matched
endfunction

" Find unpaired matched characters by eliminating paired ones
function! s:Unbalanced( matched )
    let matched = a:matched
    let tmp = matched
    while 1
        let matched = tmp
        let tmp = substitute( tmp, '(\(\s*\))',   ' \1 ', 'g')
        if &ft =~ '.*\(clojure\|scheme\).*'
            let tmp = substitute( tmp, '\[\(\s*\)\]', ' \1 ', 'g')
            let tmp = substitute( tmp, '{\(\s*\)}',   ' \1 ', 'g')
        endif
        let tmp = substitute( tmp, '"\(\s*\)"',   ' \1 ', 'g')
        if tmp == matched
            " All paired chars eliminated
            let tmp = substitute( tmp, ')\(\s*\)(',   ' \1 ', 'g')
            if &ft =~ '.*\(clojure\|scheme\).*'
                let tmp = substitute( tmp, '\]\(\s*\)\[', ' \1 ', 'g')
                let tmp = substitute( tmp, '}\(\s*\){',   ' \1 ', 'g')
            endif
            if tmp == matched
                " Also no more inverse pairs can be eliminated
                break
            endif
        endif
    endwhile
    return matched
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
        if &selection == 'inclusive'
            " Trim last character from the selection, it will be included anyway
            normal! oho
        endif
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

" Returns the nearest opening character to the cursor
" Used for smart jumping in Clojure
function! PareditSmartJumpOpening( select )
    let [paren_line, paren_col] = searchpairpos('(', '', ')', 'bWn', s:skip_sc)
    let [bracket_line, bracket_col] = searchpairpos('\[', '', '\]', 'bWn', s:skip_sc)
    let [brace_line, brace_col] = searchpairpos('{', '', '}', 'bWn', s:skip_sc)
    let paren_score = paren_line * 10000 + paren_col
    let bracket_score = bracket_line * 10000 + bracket_col
    let brace_score = brace_line * 10000 + brace_col
    if (brace_score > paren_score || paren_score == 0) && (brace_score > bracket_score || bracket_score == 0) && brace_score != 0
	call PareditFindOpening('{','}', a:select)
    elseif (bracket_score > paren_score || paren_score == 0) && bracket_score != 0
	call PareditFindOpening('[',']', a:select)
    else
	call PareditFindOpening('(',')', a:select)
    endif
endfunction

" Returns the nearest opening character to the cursor
" Used for smart jumping in Clojure
function! PareditSmartJumpClosing( select )
    let [paren_line, paren_col] = searchpairpos('(', '', ')', 'Wn', s:skip_sc)
    let [bracket_line, bracket_col] = searchpairpos('\[', '', '\]', 'Wn', s:skip_sc)
    let [brace_line, brace_col] = searchpairpos('{', '', '}', 'Wn', s:skip_sc)
    let paren_score = paren_line * 10000 + paren_col
    let bracket_score = bracket_line * 10000 + bracket_col
    let brace_score = brace_line * 10000 + brace_col
    if (brace_score < paren_score || paren_score == 0) && (brace_score < bracket_score || bracket_score == 0) && brace_score != 0
	call PareditFindClosing('{','}', a:select)
    elseif (bracket_score < paren_score || paren_score == 0) && bracket_score != 0
	call PareditFindClosing('[',']', a:select)
    else
	call PareditFindClosing('(',')', a:select)
    endif
endfunction

" Find defun start backwards
function! PareditFindDefunBck()
    let l = line( '.' )
    let matchb = max( [l-g:paredit_matchlines, 1] )
    let oldpos = getpos( '.' ) 
    let newpos = searchpairpos( '(', '', ')', 'brW', s:skip_sc, matchb )
    if newpos[0] == 0
        " Already standing on a defun, find the end of the previous one
        let newpos = searchpos( ')', 'bW' )
        while newpos[0] != 0 && (s:InsideComment() || s:InsideString())
            let newpos = searchpos( ')', 'W' )
        endwhile
        if newpos[0] == 0
            " No ')' found, don't move cursor
            call setpos( '.', oldpos )
        else
            " Find opening paren
            let pairpos = searchpairpos( '(', '', ')', 'brW', s:skip_sc, matchb )
            if pairpos[0] == 0
                " ')' has no matching pair
                call setpos( '.', oldpos )
            endif
        endif
    endif
endfunction

" Find defun start forward
function! PareditFindDefunFwd()
    let l = line( '.' )
    let matchf = min( [l+g:paredit_matchlines, line('$')] )
    let oldpos = getpos( '.' ) 
    call searchpair( '(', '', ')', 'brW', s:skip_sc, matchf )
    normal! %
    let newpos = searchpos( '(', 'W' )
    while newpos[0] != 0 && (s:InsideComment() || s:InsideString())
        let newpos = searchpos( '(', 'W' )
    endwhile
    if newpos[0] == 0
        " No '(' found, don't move cursor
        call setpos( '.', oldpos )
    endif
endfunction

" Insert opening type of a paired character, like ( or [.
function! PareditInsertOpening( open, close )
    if !g:paredit_mode || s:InsideComment() || s:InsideString() || !s:IsBalanced()
        return a:open
    endif
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if pos > 0 && line[pos-1] == '\' && (pos < 2 || line[pos-2] != '\')
        " About to enter a \( or \[
        return a:open
    elseif line[pos] !~ b:any_wsclose_char && pos < len( line )
        " Add a space after if needed
        let retval = a:open . a:close . " \<Left>\<Left>"
    else
        let retval = a:open . a:close . "\<Left>"
    endif
    if pos > 0 && line[pos-1] !~ b:any_wsopen_char && line[pos-1] !~ s:any_macro_prefix
        " Add a space before if needed
        let retval = " " . retval
    endif
    return retval
endfunction

" Re-gather electric returns up
function! s:ReGatherUp()
    if g:paredit_electric_return && getline('.') =~ '^\s*)'
        " Re-gather electric returns in the current line for ')'
        normal! k
        while getline( line('.') ) =~ '^\s*$'
            " Delete all empty lines
            normal! ddk
        endwhile
        normal! Jl
    elseif g:paredit_electric_return && getline('.') =~ '^\s*\(\]\|}\)' && &ft =~ '.*\(clojure\|scheme\).*' 
        " Re-gather electric returns in the current line for ']' and '}'
        normal! k
        while getline( line('.') ) =~ '^\s*$'
            " Delete all empty lines
            normal! ddk
        endwhile
        call setline( line('.'), substitute( line, '\s*$', '', 'g' ) )
        normal! Jxl
    endif
    " Already have the desired character, move right
    normal! l
endfunction

" Insert closing type of a paired character, like ) or ].
function! PareditInsertClosing( open, close )
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if !g:paredit_mode || s:InsideComment() || s:InsideString() || !s:IsBalanced()
        call setline( line('.'), line[0 : pos-1] . a:close . line[pos : -1] )
        normal! l
        return
    endif
    if pos > 0 && line[pos-1] == '\' && (pos < 2 || line[pos-2] != '\')
        " About to enter a \) or \]
        call setline( line('.'), line[0 : pos-1] . a:close . line[pos : -1] )
        normal! l
        return
    elseif line[pos] == a:close
        call s:ReGatherUp()
        return
    endif
    let open  = escape( a:open , '[]' )
    let close = escape( a:close, '[]' )
    let newpos = searchpairpos( open, '', close, 'nW', s:skip_sc )
    if g:paredit_electric_return && newpos[0] > line('.')
        " Closing paren is in a line below, check if there are electric returns to re-gather
        while getline('.') =~ '^\s*$'
            " Delete all empty lines above the cursor
            normal! ddk
        endwhile
        let oldpos = getpos( '.' ) 
        normal! j
        while getline('.') =~ '^\s*$'
            " Delete all empty lines below the cursor
            normal! dd
        endwhile
        let nextline = substitute( getline('.'), '\s', '', 'g' )
        call setpos( '.', oldpos )
        if len(nextline) > 0 && nextline[0] == ')'
            " Re-gather electric returns in the line of the closing ')'
            call setline( line('.'), substitute( getline('.'), '\s*$', '', 'g' ) )
            normal! Jl
            return
        endif
        if len(nextline) > 0 && nextline[0] =~ '\]\|}' && &ft =~ '.*\(clojure\|scheme\).*' 
            " Re-gather electric returns in the line of the closing ']' or '}'
            call setline( line('.'), substitute( line, '\s*$', '', 'g' ) )
            normal! Jxl
            return
        endif
    elseif g:paredit_electric_return && line =~ '^\s*)'
        " Re-gather electric returns in the current line
        call s:ReGatherUp()
        return
    endif
    if searchpair( open, '', close, 'W', s:skip_sc ) > 0
        normal! l
    endif
    "TODO: indent after going to closing character
endfunction

" Insert an (opening or closing) double quote
function! PareditInsertQuotes()
    if !g:paredit_mode || s:InsideComment()
        return '"'
    endif
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if pos > 0 && line[pos-1] == '\' && (pos < 2 || line[pos-2] != '\')
        " About to enter a \"
        return '"'
    elseif s:InsideString()
        "TODO: skip comments in search(...)
        if line[pos] == '"'
            " Standing on a ", just move to the right
            return "\<Right>"
        elseif search('[^\\]"\|^"', 'nW') == 0
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

" Handle <Enter> keypress, insert electric return if applicable
function! PareditEnter()
    let line = getline( '.' )
    let pos = col( '.' ) - 1
    if g:paredit_electric_return && pos > 0 && line[pos] =~ b:any_closing_char && !s:InsideString() && s:IsBalanced()
        " Electric Return
        return "\<CR>\<CR>\<Up>"
    else
        " Regular Return
        return "\<CR>"
    endif
endfunction

" Handle <BS> keypress
function! PareditBackspace( repl_mode )
    let [lp, cp] = s:GetReplPromptPos()
    if a:repl_mode && line( "." ) == lp && col( "." ) <= cp
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
    elseif s:InsideString() && line[pos-1] =~ b:any_openclose_char
        " Deleting a paren inside a string
        return "\<BS>"
    elseif pos > 1 && line[pos-1] =~ b:any_matched_char && line[pos-2] == '\' && (pos < 3 || line[pos-3] != '\')
        " Deleting an escaped matched character
        return "\<BS>\<BS>"
    elseif line[pos-1] !~ b:any_matched_char
        " Deleting a non-special character
        return "\<BS>"
    elseif line[pos-1] != '"' && !s:IsBalanced()
        " Current top-form is unbalanced, can't retain paredit mode
        return "\<BS>"
    endif

    if line[pos-1:pos] =~ b:any_matched_pair
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
    elseif line[pos] == '\' && line[pos+1] =~ b:any_matched_char && (pos < 1 || line[pos-1] != '\')
        " Deleting an escaped matched character
        return "\<Del>\<Del>"
    elseif line[pos] !~ b:any_matched_char
        " Erasing a non-special character
        return "\<Del>"
    elseif line[pos] != '"' && !s:IsBalanced()
        " Current top-form is unbalanced, can't retain paredit mode
        return "\<Del>"
    elseif pos == 0
        return "\<Right>"
    endif

    if line[pos-1:pos] =~ b:any_matched_pair
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
    let ve_save = &virtualedit
    set virtualedit=all
    let c = a:count
    while c > 0
        if line[pos] == '\' && line[pos+1] =~ b:any_matched_char && (pos < 1 || line[pos-1] != '\')
            " Erasing an escaped matched character
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
        elseif pos > 0 && line[pos-1:pos] =~ b:any_matched_pair
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
        elseif line[pos] =~ b:any_matched_char
            " Character-pair is not empty, don't erase just move inside
            call s:AddYankPos( len(reg) )
            let pos = pos + 1
            normal! l
        elseif pos < len(line) && pos >= a:startcol
            " Erasing a non-special character
            let reg = reg . line[pos]
            let line = strpart( line, 0, pos ) . strpart( line, pos+1 )
        endif
        let c = c - 1
    endwhile
    let &virtualedit = ve_save
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
        if pos > 1 && line[pos-2] == '\' && line[pos-1] =~ b:any_matched_char && (pos < 3 || line[pos-3] != '\')
            " Erasing an escaped matched character
            let reg = reg . line[pos-2 : pos-1]
            let line = strpart( line, 0, pos-2 ) . strpart( line, pos )
            normal! h
            let pos = pos - 1
        elseif s:InsideComment() || ( s:InsideString() && line[pos-1] != '"' )
            let reg = reg . line[pos-1]
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos )
        elseif line[pos-1:pos] =~ b:any_matched_pair
            " Erasing an empty character-pair
            let p2 = s:RemoveYankPos()
            let reg = strpart( reg, 0, p2 ) . line[pos-1] . strpart( reg, p2 )
            let reg = reg . line[pos]
            let line = strpart( line, 0, pos-1 ) . strpart( line, pos+1 )
        elseif line[pos-1] =~ b:any_matched_char
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
                if line[c-1] =~ b:any_closing_char
                    " Skip to the beginning of this sub-expression
                    let symbol_pos = [l, c]
                    normal! %
                    let line2 = getline( '.' )
                    let c2 = col('.') - 1
                    if c2 > 0 && line2[c2-1] =~ s:any_macro_prefix
                        normal! h
                    endif
                elseif line[c-1] =~ b:any_opening_char
                    " Opening delimiter found: stop
                    call setpos( '.', [0, l0, c0, 0] )
                    return [0, 0]
                elseif line[c-1] =~ '\S'
                    " Previous symbol starting
                    let symbol_pos = [l, c]
                endif
            else
                if line[c-1] =~ b:any_opening_char || (a:skip_whitespc && line[c-1] =~ '\S' && symbol_end != [0, 0])
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
                    return [l, c + ([l, c] == [l1, c1])]
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
            if line[c-1] =~ s:any_macro_prefix && line[c] =~ b:any_opening_char
                " Skip to the end of this prefixed sub-expression
                let symbol_pos = [l, c]
                normal! l%
            elseif line[c-1] =~ b:any_opening_char
                " Skip to the end of this sub-expression
                let symbol_pos = [l, c]
                normal! %
            elseif line[c-1] =~ b:any_closing_char
                " Closing delimiter found: stop
                call setpos( '.', [0, l0, c0, 0] )
                return [0, 0]
            elseif line[c-1] =~ '\S'
                " Next symbol starting
                let symbol_pos = [l, c]
            endif
        else
            if line[c-1] =~ b:any_closing_char || (a:skip_whitespc && line[c-1] =~ '\S' && symbol_end != [0, 0])
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
    if line[c0-1] !~ b:any_openclose_char
        " OK, we are not standing on a paren to move, but check if there is one nearby
        if (c0 < 2 || line[c0-2] !~ b:any_openclose_char) && line[c0] =~ b:any_openclose_char
            normal! l
        elseif c0 > 1 && line[c0-2] =~ b:any_openclose_char && line[c0] !~ b:any_openclose_char
            normal! h
        endif
    endif

    " Skip macro prefix character    
    let c0 =  col( '.' )
    if line[c0-1] =~ s:any_macro_prefix && line[c0] =~ b:any_opening_char
        normal! l
    endif

    " If still not standing on a paren then find the next closing one
    if line[c0-1] !~ b:any_openclose_char
        call search(b:any_closing_char, 'W')
    endif
endfunction

" Move delimiter one atom or s-expression to the left
function! PareditMoveLeft()
    call s:FindParenNearby()

    let line = getline( '.' )
    let l0 = line( '.' )
    let c0 =  col( '.' )

    if line[c0-1] =~ b:any_opening_char
        let closing = 0
    elseif line[c0-1] =~ b:any_closing_char
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
    if closing && line[c+1] !~ b:any_wsclose_char
        " Insert a space after if needed
        execute "normal! a "
        normal! h
    endif
    if !closing && c > 0 && line[c-len] !~ b:any_wsopen_char
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

    if line[c0-1] =~ b:any_opening_char
        let opening = 1
    elseif line[c0-1] =~ b:any_closing_char
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
    if opening && c0 > 1 && line[c0-2] =~ s:any_macro_prefix
        call s:MoveChar( l0, c0-1, l1, c1 )
        call s:MoveChar( l0, c0-1, l1, c1 + (l0 != l1) )
        let len = 2
    else
        call s:MoveChar( l0, c0, l1, c1 )
        let len = 1
    endif
    let line = getline( '.' )
    let c =  col( '.' ) - 1
    if opening && c > 0 && line[c-len] !~ b:any_wsopen_char
        " Insert a space before if needed
        if len > 1
            execute "normal! hi "
            normal! ll
        else
            execute "normal! i "
            normal! l
        endif
    endif
    if !opening && line[c+1] !~ b:any_wsclose_char
        " Insert a space after if needed
        execute "normal! a "
        normal! h
    endif
endfunction

" Find closing of the innermost structure: (...) or [...] or {...}
" Return a list where first element is the closing character,
" second and third is its position (line, column)
function! s:FindClosing()
    let l = line( '.' )
    let c = col( '.' )
    let paren = ''
    let l2 = 0
    let c2 = 0

    call PareditFindClosing( '(', ')', 0 )
    let lp = line( '.' )
    let cp = col( '.' )
    if [lp, cp] != [l, c]
        " Do we have a closing ')'?
        let paren = ')'
        let l2 = lp
        let c2 = cp
    endif
    call setpos( '.', [0, l, c, 0] )

    if &ft =~ '.*\(clojure\|scheme\).*'
        call PareditFindClosing( '[', ']', 0 )
        let lp = line( '.' )
        let cp = col( '.' )
        if [lp, cp] != [l, c] && (lp < l2 || (lp == l2 && cp < c2))
            " Do we have a ']' closer?
            let paren = ']'
            let l2 = lp
            let c2 = cp
        endif
        call setpos( '.', [0, l, c, 0] )

        call PareditFindClosing( '{', '}', 0 )
        let lp = line( '.' )
        let cp = col( '.' )
        if [lp, cp] != [l, c] && (lp < l2 || (lp == l2 && cp < c2))
            " Do we have a '}' even closer?
            let paren = '}'
            let l2 = lp
            let c2 = cp
        endif
        call setpos( '.', [0, l, c, 0] )
    endif

    return [paren, l2, c2]
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
            if c == 0 || (c > 0 && getline('.')[c-1] =~ b:any_wsopen_char)
                " OK, we are standing on the first character of the symbol
            else
                normal! b
            endif
        endif

        " First find which kind of paren is the innermost
        let [p, l, c] = s:FindClosing()
        if p !~ b:any_closing_char
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
        elseif p == '}'
            normal! i} {
        else
            normal! i] [
        endif
    endif
endfunction

" Join two neighboring lists or strings
function! PareditJoin()
    if !g:paredit_mode || s:InsideComment() || s:InsideString()
        return
    endif

    "TODO: skip parens in comments
    let [l0, c0] = searchpos(b:any_matched_char, 'nbW')
    let [l1, c1] = searchpos(b:any_matched_char, 'ncW')
    if [l0, c0] == [0, 0] || [l1, c1] == [0, 0]
        return
    endif
    let line0 = getline( l0 )
    let line1 = getline( l1 )
    let p0 = line0[c0-1]
    let p1 = line1[c1-1]
    if (p0 == ')' && p1 == '(') || (p0 == ']' && p1 == '[') || (p0 == '}' && p1 == '{') || (p0 == '"' && p1 == '"')
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
    if &selection == 'inclusive'
        let c1 = c1 + strlen(matchstr(getline(l1)[c1-1 :], '.'))
    endif
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
endfunction

" Wrap current symbol in parens of the given kind
" If standing on a paren then wrap the whole s-expression
" Stand on the opening paren (if not wrapping in "")
function! PareditWrap( open, close )
    if a:open != '"' && getline('.')[col('.') - 1] =~ b:any_openclose_char
        execute "normal! " . "v%\<Esc>"
    else
        execute "normal! " . "viw\<Esc>"
    endif
    call s:WrapSelection( a:open, a:close )
    if a:open != '"'
        normal! %
    endif
endfunction

" Splice current list into the containing list
function! PareditSplice()
    if !g:paredit_mode
        return
    endif

    " First find which kind of paren is the innermost
    let [p, l, c] = s:FindClosing()
    if p !~ b:any_closing_char
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

" Raise: replace containing form with the current symbol or sub-form
function! PareditRaise()
    if getline('.')[col('.')-1] =~ b:any_openclose_char
        " Raise sub-form and re-indent
        normal! y%d%dab
        normal! "0P=%
    else
        " Raise symbol
        normal! yiwdab
        normal! "0Pb
    endif
endfunction

" =====================================================================
"  Autocommands
" =====================================================================

au BufNewFile,BufRead *.lisp call PareditInitBuffer()
au BufNewFile,BufRead *.cl   call PareditInitBuffer()
au BufNewFile,BufRead *.clj  call PareditInitBuffer()
au BufNewFile,BufRead *.cljs call PareditInitBuffer()
au BufNewFile,BufRead *.scm  call PareditInitBuffer()
au BufNewFile,BufRead *.rkt  call PareditInitBuffer()

