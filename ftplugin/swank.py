#!/usr/bin/env python)

###############################################################################
#
# SWANK client for Slimv
# swank.py:     SWANK client code for slimv.vim plugin
# Version:      0.8.0
# Last Change:  27 Feb 2011
# Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
# License:      This file is placed in the public domain.
#               No warranty, express or implied.
#               *** ***   Use At-Your-Own-Risk!   *** ***
# 
############################################################################### 


import sys
import socket
import time
import select
import string
from threading import Thread

HOST            = ''            # Symbolic name meaning the local host
PORT            = 5151          # Slimv port
input_port      = 4005
output_port     = 4006
lenbytes        = 6             # Message length is encoded in this number of bytes
maxmessages     = 50            # Maximum number of messages to receive in one listening session
sock            = None          # Swank socket object
id              = 0             # Message id
debug           = False
log             = False
current_thread  = '0'
debug_activated = False         # Swank debugger activated
prompt          = 'SLIMV'       # Command prompt
#package         = 'user'        # Current package
package         = 'COMMON-LISP-USER' # Current package


###############################################################################
# Simple Lisp s-expression parser
###############################################################################

# Possible error codes
PARSERR_NOSTARTBRACE        = -1    # s-expression does not start with a '('
PARSERR_NOCLOSEBRACE        = -2    # s-expression does not end with a '('
PARSERR_NOCLOSESTRING       = -3    # string is not closed with double quote
PARSERR_MISSINGLITERAL      = -4    # literal is missing after the escape character
PARSERR_EMPTY               = -5    # s-expression is empty


def parse_comment( sexpr ):
    """Parses a ';' Lisp comment till the end of line, returns comment length
    """
    pos = sexpr.find( '\n' )
    if pos >= 0:
        return pos + 1
    return len( sexpr )

def parse_keyword( sexpr ):
    """Parses a Lisp keyword, returns keyword length
    """
    for pos in range( len( sexpr ) ):
        if sexpr[pos] in string.whitespace + ')]':
            return pos
    return pos

def parse_sub_sexpr( sexpr, opening, closing ):
    """Parses a Lisp sub -expression, returns parsed string length
       and a Python list built from the s-expression,
       expression can be a Clojure style list surrounded by braces
    """
    result = []
    l = len( sexpr )
    for pos in range( l ):
        # Find first opening '(' or '['
        if sexpr[pos] == opening:
            break
        if not sexpr[pos] in string.whitespace:
            # S-expression does not start with '(' or '['
            return [PARSERR_NOSTARTBRACE, result]
    else:
        # Empty s-expression
        return [PARSERR_EMPTY, result]

    pos = pos + 1
    quote_cnt = 0
    while pos < l:
        literal = 0
        if sexpr[pos] == '\\':
            literal = 1
            pos = pos + 1
            if pos == l:
                return [PARSERR_MISSINGLITERAL, result]
        if not literal and sexpr[pos] == '"':
            # We toggle a string
            quote_cnt = 1 - quote_cnt
            if quote_cnt == 1:
                quote_pos = pos
            else:
                result = result + [sexpr[quote_pos:pos+1]]
        elif quote_cnt == 0:
            # We are not in a string
            if not literal and sexpr[pos] == '(':
                # Parse sub expression
                [slen, subresult] = parse_sub_sexpr( sexpr[pos:], '(', ')' )
                if slen < 0:
                    # Sub expression parsing error
                    return [slen, result]
                result = result + [subresult]
                pos = pos + slen - 1
            elif not literal and sexpr[pos] == '[':
                # Parse sub expression
                [slen, subresult] = parse_sub_sexpr( sexpr[pos:], '[', ']' )
                if slen < 0:
                    # Sub expression parsing error
                    return [slen, result]
                result = result + [subresult]
                pos = pos + slen - 1
            elif not literal and sexpr[pos] == closing:
                # End of this sub expression
                return [pos + 1, result]
            elif not literal and sexpr[pos] != closing and sexpr[pos] in ')]':
                # Wrong closing brace/bracket
                return [PARSERR_NOCLOSEBRACE, result]
            elif not literal and sexpr[pos] == ';':
                # Skip coment
                pos = pos + parse_comment( sexpr[pos:] ) - 1
            elif not sexpr[pos] in string.whitespace + '\\':
                # Parse keyword
                klen = parse_keyword( sexpr[pos:] )
                result = result + [sexpr[pos:pos+klen]]
                pos = pos + klen - 1
        pos = pos + 1

    if quote_cnt != 0:
        # Last string is not closed
        return [PARSERR_NOCLOSESTRING, result]
    # Closing ')' or ']' not found
    return [PARSERR_NOCLOSEBRACE, result]

def parse_sexpr( sexpr ):
    """Parses a Lisp s-expression, returns parsed string length
       and a Python list built from the s-expression
    """
    return parse_sub_sexpr( sexpr, '(', ')' )


###############################################################################
# Swank server interface
###############################################################################

def unquote(s):
    if s[0] == '"' and s[-1] == '"':
        return s[1:-1]
    else:
        return s

def make_keys(lst):
    keys = {}
    for i in range(len(lst)):
        if i < len(lst)-1 and lst[i][0] == ':':
            keys[lst[i]] = unquote( lst[i+1] )
    return keys

def swank_send(text):
    global sock

    if log:
        print '[---Sent---]', text
    l = hex(len(text))[2:]
    t = '0'*(lenbytes-len(l)) + l + text
    if debug:
        print 'Sending:', t
    sock.send(t)

def swank_recv(msglen):
    global sock

    rec = ''
    if msglen > 0:
        sock.setblocking(0)
        ready = select.select([sock], [], [], 0.1) # 0.1: timeout in seconds
        if ready[0]:
            l = msglen
            sock.setblocking(1)
            data = sock.recv(l)
            while data and len(rec) < msglen:
                rec = rec + data
                l = l - len(data)
                if l > 0:
                    data = sock.recv(l)
    return rec

def swank_listen():
    global output_port
    global debug_activated
    global current_thread
    global prompt
    global package

    retval = ''
    msgcount = 0
    while msgcount < maxmessages:
        rec = swank_recv(lenbytes)
        if rec == '':
            break
        msgcount = msgcount + 1
        if debug:
            print 'swank_recv received', rec
        msglen = int(rec, 16)
        if debug:
            print 'Received length:', msglen
        if msglen > 0:
            rec = swank_recv(msglen)
            if log:
                print '[-Received-]', rec
            [s, r] = parse_sexpr( rec )
            if debug:
                print 'Parsed:', r
            if len(r) > 0:
                message = r[0].lower()
                if debug:
                    print 'Message:', message

                if message == ':open-dedicated-output-stream':
                    output_port = int( r[1].lower(), 10 )
                    if debug:
                        print ':open-dedicated-output-stream result:', output_port
                    break

                elif message == ':write-string':
                    # REPL has new output to display
                    retval = retval + unquote( r[1] )

                elif message == ':return':
                    result = r[1][0].lower()
                    if result == ':ok':
                        #debug_activated = False
                        actions = r[1][1]
                        if actions != 'nil':
                            if type( actions[0] ) == type( '' ):
                                actions = [actions]
                            for a in actions:
                                action = a[0].lower()
                                if action == ':present':
                                    retval = retval + unquote( a[1][0][0] ) + '\n'
                                elif action == ':values':
                                    retval = retval + a[1][0] + '\n'
                                elif action == ':suppress-output':
                                    pass
                                elif action == ':pid':
                                    conn_info = make_keys( a )
                                    imp = make_keys( conn_info[':lisp-implementation'] )
                                    pkg = make_keys( conn_info[':package'] )
                                    package = pkg[':name']
                                    prompt = pkg[':prompt']
                                    if log:
                                        print 'Implementation:' + imp[':type'] + ' Package:' + package + ' Prompt:' + prompt
                                elif action == ':name':
                                    keys = make_keys( a )
                                    retval = retval + '  ' + keys[':name'] + ' = ' + keys[':value'] + '\n'
                                else:
                                    retval = retval + a + '\n'
                        # No more output from REPL, write new prompt
                        retval = retval + prompt + '> '
                    elif result == ':abort':
                        debug_activated = False
                        retval = retval + '; Evaluation aborted\n' + prompt + '> '
                    #break

                elif message == ':debug':
                    [thread, level, condition, restarts, frames, conts] = r[1:7]
                    retval = retval + condition[0] + '\n' + condition[1] + '\n\nRestarts:\n'
                    for i in range( len(restarts) ):
                        retval = retval + str(i).rjust(3) + ': [' + unquote( restarts[i][0] ) + '] ' + unquote( restarts[i][1] ) + '\n'
                    retval = retval + '\nBacktrace:\n'
                    for f in frames:
                        frame = str(f[0])
                        retval = retval + frame.rjust(3) + ': ' + unquote( f[1] ) + '\n'
                    retval = retval + prompt + '> '

                elif message == ':debug-activate':
                    debug_activated = True
                    current_thread = r[1]
                    #break

                elif message == ':debug-return':
                    debug_activated = False
                    retval = retval + '; Quit to level ' + r[2] + '\n' + prompt + '> '
                    #break
    return retval

def swank_rex(cmd, package, thread):
    global id
    id = id + 1
    form = '(:emacs-rex ' + cmd + ' ' + package + ' ' + thread + ' ' + str(id) + ')\n'
    swank_send(form)

def swank_connection_info():
    cmd = '(swank:connection-info)'
    swank_rex(cmd, 'nil', 't')

def swank_eval(exp, package):
    cmd = '(swank:listener-eval "' + exp + '")'
    swank_rex(cmd, '"'+package+'"', ':repl-thread')

def swank_invoke_restart(level, restart):
    cmd = '(swank:invoke-nth-restart-for-emacs ' + level + ' ' + restart + ')'
    swank_rex(cmd, 'nil', current_thread)

def swank_throw_toplevel():
    cmd = '(swank:throw-to-toplevel)'
    swank_rex(cmd, 'nil', current_thread)

def swank_invoke_abort():
    cmd = '(swank:sldb-abort)'
    swank_rex(cmd, 'nil', current_thread)

def swank_invoke_continue():
    cmd = '(swank:sldb-continue)'
    swank_rex(cmd, 'nil', current_thread)

def swank_frame_locals(frame):
    cmd = '(swank:frame-locals-for-emacs ' + frame + ')'
    swank_rex(cmd, 'nil', current_thread)
    sys.stdout.write( 'Locals:\n' )

#def send_to_vim(msg):
#    cmd = ":call setreg('" + '"' + "s', '" + msg + "')"
#    vim.command(cmd)

def swank_connect(varname):
    """Create socket to swank server and request connection info
    """
    global sock

    if not sock:
        try:
            swank_server = ('localhost', input_port)
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(swank_server)
            swank_connection_info()
            vim.command('let ' + varname + '=""')
            return sock
        except socket.error:
#            sys.stdout.write('SWANK server is not running.')
            vim.command('let ' + varname + '="SWANK server is not running."')
#            send_to_vim('SWANK server is not running.')
            return None
    vim.command('let ' + varname + '=""')
    return sock

def swank_disconnect():
    global sock
    sock.close()
    sock = None

def swank_send_form(form):
    handled = False
    if form != 'exit':
        if debug_activated:
            if form[0] == '#':
                swank_frame_locals(form[1:])
            elif form[0].lower() == 'q':
                swank_throw_toplevel()
            elif form[0].lower() == 'a':
                swank_invoke_abort()
            elif form[0].lower() == 'c':
                swank_invoke_continue()
            else:
                swank_invoke_restart("1", form)
        else:
            swank_eval(form + '\n', package)
        handled = True
        
    return handled

#def swank_append_buffer(bufno, text):
#    if bufno and text != '':
#        buf = vim.buffers[bufno-1]
#        lines = text.split('\n')
#        buf.append(lines)

def swank_input(varname):
    form = vim.eval(varname)
    #sys.stdout.write(form)
    #sys.stdout.write(prompt + '> ' + form)
    return swank_send_form(form)

def swank_output():
    result = swank_listen()
    sys.stdout.write(result)
    return result

