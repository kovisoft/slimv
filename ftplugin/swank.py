#!/usr/bin/env python)

###############################################################################
#
# SWANK client for Slimv
# swank.py:     SWANK client code for slimv.vim plugin
# Version:      0.8.3
# Last Change:  17 May 2011
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

input_port      = 4005
output_port     = 4006
lenbytes        = 6             # Message length is encoded in this number of bytes
maxmessages     = 50            # Maximum number of messages to receive in one listening session
sock            = None          # Swank socket object
id              = 0             # Message id
debug           = False
log             = False         # Set this to True in order to enable logging
logfile         = 'swank.log'   # Logfile name in case logging is on
current_thread  = '0'
debug_activated = False         # Swank debugger activated
read_string     = None          # Thread and tag in Swank read string mode
prompt          = 'SLIMV'       # Command prompt
package         = 'COMMON-LISP-USER' # Current package
actions         = dict()        # Swank actions (like ':write-string'), by message id
indent_info     = dict()        # Data of :indentation-update


###############################################################################
# Basic utility functions
###############################################################################

def logprint(text):
    if log:
        f = open(logfile, "a")
        f.write(text + '\n')
        f.close()

def logtime(text):
    logprint(text + ' ' + str(time.clock()))

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

class swank_action:
    def __init__ (self, id, name):
        self.id = id
        self.name = name
        self.result = ''
        self.pending = True

def unquote(s):
    if len(s) < 2:
        return s
    if s[0] == '"' and s[-1] == '"':
        t = s[1:-1].replace('\\"', '"')
        return t.replace('\\\\', '\\')
    else:
        return s

def requote(s):
    t = s.replace('\\', '\\\\')
    t = t.replace('"', '\\"')
    return '"' + t + '"'

def make_keys(lst):
    keys = {}
    for i in range(len(lst)):
        if i < len(lst)-1 and lst[i][0] == ':':
            keys[lst[i]] = unquote( lst[i+1] )
    return keys

def parse_plist(lst, keyword):
    for i in range(0, len(lst), 2):
        if keyword == lst[i]:
            return unquote(lst[i+1])
    return ''

def swank_send(text):
    global sock

    logtime('[---Sent---]')
    logprint(text)
    l = hex(len(text))[2:]
    t = '0'*(lenbytes-len(l)) + l + text
    if debug:
        print 'Sending:', t
    try:
        sock.send(t)
    except socket.error:
        sys.stdout.write( 'Socket error when sending to SWANK server.\n' )
        swank_disconnect()

def swank_recv(msglen):
    global sock

    rec = ''
    if msglen > 0:
        sock.setblocking(0)
        ready = select.select([sock], [], [], 0.01) # 0.01: timeout in seconds
        if ready[0]:
            l = msglen
            sock.setblocking(1)
            try:
                data = sock.recv(l)
            except socket.error:
                sys.stdout.write( 'Socket error when receiving from SWANK server.\n' )
                swank_disconnect()
                return rec
            while data and len(rec) < msglen:
                rec = rec + data
                l = l - len(data)
                if l > 0:
                    try:
                        data = sock.recv(l)
                    except socket.error:
                        sys.stdout.write( 'Socket error when receiving from SWANK server.\n' )
                        swank_disconnect()
                        return rec
    return rec

def swank_parse_inspect(struct):
    """
    Parse the swank inspector output
    """
    buf = '\n \nInspecting ' + parse_plist(struct, ':title') + '\n--------------------\n'
    pcont = parse_plist(struct, ':content')
    cont = pcont[0]
    lst = []
    linestart = 0
    for el in cont:
        logprint(str(el))
        if type(el) == list:
            if el[0] == ':action':
                item = '<' + unquote(el[2]) + '> '
            else:
                item = '[' + unquote(el[2]) + '] '
            if linestart < 0:
                lst.append("\n")
                linestart = len(lst)
            lst.insert(linestart, item)
            linestart = -1
            text = unquote(el[1])
            if text[-len(item):] == ' ' * len(item):
                # If possible, remove spaces from the end in the length of item info
                lst.append(text[:-len(item)])
            else:
                lst.append(text)
        else:
            text = unquote(el)
            lst.append(text)
            if text == "\n":
                linestart = len(lst)
    for s in lst:
        buf = buf + s
    buf = buf + '\n \n[<<]'
    return buf

def swank_parse_xref(struct):
    """
    Parse the swank xref output
    """
    buf = ''
    for e in struct:
        buf = buf + unquote(e[0]) + ' - '
        if len(e) > 1:
            key = e[1][0]
            if key == ':error':
                buf = buf + 'no source information\n'
            elif type(unquote(e[1][1])) == str:
                buf = unquote(e[1][1]) + '\n'
            else:
                buf = unquote(e[1][1][1]) + '\n'
    return buf

def swank_parse_compile(struct):
    """
    Parse compiler output
    """
    buf = ''
    warnings = struct[1]
    time = struct[3]
    filename = struct[5]
    if type(warnings) == list:
        buf = '\n' + str(len(warnings)) + ' compiler notes:\n\n'
        for w in warnings:
            msg      = parse_plist(w, ':message')
            severity = parse_plist(w, ':severity')
            if severity[0] == ':':
                severity = severity[1:]
            location = parse_plist(w, ':location')
            if location[0] == ':error':
                # "no error location available"
                buf = buf + '  ' + unquote(location[1]) + '\n'
                buf = buf + '  ' + severity + ': ' + msg + '\n\n'
            else:
                fname   = unquote(location[1][1])
                pos     = location[2][1]
                if location[3] != 'nil':
                    snippet = unquote(location[3][1]).replace('\r', '')
                    buf = buf + snippet + '\n'
                buf = buf + fname + ':' + pos + '\n'
                buf = buf + '  ' + severity + ': ' + msg + '\n\n' 
    else:
        buf = '\nCompilation finished. (No warnings)  [' + time + ' secs]\n\n'
    return buf

def swank_parse_frame_call(struct):
    """
    Parse frame call output
    """
    if type(struct) == list:
        buf = struct[1][1] + '\n'
        #buf = '{{{' + struct[1][1] + '}}}\n'
    else:
        buf = 'No frame call information\n'
    return buf

def swank_parse_frame_source(struct):
    """
    Parse frame source output
    http://comments.gmane.org/gmane.lisp.slime.devel/9961 ;-(
    'Well, let's say a missing feature: source locations are currently not available for code loaded as source.'
    """
    if type(struct) == list and len(struct) == 4:
        buf = ' in ' + struct[1][1] + ' line ' + struct[2][1] + '\n'
    else:
        buf = ' No source line information\n'
    return buf

def swank_parse_locals(struct):
    """
    Parse frame locals output
    """
    if type(struct) == list:
        buf = 'Locals:\n'
        for f in struct:
            name  = parse_plist(f, ':name')
            id    = parse_plist(f, ':id')
            value = parse_plist(f, ':value')
            buf = buf + '  ' + name + ' = ' + value + '\n'
    else:
        buf = 'No locals\n'
    return buf

def swank_listen():
    global output_port
    global debug_activated
    global read_string
    global current_thread
    global prompt
    global package

    retval = ''
    msgcount = 0
    #logtime('[- Listen--]')
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
            logtime('[-Received-]')
            logprint(rec)
            [s, r] = parse_sexpr( rec )
            if debug:
                print 'Parsed:', r
            if len(r) > 0:
                r_id = r[-1]
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
                    s = unquote(r[1])
                    add_prompt = True
                    for k,a in actions.items():
                        if a.pending and a.name.find('eval'):
                            add_prompt = False
                            break
                    if add_prompt:
                        retval = retval + '\n' + s
                        if len(retval) > 0 and retval[-1] != '\n':
                            retval = retval + '\n'
                        retval = retval + prompt + '> '
                    else:
                        retval = retval + s

                elif message == ':read-string':
                    # RERL requests entering a string
                    read_string = r[1:3]

                elif message == ':indentation-update':
                    for el in r[1]:
                        indent_info[ unquote(el[0]) ] = el[2]

                elif message == ':new-package':
                    package = unquote( r[1] )
                    prompt  = unquote( r[2] )

                elif message == ':return':
                    read_string = None
                    result = r[1][0].lower()
                    if type(r_id) == str and r_id in actions:
                        action = actions[r_id]
                        action.pending = False
                    else:
                        action = None
                    if log:
                        logtime('[Actionlist]')
                        for k,a in sorted(actions.items()):
                            if a.pending:
                                pending = 'pending '
                            else:
                                pending = 'finished'
                            logprint("%s: %s %s %s" % (k, str(pending), a.name, a.result))

                    if result == ':ok':
                        params = r[1][1]
                        if type(params) == str:
                            element = params.lower()
                            if element != 'nil':
                                s = unquote(params)
                                retval = retval + s
                                if action:
                                    action.result = retval
                            # List of actions needing a prompt
                            to_prompt = [':describe-symbol', ':undefine-function', ':swank-macroexpand-1', ':swank-macroexpand-all', ':load-file', ':toggle-profile-fdefinition', ':profile-by-substring', ':disassemble-form']
                            if element == 'nil' or (action and action.name in to_prompt):
                                # No more output from REPL, write new prompt
                                if len(retval) > 0 and retval[-1] != '\n':
                                    retval = retval + '\n'
                                retval = retval + prompt + '> '

                        elif type(params) == list:
                            if type(params[0]) == list: 
                                params = params[0]
                            element = ''
                            if type(params[0]) == str: 
                                element = params[0].lower()
                            if element == ':present':
                                # No more output from REPL, write new prompt
                                retval = retval + unquote(params[1][0][0]) + '\n' + prompt + '> '
                            elif element == ':values':
                                if type(params[1]) == list: 
                                    retval = retval + unquote(params[1][0]) + '\n'
                                else:
                                    retval = retval + unquote(params[1]) + '\n' + prompt + '> '
                            elif element == ':suppress-output':
                                pass
                            elif element == ':pid':
                                conn_info = make_keys(params)
                                pid = conn_info[':pid']
                                ver = conn_info.get(':version', 'nil')
                                imp = make_keys( conn_info[':lisp-implementation'] )
                                pkg = make_keys( conn_info[':package'] )
                                package = pkg[':name']
                                prompt = pkg[':prompt']
                                vim.command('let s:swank_version="' + ver + '"')
                                retval = retval + imp[':type'] + '  Port: ' + str(input_port) + '  Pid: ' + pid + '\n; SWANK ' + ver
                                retval = retval + '\n' + prompt + '> '
                                logprint(' Package:' + package + ' Prompt:' + prompt)
                            elif element == ':name':
                                keys = make_keys(params)
                                retval = retval + '  ' + keys[':name'] + ' = ' + keys[':value'] + '\n'
                            elif element == ':title':
                                logprint(str(params))
                                retval = retval + swank_parse_inspect(params)
                            elif element == ':compilation-result':
                                logprint(str(params))
                                filename = params[5]
                                if filename[0] != '"':
                                    filename = '"' + filename + '"'
                                vim.command('let s:compiled_file=' + filename + '')
                                retval = retval + swank_parse_compile(params) + prompt + '> '
                            else:
                                logprint(str(params))
                                if action.name == ':simple-completions':
                                    if type(params) == list and type(params[0]) == str and params[0] != 'nil':
                                        compl = "\n".join(params)
                                        retval = retval + compl.replace('"', '')
                                elif action.name == ':fuzzy-completions':
                                    if type(params) == list and type(params[0]) == list:
                                        compl = "\n".join(map(lambda x: x[0], params))
                                        retval = retval + compl.replace('"', '')
                                elif action.name == ':xref':
                                    retval = retval + swank_parse_xref(r[1][1])
                                    if len(retval) > 0 and retval[-1] != '\n':
                                        retval = retval + '\n'
                                    retval = retval + prompt + '> '
                                elif action.name == ':set-package':
                                    package = unquote(params[0])
                                    prompt = unquote(params[1])
                                    retval = retval + prompt + '> '
                                elif action.name == ':frame-call':
                                    retval = retval + swank_parse_frame_call(params)
                                    retval = retval + prompt + '> '
                                elif action.name == ':frame-source-location':
                                    retval = retval + swank_parse_frame_source(params)
                                    #retval = retval + prompt + '> '
                                elif action.name == ':frame-locals-and-catch-tags':
                                    retval = retval + swank_parse_locals(params)
                                    retval = retval + prompt + '> '
                                elif action.name == ':profiled-functions':
                                    retval = retval + 'Profiled functions:\n'
                                    for f in params:
                                        retval = retval + '  ' + f + '\n'
                                    retval = retval + prompt + '> '
                                if action:
                                    action.result = retval

                    elif result == ':abort':
                        debug_activated = False
                        vim.command('let s:debug_activated=0')
                        if len(r[1]) > 1:
                            retval = retval + '; Evaluation aborted on ' + unquote(r[1][1]) + '\n' + prompt + '> '
                        else:
                            retval = retval + '; Evaluation aborted\n' + prompt + '> '

                elif message == ':inspect':
                    retval = retval + swank_parse_inspect(r[1])

                elif message == ':debug':
                    [thread, level, condition, restarts, frames, conts] = r[1:7]
                    retval = retval + '\n' + unquote(condition[0]) + '\n' + unquote(condition[1]) + '\n\nRestarts:\n'
                    for i in range( len(restarts) ):
                        r0 = unquote( restarts[i][0] )
                        r1 = unquote( restarts[i][1] )
                        retval = retval + str(i).rjust(3) + ': [' + r0 + '] ' + r1 + '\n'
                    retval = retval + '\nBacktrace:\n'
                    for f in frames:
                        frame = str(f[0])
                        ftext = unquote( f[1] )
                        ftext = ftext.replace('\n', '')
                        ftext = ftext.replace('\\\\n', '')
                        retval = retval + frame.rjust(3) + ': ' + ftext + '\n'
                    retval = retval + prompt + '> '

                elif message == ':debug-activate':
                    debug_activated = True
                    vim.command('let s:debug_activated=1')
                    vim.command('let s:debug_move_cursor=1')
                    current_thread = r[1]

                elif message == ':debug-return':
                    debug_activated = False
                    vim.command('let s:debug_activated=0')
                    retval = retval + '; Quit to level ' + r[2] + '\n' + prompt + '> '

                elif message == ':ping':
                    [thread, tag] = r[1:3]
                    swank_send('(:emacs-pong ' + thread + ' ' + tag + ')')
    return retval

def swank_rex(action, cmd, package, thread):
    """
    Send an :emacs-rex command to SWANK
    """
    global id
    id = id + 1
    key = str(id)
    actions[key] = swank_action(key, action)
    form = '(:emacs-rex ' + cmd + ' ' + package + ' ' + thread + ' ' + str(id) + ')\n'
    swank_send(form)

def get_package():
    pkg = vim.eval("s:swank_package")
    if pkg == '':
        return 'nil'
    else:
        return requote(pkg)

def get_indent_info(name):
    indent = ''
    if name in indent_info:
        indent = indent_info[name]
    vc = ":let s:indent='" + indent + "'"
    vim.command(vc)

###############################################################################
# Various SWANK messages
###############################################################################

def swank_connection_info():
    indent_info.clear()
    swank_rex(':connection-info', '(swank:connection-info)', 'nil', 't')

def swank_create_repl():
    swank_rex(':create-repl', '(swank:create-repl nil)', 'nil', 't')

def swank_eval(exp, package):
    cmd = '(swank:listener-eval ' + requote(exp) + ')'
    swank_rex(':listener-eval', cmd, '"'+package+'"', ':repl-thread')

def swank_pprint_eval(exp, package):
    cmd = '(swank:pprint-eval ' + requote(exp) + ')'
    swank_rex(':pprint-eval', cmd, '"'+package+'"', ':repl-thread')

def swank_interrupt():
    swank_send('(:emacs-interrupt :repl-thread)')

def swank_invoke_restart(level, restart):
    cmd = '(swank:invoke-nth-restart-for-emacs ' + level + ' ' + restart + ')'
    swank_rex(':invoke-nth-restart-for-emacs', cmd, 'nil', current_thread)

def swank_throw_toplevel():
    swank_rex(':throw-to-toplevel', '(swank:throw-to-toplevel)', 'nil', current_thread)

def swank_invoke_abort():
    swank_rex(':sldb-abort', '(swank:sldb-abort)', 'nil', current_thread)

def swank_invoke_continue():
    swank_rex(':sldb-continue', '(swank:sldb-continue)', 'nil', current_thread)

def swank_require(contrib):
    cmd = "(swank:swank-require '" + contrib + ')'
    swank_rex(':swank-require', cmd, 'nil', ':repl-thread')

def swank_frame_call(frame):
    cmd = '(swank-backend:frame-call ' + frame + ')'
    swank_rex(':frame-call', cmd, 'nil', current_thread)

def swank_frame_source_loc(frame):
    cmd = '(swank:frame-source-location ' + frame + ')'
    swank_rex(':frame-source-location', cmd, 'nil', current_thread)

def swank_frame_locals(frame):
    cmd = '(swank:frame-locals-and-catch-tags ' + frame + ')'
    swank_rex(':frame-locals-and-catch-tags', cmd, 'nil', current_thread)

def swank_set_package(pkg):
    cmd = '(swank:set-package "' + pkg + '")'
    swank_rex(':set-package', cmd, get_package(), ':repl-thread')

def swank_describe_symbol(fn):
    cmd = '(swank:describe-symbol "' + fn + '")'
    swank_rex(':describe-symbol', cmd, get_package(), 't')

def swank_describe_function(fn):
    cmd = '(swank:describe-function "' + fn + '")'
    swank_rex(':describe-function', cmd, get_package(), 't')

def swank_op_arglist(op):
    cmd = '(swank:operator-arglist "' + op + '" "' + package + '")'
    swank_rex(':operator-arglist', cmd, get_package(), 't')

def swank_completions(symbol):
    cmd = '(swank:simple-completions "' + symbol + '" "' + package + '")'
    swank_rex(':simple-completions', cmd, 'nil', 't')

def swank_fuzzy_completions(symbol):
    cmd = '(swank:fuzzy-completions "' + symbol + '" "' + package + '" :limit 200 :time-limit-in-msec 2000)' 
    swank_rex(':fuzzy-completions', cmd, 'nil', 't')

def swank_undefine_function(fn):
    cmd = '(swank:undefine-function "' + fn + '")'
    swank_rex(':undefine-function', cmd, get_package(), 't')

def swank_return_string(s):
    global read_string
    swank_send('(:emacs-return-string ' + read_string[0] + ' ' + read_string[1] + ' ' + s + ')')
    read_string = None

def swank_inspect(symbol):
    cmd = '(swank:init-inspector "' + symbol + '")'
    swank_rex(':init-inspector', cmd, get_package(), 't')

def swank_inspect_nth_part(n):
    cmd = '(swank:inspect-nth-part ' + str(n) + ')'
    swank_rex(':inspect-nth-part', cmd, 'nil', 't')

def swank_inspector_nth_action(n):
    cmd = '(swank:inspector-call-nth-action ' + str(n) + ')'
    swank_rex(':inspector-call-nth-action', cmd, 'nil', 't')

def swank_inspector_pop():
    swank_rex(':inspector-pop', '(swank:inspector-pop)', 'nil', 't')

def swank_toggle_trace(symbol):
    cmd = '(swank:swank-toggle-trace "' + symbol + '")'
    swank_rex(':swank-toggle-trace', cmd, get_package(), 't')

def swank_untrace_all():
    swank_rex(':untrace-all', '(swank:untrace-all)', 'nil', 't')

def swank_macroexpand(formvar):
    form = vim.eval(formvar)
    cmd = '(swank:swank-macroexpand-1 ' + requote(form) + ')'
    swank_rex(':swank-macroexpand-1', cmd, get_package(), 't')

def swank_macroexpand_all(formvar):
    form = vim.eval(formvar)
    cmd = '(swank:swank-macroexpand-all ' + requote(form) + ')'
    swank_rex(':swank-macroexpand-all', cmd, get_package(), 't')

def swank_disassemble(symbol):
    cmd = '(swank:disassemble-form "' + "'" + symbol + '")'
    swank_rex(':disassemble-form', cmd, get_package(), 't')

def swank_xref(fn, type):
    cmd = "(swank:xref '" + type + " '" + '"' + fn + '")'
    swank_rex(':xref', cmd, get_package(), 't')

def swank_compile_string(formvar):
    form = vim.eval(formvar)
    cmd = '(swank:compile-string-for-emacs ' + requote(form) + ' nil ' + "'((:position 1) (:line 1 1))" + ' nil nil)'
    swank_rex(':compile-string-for-emacs', cmd, get_package(), 't')

def swank_compile_file(name):
    cmd = '(swank:compile-file-for-emacs ' + requote(name) + ' t)'
    swank_rex(':compile-file-for-emacs', cmd, get_package(), 't')

def swank_load_file(name):
    cmd = '(swank:load-file ' + requote(name) + ')'
    swank_rex(':load-file', cmd, get_package(), 't')

def swank_toggle_profile(symbol):
    cmd = '(swank:toggle-profile-fdefinition "' + symbol + '")'
    swank_rex(':toggle-profile-fdefinition', cmd, get_package(), 't')

def swank_profile_substring(s, package):
    if package == '':
        p = 'nil'
    else:
        p = requote(package)
    cmd = '(swank:profile-by-substring ' + requote(s) + ' ' + p + ')'
    swank_rex(':profile-by-substring', cmd, get_package(), 't')

def swank_unprofile_all():
    swank_rex(':unprofile-all', '(swank:unprofile-all)', 'nil', 't')

def swank_profiled_functions():
    swank_rex(':profiled-functions', '(swank:profiled-functions)', 'nil', 't')

def swank_profile_report():
    swank_rex(':profile-report', '(swank:profile-report)', 'nil', 't')

def swank_profile_reset():
    swank_rex(':profile-reset', '(swank:profile-reset)', 'nil', 't')

###############################################################################
# Generic SWANK connection handling
###############################################################################

def swank_connect(portvar, resultvar):
    """
    Create socket to swank server and request connection info
    """
    global sock
    global input_port

    if not sock:
        try:
            input_port = int(vim.eval(portvar))
            swank_server = ('localhost', input_port)
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(swank_server)
            swank_connection_info()
            vim.command('let ' + resultvar + '=""')
            return sock
        except socket.error:
            vim.command('let ' + resultvar + '="SWANK server is not running."')
            sock = None
            return sock
    vim.command('let ' + resultvar + '=""')
    return sock

def swank_disconnect():
    """
    Disconnect from swank server
    """
    global sock
    try:
        # Try to close socket but don't care if doesn't succeed
        sock.close()
    finally:
        sock = None
        vim.command('let s:swank_connected = 0')
        sys.stdout.write( 'Connection to SWANK server is closed.\n' )

def swank_input(formvar):
    form = vim.eval(formvar)
    if read_string:
        # We are in :read-string mode, pass string entered to REPL
        swank_return_string('"' + form + '\n"')
    elif debug_activated and form[0] != '(' and form[0] != ' ':
        # We are in debug mode and an SLDB command follows (that is not an s-expr)
        if form[0] == '#':
            swank_frame_call(form[1:])
            swank_frame_source_loc(form[1:])
            swank_frame_locals(form[1:])
        elif form[0].lower() == 'q':
            swank_throw_toplevel()
        elif form[0].lower() == 'a':
            swank_invoke_abort()
        elif form[0].lower() == 'c':
            swank_invoke_continue()
        else:
            swank_invoke_restart("1", form)
    elif form[0] == '[':
        if form[1] == '-':
            swank_inspector_pop()
        else:
            swank_inspect_nth_part(form[1:-2])
    elif form[0] == '<':
        swank_inspector_nth_action(form[1:-2])
    else:
        # Normal s-expression evaluation
        pkg = vim.eval("s:swank_package")
        if pkg == '':
            pkg = package
        swank_eval(form, pkg)

def swank_output():
    global sock

    if not sock:
        return "SWANK server is not connected."
    result = swank_listen()
    sys.stdout.write(result)
    return result

def swank_response(name):
    #logtime('[-Response-]')
    for k,a in sorted(actions.items()):
        if not a.pending and (name == '' or name == a.name):
            vc = ":let s:swank_action='" + a.name + "'"
            vim.command(vc)
            sys.stdout.write(a.result)
            actions.pop(a.id)
            vc = ":let s:swank_actions_pending=" + str(len(actions))
            vim.command(vc)
            return
    vc = ":let s:swank_action=''"
    vim.command(vc)
    vc = ":let s:swank_actions_pending=" + str(len(actions))
    vim.command(vc)

