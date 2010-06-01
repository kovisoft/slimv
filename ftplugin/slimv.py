#!/usr/bin/env python

###############################################################################
#
# Client/Server code for Slimv
# slimv.py:     Client/Server code for slimv.vim plugin
# Version:      0.6.2
# Last Change:  01 Jun 2010
# Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
# License:      This file is placed in the public domain.
#               No warranty, express or implied.
#               *** ***   Use At-Your-Own-Risk!   *** ***
# 
###############################################################################

import os
import sys
import getopt
import time
import shlex
import socket
import traceback
from subprocess import Popen, PIPE, STDOUT
from threading import Thread, BoundedSemaphore

autoconnect = 1             # Start and connect server automatically

HOST        = ''            # Symbolic name meaning the local host
PORT        = 5151          # Arbitrary non-privileged port

debug_level = 0             # Debug level for diagnostic messages
terminate   = 0             # Main program termination flag

python_path = 'python'      # Path of the Python interpreter (overridden via command line args)
lisp_path   = 'clisp.exe'   # Path of the Lisp interpreter (overridden via command line args)
slimv_path  = 'slimv.py'    # Path of this script (determined later)
run_cmd     = ''            # Complex server-run command (if given via command line args)

newline     = '\n'

# Check if we're running Windows or Mac OS X, otherwise assume Linux
mswindows = (sys.platform == 'win32')
darwin = (sys.platform == 'darwin')

if not (mswindows or darwin):
    import pty

def log( s, level ):
    """Print diagnostic messages according to the actual debug level.
    """
    if debug_level >= level:
        print s


###############################################################################
#
# Client part
#
###############################################################################

def start_server():
    """Spawn server. Does not check if the server is already running.
    """
    if run_cmd == '':
        # Complex run command not given, build it from the information available
        if mswindows or darwin:
            cmd = []
        else:
            cmd = ['xterm', '-T', 'Slimv', '-e']
        cmd = cmd + [python_path, slimv_path, '-p', str(PORT), '-l', lisp_path, '-s']
    else:
        cmd = shlex.split(run_cmd)

    # Start server
    #TODO: put in try-block
    if mswindows:
        CREATE_NEW_CONSOLE = 16
        server = Popen( cmd, creationflags=CREATE_NEW_CONSOLE )
    elif darwin:
        from ScriptingBridge import SBApplication

        term = SBApplication.applicationWithBundleIdentifier_("com.apple.Terminal")
        term.doScript_in_(" ".join(cmd) + " ; exit", 0) 
    else:
        server = Popen( cmd )

    # Allow subprocess (server) to start
    time.sleep( 2.0 )


def connect_server():
    """Try to connect server, if server not found then spawn it.
       Return socket object on success, None on failure.
    """

    s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
    try:
        s.connect( ( 'localhost', PORT ) )
    except socket.error, msg:
        if autoconnect:
            # We need to try to start the server automatically
            s.close()
            start_server()

            # Open socket to the server
            s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
            try:
                s.connect( ( 'localhost', PORT ) )
            except socket.error, msg:
                s.close()
                s =  None
        else:   # not autoconnect
            print "Server not found"
            s = None
    return s


def send_line( server, line ):
    """Send a line to the server:
       first send line length in 4 bytes, then send the line itself.
    """
    l = len(line)
    lstr = chr(l&255) + chr((l>>8)&255) + chr((l>>16)&255) + chr((l>>24)&255)
    server.send( lstr )     # send message length first
    server.send( line )     # then the message itself


def client_file( input_filename ):
    """Main client routine - input file version:
       starts server if needed then send text to server.
       Input is read from input file.
    """
    s = connect_server()
    if s is None:
        return

    try:
        file = open( input_filename, 'rt' )
        try:
            # Send contents of the file to the server
            lines = ''
            for line in file:
                lines = lines + line
            send_line( s, lines )
        finally:
            file.close()
    except:
        return

    s.close()


###############################################################################
#
# Server part
#
###############################################################################

class repl_buffer:
    def __init__ ( self, output_pipe ):

        self.output   = output_pipe
        self.filename = ''
        self.buffer   = ''
        self.sema     = BoundedSemaphore()
                            # Semaphore to synchronize access to the global display queue

    def setfile( self, filename ):
        """Set output filename. Greet user if this is the first time.
        """
        self.sema.acquire()
        oldname = self.filename
        self.filename = filename
        if oldname == '':
            try:
                # Delete old file creted at a previous run
                os.remove( self.filename )
            except:
                # OK, at least we tried
                pass
            self.write_nolock( newline + ';;; Slimv client is connected to REPL on port ' + str(PORT) + '.' + newline, True )
            user = None
            if mswindows:
                user = os.getenv('USERNAME')
            else:
                user = os.getenv('USER')
            if not user:
                self.write_nolock( ';;; This could be the start of a beautiful program.' + newline + newline, True )
            else:
                self.write_nolock( ';;; ' + user + ', this could be the start of a beautiful program.' + newline + newline, True )
        self.sema.release()

    def writebegin( self ):
        """Begin the writing process. The process is protected by a semaphore.
        """
        self.sema.acquire()

    def writeend( self ):
        """Finish the writing process. Release semaphore
        """
        self.sema.release()

    def write_nolock( self, text, fileonly=False ):
        """Write text into the global display queue buffer.
           The writing process is not protected.
        """
        if not fileonly:
            try:
                # Write all lines to the display
                os.write( self.output.fileno(), text )
            except:
                pass

        if self.filename != '':
            tries = 4
            while tries > 0:
                try:
                    file = open( self.filename, 'at' )
                    try:
                        #file.write( text )
                        if self.buffer != '':
                            # There is output pending
                            os.write(file.fileno(), self.buffer )
                            self.buffer = ''
                        os.write(file.fileno(), text )
                    finally:
                        file.close()
                    tries = 0
                except IOError:
                    tries = tries - 1
                    if tries == 0:
                        traceback.print_exc()
                    time.sleep(0.05)
                except:
                    tries = tries - 1
                    time.sleep(0.05)
        elif len( self.buffer ) < 2000:
            # No filename supplied, collect output info a buffer until filename is given
            # We collect only some bytes, then probably no filename will be given at all
            self.buffer = self.buffer + text

    def write( self, text, fileonly=False ):
        """Write text into the global display queue buffer.
           The writing process is protected by a semaphome.
        """
        self.writebegin()
        self.write_nolock( text, fileonly )
        self.writeend()


class socket_listener( Thread ):
    """Server thread to receive text from the client via socket.
    """

    def __init__ ( self, inp, buffer, pid ):
        Thread.__init__( self )
        self.inp = inp
        self.buffer = buffer
        self.pid = pid

    def run( self ):
        global terminate

        # Open server socket
        self.s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
        self.s.bind( (HOST, PORT) )

        while not terminate:
            # Listen server socket
            self.s.listen( 1 )
            conn, addr = self.s.accept()

            while not terminate:
                l = 0
                lstr = ''
                # Read length first, it comes in 4 bytes
                try:
                    lstr = conn.recv(4)
                    if len( lstr ) <= 0:
                        break
                except:
                    traceback.print_exc()
                    break
                if terminate:
                    break
                l = ord(lstr[0]) + (ord(lstr[1])<<8) + (ord(lstr[2])<<16) + (ord(lstr[3])<<24)
                if l > 0:
                    # Valid length received, now wait for the message
                    try:
                        # Read the message itself
                        received = ''
                        while len( received ) < l:
                            r = conn.recv(l)
                            if len( r ) == 0:
                                break
                            received = received + r
                        if len( received ) < l:
                            break
                    except:
                        traceback.print_exc()
                        break

                    if len(received) >= 7 and received[0:7] == 'SLIMV::':
                        command = received[7:]
                        if len(command) >= 9 and command[0:9] == 'INTERRUPT':
                            try:
                                if mswindows:
                                    import win32api
                                    CTRL_C_EVENT = 0
                                    win32api.GenerateConsoleCtrlEvent( CTRL_C_EVENT, 0 )
                                else:
                                    import signal
                                    os.kill( self.pid, signal.SIGINT )
                            except:
                                # OK, at least we tried
                                # Go on without interruption
                                pass
                        if len(command) >= 8 and command[0:8] == 'OUTPUT::':
                            output_filename = command[8:].rstrip( '\n' )
                            self.buffer.setfile( output_filename )
                    else:
                        # Fork here: write message to the stdin of REPL
                        # and also write it to the display (display queue buffer)
                        self.buffer.writebegin()
                        self.buffer.write_nolock( received )
                        os.write(self.inp.fileno(), received)
                        self.buffer.writeend()

            conn.close()


class output_listener( Thread ):
    """Server thread to receive REPL output.
    """

    def __init__ ( self, out, buffer ):
        Thread.__init__( self )
        self.out = out
        self.buffer = buffer

    def run( self ):
        global terminate

        while not terminate:
            try:
                # Read input from the stdout of REPL
                # and write it to the display (display queue buffer)
                if mswindows:
                    c = self.out.read( 1 )
                    if ord( c ) == 0x0D:
                        # Special handling of 0x0D+0x0A on Windows
                        c2 = self.out.read( 1 )
                        if ord( c2 ) == 0x0A:
                            self.buffer.write( '\n' )
                        else:
                            self.buffer.write( c )
                            self.buffer.write( c2 )
                    else:
                        self.buffer.write( c )
                elif darwin:
                    c = self.out.read( 1 )
                    self.buffer.write( c )
                else:
                    c = self.out.read( 1 )
                    if ord( c ) != 0x0D:
                        self.buffer.write( c )
            except:
                terminate = 1


def server():
    """Main server routine: starts REPL and helper threads for
       sending and receiving data to/from REPL.
    """
    global terminate

    # First check if server already runs
    s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
    try:
        s.connect( ( 'localhost', PORT ) )
    except socket.error, msg:
        # Server not found, our time has come, we'll start a new server in a moment
        pass
    else:
        # Server found, nothing to do here
        s.close()
        print "Server is already running"
        return

    # Build Lisp-starter command
    lisp_exp = lisp_path.replace( '\\', '\\\\' )
    if not mswindows:
        # Popen does not work with tilde-prefix on Linux
        # so we expand them to the home directory
        user = os.path.expanduser( '~/' )
        lisp_exp = lisp_exp.replace( ' ~/', ' ' + user )
    cmd = shlex.split( lisp_exp )

    # Start Lisp
    if mswindows or darwin:
        repl = Popen( cmd, stdin=PIPE, stdout=PIPE, stderr=STDOUT )
        repl_stdin = repl.stdin
        repl_stdout = repl.stdout
        repl_pid = repl.pid
    else:
        repl_pid, repl_fd = pty.fork()
        if repl_pid == 0:
            os.execvp( cmd[0], cmd )
            os._exit(1)
        repl_stdin = repl_stdout = os.fdopen( repl_fd )

    buffer = repl_buffer( sys.stdout )

    # Create and start helper threads
    sl = socket_listener( repl_stdin, buffer, repl_pid )
    sl.start()
    ol = output_listener( repl_stdout, buffer )
    ol.start()

    # Allow Lisp to start, confuse it with some fancy Slimv messages
    sys.stdout.write( ";;; Slimv server is started on port " + str(PORT) + newline )
    sys.stdout.write( ";;; Slimv is spawning REPL..." + newline + newline )
    time.sleep(0.5)             # wait for Lisp to start
    #sys.stdout.write( ";;; Slimv connection established" + newline )

    # Main server loop
    while not terminate:
        try:
            # Read input from the console and write it
            # to the stdin of REPL
            text = raw_input()
            os.write( repl_stdin.fileno(), text + newline )
            buffer.write( text + newline, True )
        except EOFError:
            # EOF (Ctrl+Z on Windows, Ctrl+D on Linux) pressed?
            terminate = 1
        except KeyboardInterrupt:
            # Interrupted from keyboard (Ctrl+C)?
            # We just ignore it here, it will be propagated to the child anyway
            pass

    # The socket is opened here only for waking up the server thread
    # in order to recognize the termination message
    #TODO: exit REPL if this script is about to exit
    cs = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
    try:
        cs.connect( ( 'localhost', PORT ) )
        cs.send( " " )
    finally:
        # We don't care if this above fails, we'll exit anyway
        cs.close()

    # Send exit command to child process and
    # wake output listener up at the same time
    try:
        repl_stdin.close()
    except:
        # We don't care if this above fails, we'll exit anyway
        pass

    # Be nice
    print 'Thank you for using Slimv.'

    # Wait for the child process to exit
    time.sleep(1)


def escape_path( path ):
    """Surround path containing spaces with backslash + double quote,
       so that it can be passed as a command line argument.
    """
    if path.find( ' ' ) < 0:
        return path
    if path[0:2] == '\\\"':
        return path
    elif path[0] == '\"':
        return '\\' + path + '\\'
    else:
        return '\\\"' + path + '\\\"'


def usage():
    """Displays program usage information.
    """
    progname = os.path.basename( sys.argv[0] )
    print 'Usage: ', progname + ' [-d LEVEL] [-s] [-f INFILE]'
    print
    print 'Options:'
    print '  -?, -h, --help                show this help message and exit'
    print '  -l PATH, --lisp=PATH          path of Lisp interpreter'
    print '  -r PATH, --run=PATH           full command to run the server'
    print '  -p PORT, --port=PORT          port number to use by the server/client'
    print '  -d LEVEL, --debug=LEVEL       set debug LEVEL (0..3)'
    print '  -s                            start server'
    print '  -f FILENAME, --file=FILENAME  start client and send contents of file'
    print '                                named FILENAME to server'


###############################################################################
#
# Main program
#
###############################################################################

if __name__ == '__main__':

    EXIT, SERVER, CLIENT = range( 3 )
    mode = EXIT
    slimv_path = sys.argv[0]
    python_path = sys.executable
    input_filename = ''

    # Always this trouble with the path/filenames containing spaces:
    # enclose them in double quotes
    if python_path.find( ' ' ) >= 0:
        python_path = '"' + python_path + '"'

    # Get command line options
    try:
        opts, args = getopt.getopt( sys.argv[1:], '?hcsf:p:l:r:d:', \
                                    ['help', 'client', 'server', 'file=', 'port=', 'lisp=', 'run=', 'debug='] )

        # Process options
        for o, a in opts:
            if o in ('-?', '-h', '--help'):
                usage()
                break
            if o in ('-p', '--port'):
                try:
                    PORT = int(a)
                except:
                    # If given port number is malformed, then keep default value
                    pass
            if o in ('-l', '--lisp'):
                lisp_path = a
            if o in ('-r', '--run'):
                run_cmd = a
            if o in ('-d', '--debug'):
                try:
                    debug_level = int(a)
                except:
                    # If given level is malformed, then keep default value
                    pass
            if o in ('-s', '--server'):
                mode = SERVER
            if o in ('-c', '--client'):
                mode = CLIENT
            if o in ('-f', '--file'):
                mode = CLIENT
                input_filename = a

    except getopt.GetoptError:
        # print help information and exit:
        usage()

    if mode == SERVER:
        # We are started in server mode
        server()

    if mode == CLIENT:
        # We are started in client mode
        if run_cmd != '':
            # It is possible to pass special argument placeholders to run_cmd
            run_cmd = run_cmd.replace( '@p', escape_path( python_path ) )
            run_cmd = run_cmd.replace( '@s', escape_path( slimv_path ) )
            run_cmd = run_cmd.replace( '@l', escape_path( lisp_path ) )
            run_cmd = run_cmd.replace( '@@', '@' )
            log( run_cmd, 1 )
        if input_filename != '':
            client_file( input_filename )
        else:
            start_server()

# --- END OF FILE ---
