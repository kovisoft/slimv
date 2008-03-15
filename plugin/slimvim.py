# run with: c:\Python24\1.py | clisp

# :py import vim
# :py import sys
# :'<,'>py sys.argv = [''] + vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
# :pyfile client.py

# :'<,'>py for l in vim.current.buffer[vim.current.range.start:vim.current.range.end+1]: print l

#TODO: check pty module
#TODO: merge with server.py, run server upon request (-s command line switch)

nolisptest = 0

import os
import sys
import getopt
import time
import shlex
#import msvcrt # for getch()
import socket
#import select
#from errno import EALREADY, EINPROGRESS, EWOULDBLOCK, ECONNRESET, ENOTCONN
#from threading import Thread
if not nolisptest:	#TODO: support older python versions that does not have subprocess module
	from subprocess import *
from threading import Thread

autoconnect	= 1		# Start and connect server automatically

HOST		= ''		# Symbolic name meaning the local host
PORT		= 5151		# Arbitrary non-privileged port. TODO: make this configurable

debug_level	= 0		# Debug level for diagnostic messages
terminate	= 0		# Main program termination flag

buffer		= ''
buflen		= 0

#python_path     = 'python24.exe'
python_path     = 'c:/Python24/python'
#lisp_path       = 'clisp.exe'
lisp_path       = 'clisp.exe'
#slimvim_path    = os.environ.get['$VIMR'] + '/vimfiles/plugin/slimvim.py' #TODO: get this from caller
#slimvim_path    = os.environ.get['$VIMRUNTIME'] + '/plugin/slimvim.py' #TODO: get this from caller
slimvim_path    = 'slimvim.py'
run_cmd		= ''
# Linux:
#python_path = '/opt/python2.5/usr/local/bin/python'
#lisp_path   = '/usr/bin/sbcl'

mswindows = (sys.platform == 'win32')

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

def connect_server():
	"""Try to connect server, if server not found then spawn it.
	   Return socket object on success, None on failure.
	"""
	global python_path
	global run_cmd
	global autoconnect

	s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
	try:
		s.connect( ( 'localhost', PORT ) )
	except socket.error, msg:
		if autoconnect:
			s.close()
			if run_cmd == '':
				if mswindows:
					cmd = [python_path, slimvim_path, '-l', lisp_path, '-s']
				else:
					#cmd = ['xterm', '-e', python_path, slimvim_path, '-l', lisp_path, '-s &']
					cmd = ['xterm', '-e', python_path, slimvim_path, '-l', lisp_path, '-s']
			else:
			    cmd = shlex.split(run_cmd)
			if mswindows:
				from win32process import CREATE_NEW_CONSOLE
				server = Popen( cmd, creationflags=CREATE_NEW_CONSOLE )
			else:
				#TODO support older python versions with no subprocess module?
				server = Popen( cmd )
				# call server example: 'xterm -e python slimvim.py -l sbcl -s'

			s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
			try:
				time.sleep( 2.0 )	# Allow subprocess to start
				s.connect( ( 'localhost', PORT ) )
			except socket.error, msg:
				s.close()
				s =  None
		else:	# not autoconnect
			print "Server not found"
			s = None
	return s


def send_line( server, line ):
	"""Send a line to the swank server:
	   first send line length in 4 bytes, then send the line itself.
	   All backslash+n character-pairs are converted to newline.
	"""
	line = line.replace( '\\n', '\n' )
	#if line.find( '\n' ) < 0:
	#	line = line + '\n'
	l = len(line)
	lstr = chr(l&255) + chr((l>>8)&255) + chr((l>>16)&255) + chr((l>>24)&255)
	server.send( lstr )		# make it a swank protocol
	server.send( line )
	time.sleep(0.01)


def client( args ):
	"""Main client routine: starts server if needed then send text to server.
	"""
	s = connect_server()
	if s is None:
		return

	if len( args ) < 1:
		# No command line arguments specified, read input from stdin
		while 1:
			try:
				line = raw_input()
				send_line( s, line )
			except ( EOFError, KeyboardInterrupt ):
				log( 'breaking', 1 )
				break

	else:
		# Send command line arguments to the server
		print args
		for line in args:
			send_line( s, line )

	log( 'closing', 1 )
	s.close()


###############################################################################
#
# Server part
#
###############################################################################

class socket_listener( Thread ):
	"""Server thread to receive text from the client via socket.
	"""

	def __init__ ( self, inp ):
		Thread.__init__( self )
		self.inp = inp

	def run( self ):
		global buffer
#		global buflen
		global terminate
		self.s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
		log( "sl.bind", 1 )
		self.s.bind( (HOST, PORT) )
		while not terminate:
			log( "sl.listen", 1 )
			self.s.listen( 1 )
			conn, addr = self.s.accept()
			while not terminate:
				l = 0
				lstr = ''
				# swank server: read length first
				log( "sl.recv len", 1 )
				try:
					lstr = conn.recv(4)
					if len( lstr ) <= 0:
						break;
				except:
					break
				if terminate:
					break
				l = ord(lstr[0]) + (ord(lstr[1])<<8) + (ord(lstr[2])<<16) + (ord(lstr[3])<<24)
				if l > 0:
					log( "sl.recv data", 1 )
					try:
						received = conn.recv(l)
						if len( received ) < l:
							break
					except:
						break
					self.inp.write( received + '\n' )
					buffer = buffer + received + '\n'
			log( "sl.close", 1 )
			conn.close()


class input_listener( Thread ):
	"""Server thread to receive input from console.
	"""

	def __init__ ( self, inp ):
		Thread.__init__( self )
		self.inp = inp

	def run( self ):
		global buffer
		global terminate
		log( "il.start", 1 )
		while not terminate:
			try:
				log( "il.raw_input", 1 )
				self.inp.write( raw_input() + '\n' )
			except EOFError:
				log( "il.EOFError", 1 )
				terminate = 1
			except KeyboardInterrupt:
				log( "il.KeyboardInterrupt", 1 )
				terminate = 1



class output_listener( Thread ):
	"""Server thread to receive REPL output.
	"""

	def __init__ ( self, out ):
		Thread.__init__( self )
		self.out = out

	def run( self ):
		global buffer
		global terminate
		log( "ol.start", 1 )
		while not terminate:
			#line = self.out.readline()
			log( "ol.read", 1 )
			try:
				c = self.out.read(1)
				buffer = buffer + c
			except:
				break


def buffer_read_and_display():
	"""Read and display lines received in global buffer.
	"""
	global buffer
	global buflen

	l = len( buffer )
	while buflen < l:
		try:
			sys.stdout.write( buffer[buflen] )
			buflen = buflen + 1
		except:
			break


def server( args ):
	"""Main server routine: starts REPL and helper threads for
	   sending and receiving data to/from REPL.
	"""
	global lisp_path
	global terminate
	global buffer
	global buflen
	global nolisptest

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

	if nolisptest:
		il = input_listener( sys.stdout )
		il.start()
		sl = socket_listener( sys.stdout )
		sl.start()
	else:
		cmd = shlex.split( lisp_path )
		if mswindows:
			from win32con import CREATE_NO_WINDOW
			p1 = Popen( cmd, stdin=PIPE, stdout=PIPE, stderr=STDOUT, \
					creationflags=CREATE_NO_WINDOW )
		else:
			p1 = Popen( cmd, stdin=PIPE, stdout=PIPE, stderr=STDOUT )
	#	p1 = Popen(["c:\\lispbox\\clisp-2.37\\clisp.exe"], stdin=PIPE, stdout=PIPE, stderr=PIPE,
	#			creationflags=win32con.CREATE_NO_WINDOW)
		ol = output_listener( p1.stdout )
		ol.start()
		il = input_listener( p1.stdin )
		il.start()
		sl = socket_listener( p1.stdin )
		sl.start()

	log( "in.start", 1 )
	sys.stdout.write( ";;; Slimvim is starting Lisp...\n" )
	time.sleep(0.5)			# wait for Lisp to start
	buffer_read_and_display()	# read Lisp startup messages
	sys.stdout.write( ";;; Slimvim connection established\n" )
	while not terminate:
		try:
			log( "in.step", 1 )
			time.sleep(0.01)
			#time.sleep(1)
			buffer_read_and_display()
			#l = len( buffer )
			#while buflen < l:
			#	sys.stdout.write( buffer[buflen] )
			#	buflen = buflen + 1
#			time.sleep(1)
#			log( 'check buflen', 1 )
#			log( "in.raw_input", 1 )
#			p1.stdin.write( raw_input() + '\n' )

		except EOFError:
			log( "in.EOFError", 1 )
			terminate = 1
		except KeyboardInterrupt:
			log( "in.KeyboardInterrupt", 1 )
			terminate = 1

#	p1.communicate()
	# The socket is opened only for waking up the server thread
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
	if not nolisptest:
		#TODO: put this in a try-block
	#	p1.stdin.write( "(exit)\n" )
		try:
			p1.stdin.close()
		except:
			# We don't care if this above fails, we'll exit anway
			pass
		#p1.stdout.close()

	#print 'Come back soon...'
	print 'Thank you for using Slimvim.'

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
	print 'Usage: ', progname + ' [-d LEVEL] [-s] [-c ARGS]'
	print
	print 'Options:'
	print '  -?, -h, --help             show this help message and exit'
	print '  -p, --python=PATH          path of Python interpreter'
	print '  -l, --lisp=PATH            path of Lisp interpreter'
	print '  -r, --run=PATH             full command to run the server'
	print '  -d LEVEL, --debug=LEVEL    set debug LEVEL (0..3)'
	print '  -s                         start server'
	print '  -c LINE1 LINE2 ... LINEn   start client mode and send LINE1...LINEn to server'
	print '                             (if present, this option must be the last one)'


###############################################################################
#
# Main program
#
###############################################################################

if __name__ == '__main__':
#	global python_path
#	global lisp_path

	EXIT, SERVER, CLIENT = range( 3 )
	mode = EXIT
	slimvim_path = sys.argv[0]
	python_path = sys.executable
	if python_path.find( ' ' ) >= 0:
		python_path = '"' + python_path + '"'

	# Get command line options
	try:
		opts, args = getopt.getopt( sys.argv[1:], '?hcsp:l:r:d:', \
                                            ['help', 'client', 'server', 'python=', 'lisp=', 'run=', 'debug='] )

		# Process options
		for o, a in opts:
			if o in ('-?', '-h', '--help'):
				usage()
				break
			if o in ('-p', '--python'):
				python_path = a
			if o in ('-l', '--lisp'):
				lisp_path = a
			if o in ('-r', '--run'):
				run_cmd = a
			if o in ('-d', '--debug'):
				debug_level = int(a)
			if o in ('-s', '--server'):
				mode = SERVER
			if o in ('-c', '--client'):
				mode = CLIENT

	except getopt.GetoptError:
		# print help information and exit:
		usage()

	if mode == SERVER:
		server( args )

	if mode == CLIENT:
		if run_cmd != '':
			# It is possible to pass special argument placeholders to run_cmd
			#print run_cmd
			run_cmd = run_cmd.replace( '@p', escape_path( python_path ) )
			run_cmd = run_cmd.replace( '@s', escape_path( slimvim_path ) )
			run_cmd = run_cmd.replace( '@l', escape_path( lisp_path ) )
			run_cmd = run_cmd.replace( '@@', '@' )
			#run_cmd = run_cmd.replace( '"', '\\"' )
			#print run_cmd
		client( args )

