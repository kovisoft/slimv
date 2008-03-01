# run with: c:\Python24\1.py | clisp

# :py import vim
# :py import sys
# :'<,'>py sys.argv = [''] + vim.current.buffer[vim.current.range.start:vim.current.range.end+1]
# :pyfile client.py

# :'<,'>py for l in vim.current.buffer[vim.current.range.start:vim.current.range.end+1]: print l

#TODO: check pty module
#TODO: merge with server.py, run server upon request (-s command line switch)


import os
import sys
import getopt
import time
#import msvcrt # for getch()
import socket
#import select
#from errno import EALREADY, EINPROGRESS, EWOULDBLOCK, ECONNRESET, ENOTCONN
#from threading import Thread
from subprocess import *
from threading import Thread

HOST		= ''		# Symbolic name meaning the local host
PORT		= 7171		# Arbitrary non-privileged port. TODO: make this configurable

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

mswindows = (sys.platform == 'win32')
 

def log( s, level ):
	"""
	Print diagnostic messages according to the actual debug level
	"""
	if debug_level >= level:
		print s


def connect_server():
	global python_path

	s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
	try:
		s.connect( ( 'localhost', PORT ) )
	except socket.error, msg:
		s.close()
		#TODO: Modify this to work outside Windows
		#TODO: spawn subprocess only if socket connect failed
		if mswindows:
			from win32process import CREATE_NEW_CONSOLE	#TODO: only for Windows
#			server = Popen( ['c:\\Python24\\python', 'c:\\Python24\\server.py'], creationflags=CREATE_NEW_CONSOLE )
#			server = Popen( [python_path, 'c:\\Python24\\slimvim.py', '-s'], creationflags=CREATE_NEW_CONSOLE )
#			server = Popen( [python_path, slimvim_path, '-p', python_path, '-l', lisp_path, '-s'], \
#					creationflags=CREATE_NEW_CONSOLE )
			server = Popen( [python_path, slimvim_path, '-p', python_path, '-l', lisp_path, '-s'], \
					creationflags=CREATE_NEW_CONSOLE )
		else:
			server = Popen( ['server.py'] )

		s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
		try:
			s.connect( ( 'localhost', PORT ) )
			time.sleep( 1.0 )	# Allow subprocess to start
		except socket.error, msg:
			s.close()
			#sys.exit()
			s =  None
	return s


def client( args ):
	s = connect_server()
	if s is None:
		return

	if len( args ) < 1:
		# No command line arguments specified, read input from stdin
		while 1:
#			if sys.stdout.closed:
#				sys.stdout = os.open( 'CON:', 'wt' ) # 1=stdout
			try:
				line = raw_input()
				s.send( line )
			except ( EOFError, KeyboardInterrupt ):
				#sys.stdout.write( chr( 26 ) + '\n' )
				#sys.stdout.flush()
#				sys.stdout.close()
#				os.close( 1 ) # 1=stdout
				log( 'breaking', 1 )
				break

	else:
		# Send command line arguments to the server
		for line in args:
			s.send( line )
			time.sleep(0.01)

	log( 'closing', 1 )
	s.close()


class socket_listener( Thread ):
	"""
	Server thread to receive text from the client via socket
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
				log( "sl.recv", 1 )
				received = conn.recv(1024)
#				sys.stdout.write( received + '\n' )
#				sys.stdout.flush()
				if len( received ) == 0:
					break
				self.inp.write( received + '\n' )
				#time.sleep(0.1)
#				l = len( buffer )
#				while buflen < l:
#					sys.stdout.write( buffer[buflen] )
#					buflen = buflen + 1
				buffer = buffer + received + '\n'
			log( "sl.close", 1 )
			conn.close()


class input_listener( Thread ):
	"""
	
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
	"""
	
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
			c = self.out.read(1)
			buffer = buffer + c


def server( args ):
	global lisp_path
	global terminate
	global buffer
	global buflen

	if mswindows:
		from win32con import CREATE_NO_WINDOW
		p1 = Popen( [lisp_path], stdin=PIPE, stdout=PIPE, stderr=STDOUT, \
			    creationflags=CREATE_NO_WINDOW )
	else:
		p1 = Popen( [lisp_path], stdin=PIPE, stdout=PIPE, stderr=STDOUT )
#	p1 = Popen(["c:\\lispbox\\clisp-2.37\\clisp.exe"], stdin=PIPE, stdout=PIPE, stderr=PIPE,
#			creationflags=win32con.CREATE_NO_WINDOW)
	ol = output_listener( p1.stdout )
	ol.start()
	il = input_listener( p1.stdin )
	il.start()
	sl = socket_listener( p1.stdin )
	sl.start()
	log( "in.start", 1 )
	while not terminate:
		try:
			log( "in.step", 1 )
			time.sleep(0.01)
			#time.sleep(1)
			l = len( buffer )
			while buflen < l:
				sys.stdout.write( buffer[buflen] )
				buflen = buflen + 1
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
	cs = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
	cs.connect( ( 'localhost', PORT ) )
	cs.send( " " )
	cs.close()

	# Send exit command to child process and
	# wake output listener up at the same time
#	p1.stdin.write( "(exit)\n" )
	p1.stdin.close()
	#p1.stdout.close()

	print 'Come back soon...'

	# Wait for the child process to exit
	time.sleep(1)


def usage():
	"""Displays program usage information
	"""
	progname = os.path.basename( sys.argv[0] )
	print 'Usage: ', progname + ' [-d LEVEL] [-s] [-c ARGS]'
	print
	print 'Options:'
	print '  -?, -h, --help             show this help message and exit'
	print '  -p, --python=PATH          path of Python interpreter'
	print '  -l, --lisp=PATH            path of Lisp interpreter'
	print '  -d LEVEL, --debug=LEVEL    set debug LEVEL (0..3)'
	print '  -s                         start server'
	print '  -c LINE1 LINE2 ... LINEn   start client mode and send LINE1...LINEn to server'
	print '                             (if present, this option must be the last one)'


if __name__ == '__main__':
#	global python_path
#	global lisp_path

	EXIT, SERVER, CLIENT = range( 3 )
	mode = EXIT
	slimvim_path = sys.argv[0]

	# Get command line options
	try:
		opts, args = getopt.getopt( sys.argv[1:], '?hcsp:l:d:', \
                                            ['help', 'client', 'server', 'python=', 'lisp=', 'debug='] )

		# Process options
		for o, a in opts:
			if o in ('-?', '-h', '--help'):
				usage()
				break
			if o in ('-p', '--python'):
				python_path = a
			if o in ('-l', '--lisp'):
				lisp_path = a
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
		client( args )

