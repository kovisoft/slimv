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

HOST		= ''		# Symbolic name meaning the local host
PORT		= 7171		# Arbitrary non-privileged port. TODO: make this configurable

debug_level	= 0		# Debug level for diagnostic messages

mswindows = (sys.platform == 'win32')
 

def log( s, level ):
	"""
	Print diagnostic messages according to the actual debug level
	"""
	if debug_level >= level:
		print s


def connect_server():
	s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
	try:
		s.connect( ( 'localhost', PORT ) )
	except socket.error, msg:
		s.close()
		#TODO: Modify this to work outside Windows
		#TODO: spawn subprocess only if socket connect failed
		if mswindows:
			from win32process import CREATE_NEW_CONSOLE	#TODO: only for Windows
			server = Popen( ['c:\\Python24\\python', 'c:\\Python24\\server.py'], creationflags=CREATE_NEW_CONSOLE )
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


def server( args ):
	print 'server', args


def usage():
	"""Displays program usage information
	"""
	progname = os.path.basename( sys.argv[0] )
	print 'Usage: ', progname + ' [-d LEVEL] [-s] [-c ARGS]'
	print
	print 'Options:'
	print '  -?, -h, --help             show this help message and exit'
	print '  -d LEVEL, --debug=LEVEL    set debug LEVEL (0..3)'
	print '  -s                         start server'
	print '  -c LINE1 LINE2 ... LINEn   start client mode and send LINE1...LINEn to server'
	print '                             (if present, this option must be the last one)'


if __name__ == '__main__':

	EXIT, SERVER, CLIENT = range( 3 )
	mode = EXIT

	# Get command line options
	try:
		opts, args = getopt.getopt( sys.argv[1:], '?hcsd:', ['help', 'client', 'server', 'debug='] )

		# Process options
		for o, a in opts:
			if o in ('-?', '-h', '--help'):
				usage()
				break
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

