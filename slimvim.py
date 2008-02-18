# run with: c:\Python24\1.py | clisp

# Special characters:
# 08		Backspace
# 16		Ctrl+V
# E0 52		Shift+Ins
# E0 48		Up
# E0 50		Down

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

mswindows = (sys.platform == "win32")
 

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


def client( argv ):
	s = connect_server()
	if s is None:
		return

	if len( argv ) < 2:
		# No command line arguments specified, read input from stdin
		while 1:
#			if sys.stdout.closed:
#				sys.stdout = os.open( 'CON:', 'wt' ) # 1=stdout
			try:
				line = raw_input()
				s.send( line )
			except ( EOFError, KeyboardInterrupt ):
				#sys.stdout.write( chr( 26 ) + "\n" )
				#sys.stdout.flush()
#				sys.stdout.close()
#				os.close( 1 ) # 1=stdout
				log( 'breaking', 1 )
				break

	else:
		# Send command line arguments to the server
		for line in argv[1:]:
			s.send( line )
			time.sleep(0.01)

	log( 'closing', 1 )
	s.close()

if __name__ == "__main__":
	client( sys.argv )

