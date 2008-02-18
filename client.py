# run with: c:\Python25\1.py | clisp

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

#TODO: use popen for opening a pipe to subprocess
#TODO: check pty module

"""
(defun square (x) (* x x))
(square 16)
"""


import os
import sys
import time
#import msvcrt # for getch()
import socket
#import select
#from errno import EALREADY, EINPROGRESS, EWOULDBLOCK, ECONNRESET, ENOTCONN
#from threading import Thread

HOST		= ''		# Symbolic name meaning the local host
PORT		= 7171		# Arbitrary non-privileged port

debug_level	= 0		# Debug level for diagnostic messages


def log( s, level ):
	"""
	Print diagnostic messages according to the actual debug level
	"""
	if debug_level >= level:
		print s


if __name__ == "__main__":

	s = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
	s.connect( ( 'localhost', PORT ) )

	if len( sys.argv ) < 2:
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
		for line in sys.argv[1:]:
			s.send( line )
			time.sleep(0.1)

	log( 'closing', 1 )
	s.close()

