# run with: c:\Python24\1.py | clisp

# Special characters:
# 08		Backspace
# 16		Ctrl+V
# E0 52		Shift+Ins
# E0 48		Up
# E0 50		Down

#TODO: use popen for opening a pipe to subprocess
#TODO: check pty module
#TODO: make buffer a cStringIO.StringIO object

import os
import sys
import time
#import msvcrt # for getch()
import socket
#import select
import subprocess
from subprocess import *
#from errno import EALREADY, EINPROGRESS, EWOULDBLOCK, ECONNRESET, ENOTCONN
from threading import Thread
import win32con

HOST		= ''		# Symbolic name meaning the local host
PORT		= 7171		# Arbitrary non-privileged port

debug_level	= 0		# Debug level for diagnostic messages
terminate	= 0		# Main program termination flag

buffer		= ''
buflen		= 0

def log( s, level ):
	"""
	Print diagnostic messages according to the actual debug level
	"""
	if debug_level >= level:
		print s



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


if __name__ == "__main__":

	p1 = Popen(["c:\\lispbox\\clisp-2.37\\clisp.exe"], stdin=PIPE, stdout=PIPE, stderr=STDOUT,
			creationflags=win32con.CREATE_NO_WINDOW)
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

