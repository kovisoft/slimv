# Copy ELOS files to local directory via ftp
# Tamas Kovacs (TKS), Essnet 2005

import os
import time
from ftplib import FTP

global localfile

#ftpserver	= 'odinx.essnet.se10.11.3.92'
ftpserver	= '10.11.3.92'
ftpuser		= ''
ftppassword	= ''

# Different project roots used in ELOS
#ftpproj		= ('$1$DGA104:[PROJ.','RF10:[PROJ.','RF5:[PROJ.')
ftpproj		= ('DSA104:[PROJ.','RF10:[PROJ.','RF5:[PROJ.')

# ELOS domains
ftpdomain	= ('ELOS','DB','ISBB','LANG','PUBLIC','TEST', 'GOLD', 'HANN', 'HANSA', 'LK', 'LOKET', 'MBURG', 'NASCO', 'NSL', 'PAK', 'RIGA', 'RK', 'SAZKA', 'SZRT', 'TATTS', 'WIES')

# Connection between (version,domain) and project
ftpconn		= (#V071(   0,   0,     0,     0,       0,     0,     -1,     -1,      -1,   -1,      -1,      -1,      -1,    -1,    -1,     -1,   -1,      -1,     -1,      -1,     -1),
		   (   0,   0,     0,     0,       0,     0,     -1,     -1,      -1,   -1,      -1,      -1,      -1,    -1,    -1,     -1,   -1,      -1,     -1,      -1,     -1),
		   (   1,   2,    -1,     2,       2,     2,      2,      2,       2,    2,       2,       2,       2,     2,     2,      2,    2,       2,      2,       2,      2))

# ELOS versions and directory to retrieve
#ftpversion	= ('V071','V075','V096')
ftpversion	= ('V075','V098')
ftpdirectory	= ('COM','SRC',)

# Where should retrieved data go
localproj	= ('c:/work',)
localversion	= ftpversion
localdomain	= ftpdomain

# retrieve only files with these extensions
extension	= ('COM','SQL','C','H','SC','IFDL','IFDL_DATA','IFDL_PANEL','IFDL_RECORD','IFDL_RESPONSE','LSF')

dirlist = []


def write_callback(line):
	""" Write line to file with ending CR/LF
	"""
	global localfile
    	localfile.write(line)
    	localfile.write('\n')
    
def print_callback(line):
	""" Add line to the directory list
	"""
	if line[0:1] == ' ':
		dirlist[-1] = dirlist[-1] + line
	else:
		dirlist.append(line)
		
def process(line):
	""" Split line to obtain file information and copy file to local directory
	"""
	global localfile
	dot = line.find('.')
	semicol = dot + line[dot:].find(';')
	
	# Get filename
	name = line[:dot]
	
	# Get file extension
	ext = line[dot+1 : semicol]
	
	# Get file version number
	for v1 in range(semicol+1, len(line)):
	    	if line[v1:v1+1] != ' ':
			break
	for v2 in range(v1, len(line)):
	    	if line[v2:v2+1] == ' ':
			break
	ver = line[v1:v2]
	
	# Get file size
	for s1 in range(v2, len(line)):
	    	if line[s1:s1+1] != ' ':
			break
	for s2 in range(s1, len(line)):
	    	if line[s2:s2+1] == ' ':
			break
	size = line[s1:s2]
	
	# Get file date
	for d1 in range(s2, len(line)):
	    	if line[d1:d1+1] != ' ':
			break
	d2 = d1 + 19
	if line[d2:d2+1] != ' ':
		# Day number is one digit
		date = ''
	    	d2 = d2 + 1
	else:
		# Day number is two digits
		date = '0'
	date = date + line[d1:d2]
	
	if ext in extension:
		#print time.strptime(date, '%d-%b-%Y %H:%M:%S')
		localname = ldomain + '/' + name + '.' + ext
		filetime = time.mktime(time.strptime(date, '%d-%b-%Y %H:%M:%S'))
		do_copy = 0
		try:
			# Get local file info (if file exists)
			filestat = os.stat(localname)
			if filestat.st_mtime < filetime:
			    do_copy = 1
		except:
			do_copy = 1
		if do_copy:
			# File not exists or 
			ftpname = name + '.' + ext + ';'  + ver
			namefiller = ' '
			if len(ftpname) < 45:
			    namefiller = ' ' * (45 - len(ftpname))
			sizefiller = ' '
			if len(size) < 12:
			    sizefiller = ' ' * (12 - len(size))
			print ftpname + namefiller + size + sizefiller + date
			# Copy file
			localfile = open(localname, 'wt')
			ftp.retrlines('RETR ' + name + '.' + ext, write_callback)
			localfile.close()
			
			# Set modification date to the original file date (in ftp dir)
			os.utime(ldomain + '/' + name + '.' + ext, (filetime, filetime))
		
		
if __name__ == "__main__":
	from getpass import getpass

	# Create project root directory if not exists
	if not os.path.exists(localproj[0]):
		print 'Creating directory:', localproj[0]
		os.makedirs(localproj[0])
		
	# Ask for username and password if not defined in code
	if ftpuser == '':
		ftpuser = raw_input('Username: ')
	if ftppassword == '':
		#ftppassword = raw_input('Password: ')
		ftppassword = getpass('Password: ')

	# Connect to ftp server
	ftp = FTP(ftpserver, ftpuser, ftppassword)

	proc = 'Y'
	for v in range(len(ftpversion)):
		if proc[0] != 'a' and proc[0] != 'A':
			proc = raw_input('Process version ' + ftpversion[v] + ' (Yes/No/All)? [Y] ')
			proc = proc.replace('\n', '')
			proc = proc.replace('\r', '')
			if proc == '':
				proc = 'Y'
		if proc[0] != 'n' and proc[0] != 'N':
			# Create version directory if not exists
			lversion = localproj[0] + '/' + localversion[v]
			if not os.path.exists(lversion):
				print 'Creating directory:', lversion
				os.makedirs(lversion)

			for d in range(len(ftpdomain)):
				# Check if version contains domain
				if ftpconn[v][d] < 0:
				    continue

				for f in range(len(ftpdirectory)):

					# Get directory list
					dirlist = []
					ftpdir = ftpproj[ftpconn[v][d]] + ftpdomain[d] + '.'  + ftpversion[v] + '.' + ftpdirectory[f] + ']'
					print 'Retrieving directory list:', ftpdir
					ftp.cwd(ftpdir)
					ftp.retrlines('LIST', print_callback)
			
					# Create domain directory if not exists
					ldomain = localproj[0] + '/' + localversion[v] + '/' + localdomain[d]
					if not os.path.exists(ldomain):
						print 'Creating directory:', ldomain
						os.makedirs(ldomain)
					
					print 'Copying files to local directory', ldomain
					for file in dirlist:
						process(file)
						#print file
					print 'Finished:', ftpdir

	try:
		ftp.quit()
	except:
		print "Disconnect error"
	
	# Make sure output window does not disappear if run directly from OS
#	quit = raw_input('Press ENTER to quit')

