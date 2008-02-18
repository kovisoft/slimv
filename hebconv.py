# Converts a Unicode Hebrew text on clipboard to Hebrew codepage format.
# Puts the converted Hebrew text back to the clipboard.
#
# The Hebrew codepage format can be used under VMS
# The program also prints the unicode representation of the text,
# which can be used by Java
#
# Works on Windows only!
# Tamas Kovacs (TKS), 2005
# Version 1.0.1

import string

CF_TEXT		= 1
CF_UNICODETEXT	= 13
#codepage	= 'cp424'
#codepage	= 'cp856'
#codepage	= 'cp862'
codepage	= 'cp1255'

def reverse_string(s):
    """
    Reverse a string
    """
    result = ''
    for c in s:
	result = c + result
    return result

def reverse_words(line):
    """
    Reverse all words in a line
    """
    words = line.split(' ')
    print 'words', repr(words)
    rev = []
    for w in words:
	r = reverse_string(w)
	rev.append(r)
    print 'rev', repr(rev)
    return string.join(rev)

def translate_hebrew(txt):
    """
    Translate a Hebrew input text from Unicode
    format to a Hebrew codepage format
    """
    print 'Converting text:'
    u = repr(txt)
    uni = ''
    skip = 0
    for i in range(len(u)):
	if skip > 0:
	    skip = skip - 1
	else:
	    if i < len(u)-5 and u[i:i+2] == '\u':
		uni = uni + '\u' + u[i+2:i+6].upper()
		skip = 5
	    else:
		uni = uni + u[i]
    print uni
    y = txt.encode('utf-16')
    #print repr(y)
    z = txt.encode(codepage)
#    conv = reverse_words(z)
    conv = reverse_string(z)
    #for c in conv:
	#print ord(c)
    try:
	from win32clipboard import OpenClipboard, EmptyClipboard, SetClipboardData, CloseClipboard
	OpenClipboard(0)
	try:
	    EmptyClipboard()
	    SetClipboardData(CF_TEXT, conv)
	except:
	    print 'Cannot set clipboard data'
	CloseClipboard()
    except:
	print 'Cannot import module: win32clipboard'
    print 'Text converted and replaced on clipboard'

def clp_get_text():
    """
    Get string from clipboard (CF_TEXT format)
    """
    try:
	from win32clipboard import OpenClipboard, GetClipboardData, CloseClipboard
	OpenClipboard(0)
	try:
	    #text = GetClipboardData(CF_TEXT)
	    text = GetClipboardData(CF_UNICODETEXT)
	except:
	    text = ''
	    print 'Cannot get clipboard data'
	CloseClipboard()
    except:
	print 'Cannot import module: win32clipboard'
	text = ''
    return text

if __name__ == "__main__":
    txt = clp_get_text()
    translate_hebrew(txt)
    quit = raw_input('Press ENTER to quit')

