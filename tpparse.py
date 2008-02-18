#!/usr/bin/python
# ELOS Transaction Protocol Parser
# Tamas Kovacs (TKS), 2005

import sys
import getopt
import sgmllib
import urllib
import htmllib
import HTMLParser
import formatter
import traceback
import string
import re
from types import TupleType, ListType, IntType, FloatType, StringType
            

#TODO: implement tables with unusual table header, without proper if-else block
#TODO: find a way to work through VPN (currently the page cannot be accessed outside the browser window that has the VPN connection)
#TODO: implement COM transactions

# Debug level:
# 0 = no debug info, normal program output
# 1 = no debug info, verbose program output
# 2 = important debug messages only
# 3 = all debug info
debug = 1

# Default Options
#use_clp_opt = '?'      # Ask if use or not
use_clp_opt = 'Y'       # Use clipboard automatically
quit        = 0         # Process transactions in a loop
#quit       = 1         # Quit immediately, don't process transactions in a loop

need_any    = 0
need_param  = 1
need_oper   = 2

input_file  = ''

# Base of the url of the Transaction Specification
transpec_base_isbb      = 'http://cvs.scigames.at/viewcvs.cgi/*checkout*/central/transpecISBB/'
#transpec_base_isbb     = 'https://webvpn.scigames.at/get/uri/http://cvs/viewcvs.cgi/*checkout*/transpec/'
transpec_base_mainline  = 'http://cvs.scigames.at/viewcvs.cgi/*checkout*/central/transpecMAIN/'
transpec_base           = transpec_base_mainline

# Request type transaction page definitions (in lexical order)
#    txn type,    txn name,         txn webpage
transaction = (
	# COM ACK-s come first, otherwise request would hide ACK
        (  72, 'GameInfoReq. [ACK]',    'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'Wager Req. [ACK]',      'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'WagerReq-GS [ACK]',     'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'SpecialDraws [ACK]',    'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'XID Request [ACK]',     'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'ATS Text [ACK]',        'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'Agent Request [ACK]',   'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'Term Init Job Request [ACK]','TP_T_COM_V?_ACK.html'   , 'appl_header' ),
        (  72, 'GetAccountingAgent [ACK]',   'TP_T_COM_V?_ACK.html'   , 'appl_header' ),
        (  72, 'GetWinResults [ACK]',   'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'GetCswTimeSynch [ACK]', 'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'BonusWinAction [ACK]',  'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'DisqualifyWID [ACK]',   'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'MonetaryTransfer [ACK]','TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'Representative [ACK]',  'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'ExtInfo [ACK]',         'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'Monetary Xfer [ACK]',   'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  72, 'ModSubscr. [ACK]',      'TP_T_COM_V?_ACK.html'        , 'appl_header' ),

	# COM requests
        (   8, 'GameInfoReq.',          'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'Wager Req.',            'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'WagerReq-GS',           'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'SpecialDraws',          'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'XID Request',           'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'ATS Text',              'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'Agent Request',         'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'Term Init Job Request', 'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'GetAccountingAgent',    'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'GetWinResults',         'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'GetCswTimeSynch',       'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'BonusWinAction',        'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'DisqualifyWID',         'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'MonetaryTransfer',      'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'Representative',        'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'ExtInfo',               'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'Monetary Xfer',         'TP_T_COM_V?.html'            , 'appl_header' ),
        (   8, 'ModSubscr.',            'TP_T_COM_V?.html'            , 'appl_header' ),

	# Requests
        (   0,    '???',                ''                            , 'appl_header' ),
        (   1,    '???',                ''                            , 'appl_header' ),
        (   2,    'Logon',              'TP_T_LOGON_V?.html'          , 'appl_header' ),
        (   3,    'Logoff',             'TP_T_LOGOFF_V?.html'         , 'appl_header' ),
        (   4,    'Sell',               'TP_T_SELL_V?.html'           , 'appl_header' ),
        (   5,    'Pay',                'TP_T_PAY_V?.html'            , 'appl_header' ),
        (   6,    'Canc.Sell',          'TP_T_CAN_SELL_V?.html'       , 'appl_header' ),
        (   7,    'Canc.Pay',           'TP_T_CAN_PAY_V?.html'        , 'appl_header' ),
        (   8,    'Op.Comm',            'TP_T_COM_V?.html'            , 'appl_header' ),
        (   9,    'Memo Read',          'TP_T_MEMO_READ_V?.html'      , 'appl_header' ),
        (  10,    'Report',             'TP_T_ACC_REP_V?.html'        , 'appl_header' ),
        (  11,    'Cent.Mess',          'TP_T_C_MESSAGE_V?.html'      , 'appl_header' ),
        (  12,    'End Acct.',          ''                            , 'appl_header' ),
        (  13,    'Gen Img.',           'TP_T_GEN_IMAGE_V?.html'      , 'appl_header' ),
        (  14,    'Hotl. ASA',          ''                            , 'appl_header' ),
        (  15,    'Del.Memo',           'TP_T_MEMO_DELETE_V?.html'    , 'appl_header' ),
        (  16,    '???',                ''                            , 'appl_header' ),
        (  17,    'Stock Ord.',         'TP_T_SORD_V?.html'           , 'appl_header' ),
        (  18,    'Serv Logon',         'TP_T_TECH_LOGON_V?.html'     , 'appl_header' ),
        (  19,    'Install',            'TP_T_INSTALL_V?.html'        , 'appl_header' ),
        (  20,    'User Upd.',          'TP_T_USER_UPDATE_V?.html'    , 'appl_header' ),
        (  21,    'End Draw',           ''                            , 'appl_header' ),
        (  22,    'Reconcil.',          ''                            , 'appl_header' ),
        (  23,    'U.E.R.',             'TP_T_UER_V?.html'            , 'appl_header' ),
        (  24,    'R.P.R.',             'TP_T_C_RPR_V?.html'          , 'appl_header' ),
        (  25,    'High Win.',          'TP_T_HIWI_V?.html'           , 'appl_header' ),
        (  26,    'Art.List',           'TP_T_ART_LIST_V?.html'       , 'appl_header' ),
        (  27,    'F.T.F.' ,            'TP_T_C_FTF_V?.html'          , 'appl_header' ),
        (  28,    'Read ASA',           ''                            , 'appl_header' ),
        (  29,    'Rslvd.Rprt',         ''                            , 'appl_header' ),
        (  30,    'GParReq',            'TP_T_GPAR_V?.html'           , 'appl_header' ),
        (  31,    'Memo Avail',         'TP_T_C_MEMO_AVAIL_V?.html'   , 'appl_header' ),
        (  32,    'Dis.Logon',          ''                            , 'appl_header' ),
        (  33,    'Ch.Passw',           'TP_T_TERM_PWD_V?.html'       , 'appl_header' ),
        (  34,    'Deprec. 34',         ''                            , 'appl_header' ),
        (  35,    'Sess.Conn.',         ''                            , 'appl_header' ),
        (  36,    'End DLL',            'TP_T_DLL_COMPLETE_V?.html'   , 'appl_header' ),
        (  37,    'DLL Req.',           'TP_T_DLL_REQUEST_V?.html'    , 'appl_header' ),
        (  38,    'Deprec. 38',         ''                            , 'appl_header' ),
        (  39,    'IT-packet',          'TP_T_IT_PKT_STS_V?.html'     , 'appl_header' ),
        (  40,    'Checkpoint',         ''                            , 'appl_header' ),
        (  41,    'Force DLL',          'TP_T_C_FDLL_V?.html'         , 'appl_header' ),
        (  42,    'TrModePar',          'TP_T_TMP_V?.html'            , 'appl_header' ),
        (  43,    'Rest.Comp.',         ''                            , 'appl_header' ),
        (  44,    'PlayerCard',         'TP_T_PL_CARD_V?.html'        , 'appl_header' ),
        (  45,    'Voucher',            'TP_T_VOUCHER_V?.html'        , 'appl_header' ),
        (  46,    'T.O.P.C',            'TP_T_C_TOC_V?.html'          , 'appl_header' ),
        (  47,    'Ext Trans',          'TP_T_EXTENDED_V?.html'       , 'appl_header' ),
        (  48,    'AccReqst',           ''                            , 'appl_header' ),
        (  49,    'AccReply',           ''                            , 'appl_header' ),
        (  50,    'Batch Hdr',          'TP_T_B_HEADER_V?.html'       , 'appl_header' ),
        (  51,    'Batch Sell',         'TP_T_B_SELL_V?.html'         , 'appl_header' ),
        (  52,    'Batch Trl',          'TP_T_B_TRAILER_V?.html'      , 'appl_header' ),
        (  53,    'Batch Canc',         'TP_T_CAN_BATCH_V?.html'      , 'appl_header' ),
        (  54,    'DLL Push',           ''                            , 'appl_header' ),
        (  55,    'I-Net-Serv',         'TP_T_IG_V?_ACK.html'         , 'appl_header' ),
        (  56,    'Ticket Srv',         'TP_T_TS_V?.html'             , 'appl_header' ),
        (  57,    'Downtm.Rep',         'TP_T_SDT_V?.html'            , 'appl_header' ),
        (  58,    'MultiMedia',         'TP_T_MUL_MED_V?.html'        , 'appl_header' ),
        (  59,    'Inquiry',            'TP_T_INQ_V?.html'            , 'appl_header' ),
        (  60,    'Term.Cap.',          'TP_T_TERM_CAP_V?.html'       , 'appl_header' ),
        (  61,    'ParamFile',          'TP_T_PARAM_FILE_V?.html'     , 'appl_header' ),
        (  62,    '???',                ''                            , 'appl_header' ),
        (  63,    '???',                ''                            , 'appl_header' ),

	# ACK-s
        (  64,    'NAK',                'TP_T_NAK_Info.html'          , 'appl_header' ),
        (  65,    'ACK Stop Pay',       ''                            , 'appl_header' ),
        (  66,    'ACK Logon',          'TP_T_LOGON_V?_ACK.html'      , 'appl_header' ),
        (  67,    'ACK Logoff',         'TP_T_LOGOFF_V?_ACK.html'     , 'appl_header' ),
        (  68,    'ACK Sell',           'TP_T_SELL_V?_ACK.html'       , 'appl_header' ),
        (  69,    'ACK Pay',            'TP_T_PAY_V?_ACK.html'        , 'appl_header' ),
        (  70,    'ACK Canc.Sell',      'TP_T_CAN_SELL_V?_ACK.html'   , 'appl_header' ),
        (  71,    'ACK Canc.Pay',       'TP_T_CAN_PAY_V?_ACK.html'    , 'appl_header' ),
        (  72,    'ACK Op.Comm',        'TP_T_COM_V?_ACK.html'        , 'appl_header' ),
        (  73,    'ACK Memo Read',      'TP_T_MEMO_READ_V?_ACK.html'  , 'appl_header' ),
        (  74,    'ACK Acct.Rept',      'TP_T_ACC_REP_V?_ACK.html'    , 'appl_header' ),
        (  75,    'ACK Cent.Mess',      ''                            , 'appl_header' ),
        (  76,    'ACK End Acct.',      ''                            , 'appl_header' ),
        (  77,    'ACK Gen Img.',       'TP_T_GEN_IMAGE_V?_ACK.html'  , 'appl_header' ),
        (  78,    'ACK Hotl. ASA',      ''                            , 'appl_header' ),
        (  79,    'ACK Del.Memo',       ''                            , 'appl_header' ),
        (  80,    'ACK Reconnect',      ''                            , 'appl_header' ),
        (  81,    'ACK Stock Ord.',     'TP_T_SORD_V?_ACK.html'       , 'appl_header' ),
        (  82,    'ACK Serv Logon',     'TP_T_TECH_LOGON_V?_ACK.html' , 'appl_header' ),
        (  83,    'ACK Install',        'TP_T_INSTALL_V?_ACK.html'    , 'appl_header' ),
        (  84,    'ACK User Upd',       'TP_T_USER_UPDATE_V?_ACK.html', 'appl_header' ),
        (  85,    'ACK End Draw',       ''                            , 'appl_header' ),
        (  86,    'ACK Reconcil.',      ''                            , 'appl_header' ),
        (  87,    'ACK U.E.R.',         'TP_T_UER_V?_ACK.html'        , 'appl_header' ),
        (  88,    'ACK R.P.R.',         'TP_T_C_RPR_V?_ACK.html'      , 'appl_header' ),
        (  89,    'ACK High Win.',      'TP_T_HIWI_V?_ACK.html'       , 'appl_header' ),
        (  90,    'ACK Art.List',       'TP_T_ART_LIST_V?_ACK.html'   , 'appl_header' ),
        (  91,    'ACK F.T.F.',         'TP_T_C_FTF_V?_ACK.html'      , 'appl_header' ),
        (  92,    'ACK Read ASA',       ''                            , 'appl_header' ),
        (  93,    'ACK Rslvd.Rprt',     ''                            , 'appl_header' ),
        (  94,    'ACK GParReq',        'TP_T_GPAR_V?_ACK.html'       , 'appl_header' ),
        (  95,    'ACK Memo Avail',     ''                            , 'appl_header' ),
        (  96,    'ACK Dis.Logon',      ''                            , 'appl_header' ),
        (  97,    'ACK Ch.Passw',       'TP_T_TERM_PWD_V?_ACK.html'   , 'appl_header' ),
        (  98,    'ACK Deprec. 98',     ''                            , 'appl_header' ),
        (  99,    'ACK Sess.Conn.',     ''                            , 'appl_header' ),
        ( 100,    'ACK End DLL',        ''                            , 'appl_header' ),
        ( 101,    'ACK DLL Req.',       'TP_T_DLL_REQUEST_V?_ACK.html', 'appl_header' ),
        ( 102,    'ACK Deprec.102',     ''                            , 'appl_header' ),
        ( 103,    'ACK IT-packet',      'TP_T_IT_PKT_STS_V?_ACK.html' , 'appl_header' ),
        ( 104,    'ACK Checkpoint',     ''                            , 'appl_header' ),
        ( 105,    'ACK Force DLL',      ''                            , 'appl_header' ),
        ( 106,    'ACK TrModePar',      'TP_T_TMP_V?_ACK.html'        , 'appl_header' ),
        ( 107,    'ACK Rest.Comp.',     ''                            , 'appl_header' ),
        ( 108,    'ACK PlayerCard',     'TP_T_PL_CARD_V?_ACK.html'    , 'appl_header' ),
        ( 109,    'ACK Voucher',        'TP_T_VOUCHER_V?_ACK.html'    , 'appl_header' ),
        ( 110,    'ACK T.O.P.C',        'TP_T_C_TOC_V?_ACK.html'      , 'appl_header' ),
        ( 111,    'ACK Ext Trans',      'TP_T_EXTENDED_V?_ACK.html'   , 'appl_header' ),
        ( 112,    'ACK AccReqst',       ''                            , 'appl_header' ),
        ( 113,    'ACK AccReply',       ''                            , 'appl_header' ),
        ( 114,    'ACK Batch Hdr',      'TP_T_B_HEADER_V?_ACK.html'   , 'appl_header' ),
        ( 115,    'ACK Batch Sell',     'TP_T_B_SELL_V?_ACK.html'     , 'appl_header' ),
        ( 116,    'ACK Batch Trl',      'TP_T_B_TRAILER_V?_ACK.html'  , 'appl_header' ),
        ( 117,    'ACK Batch Canc',     'TP_T_CAN_BATCH_V?_ACK.html'  , 'appl_header' ),
        ( 118,    'NOT USED 118',       ''                            , 'appl_header' ),
        ( 119,    'ACK I-Net-Serv',     'TP_T_IG_V?_ACK.html'         , 'appl_header' ),
        ( 120,    'ACK Ticket Srv',     'TP_T_TS_V?_ACK.html'         , 'appl_header' ),
        ( 121,    'ACK Downtm.Rep',     'TP_T_SDT_V?_ACK.html'        , 'appl_header' ),
        ( 122,    'ACK MultiMedia',     'TP_T_MUL_MED_V?_ACK.html'    , 'appl_header' ),
        ( 123,    'ACK Inquiry',        'TP_T_INQ_V?_ACK.html'        , 'appl_header' ),
        ( 124,    'ACK Term.Cap.',      'TP_T_TERM_CAP_V?_ACK.html'   , 'appl_header' ),
        ( 125,    'ACK ParamFile',      'TP_T_PARAM_FILE_V?_ACK.html' , 'appl_header' ),
        ( 126,    'NOT USED',           ''                            , 'appl_header' ),
)


# List of necessary patches to incorrect webpages
#       (  txn webpage (RegExp),        search string (RegExp), replace string (RegExp) )
patchlist       = (
#       ( 'TP_T_LOGON_V.*_ACK.html', 'Note: The data.*\(x\) below\.', '' ),
        ( 'TP_T_COM_V.*.html',          '<a name="R.."></a>',      ''              ),
        ( 'TP_T_COM_V.*_ACK.html',      '\.gif" " WIDTH',          '.gif" WIDTH'   ),
        ( 'TP_T_TS_V.*.html',           '<A\s*name=REQ.></A>',     ''              ),
        ( 'TP_T_PARAM_FILE_V.*.html',   'num_docs_types',          'num_doc_types' ),
        ( 'TP_T_INSTALL_V.*_ACK.html',  'base_enrc',               'base_encr'     ),
        ( 'TP_T_MEMO_READ_V.*_ACK.html','memo_flag = 0 OR 1 OR 2', 'memo_flag = 0 OR memo_flag = 1 OR memo_flag = 2' ),
#       ( 'TP_T_COM_V.*.html',          '<a NAME="R39"></a>',   ''              ),
)


# List of parameters to remove unnecessary tables from the TP page
# level:   keep tables of this level, remove all others
# start:   remove from this position
# count:   remove count (-1 for all)
#
#       (  txn webpage (RegExp), keep level, delete start, delete count )
remove_table_list = (
        ( 'TP_T_.*.html',               1, 0, -1 ),     # keep only level 1 tables
        ( 'TP_T_.*.html',               0, 0,  1 ),     # remove first table

        ( 'TP_T_NAK_Info.html',         0, 1,  2 ),     # remove 2.-3. tables

        ( 'TP_SO_.*.html',              1, 0, -1 ),     # keep only level 1 tables
        ( 'TP_SO_.*.html',              0, 0,  1 ),     # remove first table

        ( 'TP_GP_.*.html',              1, 0, -1 ),     # keep only level 1 tables
        ( 'TP_GP_.*.html',              0, 0,  1 ),     # remove first table

        ( 'TP_PIO.html',                0, 0,  1 ),     # remove first table

        ( 'TP_GEN_Wager_ID.html',       2, 0, -1 ),     # keep only level 2 tables

#        ( 'TP_GEN_Format.html',         1, 0, -1 ),     # keep only level 1 tables
#        ( 'TP_GEN_Format.html',         0, 0,  3 ),     # remove first three tables
#        ( 'TP_GEN_Format.html',         0, 4,  2 ),     # remove 4.-5. tables
#        ( 'TP_GEN_Format.html',         0, 7, -1 ),     # remove all tables from 7.

        ( 'TP_GEN_Format.html',         1, 0, -1 ),     # keep only level 1 tables
        ( 'TP_GEN_Format.html',         0, 0,  4 ),     # remove first four tables
        ( 'TP_GEN_Format.html',         0, 4,  2 ),     # remove 4.-5. tables
        ( 'TP_GEN_Format.html',         0,10, -1 ),     # remove all tables from 10.

        ( 'TP_T_EXTENDED_2_V.*_ACK',    0, 1,  1 ),     # remove second table (it was originally the third)
)

# Commands:
#
# data          name    size    type    comment         flag
# if                                    condition
# endif                                 condition
# repeat                                count
# endrepeat                             count
# bitfield      name            bit     comment
# block         name


################################################################################

class TPParseError(Exception):
    """
    Parsing error exception class
    """
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

################################################################################

def read_byte(buf):
    """
    Read one byte from input hexdump
    """
    return (int(buf[0:2], 16), 3)

################################################################################

def read_short(buf):
    """
    Read two byte integer from input hexdump
    """
    return (int(buf[0:2], 16) + 256*int(buf[3:5], 16), 6)

################################################################################

def read_long(buf):
    """
    Read four byte integer from input hexdump
    """
    return (int(buf[0:2], 16) + 256*int(buf[3:5], 16) + 256*256*int(buf[6:8], 16) + 256*256*256*int(buf[9:11], 16), 12)

################################################################################

def read_bytes(buf, num):
    """
    Read num byte integer from input hexdump, where num is given as parameter
    """
    value = 0
    mul = 1
    for i in range(num):
        value = value + mul * int(buf[i*3:i*3+2], 16)
        mul = mul * 256
    return (value, num*3)

################################################################################

def read_tagged_int(buf):
    """
    Read tagged int from input hexdump
    """
    value = int(buf[0:2], 16)
    if value < 128:
        return (value, 3)
    else:
        num = value - 0x90
        value,size = read_bytes(buf[3:], num)
        return (value, size + 3)

################################################################################

def read_comp_date(buf):
    """
    Read compressed date from input hexdump
    """
    year  = int(buf[0:2], 16) + 1900
    month = int(buf[3:5], 16)
    day   = int(buf[6:8], 16)
    if (year >= 0 and year < 90) or month < 1 or month > 12 or day < 1 or day > 31:
        value = '#Error'
    else:
        monthlist = ['???','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
        value = str(day) + '-' + monthlist[month] + '-' + str(year)
    return (value, 9)

################################################################################

def read_comp_time(buf):
    """
    Read compressed time from input hexdump
    """
    hour = int(buf[0:2], 16)
    min  = int(buf[3:5], 16)
    sec  = int(buf[6:8], 16)
    if hour < 0 or hour > 23 or min < 0 or min > 59 or sec < 0 or sec > 59:
        value = '#Error'
    else:
        value = str(hour) + ':' + str(min) + ':' + str(sec)
    return (value, 9)

################################################################################

def read_text(buf, length):
    """
    Read text from input hexdump of the given length
    """
    value = ''
    for i in range(length):
        value = value + chr(int(buf[i*3:i*3+2], 16))
    return (value, length*3)


################################################################################

def read_comp_text(buf, length):
    """
    Read compressed text from input hexdump of the given length
    """
    value = ''
    skip = 0
    for i in range(length):
        if skip > 0:
            skip = skip - 1
        else:
            x = int(buf[i*3:i*3+2], 16)
            if x == 0x09:
                # TAB chanacter: substitute 8 spaces
                value = value + '        '
            if x == 0xFF:
                # Escape (not ESC!) character: byte count and character follows
                # Insert count times the given character
                count = int(buf[i*3+3:i*3+5], 16)
                x     = int(buf[i*3+6:i*3+8], 16)
                value = value + count * chr(x)
                skip = 2
            else:
                # Simple character
                value = value + chr(x)
    return (value, length*3)

################################################################################

def read_buf(buf, length):
    """
    Read buffer from input hexdump of the given length
    """
    value = buf[0:length*3]
    return (value, length*3)


################################################################################

#def flatten(seq):
#    """
#    Flatten sequence: remove hierarchy information (embedded []-s) from sequence
#    """
#    res = []
#    for item in seq:
#        if type(item) in (TupleType, ListType):
#            res.extend(flatten(item))
#        else:
#            res.append(item)
#    return res

################################################################################

def translate(param, var):
    """
    Translate parameter using the given symbol replacement dictionary
    or convert it to numeric type if it is not a symbol
    """
    if type(param) in (IntType, FloatType):
        return param
    if var.has_key(param):
        return var[param]
    if param.isdigit():
        return int(param)
# We cannot do this because default 0 is heavily used in TP
#    if param != '':
#       raise TPParseError, 'Undefined symbol ' + str(param)
    return 0

################################################################################

def operator_precedence(oper):
    """
    Get the operator precedence level (1 is the highest, -1 if invalid)
    """
    o = oper.lower()
    if o == '*' or o == '/':
        return 1
    if o == '+' or o == '-':
        return 2
    if o == '=' or o == '==' or o == '!=' or o == '<' or o == '>' or o == '<=' or o == '>=' or o == '<>':
        return 3
    if o == 'and' or o == 'or' or o == 'not':
        return 4
    return -1

################################################################################

def apply_operator(oper, param1, param2, var):
    """
    Apply operator to param1 and param2, using the given symbol replacement dictionary
    """
    o = oper.lower()
    #print 'apply_operator1:', param1, oper, param2
    p1 = translate(param1, var)
    p2 = translate(param2, var)
    #print 'apply_operator2:', p1, oper, p2

    if o == '*':
        return p1 * p2
    if o == '/':
        return p1 / p2
    if o == '+':
        return p1 + p2
    if o == '-':
        return p1 - p2
    if o == '=' or o == '==':
        if p1 == p2:
            return 1
        else:
            return 0
    if o == '==' or o == '<>':
        if p1 != p2:
            return 1
        else:
            return 0
    if o == '<':
        if p1 < p2:
            return 1
        else:
            return 0
    if o == '<=':
        if p1 <= p2:
            return 1
        else:
            return 0
    if o == '>':
        if p1 > p2:
            return 1
        else:
            return 0
    if o == '>=':
        if p1 >= p2:
            return 1
        else:
            return 0
    if o == 'and':
        if p1 != 0 and p2 != 0:
            return 1
        else:
            return 0
    if o == 'or':
        if p1 != 0 or p2 != 0:
            return 1
        else:
            return 0
    if o == 'not':
        if p1 == 0:
            return 1
        else:
            return 0
    raise TPParseError, 'Unknown operator: ' + str(param1) + oper + str(param2)
    return 0


def apply(oper, param, var):
    """
    Apply the topmost operator on the topmost parameter(s)
    in the stack of operators and parameters
    using the given symbol replacement dictionary
    """
    o = oper[-1].lower()
    if o == 'not':
        # Unary operator: apply it to the last parameter
        res = apply_operator(o, param[-1], '', var)
        del param[-1]
    else:
        # Binary operator: apply it to the last two parameters
        res = apply_operator(o, param[-2], param[-1], var)
        del param[-1]
        del param[-1]
    param.append(res)
    del oper[-1]

################################################################################

def evaluate(expr, var):
    """
    Evaluate expression using the given variable dictionary
    """
    if debug > 2:
        print 'Evaluate:', expr
    i = 0

    # Split expression into words (parameters and tokens)
    ws          = ' \r\n'               # whitespaces
    op          = '=!<>+-*/'            # operators
    br          = '()'                  # braces
    op_br       = op + br               # operators + braces
    op_br_ws    = op_br + ws            # operators + braces + whitespaces
    word        = []

    while i < len(expr):
        for j in range(i, len(expr)):
            if expr[j] not in ws:
                break
        i = j
        for j in range(i+1, len(expr)):
            if not expr[i] in op_br and expr[j] in op_br_ws:
                break
            if expr[i] in op and not expr[j] in op:
                break
            if expr[i] in br and not expr[j] in br:
                break
        else:
            j = len(expr)
        word.append(expr[i:j])
        i = j

    if debug > 2:
        print 'Words   (rough):', word

    # Try to remove problematic parts from the expression (like explanations in braces)
    oprev = ''
    need = need_any
    for i in range(len(word)):
        w = word[i]
        o = w.lower()
        prec = operator_precedence(o)
        if prec > 0:
            # Operator comes
	    if need == need_param:
		if o == '=' and (oprev == '<' or oprev == '>'):
		    # This is a '<=' or '>=' but in two pieces,
		    # stick them together
		    word[i-1] = oprev + o;
		    word[i] = ''
		elif o != 'not':
		    # Waiting for parameter: this is a bug in the expression
		    # Delete everything from the place of the problem to the end
		    del word[i:]
		    break
            need = need_param
        else:
            # Parameter comes
            if need == need_oper and w[0] == '(':
                # Waiting for operator, receiving '(':
                # Delete everything from the place of the problem to the end
                del word[i:]
                break
            elif need == need_oper and w[0] != ')':
                # Waiting for operator, receiving param:
                # stick two params together to build one param
		word[i-1] = word[i-1] + word[i];
		word[i] = ''
            elif need == need_param and w[0] == ')':
                # Waiting for param, receiving ')': this is a bug in the expression
                # Delete everything from the place of the problem to the end
                del word[i:]
                break
            if w == '(' or w == ')':
                need = need_any
            else:
                need = need_oper
	oprev = o

    if debug > 2:
        print 'Words (cleaned):', word

    # Convert infix expression to postfix notation
    # and at the same time evaluate it
    param = []
    oper = []
    for i in range(len(word)):
        w = word[i]
	if w == '':
	    continue
        prec = operator_precedence(w)
        if prec > 0:
            # This is a valid operator
            if len(oper) > 0:
                o = oper[-1].lower()
                prec2 = operator_precedence(o)
                if prec2 > 0 and prec2 <= prec:
                    apply(oper, param, var)
            oper.append(w)
        elif w == '(':
            # This is an opening brace
            oper.append(w)
        elif w == ')':
            # This is a closing brace
            while len(oper) > 0 and oper[-1] != '(':
                apply(oper, param, var)
            del oper[-1]
        else:
            # This is a parameter (operand)
            param.append(w)

    while len(oper) > 0:
        apply(oper, param, var)
        
    result = translate(param[0], var)
    if debug > 2:
        print 'Evaluated:', result
    return result

        
################################################################################

class TPCommand:
    """
    Command descriptor class: represents a command line in the Transaction Protocol
    """

    def __init__(self, command = '', param1 = '', param2 = '', param3 = '', param4 = ''):
        """
        Initialize an empty object
        """
        self.command = command
        self.name = ''
        self.size = ''
        self.type = ''
        self.link = ''
        self.bit = ''
        self.comment = ''
        self.condition = ''
        self.count = ''
        self.flag = ''
        if command == 'data':
            self.name = param1
            self.size = param2
            self.type = param3
            self.link = param4
        if command == 'if' or command == 'endif':
            self.condition = param1
        if command == 'repeat' or command == 'endrepeat':
            self.count = param1
        if command == 'bitfield':
            self.name = param1
            self.bit = param2
        if command == 'block':
            self.name = param1

    def __str__(self):
        """
        Return the string representation of the object
        """
        s = [self.command]
        if self.command == 'data':
            s.extend([self.name, self.size, self.type, self.link, self.comment, self.flag])
        elif self.command == 'if' or self.command == 'endif':
            s.extend([self.condition])
        elif self.command == 'repeat' or self.command == 'endrepeat':
            s.extend([self.count])
        elif self.command == 'bitfield':
            s.extend([self.name, self.bit, self.comment])
        elif self.command == 'block':
            s.extend([self.name])
        return str(s)

    def __repr__(self):
        """
        Return the string representation of the object
        """
        return self.__str__()


################################################################################

def strip_list(list):
    """
    Strip leading and trailing spaces from all element of a list
    """
    newlist = []
    for s in list:
        newlist.append(s.strip())
    return newlist


################################################################################

class TableParser(htmllib.HTMLParser):
    """
    HTML parser that produces table and row information
    together with the common textual data
    """

    def __init__(self):
        self.skipping = 0
        self.list = []
        self.current_data = []
        htmllib.HTMLParser.__init__(self, formatter.NullFormatter())

    def start_table(self, attributes):
        if debug > 2:
            print '[ start_table  ]', attributes
        self.list.append(['+table'])

    def end_table(self):
        if debug > 2:
            print '[   end_table  ]'
        self.list.append(['-table'])

    def start_tbody(self, attributes):
        if debug > 2:
            print '[ start_tbody  ]', attributes
        self.skipping=0

    def end_tbody(self):
        if debug > 2:
            print '[   end_tbody  ]'
#        self.skipping=1

    def start_thead(self, attributes):
        if debug > 2:
            print '[ start_thead  ]', attributes
        self.skipping=1

    def end_thead(self):
        if debug > 2:
            print '[   end_thead  ]'
        self.skipping=0

    def start_tfoot(self, attributes):
        if debug > 2:
            print '[ start_tfoot  ]', attributes
        self.skipping=1

    def end_tfoot(self):
        if debug > 2:
            print '[   end_tfoot  ]'
        self.skipping=0

    def start_caption(self, attributes):
        if debug > 2:
            print '[ start_caption]', attributes
        self.skipping=1

    def end_caption(self):
        if debug > 2:
            print '[   end_caption]'
        self.skipping=0

    def start_th(self, attributes):
        if debug > 2:
            print '[ start_th     ]', attributes
        #self.skipping=self.skipping+1
        self.list.append(['+header'])
        if not self.skipping:
            self.current_data = []

    def end_th(self):
        if debug > 2:
            print '[   end_th     ]'
        #self.skipping=self.skipping-1
        if not self.skipping:
            self.list.append(string.join(strip_list(self.current_data)))
            self.current_data = []
        self.list.append(['-header'])

    def start_tr(self, attributes):
        if debug > 2:
            print '[ start_tr     ]', attributes
        if not self.skipping:
            self.list.append(['+row'])

    def end_tr(self):
        if debug > 2:
            print '[   end_tr     ]'
        if not self.skipping:
            self.list.append(['-row'])

    def start_td(self, attributes):
        if debug > 2:
            print '[ start_td     ]', attributes
        if not self.skipping:
            self.current_data = []

    def end_td(self):
        if debug > 2:
            print '[   end_td     ]'
        if not self.skipping:
            self.list.append(string.join(strip_list(self.current_data)))
            self.current_data = []

    def start_img(self, attributes):
        if debug > 2:
            print '[ start_img    ]', attributes
        for a in attributes:
            if a[0] == 'alt':
                if a[1][0:2] == 'IF':
                    self.handle_if()
                    break
                elif a[1][0:5] == 'ENDIF':
                    self.handle_endif()
                    break
                elif a[1][0:6] == 'REPEAT':
                    self.handle_repeat()
                    break
                elif a[1][0:9] == 'ENDREPEAT':
                    self.handle_endrepeat()
                    break
            elif a[0] == 'src':
                if a[1].find('endif.gif') >= 0:
                    # endif.gif must be before if.gif as if.gif is inside endif.gif as well
                    self.handle_endif()
                    break
                elif a[1].find('if.gif') >= 0:
                    self.handle_if()
                    break
                elif a[1].find('endrepeat.gif') >= 0:
                    # endrepeat.gif must be before repeat.gif as repeat.gif is inside endrepeat.gif as well
                    self.handle_endrepeat()
                    break
                elif a[1].find('repeat.gif') >= 0:
                    self.handle_repeat()
                    break
                elif a[1].find('bullet.gif') >= 0:
                    self.handle_bullet()
                    break

    def end_img(self):
        if debug > 2:
            print '[   end_img    ]'

    def start_a(self, attributes):
        if debug > 2:
            print '[ start_a      ]', attributes
        for a in attributes:
            if a[0] == 'name':
                self.list.append(['+block', a[1]])
            elif a[0] == 'href':
                self.list.append(['+link', a[1]])

    def end_a(self):
        if debug > 2:
            print '[   end_a      ]'

    def handle_data(self, data):
        dummies = data.count(chr(10)) + data.count(chr(13)) + data.count(chr(32)) + data.count(chr(160))
        if len(data) > dummies:
            if debug > 2:
                print '[handle_data   ]', data
            if not self.skipping:
                d = data.replace(chr(10), '')
                d = d.replace(chr(13), '')
                d = d.replace(chr(160), '')
                self.current_data.append(d)

    def handle_if(self):
        self.current_data.append('if')

    def handle_endif(self):
        self.current_data.append('endif')

    def handle_repeat(self):
        self.current_data.append('repeat')

    def handle_endrepeat(self):
        self.current_data.append('endrepeat')

    def handle_bullet(self):
        self.current_data.append('member')


################################################################################

def remove_tables(list, keeplevel, start, num):
    """
    Remove specified tables from the page, tables can be nested
    keeplevel = the level of tables to keep, all other levels will be removed
    start     = remove from that position
    num       = remove that many tables, -1 means all
    """
    newlist = []
    remove = 0
    level = 0
    i = -1
    for x in list:
        if x == ['+block']:
            newlist.append(x)
        elif x == ['+table']:
            level = level + 1
            if level == keeplevel:
                remove = 0
            else:
                i = i + 1
                if i >= start and (num <= 0 or i < start + num):
                    remove = 1
            if remove == 0:
                newlist.append(x)
        elif x == ['-table']:
            level = level - 1
            if remove == 0:
                newlist.append(x)
            else:
                remove = 0
        else:
            if not remove:
                newlist.append(x)
    return newlist


################################################################################

def patch_tables(urlname, list):
    """
    Remove unnecessay tables from the table-parsed html
    """
    # Find patches for this page
    for r in remove_table_list:
        if debug > 2:
            print 'remove_table_list:', r
        name = re.compile(r[0])
        if name.search(urlname):
            if debug > 2:
                print 'Remove tables:', r[1], r[2], r[3]
            list = remove_tables(list, r[1], r[2], r[3])
    return list


################################################################################

def build_rows(list):
    """
    Concatenate data items between row markers to build whole TP rows
    """
    newlist = []
    current_row = []
    for x in list:
        if len(x) > 0 and x[0] == '+block':
            newlist.append(x)
        if x == ['+row']:
            current_row = []
        elif x == ['-row']:
            newlist.append(current_row)
            current_row = []
        elif x != ['+table'] and x != ['-table']:
            current_row.append(x)
    return newlist


################################################################################

def compile(list):
    """
    Translate every TP row into an interpretable TP command
    """
    command = []
    for row in list:
        cmd = row[0]
        if cmd == 'if' or cmd == 'endif':
            # This is an if or endif command
            c = TPCommand(cmd, row[3])
            command.append(c)
        elif cmd == 'repeat' or cmd == 'endrepeat':
            # This is a repeat or endrepeat command
            c = TPCommand(cmd, row[3])
            command.append(c)
        elif cmd == '+block':
            # This is a named block
            c = TPCommand('block', row[1])
            command.append(c)
        elif cmd == ['+header']:
            #TODO: evaluate expressions in headers (like in SellObjects)
            pass
        else:
#           for x in row:
#               if type(x) == ListType and len(x) > 1:
#                   if x[0] == '+block':
#                       # This is a named block
#                       c = TPCommand('block', x[1])
#                       command.append(c)
            if len(row) > 2:
                ok = 0
                # Check if this is a member of the previous data (like bit or sub-byte)
                if len(row[0]) > 6 and row[0][0:7] == 'member ':
                    row[0] = row[0][7:]
                    member = 'member'
                else:
                    member = ''
                for x in row[0]:
                    # Check if this is a real name (contains alphanumeric characters)
                    if x.isalnum():
                        ok = 1
                        break
                if ok:
                    # This is normal data
                    c = TPCommand('data', row[0], row[1])
                    if type(row[2]) == ListType and row[2][0] == '+link':
                        # Data contains link to data type
                        c.type = row[3]
                        c.link = row[2][1]
                        if len(row) > 4:
                            c.comment = row[4]
                    else:
                        c.type = row[2]
                        c.comment = row[3]
                    c.flag = member
                    command.append(c)
    return command


################################################################################

def patch_gen_format(command):
    """
    Do necessary error corrections for general formats
    """
    newcommand = []
    block = ''
    first_in_block = 0
    skip = 0
    for i in range(len(command)):
        c = command[i]
        if skip > 0:
            skip = skip - 1
        else:
            if c.command == 'block':
                block = c.name
                first_in_block = 1
            else:
                first_in_block = 0
            if block == 'VarSize2DData':
                # This is a VarSize2DData block:
                # rename some fields and wrap it into a repeat-endrepeat shell
                c.condition = c.condition.replace('NumOfResults', 'num_results')
                if c.name.find('event_2') >= 0:
                    c2 = TPCommand('endrepeat', 'num_events')
                    newcommand.append(c2)
                    skip = 2
                else:
                    newcommand.append(c)
                if first_in_block == 1:
                    c2 = TPCommand('repeat', 'num_events')
                    newcommand.append(c2)
                if c.command == 'endif' and c.condition.find('num_results') >= 0 and c.condition.find('16') >= 0:
                    # Append missing declarations for num_results > 24 cases (until 60 results)
                    for i in [3, 4, 5, 6, 7]:
                        c1 = TPCommand('if',    'num_results > ' + str(i*8))
                        c2 = TPCommand('data',  'event_1_cont_' + str(i),   '1',     'BitField', 'TP_GEN_Format.html#BitField')
                        c3 = TPCommand('data',  'selected_' + str(i*8+1),   '',      'Bit 7',    'TP_GEN_Format.html#Bit')
                        c4 = TPCommand('data',  'selected_' + str(i*8+2),   '',      'Bit 6',    'TP_GEN_Format.html#Bit')
                        c5 = TPCommand('endif', 'num_results > ' + str(i*8))
                        newcommand.append(c1)
                        newcommand.append(c2)
                        newcommand.append(c3)
                        newcommand.append(c4)
                        newcommand.append(c5)
            else:
                newcommand.append(c)
    return newcommand

def patch_gen_wager(command):
    """
    Do necessary error corrections for wager id-s
    """
    newcommand = []
    first = 1
    last_condition = ''
    for i in range(len(command)):
        c = command[i]
        if c.name == 'wid_type':
            # WID found: bring wid_type to top and wrap remaining parts
            # into an if-endif block depending on wid_type
            if first:
                newcommand.append(c)
                first = 0
            else:
                d = TPCommand('endif', last_condition)
                newcommand.append(d)
            comment = c.comment.split()
            last_condition = 'wid_type=' + comment[-1]
            d = TPCommand('if', last_condition)
            newcommand.append(d)
        else:
            newcommand.append(c)
            if i == len(command) - 1:
                d = TPCommand('endif', last_condition)
                newcommand.append(d)
    return newcommand

def patch_win_info(command):
    """
    Do necessary error corrections for wininfo
    """
    newcommand = []
    for i in range(len(command)):
        c = command[i]
        if c.condition == 'win_info_ver=1':
            c.condition = 'win_info_ver=1 OR win_info_ver_follows=0'
        newcommand.append(c)
    return newcommand

def patch_sell_object(command):
    """
    Do necessary error corrections for sell objects
    """
    #TODO: 2D-BaseData loop size depends on panel_type (or if it is already in a loop or not)
    newcommand = []
    skip = 0
    first = 1
    last_condition = ''
    for i in range(len(command)):
        c = command[i]
        if skip > 0:
            skip = skip - 1
        else:
            if i < len(command) - 1:
                c2 = command[i+1]
            if ( c.name == 'sell_obj_code' or  c.name == 'obj_code') and \
               (c2.name == 'sell_sub_code' or c2.name == 'sub_code'):
                # sell object code and subcode found: bring them to top and wrap
                # remaining parts into if-endif block depending on subcode
                c.name  = 'sell_obj_code'
                c2.name = 'sell_sub_code'
                if first:
                    newcommand.append(c)
                    newcommand.append(command[i+1])
                    first = 0
                else:
                    d = TPCommand('endif', last_condition)
                    newcommand.append(d)
                comment = c2.comment.split()
                last_condition = 'sell_sub_code=' + comment[0]
                d = TPCommand('if', last_condition)
                newcommand.append(d)
                skip = 1
            else:
                newcommand.append(c)
                if i == len(command) - 1:
                    d = TPCommand('endif', last_condition)
                    newcommand.append(d)
    return newcommand

def patch_pay_info_object(command):
    """
    Do necessary error corrections for pay info objects
    """
    newcommand = []
    last_condition = ''
    for i in range(len(command)):
        c = command[i]
        if c.command == 'data':
            if type(c.comment) == StringType and c.comment.find('General') >= 0:
                last_condition = 'draw_game_obj_code<>42 and draw_game_obj_code<>43'
                d = TPCommand('if', last_condition)
                newcommand.append(d)
            elif type(c.comment) == ListType and len(c.comment)>1 and c.comment[1].find('Keno') >= 0:
                d = TPCommand('endif', last_condition)
                newcommand.append(d)
                last_condition = 'draw_game_obj_code=42'
                d = TPCommand('if', last_condition)
                newcommand.append(d)
            elif type(c.comment) == ListType and len(c.comment)>1 and c.comment[1].find('FOG') >= 0:
                d = TPCommand('endif', last_condition)
                newcommand.append(d)
                last_condition = 'draw_game_obj_code=43'
                d = TPCommand('if', last_condition)
                newcommand.append(d)
            else:
                newcommand.append(c)
        else:
            newcommand.append(c)

    d = TPCommand('endif', last_condition)
    newcommand.append(d)
    return newcommand

def patch_gparreq_ack(command):
    """
    Do necessary error corrections for GParReq Ack
    """
    newcommand = command
    c = TPCommand('data', 'uncomp_gpar_block_data', 'n', 'GParData', 'TP_GP_Overview.html')
    newcommand.append(c)
    return newcommand

def patch_exttrans_ack(command):
    """
    Do necessary error corrections for Extended Transaction Ack
    """
    newcommand = command

    # Add Subtype Version
#    c = TPCommand('data', 'sub_type_version_bits', '1', 'Integer', 'TP_GEN_Format.html#Integer')
#    newcommand.append(c)

    # Add Parameter Block Ack
    last_condition = 'sub_type=2'
    c = TPCommand('if', last_condition)
    newcommand.append(c)
    c = TPCommand('data', 'parameter_block_ack', 'n', 'ParBlockAck', 'TP_T_EXTENDED_2_V1_ACK.html')
    newcommand.append(c)
    c = TPCommand('endif', last_condition)
    newcommand.append(c)

    # Add Coupon Definition
    last_condition = 'sub_type=4'
    c = TPCommand('if', last_condition)
    newcommand.append(c)
    c = TPCommand('data', 'coupon_definition_ack', 'n', 'CoupDefAck', 'TP_T_EXTENDED_4_V1_ACK.html')
    newcommand.append(c)
    c = TPCommand('endif', last_condition)
    newcommand.append(c)

    return newcommand

def patch_exttrans2_ack(command):
    """
    Do necessary error corrections for Extended Transaction Subtype 2 Ack
    """
    newcommand = []
    obj_code = 0
    new_obj_code = 0
    last_condition = ''
    skip = 0
    for i in range(len(command)):
        c = command[i]
        if   obj_code == 0 and c.name == 'no_texts':
            new_obj_code = obj_code + 1
        elif obj_code == 1 and c.name == 'no_texts':
            new_obj_code = obj_code + 1
        elif obj_code == 2 and c.name == 'no_pti':
            new_obj_code = obj_code + 1
        elif obj_code == 3 and c.name == 'no_win_types':
            new_obj_code = obj_code + 1
        elif obj_code == 4 and c.name == 'no_sets':
            new_obj_code = obj_code + 1
        elif obj_code == 5 and c.name == 'no_ranges':
            new_obj_code = obj_code + 1
#        elif obj_code == 6 and c.command == 'if' and c.condition == 'sub_type_version = 1':
#            new_obj_code = obj_code + 1
        elif obj_code == 6 and c.name == 'game_name_types':
            new_obj_code = obj_code + 1
        elif obj_code == 7 and c.name == 'num_reason_codes':
            new_obj_code = obj_code + 1
#        elif obj_code == 8 and c.name == 'info_flag':
#            new_obj_code = obj_code + 1
        elif obj_code == 8 and c.name == 'num_coupon_codes':
            new_obj_code = obj_code + 1
#        elif obj_code == 9 and c.name == 'info_flag':
#            new_obj_code = obj_code + 1
        elif obj_code == 9 and c.name == 'num_pay_games':
            new_obj_code = obj_code + 1
#        elif obj_code == 10 and c.name == 'no_draw_info_block':
#            new_obj_code = obj_code + 1
        elif obj_code == 10 and c.name == 'compl_status':
            new_obj_code = obj_code + 1

        if new_obj_code != obj_code:
            if last_condition != '':
                d = TPCommand('endif', last_condition)
                newcommand.append(d)
            last_condition = 'par_block_obj_code=' + str(new_obj_code)
            d = TPCommand('if', last_condition)
            newcommand.append(d)
            obj_code = new_obj_code

        if c.name == 'appl_header':
            skip = 1
        if not skip:
            newcommand.append(c)
        if c.name == 'sub_type':
            skip = 0
        if c.name == 'sub_type_version' and c.type == 'BitField':
            c.name = 'sub_type_version_flags'

    if last_condition != '':
        d = TPCommand('endif', last_condition)
        newcommand.append(d)
    return newcommand

def patch_all(command):
    """
    Do necessary error corrections for all type of transactions
    """
    newcommand = []
    for i in range(len(command)):
        c = command[i]
        if c.type[0:4] == 'Bit ' and c.size == '1':
            # Remove incidental size from bit definitions
            c.size = ''
        newcommand.append(c)
    return newcommand

def patch_command(urlname, command):
    """
    Does the last time error corrections on the compiled command list
    """
    command = patch_all(command)
    if urlname.find('TP_GEN_Format.html') >= 0:
        return patch_gen_format(command)
    elif urlname.find('TP_GEN_Wager_ID.html') >= 0:
        return patch_gen_wager(command)
    elif urlname.find('TP_T_WIN_INFO.html') >= 0:
        return patch_win_info(command)
    elif urlname.find('TP_SO_') >= 0:
        return patch_sell_object(command)
    elif urlname.find('TP_PIO.html') >= 0:
        return patch_pay_info_object(command)
    elif urlname.find('TP_T_GPAR_V') >= 0 and urlname.find('_ACK.html') >= 0:
        return patch_gparreq_ack(command)
    elif urlname.find('TP_T_EXTENDED_V') >= 0 and urlname.find('_ACK.html') >= 0:
        return patch_exttrans_ack(command)
    elif urlname.find('TP_T_EXTENDED_2_V') >= 0 and urlname.find('_ACK.html') >= 0:
        return patch_exttrans2_ack(command)
    else:
        return command


################################################################################

def process(urlname, html):
    """
    Run html page through various filters and compiler
    to produce an interpretable TP command list
    """
    parser = TableParser()
    parser.feed(html)
    parser.close()
    if debug > 2:
        print '----------------------------------------------------------------------'
        print 'parse'
        for x in parser.list:
            print x
        print '----------------------------------------------------------------------'
        print 'patch_tables'
    list = patch_tables(urlname, parser.list)
    if debug > 2:
        for x in list:
            print x
        print '----------------------------------------------------------------------'
        print 'build_rows'
    list = build_rows(list)
    if debug > 2:
        for x in list:
            print x
        print '----------------------------------------------------------------------'
        print 'compile'
    list = compile(list)
    if debug > 2:
        for x in list:
            print x
        print '----------------------------------------------------------------------'
        print 'patch_command'
    list = patch_command(urlname, list)
    if debug > 2:
        for x in list:
            print x
    return list


################################################################################

def patch_html(urlname, html):
    """
    Patch a html page to correct incidental errors on page
    """
    # Find patches for this page
    for patch in patchlist:
        name = re.compile(patch[0])
        if name.search(urlname):
            # Replace incorrect string with correct one
            exp = re.compile(patch[1], re.DOTALL)
            if debug > 1:
                print 'Find', patch[1], 'in', patch[0], ':', exp.search(html)
            html = exp.sub(patch[2], html)
    return html


################################################################################

def parse_html(filename, block):
    """
    Parse and compile a html TP-page into a command list
    """
    anchor = ''
    pos = filename.find('#')
    if pos >= 0:
        # We have an anchor (bookmark) in the url as well
        anchor = filename[pos+1:]
        filename = filename[0:pos]
    if debug > 1:
        print filename, anchor

    if filename[0:7] == "http://" or filename[0:8] == "https://":
        # The given filename is actually an url
        try:
            if debug > 1:
                print 'urllib.urlopen(', filename, ')'
            file = urllib.urlopen(filename)
        except:
            raise TPParseError, 'Cannot open ' + filename
    else:
        # The given filename is a valid filename
        file = open(filename)

    html = file.read()
    file.close()
    if html.find('404 Not Found') >= 0:
        raise TPParseError, 'Page ' + filename + ' not found, error 404'

    html = patch_html(filename, html)

    # Pass 1: Process html with parser and compile into command list
    command = process(filename, html)

    if debug > 2:
        print '----------------------------------------------------------------------'
        print 'parse_html'

    # Pass 2: Find appropriate block and beautify command list
    # (indent and concatenate multi-row data, remove whitespaces, etc.)
    indent = 0
    block_found = 0
    newcommand = []
    for c in command:
        if c.command != '':
            if c.command == 'endif' or c.command == 'endrepeat':
                # Backindent at sub-block end
                indent = indent - 1

            if c.command == 'block':
                # Check if this is the block we are looking for
                if anchor != '':
                    if c.name == anchor:
                        block_found = 1
                    else:
                        block_found = 0
                elif block == '':
                    # No anchor and block was given: the first block we meet is the one we need
                    block_found = 1
            elif c.command == 'data':
                d = TPCommand(c.command)
                skip = 1
                d.name = c.name.strip()
                if d.name != '':
                    skip = 0
                d.size = c.size
                if d.size != '':
                    skip = 0
                d.type = c.type
                if d.type != '':
                    skip = 0
                d.link = c.link
                d.flag = c.flag
                if not skip and block_found:
                    # Append currently built data to command list
                    if d.name.find('Name') < 0 or d.size.find('Bytes') < 0:
                        # We checked that it is not a table header,
                        # so we can append it to the command list
                        newcommand.append(d)
                        if debug > 1:
                            print '    ' * indent,
                            print d
                if anchor == '':
#                   if (block == '' and d.name.find('Name') >= 0 and d.size.find('Bytes') >= 0) or \
#                      (block != '' and d.name == block):
                    if (block == '') or (block != '' and d.name == block):
                        # The block name is not given in the URL,
                        # it is given as function parameter (usually it is the appl_header)
                        # or not given at all
                        block_found = 1
            elif c.command == 'if' or c.command == 'endif' or c.command == 'repeat' or c.command == 'endrepeat':
                # Edge of a sub-block
                d = TPCommand(c.command)
                if c.command == 'if' or c.command == 'endif':
                    d.condition = c.condition
                if c.command == 'repeat' or c.command == 'endrepeat':
                    d.count = c.count
                if block_found:
                    newcommand.append(d)
                    if debug > 1:
                        print '    ' * indent,
                        print d
            else:
                # ??? We don't know this
                # Anyway, append it to command list and see what will happen...
                if block_found:
                    d = TPCommand(c.command)
                    #command.append(c.command)
                    newcommand.append(d)
                    if debug > 1:
                        print '    ' * indent,
                        print c

            if c.command == 'if' or c.command == 'repeat':
                # Indent at sub-block start
                indent = indent + 1

    return newcommand


################################################################################

def link_sell_object(txn, pos):
    """
    """
    sell_obj_code,size = read_byte(txn[pos:])
    if sell_obj_code < 0 or sell_obj_code > 110:
        raise TPParseError, 'Invalid Sell Object Code ' + str(sell_obj_code)
    sell_sub_code,size = read_byte(txn[pos+size:])
    sell_obj_page = sell_obj_code
    if sell_obj_code >= 0 and sell_obj_code <=99:
        sell_obj_page = 65
    return ['', '', 'TP_SO_' + str(sell_obj_page) + '.html', '']

def link_gpar_data(var):
    """
    """
    if not var.has_key('sell_obj_code'):
        raise TPParseError, 'Missing sell_obj_code'
    sell_obj_code = var['sell_obj_code']
    if sell_obj_code < 30 or sell_obj_code > 99:
        raise TPParseError, 'Invalid Sell Object Code ' + str(sell_obj_code)
    gpar_page = sell_obj_code
    if sell_obj_code >= 0 and sell_obj_code <=99:
        gpar_page = 65
    return ['', '', 'TP_GP_' + str(gpar_page) + '_V1.html', ''] #TODO: determine correct version

def parse_txn(txn, desc, var):
    """
    Parse transaction dump using the given transaction descriptor
    """
    # Build transaction html file name using txn version
    if desc[2].find('#') < 0 or desc[2].find('?') >= 0:
        # This is a transaction descriptor
        # Read transaction version: this is the first byte
        ver,size = read_byte(txn[0:])
        descver = desc[2].replace('?', str(ver))
        transpec_full = transpec_base + descver
    else:
        # This is a sub-structure descriptor
        transpec_full = transpec_base + desc[2]

    if debug > 1:
        print transpec_full
        print txn

    # Compile html to get command list
    command = parse_html(transpec_full, desc[3])
    if debug > 2:
        print
        print command

    pos = 0
    cmd = 0
    lastdata = ''
    lastvalue = 0
    repeatstart = []
    repeatcount = []

    # Process transaction dump
    while pos < len(txn) and cmd < len(command):

        if command[cmd].command == 'data':
            # Read and store value of data variable
            value = '???'
            link = 0

            # This is a member, moreover the first one:
            # move read pointer back to the beginning of the struct
            # so that members will re-read the structure bytes
            if cmd > 0 and command[cmd-1].command == 'data' and command[cmd].command == 'data' and \
               command[cmd-1].flag != 'member' and command[cmd].flag == 'member' and command[cmd].type[0:4] != 'Bit ':
                if command[cmd-1].size[0:1] >= '0' and command[cmd-1].size[0:1] <= '9':
                    bytecount = int(command[cmd-1].size[0:1])
                    pos = pos - bytecount*3

            if command[cmd].size[0:1] >= '0' and command[cmd].size[0:1] <= '9':
                # This is a fixed number of bytes data
                bytecount = int(command[cmd].size[0:1])
                if command[cmd].size[1:2] >= '0' and command[cmd].size[1:2] <= '9':
                    # The number of bytes is a two-digit value
                    bytecount = bytecount * 10 + int(command[cmd].size[1:2])
                lastdata = command[cmd].name

                if bytecount == 1:
                    if command[cmd].type == 'Tagged Int' or command[cmd].type == 'TaggedInt':
                        # This is a Tagged Int
                        lastdata = command[cmd].name
                        value,size = read_tagged_int(txn[pos:])
                    else:
                        # This is a one-byte data
                        value,size = read_byte(txn[pos:])

                elif bytecount == 2:
                    # This is a two-byte data
                    value,size = read_short(txn[pos:])

                elif bytecount == 3:
                    if command[cmd].type == 'CompDate':
                        # This is a Compressed Date
                        lastdata = command[cmd].name
                        value,size = read_comp_date(txn[pos:])
                    elif command[cmd].type == 'CompTime':
                        # This is a Compressed Time
                        lastdata = command[cmd].name
                        value,size = read_comp_time(txn[pos:])
                    else:
                        # Any other number of bytes
                        value,size = read_bytes(txn[pos:], bytecount)

                elif bytecount == 4:
                    # This is a four-byte data
                    value,size = read_long(txn[pos:])

                else:
                    # Any other number of bytes
                    value,size = read_bytes(txn[pos:], bytecount)

                if debug > 0:
                    print 'Read:', txn[pos:pos+size]
                pos = pos + size
                lastvalue = value

            elif command[cmd].size[0:1] == 'n':
                # This is an n-byte data: we have to determine the value of n
                # (and the structure of that n-byte buffer)
                if command[cmd].type == 'Tagged Int' or command[cmd].type == 'TaggedInt':
                    # This is a Tagged Int
                    lastdata = command[cmd].name
                    value,size = read_tagged_int(txn[pos:])
                elif lastdata.find('length') >= 0 or lastdata.find('_len') >= 0:
                    # The length is given in the previous data value
                    lastdata = command[cmd].name
                    if command[cmd].type == 'CompText':
                        value,size = read_text(txn[pos:], lastvalue)
                    else:
                        value,size = read_buf(txn[pos:], lastvalue)
                    if debug > 0:
                        print 'Read:', txn[pos:pos+size]
                else:
                    # Here comes the tricky part:
                    # we have a link to a complex sub-structure 
                    if debug > -1:
                        print command[cmd].name, 'begin'
                    if command[cmd].type == 'SellObject':
                        # Special handling of sell object
                        desc2 = link_sell_object(txn, pos)
                    elif command[cmd].type == 'GParData':
                        # Special handling of game parameter data
                        desc2 = link_gpar_data(var)
                    else:
                        desc2 = ['', '', command[cmd].link, '']
                        if desc2[2][0:1] == '#':
                            # this is a link to the current html, so add html name
                            this_html = desc[2]
                            anchorpos = this_html.find('#')
                            if anchorpos >= 0:
                                this_html = this_html[0:anchorpos]
                            desc2[2] = this_html + desc2[2]
                    link = 1
                    size = parse_txn(txn[pos:], desc2, var)
                    if debug > -1:
                        print command[cmd].name, 'end'
                    value = ''

                pos = pos + size
                lastvalue = value

            elif command[cmd].size[0:1] == '' and command[cmd].type[0:4] == 'Bit ':
                # This is a specific bit (or bit range) in a bitfield
                dash = command[cmd].type.find('-')
                if dash < 0:
                    # This is only one bit
                    bit = int(command[cmd].type[4:])
                    mask = 1 << bit
                else:
                    # This is a bit range
                    bit  = int(command[cmd].type[4:dash])
                    bit2 = int(command[cmd].type[dash+1:])
                    mask = 0
                    for b in range(bit, bit2):
                        mask = mask + (1 << b)
                value = (lastvalue & mask) >> bit

            # Save the previously extracted value (values) in the command list
            var[command[cmd].name] = value
            if debug > -1 and not link:
                print command[cmd].name, '=', value
            cmd = cmd + 1

        elif command[cmd].command == 'if':
            # Evaluate conditional expression
            if not evaluate(command[cmd].condition, var):
                # Expression evaluated to false: skip until matching endif
                ifcount = 1
                while ifcount > 0 and cmd < len(command) - 1:
                    cmd = cmd + 1
                    if command[cmd].command == 'if':
                        ifcount = ifcount + 1
                    elif command[cmd].command == 'endif':
                        ifcount = ifcount - 1
            cmd = cmd + 1

        elif command[cmd].command == 'endif':
            # End of conditional expression: nothing really to do just go on
            # We could check here if there really has been an opening 'if' before...
            cmd = cmd + 1

        elif command[cmd].command == 'repeat':
            # Start of a 'repeat' block: determine the count of repetitions
            if var.has_key(command[cmd].count):
                cnt = var[command[cmd].count]
                if debug > 1:
                    print 'repeat   ', command[cmd].count, cnt
                if cnt > 0:
                    repeatstart.append(cmd + 1)
                    repeatcount.append(cnt)
                else:
                    # zero-count repeat: do nothing just skip until matching endrepeat
                    rcount = 1
                    while rcount > 0 and cmd < len(command) - 1:
                        cmd = cmd + 1
                        if command[cmd].command == 'repeat':
                            rcount = rcount + 1
                        elif command[cmd].command == 'endrepeat':
                            rcount = rcount - 1
                    if debug > 1:
                        print 'endrepeat', command[cmd].count, cnt
            else:
                raise TPParseError, 'Undefined symbol: repeat ' + command[cmd].count
            cmd = cmd + 1

        elif command[cmd].command == 'endrepeat':
            # End of a 'repeat' block: decrement repeat count and go back to the blcok start
            # Note: embedded repeat blocks are handled by using a list of repeat parameters
            if debug > 1:
                print 'endrepeat', command[cmd].count, repeatcount[-1]
            repeatcount[-1] = repeatcount[-1] - 1
            if repeatcount[-1] > 0:
                cmd = repeatstart[-1]
            else:
                del repeatstart[-1]
                del repeatcount[-1]
                cmd = cmd + 1
                
        else:
            # None of the above: nothing really to do just go on
            cmd = cmd + 1

    # Return the number of bytes processed
    return pos


################################################################################

def parse(lines):
    """
    Read input buffer containing the transaction as dumped by PL_LOG
    """
    hdr_found = 0
    txn_found = 0
    txn_type = ''
    txn = ''
    desc = []
    for line in lines:
        # Check for header info (transaction start)
        if line[0 : 14] == 'HEADER Info : ':
            if hdr_found and txn_found:
                # Process previously found and parsed txn
                var = {}
                parse_txn(txn, desc, var)
            txn_found = 0
            hdr_found = 1
            txn = ''

            # Try to find transaction type
            for t in transaction:
                txn_type = line[14 : 14+len(t[1])]
                if t[1] == txn_type:
                    txn_found = 1
                    print
                    #print t[1]
                    print line[14:]
                    if debug > 1:
                        print t
                    desc = t
                    break

        if hdr_found and txn_found and line[0 : 14] == 'PL Data     : ':
            # We have a transaction beeing processed:
            # collect transaction dump lines
            #txn = txn + line[14 : 14+60]
            txnline = line[14 : 14+60].rstrip() + ' '
            txn = txn + txnline

    if not hdr_found:
        raise TPParseError, 'HEADER Info not found'
    if not txn_found:
        raise TPParseError, 'Txn type "' + txn_type + '" not found'

    # Process previously found and parsed txn (the last one)
    var = {}
    parse_txn(txn, desc, var)


################################################################################

def clp_get_text():
    """
    Get string from clipboard (CF_TEXT format)
    """
    CF_TEXT = 1
    try:
        from win32clipboard import OpenClipboard, GetClipboardData, CloseClipboard
        OpenClipboard(0)
        try:
            text = GetClipboardData(CF_TEXT)
        except:
            text = ''
            print 'Cannot get clipboard data'
        CloseClipboard()
    except:
        text = ''
    return text


################################################################################

def usage():
    """
    Display program usage help screen
    """
    print 'usage: tpparse.py [options]'
    print
    print 'options:'
    print '  -h, --help               show this help message and exit'
    print '  -b URL, --base=URL       set TP base URL'
    print '  -i, --isbb               use ISBB TP base URL (default), i.e.:'
    print '                          ', transpec_base_isbb
    print '  -m, --mainline           use mainline TP base URL, i.e.:'
    print '                          ', transpec_base_mainline
    print '  -c, --clipboard          use clipboard without asking (Windows only, default)'
    print '  -n, --noclipboard        do not use clipboard'
    print '  -t FILE, --txn=FILE      input FILE that contains transaction dump'
    print '  -d LEVEL, --debug=LEVEL  set debug LEVEL (0..3)'
    print '  -q, --quit               do not parse txn-s in loop, quit at program end'
    print


################################################################################

if __name__ == "__main__":
    # Get command line options      
    try:
        opts, args = getopt.getopt(sys.argv[1:], "?hb:imcnt:d:q", \
                     ["help", "base=", "isbb", "mainline", "clipboard", "noclipboard", "txn=", "debug=", "quit"])
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)

    # Process options
    for o, a in opts:
        if o in ("-b", "--base"):
            transpec_base = a
        if o in ("-i", "--isbb"):
            transpec_base = transpec_base_isbb
        if o in ("-m", "--mainline"):
            transpec_base = transpec_base_mainline
        if o in ("-c", "--clipboard"):
            use_clp_opt = 'Y'
        if o in ("-n", "--noclipboard"):
            use_clp_opt = 'N'
        if o in ("-t", "--txn"):
            input_file = a
            quit = 1            # No use to repeat for the same input file over and over again
        if o in ("-d", "--debug"):
            debug = int(a)
        if o in ("-q", "--quit"):
            quit = 1
        if o in ("-?", "-h", "--help"):
            usage()
            if not quit:
                # Make sure output window does not disappear if run directly from OS
                quit = raw_input('Press ENTER to quit')
            sys.exit()

    while 1:
        # Get Windows clipboard
        clp = clp_get_text()

        # Check if text on cliboard contains Ticketing data
        use_clp = use_clp_opt
        if use_clp_opt != 'N' and len(clp) > 11 and clp[0:11] == 'HEADER Info':
            if use_clp == '?':
                use_clp = raw_input('Transaction data found on clipboard, use it? [Y]')
            if use_clp == '' or use_clp == '\n' or use_clp == '\r':
                use_clp = 'Y'
        else:
            use_clp = 'N'

        inp = ''
        if use_clp[0:1] != 'y' and use_clp[0:1] != 'Y':
            # If not using clipboard then load hexdump file
            if input_file == '':
                input_file = raw_input('Transaction dump file (txn.txt): ')
            input_file = input_file.replace('\n', '')
            input_file = input_file.replace('\r', '')
            if input_file == '':
                input_file = 'txn.txt'

            # Open and read inut file
            print 'Open file:', input_file
            try:
                file = open(input_file, 'rt')
                inp = file.readlines()
                file.close()
            except:
                print 'Input file', input_file, 'not found'
        else:
            # Using the buffer copied from clipboard
            inp = string.split(clp, '\r\n')

        # Do the actual parsing
        try:
            if inp != '':
                parse(inp)
        except TPParseError, e:
            print "Parsing error:", e.value
        except:
            traceback.print_exc()
            #raise

        if quit:
            break
        if not quit:
           # Make sure output window does not disappear if run directly from OS
           q = raw_input('Press ENTER to quit or C to continue ')
           if len(q) == 0 or (q[0] != 'c' and q[0] != 'C'):
                break

        input_file = ''

# -End of program-

