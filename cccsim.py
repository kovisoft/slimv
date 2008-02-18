# Echo server program
import socket
import time

# Transaction structure

# Field	Description			Length

# B	Credit Card Number		19
# C	Debit/Credit Amount * 100	8
# D	Transaction Type		2
#	Credit Type			1
#	Currency			1
#	Transaction Code		2
# E	Confirmation Number		7
# J	Operation Way			1
# T	Validity Month			4
# U	CVV2				3-4
# Y	Israeli Reg.No.			0-9
# X	Special Data			19
#	End Of Record (CR + LF)


# Changeable parameters
HOST = ''                 # Symbolic name meaning the local host
PORT = 22000              # Arbitrary non-privileged port
delay = 0                 # Latency (seconds)
send_reply = 1            # send reply or disconnect without replying
status = '000'            # Debit status

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((HOST, PORT))

print 'Credit Card Server Simulator started on', time.asctime()
while 1:
    s.listen(1)
    conn, addr = s.accept()
    print 'Connected by', addr
    confirmation_code = '1234567'
    
    while 1:
        received = conn.recv(1024)
        print 'Received on', time.asctime(), ':'
        print `received`                           # String representation of variable "received"
        if not received:
            break
        reply = ''
        print 'Decoded:'
        lines = received.splitlines()
        
        for data in lines:
            i = 0
            while i < len(data):
                if   data[i] == 'B':
                    credit_card_number = data[i+1:i+20]
                    print 'Field B:', credit_card_number
                    credit_card_number = credit_card_number.zfill(19)
                    i = i + 20
                    
                elif data[i] == 'C':
                    for j in range(8):
                        if data[i+j+1] not in '0123456789':
                            break
                        else:
                            j = j + 1
                    debit_amount = data[i+1:i+j+1]
                    debit_amount = debit_amount.zfill(8)
                    print 'Field C:', debit_amount
                    i = i + j + 1
                    
                elif data[i] == 'D':
                    print 'Field D:', data[i+1:i+7]
                    transaction_type = data[i+1:i+3]
                    credit_type      = data[i+3:i+4]
                    currency_code    = data[i+4:i+5]
                    transaction_code = data[i+5:i+7]
                    i = i + 7
                    
                elif data[i] == 'E':
                    confirmation_code = data[i+1:i+8]
                    print 'Field E:', confirmation_code
                    i = i + 8
                    
                elif data[i] == 'J':
                    print 'Field J:', data[i+1:i+2]
                    i = i + 2
                    
                elif data[i] == 'T':
                    valid_month = data[i+1:i+5]
                    print 'Field T:', valid_month
                    i = i + 5
                    
                elif data[i] == 'U':
                    for j in range(4):
                        if data[i+j+1] not in '0123456789':
                            break
                        else:
                            j = j + 1
                    print 'Field U:', data[i+1:i+j+1]
                    i = i + j + 1
                    
                elif data[i] == 'Y':
                    for j in range(9):
                        if data[i+j+1] not in '0123456789':
                            break
                        else:
                            j = j + 1
                    print 'Field Y:', data[i+1:i+j+1]
                    i = i + j + 1
                    
                elif data[i] == 'X':
                    print 'Field X:', data[i+1:i+20]
                    i = i + 20
                    
                else:
                    print 'Error at pos', i
                    break
                    #i = i + 1
                    
            else:
                # no error, send reply
                #status = '000'
                filler1 = ' '
                #credit_card_number = '1234567890123456789'
                brand_code = '0'
                clearing_code = '1'
                service_code = '000'
                j_param = '0'
                #valid_month = '0105'
                id_answer = '2'
                ccv2_answer = '2'
                #debit_amount = '00010000'
                filler2 = '        '
                sum_in_stars = '        '
                company_code = '1'
                #transaction_type = '02'
                #credit_type = '1'
                filler3 = ' '
                #currency_code = '1'
                #transaction_code = '01'
                club_code = 'A'
                connection_reason = '8'
                confirmation_source = '2'
                #confirmation_code = '1234567'
                first_payment = '00010000'
                regular_payment = '00010000'
                num_payments = '01'
                file_number = '12'
                cash_number = '123'
                cash_serial_number = '123'
                card_name = 'ABCDEFGHIJKLMNO'
                abroad_indicator = '0'
                unique_number = '1234567890123456789'
                
                out = status + filler1 + credit_card_number + brand_code + clearing_code + service_code + \
                      j_param + valid_month + id_answer + ccv2_answer + debit_amount + filler2 + sum_in_stars + \
                      company_code + transaction_type + credit_type + filler3 + currency_code + transaction_code + \
                      club_code + connection_reason + confirmation_source + confirmation_code + first_payment + \
                      regular_payment + num_payments + file_number + cash_number + cash_serial_number + \
                      card_name + abroad_indicator + unique_number
                reply = reply + out + '\r' + '\n'

        # Apply latency if needed
        if delay > 0:
            time.sleep(delay)

        # Send reply
        if send_reply:
            print 'Reply:'
            print reply
            conn.send(reply)
    conn.close()

