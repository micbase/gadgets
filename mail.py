#!/usr/bin/python2.7

import imaplib
import os

#first field is imap server, second - port (993 for gmail SSL IMAP)
M=imaplib.IMAP4_SSL("imap.gmail.com", 993)
#first field is imap login (gmail uses login with domain and '@' character), second - password
M.login("user@gmail.com", "passwd")

status, counts = M.status("Inbox", "(MESSAGES UNSEEN)")

unread = counts[0].split()[4][:-1]
if int(unread) == 0:
	print "  No new mails.  "
else:
	os.system("notify-send '<span color=\"green\" size=\"18000\">You have " + unread  + " new mail(s)!</span>'")

M.logout()
