# Note: this script makes heavy us of regex to extract sent and reply times from HRC's emails
#       due to non-uniform date strings, some values slip through the regex, leaving room for improvement
#       However, the number of samples missed is relatively small, and should not too greatly affect reply time statistics
#       Some checking of long reply times is also needed to ensure that it is not caused by some regex issue

import pandas as pd
import sqlite3
import seaborn as sns
import numpy as np
from collections import Counter
import matplotlib.colors as mcolors
import matplotlib.pyplot as plt

import sys
import re

# read in sqlite database
import sqlite3
con = sqlite3.connect('database.sqlite')

#Grab all emails sent from HC 
emails_from = pd.read_sql_query("""SELECT * FROM emails WHERE MetadataFrom='H'""", con)
# Grab all emails which were replies from HC
hilary_replied = pd.read_sql_query("""SELECT * FROM emails WHERE MetadataFrom='H' AND ExtractedSubject LIKE 'Re:%' """, con)


sent_times = []
replied_times = []

# get the message times (original message and the reply)
mail_num = 0
for text in hilary_replied['RawText']:
        
    # some dates have quotes or weird characters. Remove them
    text = text.replace('\'', '')
    
    #dates = re.findall(r'Sent:\s+(\w.*)', text)
    dates = re.findall(r'Sent:\s+(\w.*)', text)

    try:
        sent = dates[1]
        replied = dates[0]
        sent_times.append(sent)
        replied_times.append(replied)
        #print mail_num, dates[0], dates[1]
    except:
        continue  # some dates will be malformed and can't be caught by our regex. Skip them as they are a small percentage

    mail_num = mail_num + 1

    
# Convert sent times and replied times into datestrings that can be subtracted and sorted       
from datetime import datetime

reply_datetimes = []
sent_datetimes = []

for replied_time in replied_times:
    replied_time = re.sub(",", "", replied_time)
    data = replied_time.split(' ') # split by spaces
    date_object = 0
    try:
        date_object = datetime.strptime(replied_time, '%A %B %d %Y %I:%M %p')  
        reply_datetimes.append(date_object)
    except:
        reply_datetimes.append(0)
        continue

for sent_time in sent_times:
    #print sent_time
    sent_time = re.sub(",", "", sent_time)
    try:
        date_object = datetime.strptime(sent_time, '%a %b %d %H:%M:%S %Y')  
        sent_datetimes.append(date_object)
    except:
        sent_datetimes.append(0)
        continue

#for i in range(0, len(sent_times)):
#    print "Date Sent: %s - Date HRC replied: %s" % (sent_times[i],replied_times[i])  
#    print 




reply_times_delta = []

for i in range(0, len(sent_datetimes)):
    #print reply_datetimes[i], sent_datetimes[i]
    if (reply_datetimes[i] !=0 and sent_datetimes[i] != 0):  # marked bad regex with zero value - ignore
        reply_times_delta.append(reply_datetimes[i] - sent_datetimes[i])
    


import datetime
average_timedelta = sum(reply_times_delta, datetime.timedelta(0)) / len(reply_times_delta)
print average_timedelta






