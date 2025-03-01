# Sends the welcome email to participants who just registered on WuFoo
# By Jake Vestal

import time
import win32com.client as win32
import codecs
import requests
from bs4 import BeautifulSoup
import os
import json
import pandas as pd


subject = 'Duke FinTech Trading Competition: REGISTRATION EXTENDED TO FRIDAY ' \
          '06 Oct'
message_filename = 'final_invite.html'

with codecs.open(
        'C:\\Users\\Jake\\Desktop\\fintech.trading.competition\\legacy' + \
        '\\legacy scripts\\Email Templates\\' + message_filename, 'r') as f:
    message_body = f.read()

email_df = pd.read_csv("C:\\Users\\Jake\Desktop\\fintech.trading.competition"
                       "\\legacy\\legacy scripts\\Email "
                       "Templates\\contacts_final_final.csv")

for i, row in email_df.iterrows():

    print('emailing: ', row.SCHOOL)

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)
    mail.To = row.to
    mail.CC = "zic3@duke.edu"
    mail.Attachments.Add('C:\\Users\\Jake\\Desktop\\2023 FinTech Trading '
                             'Competition ACCOUNCEMENT FINAL.pdf')
    mail.Subject = subject
    mail.HTMLBody = message_body.replace(
        'salutation', row.salutation
    )

    mail.Send()

    time.sleep(2.5)
