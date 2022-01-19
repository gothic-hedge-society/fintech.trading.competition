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


subject = 'DUKE FINTECH TRADING COMPETITION UPDATE 19 Jan'
message_filename = 'update_1.txt'

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts' + \
        '\\Email Templates\\' + message_filename, 'r') as f:
    message_body = f.read()

invited = pd.read_csv(
    'C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\' + \
    'wufoo_registrants.csv'
).drop_duplicates(subset=['email'])

for i, row in invited.iterrows():

    print('emailing: ', row.email)

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)
    mail.To = row.email
    mail.Subject = subject
    mail.Body = message_body.replace(
        'tradername', row.tradername
    )

    mail.Send()

    time.sleep(2.5)
