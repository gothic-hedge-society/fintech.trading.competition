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


subject = 'DUKE FINTECH TRADING COMPETITION SET TO BEGIN'
message_filename = 'start_and_pruning.html'

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts' + \
        '\\Email Templates\\' + message_filename, 'r') as f:
    message_body = f.read()

# email_df = pd.read_csv(
#     'C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\' + \
#     'wufoo_registrants.csv'
# ).drop_duplicates(subset=['email'])

# email_df = pd.read_csv(
#     'C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\' + \
#     'full_registry.csv'
# )

email_df = pd.read_csv(
    'C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\' + \
    'full_registry.csv'
)

for i, row in full_registry.iterrows():

    print('emailing: ', row.email)

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)
    mail.To = row.email
    mail.Subject = subject
    mail.HTMLBody = message_body.replace(
        'first_name', row.first_name
    )

    mail.Send()

    time.sleep(2.5)
