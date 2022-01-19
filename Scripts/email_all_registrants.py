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

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts\\Email Templates\\IBKR_is_down.html',
        'r') as f:
    message_body = f.read()

subject = 'DUKE FINTECH TRADING COMPETITION UPDATE 18 Jan'

########################################################################################################################
# Refresh local WuFoo

def get_wufoo_data(url, un, pwd):
    req = requests.get(url, auth = (un, pwd))
    soup = BeautifulSoup(req.text, 'html.parser')
    return json.loads(soup.text)

def fetch_entries(pgstart):
    entries_json = get_wufoo_data(os.getenv('WUFOO_ENTRIES_URL').replace("pgstart", str(pgstart)),
                   os.getenv('WUFOO_UN'), os.getenv('WUFOO_PWD'))['Entries']
    return pd.json_normalize(entries_json)

number_of_entries_json = get_wufoo_data(os.getenv('WUFOO_COUNT_URL'), os.getenv('WUFOO_UN'), os.getenv('WUFOO_PWD'))
number_of_entries = int(number_of_entries_json['EntryCount'])
competition_registrants = fetch_entries(0)

while competition_registrants.shape[0] < number_of_entries:
    new_fetch = fetch_entries(competition_registrants.shape[0])
    competition_registrants = pd.concat([competition_registrants, new_fetch])

competition_registrants = competition_registrants.rename(columns={
  'EntryId': 'id', 'Field1': 'first_name', 'Field2': 'last_name',
  'Field3': 'email', 'Field4': 'School', 'Field124': 'graduation_year',
  'Field120': 'undergrad_major', 'Field10': 'graduate_dept', 'Field118': 'sex',
  'Field116': 'country', 'Field5': 'tradername',  'Field126': 'linkedin'
})

competition_registrants.to_csv('C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\wufoo_registrants.csv', index = False)

registrants = list(filter(lambda x:'.edu' in x, competition_registrants['email']))


for invite_email in registrants:

    print('emailing: ', invite_email)

    participant_row = competition_registrants.loc[competition_registrants['email'] == invite_email]

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)

    mail.To = invite_email
    mail.Subject = subject
    mail.Body = message_body.replace('firstname', participant_row.iloc[0]['first_name'])

    mail.Send()

    time.sleep(2.5)
