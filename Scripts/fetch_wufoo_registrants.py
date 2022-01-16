# Fetch most recent Wufoo respondants and save as csv
# By Jake Vestal

import requests
from bs4 import BeautifulSoup
import os
import json
import pandas as pd

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
