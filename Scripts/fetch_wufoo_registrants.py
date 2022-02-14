# Fetch most recent Wufoo respondants and save as csv
# By Jake Vestal

import requests
from bs4 import BeautifulSoup
import os
import json
import pandas as pd
from math import ceil

def get_wufoo_data(url, un, pwd):
    req = requests.get(url, auth=(un, pwd))
    soup = BeautifulSoup(req.text, 'html.parser')
    return json.loads(soup.text)

def fetch_entries(pgstart):
    # Fetches all entries for a single WuFoo form page of 100
    entries_json = get_wufoo_data(
        os.getenv('WUFOO_ENTRIES_URL').replace("pgstart", str(pgstart)),
        os.getenv('WUFOO_UN'), os.getenv('WUFOO_PWD')
    )['Entries']
    return pd.json_normalize(entries_json)

print("Checking WuFoo")
print("Querying WuFoo for Number of Entries")

number_of_entries_json = get_wufoo_data(
    os.getenv('WUFOO_COUNT_URL'),
    os.getenv('WUFOO_UN'),
    os.getenv('WUFOO_PWD')
)
number_of_entries = int(number_of_entries_json['EntryCount'])

print('Fetching first WuFoo batch...')

competition_registrants = fetch_entries(0)
number_of_fetches = ceil(number_of_entries/100)

for i in range(1, number_of_fetches):
    print(
        'Fetching next WuFoo batch (' + str(i) + ' of ' + \
        str(number_of_fetches - 1) + ')'
    )
    competition_registrants = pd.concat(
        [competition_registrants, fetch_entries(competition_registrants.shape[0])],
        ignore_index=True
    )

competition_registrants = competition_registrants.rename(columns={
    'EntryId': 'id', 'Field1': 'first_name', 'Field2': 'last_name',
    'Field3': 'email', 'Field4': 'school', 'Field124': 'graduation_year',
    'Field120': 'undergrad_major', 'Field10': 'graduate_dept',
    'Field118': 'sex', 'Field116': 'country', 'Field5': 'tradername',
    'Field130': 'secret', 'Field126': 'linkedin'
})

competition_registrants['email'].str.lower()
competition_registrants['first_name'].str.strip()
competition_registrants['last_name'].str.strip()
competition_registrants['email'].str.strip()
competition_registrants['school'].str.strip()
competition_registrants['country'].str.strip()
competition_registrants['tradername'].str.strip()
competition_registrants['secret'].str.strip()

valid_email_vector = ['\.edu$', '\.edu\.', 'cam\.ac\.uk', 'nemudrova']
valid_emails = pd.DataFrame([competition_registrants['email'].str.contains(x) for x in valid_email_vector]).any()

invalid_registrants = competition_registrants[~valid_emails]
valid_registrants = competition_registrants[valid_emails]

competition_registrants.to_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\wufoo_complete_form_data.csv',
    encoding = 'utf_8_sig',
    index = False
)
invalid_registrants.to_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\wufoo_invalid_registrants.csv',
    encoding = 'utf_8_sig',
    index = False
)
valid_registrants.to_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\wufoo_valid_registrants.csv',
    encoding = 'utf_8_sig',
    index = False
)
