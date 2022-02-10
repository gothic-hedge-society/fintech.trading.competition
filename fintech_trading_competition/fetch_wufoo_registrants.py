# Fetch most recent Wufoo respondants and save as csv
# By Jake Vestal

import requests
from bs4 import BeautifulSoup
import os
import json
import pandas as pd
from math import ceil

def fetch_wufoo_registrants():

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
        'Field3': 'email', 'Field4': 'School', 'Field124': 'graduation_year',
        'Field120': 'undergrad_major', 'Field10': 'graduate_dept',
        'Field118': 'sex', 'Field116': 'country', 'Field5': 'tradername',
        'Field126': 'linkedin'
    })

    competition_registrants.email = competition_registrants.email.str.lower()

    competition_registrants.to_csv(
        os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
        '2022' + '\\wufoo_registrants.csv',
        index = False
    )

    return competition_registrants
