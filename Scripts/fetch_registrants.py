from urllib.request import HTTPPasswordMgrWithDefaultRealm, \
    HTTPBasicAuthHandler, build_opener, install_opener, urlopen
import json
import os

def fetch_registrants():
  
  base_url = 'https://dukefinance.wufoo.com/api/v3/'
  username = os.environ['WUFOO_REGISTRATION_KEY']
  password = 'footastic'

  password_manager = HTTPPasswordMgrWithDefaultRealm()
  password_manager.add_password(None, base_url, username, password)
  handler = HTTPBasicAuthHandler(password_manager)
  opener = build_opener(handler)

  install_opener(opener)

  response = urlopen(base_url+'forms/duke-fintech-trading-competition/entries.json?sort=EntryId&sortDirection=DESC')
  data = json.load(response)
  
  return json.dumps(data, indent=4, sort_keys=True)
