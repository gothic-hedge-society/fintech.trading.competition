# Fetch most recent Wufoo respondants and save as json text
# By Jake Vestal

import requests
from bs4 import BeautifulSoup
import pandas as pd

YYYY = 2022


# Requests the USDT's daily yield data for a given year. Results are
#   returned as a DataFrame object with the 'Date' column formatted as a
#   pandas datetime type.

URL = 'https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=' + \
      'daily_treasury_yield_curve&field_tdr_date_value=' + str(YYYY)

cmt_rates_page = requests.get(URL)

soup = BeautifulSoup(cmt_rates_page.content, 'html.parser')

table_html = soup.findAll('table', {'class': 'views-table'})

df = pd.read_html(str(table_html))[0]
df.Date = pd.to_datetime(df.Date)

usdt_3mo_cmt = df.rename(columns={'3 Mo': '3_mo'})[['Date', "3_mo"]]

usdt_3mo_cmt.to_csv('C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\usdt_3mo_cmt.csv', index = False)
