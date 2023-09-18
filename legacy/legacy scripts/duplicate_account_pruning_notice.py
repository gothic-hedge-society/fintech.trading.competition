# Sends the welcome email to participants who just registered on WuFoo
# By Jake Vestal

import time
import win32com.client as win32
import codecs
import pandas as pd
import math

subject = 'DELETING DUPLICATE ACCOUNT'
message_filename = 'duplicate_account_pruning_notice.html'

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
    'accounts_to_prune.csv'
)

for i, row in email_df.iterrows():

    print('emailing: ', row.email)

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)
    mail.To = row.email
    mail.Subject = subject
    mail.HTMLBody = message_body.replace(
        'name', row['name']
    ).replace('duplicate_account', row.account_id)

    mail.Send()

    time.sleep(2.5)
