
# Sends the welcome email to participants who just registered on WuFoo
# By Jake Vestal

import time
import win32com.client as win32
import codecs
import pandas as pd


subject = 'Account Deletion Notice'
message_filename = 'account_deletion_notice.html'

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts' + \
        '\\Email Templates\\' + message_filename, 'r') as f:
    message_body = f.read()

email_df = pd.read_csv(
    'C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\' + \
    'trader_key_private.csv'
)
email_df = email_df[email_df['status'] == 'pending deletion']

for i, row in email_df.iterrows():

    print('emailing: ', row.email_ibkr)

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)
    mail.To = row.email_ibkr
    # mail.To = 'jmv13@duke.edu'
    mail.Subject = subject
    mail.HTMLBody = message_body.replace(
        'first_name', row.first_name
    ).replace(
        'tradername', row.tradername
    ).replace(
        'account_id', row.account_id
    ).replace(
        'date_created', row.DateCreated[:10]
    )

    mail.Send()

    time.sleep(2.5)
