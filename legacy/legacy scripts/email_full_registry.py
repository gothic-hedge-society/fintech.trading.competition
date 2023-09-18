# Sends the welcome email to participants who just registered on WuFoo
# By Jake Vestal

import time
import win32com.client as win32
import codecs
import pandas as pd
import os


subject_chaser = 'DUKE FINTECH TRADING COMPETITION CHASER 1'
subject_update = 'DUKE FINTECH TRADING COMPETITION UPDATE 2'

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts' + \
        '\\Email Templates\\chaser_1.html', 'r') as f:
    message_body_chaser = f.read()

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts' + \
        '\\Email Templates\\update_2.html', 'r') as f:
    message_body_update = f.read()

full_registry = pd.read_csv(
    'C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\' + \
    'full_registry.csv'
)

for i, row in full_registry.iterrows():

    print('emailing: ', row.email)

    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)
    mail.To = row.email

    if row.account_id == 'account pending creation':
        mail.Subject = subject_chaser
        mail.HTMLBody = message_body_chaser.replace(
            'first_name', row.first_name).replace('tradername', row.tradername)
        attachment = os.getenv('APP_BASE_PATH') + "\\fintech.trading.competition" + \
                     "\\Scripts\\Email Templates\\paper_trader_invite.png"
        mail.Attachments.Add(attachment)
    else:
        mail.Subject = subject_update
        mail.HTMLBody = message_body_update.replace(
            'first_name', row.first_name).replace(
            'tradername', row.tradername).replace('account_id', row.account_id)

    mail.Send()
    time.sleep(2.5)
