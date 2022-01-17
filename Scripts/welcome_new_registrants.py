# Sends the welcome email to participants who just registered on WuFoo
# By Jake Vestal

import time
import pandas as pd
import win32com.client as win32
import codecs

registrants = pd.read_csv('C:\\Users\\vcm\\Desktop\\duke_fintech_trading_competition_2022\\wufoo_registrants.csv')
invited = pd.read_csv('C:\\Users\\vcm\\Desktop\\invited\\invited.csv')

invited_registrants = pd.merge(registrants, invited, on = 'email')

with codecs.open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts\\Email Templates\\welcome_new_registrant.html',
        'r') as f:
    message_body = f.read()

for index, row in invited_registrants[['email', 'tradername']].iterrows():
    outlook = win32.Dispatch('outlook.application')
    mail = outlook.CreateItem(0)

    mail.To = row['email']
    mail.Subject = 'Welcome to the 2022 FINTECH Trading Competition!'
    mail.Body = message_body.replace('yourusername', row['tradername'])

    mail.Send()

    time.sleep(2.5)
