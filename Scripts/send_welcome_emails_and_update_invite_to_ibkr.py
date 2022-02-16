# Gives a handy string to copy into IBKR for invites
# Sends personalized invite emails
# By Jake Vestal

import time
import os
import pandas as pd
import win32com.client as win32
import win32clipboard as clip
from pprint import pprint
from math import ceil

# Max number of comma-delimited emails that IBKR's invite system can handle
ibkr_max = 10

need_welcome_emails = list(set(pd.read_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\wufoo_valid_registrants.csv',
)['email']) - set(pd.read_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\welcomed_emails.csv',
)['email']))

need_ibkr_invitations = list(set(pd.read_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\wufoo_valid_registrants.csv',
)['email']) - set(pd.read_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\flex_statement_df.csv',
)['email_ibkr']))

YOU ARE HERE.

def chunkify(x, n):
    chunks = list()
    for i in range(0, ceil(len(x) / n)):
        chunks.append(x[(i * n):((i + 1) * n)])
    return chunks

# Take 10 at a time, IBKR can't handle more than that
invite_chunks = chunkify(need_to_invite, ibkr_max)

with open('C:\\Users\\vcm\\Desktop\\need_to_invite.txt', 'w') as f:
    f.write('need_to_invite')

for chunk in invite_chunks:

    invite_string = ", ".join(chunk)
    print('Please invite the following batch:')
    pprint(invite_string)
    clip.OpenClipboard()
    clip.EmptyClipboard()
    clip.SetClipboardText(invite_string, clip.CF_UNICODETEXT)
    clip.CloseClipboard()
    print('This string has been added to the clipboard.')
    input('Press any key when ready.')

    for invite_email in chunk:

        print('inviting: ', invite_email)

        participant_row = full_wufoo_form.loc[
            full_wufoo_form['email'] == invite_email
            ]

        outlook = win32.Dispatch('outlook.application')
        mail = outlook.CreateItem(0)
        mail.To = invite_email
        mail.Subject = sbj
        mail.Body = message_body.replace(
            'yourusername', participant_row.iloc[0]['tradername']
        ).replace(
            'firstname', participant_row.iloc[0]['first_name']
        )

        mail.Send()

        time.sleep(2.5)
