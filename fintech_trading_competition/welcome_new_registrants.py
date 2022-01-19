# Gives a handy string to copy into IBKR for invites
# Sends personalized invite emails
# By Jake Vestal

import time
import win32com.client as win32
import win32clipboard as clip
from pprint import pprint
from math import ceil

def welcome_new_registrants(newly_registered, full_wufoo_form, ibkr_max, sbj):

    def chunkify(x, n):
        chunks = list()
        for i in range(0, ceil(len(x) / n)):
            chunks.append(x[(i * n):((i + 1) * n)])
        return chunks

    # Take 10 at a time, IBKR can't handle more than that
    invite_chunks = chunkify(newly_registered, ibkr_max)

    with open(
        'C:\\Users\\vcm\\Desktop\\fintech.trading.competition\\Scripts\\' + \
            'Email Templates\\welcome_new_registrant.txt', 'r') as f:
        message_body = f.read()

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
