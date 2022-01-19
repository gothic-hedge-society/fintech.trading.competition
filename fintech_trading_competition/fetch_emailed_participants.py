# Returns a list of every email address to which the Competition email
#   account has sent an email having a specified subject line
# By Jake Vestal

import win32com.client as win32
import os
import pandas as pd

def fetch_emailed_participants(subject):

    # subject = "Welcome to the 2022 FINTECH Trading Competition!"

    messages = win32.Dispatch('outlook.application').GetNamespace(
        'MAPI').Folders("fintechtradingcompetition@duke.edu").Folders(
        "Sent Items").Items.Restrict("[Subject] = '" + subject + "'")

    emailed = []

    for message in messages:
        if len(message.ConversationIndex) == 44:
            recipient = message.Recipients(1)

            if 'EX' == recipient.AddressEntry.Type:
                sent_to = recipient.AddressEntry.GetExchangeUser(
                    ).PrimarySmtpAddress

            if 'SMTP' == recipient.AddressEntry.Type:
                sent_to = recipient.AddressEntry.Address

            emailed.append(sent_to)

    invited = pd.DataFrame(emailed, columns=['invited'])

    invited.to_csv(
        os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
        '2022' + '\\invited.csv',
        index=False
    )

    return list(invited['invited'])
