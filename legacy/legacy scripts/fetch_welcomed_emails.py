# Save a df of all email addrsses who have been sent a welcome message
# By Jake Vestal

import win32com.client as win32
import pandas as pd
import os

subject = "Welcome to the 2022 FINTECH Trading Competition!"

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

welcomed_emails = pd.DataFrame(emailed, columns=['email'])
welcomed_emails.to_csv(
    os.getenv('APP_BASE_PATH') + '\\duke_fintech_trading_competition_' + \
    '2022' + '\\welcomed_emails.csv',
    index = False
)
