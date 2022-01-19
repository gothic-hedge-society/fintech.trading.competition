# Sends the welcome email to participants who just registered on WuFoo
# 'WELCOME' means you've sent them an IBKR invite AND a welcome email.
# By Jake Vestal

import fintech_trading_competition.fetch_wufoo_registrants as get_registrants
import fintech_trading_competition.fetch_emailed_participants as get_invited
import fintech_trading_competition.welcome_new_registrants as welcome
import pandas as pd

# Fully up-to-date and complete WuFoo form
competition_registrants = get_registrants()
all_signed_up_emails_to_date = set(filter(
    lambda x:'.edu' in x, [x.lower() for x in competition_registrants.email]
))


# Fetch a list of the participating emails that have already been emailed their
#   IBKR forms and welcome messages
sbjct = "Welcome to the 2022 FINTECH Trading Competition!"
already_welcomed = set([x.lower() for x in get_invited(sbjct)])
need_to_invite = list(all_signed_up_emails_to_date.difference(already_welcomed))

welcome(need_to_invite, competition_registrants, 10, sbjct)

pd.DataFrame(
    set([x.lower() for x in get_invited(sbjct)]), columns = ['invited']
).to_csv(
    'C:\\Users\\vcm\\Desktop\\' + \
    'duke_fintech_trading_competition_2022\\invited.csv',
    index = False
)
