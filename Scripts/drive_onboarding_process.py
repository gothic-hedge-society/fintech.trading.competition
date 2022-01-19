# Sends the welcome email to participants who just registered on WuFoo
# 'WELCOME' means you've sent them an IBKR invite AND a welcome email.
# By Jake Vestal

import fintech_trading_competition.fetch_wufoo_registrants as get_registrants
import fintech_trading_competition.fetch_emailed_participants as get_invited
import fintech_trading_competition.welcome_new_registrants as welcome
from math import ceil

# Fully up-to-date and complete WuFoo form
competition_registrants = get_registrants()
all_signed_up_emails_to_date = set(filter(
    lambda x:'.edu' in x, competition_registrants.email
))


# Fetch a list of the participating emails that have already been emailed their
#   IBKR forms and welcome messages
sbjct = "Welcome to the 2022 FINTECH Trading Competition!"
already_welcomed = set(get_invited(sbjct))
need_to_invite = list(all_signed_up_emails_to_date.difference(already_welcomed))

welcome(need_to_invite, competition_registrants, 10, sbjct)
