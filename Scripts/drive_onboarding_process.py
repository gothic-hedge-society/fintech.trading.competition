# Sends the welcome email to participants who just registered on WuFoo
# 'WELCOME' means you've sent them an IBKR invite AND a welcome email.
# By Jake Vestal

from fintech_trading_competition import *
import os

# Fully up-to-date and complete WuFoo form
competition_registrants = fetch_wufoo_registrants()

# Get emails of valid participants
specially_allowed_emails = ['u.nemudrova@gmail.com']
# If mail delivery subsystem says 'undeliverable', banned for some reason, etc
bad_emails = ['hongzhen.du@student.xjtlu.edu.cn', 'c.you@edu.com']
def email_filter(email_address):
    if email_address in bad_emails:
        return False
    elif email_address in specially_allowed_emails:
        return True
    elif '.edu' in email_address:
        return True
    else:
        return False
all_signed_up_emails_to_date = set(
    filter(email_filter, competition_registrants.email)
)

print("Fetching participants who've been welcomed")

# Fetch a list of the participating emails that have already been emailed their
#   IBKR forms and welcome messages
sbjct = "Welcome to the 2022 FINTECH Trading Competition!"
already_welcomed = set([x.lower() for x in fetch_emailed_participants(sbjct)])

# Need to invite the registrants who signed up but haven't been welcomed:
need_to_invite = list(all_signed_up_emails_to_date.difference(already_welcomed))

print("Welcoming new registrants...")

# Go through the email invitiation process
welcome_new_registrants(need_to_invite, competition_registrants, 10, sbjct)

print("Updating invited participant list...")

# Re-fetch all invited participants
all_invited_participants = set(
    filter(email_filter, fetch_emailed_participants(sbjct))
)

print("Saving the registry")

# Now have the registry csv
registry = competition_registrants[
    competition_registrants.email.isin(all_invited_participants)
].drop_duplicates(subset = 'email')

# Save the registry
registry.to_csv(
    os.getenv('APP_BASE_PATH') + \
    '\\duke_fintech_trading_competition_2022\\registry.csv',
    index = False
)

print('refreshing R package...')

# Refresh the R package
import subprocess

command = 'Rscript'
path2script = os.getenv('APP_BASE_PATH') + \
    '\\fintech.trading.competition\\data-raw\\registry.R'
cmd = [command, path2script]
registry_response = subprocess.check_output(cmd, universal_newlines=True)
