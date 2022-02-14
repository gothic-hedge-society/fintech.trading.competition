# Sends the welcome email to participants who just registered on WuFoo
# 'WELCOME' means you've sent them an IBKR invite AND a welcome email.
# By Jake Vestal


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

print("Finished")

# print('refreshing R package...')
#
# # Refresh the R package
# import subprocess
#
# command = 'Rscript'
# path2script = os.getenv('APP_BASE_PATH') + \
#     '\\fintech.trading.competition\\data-raw\\registry.R'
# cmd = [command, path2script]
# registry_response = subprocess.check_output(cmd, universal_newlines=True)
