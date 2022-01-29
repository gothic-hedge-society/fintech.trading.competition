import pandas as pd
from ibapi.client import EClient
from ibapi.wrapper import EWrapper
import threading
import time
import os

def fetch_managed_accounts():

    class ibkr_app(EWrapper, EClient):
        def __init__(self):
            EClient.__init__(self, self)
            self.error_messages = pd.DataFrame(columns = [
                'reqId', 'errorCode', 'errorString'
            ])
            self.managed_accounts = []

        def error(self, reqId, errorCode, errorString):
            print("Error: ", reqId, " ", errorCode, " ", errorString)
            df = pd.DataFrame({
                'reqId':[reqId],
                'errorCode':[errorCode],
                'errorString':[errorString]
            })
            self.error_messages = self.error_messages.append(df)

        def managedAccounts(self, accountsList):
            self.managed_accounts = [i for i in accountsList.split(",") if i]

    app = ibkr_app()

    app.connect('127.0.0.1', 4002, 1100)
    while not app.isConnected():
        time.sleep(0.5)

    print('connected')

    def run_loop():
        app.run()

    # Start the socket in a thread
    api_thread = threading.Thread(target=run_loop, daemon=True)
    api_thread.start()

    while len(app.managed_accounts) == 0:
        time.sleep(0.5)

    print('handshake complete')

    return app.managed_accounts

managed_accounts = fetch_managed_accounts()

pd.DataFrame({'managed_accounts':managed_accounts}).to_csv(
    os.getenv('APP_BASE_PATH') + \
    '\\duke_fintech_trading_competition_2022\\managed_accounts.csv',
    index=False
)
