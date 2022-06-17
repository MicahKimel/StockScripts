from datetime import datetime, date
import time
import os
from io import StringIO
import threading, socket, sys, time
import pandas as pd
import pyodbc
import requests
import json

conn = pyodbc.connect('DSN=MYSQL_Stock')
df = pd.read_sql_query('SELECT distinct Ticker FROM stock_db.options;', conn)

#change directory to script location
abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

result = pd.DataFrame()
url = "https://api.tdameritrade.com//v1/marketdata/chains?apikey=LOCAL_KEY6&symbol="
def getChain(worker):
    urlRequest = url+df.Ticker[worker]
    urlData = requests.get(urlRequest).content
    j = json.load(StringIO(urlData.decode('utf-8')))
    calls = j['callExpDateMap']
    puts = j['putExpDateMap']
    rawData = pd.DataFrame.from_dict(calls)
    result.append(rawData, ignore_index=True, sort=False)
    


def threader():
    while True:
        worker = q.get()
        getChain(worker)
        q.task_done()
q = Queue()
for x in range(120):
    t = threading.Thread(target=threader)
    t.daemon = True
    t.start()

timenow = datetime.now()
df.index += 1
for worker in range(1, df.count()):
    if (worker % 120 == 0):
        later = datetime.now()
        sub = later - timenow
        ProccessTime = 61 - sub.total_seconds()
        if (ProccessTime > 0):
            time.sleep(ProccessTime)
        timenow = datetime.now()
    q.put(worker)  

x = q.join()