import requests
import json
from datetime import datetime
from tkinter import *
from tkinter import ttk
import pandas as pd
import openpyxl
cryptos = ('Bitcoin', 'Ethereum', 'Tether', 'Cardano', 'Dogecoin', 'XRP', 'Litecoin') #countrynames

url = "http://api.coincap.io/v2/assets"
payload={}
headers = {}
response = requests.request("GET", url, headers=headers, data=payload)
data = json.loads(response.text)
dict_ = data['data']
prices = dict()
for currency in dict_:
    if currency['name'] in cryptos:
        prices[currency['name']] = round(float(currency['priceUsd']), 2)
gifts = {'m15' : 'Each 15 minutes', 'h1' : 'Each hour', 'h12' : 'Each 12 hours'}


root = Tk()
gift = StringVar()
sentmsg = StringVar()
statusmsg = StringVar()
cnames = StringVar(value=cryptos)
def showPopulation(*args):
    idxs = lbox.curselection()
    if len(idxs)==1:
        idx = int(idxs[0])
        name = cryptos[idx]
        price = str(prices[name])
        statusmsg.set(f"Current price of {name} : {price}")
    sentmsg.set('')

def sendGift(*args):
    idxs = lbox.curselection()
    if len(idxs)==1:
        idx = int(idxs[0])
        name = cryptos[idx]
        timeframe = list(gifts.keys())[list(gifts.values()).index(gifts[gift.get()])]
        url = "http://api.coincap.io/v2/assets/" + name.lower() + "/history?interval=" + timeframe
        payload = {}
        headers = {}
        response = requests.request("GET", url, headers=headers, data=payload)
        data = json.loads(response.text)
        dict_ = data['data']
        prices, time = [], []
        for snapshot in dict_:
            time.append(datetime.utcfromtimestamp(snapshot['time']/1000).strftime('%Y-%m-%d %H:%M:%S'))
            prices.append(round(float(snapshot['priceUsd']), 2))
        dict_df = dict(zip(time, prices))
        pd.DataFrame(pd.Series(dict_df), columns = ['prices']).to_excel(f'{name} data.xlsx')
        sentmsg.set('Completed. Look up your working directory.')


c = ttk.Frame(root, padding=(5, 5, 12, 0))
c.grid(column=0, row=0, sticky=(N,W,E,S))
root.grid_columnconfigure(0, weight=1)
root.grid_rowconfigure(0,weight=1)

lbox = Listbox(c, listvariable=cnames, height=5)
lbl = ttk.Label(c, text="Data in format of")
g1 = ttk.Radiobutton(c, text=gifts['m15'], variable=gift, value='m15')
g2 = ttk.Radiobutton(c, text=gifts['h1'], variable=gift, value='h1')
g3 = ttk.Radiobutton(c, text=gifts['h12'], variable=gift, value='h12')
send = ttk.Button(c, text='Get excel', command=sendGift, default='active')
sentlbl = ttk.Label(c, textvariable=sentmsg, anchor='center')
status = ttk.Label(c, textvariable=statusmsg, anchor=W)

lbox.grid(column=0, row=0, rowspan=6, sticky=(N,S,E,W))
lbl.grid(column=1, row=0, padx=10, pady=5)
g1.grid(column=1, row=1, sticky=W, padx=20)
g2.grid(column=1, row=2, sticky=W, padx=20)
g3.grid(column=1, row=3, sticky=W, padx=20)
send.grid(column=2, row=4, sticky=E)
sentlbl.grid(column=1, row=5, columnspan=2, sticky=N, pady=5, padx=5)
status.grid(column=0, row=6, columnspan=2, sticky=(W,E))
c.grid_columnconfigure(0, weight=1)
c.grid_rowconfigure(5, weight=1)

lbox.bind('<<ListboxSelect>>', showPopulation)
lbox.bind('<Double-1>', sendGift)
root.bind('<Return>', sendGift)

# Colorize alternating lines of the listbox
for i in range(0,len(cryptos),2):
    lbox.itemconfigure(i, background='#f0f0ff')

gift.set('h1')
sentmsg.set('')
statusmsg.set('')
lbox.selection_set(0)
showPopulation()

root.mainloop()