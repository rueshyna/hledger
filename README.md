# Hledger
Additional plugins based on **hledger** 1.32.1 for fetching market prices and displaying lots from commodities.

## How to Install
```bash
stack build hledger-lots
stack install hledger-lots
```
Both `hledger-lots` and `hledger-tickers` should be installed.

## hledger-lots
The command is similar to `reg`, but it displays additional information by adding the average cost per unit, represented with `@`.

```bash
> cat ./stock.journal
; example data

2021-07-16 * buy
    assets:tsmc         tsmc 100 @@ $50000
    assets:brokerage               $-50000

2022-07-26 * buy
    assets:tsmc         tsmc 50 @@ $27500
    assets:brokerage               $-27500

2024-06-24 * sell
    assets:tsmc         tsmc -150 @@ $90000
    assets:brokerage                $90000
```

```bash
> hledger lots assets:tsmc -f stock.journal
2021-07-16 buy           assets:tsmc               tsmc 100 @ $500    tsmc 100 @@ $50000 @ $500
2022-07-26 buy           assets:tsmc                tsmc 50 @ $550    tsmc 150 @@ $77500 @ $517
2024-06-24 sell          assets:tsmc              tsmc -150 @ $600            tsmc 0 @@ $-12500
```

## hledger-tickers
Supports fetching current market prices from the Yahoo Finance API, and also supports multiple currencies. Additional tags following the **hledger** format are required.

Tags:
- `yahoo-ticker` is optional. If it isn't provided and `status` is active, an error will occur.
- `status` defaults to inactive. `hledger-tickers` will only fetch market prices when `status` is active.
- `name` is optional. This is a human-readable name.
- `alias` is optional. If the currency of the commodity differs from the Yahoo Finance API, `alias` is required.

```bash
> cat ./commodity.journal
; example data

commodity $1000.                   ; yahoo_ticker:, status:inactive, alias:TWD
commodity USD1000.00               ; yahoo_ticker:TWD=X, status:active
commodity EUR1000.00               ; yahoo_ticker:EURUSD=X, status:active
commodity twfiv 1000.              ; yahoo_ticker:0050.TW, status:active, name:台灣50
commodity tsmc 1000.               ; yahoo_ticker:2330.TW, status:active, name:台積電
commodity aapl 1000.0000           ; yahoo_ticker:AAPL, status:active
commodity vti 1000.0000            ; yahoo_ticker:VTI, status:active
```

```bash
> hledger tickers -f ./commodity.journal
P 2024-09-19 USD $31.981
P 2024-09-19 EUR USD1.1104941
P 2024-09-19 twfiv $179.7
P 2024-09-19 tsmc $949.0
P 2024-09-18 aapl USD220.69
P 2024-09-18 vti USD277.35
```
