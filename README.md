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

Supports fetching current market prices from the Yahoo Finance API, and supports multiple currencies. Additional tags following the **hledger** format are supported, including the new **group** feature.

> New in this version: **Group (group)** — tag commodities with one or more subgroups, and declare up to two levels of grouping (Top group ➝ Subgroups).

### Tags

- `yahoo-ticker` (optional): If omitted while `status` is `active`, the command fails with an error.
- `status` (default: `inactive`): `hledger-tickers` fetches prices only when `status` is `active`.
- `name` (optional): Human-readable name.
- `alias` (optional): Required if the commodity’s currency differs from the Yahoo Finance API’s currency.
- `group` (optional / **new**): One or more subgroup names, comma-separated. Subgroups must be defined in a `;; G` declaration (see next section).

### Group declarations (two-level structure) — **new**

Declare groups at the top of your journal using lines that start with `;; G`. Each line defines a **top group** and a comma-separated list of **subgroups**:

```
;; G <top-group> <subgroup1>,<subgroup2>,<subgroup3>
```

Rules:

- One `;; G` line = one top group with multiple subgroups.
- Subgroup names must not contain spaces; separate multiple subgroups with commas.
- On any `commodity` line, the `group:` tag must reference one or more **previously declared subgroups** (comma-separated is allowed).
- Exactly two levels are supported (top group ➝ subgroup); deeper nesting is not supported.


### Example

```bash
> cat ./commodity.journal
; example data

;; G Currency currency
;; G TW semi,tw_etf,network,ai_pc,tradition
;; G US us_etf,m7

; active
commodity $1000.00                 ; yahoo_ticker:, status:inactive, alias:TWD, group:currency
commodity USD1000.00               ; yahoo_ticker:TWD=X, status:active, group:currency
commodity EUR1000.00               ; yahoo_ticker:EURUSD=X, status:active, group:currency

commodity tsmc 1000.00             ; yahoo_ticker:2330.TW, status:active, name:台積電, group:giant_tech
commodity delta 1000.00            ; yahoo_ticker:2308.TW, status:active, name:台達電, group:giant_tech
commodity mtk 1000.00              ; yahoo_ticker:2454.TW, status:active, name:聯發科, group:giant_tech

commodity aapl 1000.0000           ; yahoo_ticker:AAPL, status:active, group:m7
commodity nvda 1000.0000           ; yahoo_ticker:NVDA, status:active, group:m7
commodity meta 1000.0000           ; yahoo_ticker:META, status:active, group:m7

commodity vti 1000.0000            ; yahoo_ticker:VTI, status:active, group:us_etf
commodity spy 1000.0000            ; yahoo_ticker:SPY, status:active, group:us_etf


```bash
> hledger tickers -f ./commodity.journal
P 2025-10-21 USD $30.627
P 2025-10-21 EUR USD1.1655

P 2025-10-21 tsmc $1465.00
P 2025-10-21 delta $400.00
P 2025-10-21 mtk $1340.00

P 2025-10-21 aapl USD262.24
P 2025-10-21 nvda USD182.64
P 2025-10-21 meta USD732.17

P 2025-10-21 vti USD330.95
P 2025-10-21 spy USD671.30
```
