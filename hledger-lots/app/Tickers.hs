{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import Hledger.Ticker.Report

main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode
    if isDisplayCommodityInfo opts
    then
      displayCommodityInfo opts >>= ppDisplayCommodityInfo
    else fetchMarketPrice opts >>= ppMarketPrice opts

