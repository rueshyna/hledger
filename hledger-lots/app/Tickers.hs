{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import Hledger.Ticker.Report

import qualified Data.ByteString.Lazy.Char8 as BL


main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode
    if isDisplayCommodityInfo opts
    then
      BL.putStrLn =<< displayCommodityInfo opts
    else fetchMarketPrice opts

