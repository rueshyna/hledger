{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import Lib.MarketPrices as LM
import Lib.CommoditiesTags as LCT
import Lib.Config as LC
import qualified Data.Text as T

cmdmode :: Mode RawOpts
cmdmode = hledgerCommandMode (unlines
    -- Command name, then --help text, then _FLAGS; empty help lines get stripped:
  ["hledger-ticker"
  ,"Usage: hledger-ticker [OPTS] [ARGS]"
  ,"or:    hledger ticker -- [OPTS] [ARGS]"
    ------------------------------------78----------------------------------------
  ,""
  ,"_FLAGS"
  ])
  [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing

main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode

    withJournalDo opts $ \j -> do
      -- parse to get tags
      let filepath = head $ jincludefilestack j
      cTags <- LCT.parse filepath
      let symbols = fmap (fmap ctyahooTicker) cTags

      -- read API token
      homeDir <- getEnv "HOME"
      let configPath = homeDir </> ".config/hledger-lots/config.yaml"
      configResult <- LC.parse configPath
      let apiToken = fmap (yahooFinanceApiKey . LC.tickers) configResult

      -- get price
      price <- sequence $ craw <$> apiToken <*> symbols
      case (join price) of
        Left e -> print e
        Right ts -> mapM_ putStrLn $ fmap (T.unpack . LM.strPriceDirtive) (ts :: [Ticker])

     -- TODO
     -- handle alias
     -- handle amountStyle
     -- case M.lookup "ibkr" $ jcommodities j of
     --    Just c -> putStrLn $ showAmount $ Amount "ibkr" 1234 (fromMaybe amountstyle $ cformat  c) Nothing
     --    Nothing -> fail "ibkr not found"
