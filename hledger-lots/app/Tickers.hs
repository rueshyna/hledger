{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import Lib.MarketPrices as LM
import Lib.CommoditiesTags as LCT
import Lib.Config as LC
import Lib.Error

import qualified Data.Text as T
import qualified Data.Map as M

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
      -- parse to get tags, alias table, commodities' name table
      let filepath = head $ jincludefilestack j
      info <- LCT.parse filepath
      let taliases :: Either Error LCT.Aliases
          taliases = (\(_,x,_) -> x) <$> info
          cNameInfo :: Either Error LCT.CommodityNames
          cNameInfo = (\(_,_,x) -> x) <$> info
          yTickers :: Either Error [LCT.YTicker]
          yTickers = LCT.activeYTicker <$> cNameInfo

      -- read API token
      homeDir <- getEnv "HOME"
      let configPath = homeDir </> ".config/hledger-lots/config.yaml"
      configResult <- LC.parse configPath
      let apiToken = fmap (yahooFinanceApiKey . LC.tickers) configResult

      -- get price
      priceM <- sequence $ craw <$> apiToken <*> yTickers
      let price = join priceM
      let pdirective = fmap <$> (strPriceDirtive <$> taliases <*> cNameInfo) <*> price
      case pdirective of
        Left e -> print e
        Right ts -> mapM_ putStrLn $ fmap T.unpack ts



     -- TODO
     -- handle amountStyle
     -- case M.lookup "ibkr" $ jcommodities j of
     --    Just c -> putStrLn $ showAmount $ Amount "ibkr" 1234 (fromMaybe amountstyle $ cformat  c) Nothing
     --    Nothing -> fail "ibkr not found"
