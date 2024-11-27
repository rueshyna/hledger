{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hledger.Ticker.Report where

import Hledger.Cli.Script hiding (Group)
import qualified Hledger.Ticker.CommoditiesTags as LCT
import Hledger.Ticker.MarketPrices as LM
import Hledger.Ticker.Config as LC
import Hledger.Ticker.Error

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

commodityInfoFlag :: Flag RawOpts
commodityInfoFlag = flagNone
  ["display-commodity-info","d"] (setboolopt "display-commodity-info") "Display commodity info"

isDisplayCommodityInfo :: CliOpts -> Bool
isDisplayCommodityInfo = boolopt "display-commodity-info" . rawopts_

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
  [ commodityInfoFlag ]
  [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing

run :: CliOpts -> IO ()
run opts = do
    withJournalDo opts $ \j -> do
      -- parse to get tags, alias table, commodities' name table
      let filepath = head $ jincludefilestack j
      info <- LCT.parse filepath
      let tAliases :: Either Error LCT.Aliases
          tAliases = (\(_,x,_) -> x) <$> info
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
      let price :: Either Error [TickerInfo]
          price = join priceM

      let pdirectiveWithAlias :: Either Error (LCT.CommodityNames -> LM.TickerInfo -> (LCT.CommodityNames, T.Text))
          pdirectiveWithAlias = strPriceDirtive <$> tAliases

      let aux :: [Either Error (LCT.CommodityNames, T.Text)] -> TickerInfo -> [Either Error (LCT.CommodityNames, T.Text)]
          aux [] a = (pdirectiveWithAlias <*> cNameInfo <*> pure a):[(fmap (,"") cNameInfo)]
          aux acc@(x:_) a = (pdirectiveWithAlias <*> (fst <$> x) <*> pure a):acc

      let pdirective = foldl' aux [] <$> price
      case pdirective of
        Left e -> print e
        Right ts -> do
          mapM_ putStrLn $ (T.unpack . either (T.pack . show) snd) <$> ts
          case head ts of
            Left e -> print e
            Right e -> do
              let ss = LCT.activeSymbol $ fst e
              if null ss then
                return ()
              else do
                hPutStrLn stderr "-------" 
                hPutStrLn stderr "The following the price of commodities couldn't be found in Yahoo Finance."
                hPutStrLn stderr "Please check the quotes in .journal are correct."
                hPutStrLn stderr ""
                mapM_ (hPutStrLn stderr. toStr) ss
                  where toStr (LCT.S yt) = T.unpack yt

displayCommodityInfo :: CliOpts -> IO BL.ByteString
displayCommodityInfo opts = do
    withJournalDo opts $ \j -> do
      -- parse to get tags, alias table, commodities' name table
      let filepath = head $ jincludefilestack j
      info <- LCT.parse filepath
      let cs :: Either Error [LCT.CommodityTags]
          cs = (\(x,_,_) -> x) <$> info
      return $ case cs of
        Left e -> BLC.pack $ show e
        Right e -> A.encode e
