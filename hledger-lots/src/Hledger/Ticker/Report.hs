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
import Data.Foldable (foldlM)

commodityInfoFlag :: Flag RawOpts
commodityInfoFlag = flagNone
  ["display-commodity-info","d"] (setboolopt "display-commodity-info") "Display commodity info. (Only Support JSON)"

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
  [ commodityInfoFlag, outputFormatFlag ["txt","csv"] ]
  [ generalflagsgroup1 ] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing

-- only support Csv
marketPriceOutputFormatCsv :: Maybe String -> Bool
marketPriceOutputFormatCsv Nothing = False
marketPriceOutputFormatCsv (Just s)
  | map toLower s == "csv" = True
  | otherwise = False

fetchMarketPrice :: CliOpts -> IO ()
fetchMarketPrice opts = do
    withJournalDo opts $ \j -> do
      -- parse to get tags, alias table, commodities' name table
      let filepath = head $ jincludefilestack j
          isCsvFormat = marketPriceOutputFormatCsv $ output_format_ opts
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
          pdirectiveWithAlias = strPriceDirtive isCsvFormat <$> tAliases

      let aux :: [(LCT.CommodityNames, T.Text)] -> TickerInfo -> Either Error [(LCT.CommodityNames, T.Text)]
          aux [] a = fmap (:[]) (pdirectiveWithAlias <*> cNameInfo <*> pure a)
          aux acc@(x:_) a = (:) <$> (pdirectiveWithAlias <*> (pure $ fst x) <*> pure a) <*> pure acc

      let pdirective = foldlM aux [] =<< price

      case pdirective of
        Left e -> hPrint stderr e
        Right [] -> putStrLn ""
        Right ts@(t:_) -> do
          mapM_ putStrLn $ (T.unpack . snd) <$> ts
          let ss = LCT.activeSymbol $ fst t
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

-- TODO: support market price
run :: IO BL.ByteString
run = do
    opts <- getHledgerCliOpts cmdmode
    if isDisplayCommodityInfo opts
    then displayCommodityInfo opts
    else return "Not Implement"

