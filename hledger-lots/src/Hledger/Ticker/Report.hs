{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Hledger.Ticker.Report where

import Hledger.Cli.Script hiding (Group)
import qualified Hledger.Ticker.CommoditiesTags as LCT
import Hledger.Ticker.MarketPrices as LM
import Hledger.Ticker.Config as LC
import Hledger.Ticker.Error

import qualified Data.Aeson as A
import qualified Data.Text as T
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

fetchMarketPrice
  :: Maybe FilePath
  -> CliOpts
  -> IO (Either Error [(LCT.CommodityNames, YPriceDirective)])
fetchMarketPrice out opts = do
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

      let pdirectiveWithAlias :: Either Error (LCT.CommodityNames -> LM.TickerInfo -> (LCT.CommodityNames, YPriceDirective))
          pdirectiveWithAlias = toPriceDirective <$> tAliases

      let aux :: [(LCT.CommodityNames, YPriceDirective)] -> TickerInfo -> Either Error [(LCT.CommodityNames, YPriceDirective)]
          aux [] a = fmap (:[]) (pdirectiveWithAlias <*> cNameInfo <*> pure a)
          aux acc@(x:_) a = (:) <$> (pdirectiveWithAlias <*> (pure $ fst x) <*> pure a) <*> pure acc

      let res = foldlM aux [] =<< price
      ppMarketPrice out opts res
      return res

ppMarketPrice
  :: Maybe FilePath
  -> CliOpts
  -> Either Error [(LCT.CommodityNames, YPriceDirective)]
  -> IO ()
ppMarketPrice out opts pdirective = do
  let isCsvFormat = marketPriceOutputFormatCsv $ output_format_ opts
  handleOut <- maybe (return stdout) (flip openFile AppendMode) out
  case pdirective of
    Left e -> hPrint stderr e
    Right [] -> hPutStrLn handleOut ""
    Right ts@(t:_) -> do
      let ss = LCT.activeSymbol $ fst t
      if null ss then
        mapM_ (hPutStrLn handleOut) $ (T.unpack . strPriceDirective isCsvFormat . snd) <$> ts
      else do
        hPutStrLn stderr "-------"
        hPutStrLn stderr "The following the price of commodities couldn't be found in Yahoo Finance."
        hPutStrLn stderr "Please check the quotes in .journal are correct."
        hPutStrLn stderr ""
        mapM_ (hPutStrLn stderr. toStr) ss
          where toStr (LCT.S yt) = T.unpack yt

displayCommodityInfo :: CliOpts -> IO (Either Error LCT.CommodityInfo)
displayCommodityInfo opts = do
    withJournalDo opts $ \j -> do
      -- parse to get tags, alias table, commodities' name table
      let filepath = head $ jincludefilestack j
      info <- LCT.parse filepath
      return $ (\(x,_,_) -> x) <$> info

ppDisplayCommodityInfo :: Either Error LCT.CommodityInfo -> IO ()
ppDisplayCommodityInfo (Left e) = hPrint stderr e
ppDisplayCommodityInfo (Right e) = BLC.putStrLn $ A.encode e

runWithArg :: (CliOpts -> IO a) -> IO a
runWithArg f = do
    opts <- getHledgerCliOpts cmdmode
    f opts

