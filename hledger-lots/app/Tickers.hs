{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import qualified Lib.CommoditiesTags as LCT
import Lib.MarketPrices as LM
import Lib.Config as LC
import Lib.Error


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



     -- TODO
     -- handle amountStyle
     -- case M.lookup "ibkr" $ jcommodities j of
     --    Just c -> putStrLn $ showAmount $ Amount "ibkr" 1234 (fromMaybe amountstyle $ cformat  c) Nothing
     --    Nothing -> fail "ibkr not found"
