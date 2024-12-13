{-# LANGUAGE OverloadedStrings #-}

module Hledger.Lots.Report where

import qualified Data.Text.Lazy as TL
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

import Hledger.Read.CsvUtils (printCSV)
import Hledger.Cli.Script hiding (Group)

import Hledger.Lots.Porting

balAvgCostFlag :: Flag RawOpts
balAvgCostFlag = flagNone
  ["display-avg-cost","d"] (setboolopt "display-avg-cost") "Display average cost of balance."

isDisplaybalAvgCost :: CliOpts -> Bool
isDisplaybalAvgCost = boolopt "display-avg-cost" . rawopts_

cmdmode :: Mode RawOpts
cmdmode = hledgerCommandMode (unlines
    -- Command name, then --help text, then _FLAGS; empty help lines get stripped:
  ["hledger-lots"
  ,"Usage: hledger-lots [OPTS] [ARGS]"
  ,"or:    hledger lots -- [OPTS] [ARGS]"
    ------------------------------------78----------------------------------------
  ,""
  ,"_FLAGS"
  ])
  [ outputFormatFlag ["txt","csv","json" ]
  , balAvgCostFlag
  ] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing

run :: IO (CliOpts, TL.Text)
run = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode

  if isDisplaybalAvgCost opts then
    withJournalDo opts $ \j -> do
      let
        styles = journalCommodityStylesWith SoftRounding j
        lastRptItem = safeLast $ postingsReport' rspec j
        amts :: Maybe [Amount]
        amts = amounts . fifth <$> lastRptItem 
        unitPrices :: Maybe [Maybe (CommoditySymbol, Amount)]
        unitPrices  = fmap (fmap findUnitPrice) amts
        render = maybe "" (printUnitPrices styles) unitPrices
      return $ (opts, render)
  else
    withJournalDo opts $ \j -> do
      let
        styles = journalCommodityStylesWith SoftRounding j
        rpt = postingsReport' rspec j
        render | fmt=="txt"  = postingsReportAsText' opts
               | fmt=="csv"  = printCSV . postingsReportAsCsv'
               | fmt=="json" = toJsonText
               | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
          where fmt = outputFormatFromOpts opts
      return $ (opts, render $ styleAmounts styles rpt)

printUnitPrices :: M.Map CommoditySymbol AmountStyle -> [Maybe (CommoditySymbol, Amount)] -> TL.Text
printUnitPrices styles =
  F.foldr' aux ""
    where aux :: Maybe (CommoditySymbol, Amount) -> TL.Text -> TL.Text
          aux ca acc =
            if TL.null item then
              acc
            else
              item <> "\n" <> acc
            where item = maybe "" showItems ca
          showItems :: (CommoditySymbol, Amount) -> TL.Text
          showItems (c, a) = TL.fromStrict c <> " " <> TL.pack (showAmount $ styleAmounts styles a)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

fifth :: (a, b, c, d, e) -> e
fifth (_, _, _, _, x) = x

findUnitPrice :: Amount -> Maybe (CommoditySymbol, Amount)
findUnitPrice amount = findUnitPrice' (acommodity amount) amount
  where findUnitPrice' :: CommoditySymbol -> Amount -> Maybe (CommoditySymbol, Amount)
        findUnitPrice' c a =
          case aprice a of
            Just (UnitPrice unitAmount) -> Just (c, unitAmount)
            Just (TotalPrice totalAmount) -> findUnitPrice' c totalAmount
            Nothing -> Nothing
