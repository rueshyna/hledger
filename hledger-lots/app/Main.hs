{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import Hledger.Read.CsvUtils (printCSV)
import Lib.Porting
import qualified Data.Text.Lazy as TL

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
  [ outputFormatFlag ["txt","csv","tsv","json" ]
  ] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing


output :: IO (CliOpts, TL.Text)
output = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournalDo opts $ \j -> do
    let
      styles = journalCommodityStylesWith HardRounding j
      rpt = postingsReport' rspec j
      render | fmt=="txt"  = postingsReportAsText' opts
             | fmt=="csv"  = printCSV . postingsReportAsCsv'
             | fmt=="json" = toJsonText
             | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        where fmt = outputFormatFromOpts opts
    return $ (opts, render $ styleAmounts styles rpt)

main :: IO ()
main = do
  (opts, content) <- output
  writeOutputLazyText opts content
