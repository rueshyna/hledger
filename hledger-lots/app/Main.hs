{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)
import Lib.Porting

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
  [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing


main :: IO ()
main = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournalDo opts $ \j -> do
    let
      styles = journalCommodityStylesWith HardRounding j
      rpt =  postingsReport' rspec j
      render | fmt=="txt"  = postingsReportAsText' opts
             | fmt=="json" = toJsonText
             | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        where fmt = outputFormatFromOpts opts
    writeOutputLazyText opts $ render $ styleAmounts styles rpt
