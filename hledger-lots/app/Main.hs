{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hledger.Cli.Script hiding (Group)
import Lib

cmdmode :: Mode RawOpts
cmdmode = hledgerCommandMode (unlines
    -- Command name, then --help text, then _FLAGS; empty help lines get stripped:
  ["script-example"
  ,"This is an example of a (hledger-lib-using) hledger script."
  ,"Usage: hledger-script-example [OPTS] [ARGS]"
  ,"or:    hledger script-example -- [OPTS] [ARGS]"
  ,"Save it under another name and customise it."
  ,"The hledger- name makes it appear in hledger's commands list."
  ,"Examples:"
  ,"$ hledger-script-example --help"
  ,"(this help)"
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
      rpt =  Lib.postingsReport' rspec j
      render | fmt=="txt"  = Lib.postingsReportAsText' opts
             | fmt=="json" = toJsonText
             | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        where fmt = outputFormatFromOpts opts
    writeOutputLazyText opts $ render $ styleAmounts styles rpt
