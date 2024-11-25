{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Hledger.Cli.Script hiding (Group)

import Hledger.Lots.Report

main :: IO ()
main = do
  (opts, content) <- run
  writeOutputLazyText opts content
