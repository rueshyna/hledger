{-# LANGUAGE OverloadedStrings #-}

module Spec.TestCommoditiesTags where

import Test.Hspec
import Hledger.Ticker.CommoditiesTags
import Hledger.Ticker.Report (commodityNamesFromHeldSymbols)
import qualified Data.Map.Strict as MA
import qualified Data.Text as T
import Text.Parsec as P

parseOne :: T.Text -> Either ParseError GroupHierarchyOrCommodityTags
parseOne = P.parse commodityTagsParser ""

parseManyTags :: T.Text -> Either ParseError [CommodityTags]
parseManyTags input =
  fmap onlyTags $ P.parse groupHierarchyOrCommodityTagsParser "" input
  where
    onlyTags [] = []
    onlyTags (CT tags:xs) = tags : onlyTags xs
    onlyTags (_:xs) = onlyTags xs

testCommoditiesTags :: Spec
testCommoditiesTags =
  describe "CommodityTagsParser" $ do
    it "parses a valid commodity line with active" $ do
      let input = "commodity tsmc 1000.0000 ; yahoo_ticker:TSMC, status:active, alias:2330 \n"
      let expected = CT $ CommodityTags (S "tsmc") (YT "TSMC") True (Just $ A "2330") Nothing []
      parseOne (T.pack input) `shouldBe` Right expected

    it "parses a valid commodity line with underscore symbol" $ do
      let input = "commodity sunplus_inn 1000.0000 ; yahoo_ticker:TSMC, status:active, alias:2330 \n"
      let expected = CT $ CommodityTags (S "sunplus_inn") (YT "TSMC") True (Just $ A "2330") Nothing []
      parseOne (T.pack input) `shouldBe` Right expected

    it "parses a commodity line with status inactive" $ do
      let input = "commodity dell 1000.0000 ; yahoo_ticker:DELL, status:inactive\n"
      let expected = CT $ CommodityTags (S "dell") (YT "DELL") False Nothing Nothing []
      parseOne (T.pack input) `shouldBe` Right expected

    it "parses a commodity line without status as active by default" $ do
      let input = "commodity dell 1000.0000 ; yahoo_ticker:DELL\n"
      let expected = CT $ CommodityTags (S "dell") (YT "DELL") True Nothing Nothing []
      parseOne (T.pack input) `shouldBe` Right expected

    it "parses a commodity in quotes" $ do
      let input = "commodity \"2330 tw\" ; yahoo_ticker:2330.TW\n"
      let expected = CT $ CommodityTags (S "2330 tw") (YT "2330.TW") True Nothing Nothing []
      parseOne (T.pack input) `shouldBe` Right expected

    it "parses commodity lines" $ do
      let input = "commodity dell 1000.0000 ; yahoo_ticker:DELL\ncommodity nvda 1000.0000 ; yahoo_ticker:NVDA\n"
      let expected =
            [ CommodityTags (S "dell") (YT "DELL") True Nothing Nothing []
            , CommodityTags (S "nvda") (YT "NVDA") True Nothing Nothing []
            ]
      parseManyTags (T.pack input) `shouldBe` Right expected

    it "ignores lines not starting with commodity 1" $ do
      let input = "abc \n; this is a comment\ncommodity dell 1000.0000 ; yahoo_ticker:DELL, status:active\n"
      let expected = [CommodityTags (S "dell") (YT "DELL") True Nothing Nothing []]
      parseManyTags (T.pack input) `shouldBe` Right expected

    it "ignores lines not starting with commodity 2" $ do
      let input = "  ; this is a comment\ncommodity dell 1000.0000 ; yahoo_ticker:DELL, status:active\n"
      let expected = [CommodityTags (S "dell") (YT "DELL") True Nothing Nothing []]
      parseManyTags (T.pack input) `shouldBe` Right expected

    it "throws an error if yahoo_ticker is missing" $ do
      let input = "commodity dell 1000.0000 ; status:active\n"
      case parseOne (T.pack input) of
        Left _ -> return ()
        Right result -> expectationFailure $ "Expected parse to fail, got " <> show result

    it "builds ticker names from held active commodities only" $ do
      let info =
            CS MA.empty
              [ CommodityTags (S "dell") (YT "DELL") True Nothing Nothing []
              , CommodityTags (S "nvda") (YT "NVDA") True Nothing Nothing []
              , CommodityTags (S "old") (YT "OLD") False Nothing Nothing []
              , CommodityTags (S "cash") (YT "CASH") True Nothing Nothing []
              ]
      commodityNamesFromHeldSymbols [S "dell", S "nvda", S "old", S "missing"] info
        `shouldBe` CN (MA.fromList [(YT "DELL", S "dell"), (YT "NVDA", S "nvda")])
