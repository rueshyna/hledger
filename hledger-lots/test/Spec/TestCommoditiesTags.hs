{-# LANGUAGE OverloadedStrings #-}

module Spec.TestCommoditiesTags where

import Test.Hspec
import Lib.CommoditiesTags
import qualified Data.Text as T
import Text.Parsec as P

testCommoditiesTags :: Spec
testCommoditiesTags =
  describe "CommodityTagsParser" $ do
    it "parses a valid commodity line with active" $ do
      let input = "commodity tsmc 1000.0000 ; yahoo_ticker:TSMC, status:active, alias:2330 \n"
      let expected = [CommodityTags (S "tsmc") (YT "TSMC") True (Just (A "2330"))]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "parses a valid commodity line with active" $ do
      let input = "commodity sunplus_inn 1000.0000 ; yahoo_ticker:TSMC, status:active, alias:2330 \n"
      let expected = [CommodityTags (S "sunplus_inn") (YT "TSMC") True (Just (A "2330"))]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "parses a commodity line with status inactive" $ do
      let input = "commodity dell 1000.0000 ; yahoo_ticker:DELL, status:inactive\n"
      let expected = [CommodityTags (S "dell") (YT "DELL") False Nothing]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "parses a commodity line without status as active by default" $ do
      let input = "commodity dell 1000.0000 ; yahoo_ticker:DELL\n"
      let expected = [CommodityTags (S "dell") (YT "DELL") True Nothing]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "parses a commodity in quotes" $ do
      let input = "commodity \"2330 tw\" ; yahoo_ticker:2330.TW\n"
      let expected = [CommodityTags (S "2330 tw") (YT "2330.TW") True Nothing]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "parses commodity lines" $ do
      let input = "commodity dell 1000.0000 ; yahoo_ticker:DELL\ncommodity nvda 1000.0000 ; yahoo_ticker:NVDA"
      let expected = [CommodityTags (S "dell") (YT "DELL") True Nothing, CommodityTags (S "nvda") (YT "NVDA") True Nothing]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "ignores lines not starting with commodity 1" $ do
      let input = "abc \n; this is a comment\ncommodity dell 1000.0000 ; yahoo_ticker:DELL, status:active\n"
      let expected = [CommodityTags (S "dell") (YT "DELL") True Nothing]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "ignores lines not starting with commodity 2" $ do
      let input = "  ; this is a comment\ncommodity dell 1000.0000 ; yahoo_ticker:DELL, status:active\n"
      let expected = [CommodityTags (S "dell") (YT "DELL") True Nothing]
      case P.parse commoditiesTagsParser "" (T.pack input) of
        Left err -> expectationFailure (show err)
        Right result -> result `shouldBe` expected

    it "throws an error if yahoo_ticker is missing" $ do
       let input = "commodity dell 1000.0000 ; status:active\n"
       case P.parse commoditiesTagsParser "" (T.pack input) of
         Left _ -> return ()
         Right _ -> expectationFailure "Expected parse to fail due to missing yahoo_ticker"

