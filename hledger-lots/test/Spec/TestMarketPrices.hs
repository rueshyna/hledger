{-# LANGUAGE OverloadedStrings #-}

module Spec.TestMarketPrices where

import Test.Hspec
import Lib.MarketPrices

import qualified Data.Time.Clock.POSIX as D
import Data.Aeson (decode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

testMarketPrices :: Spec
testMarketPrices =
  describe "strPriceDirtive" $ do
    it "formats the Ticker correctly" $ do
      let marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
      let ticker = Ticker
            { tkcommodity = "Gold"
            , tkmarkettime = marketTime
            , tkprice = 1234.56
            , tkunit = "oz"
            }
      strPriceDirtive ticker `shouldBe` "P 2024-08-02 Gold oz1234.56"

testDecodeTicker :: Spec
testDecodeTicker =
  describe "Ticker JSON Decoding" $ do
    it "decodes a Ticker from JSON" $ do
      let jsonStr = "{ \"symbol\": \"2330.TW\", \"regularMarketTime\": 1722576602, \"regularMarketPrice\": 903.0, \"currency\": \"TWD\" }"
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          expectedTicker = Ticker { tkcommodity = "2330.TW", tkmarkettime = marketTime, tkprice = 903.0, tkunit = "TWD" }
          decodedTicker = decode (LB.fromStrict (TE.encodeUtf8 jsonStr)) :: Maybe Ticker
      decodedTicker `shouldBe` Just expectedTicker
    it "decodes a TickerResponse from JSON" $ do
      let jsonStr = "{\"quoteResponse\": {\"result\": [{\"symbol\": \"2330.TW\", \"regularMarketTime\": 1722576602, \"regularMarketPrice\": 903.0, \"currency\": \"TWD\"}], \"error\": null}}"
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          expectedTicker = Ticker { tkcommodity = "2330.TW", tkmarkettime = marketTime, tkprice = 903.0, tkunit = "TWD" }
          expectedResponse = TickerResponse { tickers = V.fromList [expectedTicker], errorMsg = Nothing }
          decodedResponse = decode (LB.fromStrict (TE.encodeUtf8 jsonStr)) :: Maybe TickerResponse
      decodedResponse `shouldBe` Just expectedResponse

