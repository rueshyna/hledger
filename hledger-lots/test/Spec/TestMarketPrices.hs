{-# LANGUAGE OverloadedStrings #-}

module Spec.TestMarketPrices where

import Test.Hspec
import Lib.MarketPrices

import qualified Data.Time.Clock.POSIX as D
import Data.Aeson (decode, eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Map as MA
import qualified Lib.CommoditiesTags as L

testMarketPrices :: Spec
testMarketPrices =
  describe "strPriceDirtive" $ do
    it "formats the TickerInfo correctly" $ do
      let marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
      let ticker = TickerInfo
            { tksymbol = L.YT "Gold"
            , tkmarkettime = marketTime
            , tkprice = 1234.56
            , tkunit = "oz"
            }
      let as = L.Aliases $ MA.empty
      let cn = L.CN $ MA.empty
      strPriceDirtive as cn ticker `shouldBe` "P 2024-08-02 Gold oz1234.56"

testDecodeTickerInfo :: Spec
testDecodeTickerInfo =
  describe "TickerInfo JSON Decoding" $ do
    it "decodes a TickerInfo from JSON" $ do
      let jsonStr = "{ \"symbol\": \"2330.TW\", \"regularMarketTime\": 1722576602, \"regularMarketPrice\": 903.0, \"currency\": \"TWD\" }"
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          expectedTickerInfo = TickerInfo { tksymbol = L.YT "2330.TW", tkmarkettime = marketTime, tkprice = 903.0, tkunit = "TWD" }
          decodedTickerInfo = decode (LB.fromStrict (TE.encodeUtf8 jsonStr)) :: Maybe TickerInfo
      decodedTickerInfo `shouldBe` Just expectedTickerInfo
    it "decodes a TickerInfoResponse from JSON" $ do
      let jsonStr = "{\"quoteResponse\": {\"result\": [{\"symbol\": \"2330.TW\", \"regularMarketTime\": 1722576602, \"regularMarketPrice\": 903.0, \"currency\": \"TWD\"}], \"error\": null}}"
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          expectedTickerInfo = TickerInfo { tksymbol = L.YT "2330.TW", tkmarkettime = marketTime, tkprice = 903.0, tkunit = "TWD" }
          expectedResponse = TickerInfoResponse { tickersInfo = V.fromList [expectedTickerInfo], errorMsg = Nothing }
          decodedResponse = decode (LB.fromStrict (TE.encodeUtf8 jsonStr)) :: Maybe TickerInfoResponse
      decodedResponse `shouldBe` Just expectedResponse
    it "decodes a TickerInfo from JSON fail" $ do
      let jsonStr = "{ , \"regularMarketTime\": 1722576602, \"regularMarketPrice\": 903.0, \"currency\": \"TWD\" }"
          expectedTickerInfo = Left "Error in $: Failed reading: satisfy. Expecting object key at ',regularMarketTime:1722576602,regularMarketPrice:903.0,currency:TWD}'"
          decodedTickerInfo = eitherDecode (LB.fromStrict (TE.encodeUtf8 jsonStr)) :: Either String TickerInfo
      decodedTickerInfo `shouldBe` expectedTickerInfo

