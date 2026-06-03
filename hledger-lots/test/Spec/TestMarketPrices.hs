{-# LANGUAGE OverloadedStrings #-}

module Spec.TestMarketPrices where

import Test.Hspec
import Hledger.Ticker.MarketPrices

import qualified Data.Time.Clock.POSIX as D
import Data.Time (UTCTime(..), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Aeson (decode, eitherDecode, encode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Map as MA
import qualified Data.Set as SE
import qualified Data.Text as T
import qualified Hledger.Ticker.CommoditiesTags as L

testMarketPrices :: Spec
testMarketPrices =
  describe "toPriceDirective" $ do
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
      let (_, directive) = toPriceDirective as cn ticker
      strPriceDirective False directive `shouldBe` "P 2024-08-02 Gold oz1234.56"

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

testQuoteCache :: Spec
testQuoteCache =
  describe "Yahoo quote cache" $ do
    it "uses fresh cached quotes and reports missing tickers" $ do
      let now = UTCTime (fromGregorian 2026 6 3) (secondsToDiffTime 0)
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          nvdaInfo = TickerInfo (L.YT "NVDA") marketTime 123.45 "USD"
          cache = mergeTickerInfos now (emptyQuoteCache now) [nvdaInfo]
      splitCachedTickers (addUTCTime 60 now) False cache [L.YT "NVDA", L.YT "AMD"]
        `shouldBe` ([nvdaInfo], [L.YT "AMD"])

    it "refreshes expired cached quotes" $ do
      let now = UTCTime (fromGregorian 2026 6 3) (secondsToDiffTime 0)
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          nvdaInfo = TickerInfo (L.YT "NVDA") marketTime 123.45 "USD"
          cache = mergeTickerInfos now (emptyQuoteCache now) [nvdaInfo]
      splitCachedTickers (addUTCTime (13 * 60 * 60) now) False cache [L.YT "NVDA"]
        `shouldBe` ([], [L.YT "NVDA"])

    it "ignores cached quotes when refresh is requested" $ do
      let now = UTCTime (fromGregorian 2026 6 3) (secondsToDiffTime 0)
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          nvdaInfo = TickerInfo (L.YT "NVDA") marketTime 123.45 "USD"
          cache = mergeTickerInfos now (emptyQuoteCache now) [nvdaInfo]
      splitCachedTickers (addUTCTime 60 now) True cache [L.YT "NVDA"]
        `shouldBe` ([], [L.YT "NVDA"])

    it "updates the cache timestamp when merging fetched quotes" $ do
      let oldTime = UTCTime (fromGregorian 2026 6 3) (secondsToDiffTime 0)
          newTime = addUTCTime 3600 oldTime
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          nvdaInfo = TickerInfo (L.YT "NVDA") marketTime 123.45 "USD"
          cache = mergeTickerInfos newTime (emptyQuoteCache oldTime) [nvdaInfo]
      qcFetchedAt cache `shouldBe` newTime

    it "round-trips the cache as JSON" $ do
      let now = UTCTime (fromGregorian 2026 6 3) (secondsToDiffTime 0)
          marketTime = Timestamp $ D.posixSecondsToUTCTime 1722576602
          nvdaInfo = TickerInfo (L.YT "NVDA") marketTime 123.45 "USD"
          cache = mergeTickerInfos now (emptyQuoteCache now) [nvdaInfo]
      (decode $ encode cache :: Maybe QuoteCache) `shouldBe` Just cache

testPriceDedup :: Spec
testPriceDedup =
  describe "journal price dedup" $ do
    it "parses unquoted journal price directive keys" $ do
      parsePriceDirectiveKey "P 2026-06-03 AMD USD123.45"
        `shouldBe` Just (PriceKey "2026-06-03" "AMD")

    it "parses quoted journal price directive keys" $ do
      parsePriceDirectiveKey "P 2026-06-03 \"2330\" TWD903.0"
        `shouldBe` Just (PriceKey "2026-06-03" "2330")

    it "skips directives that already exist by date and commodity" $ do
      let directives =
            [ priceDirective "2026-06-03" "AMD" 123.45
            , priceDirective "2026-06-04" "AMD" 124.45
            , priceDirective "2026-06-03" "NVDA" 987.65
            ]
          existing = SE.singleton $ PriceKey "2026-06-03" "AMD"
      strPriceDirective False <$> dedupPriceDirectives existing directives
        `shouldBe`
          [ "P 2026-06-04 AMD USD124.45"
          , "P 2026-06-03 NVDA USD987.65"
          ]

    it "keeps only the first directive with the same date and commodity in one run" $ do
      let directives =
            [ priceDirective "2026-06-03" "AMD" 123.45
            , priceDirective "2026-06-03" "AMD" 124.45
            ]
      strPriceDirective False <$> dedupPriceDirectives SE.empty directives
        `shouldBe` ["P 2026-06-03 AMD USD123.45"]

    it "prepends an update header when price lines are present" $ do
      priceUpdateHeaderLines (fromGregorian 2026 6 3) ["P 2026-06-03 AMD USD123.45"]
        `shouldBe` ["", "; updated 2026-06-03", "P 2026-06-03 AMD USD123.45"]

    it "does not add an update header when there are no price lines" $ do
      priceUpdateHeaderLines (fromGregorian 2026 6 3) [] `shouldBe` []
  where
    priceDirective :: T.Text -> T.Text -> Rational -> YPriceDirective
    priceDirective dateText name price =
      PD
        { ypdname = name
        , ypdtime = Timestamp $ D.posixSecondsToUTCTime $ fromInteger $ dateToPosix dateText
        , ypdunit = "USD"
        , ypdprice = fromRational price
        }

    dateToPosix :: T.Text -> Integer
    dateToPosix "2026-06-03" = 1780444800
    dateToPosix "2026-06-04" = 1780531200
    dateToPosix _ = 0
