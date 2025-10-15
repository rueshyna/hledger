{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hledger.Ticker.MarketPrices where

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.KeyMap as J
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Map.Strict as MA
import           GHC.Generics (Generic)
import           Data.Time as TI
import qualified Data.Time.Clock.POSIX as D
import           Data.Scientific as S

import qualified Hledger.Ticker.CommoditiesTags as L
import qualified Hledger.Ticker.Config as C
import Hledger.Ticker.Error

newtype Timestamp = Timestamp TI.UTCTime deriving (Generic, Show, Eq)

instance J.FromJSON Timestamp where
  parseJSON
    i = Timestamp . D.posixSecondsToUTCTime . fromInteger <$> J.parseJSON i

data TickerInfo = TickerInfo
  { tksymbol :: L.YTicker
  , tkmarkettime :: Timestamp
  , tkprice :: Scientific
  , tkunit :: T.Text
  }
  deriving (Show, Generic, Eq)

instance J.FromJSON TickerInfo where
  parseJSON j@(J.Object v) = do
    symbol <- v J..:? "symbol"
    time   <- v J..:? "regularMarketTime"
    price  <- v J..:? "regularMarketPrice"
    currency <- v J..:? "currency"
    let ti = TickerInfo <$> (L.YT <$> symbol) <*> time <*> price <*> currency
    case ti of
      Just t -> return t
      Nothing -> J.prependFailure "Decode TickerInfo fail" $ J.typeMismatch "TickerInfo" j

  parseJSON j = J.prependFailure "Not expected returned body" $ J.typeMismatch "TickerInfo" j

formatUTCTime :: Timestamp -> String
formatUTCTime (Timestamp t) = formatTime defaultTimeLocale "%Y-%m-%d" t

data YPriceDirective = PD
  { ypdname :: T.Text
  , ypdtime :: Timestamp
  , ypdunit :: T.Text
  , ypdprice :: Scientific
  }

toPriceDirective :: L.Aliases -> L.CommodityNames -> TickerInfo -> (L.CommodityNames, YPriceDirective)
toPriceDirective (L.Aliases as) (L.CN cn) t =
  ( L.CN $ MA.delete yt cn
  , PD cname (tkmarkettime t) unit (tkprice t))
      where
        yt =  tksymbol t
        toSymbol (L.YT y) = (L.S y)
        (L.S cname) = MA.findWithDefault (toSymbol yt) yt cn
        (L.S unit) = MA.findWithDefault (L.S $ tkunit t) (L.A $ tkunit t) as

strPriceDirective :: Bool -> YPriceDirective -> T.Text
strPriceDirective isCsv pd =
  prefix <>
  T.pack (formatUTCTime (ypdtime pd)) <> delim <>
  (ypdname pd) <> delim <>
  (ypdunit pd) <> splitUnit <> T.pack (show $ ypdprice pd)
      where prefix = if isCsv then "" else "P "
            delim = if isCsv then "," else " "
            splitUnit = if isCsv then "," else ""

data TickerInfoResponse =
  TickerInfoResponse { tickersInfo :: V.Vector TickerInfo, errorMsg :: Maybe T.Text }
  deriving (Generic, Eq, Show)

instance J.FromJSON TickerInfoResponse where
  parseJSON = J.withObject "topQuoteResponse"
    $ \v -> case J.lookup "quoteResponse" v of
      (Just h) -> J.withObject
        "mapQuoteResponse"
        (\o -> TickerInfoResponse <$> o J..: "result" <*> o J..: "error")
        h
      Nothing  -> J.prependFailure
        "quoteRespose is not there, "
        (J.unexpected $ J.Object v)

type YTickers = [L.YTicker]

craw :: C.ApiKey -> YTickers -> IO (Either Error [TickerInfo])
craw (C.AK key) ss = do
  man <- newManager tlsManagerSettings
  let toText (L.YT t) = t
  let qs = T.intercalate "%2C" $ map toText ss
  initReq <- parseRequest
    $ "https://yfapi.net/v6/finance/quote?region=US&lang=en&symbols="
    <> T.unpack qs
  let req = initReq { requestHeaders = [ (hAccept, "application/json")
                                       , (CI.mk "X-API-KEY", TE.encodeUtf8 key)]
                    }
  response <- httpLbs req man
  let body = responseBody response
  let quote = J.eitherDecode body :: (Either String TickerInfoResponse)
  case quote of
    Right (TickerInfoResponse q e) -> case e of
      Just m  -> return $ Left $ YahooFinanceError m
      Nothing -> return $ Right $ V.toList q
    Left e -> return $ Left $ DecodeResponseError e
