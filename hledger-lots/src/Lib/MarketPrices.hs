{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.MarketPrices where

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

import qualified Lib.CommoditiesTags as L
import qualified Lib.Config as C
import Lib.Error

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
  parseJSON = J.withObject "TickerInfo"
    $ \v -> TickerInfo <$> (L.YT <$> v J..: "symbol")
    <*> v J..: "regularMarketTime"
    <*> v J..:? "regularMarketPrice" J..!= 0
    <*> v J..: "currency"

formatUTCTime :: Timestamp -> String
formatUTCTime (Timestamp t) = formatTime defaultTimeLocale "%Y-%m-%d" t

strPriceDirtive :: L.Aliases -> L.CommodityNames -> TickerInfo -> T.Text
strPriceDirtive (L.Aliases as) (L.CN cn) t =
   "P " <>
   T.pack (formatUTCTime (tkmarkettime t)) <> " " <> 
   cname <> " " <>
   unit <> T.pack (show $ tkprice t)
     where yt =  tksymbol t
           toSymbol (L.YT y) = (L.S y)
           (L.S cname) = MA.findWithDefault (toSymbol yt) yt cn
           (L.S unit) = MA.findWithDefault (L.S $ tkunit t) (L.A $ tkunit t) as

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
  let quote = J.decode body :: (Maybe TickerInfoResponse)
  case quote of
    Just (TickerInfoResponse q e) -> case e of
      Just m  -> return $ Left $ YahooFinanceError m
      Nothing -> return $ Right $ V.toList q
    Nothing -> return $ Left $ DecodeResponseError body
