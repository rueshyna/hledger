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
import           GHC.Generics (Generic)
import           Data.Time as TI
import qualified Data.Time.Clock.POSIX as D
import           Data.Scientific as S

import Lib.Error

newtype Timestamp = Timestamp TI.UTCTime deriving (Generic, Show, Eq)

instance J.FromJSON Timestamp where
  parseJSON
    i = Timestamp . D.posixSecondsToUTCTime . fromInteger <$> J.parseJSON i

data Ticker = Ticker
  { tkcommodity :: T.Text
  , tkmarkettime :: Timestamp
  , tkprice :: Scientific
  , tkunit :: T.Text
  }
  deriving (Show, Generic, Eq)

instance J.FromJSON Ticker where
  parseJSON = J.withObject "Ticker"
    $ \v -> Ticker <$> v J..: "symbol"
    <*> v J..: "regularMarketTime"
    <*> v J..:? "regularMarketPrice" J..!= 0
    <*> v J..: "currency"

formatUTCTime :: Timestamp -> String
formatUTCTime (Timestamp t) = formatTime defaultTimeLocale "%Y-%m-%d" t

strPriceDirtive :: Ticker -> T.Text
strPriceDirtive t =
   "P " <>
   T.pack (formatUTCTime (tkmarkettime t)) <> " " <> 
   tkcommodity t <> " " <>
   tkunit t <> T.pack (show $ tkprice t)

data TickerResponse =
  TickerResponse { tickers :: V.Vector Ticker, errorMsg :: Maybe T.Text }
  deriving (Generic, Eq, Show)

instance J.FromJSON TickerResponse where
  parseJSON = J.withObject "topQuoteResponse"
    $ \v -> case J.lookup "quoteResponse" v of
      (Just h) -> J.withObject
        "mapQuoteResponse"
        (\o -> TickerResponse <$> o J..: "result" <*> o J..: "error")
        h
      Nothing  -> J.prependFailure
        "quoteRespose is not there, "
        (J.unexpected $ J.Object v)

type Symbols = [T.Text]
type ApiKey = T.Text

craw :: ApiKey -> Symbols -> IO (Either Error ([Ticker]))
craw key ss = do
  man <- newManager tlsManagerSettings
  let qs = T.intercalate "%2C" ss
  initReq <- parseRequest
    $ "https://yfapi.net/v6/finance/quote?region=US&lang=en&symbols="
    <> T.unpack qs
  let req = initReq { requestHeaders = [ (hAccept, "application/json")
                                       , (CI.mk "X-API-KEY", TE.encodeUtf8 key)]
                    }
  response <- httpLbs req man
  let body = responseBody response
  let quote = J.decode body :: (Maybe TickerResponse)
  case quote of
    Just (TickerResponse q e) -> case e of
      Just m  -> return $ Left $ YahooFinanceError m
      Nothing -> return $ Right $ V.toList q
    Nothing -> return $ Left $ DecodeResponseError body
