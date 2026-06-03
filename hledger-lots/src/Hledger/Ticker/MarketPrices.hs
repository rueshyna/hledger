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
import qualified Data.ByteString.Lazy as LB
import           GHC.Generics (Generic)
import           Data.Time as TI
import qualified Data.Time.Clock.POSIX as D
import           Data.Scientific as S
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)
import           Control.Exception (SomeException, try)

import qualified Hledger.Ticker.CommoditiesTags as L
import qualified Hledger.Ticker.Config as C
import Hledger.Ticker.Error
import Data.Char (isDigit)

newtype Timestamp = Timestamp TI.UTCTime deriving (Generic, Show, Eq)

instance J.FromJSON Timestamp where
  parseJSON
    i = Timestamp . D.posixSecondsToUTCTime . fromInteger <$> J.parseJSON i

instance J.ToJSON Timestamp where
  toJSON (Timestamp t) =
    J.toJSON (floor (D.utcTimeToPOSIXSeconds t) :: Integer)

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

data CachedQuote = CachedQuote
  { cqSymbol :: L.YTicker
  , cqMarketTime :: Timestamp
  , cqPrice :: Scientific
  , cqUnit :: T.Text
  , cqFetchedAt :: TI.UTCTime
  }
  deriving (Show, Generic, Eq)

instance J.FromJSON CachedQuote
instance J.ToJSON CachedQuote

type QuoteCache = MA.Map L.YTicker CachedQuote

quoteCacheTtl :: TI.NominalDiffTime
quoteCacheTtl = 12 * 60 * 60

tickerToCache :: TI.UTCTime -> TickerInfo -> CachedQuote
tickerToCache fetchedAt ti =
  CachedQuote
    { cqSymbol = tksymbol ti
    , cqMarketTime = tkmarkettime ti
    , cqPrice = tkprice ti
    , cqUnit = tkunit ti
    , cqFetchedAt = fetchedAt
    }

cachedToTicker :: CachedQuote -> TickerInfo
cachedToTicker cq =
  TickerInfo
    { tksymbol = cqSymbol cq
    , tkmarkettime = cqMarketTime cq
    , tkprice = cqPrice cq
    , tkunit = cqUnit cq
    }

isFreshCachedQuote :: TI.UTCTime -> CachedQuote -> Bool
isFreshCachedQuote now cq =
  TI.diffUTCTime now (cqFetchedAt cq) < quoteCacheTtl

splitCachedTickers :: TI.UTCTime -> Bool -> QuoteCache -> YTickers -> ([TickerInfo], YTickers)
splitCachedTickers now refresh cache tickers
  | refresh = ([], tickers)
  | otherwise = foldr splitTicker ([], []) tickers
  where
    splitTicker ticker (cached, missing) =
      case MA.lookup ticker cache of
        Just cq | isFreshCachedQuote now cq ->
          (cachedToTicker cq : cached, missing)
        _ -> (cached, ticker : missing)

mergeTickerInfos :: TI.UTCTime -> QuoteCache -> [TickerInfo] -> QuoteCache
mergeTickerInfos now =
  foldr insertTicker
  where
    insertTicker ti = MA.insert (tksymbol ti) (tickerToCache now ti)

quoteCachePath :: IO FilePath
quoteCachePath = do
  home <- getHomeDirectory
  return $ home </> ".cache" </> "hledger-lots" </> "yahoo-quotes.json"

readQuoteCache :: IO QuoteCache
readQuoteCache = do
  cachePath <- quoteCachePath
  exists <- doesFileExist cachePath
  if not exists
    then return MA.empty
    else do
      content <- try (LB.readFile cachePath) :: IO (Either SomeException LB.ByteString)
      return $ case content of
        Left _ -> MA.empty
        Right bytes -> case J.eitherDecode bytes of
          Left _ -> MA.empty
          Right cache -> cache

writeQuoteCache :: QuoteCache -> IO ()
writeQuoteCache cache = do
  cachePath <- quoteCachePath
  _ <- try
    ( do
        createDirectoryIfMissing True (takeDirectory cachePath)
        LB.writeFile cachePath (J.encode cache)
    ) :: IO (Either SomeException ())
  return ()

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
  quotedName <> delim <>
  (ypdunit pd) <> splitUnit <> T.pack (show $ ypdprice pd)
      where prefix = if isCsv then "" else "P "
            delim = if isCsv then "," else " "
            splitUnit = if isCsv then "," else ""
            nameRaw = ypdname pd
            quotedName =
              if T.any isDigit nameRaw
              then "\"" <> nameRaw <> "\""
              else nameRaw

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

crawCached :: Bool -> C.ApiKey -> YTickers -> IO (Either Error [TickerInfo])
crawCached refresh apiKey tickers = do
  now <- TI.getCurrentTime
  cache <- readQuoteCache
  let (cached, missing) = splitCachedTickers now refresh cache tickers
  case missing of
    [] -> return $ Right cached
    _ -> do
      fetched <- craw apiKey missing
      case fetched of
        Left e -> return $ Left e
        Right infos -> do
          writeQuoteCache $ mergeTickerInfos now cache infos
          return $ Right (cached <> infos)

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
