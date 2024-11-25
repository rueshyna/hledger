{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hledger.Ticker.Config where

import qualified Data.Text as T
import qualified Data.Yaml as Y
import           GHC.Generics (Generic)

import           Hledger.Ticker.Error

newtype ApiKey = AK T.Text
  deriving (Show)

newtype TickersConfig = TickersConfig
    { yahooFinanceApiKey :: ApiKey
    } deriving (Show, Generic)

newtype Config = Config
    { tickers :: TickersConfig
    } deriving (Show, Generic)

instance Y.FromJSON TickersConfig where
    parseJSON (Y.Object v) = TickersConfig
        <$> (AK <$> v Y..: "YahooFinanceApiKey")
    parseJSON _ = fail "Expected Object for TickersConfig"

instance Y.FromJSON Config

parse :: FilePath -> IO (Either Error Config)
parse configPath = do
   configResult :: Either Y.ParseException Config  <- Y.decodeFileEither configPath
   case configResult of
     Left e -> return $ Left $ ParseConfigError e
     Right r -> return $ Right r
