module Lib.Error where

import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.ByteString.Lazy as LB
import qualified Data.Yaml as Y

data Error = DecodeResponseError LB.ByteString
           | YahooFinanceError T.Text
           | ParseCommoditiesTagsError P.ParseError
           | ParseConfigError Y.ParseException
  deriving Show
