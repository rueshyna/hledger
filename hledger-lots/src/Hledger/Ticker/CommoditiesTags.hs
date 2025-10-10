{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hledger.Ticker.CommoditiesTags where

import qualified Data.Maybe as M
import qualified Data.Map as MA
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Aeson as A
import Data.Char (isAlpha)

import Text.Parsec as P
import qualified Text.Parsec.Text as PT
import qualified Text.Parsec.Char as PC
import Hledger.Ticker.Error
import GHC.Generics (Generic)

newtype Name = N T.Text
  deriving (Generic, Show, Eq, Ord)

newtype Alias = A T.Text
  deriving (Generic, Show, Eq, Ord)

newtype Symbol = S T.Text
  deriving (Generic, Show, Eq, Ord)

newtype YTicker = YT T.Text
  deriving (Generic, Show, Eq, Ord)

data CommodityTags = CommodityTags
  { ctsymbol :: Symbol
  , ctyahooTicker :: YTicker
  , ctstatus :: Bool
  , ctalias :: Maybe Alias
  , ctname :: Maybe Name
  } deriving (Generic, Show, Eq)

instance A.ToJSON YTicker
instance A.ToJSON Name
instance A.ToJSON Alias
instance A.ToJSON Symbol
instance A.ToJSON CommodityTags

instance A.FromJSON YTicker
instance A.FromJSON Name
instance A.FromJSON Alias
instance A.FromJSON Symbol
instance A.FromJSON CommodityTags

newtype CommodityNames = CN (MA.Map YTicker Symbol)
   deriving (Show, Eq, Generic)

newtype Aliases = Aliases (MA.Map Alias Symbol)
   deriving (Show, Eq, Generic)

symbolToAlias :: Symbol -> Alias
symbolToAlias (S s) = A s

chineseChar :: PT.Parser Char
chineseChar = PC.satisfy (\c -> isAlpha c && c >= '\x4E00' && c <= '\x9FFF')

parseTagValue :: PT.Parser (T.Text, T.Text)
parseTagValue = do
  skipMany space
  tag <- T.pack <$> many1 (letter <|> char '_' )
  _ <- char ':'
  value <- T.pack <$>  (many (alphaNum <|> char '.' <|> char '='<|> char '^' <|> char '-' <|> chineseChar))
  skipMany space
  return (tag, value)

commodityTagsParser :: PT.Parser CommodityTags
commodityTagsParser = do
  _ <- string "commodity"
  spaces
  sym <- T.pack <$>
    (between (string "\"") (string "\"") (many1 (alphaNum <|> space <|> char '-' <|> char '.')) <|>
    many1 (letter <|> char '$' <|> char '_'))
  _ <- manyTill anyChar $ string ";"
  fields <-   parseTagValue `sepEndBy` (char ',')
  let yt = lookup "yahoo_ticker" fields
  let status = lookup "status" fields >>= parseStatusText
  let alias = lookup "alias" fields
  let name = lookup "name" fields
  case yt of
    Nothing -> fail "Missing required field: yahoo_ticker"
    Just ytValue -> return $ CommodityTags (S sym) (YT ytValue) (M.fromMaybe True status) (A <$> alias) (N <$> name)
  where
    parseStatusText :: T.Text -> Maybe Bool
    parseStatusText txt = case txt of
      "active" -> Just True
      "inactive" -> Just False
      _ -> Nothing

ignoreLine :: PT.Parser (Maybe CommodityTags)
ignoreLine = do
  _ <- notFollowedBy $ string "commodity "
  _ <- manyTill anyChar endOfLine
  return Nothing

commoditiesTagsParser :: PT.Parser [CommodityTags]
commoditiesTagsParser = M.catMaybes <$> many (ignoreLine <|> (Just <$> commodityTagsParser <* many endOfLine))

buildAliases :: [CommodityTags] -> Aliases
buildAliases ct = Aliases $ L.foldl' insertAlias MA.empty ct
  where
    insertAlias acc tags = case ctalias tags of
      Just alias -> MA.insert alias (ctsymbol tags) acc
      Nothing -> acc

buildCommodityNames :: [CommodityTags] -> CommodityNames
buildCommodityNames cn = CN $ L.foldl' insertCN MA.empty $ filter ctstatus cn
  where
    insertCN acc tags =  MA.insert (ctyahooTicker tags) (ctsymbol tags) acc

activeYTicker :: CommodityNames -> [YTicker]
activeYTicker (CN cm) = MA.keys cm

activeSymbol :: CommodityNames -> [Symbol]
activeSymbol (CN cm) = MA.elems cm

parse :: FilePath -> IO (Either Error ([CommodityTags], Aliases, CommodityNames))
parse filepath = do
  content <- TIO.readFile filepath
  case P.parse commoditiesTagsParser "commodities with tags" content of
    Left e -> return $ Left $ ParseCommoditiesTagsError e
    Right ct -> return $ Right (ct, buildAliases ct, buildCommodityNames ct)
