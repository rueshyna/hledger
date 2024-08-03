{-# LANGUAGE OverloadedStrings #-}

module Lib.CommoditiesTags where

import qualified Data.Maybe as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Text.Parsec as P
import qualified Text.Parsec.Text as PT
import Lib.Error

data CommodityTags = CommodityTags
  { ctsymbol :: T.Text
  , ctyahooTicker :: T.Text
  , ctstatus :: Bool
  , ctalias :: Maybe T.Text
  } deriving (Show, Eq)

parseTagValue :: PT.Parser (T.Text, T.Text)
parseTagValue = do
  skipMany space
  tag <- T.pack <$> many1 (letter <|> char '_')
  _ <- char ':'
  value <- T.pack <$>  (many1 (alphaNum <|> char '.' <|> char '='))
  skipMany space
  return (tag, value)

commodityTagsParser :: PT.Parser CommodityTags
commodityTagsParser = do
  _ <- string "commodity"
  spaces
  sym <- T.pack <$> (between (string "\"") (string "\"") (many1 (alphaNum <|> space)) <|> many1 letter)
  _ <- manyTill anyChar $ string ";"
  fields <-  parseTagValue `sepEndBy` (char ',')
  let yt = lookup "yahoo_ticker" fields
  let status = lookup "status" fields >>= parseStatusText
  let alias = lookup "alias" fields
  case yt of
    Nothing -> fail "Missing required field: yahoo_ticker"
    Just ytValue -> return $ CommodityTags sym ytValue (M.fromMaybe True status) alias
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

parse :: FilePath -> IO (Either Error [CommodityTags])
parse filepath = do
  content <- TIO.readFile filepath
  case P.parse commoditiesTagsParser "commodities with tags" content of
    Left e -> return $ Left $ ParseCommoditiesTagsError e
    Right ct -> return $ Right ct
