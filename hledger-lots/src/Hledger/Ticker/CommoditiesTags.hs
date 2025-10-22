{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hledger.Ticker.CommoditiesTags where

import qualified Data.Maybe as M
import qualified Data.Map as MA
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Char (isAlpha)

import Text.Parsec as P
import qualified Text.Parsec.Text as PT
import qualified Text.Parsec.Char as PC
import Hledger.Ticker.Error
import GHC.Generics (Generic)

newtype Name = N { name :: T.Text }
  deriving (Generic, Show, Eq, Ord)

newtype Alias = A { alias :: T.Text }
  deriving (Generic, Show, Eq, Ord)

newtype Symbol = S { symbol :: T.Text }
  deriving (Generic, Show, Eq, Ord)

newtype YTicker = YT { yticker :: T.Text }
  deriving (Generic, Show, Eq, Ord)

newtype Group = G { group :: T.Text }
  deriving (Generic, Show, Eq, Ord)

newtype Subgroup = SG { subgroup :: T.Text }
  deriving (Generic, Show, Eq, Ord)

newtype GroupHierarchy = GroupHierarchy (Group, [Subgroup])
  deriving Show

data CommodityTags = CommodityTags
  { ctsymbol :: Symbol
  , ctyahooTicker :: YTicker
  , ctstatus :: Bool
  , ctalias :: Maybe Alias
  , ctname :: Maybe Name
  , csubgroup :: [Subgroup]
  } deriving (Generic, Show, Eq)

data GroupHierarchyOrCommodityTags
  = GH GroupHierarchy
  | CT CommodityTags
  deriving Show

data CommodityInfo = CS
  { csgroupHierarchy :: M.Map Group [Subgroup]
  , cscommoditiesTags :: [CommodityTags]
  } deriving (Show, Generic, Eq)

instance A.ToJSONKey Group
instance A.ToJSON YTicker
instance A.ToJSON Name
instance A.ToJSON Alias
instance A.ToJSON Symbol
instance A.ToJSON Subgroup
instance A.ToJSON CommodityTags
instance A.ToJSON Group
instance A.ToJSON CommodityInfo

instance A.FromJSONKey Group
instance A.FromJSON YTicker
instance A.FromJSON Name
instance A.FromJSON Alias
instance A.FromJSON Symbol
instance A.FromJSON Subgroup
instance A.FromJSON CommodityTags
instance A.FromJSON Group
instance A.FromJSON CommodityInfo

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
  value <- T.pack <$>  many (alphaNum <|> char '.' <|> char '='<|> char '^' <|> char '-' <|> char '_' <|>  char '/' <|> chineseChar)
  skipMany space
  return (tag, value)

groupParser :: Maybe T.Text -> PT.Parser [Subgroup]
groupParser Nothing =  return []
groupParser (Just raw) = return $ map SG $ T.split (=='/') raw

commodityTagsParser :: PT.Parser GroupHierarchyOrCommodityTags
commodityTagsParser = do
  _ <- string "commodity"
  spaces
  sym
    <- T.pack
    <$> (between
           (string "\"")
           (string "\"")
           (many1 (alphaNum <|> space <|> char '-' <|> char '.'))
    <|> many1 (letter <|> char '$' <|> char '_'))
  _ <- manyTill anyChar $ string ";"
  fields <-   parseTagValue `sepEndBy` char ','
  let yt = lookup "yahoo_ticker" fields
  let status = lookup "status" fields >>= parseStatusText
  let alias_ = lookup "alias" fields
  let name_ = lookup "name" fields
  group_ <- groupParser $ lookup "group" fields
  case yt of
    Nothing -> fail "Missing required field: yahoo_ticker"
    Just ytValue -> return $ CT $ CommodityTags (S sym) (YT ytValue) (Just False /= status) (A <$> alias_) (N <$> name_) group_
  where
    parseStatusText :: T.Text -> Maybe Bool
    parseStatusText txt = case txt of
      "active" -> Just True
      "inactive" -> Just False
      _ -> Nothing

ignoreLine :: PT.Parser (Maybe GroupHierarchyOrCommodityTags)
ignoreLine = do
  _ <- notFollowedBy (string "commodity " <|> string ";; G")
  _ <- manyTill anyChar endOfLine
  return Nothing

parseSubgroupName :: PT.Parser Subgroup
parseSubgroupName = do
  skipMany space
  SG . T.pack <$> many1 (alphaNum <|> char '_' )

groupSystemParsers :: PT.Parser GroupHierarchyOrCommodityTags
groupSystemParsers = do
  _ <- string ";; G"
  spaces
  groupName <- T.pack <$> many1 (letter <|> char '$' <|> char '_')
  spaces
  subgroup_ <- parseSubgroupName `sepEndBy` char ','
  return $ GH $ GroupHierarchy (G groupName, subgroup_)

groupHierarchyOrCommodityTagsParser :: PT.Parser [GroupHierarchyOrCommodityTags]
groupHierarchyOrCommodityTagsParser =
  M.catMaybes <$>
    many (
          ignoreLine
      <|> Just <$> commodityTagsParser <* many endOfLine
      <|> Just <$> groupSystemParsers <* many endOfLine
    )

toCommodityInfo
  :: [GroupHierarchyOrCommodityTags]
  -> CommodityInfo
toCommodityInfo x = sep x (CS M.empty  [])
  where
    sep
      :: [GroupHierarchyOrCommodityTags]
      -> CommodityInfo
      -> CommodityInfo
    sep [] cs = cs
    sep (GH (GroupHierarchy (k,v)):xs) (CS gc cc) = sep xs (CS (M.insert k v gc) cc)
    sep (CT c:xs) (CS gc cc) = sep xs (CS gc (c:cc))

buildAliases :: [CommodityTags] -> Aliases
buildAliases ct = Aliases $ L.foldl' insertAlias MA.empty ct
  where
    insertAlias acc tags = case ctalias tags of
      Just alias_ -> MA.insert alias_ (ctsymbol tags) acc
      Nothing -> acc

buildCommodityNames :: [CommodityTags] -> CommodityNames
buildCommodityNames cn = CN $ L.foldl' insertCN MA.empty $ filter ctstatus cn
  where
    insertCN acc tags =  MA.insert (ctyahooTicker tags) (ctsymbol tags) acc

activeYTicker :: CommodityNames -> [YTicker]
activeYTicker (CN cm) = MA.keys cm

activeSymbol :: CommodityNames -> [Symbol]
activeSymbol (CN cm) = MA.elems cm

verify :: CommodityInfo -> Either Error CommodityInfo
verify c = 
  case check declearGroup ctagsGroup of
    Just g -> Left $ VerifyError $ T.pack (show $ subgroup g) <> " isn't in any group."
    Nothing -> Right c
  where
    declearGroup = concat $ M.elems $ csgroupHierarchy c
    ctagsGroup = concatMap csubgroup $ cscommoditiesTags c
    check :: [Subgroup] -> [Subgroup] -> Maybe Subgroup
    check list = L.find (`notElem` list)

parse :: FilePath -> IO (Either Error (CommodityInfo, Aliases, CommodityNames))
parse filepath = do
  content <- TIO.readFile filepath
  case P.parse groupHierarchyOrCommodityTagsParser "commodities with tags or group" content of
    Left e -> return $ Left $ ParseCommoditiesTagsError e
    Right raw -> do
      let eInfo = verify $ toCommodityInfo raw
      return $
        flip fmap eInfo $ \info ->
          let ct = cscommoditiesTags info
          in (info, buildAliases ct, buildCommodityNames ct)
