{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Zn.Telegram.Photo where

import qualified Data.Binary.Builder as B
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Text.ICU
import Data.Text.ICU.Char
import Data.Text.ICU.Replace
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Network.HTTP.Types.URI
import Text.Printf
import Web.Telegram.API.Bot as T (User(..))
import Zn.Bot
import Zn.Commands.URL.Detect
import Zn.Commands.URL.Format
import Zn.Commands.URL.Main
import Zn.Telegram.Types
import Zn.Types

photoPath :: PrivEvent Text -> UpdateSummary PhotoMsg a -> String
photoPath pr (uid, (User { user_first_name = who }), (ZnPhoto (caption, _))) =
    intercalate "-" ((not . null) `filter` components) <> ".jpg"
  where
    -- http://stackoverflow.com/questions/44290218
    canonicalForm = T.filter (not . property Diacritic) . normalize NFD
    limitChar = T.dropAround (== '-') . replaceAll (regex [] "[^a-z0-9\\-~+=]+") (rtext "-")
    captionSlug = T.take 48 . limitChar . T.toLower . canonicalForm . fromMaybe ""
    components = unpack <$> ["telegram", who, pack $ show uid, captionSlug caption]

handleFile :: PrivEvent Text -> String -> PhotoLink -> Bot (String, Text)
handleFile pr prefix link = do
    path <- fst <$> download (Just prefix) pr link
    (path, ) <$> (formatUrl <$> param "http-root" <*> pure path)
  where
    formatUrl :: Text -> String -> Text -- "http://x" "/ " => "http://x/%20"
    formatUrl root path = (root `mappend`) . TL.toStrict . TLE.decodeUtf8 $
        B.toLazyByteString (encodePathSegments [pack path])

formatPhoto :: Maybe Text -> (String, Text) -> Bot Text
formatPhoto caption (path, url) = do
    nsfw <- (formatNSFW =<<) <$> detectNSFW path
    return $ formatComponents [caption, Just url, pack . printf "(%s)" <$> nsfw]
  where
    formatComponents :: [Maybe Text] -> Text
    formatComponents = T.intercalate " " . catMaybes
