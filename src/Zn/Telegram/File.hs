{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Zn.Telegram.File where

import qualified Data.Binary.Builder as B
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Network.HTTP.Types.URI
import Text.Printf
import Zn.Bot
import Zn.Commands.URL.Detect
import Zn.Commands.URL.Format
import Zn.Commands.URL.Main
import Zn.Telegram.Types
import Zn.Types

handleFile :: PrivEvent Text -> String -> FileLink -> Bot (String, Text)
handleFile pr prefix link = do
    path <- fst <$> download (Just prefix) pr (unpack link)
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
