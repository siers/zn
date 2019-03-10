{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Zn.Telegram.File where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Magic
import Network.HTTP.Types.URI
import qualified Data.Binary.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Text.Printf
import Zn.Bot
import Zn.Commands.URL.Detect
import Zn.Commands.URL.Format
import Zn.Commands.URL.Main
import Zn.Telegram.Types
import Zn.Types

storeFile :: PrivEvent Text -> String -> FileLink -> Bot (String, Text)
storeFile pr prefix link = do
    path <- fst <$> download (Just prefix) pr (unpack link)
    (path, ) <$> (formatUrl <$> param "http-root" <*> pure path)
  where
    formatUrl :: Text -> String -> Text -- "http://x" "/ " => "http://x/%20"
    formatUrl root path = (root `mappend`) . TL.toStrict . TLE.decodeUtf8 $
        B.toLazyByteString (encodePathSegments [pack path])

formatComponents :: [Maybe Text] -> Text
formatComponents = T.intercalate " " . catMaybes

formatFile :: Maybe Text -> PathLinkPair -> Bot Text
formatFile caption (path, url) = do
  magic <- liftIO $ magicOpen [MagicMime]
  liftIO $ magicLoadDefault magic
  mime <- liftIO $ magicFile magic path

  return $ formatComponents [caption, Just url, Just "•", Just $ pack mime]

formatPhoto :: Maybe Text -> PathLinkPair -> Bot Text
formatPhoto caption (path, url) = do
    nsfw <- (formatNSFW =<<) <$> detectNSFW path
    return $ formatComponents [caption, Just url, pack . printf "(%s)" <$> nsfw]
