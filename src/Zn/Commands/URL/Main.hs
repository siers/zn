{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Commands.URL.Main
  ( url
  , format
  , parseTitle
  , download
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Retry
import Data.List
import Data.Text (Text, pack, unpack, strip)
import Data.Traversable (for)
import Network.HTTP.Client
import Prelude hiding (concat)
import Text.Regex.TDFA
import Zn.Bot.Request
import Zn.Commands.URL.Detect
import Zn.Commands.URL.Format
import Zn.Commands.URL.Store
import Zn.IRC
import Zn.Types

download :: Maybe String -> PrivEvent Text -> String -> Bot (String, BodyHeaders)
download prefix pr url = do
  resp <- request url
  (,) <$> storePrefix prefix pr url resp <*> pure resp

retry :: Bot () -> Bot ()
retry = recovering (limitRetries 3) [return $ Handler handler] . return
  where handler = return (return True) :: HttpException -> Bot Bool

link :: String -> [String]
link msg = nub . map head $ msg =~ ("https?://[^ ]+" :: String)

url :: PrivEvent Text -> Bot ()
url pr = void . for (link (unpack (view cont pr))) $ \url -> do
  (path, response@(body, headers)) <- download Nothing pr url
  retry $ do
    nsfw <- fmap join . traverse (const (detectNSFW path)) $ detectImage headers
    void ((reply pr . strip . pack) `traverse` format response nsfw)
