{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Commands.URL.Main
    ( url
    , format
    , parseTitle
    , download
    , process
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Retry
import Data.List
import Data.Text (Text, pack, unpack, strip)
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
    (, resp) <$> storePrefix prefix pr url resp

process :: PrivEvent Text -> (String, BodyHeaders) -> Bot ()
process pr (path, resp) = do
    nsfw <- fmap join . sequence $ detectNSFW path <$ detectImage resp
    reply pr . strip . pack $ format resp nsfw

retry :: Bot () -> Bot ()
retry = recovering (limitRetries 3) [return $ Handler handler] . return
    where
        handler = return (return True) :: HttpException -> Bot Bool

link :: String -> [String]
link msg = nub . map head $ msg =~ ("https?://[^ ]+" :: String)

url :: PrivEvent Text -> Bot ()
url pr =
    ((retry . process pr =<<) . download Nothing pr)
    `mapM_`
    (link . unpack . view cont $ pr)
