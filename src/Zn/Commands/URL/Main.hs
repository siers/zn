{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.URL.Main
    ( url
    , format
    , parseTitle
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import Data.List
import Data.Maybe
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

process :: PrivEvent Text -> String -> Bot ()
process pr url = do
    resp <- liftIO (request url)
    path <- store pr url resp

    nsfw <-
        if isJust $ detectImage resp
        then detectNSFW path
        else return Nothing

    reply pr (strip . pack $ format resp nsfw)

retry :: Bot () -> Bot ()
retry = recovering (limitRetries 3) [return $ Handler handler] . return
    where
        handler = return (return True) :: HttpException -> Bot Bool

link :: String -> [String]
link msg = nub . map head $ msg =~ ("https?://[^ ]+" :: String)

url :: PrivEvent Text -> Bot ()
url pr = (retry . process pr) `mapM_` (link . unpack . view cont $ pr)
