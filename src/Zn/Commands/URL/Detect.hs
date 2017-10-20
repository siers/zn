{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.URL.Detect
    ( detectImage
    , detectNSFW
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Either.Combinators
import qualified Data.Map.Strict as M
import qualified Data.Text as T (unpack)
import Network.Simple.TCP (connect, recv, send)
import Safe
import Text.Regex.TDFA
import Zn.Bot
import Zn.Bot.Handle
import Zn.Bot.Request
import Zn.Types

detectImage :: BodyHeaders -> Maybe ByteString
detectImage (_, headers) =
    match >>= flip atMay 0 >>= flip atMay 1
    where
        match = (=~ ("^image/(.+)$" :: ByteString)) <$> contentType
        contentType = "content-typE" `M.lookup` M.fromList headers

detectNSFW :: String -> Bot (Maybe String)
detectNSFW path = lock "nsfw" $ do
    host <- T.unpack <$> param "nsfw-host"

    handleLabeledWithPrint host (const $ return Nothing) .
        fmap (fmap unpack) $
            connect host "http" $ \(s, _) -> do
                send s . pack $ "/data/" ++ path ++ "\n"
                waitOn (30 * 1000000) $ recv s 1024

    where
        waitOn n = liftIO . (fromRight Nothing <$>) . race (threadDelay n)
