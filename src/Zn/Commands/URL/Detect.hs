{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.URL.Detect
    ( detectImage
    , detectNSFW
    ) where

import Control.Concurrent
import Control.Concurrent.Async
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Either.Combinators
import qualified Data.Map.Strict as M
import Safe
import Network.Simple.TCP (connect, recv, send)
import Text.Regex.TDFA
import Zn.Commands.URL.Types
import Zn.Types

detectImage :: BodyHeaders -> Maybe ByteString
detectImage (_, headers) =
    match >>= flip atMay 0 >>= flip atMay 1
    where
        match = (=~ ("^image/(.+)$" :: ByteString)) <$> contentType
        contentType = "content-typE" `M.lookup` M.fromList headers

detectNSFW :: String -> Bot (Maybe String)
detectNSFW path = fmap (fmap unpack) $
    connect "172.17.0.2" "http" $ \(s, _) -> do
        send s . pack $ "/data/" ++ path ++ "\n"
        recv s 1024
