{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Control.Lens
import Data.Either.Extra
import Data.Ini
import Data.List
import qualified Data.Text as T
import Network.IRC.Client
import Zn.Bot
import Zn.Data.Ini

find :: String -> Bot String
find cmd = do
    conf <- use config
    return . either (const "") id $ lookupValueS "replies" cmd conf

list :: Bot String
list = uses config (("available replies: "++) . join . names)
    where
        names = map T.unpack . fromRight . keys "replies"
        join = concat . intersperse ", "
