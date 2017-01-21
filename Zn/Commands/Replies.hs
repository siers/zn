{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Control.Lens
import Data.Either.Extra
import Data.Ini
import Data.List
import qualified Data.Text as T
import Data.Text (unpack, pack, Text)
import Network.IRC.Client
import Zn.Bot
import Zn.Data.Ini

find :: Text -> Bot Text
find cmd = do
    conf <- use config
    return . either (const "") id $ lookupValue "replies" cmd conf

list :: Bot Text
list = uses config $ (format . names)
    where
        names = fromRight . keys "replies"
        format = T.append "available replies: " . T.intercalate ", "
