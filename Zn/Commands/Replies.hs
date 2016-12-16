{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Data.Ini
import qualified Data.Text as T
import Network.IRC.Client
import Zn.Bot
import Zn.Data.Ini

replies :: String -> [String] -> Bot String
replies cmd _name = do
    conf <- config <$> getTVar stateTVar
    return . either (const "") id $ lookupValueS "replies" cmd conf
