{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Distribute
    ( distribute
    , botcast
    , botnicks ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (unpack, pack, Text)
import Data.Time.Clock.POSIX
import Network.IRC.Client
import Text.Printf
import Zn.Bot
import Zn.Command
import Zn.Commands.Logs
import Zn.IRC

amass :: History Text -> [Text]
amass = map (\(nick, msgs) -> pack . printf "%s[%s]" (unpack nick) $ join msgs) . M.toList
    where
        join = unpack . T.toLower . joinCmds . fmap (view text) . toList

transfer :: Text -> Text -> StatefulBot ()
transfer payload nick = send . Privmsg nick . Right $ payload

distribute :: Text -> [Text] -> Bot Text
distribute payload users = do
    mapM_ (Bot . transfer payload) $ users

    start <- liftIO getPOSIXTime
    sleep $ 1 + length users * 1

    T.intercalate ", " . amass . logTail start . logsFrom users <$> use history

botnicks :: Bot [Text]
botnicks = do
    myself <- Bot $ getNick
    L.nub . (myself :) . T.splitOn "," <$> param "bots"

botcast :: Text -> Bot Text
botcast payload = distribute payload =<< botnicks
