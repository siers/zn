{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Distribute
    ( distribute
    , botcast
    , botnicks ) where

import Control.Arrow
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Format
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Network.IRC.Client
import Zn.Bot
import Zn.Commands.Logs
import Zn.IRC
import Zn.Types

amass :: History Text -> [Text]
amass = map (TL.toStrict . concat) . filter (not . null . snd) . M.toList
    where
        join = joinCmds . fmap (view text) . toList
        concat = format "{}[{}]" . second join

transfer :: Text -> Text -> StatefulBot ()
transfer payload nick = send . Privmsg nick . Right $ payload

distribute :: Text -> [Text] -> Bot Text
distribute payload users = do
    mapM_ (Bot . transfer payload) $ users

    start <- liftIO getPOSIXTime
    sleep $ 1 + length users * 1

    T.intercalate ", " . amass . logsFrom users . logTail start <$> use history

botnicks :: Bot [Text]
botnicks = do
    myself <- Bot $ getNick
    L.nub . (++ [myself]) . T.splitOn "," <$> param "bots"

botcast :: Text -> Bot Text
botcast payload =
    lock "botcast" $ do
        distribute payload =<< botnicks
