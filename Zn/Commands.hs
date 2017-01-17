module Zn.Commands where

import Data.Aeson
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)

import Control.Monad
import Data.CaseInsensitive as CI (mk)
import Data.List
import qualified Data.List.Split as List
import Data.Text as T (pack, unpack, Text, splitOn)
import qualified Network.IRC.Client as IRC
import Network.IRC.Client hiding (reply)
import Zn.Bot
import Zn.Commands.Mping
import qualified Zn.Commands.Replies as Replies
import Zn.Commands.Sed
import Zn.Commands.Uptime
import Zn.Commands.URL
import Zn.Commands.Version
import qualified Zn.Grammar as Gr
import Zn.IRC

uponAddression :: (String -> Bot a) -> UnicodeEvent -> Bot (Maybe a)
uponAddression action ev = do
    nick <- Bot $ unpack . _nick <$> instanceConfig
    Gr.ifParse (Gr.addressed nick) (body ev) action

addressed :: (String -> Bot String) -> UnicodeEvent -> Bot ()
addressed action ev = void . uponAddression (reply ev action) $ ev

command :: String -> ([String] -> Bot String) -> UnicodeEvent -> Bot ()
command name action ev = do
    addressed (cmd . parts) ev
    where
        cmd (cmd:args) = if cmd == name then action args else return ""
        parts = filter (not . null) . List.splitOn " "

commandP :: String -> ([String] -> String) -> UnicodeEvent -> Bot ()
commandP name cmd = command name (return . cmd)

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    , sed
    , addressed Replies.find
    , commandP "echo" (concat . intersperse " ")
    , commandP "version" $ return version
    , command "uptime" uptime
    , command "reload" $ return reload
    , command "mping" $ return mping
    , command "replies" $ return Replies.list

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]

ignore :: [Text] -> UnicodeEvent -> Bool
ignore list = flip elem (map CI.mk list) . CI.mk . from . _source
