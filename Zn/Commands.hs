module Zn.Commands where

import Data.Aeson
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.CaseInsensitive as CI (mk)
import Data.List
import qualified Data.List.Split as List
import Data.Text as T (pack, unpack, Text, splitOn)
import Network.IRC.Client hiding (reply)
import qualified Network.IRC.Client as IRC
import Zn.Bot
import Zn.Commands.Mping
import Zn.Commands.Replies
import Zn.Commands.Uptime
import Zn.Commands.URL
import Zn.Commands.Version
import qualified Zn.Grammar as Gr
import Zn.IRC

reply ev action msg = Bot . IRC.reply ev . pack =<< action msg
body = unpack . privtext . _message

addressed :: UnicodeEvent -> (String -> Bot m) -> Bot ()
addressed ev action = do
    nick <- Bot $ unpack . _nick <$> instanceConfig
    Gr.ifParse (Gr.addressed nick) (body ev) action

command :: String -> ([String] -> Bot String) -> UnicodeEvent -> Bot ()
command name action ev = do
    addressed ev . reply ev $ cmd . parts
    where
        cmd (cmd:args) = if cmd == name then action args else return ""
        parts = filter (not . null) . List.splitOn " "

commandP :: String -> ([String] -> String) -> UnicodeEvent -> Bot ()
commandP name cmd = command name (return . cmd)

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    , \ev -> addressed ev (reply ev replies)
    , commandP "echo" (concat . intersperse " ")
    , commandP "version" $ return version
    , command "mping" $ return mping
    , command "uptime" uptime
    , command "reload" $ return reload

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]

ignore :: [Text] -> UnicodeEvent -> Bool
ignore list = flip elem (map CI.mk list) . CI.mk . from . _source
