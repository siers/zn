module Zn.Commands where

import Data.Aeson
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.CaseInsensitive as CI (mk)
import Data.List
import qualified Data.List.Split as List
import Data.Text as T (pack, unpack, Text, splitOn)
import Network.IRC.Client
import Zn.Bot
import Zn.Commands.Uptime
import Zn.Commands.URL
import Zn.Commands.Version

command :: String -> ([String] -> Bot String) -> UnicodeEvent -> Bot ()
command name cmd ev =
    if not (null parts) && drop 1 (parts !! 0) == name
    then (cmd $ drop 1 parts) >>= reply ev . pack
    else return ()
    where
        parts = filter (not . null) . List.splitOn " " . unpack . privtext . _message $ ev

commandP :: String -> ([String] -> String) -> UnicodeEvent -> Bot ()
commandP name cmd = command name (return . cmd)

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    , commandP "echo" (concat . intersperse " ")
    , commandP "ping" (return "pong")
    , command "version" version
    , command "uptime" uptime

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]

ignore :: [Text] -> UnicodeEvent -> Bool
ignore list = flip elem (map CI.mk list) . CI.mk . from . _source
