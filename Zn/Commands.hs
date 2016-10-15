module Zn.Commands where

import Data.CaseInsensitive as CI (mk)
import Data.List
import qualified Data.List.Split as List
import Data.Text as T (pack, unpack, Text, splitOn)
import Network.IRC.Client
import Zn.Bot
import Zn.Commands.History
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
    , command "history" history
    -- Do we need the !last command?
    --, command "last" lastMsgs
    , command "version" version
    , command "uptime" uptime
    ]

ignore :: [Text] -> UnicodeEvent -> Bool
ignore list = flip elem (map CI.mk list) . CI.mk . from . _source
