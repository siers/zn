module Commands where

import Bot
import Commands.URL
import Commands.Version
import Commands.Uptime
import Data.List
import Data.List.Split
import Data.Text as T (pack, unpack)
import Network.IRC.Client

command :: String -> ([String] -> Bot String) -> UnicodeEvent -> Bot ()
command name cmd ev =
    if not (null parts) && drop 1 (parts !! 0) == name
    then (cmd $ drop 1 parts) >>= reply ev . pack
    else return ()
    where
        parts = filter (not . null) . splitOn " " . unpack . privtext . _message $ ev

commandP :: String -> ([String] -> String) -> UnicodeEvent -> Bot ()
commandP name cmd = command name (return . cmd)

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    , commandP "echo" (concat . intersperse " ")
    , commandP "ping" (return "pong")
    , command "version" version
    , command "uptime" uptime
    ]

ignore :: UnicodeEvent -> Bool
ignore = (== "Xn") . from . _source

cmdHandler :: UnicodeEvent -> Bot ()
cmdHandler ev =
    if ignore ev then return ()
    else mapM_ ($ ev) commands
