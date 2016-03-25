{-# language OverloadedStrings #-}

module Commands where

import Bot
import Commands.URL
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text as T (pack, unpack)
import Network.IRC.Client
import Network.IRC.Client.Types
import Safe
import Text.Regex.TDFA
import Debug.Trace

command :: String -> ([String] -> String) -> UnicodeEvent -> Bot ()
command name cmd ev =
    if not (null parts) && drop 1 (parts !! 0) == name
    then reply ev . pack . cmd $ drop 1 parts
    else return ()
    where
        parts = filter (not . null) . splitOn " " . unpack . privtext . _message $ ev

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    , command "echo" (concat . intersperse " ")
    , command "ping" (return "pong")
    ]

ignore :: UnicodeEvent -> Bool
ignore = (== "Xn") . from . _source

cmdHandler :: UnicodeEvent -> Bot ()
cmdHandler ev =
    if ignore ev then return ()
    else mapM_ ($ ev) commands
