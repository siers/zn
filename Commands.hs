{-# language OverloadedStrings #-}

module Commands where

import Bot
import Commands.URL
import qualified Data.ByteString.Lazy as BL
import Data.Encoding
import Data.Encoding.UTF8
import Data.Maybe
import Data.Text as T hiding (take, drop)
import qualified Data.Text.Lazy as TL hiding (take)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.IRC.Client
import Network.IRC.Client.Types
import Network.IRC.Conduit
import Text.Regex.TDFA
import Text.XML

ignore :: UnicodeEvent -> Bool
ignore = (== "Xn") . from . _source

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    ]

cmdHandler :: UnicodeEvent -> Bot ()
cmdHandler ev =
    if ignore ev then return ()
    else mapM_ ($ ev) commands
