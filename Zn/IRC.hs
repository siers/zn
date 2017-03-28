{-# LANGUAGE OverloadedStrings #-}

module Zn.IRC where

import Control.Lens
import qualified Data.Text as T
import Data.Text (Text)
import Network.IRC.Client as IRC
import Zn.Bot
import Zn.Command

target :: Source Text -> Text
target (Channel chan user) = chan
target (User user) = user

from :: Source Text -> Text
from (Channel chan user) = user
from (User user) = user

isUser (User _) = True
isUser _        = False

isChan (Channel _ _) = True
isChan _             = False

--

reply :: Packet p => p Text -> Text -> Bot ()
reply cmd = Bot . IRC.replyTo (view src cmd)

joinLines :: Text -> [Text] -> Text
joinLines sep list = T.intercalate sep . filter (not . T.null) $ list

joinCmds = joinLines cmdSep
