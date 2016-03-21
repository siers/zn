{-# language OverloadedStrings #-}

module Bot where

import Data.Text
import Network.IRC.Client.Types
import Network.IRC.Conduit

type BotState = Bool
type Bot a = StatefulIRC BotState a

target :: Source Text -> Text
target (Channel chan user) = chan
target (User user) = user

from :: Source Text -> Text
from (Channel chan user) = user
from (User user) = user

privtext :: Message Text -> Text
privtext (Privmsg _from msg) = either (const "") id msg
