module Zn.IRC where

import Data.Text
import Network.IRC.Client as IRC
import Zn.Bot

target :: Source Text -> Text
target (Channel chan user) = chan
target (User user) = user

from :: Source Text -> Text
from (Channel chan user) = user
from (User user) = user

privtext :: Message Text -> Text
privtext (Privmsg _from msg) = either (const "") id msg

reply ev action msg = Bot . IRC.reply ev =<< action msg
body = privtext . _message
