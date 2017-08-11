{-# LANGUAGE OverloadedStrings #-}

module Zn.IRC where

import Control.Lens
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Network.IRC.Client as IRC
import Zn.Bot
import Zn.Commands.Logs
import Zn.Types

target :: Source a -> a
target (Channel chan user) = chan
target (User user) = user

from :: Source a -> a
from (Channel chan user) = user
from (User user) = user

isUser (User _) = True
isUser _        = False

isChan (Channel _ _) = True
isChan _             = False

--

reply :: Packet p => p Text -> Text -> Bot ()
reply cmd text = do
    shush <- use silence
    when (not shush) $ do
        nick' <- Bot getNick
        Bot $ IRC.replyTo (view src cmd) text
        logsFor nick' (PrivEvent text (view src cmd))

joinLines :: Text -> [Text] -> Text
joinLines sep list = T.intercalate sep . filter (not . T.null) $ list

joinCmds = joinLines cmdSep
