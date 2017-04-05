{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Zn.IRC where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Conc
import Network.IRC.Client as IRC
import Zn.Bot
import Zn.Command
import Zn.Commands.Logs

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

getNick :: (MonadReader (IRCState s) m, MonadIO m) => m Text
getNick = fmap (view nick) . (liftIO . atomically . readTVar) =<< view instanceConfig

reply :: Packet p => p Text -> Text -> Bot ()
reply cmd text = do
    nick' <- Bot getNick

    Bot $ IRC.replyTo (view src cmd) text
    logsFor nick' (PrivEvent text (view src cmd))

joinLines :: Text -> [Text] -> Text
joinLines sep list = T.intercalate sep . filter (not . T.null) $ list

joinCmds = joinLines cmdSep
