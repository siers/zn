module Bot where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Ini
import Data.Text
import Data.Time
import GHC.Conc
import Network.IRC.Client.Types
import Safe

data BotState = BotState
    { initialized :: Bool
    , bootTime :: UTCTime
    , config :: Ini
    }
type Bot a = StatefulIRC BotState a

target :: Source Text -> Text
target (Channel chan user) = chan
target (User user) = user

from :: Source Text -> Text
from (Channel chan user) = user
from (User user) = user

privtext :: Message Text -> Text
privtext (Privmsg _from msg) = either (const "") id msg

sleep n = liftIO . threadDelay $ n * 1000000

getTVar :: MonadIO m => m (TVar b) -> m b
getTVar accessor = accessor >>= liftIO . atomically . readTVar

setTVar :: MonadIO m => m (TVar b) -> b -> m ()
setTVar accessor val = accessor >>= liftIO . atomically . flip writeTVar val

setting conf name =
    either (error . ("Couldn't find in config: " ++)) id $
    lookupValue "main" name conf
