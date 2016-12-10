{-# LANGUAGE DeriveGeneric #-}

module Zn.Bot where

import Data.Aeson
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Either
import Data.Ini
import Data.Text
import Data.Time
import GHC.Conc
import GHC.Generics
import Network.IRC.Client.Types
import Network.Socket
import Zn.Data.Ini
import Zn.Data.UMVar

data BotState = BotState
    { bootTime :: UTCTime
    , config :: Ini
    , ircsocket :: UnserializableMVar Socket
    } deriving (Show, Generic)

instance ToJSON BotState
instance FromJSON BotState
botStore = "data/state.json"

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

saveState :: BotState -> IO ()
saveState = writeFile botStore . L.unpack . decodeUtf8 . encode . toJSON

save :: Bot ()
save = getTVar stateTVar >>= liftIO . saveState

load :: BotState -> IO BotState
load defaults = fmap (maybe defaults id . decode) . BL.readFile $ botStore
