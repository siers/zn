{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Zn.Bot where

import Data.Aeson hiding ((.=))
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BCL
import System.IO
import qualified System.IO.Strict as SIS

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Lens.TH
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Either
import Data.Ini
import Data.List ((\\))
import Data.Text
import Data.Time
import GHC.Conc
import GHC.Generics (Generic)
import Network.IRC.Client.Types
import Network.Socket
import Text.Printf
import Zn.Data.Ini
import Zn.Data.UMVar

data BotState = BotState
    { _bootTime :: UTCTime
    , _config :: Ini
    , _ircsocket :: UnserializableMVar Socket
    } deriving (Show, Generic)

makeLenses ''BotState

instance ToJSON BotState
instance FromJSON BotState

confStore = "zn.rc"
botStore = "data/state.json"
logStore s = printf "data/logs/%s.log" $ s \\ ['.', '/']

type StatefulBot a = StatefulIRC BotState a
newtype Bot a = Bot { runBot :: StatefulBot a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadCatch, MonadThrow, MonadMask)

instance MonadState BotState Bot where
    state f = do
        tvar <- Bot stateTVar
        liftIO . atomically $ do
            (a, s) <- f <$> readTVar tvar
            a <$ writeTVar tvar s

target :: Source Text -> Text
target (Channel chan user) = chan
target (User user) = user

from :: Source Text -> Text
from (Channel chan user) = user
from (User user) = user

privtext :: Message Text -> Text
privtext (Privmsg _from msg) = either (const "") id msg

sleep n = liftIO . threadDelay $ n * 1000000

atomStateful :: (State BotState a) -> StatefulBot a
atomStateful action = do
    tvar <- stateTVar
    liftIO . atomically $ do
        iSt <- readTVar tvar
        (fVal, fSt) <- return $ runState action iSt
        writeTVar tvar fSt *> return fVal

atomState = Bot . atomStateful

saveState :: BotState -> IO ()
saveState = writeFile botStore . L.unpack . decodeUtf8 . encode . toJSON

save :: Bot ()
save = atomState get >>= liftIO . saveState

readFileStrict name = withFile name ReadMode SIS.hGetContents

load :: BotState -> IO BotState
load defaults = fmap (maybe defaults id . decode . BCL.pack) . readFileStrict $ botStore

reloadConf :: Bot String
reloadConf = read >>= either return ((*> return "") . save)
    where
        save = atomState . assign config
        read = liftIO $ readIniFile confStore

reloadState :: Bot ()
reloadState = atomState get >>= liftIO . load >>= atomState . put

reload = reloadState >> reloadConf
