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
import Data.Sequence
import Data.Either
import Data.Ini
import Data.List ((\\))
import qualified Data.Map as M
import Data.Text
import Data.Time
import GHC.Conc
import GHC.Generics (Generic)
import Network.IRC.Client.Types hiding (state)
import Network.Socket
import Text.Printf
import Zn.Data.Ini
import Zn.Data.UMVar

data BotState = BotState
    { _bootTime :: UTCTime
    , _config :: Ini
    , _history :: M.Map String (Seq [String]) -- new in front
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

param :: String -> Bot String
param = justLookupValueMStr config "main"

sleep n = liftIO . threadDelay $ n * 1000000

stateful :: (State BotState a) -> StatefulBot a
stateful = runBot . state . runState

saveState :: BotState -> IO ()
saveState = writeFile botStore . L.unpack . decodeUtf8 . encode . toJSON

save :: Bot ()
save = get >>= liftIO . saveState

readFileStrict name = withFile name ReadMode SIS.hGetContents

load :: BotState -> IO BotState
load defaults = fmap (maybe defaults id . decode . BCL.pack) . readFileStrict $ botStore

reloadConf :: Bot String
reloadConf = read >>= either return ((*> return "") . save)
    where
        save = assign config
        read = liftIO $ readIniFile confStore

reloadState :: Bot ()
reloadState = get >>= liftIO . load >>= put

reload = reloadState >> reloadConf
