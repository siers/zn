{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Zn.Bot where

import Data.Aeson hiding ((.=))
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BCL
import System.IO
import qualified System.IO.Strict as SIS

import Control.Concurrent
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Ini
import Data.List ((\\))
import qualified Data.Map as M
import Data.Sequence
import Data.Text
import Data.Time
import GHC.Generics (Generic)
import Network.IRC.Client hiding (get)
-- import Network.IRC.Client.Lens
import Text.Printf
import Zn.Data.Ini

data BotState = BotState
    { _bootTime :: UTCTime
    , _config :: Ini
    , _history :: M.Map Text (Seq [Text]) -- new in front
    } deriving (Show, Generic)

makeLenses ''BotState

instance ToJSON BotState
instance FromJSON BotState

confStore = "zn.rc"
botStore = "data/state.json"
logStore s = printf "data/logs/%s.log" $ s \\ ['.', '/']

cmdSep = seq " ▞ " " ╱ " :: Text

type StatefulBot a = IRC BotState a
newtype Bot a = Bot { runBot :: StatefulBot a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadCatch, MonadThrow, MonadMask, MonadState BotState)

instance Monoid a => Monoid (Bot a) where
    mempty = return mempty
    a `mappend` b = liftM2 mappend a b

param :: Text -> Bot Text
param = justLookupValueM config "main"

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
