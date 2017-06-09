{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Zn.Bot where

import Data.Aeson hiding ((.=))
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BCL
import System.IO
import qualified System.IO.Strict as SIS

import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Lazy
import Data.Ini
import Data.Text
import GHC.Conc
import Network.IRC.Client hiding (get)
import Zn.Data.Ini
import Zn.Types

getNick :: (MonadReader (IRCState s) m, MonadIO m) => m Text
getNick = fmap (view nick) . (liftIO . atomically . readTVar) =<< view instanceConfig

param :: Text -> Bot Text
param = justLookupValueM config "main"

sleep n = liftIO . threadDelay $ n * 1000000

whine = liftIO . hPutStrLn stderr

stateful :: (State BotState a) -> StatefulBot a
stateful = runBot . state . runState

saveState :: BotState -> IO ()
saveState = writeFile botStore . L.unpack . decodeUtf8 . encode . toJSON

save :: Bot ()
save = get >>= liftIO . saveState

readFileStrict name = withFile name ReadMode SIS.hGetContents

load :: BotState -> IO BotState
load defaults = fmap (maybe defaults id . decode . BCL.pack) . readFileStrict $ botStore

reloadConf :: Bot ()
reloadConf = read >>= either whine save
    where
        save = assign config
        read = liftIO $ readIniFile confStore

reloadState :: Bot ()
reloadState = get >>= liftIO . load >>= put

reload = reloadState >> reloadConf
