{-# language OverloadedStrings #-}

import Bot
import Commands
import Control.Concurrent
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Text as T hiding (head)
import GHC.Conc
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.IRC.Client
import Network.IRC.Client.Types
import Network.IRC.Conduit
import Safe
import System.Environment
import Data.Time
import System.Exit
import System.Posix.Files

sleep n = liftIO . threadDelay $ n * 1000000

getTVar :: MonadIO m => m (TVar b) -> m b
getTVar accessor = accessor >>= liftIO . atomically . readTVar

setTVar :: MonadIO m => m (TVar b) -> b -> m ()
setTVar accessor val = accessor >>= liftIO . atomically . flip writeTVar val

initHandler :: Text -> [Text] -> StatefulIRC BotState ()
initHandler pwd chans = do
    liftIO getCurrentTime >>= send . Privmsg "ij" . Right . pack . show
    send . Privmsg "nickserv" . Right $ "id " `append` pwd
    mapM_ (send . Join) ["#dirsas"]

initDispatcher :: Text -> [Text] -> UnicodeEvent -> StatefulIRC BotState ()
initDispatcher pwd chans event = do
    initialized <- getTVar stateTVar
    when (not initialized) $ do
        initHandler pwd chans
    setTVar stateTVar True

conf user pwd chans = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf user
        handlers =
            [ EventHandler "init handler" EMode $ initDispatcher pwd chans
            , EventHandler "cmd handler" EPrivmsg cmdHandler]

main = do
    configFound <- fileExist "zn.rc"
    when (not configFound) $ do
        putStrLn "# no conf found\n$ cp zn.rc{sample,}"
        exitFailure

    config <- either error return =<< parseConfig "zn.rc"

    conn <- connect'
        stdoutLogger
        (BS.pack . unpack $ irchost config)
        (ircport config)
        1 :: IO (ConnectionConfig BotState)
    startStateful conn (conf (user config) (pass config) (chans config)) False
