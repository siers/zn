{-# language OverloadedStrings #-}

import Bot
import Commands
import Control.Concurrent
import Control.Monad.Reader
import qualified Data.ByteString as BS
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

sleep n = liftIO . threadDelay $ n * 1000000

getTVar :: MonadIO m => m (TVar b) -> m b
getTVar accessor = accessor >>= liftIO . atomically . readTVar

setTVar :: MonadIO m => m (TVar b) -> b -> m ()
setTVar accessor val = accessor >>= liftIO . atomically . flip writeTVar val

initHandler :: Text -> StatefulIRC BotState ()
initHandler pwd = do
    liftIO getCurrentTime >>= send . Privmsg "ij" . Right . pack . show
    send . Privmsg "nickserv" . Right $ "id " `append` pwd
    mapM_ (send . Join) ["#developerslv", "#dirsas"]

initDispatcher :: Text -> UnicodeEvent -> StatefulIRC BotState ()
initDispatcher pwd event = do
    initialized <- getTVar stateTVar
    when (not initialized) $ do
        initHandler pwd
    setTVar stateTVar True

conf nick pwd = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf nick
        handlers =
            [ EventHandler "init handler" EMode $ initDispatcher pwd
            , EventHandler "cmd handler" EPrivmsg cmdHandler]

run :: BS.ByteString -> Int -> Text -> Text -> IO ()
run host port nick pwd = do
    conn <- connect' stdoutLogger host port 1 :: IO (ConnectionConfig BotState)
    startStateful conn (conf nick pwd) False

main = do
    mayPwd <- fmap pack . headMay <$> getArgs

    when (isNothing mayPwd || fromJust mayPwd == empty) $ do
        putStrLn $ "Add nickserv password as $1."
        exitFailure

    run "irc.freenode.net" 6667 "pipele" $ fromJust mayPwd
