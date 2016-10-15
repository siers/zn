{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Ini
import Data.Text as T hiding (head)
import Data.Text.Encoding
import Data.Time
import Network.IRC.Client hiding (instanceConfig)
import Network.Socket
import Safe
import System.Exit
import System.Posix.Files
import Zn.Bot
import Zn.Commands
import Zn.Handlers
import Zn.Pinger
import Zn.Restarter

instanceConfig config = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf $ setting config "user"
        handlers =
            [ EventHandler "cmd handler" EPrivmsg cmdHandler]

logger file origin = do
    appendFile file . ((show origin ++ " | ") ++) . (++ "\n") . BS.unpack

connection :: Ini -> IO (MVar Socket, ConnectionConfig BotState)
connection conf = do
    (msock, client) <- ircContinuousClient port host
    (\conn -> (msock, ) $ conn
        { _func =  client
        , _onconnect = initHandler conf
    }) <$> action

    where
        action = connect' (logger . unpack $ setting conf "logfile") host port 1
        host = (BS.pack . unpack $ setting conf "irchost")
        port = (maybe (error "cannot parse ircport") id . readMay . unpack $ setting conf "ircport")

main = do
    configFound <- fileExist "zn.rc"
    when (not configFound) $ do
        putStrLn "# no conf found\n$ cp zn.rc{.sample,}"
        exitFailure

    conf <- either error id <$> readIniFile "zn.rc"
    (msock, conn) <- connection conf
    state <- BotState <$> getCurrentTime <*> pure conf <*> pure msock

    forkIO $ pinger conn (encodeUtf8 $ setting conf "user")
    listenForRestart state >>= startStateful conn (instanceConfig conf)
