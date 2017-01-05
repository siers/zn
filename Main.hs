{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow
import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Ini
import Data.Ratio as Ratio
import Data.Maybe (fromJust)
import Data.Map as M
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
import Zn.Data.Ini
import Zn.Data.UMVar
import Zn.Handlers
import Zn.Pinger
import Zn.Restarter

instanceConfig config = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf $ parameter config "user"
        handlers =
            [ EventHandler "cmd handler" EPrivmsg cmdHandler]

connection :: Ini -> IO (MVar Socket, ConnectionConfig BotState, Bool)
connection conf = do
    (msock, client, restarted) <- ircContinuousClient port host
    (\conn -> (msock, , restarted) $ conn
        { _func =  client
        , _onconnect = initHandler conf
        , _flood = fromRational $ 1 Ratio.% 2
    }) <$> action

    where
        action = connect' stdoutLogger host port 1
        host = (BS.pack . unpack $ parameter conf "irchost")
        port = (maybe (error "cannot parse ircport") id . readMay . unpack $ parameter conf "ircport")

main = do
    configFound <- fileExist "zn.rc"
    when (not configFound) $ do
        putStrLn "# no conf found\n$ cp zn.rc{.sample,}"
        exitFailure

    conf <- either error id <$> readIniFile "zn.rc"
    (msock, conn, restarted) <- connection conf

    let defaults = BotState <$> getCurrentTime <*> pure conf <*> pure M.empty <*> (UMVar <$> newEmptyMVar)

    state <- defaults
        >>= (if restarted then load else pure)
        >>= return . (ircsocket .~ UMVar msock)

    saveState state
    forkIO $ pinger conn (encodeUtf8 $ parameter conf "user")
    listenForRestart state >>= startStateful conn (instanceConfig conf)
