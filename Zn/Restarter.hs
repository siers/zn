module Zn.Restarter where

import Control.Concurrent
import Data.Maybe
import Network.Socket
import Safe
import System.Environment
import System.Posix.Process
import System.Posix.Signals
import Zn.Bot
import Zn.Restarter.Network.IRC.Conduit

zn_fd_id = "ZN_RESTART_FD"

-- executeFile :: Command -> Maybe Search PATH -> Arguments -> Maybe ArgumentsEnvironment -> IO a
restart :: Int -> IO ()
restart fd = do
    setEnv zn_fd_id $ show fd
    executeFile "/usr/bin/stack" False ["exec", "zn"] Nothing

listenForRestart :: BotState -> IO BotState
listenForRestart bot = do
    installHandler sigUSR1 handle Nothing >> return bot
    where
        getfd (MkSocket fd _ _ _ _) = fromIntegral fd
        handle = Catch $ tryReadMVar (ircsocket bot) >>= restart . getfd . fromJust

getEnvMay :: String -> IO (Maybe String)
getEnvMay name = fmap snd . headMay . Prelude.filter ((name ==) . fst) <$> getEnvironment

ircContinuousClient :: WithPortHost (IO (MVar Socket, WithPortHost IrcClient))
ircContinuousClient port host = do
    restartable <- getEnvMay zn_fd_id
    let oldsock = read . fromJust $ restartable

    if isJust restartable
    then stealSocket <$> ircClientFd oldsock port host
    else stealSocket <$> ircClientTCP port host

    where stealSocket = \(msock, client) -> (msock, \_ _ -> client)
