{-# LANGUAGE TupleSections #-}

module Zn.Restarter where

import Control.Concurrent
import Data.Maybe
import Network.Socket
import Safe
import System.Environment
import System.Posix.Process
import System.Posix.Signals
import Zn.Data.UMVar
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
        handle = Catch $ tryReadMVar (umvar $ ircsocket bot) >>= restart . getfd . fromJust

ircContinuousClient :: WithPortHost (IO (MVar Socket, WithPortHost IrcClient, Bool))
ircContinuousClient port host = do
    potentialFd <- lookupEnv zn_fd_id
    let restartable = isJust potentialFd
    let oldsock = read . fromJust $ potentialFd

    fmap ((\f -> f (,, restartable)) . stealSocket) $
        if restartable
        then ircClientFd oldsock port host
        else ircClientTCP port host

    where stealSocket = \(msock, client) -> (\g -> g msock (\_ _ -> client))
