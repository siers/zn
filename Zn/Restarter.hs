module Zn.Restarter where

import Data.ByteString (ByteString)
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
    getProgName >>= (\name -> executeFile name False [] Nothing)

listenForRestart :: BotState -> IO BotState
listenForRestart bot = installHandler sigUSR1 handle Nothing >> return bot
    where
        handle = Catch $ restart <$> getfd $ ircsocket bot
        getfd (MkSocket fd _ _ _ _) = fromIntegral fd

getEnvMay :: String -> IO (Maybe String)
getEnvMay name = fmap snd . headMay . Prelude.filter ((name ==) . fst) <$> getEnvironment

ircContinuousClient :: WithPortHost (IO (Socket, WithPortHost IrcClient))
ircContinuousClient port host = do
    restartable <- getEnvMay zn_fd_id
    let oldsock = read . fromJust $ restartable

    if isJust restartable
    then stealSocket <$> ircClientFd oldsock port host
    else stealSocket <$> ircClientTCP port host

    where stealSocket = \(sock, client) -> (sock, \_ _ -> client)
