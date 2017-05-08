{-# LANGUAGE OverloadedStrings #-}

module Zn.Socket
    ( runRawSocket
    , ircInjectMsg
    ) where

import Control.Concurrent.Async (race)
import Control.Concurrent (MVar, takeMVar)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Monad
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as BSB
import Data.Maybe
import Data.Text.Encoding
import Network.IRC.Client
import Network.IRC.Client.Internal
import Network.Socket
import qualified Network.Socket.ByteString as SB
import System.Posix.Files
import System.Posix.Process
import Text.Printf
import Zn.Bot
import Zn.Bot.Handle as BH
import Zn.Commands.Logs
import Zn.Types

type MsgChan = TVar (TBMChan (Message BS.ByteString))

ircInjectMsg :: IRCState BotState -> [BS.ByteString] -> IO ()
ircInjectMsg ircst (head:rest) =
    atomically $ do
        chan <- readTVar (_sendqueue ircst)
        writeTBMChan chan $ rawMessage head rest

process :: Socket -> IRCState BotState -> IO ()
process sock ircst = do
    client <- fst <$> accept sock
    msg@(head:rest) <- unscramble <$> SB.recv client 8192

    ircInjectMsg ircst msg
    shutdown client ShutdownBoth

    when (head == "PRIVMSG") $ asBot $ do
        nick <- Bot getNick
        let (chan:msg:[]) = map decodeUtf8 rest
        logs $ PrivEvent msg (Channel chan nick)

    where
        {-
        unscramble :: BS.ByteString -> Either String [BS.ByteString]
        unscramble = sequence . map base64decode . fromJust . decode . toLazyBS
        base64decode = B64.decode . encodeUtf8
        -}

        unscramble :: BS.ByteString -> [BS.ByteString]
        unscramble = map base64decode . fromJust . decode . toLazyBS
        base64decode = B64.decodeLenient . encodeUtf8

        toLazyBS = BSB.toLazyByteString . BSB.byteString

        asBot = flip runIRCAction ircst . runBot

runRawSocket :: IRCState BotState -> MVar b -> IO ()
runRawSocket ircst control = void $ do
    sock <- socket AF_UNIX Stream 0
    name <- printf "/tmp/zn.sock.%i" . toInteger <$> getProcessID

    bind sock $ SockAddrUnix name
    listen sock 5

    race
        (takeMVar control)
        (forever . BH.handle $ process sock ircst)

    shutdown sock ShutdownBoth
    removeLink name
