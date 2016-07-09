{-# LANGUAGE ImpredicativeTypes #-}

module Zn.Restarter.Network.IRC.Conduit where

import Data.ByteString
import Data.Conduit
import Data.Conduit.Network
import Data.Maybe
import Data.Streaming.Network
import Network.IRC.Conduit
import Network.Socket
import Zn.Restarter.Data.Streaming.Network

type IrcClient = IO ()
              -- ^Any initialisation work (started concurrently with the
              -- producer and consumer)
              -> Consumer (Either ByteString IrcEvent) IO ()
              -- ^The consumer of irc events
              -> Producer IO IrcMessage
              -- ^The producer of irc messages
              -> IO ()
type WithPortHost a = Int -> ByteString -> a

ircClientFd :: Int -> WithPortHost (IO (Socket, IrcClient))
ircClientFd fd port host =
    runFdClient fd (clientSettings port host) $ \appdata ->
        return (
            fromJust . appRawSocket $ appdata,
            ircWithConn (\f -> f appdata)
        )

ircClientTCP :: WithPortHost (IO (Socket, IrcClient))
ircClientTCP port host =
    runTCPClient (clientSettings port host) $ \appdata ->
        return (
            fromJust . appRawSocket $ appdata,
            ircWithConn (\f -> f appdata)
        )
