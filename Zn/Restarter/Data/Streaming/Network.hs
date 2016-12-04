module Zn.Restarter.Data.Streaming.Network where

import Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as S8
import Data.Streaming.Network
import Data.Streaming.Network.Internal
import Foreign.C.Types
import Network.Socket as NS
import Network.Socket.ByteString (sendAll)
import Zn.Data.UMVar

-- Reference: network-2.6.2.1/docs/src/Network-Socket.html#socket
fdToSocket :: Int -> NS.AddrInfo -> IO Socket
fdToSocket handle ad =
    MkSocket
    (CInt . read . show $ handle)
    (addrFamily ad) (addrSocketType ad) (addrProtocol ad)
    <$> newMVar Connected

-- Converts handle into socket and adds AddrInfo.
getSocketFamilyTCPFd :: Int -> String -> Int -> NS.Family -> IO (Socket, NS.SockAddr)
getSocketFamilyTCPFd handle host' port' af = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = NS.Stream
                        , NS.addrFamily = af
                        }
    (addr:_) <- NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
    sock <- fdToSocket handle addr
    return (sock, NS.addrAddress addr)

runFdClient :: Int -> ClientSettings -> (AppData -> IO a) -> IO a
runFdClient fd (ClientSettings port host addrFamily readBufferSize) app = E.bracket
    (getSocketFamilyTCPFd fd (S8.unpack host) port addrFamily)
    return -- (NS.sClose . fst)
    (\(s, addr) -> app AppData
        { appRead' = safeRecv s readBufferSize
        , appWrite' = sendAll s
        , appSockAddr' = addr
        , appLocalAddr' = Nothing
        , appCloseConnection' = NS.close s
        , appRawSocket' = Just s
        })
