module Zn.Socket
    (
        runRawSocket
    ) where

import Control.Concurrent.Async (race)
import Control.Concurrent (MVar, takeMVar)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Exception (SomeException, catch)
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

type MsgChan = TVar (TBMChan (Message BS.ByteString))

process :: Socket -> MsgChan -> IO ()
process sock tchan = do
    client <- fst <$> accept sock
    (head:rest) <- unscramble <$> SB.recv client 8192

    atomically $ do
        chan <- readTVar tchan
        writeTBMChan chan $ rawMessage head rest

    shutdown client ShutdownBoth

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

runRawSocket :: IRCState a -> MVar b -> IO ()
runRawSocket ircst control = void $ do
    sock <- socket AF_UNIX Stream 0
    name <- printf "/tmp/zn.sock.%i" . toInteger <$> getProcessID

    bind sock $ SockAddrUnix name
    listen sock 5

    race
        (takeMVar control)
        (forever . handle $ process sock (_sendqueue ircst))

    shutdown sock ShutdownBoth
    removeLink name

    where
        handle a = catch a (print :: SomeException -> IO ())
        print = Prelude.putStrLn . ("*** zn-caught exception: " ++) . show
