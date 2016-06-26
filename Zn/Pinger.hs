module Zn.Pinger where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (writeTBMChan)
import Control.Concurrent (threadDelay)
import Control.Monad
import Network.IRC.Conduit ()
import Network.IRC.Client
import qualified Data.ByteString.Char8 as BS

threadDelaySec = threadDelay . (1000000 *)

pinger :: ConnectionConfig a -> BS.ByteString -> IO ()
pinger conn nick = forever $ do
    threadDelaySec $ 5 * 60
    atomically . writeTBMChan (_sendqueue conn) $ Privmsg nick (Right "!ping")
