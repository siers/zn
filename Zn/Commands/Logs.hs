module Zn.Commands.Logs where

import Control.Arrow
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.List (intersperse, concat)
import qualified Data.Text as T
import Data.UnixTime
import Network.IRC.Client
import Zn.Bot

storeLog :: String -> [String] -> IO ()
storeLog from = appendFile (logStore from) . (++ "\n") . concat . intersperse "\t"

logs :: UnicodeEvent -> Bot ()
logs ev = do
    time <- liftIO $ getUnixTime >>= fmap B.unpack . formatUnixTime "%F %T"
    strEv <- return . fmap T.unpack $ ev
    (msg, (logName, from)) <- return . msgsrc $ strEv
    liftIO . storeLog logName $ [time, from, msg]

    where
        msg (Privmsg _from msg) = either (const "") id msg
        source (User user) = (user, user)
        source (Channel chan user) = (chan, user)

        msgsrc = msg . _message &&& source . _source
