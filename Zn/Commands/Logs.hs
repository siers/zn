module Zn.Commands.Logs (Log, logs) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.List (intersperse, concat)
import Data.Map (Map, insertWith)
import Data.Sequence as S
import qualified Data.Text as T
import Data.UnixTime
import Network.IRC.Client
import Prelude hiding (log, take)
import Zn.Bot
import Zn.Data.Ini

type Log = (String, [String])
-- `uncurry logger $ log' must typecheck

serialize :: Log -> String
serialize = (++ "\n") . concat . intersperse "\t" . snd

fileLog :: Log -> IO ()
fileLog = serialize >>= flip (appendFile . logStore . fst)

pushQueueN :: Int -> Seq a -> Seq a -> Seq a
pushQueueN count = (\new old -> new >< S.take count old)

stateLog :: Log -> Bot ()
stateLog (from, entries) = do
    count <- read <$> param "history-length"
    history %= insertWith (pushQueueN count) from (singleton entries)

log :: Log -> Bot ()
log = uncurry (*>) . (liftIO . fileLog &&& stateLog)

logs :: UnicodeEvent -> Bot ()
logs ev = do
    strEv <- return . fmap T.unpack $ ev
    (msg, (logName, from)) <- return . msgsrc $ strEv

    time <- liftIO $ getUnixTime >>= fmap B.unpack . formatUnixTime "%F %T"
    log (logName, [time, from, msg])

    where
        msg (Privmsg _from msg) = either (const "") id msg
        source (User user) = (user, user)
        source (Channel chan user) = (chan, user)

        msgsrc = msg . _message &&& source . _source
