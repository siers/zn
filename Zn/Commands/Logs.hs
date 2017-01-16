module Zn.Commands.Logs
    ( Log
    , logs
    , logTail
    , logFrom
    , logsFrom
    )
where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive as CI (mk)
import Data.List (intersperse, concat)
import Data.Map (Map, insertWith)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (TimeZone, localTimeToUTC, getCurrentTimeZone)
import Data.Time.Parse (strptime)
import Data.UnixTime
import Network.IRC.Client
import Prelude hiding (log, take)
import System.IO.Unsafe (unsafePerformIO)
import Zn.Bot
import Zn.Data.Ini

type Log = (String, [String])
-- `uncurry logger $ log' must typecheck

logTime :: TimeZone -> [String] -> POSIXTime
logTime tz = posixtime . localtime
    where
        posixtime = utcTimeToPOSIXSeconds . localTimeToUTC tz
        localtime = fst . fromJust . strptime "%F %T" . \(time:_) -> time

logWithin :: TimeZone -> POSIXTime -> Integer -> [String] -> Bool
logWithin tz now delta log = now - (logTime tz log) <= fromInteger delta

restrictKeysCI :: M.Map String a -> Set.Set String -> M.Map String a
restrictKeysCI m s = M.filterWithKey (\k _ -> CI.mk k `Set.member` Set.map CI.mk s) m

logTail :: POSIXTime -> M.Map String (Seq.Seq [String]) -> M.Map String (Seq.Seq [String])
logTail start = M.map (Seq.filter $ logWithin tz start 10)
    where tz = unsafePerformIO getCurrentTimeZone

logsFrom :: [String] -> M.Map String a -> M.Map String a
logsFrom nicks = flip restrictKeysCI (Set.fromList nicks)

logFrom :: String -> M.Map String a -> a
logFrom name = (M.! name) . logsFrom [name]

--

serialize :: Log -> String
serialize = (++ "\n") . concat . intersperse "\t" . snd

fileLog :: Log -> IO ()
fileLog = serialize >>= flip (appendFile . logStore . fst)

pushQueueN :: Int -> Seq.Seq a -> Seq.Seq a -> Seq.Seq a
pushQueueN count = (\new old -> new Seq.>< Seq.take count old)

stateLog :: Log -> Bot ()
stateLog (from, entries) = do
    count <- read <$> param "history-length"
    history %= insertWith (pushQueueN count) from (Seq.singleton entries)

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
