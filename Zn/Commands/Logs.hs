module Zn.Commands.Logs
    ( Log
    , logs
    , logTail
    , logFrom
    , logsFrom
    , logMap
    )
where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI (mk)
import Data.Map (insertWith)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (TimeZone, localTimeToUTC, getCurrentTimeZone)
import Data.Time.Parse (strptime)
import Data.UnixTime
import Network.IRC.Client
import Prelude hiding (log, take)
import System.IO.Unsafe (unsafePerformIO)
import Zn.Bot

type Log = (Text, [Text])
-- `uncurry logger $ log' must typecheck

logTime :: TimeZone -> [Text] -> POSIXTime
logTime tz = posixtime . localtime
    where
        posixtime = utcTimeToPOSIXSeconds . localTimeToUTC tz
        localtime = fst . fromJust . strptime "%F %T" . \(time:_) -> time

logWithin :: TimeZone -> POSIXTime -> Integer -> [Text] -> Bool
logWithin tz now delta log = now - (logTime tz log) <= fromInteger delta

restrictKeysCI :: M.Map Text a -> Set.Set Text -> M.Map Text a
restrictKeysCI m s = M.filterWithKey (\k _ -> CI.mk k `Set.member` Set.map CI.mk s) m

logTail :: POSIXTime -> M.Map Text (Seq.Seq [Text]) -> M.Map Text (Seq.Seq [Text])
logTail start = M.map (Seq.filter $ logWithin tz start 10)
    where tz = unsafePerformIO getCurrentTimeZone

logsFrom :: [Text] -> M.Map Text a -> M.Map Text a
logsFrom nicks = flip restrictKeysCI (Set.fromList nicks)

logFrom :: Ord a => a -> M.Map a b -> b
logFrom = flip (M.!) -- BUG: this is case insensitive

logMap :: (Ord a, Ord b) => (a -> b) -> M.Map a (Seq.Seq [a]) -> M.Map b (Seq.Seq [b])
logMap f = M.map (fmap $ fmap f) . M.mapKeys f

--

serialize :: Log -> Text
serialize = (`T.append` "\n") . T.intercalate "\t" . snd

fileLog :: Log -> IO ()
fileLog = serialize >>= flip (TIO.appendFile . logStore . T.unpack . fst)

pushQueueN :: Int -> Seq.Seq a -> Seq.Seq a -> Seq.Seq a
pushQueueN count = (\new old -> new Seq.>< Seq.take count old)

stateLog :: Log -> Bot ()
stateLog (from, entries) = do
    count <- read . T.unpack <$> param "history-length"
    history %= insertWith (pushQueueN count) from (Seq.singleton entries)

log :: Log -> Bot ()
log = uncurry (*>) . (liftIO . fileLog &&& stateLog)

logs :: UnicodeEvent -> Bot ()
logs ev = do
    let (msg, (logName, from)) = msgsrc ev

    time <- liftIO $ getUnixTime >>= fmap B.unpack . formatUnixTime "%F %T"
    log (logName, [T.pack time, from, msg])

    where
        msg (Privmsg _from msg) = either (const "") id msg
        source (User user) = (user, user)
        source (Channel chan user) = (chan, user)

        msgsrc = msg . _message &&& source . _source
