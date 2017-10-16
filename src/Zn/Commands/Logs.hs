{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Zn.Commands.Logs
    ( Log
    , Logs
    , logs
    , logsFor
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
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (TimeZone, localTimeToUTC, getCurrentTimeZone)
import Data.Time.Parse (strptime)
import Data.UnixTime
import Network.IRC.Client
import Prelude hiding (log, take)
import System.IO.Unsafe (unsafePerformIO)
import Zn.Bot
import Zn.Types

ircParseTime :: TimeZone -> Text -> POSIXTime
ircParseTime tz = posixtime . localtime
    where
        posixtime = utcTimeToPOSIXSeconds . localTimeToUTC tz
        localtime = fst . fromJust . strptime "%F %T"

logTime :: TimeZone -> Line Text -> POSIXTime
logTime tz = ircParseTime tz . view date

logWithin :: TimeZone -> POSIXTime -> Integer -> Line Text -> Bool
logWithin tz now delta log = now - (logTime tz log) <= fromInteger delta

restrictKeysCI :: M.Map Text a -> Set.Set Text -> M.Map Text a
restrictKeysCI m s = M.filterWithKey (\k _ -> CI.mk k `Set.member` Set.map CI.mk s) m

logTail :: POSIXTime -> M.Map Text (Seq.Seq (Line Text)) -> M.Map Text (Seq.Seq (Line Text))
logTail start = M.map (Seq.filter $ logWithin tz start 10)
    where tz = unsafePerformIO getCurrentTimeZone

logsFrom :: [Text] -> M.Map Text a -> M.Map Text a
logsFrom nicks = flip restrictKeysCI (Set.fromList nicks)

logFrom :: Ord a => a -> M.Map a b -> b
logFrom = flip (M.!) -- BUG: this is case insensitive

logMap :: (Ord a, Ord b, Functor f) => (a -> b) -> M.Map a (Seq.Seq (f a)) -> M.Map b (Seq.Seq (f b))
logMap f = M.map (fmap $ fmap f) . M.mapKeys f

--

serialize :: Log -> Text
serialize (from, line) = T.intercalate "\t" items <> "\n"
    where items = map (`view` line) $ [date, author, text]

fileLog :: Log -> IO ()
fileLog = serialize >>= flip (TIO.appendFile . logStore . T.unpack . fst)

pushQueueN :: Int -> Seq.Seq a -> Seq.Seq a -> Seq.Seq a
pushQueueN count = (\new old -> new Seq.>< Seq.take count old)

stateLog :: Log -> Bot ()
stateLog (from, entries) = do
    count <- read . T.unpack <$> param "history-length"
    history %= insertWith (pushQueueN count) from (Seq.singleton entries)

logBranch :: Log -> Bot ()
logBranch = uncurry (*>) . (liftIO . fileLog &&& stateLog)

logSource (User user) = (user, user)
logSource (Channel chan user) = (chan, user)

-- exported
logs :: PrivEvent Text -> Bot ()
logs msg = do
    nick <- Bot getNick

    when (User nick /= view src msg) $
        logProcess =<< (logSource . view src) $ msg

-- exported
logsFor :: Text -> PrivEvent Text -> Bot ()
logsFor logName ev = logProcess ((_2 .~ logName) . logSource . view src $ ev) ev

logProcess :: (Text, Text) -> PrivEvent Text -> Bot ()
logProcess (logName, from) ev = do
    time <- liftIO $ getUnixTime >>= fmap B.unpack . formatUnixTime "%F %T"
    logBranch (logName, Line (T.pack time) from (view cont ev))
