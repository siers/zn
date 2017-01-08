module Zn.Commands.Mping (mping) where

import Control.Lens
import Control.Monad.IO.Class
import Data.CaseInsensitive as CI (mk)
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (unpack, pack)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Time.Parse
import Network.IRC.Client
import Text.Printf
import Zn.Bot
import Zn.Commands.Logs
import Zn.Data.Ini
import Data.Function (fix)

logTime :: TimeZone -> [String] -> POSIXTime
logTime tz = posixtime . localtime
    where
        posixtime = utcTimeToPOSIXSeconds . localTimeToUTC tz
        localtime = fst . fromJust . strptime "%F %T" . \(time:_) -> time

logWithin :: TimeZone -> POSIXTime -> Integer -> [String] -> Bool
logWithin tz now delta log = now - (logTime tz log) <= fromInteger delta

restrictKeysCI :: M.Map String a -> Set.Set String -> M.Map String a
restrictKeysCI m s = M.filterWithKey (\k _ -> CI.mk k `Set.member` Set.map CI.mk s) m

replies :: POSIXTime -> [String] -> Bot (M.Map String (Seq.Seq [String]))
replies start bots = do
    tz <- liftIO getCurrentTimeZone
    pongs <- uses history $ flip restrictKeysCI (Set.fromList bots)
    return $ M.map (Seq.filter $ logWithin tz start 10) pongs

successes :: (M.Map String (Seq.Seq [String])) -> [String]
successes = M.keys . M.filter (elem "pong" . fmap (!! 2))

ping :: String -> StatefulBot ()
ping nick = send $ Privmsg (pack nick) (Right "!ping")

pongResult :: [String] -> String -> String
pongResult successes bot = (++ status) $ bot `maybe` (successes !!) $ ponged
    where
        ponged = CI.mk bot `L.elemIndex` fmap CI.mk successes
        status = maybe " [-]" (const " [+]") ponged

mping :: Bot String
mping = do
    myself <- unpack . _nick <$> Bot instanceConfig
    bots <- L.nub . (myself :) . splitOn "," <$> param "bots"

    mapM_ (Bot . ping) $ bots
    start <- liftIO getPOSIXTime

    sleep (1 + length bots * 1)

    pongs <- successes <$> replies start bots

    return . printf "pongs from: %s" . foldr (++) "" . L.intersperse ", " . map (pongResult pongs) $ bots
