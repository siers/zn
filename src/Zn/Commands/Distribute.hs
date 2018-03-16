{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Distribute
    ( distribute
    , botcast
    , botnicks ) where

import Control.Arrow
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Text.Regex.TDFA
import Network.IRC.Client
import Zn.Bot
import Zn.Commands.Logs
import Zn.IRC
import Zn.Types

type DistMsg = (Text, [Text])

format :: [DistMsg] -> Text
format = T.intercalate ", " . map (TL.toStrict . TF.format "{}[{}]" . second joinCmds)

mbyUptimeSort :: Text -> [DistMsg] -> [DistMsg]
mbyUptimeSort payload =
    if isNothing $ ((unpack payload) =~~ ("^!?uptime\\s*$" :: String) :: Maybe String)
    then id
    else reverse . L.sortBy (curry $ strNumOrd . (times *** times))
    where
        strNumOrd (a, b) = compare (length a) (length b) <> compare a b
        times :: DistMsg -> [Integer]
        times = map (read . unpack) . T.words . T.filter (`elem` (" 1234567890" :: [Char])) . head . snd

amass :: History Text -> [DistMsg]
amass =
    map (second $ fmap (strip . view text) . toList) .
    filter (not . null . snd) .
    M.toList

transfer :: Text -> Text -> StatefulBot ()
transfer payload nick = send . Privmsg nick . Right $ payload

distribute :: Text -> [Text] -> Bot (History Text)
distribute payload users = do
    mapM_ (Bot . transfer payload) $ users

    start <- liftIO getPOSIXTime
    sleep $ 1 + length users * 1

    logsFrom users . logTail start <$> use history

botnicks :: Bot [Text]
botnicks = do
    myself <- Bot $ getNick
    L.nub . (++ [myself]) . T.splitOn "," <$> param "bots"

botcast :: Text -> Bot Text
botcast payload =
    lock "botcast" $ do
        format . mbyUptimeSort payload . amass <$> (distribute payload =<< botnicks)
