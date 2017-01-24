{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Mping (mping) where

import Control.Lens
import Control.Monad.IO.Class
import Data.CaseInsensitive as CI (mk)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Text (unpack, pack, Text)
import Data.Time.Clock.POSIX
import Network.IRC.Client
import Text.Printf
import Zn.Bot
import Zn.Commands.Logs

successes :: (M.Map Text (Seq.Seq [Text])) -> [Text]
successes = M.keys . M.filter (elem "pong" . fmap (!! 2))

ping :: Text -> StatefulBot ()
ping nick = send $ Privmsg nick (Right "!ping")

pongResult :: [Text] -> Text -> Text
pongResult successes bot = (`T.append` status) $ bot `maybe` (successes !!) $ ponged
    where
        ponged = CI.mk bot `L.elemIndex` fmap CI.mk successes
        status = maybe " [-]" (const " [+]") ponged

mping :: Bot Text
mping = do
    myself <- _nick <$> Bot instanceConfig
    bots <- L.nub . (myself :) . T.splitOn "," <$> param "bots"

    mapM_ (Bot . ping) $ bots
    start <- liftIO getPOSIXTime

    sleep (1 + length bots * 1)

    pongs <- successes . logTail start . logsFrom bots <$> use history

    return . format . join . map (pongResult pongs) $ bots

    where
        join = T.intercalate ", "
        format = pack . printf "pongs from: %s" . unpack
