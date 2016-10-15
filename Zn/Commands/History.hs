module Zn.Commands.History where

import Control.Monad
import Control.Monad.IO.Class
import Data.Ini
import Data.Text as T (unpack)
import Network.IRC.Client
import Zn.Bot

history :: [String] -> Bot String
history _ = unpack . flip setting "history" . config <$> getTVar stateTVar

lastMsgs :: [String] -> Bot String
lastMsgs a = do
    file <- readFile . unpack . flip setting "logfile" . config <$> getTVar stateTVar
    liftIO $ show . take (read $ concat a :: Int) . reverse . lines <$> file
