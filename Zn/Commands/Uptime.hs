module Zn.Commands.Uptime where

import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.List
import Data.Time
import Network.IRC.Client
import Zn.Bot

-- from Xn's source
-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: NominalDiffTime -> String
pretty ntd = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
        sectime = floor . toRational
        secs    = sectime ntd     ; mins   = secs   `div` 60
        hours   = mins   `div` 60 ; days   = hours  `div` 24
        months  = days   `div` 28 ; years  = months `div` 12
        f (i,s) | i == 0    = []
                | otherwise = show i ++ s

uptime :: [String] -> Bot String
uptime _ = fmap pretty $
    diffUTCTime
        <$> liftIO getCurrentTime
        <*> atomState (use bootTime)
