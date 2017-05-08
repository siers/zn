module Zn.Commands.Uptime where

import Control.Lens
import Control.Monad.RWS
import qualified Data.Text as T
import Data.Text (pack, Text)
import Data.Time
import Zn.Types

-- from Xn's source
-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: NominalDiffTime -> Text
pretty ntd = T.intercalate " " . filter (not . T.null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
        sectime = floor . toRational
        secs    = sectime ntd     ; mins   = secs   `div` 60
        hours   = mins   `div` 60 ; days   = hours  `div` 24
        months  = days   `div` 28 ; years  = months `div` 12
        f (i,s) | i == 0    = T.empty
                | otherwise = pack $ show i ++ s

uptime :: Bot Text
uptime = fmap pretty $
    diffUTCTime
        <$> liftIO getCurrentTime
        <*> use bootTime
