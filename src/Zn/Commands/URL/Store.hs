{-# LANGUAGE OverloadedStrings #-}
module Zn.Commands.URL.Store
    ( storePath
    , store
    ) where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, unpack)
import Data.UnixTime
import Network.IRC.Client as IRC
import Text.Printf
import Zn.Commands.URL.Types
import Zn.Types

-- both branches are distinguishable via regex
originPathSection :: Source String -> String
originPathSection (Channel c u) = c ++ "-" ++ u
originPathSection (User u) = u

linkPathSection :: String -> String
linkPathSection = filter (flip elem [' '..'~'])

storePath :: PrivEvent Text -> String -> IO String
storePath pr url = do
    time <- getUnixTime >>= fmap B.unpack . formatUnixTime "%F-%T"

    return . downStore $ printf
        "%s-%s:%s"
        time
        (originPathSection . fmap unpack $ view src pr)
        (linkPathSection url)

store :: PrivEvent Text -> String -> BodyHeaders -> Bot ()
store pr url (body, headers) = liftIO $ do
    path <- storePath pr url
    BL.writeFile path body
