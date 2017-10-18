{-# LANGUAGE OverloadedStrings #-}
module Zn.Commands.URL.Store
    ( storeName
    , store
    ) where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, unpack)
import Data.UnixTime
import Network.IRC.Client as IRC
import System.FilePath.Posix
import Text.Printf
import Zn.Commands.URL.Types
import Zn.Types

-- both branches are distinguishable via regex
originPathSection :: Source String -> String
originPathSection (Channel c u) = c ++ "-" ++ u
originPathSection (User u) = u

linkPathSection :: String -> String
linkPathSection = filter (flip elem [' '..'~'])

storeName :: PrivEvent Text -> String -> IO String
storeName pr url = do
    time <- getUnixTime >>= fmap B.unpack . formatUnixTime "%F-%T"

    return $ printf
        "%s-%s:%s"
        time
        (originPathSection . fmap unpack $ view src pr)
        (linkPathSection url)

store :: PrivEvent Text -> String -> BodyHeaders -> Bot String
store pr url (body, headers) = liftIO $ do
    path <- downStore <$> storeName pr url
    takeFileName path <$ BL.writeFile path body
