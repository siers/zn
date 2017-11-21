{-# LANGUAGE OverloadedStrings #-}
module Zn.Commands.URL.Store
    ( storeName
    , storePrefix
    , store
    ) where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Text (Text, unpack)
import Data.UnixTime
import Network.IRC.Client as IRC
import System.FilePath.Posix
import Text.Printf
import Zn.Bot.Request
import Zn.Types

-- both branches are distinguishable via regex
originPathSection :: Source String -> String
originPathSection (Channel c u) = c ++ "-" ++ u
originPathSection (User u) = u

linkPathSection :: String -> String
linkPathSection = filter (flip elem [' '..'~'])

storeName :: Maybe String -> PrivEvent Text -> String -> IO String
storeName prefix pr url = do
    time <- getUnixTime >>= fmap B.unpack . formatUnixTime "%F-%T"

    return $
        printf
            "%s-%s:%s"
            time
            (originPathSection . fmap unpack $ view src pr)
            (linkPathSection url)
        `fromMaybe`
        (printf "%s-%s" time <$> prefix)

storePrefix :: Maybe String -> PrivEvent Text -> String -> BodyHeaders -> Bot String
storePrefix prefix pr url (body, _headers) = liftIO $ do
    path <- downStore <$> storeName prefix pr url
    takeFileName path <$ BL.writeFile path body

store = storePrefix Nothing
