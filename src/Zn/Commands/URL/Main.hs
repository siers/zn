{-# LANGUAGE OverloadedStrings #-}
module Zn.Commands.URL.Main
    ( url
    , format
    , parseTitle
    ) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Text (Text, pack, unpack, strip)
import Data.Tuple
import Network.HTTP.Client
import Prelude hiding (concat)
import Text.Regex.TDFA
import Zn.Commands.URL.Format
import Zn.Commands.URL.Store
import Zn.Commands.URL.Types
import Zn.IRC
import Zn.TLS
import Zn.Types

userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.87 Safari/537.36"

request :: String -> IO BodyHeaders
request url = do
    (req, man) <- (,) . addHeaders . addTimeout <$> parseUrlThrow url <*> mkHttpManager True

    withResponse req man $
        fmap swap . sequence . (responseHeaders &&& getLittle)

    where
        addHeaders r = r { requestHeaders = [("User-Agent", userAgent)] }
        addTimeout r = r { responseTimeout = responseTimeoutMicro $ 1000000 * 3 }

        getLittle :: Response BodyReader -> IO BL.ByteString
        getLittle res = brReadSome (responseBody res) (2^22) <* responseClose res

process :: PrivEvent Text -> String -> Bot ()
process pr url = do
    resp <- liftIO (request url)

    store pr url resp
    reply pr (strip . pack . format $ resp)

retry :: Bot () -> Bot ()
retry = recovering (limitRetries 3) [return $ Handler handler] . return
    where
        handler = return (return True) :: HttpException -> Bot Bool

link :: String -> [String]
link msg = nub . map head $ msg =~ ("https?://[^ ]+" :: String)

url :: PrivEvent Text -> Bot ()
url pr = (retry . process pr) `mapM_` (link . unpack . view cont $ pr)
