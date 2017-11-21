module Zn.Bot.Request
    ( request
    , BodyHeaders
    ) where

import Control.Arrow
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.Tuple
import Network.HTTP.Client
import Network.HTTP.Types
import Zn.TLS

type BodyHeaders = (BL.ByteString, ResponseHeaders)

userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.87 Safari/537.36"

request :: MonadIO m => String -> m BodyHeaders
request url = liftIO $ do
    (req, man) <- (,) . addHeaders . addTimeout <$> parseUrlThrow url <*> mkHttpManager True

    withResponse req man $
        fmap swap . sequence . (responseHeaders &&& getLittle)

    where
        addHeaders r = r { requestHeaders = [("User-Agent", userAgent)] }
        addTimeout r = r { responseTimeout = responseTimeoutMicro $ 1000000 * 3 }

        getLittle :: Response BodyReader -> IO BL.ByteString
        getLittle res = brReadSome (responseBody res) (2^22) <* responseClose res
