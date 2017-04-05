module Zn.Commands.URL where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Data.Text (Text)
import Network.HTTP.Client
import Prelude hiding (concat)
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Zn.Bot
import Zn.Command
import Zn.IRC
import Zn.TLS

userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.87 Safari/537.36"

getLittle :: Response BodyReader -> IO BL.ByteString
getLittle res = brReadSome (responseBody res) (2^15) <* responseClose res

urlSummary :: String -> IO BL.ByteString
urlSummary url = do
    (req, man) <- (,) . addHeaders . addTimeout <$> parseUrlThrow url <*> mkHttpManager True
    withResponse req man getLittle
    where
        addHeaders r = r { requestHeaders = [("User-Agent", userAgent)] }
        addTimeout r = r { responseTimeout = responseTimeoutMicro $ 1000000 * 3 }

link :: String -> Maybe String
link msg = msg =~~ ("https?://[^ ]+" :: String)

textify :: [Tag String] -> String
textify = filter (\c -> not $ c `elem` ("\r\n" :: String)) . concatMap text
    where
        text (TagText s) = s
        text _           = ""

xnEvalSafe :: String -> String
xnEvalSafe = map (\x -> if x == '>' then 'ᐳ' else x)

title :: String -> IO (Maybe String)
title = fmap (ifAny prepare . extract . TL.unpack . decodeUtf8With substInvalid) . urlSummary
    where
        substInvalid = return (const (Just ' '))
        ifAny f l = if l == [] then Nothing else Just (f l)
        prepare = xnEvalSafe . take 1000 . textify -- it gets cut smaller, still
        dropT = dropWhile (not . isTagOpenName "title")
        takeT = takeWhile (not . isTagCloseName "title")
        extract = takeT . dropT . parseTags

announce :: PrivEvent Text -> Text -> Bot ()
announce pr what = reply pr . T.strip . joinprep =<< liftIO (title $ T.unpack what)
    where joinprep = T.concat . fmap T.pack . maybeToList

maybeWhen = maybe (return ())

url_ :: PrivEvent Text -> Bot ()
url_ pr = (announce pr . T.pack) `maybeWhen` (link . T.unpack . view cont $ pr)

url :: PrivEvent Text -> Bot ()
url pr = recovering (limitRetries 3) [return $ Handler handler] (return $ url_ pr)
    where handler = return (return True) :: HttpException -> Bot Bool
