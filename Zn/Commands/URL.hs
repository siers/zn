module Zn.Commands.URL where

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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.IRC.Client
import Prelude hiding (concat)
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Zn.Bot

userAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:30.0) Gecko/20100101 Firefox/30.0"

getLittle :: Response BodyReader -> IO BL.ByteString
getLittle res = brReadSome (responseBody res) (2^15) <* responseClose res

urlSummary :: String -> IO BL.ByteString
urlSummary url = do
    (req, man) <- (,) . addHeaders <$> parseUrl url <*> newManager tlsManagerSettings
    withResponse req man getLittle
    where addHeaders r = r { requestHeaders = [("User-Agent", userAgent)] }

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

announce :: UnicodeEvent -> T.Text -> Bot ()
announce ev what = reply ev . joinprep =<< liftIO (title $ T.unpack what)
    where joinprep = T.concat . intersperse "\n" . fmap T.pack . maybeToList

maybeWhen = maybe (return ())

url_ :: UnicodeEvent -> Bot ()
url_ ev = (announce ev . T.pack) `maybeWhen` (link . T.unpack . privtext $ _message ev)

url :: UnicodeEvent -> Bot ()
url ev = recovering (limitRetries 3) [return $ Handler handler] (return $ url_ ev)
    where handler = return (return True) :: HttpException -> Bot Bool