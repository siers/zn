module Commands.URL where

import Bot
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.IRC.Client
import Prelude hiding (concat)
import Safe
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Text.XML

getLittle :: Response BodyReader -> IO BL.ByteString
getLittle res = brReadSome (responseBody res) (2^15) <* responseClose res

urlSummary :: String -> IO BL.ByteString
urlSummary url = do
    (req, man) <- (,) <$> parseUrl url <*> newManager tlsManagerSettings
    withResponse req man getLittle

link :: String -> Maybe String
link msg = msg =~~ ("https?://[^ ]+" :: String)

unescape :: String -> String
unescape = maybe "" id . headMay . concatMap text . parseTags
    where
        text (TagText s) = [s]
        text _           = []

title :: String -> IO (Maybe String)
title = fmap (fmap prepare . extract . TL.unpack . decodeUtf8With substInvalid) . urlSummary
    where
        substInvalid = return (const (Just ' '))
        prepare = unescape . take 150 . drop 7
        extract html = (html :: String) =~~ ("<title>[^\r\n<]+" :: String) :: Maybe String

announce :: UnicodeEvent -> T.Text -> Bot ()
announce ev what = reply ev . joinprep =<< liftIO (title $ T.unpack what)
    where joinprep = T.concat . intersperse "\n" . fmap T.pack . maybeToList

maybeWhen = maybe (return ())

url_ :: UnicodeEvent -> Bot ()
url_ ev = (announce ev . T.pack) `maybeWhen` (link . T.unpack . privtext $ _message ev)

url :: UnicodeEvent -> Bot ()
url ev = recovering (limitRetries 3) [return $ Handler handler] (return $ url_ ev)
    where handler = return (return True) :: HttpException -> Bot Bool
