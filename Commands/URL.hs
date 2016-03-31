module Commands.URL where

import Bot
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import Data.Maybe
import Data.Text as T hiding (take, drop, intersperse)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.IRC.Client
import Prelude hiding (concat)
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

{- title :: String -> IO String
title = fmap (bundle . extract . makeDocument) . urlSummary
    where
        bundle = take 150 . T.unpack . mconcat
        extract html = fromDocument html $// element "title" &// content
        makeDocument = parseText_ def . TL.pack . BS.unpack -}

title :: String -> IO (Maybe String)
title = fmap (fmap prepare . extract . TL.unpack . decodeUtf8With substInvalid) . urlSummary
    where
        substInvalid = return (const (Just ' '))
        prepare = take 150 . drop 7
        extract html = (html :: String) =~~ ("<title>[^\r\n<]+" :: String) :: Maybe String

announce :: UnicodeEvent -> Text -> Bot ()
announce ev what = reply ev . joinprep =<< liftIO (title $ unpack what)
    where joinprep = concat . intersperse "\n" . fmap pack . maybeToList

maybeWhen = maybe (return ())

url :: UnicodeEvent -> Bot ()
url ev = (announce ev . pack) `maybeWhen` (link . unpack . privtext $ _message ev)
