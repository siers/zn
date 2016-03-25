{-# language OverloadedStrings #-}

module Commands.URL where

import Bot
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.Encoding
import Data.Encoding.UTF8
import Data.Maybe
import Data.Text as T hiding (take, drop)
import qualified Data.Text.Lazy as TL hiding (take)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.IRC.Client
import Network.IRC.Client.Types
import Network.IRC.Conduit
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
title = fmap (fmap prepare . extract . decodeLazyByteString UTF8) . urlSummary
    where
        prepare = take 150 . drop 7
        extract html = (html :: String) =~~ ("<title>[^\r\n<]+" :: String) :: Maybe String

announce :: Text -> Text -> Bot ()
announce to what = mapM_ push . maybeToList =<< liftIO (title $ unpack what)
    where push = send . Privmsg to . Right . pack

maybedo = maybe (return ())

url :: UnicodeEvent -> Bot ()
url ev = (announce . target $ _source ev) `maybedo` (fmap pack . link . unpack $ command ev)
