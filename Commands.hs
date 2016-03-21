{-# language OverloadedStrings #-}

module Commands where

import Bot
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Encoding
import Data.Encoding.UTF8
import Data.Maybe
import Data.Text as T hiding (take, drop)
import qualified Data.Text.Lazy as TL hiding (take)
import Network.HTTP.Client
import Network.IRC.Client
import Network.IRC.Client.Types
import Network.IRC.Conduit
import Text.Regex.TDFA
import Text.XML
import Text.XML.Cursor (content, element, fromDocument, ($//), (&//))

getLittle :: Response BodyReader -> IO BS.ByteString
getLittle res = brRead $ responseBody res <* responseClose res

urlSummary :: String -> IO BS.ByteString
urlSummary url = do
    (req, man) <- (,) <$> parseUrl url <*> newManager defaultManagerSettings
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
title = fmap (fmap prepare . extract . decodeStrictByteString UTF8) . urlSummary
    where
        prepare = take 150 . drop 7
        extract html = (html :: String) =~~ ("<title>[^\r\n<]+" :: String) :: Maybe String

announce :: Text -> Text -> Bot ()
announce to what = mapM_ push . maybeToList =<< liftIO (title $ unpack what)
    where push = send . Privmsg to . Right . pack

cmdHandler :: UnicodeEvent -> Bot ()
cmdHandler ev =
    if from (_source ev) == "Xn" then return ()
    else void . maybe (return ()) handle . decide $ ev
        where
            handle = announce . target . _source $ ev
            decide = fmap pack . link . unpack . privtext . _message
