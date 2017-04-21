{-# LANGUAGE OverloadedStrings #-}
module Zn.Commands.URL where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding as BSE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as BLE
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Types
import Prelude hiding (concat)
import Text.HTML.TagSoup
import Text.Printf
import Text.Regex.TDFA
import Zn.Bot
import Zn.Command
import Zn.IRC
import Zn.TLS

userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.87 Safari/537.36"

parseTitle :: String -> Maybe String
parseTitle = ifAny prepare . extract
    where
        ifAny f l = if l == [] then Nothing else Just (f l)
        prepare = take 1000 . textify -- it gets cut smaller, still
        dropT = dropWhile (not . isTagOpenName "title")
        takeT = takeWhile (not . isTagCloseName "title")
        extract = takeT . dropT . parseTags

format :: (BL.ByteString, ResponseHeaders) -> String
format (body, rHeaders) =
    concat .
    (["¬ "] ++) .
    intersperse " · " .
    map (printf "%s") .
    filter (not . null) $
        [title] ++ (removeEncoding contentType \\ ["text/html"])

    where
        title = concat . maybeToList . parseTitle . bleUnpack $ body
        joinPrep = T.concat . fmap T.pack . maybeToList

        removeEncoding = fmap (\c -> splitOn ";" c !! 0)
        contentType = fmap bseUnpack . maybeToList $
            CI.mk "Content-Type" `M.lookup` M.fromList rHeaders

        bleUnpack = TL.unpack . BLE.decodeUtf8With substInvalid
        bseUnpack = T.unpack . BSE.decodeUtf8With substInvalid
        substInvalid = return (const (Just ' '))

urlSummary :: String -> IO String
urlSummary url = do
    (req, man) <- (,) . addHeaders . addTimeout <$> parseUrlThrow url <*> mkHttpManager True

    withResponse req man $ \req -> do
        body <- getLittle req
        return . format $ (body, responseHeaders req)

    where
        addHeaders r = r { requestHeaders = [("User-Agent", userAgent)] }
        addTimeout r = r { responseTimeout = responseTimeoutMicro $ 1000000 * 3 }

        getLittle :: Response BodyReader -> IO BL.ByteString
        getLittle res = brReadSome (responseBody res) (2^22) <* responseClose res

textify :: [Tag String] -> String
textify = filter (\c -> not $ c `elem` ("\r\n" :: String)) . concatMap text
    where
        text (TagText s) = s
        text _           = ""

announce :: PrivEvent Text -> String -> Bot ()
announce pr what = reply pr . T.strip . T.pack =<< liftIO (urlSummary what)

retry :: Bot () -> Bot ()
retry = recovering (limitRetries 3) [return $ Handler handler] . return
    where
        handler = return (return True) :: HttpException -> Bot Bool

link :: String -> [String]
link msg = map head $ msg =~ ("https?://[^ ]+" :: String)

url :: PrivEvent Text -> Bot ()
url pr = (retry . announce pr) `mapM_` (link . T.unpack . view cont $ pr)
