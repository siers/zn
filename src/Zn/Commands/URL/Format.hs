{-# LANGUAGE OverloadedStrings #-}
module Zn.Commands.URL.Format where

import Control.Monad
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
import Hledger.Utils.Regex (regexMatchesCI)
import Network.HTTP.Types
import Prelude hiding (concat)
import Text.HTML.TagSoup (Tag (..), parseTags, isTagOpen, isTagClose)
import Text.Printf

tagName :: Tag String -> Maybe String
tagName (TagOpen n _) = Just n
tagName (TagClose n) = Just n
tagName _ = Nothing

tagNameRegCI :: Tag String -> String -> Bool
tagNameRegCI t s = regexMatchesCI s $ fromMaybe "" (tagName t)

textify :: [Tag String] -> String
textify = filter (\c -> not $ c `elem` ("\r\n" :: String)) . concatMap text
    where
        text (TagText s) = s
        text _           = ""

parseTitle :: String -> Maybe String
parseTitle = ifAny prepare . extract
    where
        ifAny f l = if l == [] then Nothing else Just (f l)
        prepare = take 1000 . textify -- it gets cut smaller, still
        dropCond t = not $ tagNameRegCI t "^title$" && isTagOpen t
        takeCond t = not $ tagNameRegCI t "^title$" && isTagClose t
        extract = takeWhile takeCond . dropWhile dropCond . parseTags

formatNSFW :: String -> Maybe String
formatNSFW score =
    printf "NSFW: %.4f%%" <$>
        if percent > 10 then Just percent else Nothing
    where
        percent = (* 100) . (read :: String -> Double) $ score

formatContentType :: ResponseHeaders -> [String]
formatContentType rHeaders =
    removeEncoding . fmap bseUnpack . maybeToList $
        CI.mk "Content-Type" `M.lookup` M.fromList rHeaders
    where
        removeEncoding = fmap (\c -> splitOn ";" c !! 0)
        bseUnpack = T.unpack . BSE.decodeUtf8With substInvalid
        substInvalid = return (const (Just ' '))

format :: (BL.ByteString, ResponseHeaders) -> Maybe String -> String
format (body, rHeaders) nsfw =
    concat .
    (["¬ "] ++) .
    intersperse " · " .
    map (printf "%s") .
    filter (not . null) $
        [title]
        ++
        (formatContentType rHeaders \\ ["text/html"])
        ++
        maybeToList (nsfw >>= formatNSFW)

    where
        title = concat . map trim . maybeToList . parseTitle . bleUnpack $ body
        trim = T.unpack . T.strip . T.pack
        bleUnpack = TL.unpack . BLE.decodeUtf8With substInvalid
        substInvalid = return (const (Just ' '))
