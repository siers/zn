{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.URL.Format where

import Control.Monad
import Data.Foldable (fold)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text.Encoding as BSE
import Data.Text.Lazy.Encoding as BLE
import Hledger.Utils.Regex (regexMatchesCI)
import Network.HTTP.Types
import Prelude hiding (concat)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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
  printf "NSFW: %.4f%%" <$> if percent > 35 then Just percent else Nothing
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

format :: (BL.ByteString, ResponseHeaders) -> Maybe String -> Maybe String
format (body, headers) nsfw =
  if not (null title) || not (null nsfw)
  then Just (concat components)
  else Nothing
  where
    substInvalid = return (const (Just ' '))
    bleUnpack = TL.unpack . BLE.decodeUtf8With substInvalid

    badTitles = ["^$", "pirms.*izmantot.*pakalpojumu", "before.*continue.*YouTube"]
    goodTitle t = not (any (flip regexMatchesCI t) badTitles)

    trim = T.unpack . T.strip . T.pack

    title = filter goodTitle . return . concat . map trim . maybeToList . parseTitle . bleUnpack $ body

    components = (["¬ "] ++ ) . intersperse " · " $ fold
      [ title
      , (formatContentType headers \\ ["text/html"])
      , maybeToList (nsfw >>= formatNSFW) ]
