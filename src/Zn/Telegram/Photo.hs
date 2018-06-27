{-# LANGUAGE OverloadedStrings #-}

module Zn.Telegram.Photo where

import qualified Data.Binary.Builder as B
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Text.ICU
import Data.Text.ICU.Char
import Data.Text.ICU.Replace
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Network.HTTP.Types.URI
import Text.Printf
import Web.Telegram.API.Bot as T (User(..))
import Zn.Bot
import Zn.Commands.URL.Detect
import Zn.Commands.URL.Format
import Zn.Commands.URL.Main
import Zn.Telegram.Types
import Zn.Types

-- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell
canonicalForm :: Text -> Text
canonicalForm = T.filter (not . property Diacritic) . normalize NFD

store :: PrivEvent Text -> UpdateSummary PhotoMsg a -> Bot String
store pr (uid, (User { user_first_name = who }), (ZnPhoto (caption, link))) = fst <$> download pathslug pr link
  where
    limitChar = T.dropAround (== '-') . replaceAll (regex [] "[^a-z0-9\\-~+=]+") (rtext "-")
    captionSlug = T.take 48 . limitChar . T.toLower . canonicalForm . fromMaybe ""
    components = unpack <$> ["telegram", who, pack $ show uid, captionSlug caption]
    pathslug = Just $ intercalate "-" ((not . null) `filter` components) <> ".jpg"

handlePhoto :: PrivEvent Text -> UpdateSummary PhotoMsg a -> Bot Text
handlePhoto pr update@(_, _, zn_msg@(ZnPhoto (caption, _))) = do
    root <- param "http-root"
    path <- store pr update
    nsfw <- (fmap (printf "(%s)") . formatNSFW =<<) <$> detectNSFW path

    return . T.intercalate " " $ catMaybes
        [ caption
        , Just $ formatUrl root path
        , pack <$> nsfw
        ]

formatUrl :: Text -> String -> Text -- "http://x" "/ " => "http://x/%20"
formatUrl root path = (root `mappend`) . TL.toStrict . TLE.decodeUtf8 $
    B.toLazyByteString (encodePathSegments [pack path])
