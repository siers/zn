{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Zn.Commands.Urban
    ( urban
    , urbanQuery
    , urbanQueryWith
    , urbanFormat
    ) where

import Control.Arrow
import Data.Aeson
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T (length, take)
import Data.Text.ICU
import Data.Text.ICU.Replace
import Data.Text (Text, pack)
import GHC.Generics
import Safe
import Text.Printf
import Zn.Bot.Request
import Zn.IRC

data UrbanDescr = UrbanDescr
    { defid        :: Integer
    , word         :: Text
    , author       :: Text
    , permalink    :: Text
    , definition   :: Text
    , example      :: Text
    , current_vote :: Text -- seems to be always empty
    , thumbs_up    :: Integer
    , thumbs_down  :: Integer
    } deriving (Generic, Show)

data UrbanReply = UrbanReply
    { tags        :: [Text]
    , result_type :: Text
    , list        :: [UrbanDescr]
    , sounds      :: [Text]
    } deriving (Generic, Show)

instance FromJSON UrbanDescr
instance FromJSON UrbanReply

urbanAPI = "http://api.urbandictionary.com/v0/define?term=%s"

urbanQueryWith :: Monad m => (String -> m BodyHeaders) -> Text -> m (Maybe UrbanReply)
urbanQueryWith request = (decode . fst <$>) . request . printf urbanAPI

urbanQuery :: Text -> IO (Maybe UrbanReply)
urbanQuery = (decode . fst <$>) . request . printf urbanAPI

urbanReprezent :: UrbanDescr -> Text
urbanReprezent desc = pack $ printf
    "\"%s\" :: %s"
    (word desc)
    (shorten 220 . join $ definition desc)

    where
        join = replaceAll (regex [Multiline] " *(\r\n)+ *") (rtext " // ")
        shorten n t = if T.length t > n then T.take n t <> "…" else t

urbanFormat :: [Maybe UrbanReply] -> Text
urbanFormat = joinCmds . map urbanReprezent . filter'
    where
        filter' :: [Maybe UrbanReply] -> [UrbanDescr]
        filter' = catMaybes . map (>>= list >>> flip atMay 0)

urban :: [Text] -> IO Text
urban = (urbanFormat <$>) . sequence . map urbanQuery
