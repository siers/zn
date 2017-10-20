{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Zn.Commands.Urban
    ( urban
    , urbanQuery
    ) where

import Control.Arrow
import Data.Aeson
import Data.Maybe
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

urbanQuery :: Text -> IO (Maybe UrbanReply)
urbanQuery = (decode . fst <$>) . request . printf urbanAPI

urbanReprezent :: UrbanDescr -> Text
urbanReprezent desc = pack $ printf
    "\"%s\" :: %s"
    (word desc)
    (definition desc)

urban :: [Text] -> IO Text
urban = (joinCmds . map urbanReprezent <$>) . descrs
    where
        descrs :: [Text] -> IO [UrbanDescr]
        descrs = fmap filter' . sequence . map urbanQuery

        filter' :: [Maybe UrbanReply] -> [UrbanDescr]
        filter' = catMaybes . map (>>= list >>> flip atMay 0)
