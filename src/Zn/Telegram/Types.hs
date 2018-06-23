{-# LANGUAGE TemplateHaskell #-}

module Zn.Telegram.Types where

import Control.Lens
import Data.Text (Text)
import Web.Telegram.API.Bot as T

type PhotoLink = String
type PhotoMsg = (Maybe Text, PhotoLink)
type PhotoSizeMsg = (Maybe Text, PhotoSize)

-- (UpdateID, Name, Maybe Caption)
type UpdateSummary a b = (Int, Text, ZnTgMsg a b)
data ZnTgMsg p t = ZnPhoto p | ZnText t
type ZnTgMsg' a = ZnTgMsg a a

makePrisms ''ZnTgMsg

znMsgJoin :: ZnTgMsg' a -> a
znMsgJoin (ZnPhoto a) = a
znMsgJoin (ZnText a) = a
