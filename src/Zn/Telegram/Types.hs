{-# LANGUAGE TemplateHaskell #-}

module Zn.Telegram.Types where

import Control.Lens
import Data.Text (Text)
import Web.Telegram.API.Bot as T

type PhotoLink = String
type PhotoFileId = Text
type PhotoMsg = (Maybe Text, PhotoLink)
type PhotoFileMsg = (Maybe Text, PhotoFileId)

data ZnTgMsg t p v = ZnText t | ZnPhoto p | ZnFile v
type ZnTgMsg' a = ZnTgMsg a a ()

-- (UpdateID, User, Maybe Caption)
type UpdateSummary a = (Int, User, a)
type ZnUpdateSummary = UpdateSummary (ZnTgMsg Text PhotoFileMsg ())

makePrisms ''ZnTgMsg

znMsgJoin :: ZnTgMsg' a -> a
znMsgJoin (ZnText a) = a
znMsgJoin (ZnPhoto a) = a
-- znMsgJoin (ZnFile a) = a
