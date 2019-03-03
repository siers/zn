{-# LANGUAGE TemplateHaskell #-}

module Zn.Telegram.Types where

import Control.Lens
import Data.Text (Text)
import Web.Telegram.API.Bot as T

type FileLink = Text
type TgFileId = Text

type LinkCaptionMsg = (Maybe Text, FileLink)
type FileCaptionMsg = (Maybe Text, TgFileId)

data ZnTgMsg t p v = ZnText t | ZnPhoto p | ZnVideo v
type ZnTgMsg' a = ZnTgMsg a a a

-- (UpdateID, User, Maybe Caption)
type UpdateSummary a = (Int, User, a)
type ZnUpdateSummary = UpdateSummary (ZnTgMsg Text FileCaptionMsg FileCaptionMsg)

makePrisms ''ZnTgMsg

znMsgJoin :: ZnTgMsg' a -> a
znMsgJoin (ZnText a) = a
znMsgJoin (ZnPhoto a) = a
znMsgJoin (ZnVideo a) = a
