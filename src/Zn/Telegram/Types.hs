{-# LANGUAGE TemplateHaskell #-}

module Zn.Telegram.Types where

import Control.Lens
import Data.Text (Text)
import Web.Telegram.API.Bot as T
import Zn.Types

type FileLink = Text
type TgFileId = Text

type PathLinkPair = (String, Text)
type ZnMediaHandler = Maybe Text -> PathLinkPair -> Bot Text

type LinkCaptionMsg = (Maybe Text, FileLink)
type FileCaptionMsg = (Maybe Text, TgFileId)

data ZnTgMsg t p v = ZnText t | ZnPhoto p | ZnDoc v deriving (Show)
type ZnTgMsg' a = ZnTgMsg a a a

-- (UpdateID, User, Maybe Caption)
type UpdateSummary a = (Int, User, a)
type ZnUpdateSummary = UpdateSummary (ZnTgMsg Text FileCaptionMsg FileCaptionMsg)
type ZnUpdated = UpdateSummary (ZnTgMsg Text LinkCaptionMsg LinkCaptionMsg)

makePrisms ''ZnTgMsg

znMsgJoin :: ZnTgMsg' a -> a
znMsgJoin (ZnText a) = a
znMsgJoin (ZnPhoto a) = a
znMsgJoin (ZnDoc a) = a
