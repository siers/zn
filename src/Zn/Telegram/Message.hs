{-# LANGUAGE OverloadedStrings #-}

module Zn.Telegram.Message where

import Control.Lens hiding (from)
import Database.Groundhog as G
import Data.Foldable
import Data.List (intercalate, intersect)
import Data.Maybe
import Data.Text.ICU
import Data.Text.ICU.Char
import Data.Text.ICU.Replace
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Network.IRC.Client as IRC
import Safe
import Web.Telegram.API.Bot as T (User(..), Chat(..), Message(..))
import Zn.IRC hiding (from)
import Zn.Persist
import Zn.Telegram.File
import Zn.Telegram.Types
import Zn.Types

telegramMsgCaption :: ZnTgMsg a LinkCaptionMsg LinkCaptionMsg -> Maybe Text
telegramMsgCaption (ZnDoc (caption, _)) = caption
telegramMsgCaption (ZnPhoto (caption, _)) = caption

telegramMsgExt :: ZnTgMsg a LinkCaptionMsg LinkCaptionMsg -> Text
telegramMsgExt (ZnPhoto (_, _)) = "jpg"
telegramMsgExt (ZnDoc (_, link)) = T.takeWhileEnd (/= '.') $ link

telegramFilePath :: PrivEvent Text -> ZnUpdateSummary -> String
telegramFilePath pr (uid, _msg, (User { user_first_name = who }), zn_msg) =
    intercalate "-" ((not . null) `filter` components) <> "." <> (unpack $ telegramMsgExt zn_msg)
  where
    -- http://stackoverflow.com/questions/44290218
    canonicalForm = T.filter (not . property Diacritic) . normalize NFD
    limitChar = T.dropAround (== '-') . replaceAll (regex [] "[^a-z0-9\\-~+=]+") (rtext "-")
    captionSlug = T.take 48 . limitChar . T.toLower . canonicalForm . fromMaybe ""
    components = unpack <$> ["telegram", who, pack $ show uid, captionSlug (telegramMsgCaption zn_msg)]

telegramMsg :: [Text] -> ZnUpdateSummary -> Bot ()
telegramMsg targets update@(_uid, msg, (User { user_first_name = who }), zn_msg) = do
  target <- fmap headMay . sql $ select (TgTargetKeyField ==. chatId)
  pr <- pure $ (privEvent "" . (`IRC.Channel` "")) . head $
    catMaybes [tgTargetChannel <$> target] ++ targets `intersect` targets

  (zn_msg
    & (_ZnPhoto %%~ handleFile pr formatPhoto)
    >>= (_ZnDoc %%~ handleFile pr formatFile))
    >>= reply pr . formatMsg who . znMsgJoin

    where
      chatId = fromIntegral . chat_id . chat $ msg :: Int
      formatMsg :: Text -> Text -> Text
      formatMsg who text = fold ["<", who, "> "] <> text

      handleFile :: PrivEvent Text -> ZnMediaHandler -> LinkCaptionMsg -> Bot Text
      handleFile pr handler (caption, link) =
        storeFile pr (telegramFilePath pr update) link >>= handler caption
