{-# LANGUAGE OverloadedStrings #-}

module Zn.Telegram.Message where

import Control.Lens hiding (from)
import Data.Foldable
import Data.List (intercalate)
import Data.Maybe
import Data.Text.ICU
import Data.Text.ICU.Char
import Data.Text.ICU.Replace
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Web.Telegram.API.Bot as T (User(..))
import Zn.IRC hiding (from)
import Zn.Telegram.File
import Zn.Telegram.Types
import Zn.Types

telegramMsgCaption :: ZnTgMsg a LinkCaptionMsg LinkCaptionMsg -> Maybe Text
telegramMsgCaption (ZnDoc (caption, _)) = caption
telegramMsgCaption (ZnPhoto (caption, _)) = caption

telegramMsgExt :: ZnTgMsg a LinkCaptionMsg LinkCaptionMsg -> Text
telegramMsgExt (ZnPhoto (_, _)) = "jpg"
telegramMsgExt (ZnDoc (_, link)) = T.takeWhileEnd (/= '.') $ link

telegramFilePath :: PrivEvent Text -> UpdateSummary (ZnTgMsg a LinkCaptionMsg LinkCaptionMsg) -> String
telegramFilePath pr (uid, (User { user_first_name = who }), zn_msg) =
    intercalate "-" ((not . null) `filter` components) <> "." <> (unpack $ telegramMsgExt zn_msg)
  where
    -- http://stackoverflow.com/questions/44290218
    canonicalForm = T.filter (not . property Diacritic) . normalize NFD
    limitChar = T.dropAround (== '-') . replaceAll (regex [] "[^a-z0-9\\-~+=]+") (rtext "-")
    captionSlug = T.take 48 . limitChar . T.toLower . canonicalForm . fromMaybe ""
    components = unpack <$> ["telegram", who, pack $ show uid, captionSlug (telegramMsgCaption zn_msg)]

telegramMsg :: PrivEvent Text -> UpdateSummary (ZnTgMsg Text LinkCaptionMsg LinkCaptionMsg) -> Bot ()
telegramMsg pr update@(_, (User { user_first_name = who }), zn_msg) = do
  (zn_msg
    & (_ZnPhoto %%~ handleFile formatPhoto)
    >>= (_ZnDoc %%~ handleFile formatFile))
    >>= reply pr . formatMsg who . znMsgJoin

    where
      formatMsg :: Text -> Text -> Text
      formatMsg who text = fold ["<", who, "> "] <> text

      handleFile :: ZnMediaHandler -> LinkCaptionMsg -> Bot Text
      handleFile handler (caption, link) =
        storeFile pr (telegramFilePath pr update) link >>= handler caption
