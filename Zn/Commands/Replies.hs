{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Control.Lens
import Data.Either.Extra
import Data.Ini
import qualified Data.Text as T
import Data.Text (Text)
import Zn.Types

find :: Text -> Bot (Maybe Text)
find name = do
    conf <- use config
    return . either (const Nothing) Just $ lookupValue "replies" name conf

-- print :: [Text] -> Bot Text
-- print args = joinCmds <$> (sequence . map find $ args)

list :: Bot Text
list = uses config $ (format . names)
    where
        names = fromRight [] . keys "replies"
        format = T.append "available replies: " . T.intercalate ", "
