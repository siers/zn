{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Control.Lens
import Data.Either.Extra
import Data.Ini
import qualified Data.Text as T
import Data.Text (Text)
import Zn.Bot
import Zn.IRC

find :: Text -> Bot Text
find cmd = do
    conf <- use config
    return . either (const "") id $ lookupValue "replies" cmd conf

print :: [Text] -> Bot Text
print args = joinCmds <$> (sequence . map find $ args)

list :: Bot Text
list = uses config $ (format . names)
    where
        names = fromRight . keys "replies"
        format = T.append "available replies: " . T.intercalate ", "
