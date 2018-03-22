{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Control.Monad.IO.Class
import Database.Groundhog as G
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack)
import Safe
import System.Random
import Zn.Persist
import Zn.Telegram (anonymize)
import Zn.Types

aliasAlphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

create :: [Text] -> Bot Text
create (n:v:_) = do
    secret <- liftIO $ randomIO :: Bot Integer
    let token = anonymize aliasAlphabet 32 (pack $ show secret)

    found <- find n
    if isJust found
    then return "Alias already exists!"
    else sql $
        insert (Fact n v (Just token)) *>
        return ("To revoke use: !alias-del " <> token)

del :: [Text] -> Bot Text
del (token:_) = do
    sql $ delete (FactSecretField ==. Just token)
    return "Acknowledged!"

find :: Text -> Bot (Maybe Text)
find n = sql $ fmap factValue . headMay <$> select (FactNameField ==. n)
