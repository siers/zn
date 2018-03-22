{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Database.Groundhog as G
import Data.Monoid
import Data.Text (Text)
import Safe
import Zn.Bot
import Zn.Persist
import Zn.Types
import Zn.Telegram (anonymize)

aliasAlphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

create :: [Text] -> Bot Text
create (n:v:_) = do
    secret <- param "alias-secret"
    let token = anonymize aliasAlphabet 32 (n <> v <> secret)
    sql $ insert (Fact n v (Just token))

    return $ "To revoke use: !alias-del " <> token

del :: [Text] -> Bot Text
del (token:_) = do
    sql $ delete (FactSecretField ==. Just token)
    return "Acknowledged!"

find :: Text -> Bot (Maybe Text)
find n = sql $ fmap factValue . headMay <$> select (FactNameField ==. n)
