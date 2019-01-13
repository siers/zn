{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.Replies where

import Control.Lens hiding (from)
import Control.Monad.IO.Class
import Database.Groundhog as G
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Network.IRC.Client hiding (reply)
import Safe
import System.Random
import Zn.IRC
import Zn.Persist
import Zn.Types

-- anonymize
import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import Numeric

-- A rather injective function into meaningless names.
anonymize :: String -> Int -> Text -> Text
anonymize alphabet len seed =
    pack . take len $
        (alphabet !!) . fromIntegral . (`mod` l) <$>
            iterate (`div` l) rnd
    where
        l = fromIntegral $ length alphabet :: Integer
        h = hash :: BS.ByteString -> Digest Keccak_512
        rnd = fst . head . readHex . show . h . BS.pack . unpack $ seed :: Integer

aliasAlphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

create' :: [Text] -> Bot Text
create' (n:v:_) = do
    secret <- liftIO $ randomIO :: Bot Integer
    let token = anonymize aliasAlphabet 32 (pack $ show secret)

    found <- find n
    if isJust found
    then return "Alias already exists!"
    else sql $
        insert (Fact n v (Just token)) *>
        return ("To revoke use: !alias-del " <> token)

create :: Command Text -> Bot ()
create c = create' (view args c) >>= reply (src %~ (User . from) $ c)

del :: [Text] -> Bot Text
del (token:_) = do
    sql $ delete (FactSecretField ==. Just token)
    return "Acknowledged!"

find :: Text -> Bot (Maybe Text)
find n = sql $ fmap factValue . headMay <$> select (FactNameField ==. n)
