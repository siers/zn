{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Zn.Data.Ini where

import Control.Lens
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Parser
import Data.Ini
import Data.Text

instance FromJSON Ini where
    parseJSON a = either error id . parseIni <$> parseJSON a

instance ToJSON Ini where
    toJSON = String . printIni

eitherWhine = either (error . ("Couldn't find in config: " ++)) id

justLookupValue :: Text -> Ini -> Text -> Text
justLookupValue section conf name = eitherWhine $ lookupValue section name conf

lookupValueS :: String -> String -> Ini -> Either String String
lookupValueS a b = fmap unpack . lookupValue (pack a) (pack b)

justLookupValueMStr :: MonadState s m => Lens' s Ini -> String -> String -> m String
justLookupValueMStr l section name = eitherWhine . lookupValueS section name <$> use l

parameter = justLookupValue "main"
