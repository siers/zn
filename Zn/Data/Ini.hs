module Zn.Data.Ini where

import Data.Aeson
import Data.Aeson.Parser
import Data.Ini
import Data.Text

instance FromJSON Ini where
    parseJSON a = either error id . parseIni <$> parseJSON a

instance ToJSON Ini where
    toJSON = String . printIni

justLookupValue :: Text -> Ini -> Text -> Text
justLookupValue section conf name =
    either (error . ("Couldn't find in config: " ++)) id $
    lookupValue section name conf

parameter = justLookupValue "main"

lookupValueS :: String -> String -> Ini -> Either String String
lookupValueS a b = fmap unpack . lookupValue (pack a) (pack b)
