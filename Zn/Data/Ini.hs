module Zn.Data.Ini where

import Data.Aeson
import Data.Aeson.Parser
import Data.Ini
import Data.Text

instance FromJSON Ini where
    parseJSON a = either error id . parseIni <$> parseJSON a

instance ToJSON Ini where
    toJSON = String . printIni

setting :: Ini -> Text -> Text
setting conf name =
    either (error . ("Couldn't find in config: " ++)) id $
    lookupValue "main" name conf
