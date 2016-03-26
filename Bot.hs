module Bot where

import Data.Ini
import Data.Text
import Network.IRC.Client.Types
import Safe

type BotState = Bool
type Bot a = StatefulIRC BotState a

data Config = Config
    { user :: Text
    , pass :: Text
    , chans :: [Text]
    , master :: Text
    , irchost :: Text
    , ircport :: Int
    }

target :: Source Text -> Text
target (Channel chan user) = chan
target (User user) = user

from :: Source Text -> Text
from (Channel chan user) = user
from (User user) = user

privtext :: Message Text -> Text
privtext (Privmsg _from msg) = either (const "") id msg

findValues :: (Text -> Either String Text) -> Either String Config
findValues lookup = do
    user <- lookup "user"
    pass <- lookup "pass"
    chans <- split (== ',') <$> lookup "chans"
    master <- lookup "master"
    irchost <- lookup "irchost"
    ircport <- maybe (error "cannot parse ircport") id . readMay . unpack
        <$> lookup "ircport"

    return $ Config user pass chans master irchost ircport

parseConfig :: FilePath -> IO (Either String Config)
parseConfig path = (>>= findValues . flip (lookupValue "main")) <$> readIniFile path
