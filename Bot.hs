module Bot where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Ini
import Data.Text
import Data.Time
import GHC.Conc
import Network.IRC.Client.Types
import Safe

data BotState = BotState
    { initialized :: Bool
    , bootTime :: UTCTime
    }
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

sleep n = liftIO . threadDelay $ n * 1000000

getTVar :: MonadIO m => m (TVar b) -> m b
getTVar accessor = accessor >>= liftIO . atomically . readTVar

setTVar :: MonadIO m => m (TVar b) -> b -> m ()
setTVar accessor val = accessor >>= liftIO . atomically . flip writeTVar val

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
