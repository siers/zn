import Bot
import Commands
import Handlers

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Ini
import Data.Text as T hiding (head)
import Data.Time
import Network.IRC.Client hiding (instanceConfig)
import System.Exit
import System.Posix.Files
import Safe

instanceConfig config = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf $ setting config "user"
        handlers =
            [ EventHandler "cmd handler" EPrivmsg cmdHandler]

connection :: Ini -> IO (ConnectionConfig BotState)
connection conf = (\conn -> conn { _onconnect = initHandler conf }) <$> action
    where
        action = connect' stdoutLogger host port 1
        host = (BS.pack . unpack $ setting conf "irchost")
        port = (maybe (error "cannot parse ircport") id . readMay . unpack $ setting conf "ircport")

main = do
    configFound <- fileExist "zn.rc"
    when (not configFound) $ do
        putStrLn "# no conf found\n$ cp zn.rc{sample,}"
        exitFailure

    conf <- either error id <$> readIniFile "zn.rc"
    state <- BotState <$> getCurrentTime <*> pure conf
    conn <- connection conf

    startStateful conn (instanceConfig conf) state
