import Bot
import Commands
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Ini
import Data.Text as T hiding (head)
import Data.Time
import Network.IRC.Client
import System.Exit
import System.Posix.Files
import Safe

initHandler :: Ini -> StatefulIRC BotState ()
initHandler conf = do
    getTVar stateTVar >>= send . Privmsg (setting conf "master") . Right . pack . show . bootTime
    send . Privmsg "nickserv" . Right $ "id " `append` (setting conf "pass")
    mapM_ (send . Join) (split (== ',') $ setting conf "chans")

initDispatcher :: Ini -> UnicodeEvent -> StatefulIRC BotState ()
initDispatcher config event = do
    state <- getTVar stateTVar
    when (not . initialized $ state) $ initHandler config
    setTVar stateTVar (state { initialized = True })

ircConf config = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf $ setting config "user"
        handlers =
            [ EventHandler "init handler" EMode $ initDispatcher config
            , EventHandler "cmd handler" EPrivmsg cmdHandler]

main = do
    configFound <- fileExist "zn.rc"
    when (not configFound) $ do
        putStrLn "# no conf found\n$ cp zn.rc{sample,}"
        exitFailure

    conf <- either error id <$> readIniFile "zn.rc"
    state <- BotState False <$> getCurrentTime <*> pure conf

    conn <- connect'
        stdoutLogger
        (BS.pack . unpack $ setting conf "irchost")
        (maybe (error "cannot parse ircport") id . readMay . unpack $ setting conf "ircport")
        1 :: IO (ConnectionConfig BotState)
    startStateful conn (ircConf conf) state
