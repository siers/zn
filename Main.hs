import Bot
import Commands
import Control.Concurrent
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Text as T hiding (head)
import GHC.Conc
import Network.IRC.Client
import Data.Time
import System.Exit
import System.Posix.Files

sleep n = liftIO . threadDelay $ n * 1000000

getTVar :: MonadIO m => m (TVar b) -> m b
getTVar accessor = accessor >>= liftIO . atomically . readTVar

setTVar :: MonadIO m => m (TVar b) -> b -> m ()
setTVar accessor val = accessor >>= liftIO . atomically . flip writeTVar val

initHandler :: Config -> StatefulIRC BotState ()
initHandler conf = do
    liftIO getCurrentTime >>= send . Privmsg (master conf) . Right . pack . show
    send . Privmsg "nickserv" . Right $ "id " `append` (pass conf)
    mapM_ (send . Join) (chans conf)

initDispatcher :: Config -> UnicodeEvent -> StatefulIRC BotState ()
initDispatcher config event = do
    initialized <- getTVar stateTVar
    when (not initialized) $ initHandler config
    setTVar stateTVar True

conf config = cfg { _eventHandlers = handlers ++ _eventHandlers cfg }
    where
        cfg = defaultIRCConf $ user config
        handlers =
            [ EventHandler "init handler" EMode $ initDispatcher config
            , EventHandler "cmd handler" EPrivmsg cmdHandler]

main = do
    configFound <- fileExist "zn.rc"
    when (not configFound) $ do
        putStrLn "# no conf found\n$ cp zn.rc{sample,}"
        exitFailure

    config <- either error return =<< parseConfig "zn.rc"

    conn <- connect'
        stdoutLogger
        (BS.pack . unpack $ irchost config)
        (ircport config)
        1 :: IO (ConnectionConfig BotState)
    startStateful conn (conf config) False
