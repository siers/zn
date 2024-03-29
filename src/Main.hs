{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Ini
import Data.List
import Data.Map as M
import Data.Ratio as Ratio
import Data.Text as T hiding (head)
import Data.Time
import Network.IRC.Client hiding (instanceConfig)
import Safe
import System.Exit
import System.Posix.Files
import System.Posix.Signals
import System.Environment (lookupEnv)
import Zn.Bot
import Zn.Data.Ini
import Zn.Handlers.Privmsg
import Zn.Handlers.Join
import Zn.Persist
import Zn.Socket
import Zn.Types

initHandler :: Ini -> StatefulBot ()
initHandler conf = do
    send . Nick $ parameter conf "user"
    stateful (use bootTime) >>= send . Privmsg (parameter conf "master") . Right . pack . show
    send . Privmsg "nickserv" . Right $ "id " `append` (parameter conf "pass")
    mapM_ (send . Join) . T.split (== ',') $ parameter conf "chans"

instanceConfig :: Ini -> InstanceConfig BotState
instanceConfig config = defaultInstanceConfig nick' & (handlers %~ (handlerList ++))
    where
        nick' = parameter config "user"
        handlerList = [kickHandler, cmdHandler, joinFailHandler]

connection :: Ini -> ConnectionConfig BotState
connection ini = conn &
    (logfunc .~ stdoutLogger) .
    (onconnect .~ initHandler ini) .
    (flood .~ (fromRational $ 1 Ratio.% 2))

    where
        conn = plainConnection host port
        host = (BS.pack . unpack $ parameter ini "irchost")
        port = (maybe (error "cannot parse ircport") id . readMay . unpack $ parameter ini "ircport")

main = do
    configPath <- maybe "zn.rc" id <$> lookupEnv "conf"
    configFound <- fileExist configPath
    when (not configFound) $ do
        putStrLn "# no conf found at current directory or at $conf\n$ cp zn.rc{.sample,}"
        exitFailure

    conf  <- either error id <$> readIniFile configPath
    state <- BotState
        <$> getCurrentTime
        <*> pure conf
        <*> pure M.empty -- logs
        <*> pure M.empty -- locks
        <*> pure False -- silence
        <*> pure False -- debug

    -- saveState state -- json
    runZnMigrations dbString

    ircst <- newIRCState (connection conf) (instanceConfig conf) state
    rcntl <- newEmptyMVar
    raw   <- async $ runRawSocket ircst rcntl
    irc   <- async $ runClientWith ircst
    main' <- async $ void $ main'' ircst rcntl raw irc

    mapConcurrently
      ((print `either` (const (return ())) =<<) . waitCatch)
      [raw, irc, main']

    where
        untilInterrupted a =
            catchJust
                (`elemIndex` [ThreadKilled, UserInterrupt])
                a (return $ return ())

        main'' ircst rcntl raw irc = do
            mainTid <- myThreadId
            installHandler sigTERM (CatchOnce (killThread mainTid)) Nothing

            untilInterrupted $ wait irc
            putMVar rcntl () >> wait raw

            ircInjectMsg ircst ["QUIT", "entering a scheduled restart"]
            race
                (sleep 3)
                (wait irc)
