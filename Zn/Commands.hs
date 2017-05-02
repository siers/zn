{-# LANGUAGE FlexibleContexts #-}

module Zn.Commands where

import Control.Applicative
import Control.Lens hiding (from)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text as T (pack, Text)
import Zn.Bot
import Zn.Command
import Zn.Commands.Mping
import qualified Zn.Commands.Replies as Replies
import Zn.Commands.Uptime
import Zn.Commands.Version as Zn
import qualified Zn.Grammar as Gr
import Zn.IRC
import Zn.Process

-- Args, Pure, Reply, Output(no input/args), Lift(IO), Monad(m a)

command = (,)
commandA name cmd = command name . cmd . view args
commandR name cmd = command name $ \msg -> cmd msg >>= reply msg
commandRA name cmd = commandR name $ cmd . view args
commandO name = commandR name . const
commandLO name = commandR name . const . liftIO
commandM name = command name . return

commandP name cmd = command name . return . cmd
commandPA name cmd = commandA name $ return . cmd
commandPO name str = commandO name $ return str
commandPRA name cmd = commandRA name $ return . cmd

commands :: M.Map Text (Command Text -> Bot ())
commands = M.fromList
    [ commandPRA    "echo"      (T.intercalate " ")
    , commandPRA    "quote"     (\x -> "\"" <> T.intercalate "\" \"" x <> "\"")
    , commandPO     "version"   Zn.version
    , commandO      "uptime"    uptime
    , commandO      "mping"     mping
    , commandO      "replies"   Replies.list
    , commandM      "reload"    reload
    , commandLO     "iesauka" $ pack <$> shell "./scripts/names-lv/bundle_wrapper.rb"

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]

lookupCmd :: Text -> Bot (Maybe (Command Text -> Bot ()))
lookupCmd name = do
    reply <- fmap (snd . commandPO "_") <$> Replies.find name
    return (M.lookup name commands <|> reply)

shellish :: PrivEvent Text -> [Command Text]
shellish msg = map (\args -> Command args (view cont msg) (view src msg)) args
    where
        args = (fmap . fmap) pack . fromJust $
            Gr.matches Gr.shellish (view cont msg)

interpret :: PrivEvent Text -> Bot ()
interpret = mapM_ execute . shellish
    where
        execute cmd = return () `maybe` ($ cmd') =<< lookupCmd (views args head cmd)
            where cmd'= cmd & args %~ drop 1
