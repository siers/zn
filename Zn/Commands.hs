{-# LANGUAGE FlexibleContexts #-}

module Zn.Commands
    ( cmdHandler
    , Command
    ) where

import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.CaseInsensitive as CI (mk)
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text as T (pack, unpack, Text, splitOn)
import GHC.Conc
import qualified Network.IRC.Client as IRC
import Network.IRC.Client hiding (reply, Message)
import Zn.Bot
import Zn.Bot hiding (config)
import Zn.Command
import Zn.Commands.Logs
import Zn.Commands.Mping
import qualified Zn.Commands.Replies as Replies
import Zn.Commands.Sed
import Zn.Commands.Uptime
import Zn.Commands.URL
import Zn.Commands.Version as Zn
import Zn.Data.Ini
import qualified Zn.Grammar as Gr
import Zn.IRC

{-
command :: Text -> (Command Text -> Bot Reply) -> Message Text -> Bot Reply
command name action msg =
    flip shellish msg $ nameGuard name action
    where
        nameGuard :: Text -> (Command Text -> Bot Text) -> [Text] -> Bot Text
        nameGuard name action parts@(cmd:args) =
            if cmd == name
            then action args
            else return mempty

commands :: [Command Text -> Bot ()]
commands = []
    [ url
    , sed
    , shellish Replies.print
    , commandP "echo"       (T.intercalate " ")
    , commandP "quote"      (\x -> T.concat $ ["\""] ++ intersperse "\" \"" x ++ ["\""])
    , commandP' "version"   Zn.version
    , command' "uptime"     uptime
    , command' "reload"   $ pack <$> reload
    , command' "mping"      mping
    , command' "replies"    Replies.list

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]
-}

addressed :: Message Text -> Bot (Maybe (Message Text))
addressed msg = do
    nick' <- Bot $ getNick
    match <- fmap (fmap pack) $ Gr.ifParse (Gr.addressed $ unpack nick') (cont `view` msg) return

    if isJust match then
        return . Just . (cont .~ fromJust match) $ msg
    else
        if (isUser . view src $ msg)
        then return $ Just msg
        else return Nothing

    where
        getNick :: (MonadReader (IRCState s) m, MonadIO m) => m Text
        getNick = fmap (view nick) . (liftIO . atomically . readTVar) =<< view instanceConfig

shellish :: Message Text -> [Command Text]
shellish msg = map (\args -> Command args (view cont msg) (view src msg)) args
    where
        args = (fmap . fmap) pack . fromJust $
            Gr.matches Gr.shellish (view cont msg)

execute :: Message Text -> Bot Reply
execute = undefined

listeners :: [Message Text -> Bot Reply]
listeners =
    [ -- url , sed,
        when addressed execute
    ]

    where
        when :: (Message Text -> Bot (Maybe a))
             -> (a -> Bot Reply)
             -> Message Text
             -> Bot Reply
        when action sink msg = foldMap sink . catMaybes . (:[]) =<< action msg

broadcast :: Message Text -> StatefulBot ()
broadcast msg = do
    ignores <- splitOn "," . flip parameter "ignores" <$> stateful (use config)

    runBot $ do
        logs msg

        when (not $ ignore ignores msg) $
            mapM_ ($ msg) listeners

        save

    where
        ignore :: [Text] -> Message Text -> Bool
        ignore list = flip elem (map CI.mk list) . CI.mk . from . view src

cmdHandler :: EventHandler BotState
cmdHandler = EventHandler (matchType _Privmsg) $ \src (_target, msg) -> do
    let text = either (error . show) id $ msg
    let cmd = Message text src
    return ()
