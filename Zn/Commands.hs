{-# LANGUAGE FlexibleContexts #-}

module Zn.Commands
    ( cmdHandler
    , Command
    ) where

import Control.Applicative
import Control.Lens hiding (from)
import Control.Monad
import Data.CaseInsensitive as CI (mk)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text as T (pack, unpack, Text, splitOn)
import Network.IRC.Client hiding (reply)
import Zn.Bot
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

command = (,)
commandA name cmd = command name . cmd . view args
commandR name cmd = command name $ \msg -> cmd msg >>= reply msg
commandRA name cmd = commandR name $ cmd . view args
commandO name = commandR name . const

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

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]

lookupCmd :: Text -> Bot (Maybe (Command Text -> Bot ()))
lookupCmd name = do
    reply <- fmap (snd . commandPO "_") <$> Replies.find name
    return (M.lookup name commands <|> reply)

addressed :: PrivEvent Text -> Bot (Maybe (PrivEvent Text))
addressed msg = do
    nick' <- Bot $ getNick
    match <- fmap (fmap pack) $ Gr.ifParse (Gr.addressed $ unpack nick') (cont `view` msg) return

    if isJust match then
        return . Just . (cont .~ fromJust match) $ msg
    else
        if (isUser . view src $ msg)
        then return $ Just msg
        else return Nothing

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

listeners :: [PrivEvent Text -> Bot ()]
listeners = map (forkCmd .) $
    [
        url,
        sed,
        when addressed interpret
    ]

    where
        forkCmd = Bot . void . fork . runBot :: Bot () -> Bot ()
        when :: (PrivEvent Text -> Bot (Maybe a))
             -> (a -> Bot ())
             -> PrivEvent Text
             -> Bot ()
        when action sink msg = foldMap sink . catMaybes . (:[]) =<< action msg

broadcast :: PrivEvent Text -> StatefulBot ()
broadcast msg = do
    ignores <- splitOn "," . flip parameter "ignores" <$> stateful (use config)

    runBot $ do
        logs msg

        when (not $ ignore ignores msg) $
            mapM_ ($ msg) listeners

        save

    where
        ignore :: [Text] -> PrivEvent Text -> Bool
        ignore list = flip elem (map CI.mk list) . CI.mk . from . view src

cmdHandler :: EventHandler BotState
cmdHandler = EventHandler (matchType _Privmsg) $ \src (_target, privmsg) -> do
    let text = either (error . show) id $ privmsg
    let msg = PrivEvent text src
    broadcast msg
