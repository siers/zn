module Zn.Handlers.Privmsg
    ( cmdHandler
    ) where

import Control.Lens hiding (from)
import Control.Monad
import Data.CaseInsensitive as CI (mk)
import Data.Maybe
import Data.Text as T (Text, pack, unpack, splitOn, intercalate, toUpper)
import Network.IRC.Client hiding (reply)
import Network.IRC.CTCP
import Zn.Bot
import Zn.Bot.Handle
import Zn.Commands
import Zn.Commands.Logs
import Zn.Commands.Sed
import Zn.Commands.URL.Main
import Zn.Data.Ini
import qualified Zn.Grammar as Gr
import Zn.IRC
import Zn.Regex
import Zn.Types

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

listeners :: [PrivEvent Text -> Bot ()]
listeners = map ((forkCmd .) handle .) $
    [ url
    , sed
    , when addressed interpret
    , regex
    ]

    where
        forkCmd = Bot . void . fork . runBot :: Bot () -> Bot ()
        when :: (PrivEvent Text -> Bot (Maybe a))
             -> (a -> Bot ())
             -> PrivEvent Text
             -> Bot ()
        when action sink msg = foldMap sink . catMaybes . (:[]) =<< action msg

broadcast :: PrivEvent Text -> StatefulBot ()
broadcast msg = runBot $ do
    logs msg
    ignoreGuard broadcast
    save

    where
        ignored :: [Text] -> PrivEvent Text -> Bool
        ignored list = flip elem (map CI.mk list) . CI.mk . from . view src

        ignoreGuard :: Bot () -> Bot ()
        ignoreGuard a = do
            ignores <- splitOn "," . flip parameter "ignores" <$> use config
            when (not $ ignored ignores msg) a

        broadcast = mapM_ ($ msg) listeners

cmdHandler :: EventHandler BotState
cmdHandler = EventHandler (matchType _Privmsg) $
    \src (_target, privmsg) ->
        void . sequence $ broadcast <$> (maybeCmd privmsg <*> pure src)

    where
        maybeCmd = either maybeCmdFromCTCP (Just . PrivEvent)
        maybeCmdFromCTCP ctcp =
            case fromCTCP ctcp & _1 %~ toUpper of
                ("ACTION", args) -> Just $ PrivEvent (intercalate " " args)
                (_, _)           -> Nothing
