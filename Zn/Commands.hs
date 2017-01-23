module Zn.Commands where

import Data.Aeson
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)

import Control.Lens hiding (from)
import Control.Monad
import Data.CaseInsensitive as CI (mk)
import Data.List
import qualified Data.List.Split as List
import Data.Maybe
import qualified Data.Text as T
import Data.Text as T (pack, unpack, Text, splitOn, null)
import qualified Network.IRC.Client as IRC
import Network.IRC.Client hiding (reply)
import Zn.Bot
import Zn.Commands.Logs
import Zn.Commands.Mping
import qualified Zn.Commands.Replies as Replies
import Zn.Commands.Sed
import Zn.Commands.Uptime
import Zn.Commands.URL
import Zn.Commands.Version
import Zn.Data.Ini
import qualified Zn.Grammar as Gr
import Zn.IRC

addressed :: (Text -> Bot a) -> UnicodeEvent -> Bot ()
addressed action ev = do
    nick <- Bot $ _nick <$> instanceConfig
    match <- Gr.ifParse (Gr.addressed $ unpack nick) (body ev) return

    if isJust match then
        void $ sequence (action . pack <$> match)
    else
        (isUser $ _source $ ev) `when` (void $ action $ body ev)

shellish :: ([Text] -> Bot Text) -> UnicodeEvent -> Bot ()
shellish action ev = do
    flip addressed ev $ \input -> do
        print . squash $
            Gr.ifParse Gr.shellish input $ \phrases ->
                map (action . fmap pack) phrases
    where
        squash :: [Maybe (Bot Text)] -> Bot Text
        squash = fmap joinCmds . sequence . catMaybes . sequence . sequence
        print :: Bot Text -> Bot ()
        print text = Bot . IRC.reply ev =<< text

command :: Text -> ([Text] -> Bot Text) -> UnicodeEvent -> Bot ()
command name action ev =
    flip shellish ev $ nameGuard name action
    where
        nameGuard :: Text -> ([Text] -> Bot Text) -> [Text] -> Bot Text
        nameGuard name action parts@(cmd:args) =
            if cmd == name
            then action args
            else return ""

command' :: Text -> Bot Text -> UnicodeEvent -> Bot ()
command' n a = command n (return a)

commandP :: Text -> ([Text] -> Text) -> UnicodeEvent -> Bot ()
commandP name cmd = command name (return . cmd)

commandP' :: Text -> Text -> UnicodeEvent -> Bot ()
commandP' n a = commandP n (return a)

commands :: [UnicodeEvent -> Bot ()]
commands =
    [ url
    , sed
    , shellish Replies.print
    , commandP "echo"       (T.intercalate " ")
    , commandP "quote"      (\x -> T.concat $ ["\""] ++ intersperse "\" \"" x ++ ["\""])
    , commandP' "version"   version
    , command' "uptime"     uptime
    , command' "reload"   $ pack <$> reload
    , command' "mping"      mping
    , command' "replies"    Replies.list

    -- leaks important data to chan, but might be useful for debugging sometimes
    -- , command "dump" (\_ -> (L.unpack . decodeUtf8 . encode . toJSON) <$> getTVar stateTVar)
    ]

ignore :: [Text] -> UnicodeEvent -> Bool
ignore list = flip elem (map CI.mk list) . CI.mk . from . _source

cmdHandler :: UnicodeEvent -> StatefulBot ()
cmdHandler ev = do
    ignores <- splitOn "," . flip parameter "ignores" <$> stateful (use config)

    runBot $ do
        logs ev

        when (not $ ignore ignores ev) $
            mapM_ ($ ev) commands

        save
