module Zn.Commands where

import Data.Aeson
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (decodeUtf8)

import Control.Monad
import Data.CaseInsensitive as CI (mk)
import Data.List
import qualified Data.List.Split as List
import Data.Text as T (pack, unpack, Text, splitOn, intercalate, null)
import qualified Network.IRC.Client as IRC
import Network.IRC.Client hiding (reply)
import Zn.Bot
import Zn.Commands.Mping
import qualified Zn.Commands.Replies as Replies
import Zn.Commands.Sed
import Zn.Commands.Uptime
import Zn.Commands.URL
import Zn.Commands.Version
import qualified Zn.Grammar as Gr
import Zn.IRC

uponAddression :: (Text -> Bot a) -> UnicodeEvent -> Bot (Maybe a)
uponAddression action ev = do
    nick <- Bot $ unpack . _nick <$> instanceConfig
    Gr.ifParse (pack <$> Gr.addressed nick) (body ev) action

addressed :: (Text -> Bot Text) -> UnicodeEvent -> Bot ()
addressed action ev = void . uponAddression (reply ev action) $ ev

command :: Text -> ([Text] -> Bot Text) -> UnicodeEvent -> Bot ()
command name action ev = do
    addressed (cmd . parts) ev
    where
        cmd (cmd:args) = if cmd == name then action args else return ""
        parts = filter (not . T.null) . T.splitOn " "

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
    , addressed Replies.find
    , commandP "echo"       (T.intercalate " ")
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
