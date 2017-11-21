{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Zn.Types where

import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import Data.Aeson
import Data.Ini
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import Network.IRC.Client hiding (reply, Message)
import Text.Printf
import Zn.Data.Ini ()

-- Log types.

data Line a = Line
    { _date :: a
    , _author :: a
    , _text :: a
    } deriving (Functor, Show, Generic)

instance ToJSON (Line Text)
instance FromJSON (Line Text)
makeLenses ''Line

type Log = (Text, Line Text)
type Logs a = Seq.Seq (Line a)
type History a = M.Map a (Logs a) -- new in front

-- Bot types.

data BotState = BotState
    { _bootTime :: UTCTime
    , _config :: Ini
    , _history :: History Text
    , _locks :: M.Map Text (MVar ())
    , _silence :: Bool
    } deriving (Generic)

makeLenses ''BotState

instance ToJSON BotState where
    toJSON (BotState bootTime config history _ _) = object
        ["bootTime" .= bootTime, "config" .= config, "history" .= history]

instance FromJSON BotState where
    parseJSON = withObject "BotState" $ \j -> BotState
        <$> j .: "bootTime"
        <*> j .: "config"
        <*> j .: "history"
        <*> pure M.empty
        <*> pure False

remove :: String -> String -> String
remove chars = filter (not . flip elem chars)

confStore = "zn.rc"
botStore = "data/state.json"

safeStore :: String -> String -> String
safeStore name = printf "data/%s/%s" name . remove "/" . dropWhile (== '.')

logStore, downStore :: String -> String
logStore = safeStore "logs"
downStore = safeStore "down"

cmdSep = seq " ▞ " " ╱ " :: Text

type StatefulBot a = IRC BotState a
newtype Bot a = Bot { runBot :: StatefulBot a }
    deriving (Functor, Applicative, Monad, MonadIO,
        MonadCatch, MonadThrow, MonadMask, MonadState BotState)

instance Monoid a => Monoid (Bot a) where
    mempty = return mempty
    a `mappend` b = liftM2 mappend a b

-- Message types

class Packet p where
    src :: Lens' (p a) (Source a)
    cont :: Lens' (p a) a

data Command a = Command
    { _args :: [a]
    , _rawC :: a -- avoid clashes with irc-client
    , _sourceC :: Source a } deriving (Show, Functor)

data PrivEvent a = PrivEvent
    { _contents :: a
    , _sourceM :: Source a
    } deriving (Show, Functor)

makeLenses ''PrivEvent
makeLenses ''Command

instance Packet Command where
    src = sourceC
    cont = rawC

instance Packet PrivEvent where
    src = sourceM
    cont = contents

-- Routes.

type Route = PrivEvent Text -> Bot ()
type Routes = M.Map Text Route
