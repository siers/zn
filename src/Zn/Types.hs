{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Zn.Types where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Ini
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import Network.IRC.Client hiding (reply, Message)
import Network.IRC.Client.Internal.Types (IRC(..))
import Text.Printf
import Zn.Data.Ini ()

-- Log types.

data Line a = Line
    { _date :: a
    , _author :: a
    , _text :: a
    , _actionL :: Bool
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
    , _debug :: Bool
    } deriving (Generic)

makeLenses ''BotState

instance ToJSON BotState where
    toJSON (BotState bootTime config history _ _ _) = object
        ["bootTime" .= bootTime, "config" .= config, "history" .= history]

instance FromJSON BotState where parseJSON = undefined
-- instance FromJSON BotState where
--     parseJSON = withObject "BotState" $ \j -> BotState
--         <$> j .: "bootTime"
--         <*> j .: "config"
--         <*> j .: "history"
--         <*> pure M.empty
--         <*> pure False

remove :: String -> String -> String
remove chars = filter (not . flip elem chars)

confStore = "zn.rc"
botStore = "data/state.json"
dbString = "data/state.sqlite"

safeStore :: String -> String -> String
safeStore name = printf "data/%s/%s" name . remove "/" . dropWhile (== '.')

logStore, downStore :: String -> String
logStore = safeStore "logs"
downStore = safeStore "down"

cmdSep = seq " ▞ " " ╱ " :: Text

type StatefulBot a = IRC BotState a
newtype Bot a = Bot { runBot :: StatefulBot a }
    deriving (Functor, Alternative, Applicative, Monad, MonadIO, MonadPlus,
        MonadCatch, MonadThrow, MonadMask, MonadState BotState, MonadBase IO)

instance Monoid a => Monoid (Bot a) where
    mempty = return mempty
    a `mappend` b = liftM2 mappend a b

instance MonadBase IO (IRC s) where
    liftBase = IRC . liftBase

instance MonadBaseControl IO (IRC s) where
    type StM (IRC s) a = ComposeSt (ReaderT (IRCState s)) IO a
    liftBaseWith f = IRC $ liftBaseWith $ \q -> f (q . runIRC)
    restoreM = IRC . restoreM

instance MonadBaseControl IO Bot where
    type StM Bot a = StM (IRC BotState) a
    liftBaseWith f = Bot $ liftBaseWith $ \q -> f (q . runBot)
    restoreM = Bot . restoreM

-- at the time of writing, this hasn't derived MonadZero.
-- https://hackage.haskell.org/package/irc-client-1.0.0.1/docs/src/Network-IRC-Client-Internal-Types.html#IRC
instance MonadPlus (IRC a) where
    mzero = IRC mzero
    a `mplus` b = IRC $ runIRC a `mplus` runIRC b

instance Alternative (IRC a) where
    empty = IRC empty
    a <|> b = IRC $ runIRC a <|> runIRC b

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
    , _action :: Bool
    } deriving (Show, Functor)

privEvent a b = PrivEvent a b False
privMeEvent a b = PrivEvent a b True

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
