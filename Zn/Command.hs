{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Zn.Command where

import Control.Lens
import Data.Aeson hiding ((.=))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.IRC.Client hiding (reply, Message)

class Packet p where
    src :: Lens' (p a) (Source a)
    cont :: Lens' (p a) a

data Command a = Command
    { _args :: [a]
    , _rawC :: a -- avoid clashes with irc-client
    , _sourceC :: Source a } deriving (Functor)

data PrivEvent a = PrivEvent
    { _contents :: a
    , _sourceM :: Source a
    } deriving (Functor)

makeLenses ''PrivEvent
makeLenses ''Command

instance Packet Command where
    src = sourceC
    cont = rawC

instance Packet PrivEvent where
    src = sourceM
    cont = contents

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
