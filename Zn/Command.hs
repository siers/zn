{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Zn.Command where

import Control.Lens
import Data.Monoid
import Data.Text as T (Text)
import qualified Network.IRC.Client as IRC
import Network.IRC.Client hiding (reply, Message)

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

class Packet p where
    src :: Lens' (p a) (Source a)
    cont :: Lens' (p a) a

instance Packet Command where
    src = sourceC
    cont = rawC

instance Packet PrivEvent where
    src = sourceM
    cont = contents
