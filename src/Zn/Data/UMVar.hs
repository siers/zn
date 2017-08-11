module Zn.Data.UMVar where

import Control.Concurrent
import Data.Aeson
import System.IO.Unsafe (unsafePerformIO)

data UnserializableMVar a = UMVar { umvar :: MVar a }

instance Show (UnserializableMVar a) where
    show _ = "<unshowable>"
instance ToJSON (UnserializableMVar a) where
    toJSON (UMVar m) = Null
instance FromJSON (UnserializableMVar a) where
    parseJSON _ = return . UMVar . unsafePerformIO $ newEmptyMVar
