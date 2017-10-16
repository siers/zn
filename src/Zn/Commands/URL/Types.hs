module Zn.Commands.URL.Types where

import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types

type BodyHeaders = (BL.ByteString, ResponseHeaders)
