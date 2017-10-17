{-# LANGUAGE OverloadedStrings #-}

module Zn.Commands.URL.Detect
    ( detectImage
    , detectNSFW
    ) where

import Zn.Commands.URL.Types
import Zn.Types

detectImage :: BodyHeaders -> Bool
detectImage = const False

detectNSFW :: String -> Bot Bool
detectNSFW = const $ return False
