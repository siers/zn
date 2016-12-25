{-# LANGUAGE TemplateHaskell #-}

module Zn.Commands.Version where

import Language.Haskell.TH
import Zn.Commands.VersionInternal

version :: String
version = $(LitE . StringL <$> runIO getVersion)

