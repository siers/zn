{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fforce-recomp #-}

module Zn.Commands.Version where

import Language.Haskell.TH
import Zn.Commands.VersionInternal
import Data.Text

version :: Text
version =  $(LitE . StringL <$> runIO getVersion)
