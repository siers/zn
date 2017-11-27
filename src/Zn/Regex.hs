{-# LANGUAGE TupleSections #-}

module Zn.Regex
    ( regex
    ) where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Text as T (pack, unpack, Text)
import Text.Regex.TDFA
import Zn.Commands
import Zn.Types

regexes :: M.Map Text (Command Text -> Bot ())
regexes = M.fromList
    [ commandPO "zn.*bot.*\\?" "i'm not a bot"
    , commandPO "@pyircbot" "<pyircbot> atpisies"
    ]

regex :: PrivEvent Text -> Bot ()
regex pr = mapM_ apply . filter (not . null . fst) . concatMap match $ M.toList regexes
    where
        apply (match, cmd) = cmd $ Command (fmap pack match) (view cont pr) (view src pr)
        match (r, cmd) = (, cmd) <$> (unpack (view cont pr) =~ (unpack r) :: [[String]])
