module Zn.Process where

import Data.List.Split
import qualified System.Process as P

cmd :: String -> IO String
cmd code = P.readProcess cmd args ""
    where cmd:args = splitOn " " code

shell :: String -> IO String
shell code = P.readCreateProcess (P.shell code) ""
