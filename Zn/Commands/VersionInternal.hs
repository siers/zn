module Zn.Commands.VersionInternal (getVersion) where

import Control.Monad.IO.Class
import Data.List.Split
import System.Process
import Text.Printf

cmd :: String -> IO String
cmd code = liftIO $ readProcess cmd args ""
    where cmd:args = splitOn " " code

getVersion :: IO String
getVersion = fmap (filter (/= '\n')) $ printf statement <$> rev <*> date <*> origin
    where
        statement = "Running %s written on %s. Public repo here: %s"
        rev = take 10 <$> cmd "git rev-parse HEAD"
        date = cmd "git show -s --format=%ci HEAD"
        origin = cmd "git remote get-url public"
