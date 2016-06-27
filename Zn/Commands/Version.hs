module Zn.Commands.Version where

import Control.Monad.IO.Class
import Data.List.Split
import System.Process
import Text.Printf
import Zn.Bot

cmd :: MonadIO m => String -> m String
cmd code = liftIO $ readProcess (head parts) (tail parts) ""
    where parts = splitOn " " code

version :: [String] -> Bot String
version _ = fmap (filter (/= '\n')) $ printf statement <$> rev <*> date <*> origin
    where
        statement = "Running %s written on %s. Public repo here: %s"
        rev = take 10 <$> cmd "git rev-parse HEAD"
        date = cmd "git show -s --format=%ci HEAD"
        origin = cmd "git remote get-url public"
