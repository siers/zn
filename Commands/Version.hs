{-# language OverloadedStrings #-}

module Commands.Version where

import Bot
import Control.Monad.IO.Class
import System.Process
import Text.Printf

cmd :: String -> IO String
cmd code = readCreateProcess (shell code) ""

version :: [String] -> Bot String
version _ = do
    rev <- liftIO $ cmd "git rev-parse HEAD | cut -b-10"
    date <- liftIO $ cmd "git show -s --format=%ci HEAD"
    origin <- liftIO $ cmd "git remote get-url public"
    return . filter (/= '\n') $ printf "Running %s written on %s. Public repo here: %s" rev date origin
