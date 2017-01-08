module Zn.Commands.VersionInternal (getVersion) where

import Control.Monad.IO.Class
import Data.List.Split
import System.Process
import Text.Printf

cmd :: String -> IO String
cmd code = liftIO $ readProcess cmd args ""
    where cmd:args = splitOn " " code

formatName :: String -> String
formatName name = reverse . dropWhile (`elem` (".\n" :: String)) . reverse $
    if length name > 80
    then take 80 name ++ "…"
    else name

getVersion :: IO String
getVersion = fmap (filter (/= '\n')) $ printf statement <$> rev <*> str <*> date <*> origin
    where
        statement = "Running %s: «%s» of %s, %s"
        str = formatName <$> cmd "git show -s --format=%s"
        rev = take 10 <$> cmd "git rev-parse HEAD"
        date = cmd "git show -s --format=%ci"
        origin = cmd "git remote get-url public"
