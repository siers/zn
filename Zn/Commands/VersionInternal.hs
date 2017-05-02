module Zn.Commands.VersionInternal (getVersion) where

import Control.Monad.IO.Class
import Data.List.Split
import qualified System.Process as P
import Text.Printf

cmd :: String -> IO String
cmd code = liftIO $ P.readProcess cmd args ""
    where cmd:args = splitOn " " code

shell :: String -> IO String
shell code = P.readCreateProcess (P.shell code) ""

formatName :: String -> String
formatName name = reverse . dropWhile (`elem` (".\n" :: String)) . reverse $
    if length name > 80
    then take 80 name ++ "…"
    else name

findURL :: String
findURL = "\
    \(git remote get-url public 2>/dev/null || \
    \ git remote get-url origin 2>/dev/null) \
    \ || echo URL_NOT_FOUND"

getVersion :: IO String
getVersion = fmap (filter (/= '\n')) $ printf statement <$> rev <*> str <*> date <*> origin
    where
        statement = "Running %s: «%s» of %s, %s"
        str = formatName <$> cmd "git show -s --format=%s"
        rev = cmd "git rev-parse --short HEAD"
        date = cmd "git show -s --format=%ci"
        origin = shell findURL
