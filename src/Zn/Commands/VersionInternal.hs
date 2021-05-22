module Zn.Commands.VersionInternal (getVersion) where

import Text.Printf
import Zn.Process

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
getVersion = do
  isGit <- shell "git status > /dev/null 2>&1; echo -n $?"
  if isGit /= "0"
  then pure "error: version info missing in build"
  else fmap (filter (/= '\n')) $ printf statement <$> rev <*> str <*> date <*> origin
    where
        statement = "Running %s: «%s» of %s, %s"
        str = formatName <$> cmd "git show -s --format=%s"
        rev = shell "git rev-parse --short HEAD"
        date = cmd "git show -s --format=%ci"
        origin = shell findURL
