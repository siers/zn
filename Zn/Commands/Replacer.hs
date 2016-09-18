module Zn.Commands.Replacer where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.IRC.Client
import Prelude hiding (concat)
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Zn.Bot

w_replace :: Eq a => [a] -> [a] -> [a] -> [a]
w_replace [] _ _ = []
w_replace s find repl =
    if take (length find) s == find
        then repl ++ (w_replace (drop (length find) s) find repl)
        else [head s] ++ (w_replace (tail s) find repl)

replacer :: UnicodeEvent -> Bot ()
replacer ev =
  if name == replacedName
  then return ()
  else (return replacedName) >>= reply ev . T.pack
  where
    replacedName = w_replace name "haskell" "***"
    name = T.unpack . privtext . _message $ ev
