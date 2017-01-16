module Zn.Commands.Sed where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Text (unpack)
import Hledger.Utils.Regex
import Network.IRC.Client hiding (reply)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Regex.TDFA
import Text.Regex.TDFA.String
import Zn.Bot
import Zn.Commands.Logs
import qualified Zn.Grammar as Gr
import Zn.IRC

sed' :: String -> M.Map String (Seq.Seq [String]) -> ((String, String), String) -> Maybe String
sed' target history ((regex', subst), flags) =
    join . find isJust . fmap perhapsSubst . limit . fmap (!! 2) . logFrom target $ history
    where
        limit = Seq.take 50 . Seq.filter (not . isJust . Gr.matches Gr.sed)
        perhapsSubst msg = replacer regex subst msg <$ (matchM regex msg :: Maybe String)

        -- affected by flags
        regex = ($ regex') $ if 'i' `elem` flags then toRegexCI else toRegex
        replacer = if 'g' `elem` flags then replaceRegex else replaceRegexSingle

sed :: UnicodeEvent -> Bot ()
sed ev = join $ fmap (sequence_ . fmap (reply ev return) . join) $

    Gr.ifParse Gr.sed body $
        (sed' source <$> use history <*>) . return

    where
        body = unpack . privtext . _message $ ev
        source = unpack . target . _source $ ev
