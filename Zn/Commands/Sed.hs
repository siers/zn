module Zn.Commands.Sed where

import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Text (unpack, pack, Text)
import Hledger.Utils.Regex
import Text.Regex.TDFA
import Zn.Bot
import Zn.Command
import Zn.Commands.Logs
import qualified Zn.Grammar as Gr
import Zn.IRC

highlight :: String -> String
highlight text = "\x02\x1d\x03" ++ "04" ++ text ++ "\x0f"

sed' :: String -> Seq.Seq [String] -> ((String, String), String) -> Maybe String
sed' target history ((regex', subst), flags) =
    join . find isJust . fmap perhapsSubst . limit . fmap (!! 2) $ history
    where
        limit = Seq.take 50 . Seq.filter (not . isJust . Gr.matches Gr.sed . pack)
        perhapsSubst msg = replacer regex (highlight subst) msg <$ (matchM regex msg :: Maybe String)

        -- affected by flags
        regex = ($ regex') $ if 'i' `elem` flags then toRegexCI else toRegex
        replacer = if 'g' `elem` flags then replaceRegex else replaceRegexSingle

sed :: PrivEvent Text -> Bot ()
sed pr = join $ fmap (sequence_ . fmap (reply pr . pack) . join) $

    Gr.ifParse Gr.sed (view cont pr) $
        (sed' source <$> uses history tailor <*>) . return

    where
        tailor = logFrom source . logMap unpack
        source = unpack . target . view src $ pr
