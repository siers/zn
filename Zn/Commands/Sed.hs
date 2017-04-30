module Zn.Commands.Sed where

import Control.Lens hiding (from)
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Text (unpack, pack, Text)
import Hledger.Utils.Regex
import Network.IRC.Client (Source)
import Text.Regex.TDFA
import Zn.Bot
import Zn.Command
import Zn.Commands.Logs
import qualified Zn.Grammar as Gr
import Zn.IRC

unhigh :: String -> String
unhigh = replaceRegex (toRegex "(\x02|\x03[0-9]{1,2}|\x1d|\x1f|\x16|\x0f)") ""

highlight :: String -> String
highlight text = "\x02\x1d\x03" ++ "04" ++ text ++ "\x0f"

subst :: ((String, String), String) -> String -> Maybe String
subst ((regex', repl), flags) = subst . unhigh

    where
        subst msg = replaced msg <$ (matchM regex msg :: Maybe String)
        replaced msg = replacer regex (highlight repl) msg

        -- affected by flags
        regex = ($ regex') $ if 'i' `elem` flags then toRegexCI else toRegex
        replacer = if 'g' `elem` flags then replaceRegex else replaceRegexSingle

tailor :: String -> Source String -> String -> History Text -> Logs String
tailor nick source flags logs =
    Seq.take length .
    Seq.filter (\l -> unsedish l && me l && recur nick l) .
    logFrom (target source) . logMap unpack
        $ logs

    where
        unsedish     = not . isJust . Gr.matches Gr.sed . pack . (view text)
        me      log  = if 'm' `elem` flags then view author log == from source else True
        recur n log  = ('r' `elem` flags) == (view author log == n)
        length       = if 'l' `elem` flags then 1000 else 50

sed :: PrivEvent Text -> Bot ()
sed pr = join $ fmap (sequence_ . fmap (reply pr . pack) . join) $

    Gr.ifParse Gr.sed (view cont pr) $
        \args@(_, flags) -> do
            nick <- Bot $ unpack <$> getNick

            pick . fmap (text (subst args)) . tailor nick source flags
                <$> use history

    where
        source = view src $ fmap unpack pr
        pick = fmap (view text) . join . find isJust :: Foldable t => t (Maybe (Line a)) -> Maybe a
