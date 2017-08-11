module Zn.Commands.Sed where

import Control.Lens hiding (from)
import Control.Monad
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Text (unpack, pack, Text)
import Hledger.Utils.Regex
import Network.IRC.Client (Source)
import Safe
import Text.Regex.TDFA
import Zn.Bot
import Zn.Commands.Logs
import qualified Zn.Grammar as Gr
import Zn.IRC
import Zn.Types

type SubstArgs = ((String, String), String)

unhigh :: String -> String
unhigh = replaceRegex (toRegex "(\x02|\x03[0-9]{1,2}|\x1d|\x1f|\x16|\x0f)") ""

highlight :: String -> String
highlight text = "\x02\x1d\x03" ++ "04" ++ text ++ "\x0f"

subst :: SubstArgs -> String -> Maybe String
subst ((regex', repl), flags) msg = replaced msg <$ (matchM regex msg :: Maybe String)

    where
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

printMaybe :: PrivEvent Text -> Bot (Maybe (Maybe String)) -> Bot ()
printMaybe pr = join . fmap (sequence_ . fmap (reply pr . pack) . join)

sed :: PrivEvent Text -> Bot ()
sed pr = printMaybe pr $ Gr.ifParse Gr.sed (view cont pr) $
        \cmds@((_, flags):_) -> do
            nick <- Bot $ unpack <$> getNick
            hist <- tailor nick source flags <$> use history

            return . pick . chain cmds $ (text %~ unhigh) <$> toList hist

    where
        pick = fmap (view text) . flip atMay 0 :: [Line a] -> Maybe a
        chain = foldr1 (.) . fmap s
        s cmd = catMaybes . fmap (text $ subst cmd) :: [Line String] -> [Line String]
        source = view src $ fmap unpack pr
