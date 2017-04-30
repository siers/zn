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

subst :: Logs String -> ((String, String), String) -> Maybe String
subst history ((regex', theSubst), flags) =

    join . find isJust . fmap subst . fmap unhigh . fmap (view text) $ history

    where
        subst msg = replacer regex (highlight theSubst) msg <$ (matchM regex msg :: Maybe String)

        -- affected by flags
        regex = ($ regex') $ if 'i' `elem` flags then toRegexCI else toRegex
        replacer = if 'g' `elem` flags then replaceRegex else replaceRegexSingle

tailor :: Source String -> String -> History Text -> Bot (Logs String)
tailor source flags logs = do
    nick <- Bot $ unpack <$> getNick

    return $
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
            latestHist <- tailor source flags =<< use history
            return $ subst latestHist args

    where
        source = view src $ fmap unpack pr
