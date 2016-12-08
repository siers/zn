module Zn.Handlers where

import Control.Monad
import Data.Ini
import Data.Text as T hiding (head)
import Network.IRC.Client
import Zn.Bot
import Zn.Commands
import Zn.Data.Ini

initHandler :: Ini -> Bot ()
initHandler conf = do
    send . Nick $ setting conf "user"
    getTVar stateTVar >>= send . Privmsg (setting conf "master") . Right . pack . show . bootTime
    send . Privmsg "nickserv" . Right $ "id " `append` (setting conf "pass")
    mapM_ (send . Join) . split (== ',') $ setting conf "chans"

cmdHandler :: UnicodeEvent -> Bot ()
cmdHandler ev = do
    ignores <- splitOn "," . flip setting "ignores" . config <$> getTVar stateTVar

    when (not $ ignore ignores ev) $
        mapM_ ($ ev) commands

    save
