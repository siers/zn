module Zn.Handlers where

import Control.Lens
import Control.Monad
import Data.Ini
import Data.Text as T hiding (head)
import Network.IRC.Client
import Zn.Bot
import Zn.Commands
import Zn.Commands.Logs
import Zn.Data.Ini

initHandler :: Ini -> StatefulBot ()
initHandler conf = do
    send . Nick $ parameter conf "user"
    stateful (use bootTime) >>= send . Privmsg (parameter conf "master") . Right . pack . show
    send . Privmsg "nickserv" . Right $ "id " `append` (parameter conf "pass")
    mapM_ (send . Join) . split (== ',') $ parameter conf "chans"

cmdHandler :: UnicodeEvent -> StatefulBot ()
cmdHandler ev = do
    ignores <- splitOn "," . flip parameter "ignores" <$> stateful (use config)

    runBot $ do
        logs ev

        when (not $ ignore ignores ev) $
            mapM_ ($ ev) commands

        save
