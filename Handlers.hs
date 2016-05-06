module Handlers where

import Bot
import Data.Ini
import Data.Text as T hiding (head)
import Network.IRC.Client

initHandler :: Ini -> StatefulIRC BotState ()
initHandler conf = do
    send . Nick $ setting conf "user"
    getTVar stateTVar >>= send . Privmsg (setting conf "master") . Right . pack . show . bootTime
    send . Privmsg "nickserv" . Right $ "id " `append` (setting conf "pass")
    mapM_ (send . Join) . split (== ',') $ setting conf "chans"
