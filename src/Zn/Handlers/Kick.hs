module Zn.Handlers.Kick
    ( kickHandler
    ) where

import Zn.Bot
import Network.IRC.Client hiding (reply)

kickHandler :: EventHandler a
kickHandler = EventHandler (matchType _Kick) $ \_src (chan, _nick, _reason) ->
    sleep 1 >> send (Join chan)
