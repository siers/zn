module Zn.Handlers.Kick
    ( kickHandler
    ) where

import Zn.Bot
import Network.IRC.Client hiding (reply, kickHandler)

-- use Network.IRC.Client's kickHandler for now
kickHandler :: EventHandler a
kickHandler = EventHandler (matchType _Kick) $ \_src (chan, _nick, _reason) ->
    sleep 1 >> send (Join chan)
