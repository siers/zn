module Zn.Handlers.Join
    ( joinFailHandler
    ) where

import Control.Lens hiding (from)
import Network.IRC.Client hiding (reply)
import Zn.Bot
import Zn.Types
import Zn.Data.Ini
import qualified Data.Text as T

-- net 477 zn #developerslv :Cannot join channel (+r) - you need to be identified with services - see https://freenode.net/kb/answer/registration
joinFailHandler :: EventHandler BotState
joinFailHandler = EventHandler (matchNumeric 477) $
    \src texts -> do
        flip mapM_ [1..3] $ \_ -> do
          sleep 7
          mapM_ (send . Join) . T.split (== ',') . (`parameter` "chans") =<< use config
