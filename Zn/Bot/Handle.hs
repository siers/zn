module Zn.Bot.Handle
    ( handle
    ) where

import Control.Exception (AsyncException(..))
import Control.Monad.Catch hiding (handle)
import Control.Monad.IO.Class

handle :: (MonadCatch m, MonadIO m) => m () -> m ()
handle a = catchJust
    (onlyNot ThreadKilled)
    a
    (liftIO . print :: MonadIO m => SomeException -> m ())

    where
        print = Prelude.putStrLn . ("*** zn-caught exception: " ++) . show
        onlyNot not e =
            if fromException e == Just not
            then Nothing
            else Just e
