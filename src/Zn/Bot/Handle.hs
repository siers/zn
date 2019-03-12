module Zn.Bot.Handle
    ( handle
    , handleWith
    , handleWithPrint
    , handleLabeled
    , handleLabeledWithPrint
    , handlePrinter
    ) where

import Control.Exception (AsyncException(..))
import Control.Monad.Catch hiding (handle)
import Control.Monad.IO.Class
import Text.Printf

onlyNot :: AsyncException -> SomeException -> Maybe SomeException
onlyNot not e =
    if fromException e == Just not
    then Nothing
    else Just e

-- The names aren't the best out there, but, essentially,
-- Label = include text into the error message,
-- Print = allow handling the exception while printing out the error.

handleWith, handleWithPrint ::
    (MonadCatch m, MonadIO m) =>
        (SomeException -> m a) -> m a -> m a

handleWith catcher a = catchJust (onlyNot ThreadKilled) a catcher
handleWithPrint = handleLabeledWithPrint ""

handleLabeled :: (MonadCatch m, MonadIO m) => String -> m () -> m ()
handleLabeled = flip handleLabeledWithPrint (\_ -> return ())

handleLabeledWithPrint :: (MonadCatch m, MonadIO m) => String -> (SomeException -> m a) -> m a -> m a
handleLabeledWithPrint label catcher = handleWith $ \e -> handlePrinter label e >> catcher e

handlePrinter :: MonadIO m => String -> SomeException -> m ()
handlePrinter label e =
    liftIO . putStrLn . (printf "*** zn-caught exception%s: " label' ++) . displayException $ e
    where label' = (if null label then "" else printf "(%s)" label) :: String

handle :: (MonadCatch m, MonadIO m) => m () -> m ()
handle = handleWithPrint $ const $ return ()
