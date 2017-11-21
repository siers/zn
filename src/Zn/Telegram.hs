{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Arrow
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Printf
import Web.Telegram.API.Bot as T
import Zn.Types

-- A rather injective function into meaningless names.
anonymize :: Text -> Text
anonymize = const "withheld"

download :: Token -> PhotoSize -> TelegramClient (Maybe String)
download (Token token) (PhotoSize { photo_file_id = pid }) =
    fmap (printf "https://api.telegram.org/file/%s/%s" token)
    . file_path . result
    <$> getFileM pid

-- [(anonymized name, largest photo)]
summarize :: [Update] -> [(Text, PhotoSize)]
summarize updates = ($ updates)
    $ map ( _2 %~ head . reverse . sortOn (\(PhotoSize { photo_file_size = Just pfs }) -> pfs))

    . flatMaybe (\(Message {
        from = Just (User { user_first_name = f, user_last_name = l }),
        photo = mby_photos }) ->

        (anonymize $ f <> fromMaybe "" l, ) <$> mby_photos)

    . flatMaybe (\(Update { message = m }) -> m)

    where
        flatMaybe :: Foldable t => (a -> Maybe b) -> t a -> [b]
        flatMaybe = concatMap . (maybeToList .)

telegramMain :: Token -> IO [(Text, String)]
telegramMain token = do
    fmap (either (error . show) id) $
        (\x -> runClient x token =<< newManager tlsManagerSettings) $ do
            updates <- result <$> getUpdatesM updatesRequest
            posts <- extractLinks (download token) (summarize updates)

            when (length updates > 0) $ -- mark read
                void $ getUpdatesM (updatesRequest
                    { updates_offset = Just . (+1) . update_id . last $ updates
                    , updates_timeout = Just 5
                    })

            return posts

    where
        updatesRequest = GetUpdatesRequest Nothing (Just 100) (Just 600) (Just ["message"])

        monadOver_2 :: Monad m => (b -> m c) -> (d, b) -> m (d, c)
        monadOver_2 = runKleisli . second . Kleisli

        extractLinks ::
            (PhotoSize -> TelegramClient (Maybe String)) ->
            [(Text, PhotoSize)] -> TelegramClient [(Text, String)]

        extractLinks a = fmap (catMaybes . fmap sequence) . traverse (monadOver_2 a)
