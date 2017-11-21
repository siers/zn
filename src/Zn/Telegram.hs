{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Arrow
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.IRC.Client as IRC
import Network.IRC.Client (runIRCAction, IRCState)
import Text.Printf
import Web.Telegram.API.Bot as T
import Zn.Bot
import Zn.Bot.Handle
import Zn.Commands.URL.Main
import Zn.IRC hiding (from)
import Zn.Types

apiFileURL = "https://api.telegram.org/file/%s/%s"

-- A rather injective function into meaningless names.
anonymize :: Text -> Text
anonymize = const "withheld"

links :: Token -> PhotoSize -> TelegramClient (Maybe (String, String))
links (Token token) (PhotoSize { photo_file_id = pid }) = do
    file <- result <$> getFileM pid
    return $ (take 16 $ unpack $ pid, ) . printf apiFileURL token <$> file_path file

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

telegramMain :: Token -> IO [(Text, (String, String))]
telegramMain token = do
    fmap (either (error . show) id) $
        (\x -> runClient x token =<< newManager tlsManagerSettings) $ do
            updates <- result <$> getUpdatesM updatesRequest
            posts <- extractLinks (links token) (summarize updates)

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
            (b -> TelegramClient (Maybe c)) ->
            [(a, b)] -> TelegramClient [(a, c)]
        extractLinks a = fmap (catMaybes . fmap sequence) . traverse (monadOver_2 a)

telegramPoll :: IRCState BotState -> IO ()
telegramPoll ircst = flip runIRCAction ircst . runBot $ do
    token <- Token <$> param "telegram-token"
    root <- param "http-root"

    forever . handleLabeledWithPrint "telegram" (return $ sleep 5) $ do
        pics <- liftIO $ telegramMain token

        target <- param "telegram-target"
        pr <- return $ PrivEvent "" (IRC.Channel target "")

        flip mapM_ pics $ \(who, (id, link)) -> do
            let pathslug = (Just $ "telegram-" <> id <> ".jpg")
            resp@(path, bh) <- download pathslug pr link

            let url = unpack root <> "/" <> path
            reply pr . pack $ printf "“%s” sends: %s" (unpack who) url
            process pr (_2 %~_2 %~ redoType $ resp)

    where
        redoType = (("content-type", "image/jpeg"): ) . filter (\h -> fst h /= "content-type")
        -- set content type to imageish, not application/octet-stream
        -- which is probably there because some dummy wanted to force downloads
