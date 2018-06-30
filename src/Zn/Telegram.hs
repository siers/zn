{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Exception
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List hiding (isInfixOf)
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack, isInfixOf)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.IRC.Client as IRC
import Network.IRC.Client (runIRCAction, IRCState)
import Text.Printf
import Web.Telegram.API.Bot as T
import Zn.Bot
import Zn.Bot.Handle
import Zn.IRC hiding (from)
import Zn.Types
import Zn.Telegram.Photo
import Zn.Telegram.Types

apiFileURL = "https://api.telegram.org/file/%s/%s"

-- [(anonymized name, largest photo)]
summarize :: [Update] -> [UpdateSummary PhotoSizeMsg Text]
summarize updates = updates &
    flatMaybe (\(update_id, Message {
        from      = Just user,
        T.caption = caption,
        photo     = mby_photos,
        T.text    = mby_text
      }) ->
        fmap (update_id, user, ) $
          (ZnPhoto . (caption, ) . largest <$> mby_photos)
          `mplus`
          (ZnText <$> mby_text))

    . flatMaybe (\(Update { update_id = uid, message = m }) -> (uid, ) <$> m)

  where
    flatMaybe :: Foldable t => (a -> Maybe b) -> t a -> [b]
    flatMaybe = concatMap . (maybeToList .)

    largest = head . reverse . sortOn
        (\(PhotoSize { photo_file_size = Just pfs }) -> pfs)

telegramMain :: Token -> IO [UpdateSummary PhotoMsg Text]
telegramMain token =
    dieOnError . runClient' $ do
        updates <- result <$> getUpdatesM updatesRequest
        catMaybes <$> extractLinks updates <* markRead updates

  where
    dieOnError = fmap $ either (error . show) id
    runClient' x = runClient x token =<< newManager tlsManagerSettings
    updatesRequest = GetUpdatesRequest Nothing (Just 100) (Just 600) (Just ["message"])

    extractLinks =
      (fmap . fmap) (_3 . _ZnPhoto . _2 $ id) -- Maybe as the lens functor
      . mapM (_3 . _ZnPhoto . _2 $ links token) -- IO as the lens functor
      . summarize

    links :: Token -> PhotoSize -> TelegramClient (Maybe PhotoLink)
    links (Token token) (PhotoSize { photo_file_id = pid }) = do (printf apiFileURL token . unpack <$>) <$> file_path . result <$> getFileM pid

    markRead updates =
        when (length updates > 0) . void $
            getUpdatesM (updatesRequest
                { updates_offset = Just . (+1) . update_id . last $ updates
                , updates_timeout = Just 5
                })

telegramPoll :: IRCState BotState -> IO ()
telegramPoll ircst = flip runIRCAction ircst . runBot $ do
    pr <- privEvent "" . (`IRC.Channel` "") <$> param "telegram-target"

    forever . handleWith (liftIO . complainUnlessTimeout) $ do
        zn_msgs <- liftIO . (evaluate =<<) . telegramMain . Token =<< param "telegram-token"

        void . forOf each zn_msgs $
            \update@(_, (User { user_id = user_id, user_first_name = who }), zn_msg) -> do
                when (user_id /= 605094437) $ do
                    dbg <- use debug
                    when dbg $ do
                      master <- param "master"
                      Bot . IRC.send $ IRC.Privmsg master (Right $ pack $ show user_id)

                    zn_msg_text <- zn_msg & (_ZnPhoto %%~ (const $ handlePhoto pr update))
                    reply pr . formatMsg who $ znMsgJoin zn_msg_text

  where
    complainUnlessTimeout :: SomeException -> IO ()
    complainUnlessTimeout e = do
        when (pack (show e) `isInfixOf` "RequestTimeout}") $
            handlePrinter "telegram" e

    formatMsg :: Text -> Text -> Text
    formatMsg who text = fold ["<", who, "> "] <> text
