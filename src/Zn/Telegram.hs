{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Exception
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Compose
import Data.List hiding (isInfixOf)
import Data.Maybe
import Data.Text (Text, pack, isInfixOf)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.IRC.Client (runIRCAction, IRCState)
import qualified Network.IRC.Client as IRC
import Text.Printf
import Web.Telegram.API.Bot as T
import Zn.Bot
import Zn.Bot.Handle
import Zn.Telegram.Message
import Zn.Telegram.Types
import Zn.Types

apiFileURL = "https://api.telegram.org/file/%s/%s"

-- [(anonymized name, largest photo)]
summarize :: Update -> [ZnUpdateSummary]
summarize updates = updates &
    flatMaybe (\(update_id, Message {
        T.from    = Just user,
        T.caption = caption,
        T.photo   = mby_photos,
        T.text    = mby_text,
        T.video   = mby_video
      }) ->
        (update_id, user, ) <$>
          (ZnText <$> mby_text) `mplus`
          (ZnPhoto . (caption, ) . largest_file <$> mby_photos) `mplus`
          (ZnVideo . (caption, ) . (\(Video { video_file_id = vfid }) -> vfid) <$> mby_video)
    )

    . (\(Update { update_id = uid, message = m }) -> (uid, ) <$> m)

  where
    flatMaybe :: Foldable t => (a -> Maybe b) -> t a -> [b]
    flatMaybe = concatMap . (maybeToList .)

    largest_file = fst . head . reverse . sortOn snd . map
        (\(PhotoSize { photo_file_size = Just pfs, photo_file_id = pid }) ->
            (pid, pfs))

telegramConsume :: Token -> Update -> TelegramClient [UpdateSummary (ZnTgMsg Text LinkCaptionMsg LinkCaptionMsg)]
telegramConsume token =
    fmap catMaybes .
    sequence .
    join .
    mapM (\x -> map (getCompose . ($ x)) $ extractors) .
    summarize
  where
    extractors =
      [ (_3 . _ZnPhoto . _2 $ Compose . links token)
      , (_3 . _ZnVideo . _2 $ Compose . links token)
      ]

    links :: Token -> TgFileId -> TelegramClient (Maybe FileLink)
    links (Token token) pid =
      (pack . printf apiFileURL token <$>) <$> file_path . result <$> getFileM pid

telegramMain :: Token -> (Update -> TelegramClient [a]) -> IO [a]
telegramMain token consumer =
    dieOnError . runClient' $ do
        updates <- result <$> getUpdatesM updatesRequest
        (concat <$> mapM consumer updates) <* markRead updates

  where
    dieOnError = fmap $ either (error . show) id
    runClient' x = runClient x token =<< newManager tlsManagerSettings
    updatesRequest = GetUpdatesRequest Nothing (Just 100) (Just 600) (Just ["message"])
    markRead updates =
        when (length updates > 0) . void $
            getUpdatesM (updatesRequest
                { updates_offset = Just . (+1) . update_id . last $ updates
                , updates_timeout = Just 5
                })

telegramPoll :: IRCState BotState -> IO ()
telegramPoll ircst = flip runIRCAction ircst . runBot $ do
    forever . handleWith (liftIO . complainUnlessTimeout) $ do
        pr <- privEvent "" . (`IRC.Channel` "") <$> param "telegram-target"
        token <- Token <$> param "telegram-token"

        zn_msgs <- liftIO . (evaluate =<<) $
            telegramMain token (telegramConsume token)

        void . forOf each zn_msgs $
            \update@(_, (User { user_id = user_id }), zn_msg) -> do
                let banned = user_id == 605094437
                dbg <- use debug

                when (not banned && dbg) $ do
                    master <- param "master"
                    Bot . IRC.send $ IRC.Privmsg master (Right $ pack $ show user_id)

                when (not banned) $ do
                    telegramMsg pr update

  where
    complainUnlessTimeout :: SomeException -> IO ()
    complainUnlessTimeout e = do
        when (pack (show e) `isInfixOf` "RequestTimeout}") $
            handlePrinter "telegram" e
