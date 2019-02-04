{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Exception
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List hiding (isInfixOf)
import Data.Maybe
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

telegramMsg :: PrivEvent Text -> UpdateSummary (ZnTgMsg Text PhotoMsg ()) -> Bot ()
telegramMsg pr update@(_, (User { user_first_name = who }), zn_msg) = do
    zn_msg & (_ZnPhoto %%~ handlePhoto) >>=
        reply pr . formatMsg who . znMsgJoin
  where
    formatMsg :: Text -> Text -> Text
    formatMsg who text = fold ["<", who, "> "] <> text

    handlePhoto :: PhotoMsg -> Bot Text
    handlePhoto (caption, link) =
        handleFile pr (photoPath pr update) link
        >>= formatPhoto caption

-- [(anonymized name, largest photo)]
summarize :: Update -> [ZnUpdateSummary]
summarize updates = updates &
    flatMaybe (\(update_id, Message {
        from      = Just user,
        T.caption = caption,
        photo     = mby_photos,
        T.text    = mby_text
        -- video     = mby_video
      }) ->
        (update_id, user, ) <$>
          (ZnText <$> mby_text) `mplus`
          (ZnPhoto . (caption, ) . largest_file <$> mby_photos) `mplus`
          (ZnFile <$> Just ())
    )

    . (\(Update { update_id = uid, message = m }) -> (uid, ) <$> m)

  where
    flatMaybe :: Foldable t => (a -> Maybe b) -> t a -> [b]
    flatMaybe = concatMap . (maybeToList .)

    largest_file = fst . head . reverse . sortOn snd . map
        (\(PhotoSize { photo_file_size = Just pfs, photo_file_id = pid }) ->
            (pid, pfs))

telegramConsume :: Token -> Update -> TelegramClient [UpdateSummary (ZnTgMsg Text PhotoMsg ())]
telegramConsume token =
    fmap (catMaybes . map (_3 . _ZnPhoto . _2 $ id)) -- Maybe as the lens functor
    . mapM (_3 . _ZnPhoto . _2 $ links token) -- IO as the lens functor
    . summarize
  where
    links :: Token -> PhotoFileId -> TelegramClient (Maybe PhotoLink)
    links (Token token) pid =
      (printf apiFileURL token . unpack <$>) <$> file_path . result <$> getFileM pid

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
