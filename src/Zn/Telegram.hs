{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Exception
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Compose
import Data.List hiding (isInfixOf, intercalate, insert)
import Data.Maybe
import Data.Text (Text, pack, isInfixOf, split, intercalate)
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

import Database.Groundhog as G
import Zn.Persist
import Safe
import GHC.Int (Int64)
import qualified Data.Map as M

apiFileURL = "https://api.telegram.org/file/%s/%s"

-- [(anonymized name, largest photo)]
summarize :: Update -> [ZnUpdateSummary]
summarize updates = updates &
    flatMaybe (\(update_id, msg@(Message {
        T.from     = Just user,
        T.caption  = caption,
        T.photo    = mby_photos,
        T.text     = mby_text,
        T.document = mby_doc,
        T.video    = mby_video
      })) ->
        (update_id, msg, user, ) <$>
          (ZnText <$> mby_text) `mplus`
          (ZnDoc . (caption, ) . (\(Document { doc_file_id = fid }) -> fid) <$> mby_doc) `mplus`
          (ZnPhoto . (caption, ) . largest_file <$> mby_photos) `mplus`
          (ZnDoc . (caption, ) . (\(Video { video_file_id = fid }) -> fid) <$> mby_video)
    )

    . (\(Update { update_id = uid, message = m }) -> (uid, ) <$> m)

  where
    flatMaybe :: Foldable t => (a -> Maybe b) -> t a -> [b]
    flatMaybe = concatMap . (maybeToList .)

    largest_file = fst . head . reverse . sortOn snd . map
        (\(PhotoSize { photo_file_size = Just pfs, photo_file_id = pid }) ->
            (pid, pfs))

telegramConsume :: Token -> Update -> TelegramClient [ZnUpdateSummary]
telegramConsume token =
    fmap catMaybes .
    mapM (\update -> foldM (flip ($)) (Just update) extractors) .
    summarize
  where
    extractors :: [Maybe ZnUpdateSummary -> TelegramClient (Maybe ZnUpdateSummary)]
    extractors = map (\f -> fmap join . sequence . (>>= Just . getCompose . f))
      [ (_4 . _ZnPhoto . _2 $ Compose . links token)
      , (_4 . _ZnDoc . _2 $ Compose . links token)
      ]

    links :: Token -> TgFileId -> TelegramClient (Maybe FileLink)
    links (Token token) pid =
      (pack . printf apiFileURL token <$>) <$> file_path . result <$> getFileM pid

targetKeys :: [Text]
targetKeys = map (pack . show) [1..9]

targetList :: [Text] -> [(Text, Text)]
targetList = zip targetKeys

replySetTarget :: [(Text, Text)] -> Int64 -> Text -> (Text -> TelegramClient ()) -> TelegramClient ()
replySetTarget targets chatId choice sendTg = do
  liftIO . sql $ do
      fact <- headMay <$> select (TgTargetKeyField ==. (fromIntegral chatId :: Int))
      if isJust fact
      then void $
          update [TgTargetChannelField =. target] (TgTargetKeyField ==. (fromIntegral chatId :: Int))
      else void $
          insert (TgTarget (fromIntegral chatId) target)

  sendTg "Acknowledged!"

  where
      target = M.fromList targets M.! choice

telegramTalk :: [Text] -> Token -> Update -> TelegramClient [ZnUpdateSummary]
telegramTalk targets token update = do
  command <-
    if isJust text
    then talk' message text
    else return False

  if command
  then return []
  else telegramConsume token update

  where
    message = T.message update
    text = message >>= T.text

    talk' m t = talk (fromJust m) (fromJust t)
    talk message text
      | any (text ==) [",target", ",t"] = True <$ replyTargetOptions
      | any (text ==) (take (length targets) targetKeys) = True <$ (replySetTarget targetL chatId text (void . sendTg))
      | otherwise = return False

      where
        targetL = targetList targets

        replyTargetOptions = sendTg $
          "Please set the bridge output destination:\n"
          `mappend`
          ("\n" `intercalate` ((\(k, v) -> k <> ": " <> v) `map` targetL))

        chatId = chat_id . chat $ message
        sendTg text = sendMessageM $ SendMessageRequest (ChatId chatId) text
          Nothing Nothing Nothing Nothing Nothing

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
    forever . handleLabeled "telegram" $ do
        targets <- split (== ',') <$> param "telegram-targets"
        token <- Token <$> param "telegram-token"

        zn_msgs <- liftIO . (evaluate =<<) $
            telegramMain token $
                seq
                    (\_ -> return [] :: TelegramClient [a])
                    (telegramTalk targets token)

        void . forOf each zn_msgs $
            \update@(_, _, (User { user_id = user_id }), zn_msg) -> do
                let banned = user_id == 605094437
                dbg <- use debug

                when (not banned && dbg) $ do
                    master <- param "master"
                    Bot . IRC.send $ IRC.Privmsg master (Right $ pack $ show user_id)

                when (not banned) $ do
                    telegramMsg targets update

  where
    complainUnlessTimeout :: SomeException -> IO ()
    complainUnlessTimeout e = do
        when (pack (show e) `isInfixOf` "RequestTimeout}") $
            handlePrinter "telegram" e
