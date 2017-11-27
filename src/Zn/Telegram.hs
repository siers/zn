{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import Data.Text (Text, pack, unpack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.URI
import qualified Network.IRC.Client as IRC
import Network.IRC.Client (runIRCAction, IRCState)
import Numeric
import Text.Printf
import Web.Telegram.API.Bot as T
import Zn.Bot
import Zn.Bot.Handle
import Zn.Commands.URL.Detect
import Zn.Commands.URL.Format
import Zn.Commands.URL.Main
import Zn.IRC hiding (from)
import Zn.Types

-- (UpdateID, Maybe Caption, Pseudonym, a)
-- a = PhotoSize or PhotoLink (link of the largest of `PhotoSize's)
type UpdateSummary' a = (Int, Maybe Text, Text, a)
type UpdateSummary = UpdateSummary' PhotoLink

type PhotoLink = String

apiFileURL = "https://api.telegram.org/file/%s/%s"

-- A rather injective function into meaningless names.
anonymize :: Text -> Text
anonymize seed = pack . take 6 $ (alphabet !!) . (`mod` l) <$> iterate (`div` l) rnd
    where
        alphabet = "aaadddeeeiiiimmmooppprrrssssuuuzzzzz"
        l = length alphabet
        h = hash :: BS.ByteString -> Digest Keccak_512
        rnd = fst . head . readHex . show . h . BS.pack . unpack $ seed

links :: Token -> PhotoSize -> TelegramClient (Maybe PhotoLink)
links (Token token) (PhotoSize { photo_file_id = pid }) = do
    (printf apiFileURL token . unpack <$>) <$> file_path . result <$> getFileM pid

-- [(anonymized name, largest photo)]
summarize :: [Update] -> [UpdateSummary' PhotoSize]
summarize updates = ($ updates)
    $ map ( _4 %~ head . reverse . sortOn (\(PhotoSize { photo_file_size = Just pfs }) -> pfs))

    . flatMaybe (\(uid, Message {
        from = Just (User { user_first_name = f, user_last_name = l }),
        T.caption = caption,
        photo = mby_photos }) ->

        (uid, caption, anonymize $ f <> fromMaybe "" l, ) <$> mby_photos)

    . flatMaybe (\(Update { update_id = uid, message = m }) -> (uid, ) <$> m)

    where
        flatMaybe :: Foldable t => (a -> Maybe b) -> t a -> [b]
        flatMaybe = concatMap . (maybeToList .)

telegramMain :: Token -> IO [UpdateSummary]
telegramMain token =
    dieOnError . runClient' $ do
        updates <- result <$> getUpdatesM updatesRequest
        catMaybes <$> extractLinks updates <* markRead updates

    where
        dieOnError = fmap $ either (error . show) id
        runClient' x = runClient x token =<< newManager tlsManagerSettings
        updatesRequest = GetUpdatesRequest Nothing (Just 100) (Just 600) (Just ["message"])

        extractLinks = fmap (_4 id <$>) . mapM (_4 $ links token) . summarize

        markRead updates =
            when (length updates > 0) . void $
                getUpdatesM (updatesRequest
                    { updates_offset = Just . (+1) . update_id . last $ updates
                    , updates_timeout = Just 5
                    })

telegramPoll :: IRCState BotState -> IO ()
telegramPoll ircst = flip runIRCAction ircst . runBot $ do
    root <- param "http-root"
    pr <- PrivEvent "" . (`IRC.Channel` "") <$> param "telegram-target"

    forever . handleLabeledWithPrint "telegram" (return $ sleep 5) $ do
        pics <- liftIO . telegramMain . Token =<< param "telegram-token"

        void . forOf each pics $ \(uid, caption, who, link) -> do
            let components = unpack <$> ["telegram", who, pack $ show uid, canonicalForm $ fromMaybe "" caption]
            let pathslug = Just $ intercalate "-" ((not . null) `filter` components) <> ".jpg"
            resp@(path, _bh) <- download pathslug pr link

            nsfw <- (fmap (\n -> "(" <> n <> ")") . formatNSFW =<<) <$> detectNSFW path
            let url = unpack root <> BL.unpack (B.toLazyByteString (encodePathSegments [pack path]))
            let components = catMaybes [caption, Just $ pack url, pack <$> nsfw]
            reply pr $ fold ["<", who, "> "] <> T.intercalate " " components

    where
        -- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell
        canonicalForm :: Text -> Text
        canonicalForm = T.filter (not . property Diacritic) . normalize NFD
