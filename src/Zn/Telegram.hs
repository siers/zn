{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Exception
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.List hiding (isInfixOf)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text.ICU
import Data.Text.ICU.Char
import Data.Text.ICU.Replace
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text, pack, unpack, isInfixOf)
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
    pr <- privEvent "" . (`IRC.Channel` "") <$> param "telegram-target"

    forever . handleWith (liftIO . complainUnlessTimeout) $ do
        pics <- liftIO . (evaluate =<<) . telegramMain . Token =<< param "telegram-token"

        void . forOf each pics $ \update@(uid, caption, who, link) -> do
            path <- store pr update
            nsfw <- (fmap (\n -> "(" <> n <> ")") . formatNSFW =<<) <$> detectNSFW path
            reply pr $ formatMsg who caption (formatUrl root path) nsfw

    where
        complainUnlessTimeout :: SomeException -> IO ()
        complainUnlessTimeout e = do
            when (pack (show e) `isInfixOf` "RequestTimeout}") $
                handlePrinter "telegram" e

        -- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell
        canonicalForm :: Text -> Text
        canonicalForm = T.filter (not . property Diacritic) . normalize NFD

        store :: PrivEvent Text -> UpdateSummary -> Bot String
        store pr (uid, caption, who, link) = fst <$> download pathslug pr link
            where
                limitChar = T.dropAround (== '-') . replaceAll (regex [] "[^a-z0-9\\-~+=]+") (rtext "-")
                captionSlug = T.take 48 . limitChar . T.toLower . canonicalForm . fromMaybe ""
                components = unpack <$> ["telegram", who, pack $ show uid, captionSlug caption]
                pathslug = Just $ intercalate "-" ((not . null) `filter` components) <> ".jpg"

        formatUrl :: Text -> String -> Text -- "http://x" "/ " => "http://x/%20"
        formatUrl root path = (root `mappend`) . TL.toStrict . TLE.decodeUtf8 $
            B.toLazyByteString (encodePathSegments [pack path])

        formatMsg :: Text -> Maybe Text -> Text -> Maybe String -> Text
        formatMsg who caption url nsfw = fold ["<", who, "> "] <> T.intercalate " " components
            where
                components = catMaybes [caption, Just $ url, pack <$> nsfw]
