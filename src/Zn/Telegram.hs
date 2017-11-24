{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Zn.Telegram where

import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
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
telegramMain token = do
    fmap (either (error . show) id) $
        (\x -> runClient x token =<< newManager tlsManagerSettings) $ do
            -- Add prints to inspect input from telegram here below.
            updates <- result <$> getUpdatesM updatesRequest
            posts <- catMaybes <$> mapAction_4 (links token) (summarize updates)

            when (length updates > 0) $ -- mark read
                void $ getUpdatesM (updatesRequest
                    { updates_offset = Just . (+1) . update_id . last $ updates
                    , updates_timeout = Just 5
                    })

            return posts

    where
        updatesRequest = GetUpdatesRequest Nothing (Just 100) (Just 600) (Just ["message"])
        mapAction_4 a = (fmap . fmap) (traverseOf _4 id) . mapM (traverseOf _4 a)
        -- I can't generalize the _3 to any lens-like variable. (can't no matter what)
        -- I can't add a catMaybes inside the this function's definiton. (maybe can't)

telegramPoll :: IRCState BotState -> IO ()
telegramPoll ircst = flip runIRCAction ircst . runBot $ do
    token <- Token <$> param "telegram-token"
    root <- param "http-root"

    forever . handleLabeledWithPrint "telegram" (return $ sleep 5) $ do
        pics <- liftIO $ telegramMain token
        target <- param "telegram-target"
        pr <- return $ PrivEvent "" (IRC.Channel target "")

        flip mapM_ pics $ \(uid, caption, who, link) -> do
            let components = fmap unpack ["telegram", who, pack $ show uid, canonicalForm $ fromMaybe "" caption]
            let pathslug = Just $ intercalate "-" ((not . null) `filter` components) <> ".jpg"
            resp@(path, bh) <- download pathslug pr link

            let url = unpack root <> BL.unpack (B.toLazyByteString (encodePathSegments [pack path]))
            reply pr . pack $ printf "“%s” sends: %s%s"
                (unpack who) (unpack . fromMaybe "" $ (<> " ") <$> caption) url
            process pr (_2 %~_2 %~ redoType $ resp)

    where
        -- set content type to imageish, not application/octet-stream
        -- which is probably there because some dummy wanted to force downloads
        redoType = (("content-type", "image/jpeg"): ) . filter (\h -> fst h /= "content-type")

        -- https://stackoverflow.com/questions/44290218/how-do-you-remove-accents-from-a-string-in-haskell
        canonicalForm :: Text -> Text
        canonicalForm = T.filter (not . property Diacritic) . normalize NFD
