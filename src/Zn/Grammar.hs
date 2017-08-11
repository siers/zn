{-# LANGUAGE TypeFamilies #-}

module Zn.Grammar
    ( ifParse
    , matches
    , sed
    , addressed
    , shellish
    )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor.Identity
import Data.List.Split (splitOn)
import Data.Text (unpack, Text)
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

ifParse :: Monad m => Parser a -> Text -> (a -> m b) -> m (Maybe b)
ifParse parser msg action =
    either
        (return . const Nothing)
        (fmap Just . action) $
        parse parser "" (unpack msg)

matches :: Parser a -> Text -> Maybe a
matches p s = runIdentity $ ifParse p s return

--

subst :: Parser ((String, String), String)
subst = (,) <$> (string "s" *> body) <*> (many $ oneOf ("gimrl" :: String))
    where
        body = do
            delim <- oneOf ("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~" :: String)
            (,)
                <$> escaped [delim] <* char delim
                <*> escaped [delim] <* (() <$ char delim <|> eof)

sed :: Parser [((String, String), String)]
sed = sepBy1 subst (string ";" *> space) <* eof

--

data Shellish = And | Usual String deriving (Show, Eq)

escaped :: String -> Parser String
escaped escape = many $ (char '\\' *> (hex <|> anyChar)) <|> (noneOf escape)
    where hex = char 'x' *> fmap (chr . fromIntegral) L.hexadecimal

str :: Char -> Parser String
str = (\q -> between (char q) (char q) (escaped [q]))

sentence :: Parser a -> Parser [a]
sentence from = sepBy1 from (skipSome spaceChar)

shellishTokens :: Parser [Shellish]
shellishTokens = sentence $ special <|> usual
    where
        special = And <$ string "&&"
        usual = fmap Usual $ str '\"' <|> str '\'' <|> escaped " \"'"

shellish :: Parser [[String]]
shellish = (fmap . fmap) content . splitOn [And] <$> shellishTokens
    where content (Usual c) = c

--

addressed :: String -> Parser String
addressed nick = by <|> fromMiddle
    where
        by = (byName <|> byPrefix) *> many anyChar
        byName = string nick *> oneOf (":," :: String) *> space
        byPrefix = void $ oneOf ("!," :: String)

        findFirst p = p <|> (anyChar *> findFirst p)

        fromMiddle = findFirst $
            char '#' *> (between (char '(') (char ')') (escaped ")"))
