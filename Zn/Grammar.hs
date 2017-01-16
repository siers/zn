module Zn.Grammar where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor.Identity
import Data.List
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

ifParse :: Monad m => Parser a -> String -> (a -> m b) -> m (Maybe b)
ifParse parser msg action =
    either
        (return . const Nothing)
        (fmap Just . action) $
        parse parser "" msg

matches :: Parser a -> String -> Maybe a
matches p s = runIdentity $ ifParse p s return

--

sed :: Parser ((String, String), String)
sed = (,) <$> (string "s" *> body) <*> (many $ oneOf "gi")
    where
        body = do
            delim <- anyChar
            (,)
                <$> escaped [delim] <* char delim
                <*> escaped [delim] <* char delim

--

escaped :: String -> Parser String
escaped escape = many $ (char '\\' *> (hex <|> anyChar)) <|> (noneOf escape)
    where
        hex = char 'x' *> fmap (chr . read . ("0x" ++)) (count 2 $ satisfy isHexDigit)

str :: Char -> Parser String
str = (\q -> between (char q) (char q) (escaped [q]))

sentence :: Parser String -> Parser [String]
sentence from = sepBy1 from (skipSome spaceChar)

shellish :: Parser [String]
shellish = sentence $ str '\"' <|> str '\'' <|> escaped " \"'"

--

addressed :: String -> Parser String
addressed nick = (byName <|> byPrefix) *> many anyChar
    where
        byName = string nick *> oneOf ":," *> space
        byPrefix = void $ oneOf "!,"
