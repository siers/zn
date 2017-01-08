module Zn.Grammar where

import Control.Monad
import Control.Applicative
import Data.List
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String

ifParse :: Monad m => Parser a -> String -> (a -> m b) -> m ()
ifParse parser msg action = either (return . const ()) (void . action) . parse parser "" $ msg

addressed :: String -> Parser String
addressed nick = (byName <|> byPrefix) *> many anyChar
    where
        byName = string nick *> oneOf ":," *> space
        byPrefix = void $ oneOf "!,"

between3 :: Parser a -> Parser b -> Parser (b, b)
between3 lim p = do
    lim; a <- p; lim; b <- p; lim
    return (a, b)

sed :: Parser ((String, String), String)
sed = (,) <$> (string "s" *> body) <*> (many $ oneOf "gi")
    where body = between3 (string "/") (many $ noneOf "/")
