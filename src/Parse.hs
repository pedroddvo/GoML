{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-imports #-}
module Parse where

import Data.Text ( Text )
import qualified Data.Text as T

import Data.Void ( Void )

import Data.Char (isAlpha, isAlphaNum)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative hiding ( some, many )

import Control.Monad.Combinators.Expr
import Control.Monad (void)

type Parser = Parsec Void Text

data Expr = MlBinary Text Expr Expr
          | MlCurried Expr Expr

          | MlLet Text [Text] Expr
          | MlLetBinding Expr Expr

          | MlFunc [Text] Expr

          | MlNumber Integer
          | MlSymbol Text
          | MlNull
          deriving Show

spaceC :: Parser ()
spaceC = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceC

symbol :: Text -> Parser Text
symbol = L.symbol spaceC

keyword :: Text -> Parser ()
keyword t = void $ string t <* (space1 <|> eof)

keywords :: [Text]
keywords = [ "let"
           , "in"
           , "func" ]

mlNumber :: Parser Integer
mlNumber = L.decimal

mlSymbol :: Parser Text
mlSymbol = do
    let se = "symbolic character"

    head <- satisfy isAlpha <?> se
    tail <- takeWhileP (Just se) isAlphaNum
    let res = T.cons head tail

    if res `elem` keywords
    then fail "unexpected keyword"
    else return res

mlFunc :: Parser Expr
mlFunc = do
    keyword "func"
    args <- sepEndBy mlSymbol space1
    keyword "->"
    MlFunc args <$> expr

mlLet :: Parser Expr
mlLet = do
    keyword "let"
    name <- lexeme mlSymbol
    args <- sepEndBy mlSymbol space1
    keyword "="
    MlLet name args <$> expr

mlLetBinding :: Parser Expr
mlLetBinding = do
    l <- mlLet
    keyword "in"
    MlLetBinding l <$> expr



expr :: Parser Expr
expr = makeExprParser (curried <|> factor) table
    where
        factor = choice [ mlFunc, mlLetBinding ]
        curried = do
            es <- some term
            return $ case es of
                [x] -> x
                xs  -> foldl1 MlCurried xs

term :: Parser Expr
term = lexeme atom
    where 
        atom = choice [ symbol "(" *> lexeme expr <* symbol ")"
                      , MlNumber <$> mlNumber
                      , MlSymbol <$> try mlSymbol
                      ]

table :: [[Operator Parser Expr]]
table = [ [ binary "*" ]
        , [ binary "+" ]
        ]

binary :: Text -> Operator Parser Expr
binary name = InfixL (MlBinary <$> symbol name)

stmt :: Parser Expr
stmt = choice [ mlLet ]

program :: Parser [Expr]
program = choice [ [] <$ eof
                 , some stmt <* eof ]

parseExpr :: Text -> Either Text [Expr]
parseExpr s = 
    case runParser program "" s of
        Left peb -> Left $ T.pack (errorBundlePretty peb)
        Right ex -> Right ex
