{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SIC.Parser where

import           Protolude                  hiding (some, try)
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import SIC.Type

type Parser = Parsec Void Text

readVar ∷ String → Int
readVar s = go 0 0 ((\x -> ord x - 97) <$> (reverse s))
  where
    go ∷ Int → Int → [Int] -> Int
    go e n (x:xs) = go (e + 1) (n + (x + 1) * 26^e) xs
    go _ n []     = n - 1

spaceC = L.space space1 empty empty
lexeme = L.lexeme spaceC
symbol = L.symbol spaceC
parens = between (symbol "(") (symbol ")")
comma  = symbol ","
dot    = symbol "."

symb = L.decimal <|> (readVar <$> some (oneOf ['a'..'z']))

term ∷ Parser Term
term = ti <|> parens ti
  where
    ti = lexeme $ choice
        [ try app_
        , try lam_
        , try var_
        ]

var_ ∷ Parser Term
var_ = Var <$> symb

lam_ = do
  symbol "\\" <|> symbol "λ"
  a <- symb
  dot
  b <- term
  return $ Lam a b

app_ = parens $ do
  a <- term
  b <- term
  return $ App a b
