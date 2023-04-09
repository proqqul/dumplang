module Expr where

import Prelude hiding (many)

import Common
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Var = MkVar Text
  deriving (Eq, Show)

data T = App BinOp T T
          | Lit Lit
          | If T T T
          | Var Var
          | Let [(Var, T)] T
  deriving (Eq, Show)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

op :: Parser BinOp
op = lexeme $ (symbol "+" $> Plus) <|> (symbol "*" $> Times) <|> (symbol "==" $> Equal)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

variable :: Parser Var
variable = MkVar . fromString <$> ((:) <$> letterChar <*> many alphaNumChar)

expr :: Parser T
expr = lexeme $
  string "true" $> (Lit . LBool $  True)
  <|> string "false" $> (Lit . LBool $ False)
  <|> Lit . LNum <$> L.decimal
  <|> Var <$> variable
  <|> parens
    (App <$> op <*> expr <*> expr
    <|> (lexeme (string "if")) *>
          (If <$> expr <*> expr <*> expr)
    <|> (lexeme (string "let")) *>
          (Let <$> parens (many (parens $ (,) <$> (lexeme variable) <*> expr)) <*> expr))


-- (let ((a 1) (b 2)) expr)
