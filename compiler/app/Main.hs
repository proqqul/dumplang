module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.ByteString.Builder as B

data BinOp = Plus | Times
  deriving (Eq, Show)

data Expr = App BinOp Expr Expr
          | Lit Int
  deriving (Eq, Show)

type Parser = Parsec Void Text

op :: Parser BinOp
op = (string "+" $> Plus) <|> (string "*" $> Times)

withspace :: Parser a -> Parser a
withspace = (<* string " ")

parens :: Parser a -> Parser a
parens p = string "(" *> p <* string ")"

expr :: Parser Expr
expr = (Lit <$> L.decimal) <|> parens (App <$> withspace op <*> withspace expr <*> expr)

data Instruction = Push Int
                 | Add
                 | Mul
  deriving (Eq, Show)

compile :: Expr -> [Instruction]
compile (Lit i) = [Push i]
compile (App Plus x y) = compile x ++ compile y ++ [Add]
compile (App Times x y) = compile x ++ compile y ++ [Mul]

serialize :: Instruction -> B.Builder
serialize (Push i) = B.int64LE 0 <> B.int64LE (fromIntegral i)
serialize Add = B.int64LE 1 <> B.int64LE 0
serialize Mul = B.int64LE 2 <> B.int64LE 0

serializeAll :: [Instruction] -> LByteString
serializeAll = B.toLazyByteString . mconcat . map serialize

main :: IO ()
main = do
  input <- (getLine :: IO Text)
  let res = parse (expr <* eof) "<nofile>" input
  case res of
    (Left err) -> putStrLn . errorBundlePretty $ err
    (Right e) -> writeFileLBS "out.bin" . serializeAll . compile $ e
