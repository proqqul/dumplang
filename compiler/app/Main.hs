module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.ByteString.Builder as B

data BinOp = Plus | Times | Equal
  deriving (Eq, Show)

data Lit = LNum Int
         | LBool Bool
  deriving (Eq, Show)

data Expr = EApp BinOp Expr Expr
          | ELit Lit
          | EIf Expr Expr Expr
  deriving (Eq, Show)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

op :: Parser BinOp
op = lexeme $ (symbol "+" $> Plus) <|> (symbol "*" $> Times) <|> (symbol "==" $> Equal)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

expr :: Parser Expr
expr = lexeme $
  (string "true") $> (ELit . LBool $  True)
  <|> (string "false") $> (ELit . LBool $ False)
  <|> ELit . LNum <$> L.decimal
  <|> parens
    (EApp <$> op <*> expr <*> expr
    <|> EIf <$> (lexeme (string "if") *> expr) <*> expr <*> expr)






data Instruction = Push Lit
                 | Add
                 | Mul
                 | Jump Int
                 | JumpIf Int
                 | Cmp
  deriving (Eq, Show)

compile :: Expr -> [Instruction]
compile (ELit l) = [Push l]
compile (EApp Plus x y) = compile x ++ compile y ++ [Add]
compile (EApp Times x y) = compile x ++ compile y ++ [Mul]
compile (EApp Equal x y) = compile x ++ compile y ++ [Cmp]
compile (EIf b e1 e2) = b' ++ [ JumpIf $ (length e2' + 1) ] ++ e2' ++ [ Jump $ length e1' ] ++ e1'
  where
    b' = compile b
    e1' = compile e1
    e2' = compile e2

serializeLit :: Lit -> B.Builder
serializeLit (LNum n) = B.int32LE 0 <> B.int32LE (fromIntegral n)
serializeLit (LBool False) = B.int32LE 1 <> B.int32LE 0
serializeLit (LBool True) = B.int32LE 1 <> B.int32LE 1

serialize :: Instruction -> B.Builder
serialize (Push l) = B.int64LE 0 <> serializeLit l
serialize Add = B.int64LE 1 <> B.int64LE 0
serialize Mul = B.int64LE 2 <> B.int64LE 0
serialize (Jump i) = B.int64LE 3 <> B.int64LE (fromIntegral i)
serialize (JumpIf i) = B.int64LE 4 <> B.int64LE (fromIntegral i)
serialize Cmp = B.int64LE 5 <> B.int64LE 0

serializeAll :: [Instruction] -> LByteString
serializeAll = B.toLazyByteString . mconcat . map serialize

main :: IO ()
main = do
  input <- (readFileText "./test.dl" :: IO Text)
  let res = parse (expr <* eof) "<nofile>" input
  case res of
    (Left err) -> putStrLn . errorBundlePretty $ err
    (Right e) -> writeFileLBS "out.bin" . serializeAll . compile $ e
