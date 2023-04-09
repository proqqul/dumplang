module Main where

import qualified Instruction
import qualified Expr
import qualified Core
import Text.Megaparsec

pipeline :: Expr.T -> LByteString
pipeline = Instruction.serializeAll . Instruction.fromCore . Core.fromExpr

main :: IO ()
main = do
  input <- (readFileText "./test.dl" :: IO Text)
  let res = parse (Expr.expr <* eof) "<nofile>" input
  case res of
    (Left err) -> putStrLn . errorBundlePretty $ err
    (Right e) -> writeFileLBS "out.bin" . pipeline $ e
