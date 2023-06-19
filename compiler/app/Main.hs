module Main where

import qualified Instruction
import qualified Expr
import qualified Core
import Text.Megaparsec

pipeline :: Expr.Program -> LByteString
pipeline = Instruction.serializeAll . Instruction.fromProgram . Core.fromProgram

main :: IO ()
main = do
  [fname] <- getArgs
  input <- (readFileText fname :: IO Text)
  let res = parse (Expr.program <* eof) fname input
  case res of
    (Left err) -> putStrLn . errorBundlePretty $ err
    (Right e) -> writeFileLBS "out.bin" . pipeline $ e
