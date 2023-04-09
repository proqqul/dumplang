module Core where

import Common
import Data.List (elemIndex)
import qualified Expr
import qualified Expr as E

newtype Id = Id Int
  deriving (Eq, Show)

data T = App BinOp T T
       | Lit Lit
       | If T T T
       | Var Id
       | Let [T] T
  deriving (Eq, Show)


fromExpr :: Expr.T -> T
fromExpr e = runReader (helper e) []
  where
    helper :: Expr.T -> Reader [E.Var] T
    helper (E.App b e1 e2) = App b <$> (helper e1) <*> (helper e2)
    helper (E.Lit l) = pure $ Lit l
    helper (E.If e1 e2 e3) = If <$> (helper e1) <*> (helper e2) <*> (helper e3)
    helper (E.Var name) = do
      vars <- ask
      let (Just i) = elemIndex name vars
      return . Var . Id $ i
    helper (E.Let bindings e) = do
      let (newvars, bodies) = unzip bindings
      bodies' <- mapM helper bodies
      Let bodies' <$> local (reverse newvars ++) (helper e)
