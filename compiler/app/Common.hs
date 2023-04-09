module Common where

data BinOp = Plus | Times | Equal
  deriving (Eq, Show)

data Lit = LNum Int
         | LBool Bool
  deriving (Eq, Show)
