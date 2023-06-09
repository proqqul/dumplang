module Instruction where

import Common
import Control.Monad.RWS
import qualified Core
import qualified Core as C
import qualified Data.ByteString.Builder as B

newtype Address = Address Int
  deriving (Eq, Show, Num, Ord, Real, Enum, Integral)

data Instruction = Push Lit
                 | Add Address Address
                 | Mul Address Address
                 | Jump Int
                 | JumpIf Int Address
                 | Cmp Address Address
                 | Copy Address
                 | TruncStack Address
  deriving (Eq, Show)

-- Reader: List of address bound to de Bruijn indices
-- Writer: The list of instructions to execute
-- State: The address of the top of the stack
-- Value: The address of the return value of the Instruction compiled
type Compile a = RWS [Address] [Instruction] Address a

scope :: Compile Address -> Compile Address
scope m = do
  startAddr <- get
  retAddr <- m
  put startAddr
  tell [ TruncStack startAddr
       , Copy retAddr ]
  modify (+1)
  pure startAddr

  -- as if we executed it, but don't actually write them to the Writer or modify the State
asIf :: Compile Address -> Compile (Address, [Instruction])
asIf m = do
  state <- get
  ret <- censor (const []) . listen $ m
  put state
  pure ret

pushOne :: Compile Address
pushOne = do
  a <- get
  modify (+1)
  pure a

compile :: Core.T -> Compile Address
compile (C.Lit l) = do
  tell [Push l]
  pushOne
compile (C.App Plus x y) = scope $ do
  inst <- Add <$> compile x <*> compile y
  tell [inst]
  pushOne
compile (C.App Times x y) = scope $ do
  inst <- Mul <$> compile x <*> compile y
  tell [inst]
  pushOne
compile (C.App Equal x y) = scope $ do
  inst <- Cmp <$> compile x <*> compile y
  tell [inst]
  pushOne
compile (C.If b e1 e2) = scope $ do
  b' <- compile b
  (_, e1_is) <- asIf . scope $ compile e1
  (_, e2_is) <- asIf . scope $ compile e2

  -- more like an unless (reverse order from if)
  -- +1 is for the Jump
  tell ([JumpIf (length e2_is + 1) b']
    ++ e2_is
    ++ [Jump (length e1_is)]
    ++ e1_is)

  -- we never actually ran e1 or e2, but one of them is going to actually put something on the stack
  pushOne
compile (C.Var (C.Id i)) = do
  addrs <- ask
  let (Just addr) = addrs !!? i
  pure addr
compile (C.Let cs c) = scope $ do
  addrs <- mapM compile cs
  local (reverse addrs ++) $ compile c

w32 :: (Integral a) => a -> B.Builder
w32 = B.int32LE . fromIntegral

w64 :: (Integral a) => a -> B.Builder
w64 = B.int64LE . fromIntegral

tag32 :: Int -> B.Builder
tag32 = w32

tag64 :: Int -> B.Builder
tag64 = w64

serializeLit :: Lit -> B.Builder
serializeLit (LNum n) = tag32 0 <> w32 n
serializeLit (LBool False) = tag32 1 <> tag32 0
serializeLit (LBool True) = tag32 1 <> tag32 1

serialize :: Instruction -> B.Builder
serialize (Push l) = tag64 0 <> serializeLit l
serialize (Add a1 a2) = tag64 1 <> w32 a1 <> w32 a2
serialize (Mul a1 a2) = tag64 2 <> w32 a1 <> w32 a2
serialize (Jump i) = tag64 3 <> w32 i <> tag32 0
serialize (JumpIf i a) = tag64 4 <> w32 i <> w32 a
serialize (Cmp a1 a2) = tag64 5 <> w32 a1 <> w32 a2
serialize (Copy a) = tag64 6 <> w32 a <> tag32 0
serialize (TruncStack a) = tag64 7 <> w32 a <> tag32 0

serializeAll :: [Instruction] -> LByteString
serializeAll = B.toLazyByteString . mconcat . map serialize


fromCore :: Core.T -> [Instruction]
fromCore c = w
  where
    (_, _, w) = runRWS (compile c) [] 0

fromProgram :: Core.Program -> [Instruction]
fromProgram = (>>= (fromCore . Core.body))
