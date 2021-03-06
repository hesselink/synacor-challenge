{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module State where

import Data.Maybe (fromMaybe)
import Data.Bits (Bits)
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

newtype Val = Val { unVal :: Word16 }
  deriving (Show, Eq, Ord, Num, Bits, Enum, Real, Integral, Read)

data Addr = Mem Word16 | Reg Word16
  deriving (Show, Eq)

data IState = IState
  { memory :: HashMap Word16 Val
  , registers :: (Val, Val, Val, Val, Val, Val, Val, Val)
  , stack :: [Val]
  , address :: Word16
  , halt :: Bool
  } deriving (Show, Read)

emptyState :: IState
emptyState = IState
  { memory = mempty
  , registers = (0,0,0,0,0,0,0,0)
  , stack = mempty
  , address = 0
  , halt = False
  }

setAt :: HasCallStack => Addr -> Val -> IState -> IState
setAt addr val st = case addr of
  Mem loc -> st { memory = HashMap.insert loc val (memory st) }
  Reg n -> st { registers = setReg n val (registers st) }

getAt :: HasCallStack => Addr -> IState -> Val
getAt addr st = case addr of
  Mem loc -> readMem loc (memory st)
  Reg n -> getReg n (registers st)

readMem :: Word16 -> HashMap Word16 Val -> Val
readMem addr mem = fromMaybe (error $ "Uninitialized memory at: " ++ show addr) $ HashMap.lookup addr mem

setReg :: HasCallStack => Word16 -> Val -> (Val, Val, Val, Val, Val, Val, Val, Val) -> (Val, Val, Val, Val, Val, Val, Val, Val)
setReg 0 v (_, b, c, d, e, f, g, h) = (v, b, c, d, e, f, g, h)
setReg 1 v (a, _, c, d, e, f, g, h) = (a, v, c, d, e, f, g, h)
setReg 2 v (a, b, _, d, e, f, g, h) = (a, b, v, d, e, f, g, h)
setReg 3 v (a, b, c, _, e, f, g, h) = (a, b, c, v, e, f, g, h)
setReg 4 v (a, b, c, d, _, f, g, h) = (a, b, c, d, v, f, g, h)
setReg 5 v (a, b, c, d, e, _, g, h) = (a, b, c, d, e, v, g, h)
setReg 6 v (a, b, c, d, e, f, _, h) = (a, b, c, d, e, f, v, h)
setReg 7 v (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g, v)
setReg n _ _ = error $ "Register out of bounds: " ++ show n

getReg :: HasCallStack => Word16 -> (Val, Val, Val, Val, Val, Val, Val, Val) -> Val
getReg 0 (v, _, _, _, _, _, _, _) = v
getReg 1 (_, v, _, _, _, _, _, _) = v
getReg 2 (_, _, v, _, _, _, _, _) = v
getReg 3 (_, _, _, v, _, _, _, _) = v
getReg 4 (_, _, _, _, v, _, _, _) = v
getReg 5 (_, _, _, _, _, v, _, _) = v
getReg 6 (_, _, _, _, _, _, v, _) = v
getReg 7 (_, _, _, _, _, _, _, v) = v
getReg n _ = error $ "Register out of bounds: " ++ show n

pushStack :: Val -> IState -> IState
pushStack v st = st { stack = v : stack st }

popStack :: IState -> (Maybe Val, IState)
popStack st =
  case stack st of
    h:tl -> (Just h, st { stack = tl })
    [] -> (Nothing, st)

valToAddr :: HasCallStack => Val -> Addr
valToAddr v =
  if v < 32768
  then Mem (unVal v)
  else if v < 32776
  then Reg (unVal v - 32768)
  else error $ "Value out of bounds: " ++ show v
