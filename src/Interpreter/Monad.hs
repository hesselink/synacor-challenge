{-# LANGUAGE FlexibleContexts #-}
module Interpreter.Monad where

import Prelude hiding (log)
import Control.Monad.State.Strict (MonadState, modify, put, get, gets)
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import qualified Control.Monad.State as State

import State (IState (address, memory, registers), Val(Val), Addr(Reg, Mem), popStack, pushStack, setAt, valToAddr, readMem, getReg)

data LogLevel = Debug | Info | Warning | Error | Critical
  deriving (Show, Read, Eq, Ord)

class MonadState IState m => Interpreter m where
  writeChar :: Char -> m ()
  readChar :: m Char
  debugMenu :: m ()
  checkDebugInterrupt :: m ()
  log :: LogLevel -> String -> m ()

readVal :: HasCallStack => Interpreter m => m Val
readVal = do
  addr <- readAddr
  rs <- gets registers
  let ret = case addr of
        Mem v -> Val v
        Reg n -> getReg n rs
  return ret

peekVals :: Interpreter m => m [Val]
peekVals = do
  st <- get
  let addr = address st
      mem = memory st
      vs = map (flip readMem mem) [addr ..]
  return vs

readAddr :: HasCallStack => Interpreter m => m Addr
readAddr = do
  st <- get
  let addr = address st
      mem = memory st
      curVal = readMem addr mem
  put st { address = addr + 1 }
  let ret = valToAddr curVal
  return ret

writeVal :: HasCallStack => Interpreter m => Addr -> Val -> m ()
writeVal addr val = modify (setAt addr val)

push :: Interpreter m => Val -> m ()
push v = modify (pushStack v)

pop :: Interpreter m => m (Maybe Val)
pop = State.state popStack
