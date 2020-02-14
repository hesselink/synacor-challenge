{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.State (MonadState (), State, get, gets, put, modify, execState)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Char (chr)
import GHC.Stack (HasCallStack)
import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict as HashMap

import State (IState (address, memory, registers), Val (Val, unVal), Addr (Mem, Reg), getReg, setAt)
import OpCode (OpCode (..))

newtype StateInterpreter a = StateInterpreter { unStateInterpreter :: State IOState a }
  deriving (Functor, Applicative, Monad)

data IOState = IOState
  { state :: IState
  , output :: String
  , input :: String
  }

instance MonadState IState StateInterpreter where
  get = StateInterpreter (gets state)
  put x = StateInterpreter (modify $ \st -> st { state = x })


class MonadState IState m => Interpreter m where
  writeChar :: Char -> m ()
  readChar :: m Char

instance Interpreter StateInterpreter where
  writeChar c = StateInterpreter $
    modify $ \st -> st { output = c : output st }
  readChar = StateInterpreter $ do
    st <- get
    put st { input = tail (input st) }
    return (head (input st))

runStateInterpreter :: IState -> String -> (IState, String)
runStateInterpreter st inp = f $ execState (unStateInterpreter run) ioState
  where
    ioState = IOState
      { state = st
      , output = []
      , input = inp
      }
    f ioSt = (state ioSt, output ioSt)

run :: Interpreter m => m ()
run = do
  halt <- step
  when (not halt) run

step :: Interpreter m => m Bool
step = do
  opCode <- readOpCode
  runOp opCode
  return $ opCode == Halt

readOpCode :: HasCallStack => Interpreter m => m OpCode
readOpCode = do
  curMem <- unVal <$> readVal
  return (parseOpCode curMem)

parseOpCode :: Int -> OpCode
parseOpCode = toEnum

runOp :: HasCallStack => Interpreter m => OpCode -> m ()
runOp cd = case cd of
  Halt -> return ()
  Set -> do
    target <- readAddr
    v1 <- readVal
    writeVal target v1
  Jmp -> do
    Val addr <- readVal
    modify $ \st -> st { address = addr }
  Jt -> do
    Val cond <- readVal
    Val addr <- readVal
    when (cond > 0) $ modify $ \st -> st { address = addr }
  Jf -> do
    Val cond <- readVal
    Val addr <- readVal
    when (cond == 0) $ modify $ \st -> st { address = addr }
  Eq -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target (if v1 == v2 then 1 else 0)
  Add -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target (v1 + v2)
  Out -> do
    i <- unVal <$> readVal
    writeChar (chr i)
  Noop -> return ()
  op -> error $ "not implemented: " ++ show op

readVal :: HasCallStack => Interpreter m => m Val
readVal = do
  addr <- readAddr
  rs <- gets registers
  return $ case addr of
    Mem v -> Val v
    Reg n -> getReg n rs

readAddr :: HasCallStack => Interpreter m => m Addr
readAddr = do
  st <- get
  let addr = address st
      mem = memory st
      curVal = fromMaybe (error $ "Uninitialized memory at: " ++ show addr) $ HashMap.lookup addr mem
  put st { address = addr + 1 }
  if curVal < 32768
  then return (Mem $ unVal curVal)
  else if curVal < 32776
  then return (Reg (unVal curVal - 32768))
  else error $ "Value out of bounds: " ++ show curVal

writeVal :: HasCallStack => Interpreter m => Addr -> Val -> m ()
writeVal addr val = modify (setAt addr val)
