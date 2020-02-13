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

import State (IState (address, memory, registers), Val (Val, unVal), Addr (Mem, Reg))
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
writeVal addr val = do
  case addr of
    Mem loc -> modify $ \st -> st { memory = HashMap.insert loc val (memory st) }
    Reg n -> modify $ \st -> st { registers = setReg n val (registers st) }

setReg :: HasCallStack => Int -> Val -> (Val, Val, Val, Val, Val, Val, Val, Val) -> (Val, Val, Val, Val, Val, Val, Val, Val)
setReg 0 v (_, b, c, d, e, f, g, h) = (v, b, c, d, e, f, g, h)
setReg 1 v (a, _, c, d, e, f, g, h) = (a, v, c, d, e, f, g, h)
setReg 2 v (a, b, _, d, e, f, g, h) = (a, b, v, d, e, f, g, h)
setReg 3 v (a, b, c, _, e, f, g, h) = (a, b, c, v, e, f, g, h)
setReg 4 v (a, b, c, d, _, f, g, h) = (a, b, c, d, v, f, g, h)
setReg 5 v (a, b, c, d, e, _, g, h) = (a, b, c, d, e, v, g, h)
setReg 6 v (a, b, c, d, e, f, _, h) = (a, b, c, d, e, f, v, h)
setReg 7 v (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g, v)
setReg n _ _ = error $ "Register out of bounds: " ++ show n

getReg :: HasCallStack => Int -> (Val, Val, Val, Val, Val, Val, Val, Val) -> Val
getReg 0 (v, _, _, _, _, _, _, _) = v
getReg 1 (_, v, _, _, _, _, _, _) = v
getReg 2 (_, _, v, _, _, _, _, _) = v
getReg 3 (_, _, _, v, _, _, _, _) = v
getReg 4 (_, _, _, _, v, _, _, _) = v
getReg 5 (_, _, _, _, _, v, _, _) = v
getReg 6 (_, _, _, _, _, _, v, _) = v
getReg 7 (_, _, _, _, _, _, _, v) = v
getReg n _ = error $ "Register out of bounds: " ++ show n
