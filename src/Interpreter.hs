{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.State (MonadState (), State, get, gets, put, modify, execState)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.Char (chr)
import qualified Control.Monad.State as State
import qualified Data.HashMap.Strict as HashMap

import State (IState (address, memory), Val (unVal))
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

readOpCode :: Interpreter m => m OpCode
readOpCode = do
  curMem <- readVal
  return (parseOpCode curMem)

parseOpCode :: Int -> OpCode
parseOpCode = toEnum

runOp :: Interpreter m => OpCode -> m ()
runOp cd = case cd of
  Halt -> return ()
  Out -> do
    i <- readVal
    writeChar (chr i)
  Noop -> return ()
  op -> error $ "not implemented: " ++ show op

readVal :: Interpreter m => m Int
readVal = do
  st <- get
  let addr = address st
      mem = memory st
      curMem = fromJust $ HashMap.lookup addr mem --  TODO what if it's a register?
  put st { address = addr + 1 }
  return (unVal curMem)
