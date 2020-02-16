{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Prelude hiding (log)
import Control.Monad.State.Strict (gets, modify)
import Control.Monad (when)
import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import Data.Char (chr, ord)
import Data.Bits ((.&.), (.|.), clearBit, complement)
import GHC.Stack (HasCallStack)

import Interpreter.Monad (Interpreter (..), readVal, readAddr, writeVal, push, pop, LogLevel (..), peekVals)
import State (IState (address, memory, stack), Val (Val, unVal), Addr (Mem), readMem, halt)
import OpCode (OpCode (..))
import Program (parseProgram)
import qualified Interpreter.State as S
import qualified Interpreter.IO as IO

runStateInterpreter :: IState -> String -> (IState, String)
runStateInterpreter = S.run run

runIOInterpreter :: IState -> IO IState
runIOInterpreter = IO.run run

run :: Interpreter m => m ()
run = do
  step
  checkDebugInterrupt
  h <- gets halt
  when (not h) run

step :: Interpreter m => m ()
step = do
  logInstr
  opCode <- readOpCode
  runOp opCode

logInstr :: Interpreter m => m ()
logInstr = do
  vs <- peekVals
  log Debug $ show . head . parseProgram $ vs

readOpCode :: HasCallStack => Interpreter m => m OpCode
readOpCode = do
  curMem <- unVal <$> readVal
  return (parseOpCode curMem)

parseOpCode :: Word16 -> OpCode
parseOpCode = toEnum . fromIntegral

runOp :: HasCallStack => Interpreter m => OpCode -> m ()
runOp cd = case cd of
  Halt -> do
    modify $ \st -> st { halt = True }
  Set -> do
    target <- readAddr
    v1 <- readVal
    writeVal target v1
  Push -> do
    v <- readVal
    push v
  Pop -> do
    target <- readAddr
    v <- fromMaybe (error "Empty stack") <$> pop
    writeVal target v
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
  Gt -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target (if v1 > v2 then 1 else 0)
  Add -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target ((v1 + v2) `mod` 32768)
  Mult -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target ((v1 * v2) `mod` 32768)
  Mod -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target (v1 `mod` v2)
  And -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target (v1 .&. v2)
  Or -> do
    target <- readAddr
    v1 <- readVal
    v2 <- readVal
    writeVal target (v1 .|. v2)
  Not -> do
    target <- readAddr
    v <- readVal
    writeVal target (clearBit (complement v) 15)
  RMem -> do
   target <- readAddr
   (Val v) <- readVal
   mem <- gets memory
   writeVal target (readMem v mem)
  WMem -> do
   (Val target) <- readVal
   v <- readVal
   writeVal (Mem target) v
  Call -> do
   Val addr <- readVal
   next <- gets address
   modify $ \st -> st { address = addr, stack = Val next : stack st }
  Ret -> do
   v <- pop
   modify $ \st ->
     case v of
       Just (Val addr) -> st { address = addr }
       Nothing -> st { halt = True }
  Out -> do
    i <- unVal <$> readVal
    writeChar (chr . fromIntegral $ i)
  In -> do
    target <- readAddr
    c <- readChar
    if c == '\ESC'
    then debugMenu
    else
      writeVal target (Val . fromIntegral . ord $ c)
  Noop -> return ()
