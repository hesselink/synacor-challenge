{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.State.Strict (MonadState (), State, get, gets, put, modify, execState, StateT, liftIO, MonadIO, execStateT)
import Control.Monad (when)
import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Char (chr, ord)
import Data.DList (DList, snoc)
import Data.Bits ((.&.), (.|.), clearBit, complement)
import GHC.Stack (HasCallStack)
import System.IO (Handle, hReady, stdin)
import qualified Control.Monad.State as State
import qualified Data.DList as DList

import State (IState (address, memory, registers, stack), Val (Val, unVal), Addr (Mem, Reg), getReg, setAt, pushStack, popStack, readMem, halt, valToAddr)
import OpCode (OpCode (..))

newtype StateInterpreter a = StateInterpreter { unStateInterpreter :: State IOState a }
  deriving (Functor, Applicative, Monad)

data IOState = IOState
  { state :: IState
  , output :: DList Char
  , input :: String
  }

instance MonadState IState StateInterpreter where
  get = StateInterpreter (gets state)
  put x = StateInterpreter (modify $ \st -> st { state = x })

instance Interpreter StateInterpreter where
  writeChar c = StateInterpreter $
    modify $ \st -> st { output = output st `snoc` c}
  readChar = StateInterpreter $ do
    st <- get
    put st { input = tail (input st) }
    return (head (input st))
  debugMenu = return ()
  checkDebugInterrupt = return ()

runStateInterpreter :: IState -> String -> (IState, String)
runStateInterpreter st inp = f $ execState (unStateInterpreter run) ioState
  where
    ioState = IOState
      { state = st
      , output = mempty
      , input = inp
      }
    f ioSt = (state ioSt, DList.toList $ output ioSt)

newtype IOInterpreter a = IOInterpreter { unIOInterpreter :: StateT IState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState IState)

instance Interpreter IOInterpreter where
  writeChar = liftIO . putChar
  readChar = liftIO getChar
  debugMenu = debugIO
  checkDebugInterrupt = checkDebugIO

class MonadState IState m => Interpreter m where
  writeChar :: Char -> m ()
  readChar :: m Char
  debugMenu :: m ()
  checkDebugInterrupt :: m ()

runIOInterpreter :: IState -> IO IState
runIOInterpreter st = execStateT (unIOInterpreter run) st

run :: Interpreter m => m ()
run = do
  step
  checkDebugInterrupt
  h <- gets halt
  when (not h) run

step :: Interpreter m => m ()
step = do
  opCode <- readOpCode
  runOp opCode

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
      curVal = readMem addr mem
  put st { address = addr + 1 }
  return (valToAddr curVal)

writeVal :: HasCallStack => Interpreter m => Addr -> Val -> m ()
writeVal addr val = modify (setAt addr val)

push :: Interpreter m => Val -> m ()
push v = modify (pushStack v)

pop :: Interpreter m => m (Maybe Val)
pop = State.state popStack

-- debug/break world

data DebugAction
  = SaveState
  | LoadState
  | SetRegister8
  | Return
  deriving (Show, Eq)

debugIO :: IOInterpreter ()
debugIO = do
  printMenu allActions
  action <- readChoice allActions
  runAction action
  where
    allActions = [LoadState, SaveState, SetRegister8, Return]

printMenu :: [DebugAction] -> IOInterpreter ()
printMenu = mapM_ (uncurry printAction) . zip [1..]

printAction :: Int -> DebugAction -> IOInterpreter ()
printAction n act = liftIO $ do
  putStrLn (show n ++ ". " ++ showAction act)
  where
    showAction LoadState = "Load state from file."
    showAction SaveState = "Save state to file."
    showAction SetRegister8 = "Set value of register 8."
    showAction Return = "Return."

readChoice :: [DebugAction] -> IOInterpreter DebugAction
readChoice acts = do
  str <- liftIO getLine
  case readMaybe str of
    Just n | n > 0 && n <= length acts -> return $ acts !! (n - 1)
    _ -> readChoice acts

runAction :: DebugAction -> IOInterpreter ()
runAction SaveState = do
  liftIO $ putStrLn "Enter file name: "
  fileName <- liftIO getLine
  contents <- gets show
  liftIO $ writeFile fileName contents
runAction LoadState = do
  liftIO $ putStrLn "Enter file name: "
  fileName <- liftIO getLine
  contents <- liftIO $ readFile fileName
  put (read contents)
runAction SetRegister8 = do
  liftIO $ putStrLn "Enter new value: "
  str <- liftIO getLine
  case readMaybe str of
    Just v -> writeVal (Reg 7) (Val v)
    _ -> runAction SetRegister8
runAction Return = return ()

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

checkDebugIO :: IOInterpreter ()
checkDebugIO = do
  ready <- liftIO $ hReady stdin
  when ready $ do
    c <- liftIO $ getChar
    case c of
      '\ESC' -> debugIO
      _ -> return ()
