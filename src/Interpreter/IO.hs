{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter.IO where

import Control.Monad.State.Strict (MonadState (), StateT, get, gets, put, modify, execStateT, MonadIO, liftIO)
import Control.Monad (when)
import Text.Read (readMaybe)
import System.IO (hReady, stdin, hSetBuffering, BufferMode (NoBuffering))

import Interpreter.Monad
import State (IState, Addr(Reg), Val(Val))

data IOState = IOState
  { state :: IState
  , logLevel :: LogLevel
  }

newtype IOInterpreter a = IOInterpreter { unIOInterpreter :: StateT IOState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState IState IOInterpreter where
  get = IOInterpreter (gets state)
  put x = IOInterpreter (modify $ \st -> st { state = x })

instance Interpreter IOInterpreter where
  writeChar = liftIO . putChar
  readChar = liftIO getChar
  debugMenu = debugIO
  checkDebugInterrupt = checkDebugIO
  log = logIO

run :: IOInterpreter a -> IState -> IO IState
run act st = do
  hSetBuffering stdin NoBuffering -- TODO set buffering to line buffering when getting input
  state <$> execStateT (unIOInterpreter act) IOState
    { state = st
    , logLevel = Warning
    }

logIO :: LogLevel -> String -> IOInterpreter ()
logIO lvl str = do
  minLvl <- IOInterpreter (gets logLevel)
  when (lvl >= minLvl) $ liftIO $ appendFile "interpreter.log" $ str ++ "\n"

-- debug/break world

data DebugAction
  = LoadState
  | SaveState
  | SetLogLevel
  | SetRegister8
  | Return
  deriving (Show, Eq, Bounded, Enum)

debugIO :: IOInterpreter ()
debugIO = do
  printMenu allActions
  action <- readChoice allActions
  runAction action
  where
    allActions = enumFromTo minBound maxBound

printMenu :: [DebugAction] -> IOInterpreter ()
printMenu = mapM_ (uncurry printAction) . zip [1..]

printAction :: Int -> DebugAction -> IOInterpreter ()
printAction n act = liftIO $ do
  putStrLn (show n ++ ". " ++ showAction act)
  where
    showAction LoadState = "Load state from file."
    showAction SaveState = "Save state to file."
    showAction SetLogLevel = "Set log level."
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
  liftIO $ writeFile fileName contents -- TODO handle failure
runAction LoadState = do
  liftIO $ putStrLn "Enter file name: "
  fileName <- liftIO getLine
  contents <- liftIO $ readFile fileName -- TODO handle failure
  put (read contents)
runAction SetLogLevel = do
  liftIO $ putStrLn "Enter log level: "
  str <- liftIO getLine
  case readMaybe str of
    Just lvl -> IOInterpreter (modify $ \st -> st { logLevel = lvl })
    _ -> runAction SetLogLevel
runAction SetRegister8 = do
  liftIO $ putStrLn "Enter new value: "
  str <- liftIO getLine
  case readMaybe str of
    Just v -> writeVal (Reg 7) (Val v)
    _ -> runAction SetRegister8
runAction Return = return ()

checkDebugIO :: IOInterpreter ()
checkDebugIO = do
  ready <- liftIO $ hReady stdin
  when ready $ do
    c <- liftIO $ getChar
    case c of
      '\ESC' -> debugIO
      _ -> return ()
