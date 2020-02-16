{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Interpreter.State where

import Control.Monad.State.Strict (MonadState (), State, get, gets, put, modify, execState)
import Data.DList (DList, snoc)
import qualified Data.DList as DList

import Interpreter.Monad
import State (IState)

newtype StateInterpreter a = StateInterpreter { unStateInterpreter :: State SState a }
  deriving (Functor, Applicative, Monad)

data SState = SState
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
  log _lvl _str = return ()

run :: StateInterpreter a -> IState -> String -> (IState, String)
run act st inp = f $ execState (unStateInterpreter act) sState
  where
    sState = SState
      { state = st
      , output = mempty
      , input = inp
      }
    f sSt = (state sSt, DList.toList $ output sSt)
