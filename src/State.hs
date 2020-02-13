{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module State where

import Data.HashMap.Strict (HashMap)

newtype Val = Val { unVal :: Int } -- TODO modulo 32768
  deriving (Show, Eq, Ord, Num)

data Addr = Mem Int | Reg Int
  deriving (Show, Eq)

data IState = IState
  { memory :: HashMap Int Val
  , registers :: (Val, Val, Val, Val, Val, Val, Val, Val)
  , stack :: [Val]
  , address :: Int
  } deriving Show

emptyState :: IState
emptyState = IState
  { memory = mempty
  , registers = (0,0,0,0,0,0,0,0)
  , stack = mempty
  , address = 0
  }
