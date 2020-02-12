module Main where

import qualified Data.HashMap.Strict as HashMap

import State (Val (Val), memory, emptyState)
import Interpreter (runStateInterpreter)

main :: IO ()
main = do
  let initMem = HashMap.fromList . zip [0..] . map (Val . read) . words $ "9 32768 32769 4 19 32768"
      st = emptyState { memory = initMem }
      out = runStateInterpreter st ""
  print out
