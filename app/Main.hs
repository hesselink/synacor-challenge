module Main where

import qualified Data.HashMap.Strict as HashMap

import ParseBin (parseBin)
import State (Val (Val), memory, emptyState)
import Interpreter (runStateInterpreter)

main :: IO ()
main = do
  st <- parseBin "./challenge.bin"
  let out = runStateInterpreter st ""
  print out
