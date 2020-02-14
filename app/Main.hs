module Main where

import ParseBin (parseBin)
import Interpreter (runStateInterpreter)

main :: IO ()
main = do
  st <- parseBin "./challenge.bin"
  let out = runStateInterpreter st ""
  print out
