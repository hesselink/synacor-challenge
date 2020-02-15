module Main where

import Control.Monad (void)

import ParseBin (parseBin)
import Interpreter (runIOInterpreter)

main :: IO ()
main = do
  st <- parseBin "./challenge.bin"
  void $ runIOInterpreter st
