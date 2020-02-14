module ParseBin where

import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap

import State (Val (Val), IState (memory), emptyState)

parseBin :: FilePath -> IO IState
parseBin pth = do
  vs <- parseVals pth
  return emptyState {
    memory = HashMap.fromList . zip [0..] $ vs
  }

parseVals :: FilePath -> IO [Val]
parseVals pth = do
  f <- BS.readFile pth
  return . map valFromWords . take2s . BS.unpack $ f

valFromWords :: (Word8, Word8) -> Val
valFromWords (low, high) = Val $ fromIntegral low + fromIntegral high * 256

take2s :: [a] -> [(a,a)]
take2s [] = []
take2s [_] = error "Odd number of bytes"
take2s (x:y:xs) = (x,y) : take2s xs
