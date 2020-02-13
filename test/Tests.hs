{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import State (Addr (..), IState (memory), emptyState, getAt, setAt, Val (Val))
import Interpreter (runStateInterpreter)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ testProperty "Addition" testAddition ]

testAddition :: Val -> Val -> Addr -> Addr -> Addr -> Property
testAddition x y target source1 source2 = target /= source1 && target /= source2 && source1 /= source2 ==>
  let st = addProgram [Add target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target (v1 + v2) st

addProgram :: Program -> IState -> IState
addProgram p st = st { memory = insertProgram p (memory st) }

runAndCheckEquals :: Addr -> Val -> IState -> Property
runAndCheckEquals addr expected st =
  let (st', _) = runStateInterpreter st ""
      actual = getAt addr st'
  in actual === expected

instance Arbitrary Val where
  arbitrary = Val <$> choose (0, 65535)

instance Arbitrary Addr where
  arbitrary = oneof [ Mem <$> choose (100, 32767), Reg <$> choose (0, 7) ] -- Start at mem 100 so we can fit the program before it

insertProgram :: Program -> HashMap Int Val -> HashMap Int Val
insertProgram p m
  = foldl' (\m' (k, v) -> HashMap.insert k v m') m
  . zip [0..]
  . map Val
  . serializeProgram
  $ p ++ [Halt]

serializeProgram :: Program -> [Int]
serializeProgram = concatMap serializeOp

serializeOp :: Op -> [Int]
serializeOp op = case op of
  Halt -> [0]
  -- TODO
  Add a b c -> 9 : map serializeAddr [a, b, c]
  -- TODO
  _ -> error "Not implemented"

serializeAddr :: Addr -> Int
serializeAddr (Mem m) = m
serializeAddr (Reg r) = r + 32768

type Program = [Op]

data Op
  = Halt
  | Set Addr Addr
  | Push Addr
  | Pop Addr
  | Eq Addr Addr Addr
  | Gt Addr Addr Addr
  | Jmp Addr
  | Jt Addr Addr
  | Jf Addr Addr
  | Add Addr Addr Addr
  | Mult Addr Addr Addr
  | Mod Addr Addr Addr
  | And Addr Addr Addr
  | Or Addr Addr Addr
  | Not Addr Addr
  | RMem Addr Addr
  | Wmem Addr Addr
  | Call Addr
  | Ret
  | Out Addr
  | In Addr
  | Noop
  deriving (Show, Eq)
