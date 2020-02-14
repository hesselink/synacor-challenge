{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import State (Addr (..), IState (memory, stack), emptyState, getAt, setAt, Val (Val, unVal))
import Interpreter (runStateInterpreter)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ testProperty "Set" testSet
  , testProperty "Push" testPush
  , testProperty "Equal" testEqual
  , testProperty "Jump" testJump
  , testProperty "JumpIfTrue" testJumpIfTrue
  , testProperty "JumpIfFalse" testJumpIfFalse
  , testProperty "Addition" testAddition
  ]

testSet :: Val -> Reg -> Addr -> Property
testSet x (R target) source = target /= source ==>
  let st = addProgram [Set target source] . setAt source x $ emptyState
      v = case source of
        Mem l -> Val l
        Reg _ -> x
  in runAndCheckEquals target v st

testPush :: Val -> Addr -> Property
testPush x source =
  let st = addProgram [Push source] . setAt source x $ emptyState
      v = case source of
        Mem l -> Val l
        Reg _ -> x
  in runAndCheckStackTop v st

testEqual :: Val -> Val -> Addr -> Addr -> Addr -> Property
testEqual x y target source1 source2 = target /= source1 && target /= source2 ==>
  let st = addProgram [Eq target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target (if v1 == v2 || source1 == source2 then 1 else 0) st

testJump :: Val -> Val -> Reg -> Addr -> Property -- TODO test jump from register
testJump x y (R target) source = target /= source ==>
  let st = addProgram [ Jmp (Mem 6)
                      , Set target (Mem (unVal x))
                      , Halt
                      , Set target (Mem (unVal y))
                      ] $ emptyState
  in runAndCheckEquals target y st

testJumpIfTrue :: Bool -> Val -> Val -> Reg -> Addr -> Property
testJumpIfTrue b x y (R target) source = target /= source ==>
  let st = addProgram [ Jt (Mem $ if b then 1 else 0) (Mem 7)
                      , Set target (Mem (unVal x))
                      , Halt
                      , Set target (Mem (unVal y))
                      ] $ emptyState
  in runAndCheckEquals target (if b then y else x) st

testJumpIfFalse :: Bool -> Val -> Val -> Reg -> Addr -> Property
testJumpIfFalse b x y (R target) source = target /= source ==>
  let st = addProgram [ Jf (Mem $ if b then 1 else 0) (Mem 7)
                      , Set target (Mem (unVal x))
                      , Halt
                      , Set target (Mem (unVal y))
                      ] $ emptyState
  in runAndCheckEquals target (if b then x else y) st

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

runAndCheckStackTop :: Val -> IState -> Property
runAndCheckStackTop expected st =
  let (st', _) = runStateInterpreter st ""
      actual = head (stack st')
  in actual === expected

instance Arbitrary Val where
  arbitrary = Val <$> choose (0, 32775)

instance Arbitrary Addr where
  arbitrary = oneof [ Mem <$> choose (100, 32767), getReg <$> arbitrary ] -- Start at mem 100 so we can fit the program before it

newtype Reg = R { getReg :: Addr }
  deriving (Show, Eq)

instance Arbitrary Reg where
  arbitrary = R . Reg <$> choose (0, 7)

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
  Set a b -> 1 : map serializeAddr [a, b]
  Push a -> [2, serializeAddr a]
  Eq a b c -> 4 : map serializeAddr [a, b, c]
  Jmp a -> [6, serializeAddr a]
  Jt a b -> 7 : map serializeAddr [a, b]
  Jf a b -> 8 : map serializeAddr [a, b]
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
