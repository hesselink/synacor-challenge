{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Property, oneof, choose, (===), (==>), testProperty, (.&&.))
import Data.Bits ((.&.), (.|.), complement, clearBit)

import State (Addr (..), IState (memory, stack), emptyState, getAt, setAt, Val (Val, unVal), pushStack)
import Interpreter (runStateInterpreter)
import Program (Op (..), Program, insertProgram)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $
  [ testProperty "Set" testSet
  , testProperty "Push" testPush
  , testProperty "Pop" testPop
  , testProperty "Equal" testEqual
  , testProperty "GreaterThan" testGreaterThan
  , testProperty "Jump" testJump
  , testProperty "JumpIfTrue" testJumpIfTrue
  , testProperty "JumpIfFalse" testJumpIfFalse
  , testProperty "Addition" testAddition
  , testProperty "Multiplication" testMultiplication
  , testProperty "Modulo" testModulo
  , testProperty "And" testAnd
  , testProperty "Or" testOr
  , testProperty "Not" testNot
  , testProperty "ReadMemory" testReadMemory
  , testProperty "WriteMemory" testWriteMemory
  , testProperty "Call" testCall
  , testProperty "Return" testReturn
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

testPop :: Val -> Addr -> Property
testPop x target =
  let st = addProgram [Pop target] . pushStack x $ emptyState
  in runAndCheckEquals target x st

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

testGreaterThan :: Val -> Val -> Addr -> Addr -> Addr -> Property
testGreaterThan x y target source1 source2 = target /= source1 && target /= source2 ==>
  let st = addProgram [Gt target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target (if v1 > v2 && source1 /= source2 then 1 else 0) st

testJump :: Val -> Val -> Reg -> Property -- TODO test jump from register
testJump x y (R target) =
  let st = addProgram [ Jmp (Mem 6)
                      , Set target (Mem (unVal x))
                      , Halt
                      , Set target (Mem (unVal y))
                      ] $ emptyState
  in runAndCheckEquals target y st

testJumpIfTrue :: Bool -> Mem -> Mem -> Reg -> Property
testJumpIfTrue = testJumpIf Jt True

testJumpIfFalse :: Bool -> Mem -> Mem -> Reg -> Property
testJumpIfFalse = testJumpIf Jf False

testJumpIf :: (Addr -> Addr -> Op) -> Bool -> Bool -> Mem -> Mem -> Reg -> Property
testJumpIf jumpOp jumpIf b (M x) (M y) (R target) =
  let st = addProgram [ jumpOp (Mem $ if b then 1 else 0) (Mem 7)
                      , Set target x
                      , Halt
                      , Set target y
                      ] $ emptyState
      Mem v1 = x
      Mem v2 = y
  in runAndCheckEquals target (Val $ if jumpIf /= b then v1 else v2) st

testAddition :: Val -> Val -> Addr -> Addr -> Addr -> Property
testAddition x y target source1 source2 = target /= source1 && target /= source2 && source1 /= source2 ==>
  let st = addProgram [Add target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target ((v1 + v2) `mod` 32768) st

testMultiplication :: Val -> Val -> Addr -> Addr -> Addr -> Property
testMultiplication x y target source1 source2 = target /= source1 && target /= source2 && source1 /= source2 ==>
  let st = addProgram [Mult target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target ((v1 * v2) `mod` 32768) st

testModulo :: Val -> Val -> Addr -> Addr -> Addr -> Property
testModulo x y target source1 source2 = target /= source1 && target /= source2 && source1 /= source2 ==>
  let st = addProgram [Mod target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target (v1 `mod` v2) st

testAnd :: Val -> Val -> Addr -> Addr -> Addr -> Property
testAnd x y target source1 source2 = target /= source1 && target /= source2 && source1 /= source2 ==>
  let st = addProgram [And target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target (v1 .&. v2) st

testOr :: Val -> Val -> Addr -> Addr -> Addr -> Property
testOr x y target source1 source2 = target /= source1 && target /= source2 && source1 /= source2 ==>
  let st = addProgram [Or target source1 source2] . setAt source1 x . setAt source2 y $ emptyState
      v1 = case source1 of
        Mem l -> Val l
        Reg _ -> x
      v2 = case source2 of
        Mem l -> Val l
        Reg _ -> y
  in runAndCheckEquals target (v1 .|. v2) st

testNot :: Val -> Addr -> Addr -> Property
testNot x target source = target /= source ==>
  let st = addProgram [Not target source] . setAt source x $ emptyState
      v = case source of
        Mem l -> Val l
        Reg _ -> x
   in runAndCheckEquals target (clearBit (complement v) 15) st

testReadMemory :: Val -> Addr -> Mem -> Property
testReadMemory x target (M source) = target /= source ==>
  let st = addProgram [RMem target source] . setAt source x $ emptyState
  in runAndCheckEquals target x st

testWriteMemory :: Val -> Mem -> Addr -> Property
testWriteMemory x (M target) source = target /= source ==>
  let st = addProgram [WMem target source] . setAt source x $ emptyState
      v = case source of
        Mem l -> Val l
        Reg _ -> x
  in runAndCheckEquals target v st

testCall :: Val -> Val -> Reg -> Property -- TODO test jump from register
testCall x y (R target) =
  let st = addProgram [ Call (Mem 6)
                      , Set target (Mem (unVal x))
                      , Halt
                      , Set target (Mem (unVal y))
                      ] $ emptyState
  in runAndCheck [equalityCheck target y, stackTopCheck 2] st

testReturn :: Val -> Val -> Reg -> Property
testReturn x y (R target) =
  let st = addProgram [ Ret
                      , Set target (Mem (unVal x))
                      , Halt
                      , Set target (Mem (unVal y))
                      ] . pushStack 5 $ emptyState
  in runAndCheckEquals target y st

addProgram :: Program -> IState -> IState
addProgram p st = st { memory = insertProgram p (memory st) }

runAndCheck :: [IState -> Property] -> IState -> Property
runAndCheck checks st =
  let (st', _) = runStateInterpreter st ""
  in foldr1 (.&&.) (map ($ st') checks)

runAndCheckEquals :: Addr -> Val -> IState -> Property
runAndCheckEquals addr expected = runAndCheck [equalityCheck addr expected]

equalityCheck :: Addr -> Val -> IState -> Property
equalityCheck addr expected st = getAt addr st === expected

runAndCheckStackTop :: Val -> IState -> Property
runAndCheckStackTop expected = runAndCheck [stackTopCheck expected]

stackTopCheck :: Val -> IState -> Property
stackTopCheck expected st = head (stack st) === expected

instance Arbitrary Val where
  arbitrary = Val <$> choose (0, 32775)

instance Arbitrary Addr where
  arbitrary = oneof [ getMem <$> arbitrary, getReg <$> arbitrary ]

newtype Reg = R { getReg :: Addr }
  deriving (Show, Eq)

newtype Mem = M { getMem :: Addr }
  deriving (Show, Eq)

instance Arbitrary Reg where
  arbitrary = R . Reg <$> choose (0, 7)

instance Arbitrary Mem where
  arbitrary = M . Mem <$> choose (100, 32767) -- Start at mem 100 so we can fit the program before it
