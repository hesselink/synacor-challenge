module Program where

import Data.HashMap.Strict (HashMap)
import Data.Word (Word16)
import Data.List (foldl')
import GHC.Stack (HasCallStack)
import qualified Data.HashMap.Strict as HashMap

import State (Addr (Reg, Mem), Val (Val, unVal), valToAddr)
import OpCode (OpCode)
import qualified OpCode as Op

insertProgram :: Program -> HashMap Word16 Val -> HashMap Word16 Val
insertProgram p m
  = foldl' (\m' (k, v) -> HashMap.insert k v m') m
  . zip [0..]
  . map Val
  . serializeProgram
  $ p ++ [Halt]

parseProgram :: [Val] -> Program
parseProgram [] = []
parseProgram (v:vs) =
  if v <= fromIntegral (fromEnum (maxBound :: OpCode))
  then let (op, vs') = parseOp (toEnum (fromIntegral v)) vs
       in op : parseProgram vs'
  else Unknown v : parseProgram vs

parseOp :: OpCode -> [Val] -> (Op, [Val])
parseOp cd vs =
  case cd of
    Op.Halt -> (Halt, vs)
    Op.Set -> parseOp2 Set vs
    Op.Push -> parseOp1 Push vs
    Op.Pop -> parseOp1 Pop vs
    Op.Eq -> parseOp3 Eq vs
    Op.Gt -> parseOp3 Gt vs
    Op.Jmp -> parseOp1 Jmp vs
    Op.Jt -> parseOp2 Jt vs
    Op.Jf -> parseOp2 Jf vs
    Op.Add -> parseOp3 Add vs
    Op.Mult -> parseOp3 Mult vs
    Op.Mod -> parseOp3 Mod vs
    Op.And -> parseOp3 And vs
    Op.Or -> parseOp3 Or vs
    Op.Not -> parseOp2 Not vs
    Op.RMem -> parseOp2 RMem vs
    Op.WMem -> parseOp2 WMem vs
    Op.Call -> parseOp1 Call vs
    Op.Ret -> (Ret, vs)
    Op.Out -> parseOp1 Out vs
    Op.In -> parseOp1 In vs
    Op.Noop -> (Noop, vs)

parseOp1 :: HasCallStack => (Addr -> Op) -> [Val] -> (Op, [Val])
parseOp1 op (v:vs) = (op (valToAddr v), vs)
parseOp1 _ _ = error "Not enough arguments"

parseOp2 :: HasCallStack => (Addr -> Addr -> Op) -> [Val] -> (Op, [Val])
parseOp2 op (v1:v2:vs) = (op (valToAddr v1) (valToAddr v2), vs)
parseOp2 _ _ = error "Not enough arguments"

parseOp3 :: HasCallStack => (Addr -> Addr -> Addr -> Op) -> [Val] -> (Op, [Val])
parseOp3 op (v1:v2:v3:vs) = (op (valToAddr v1) (valToAddr v2) (valToAddr v3), vs)
parseOp3 _ _ = error "Not enough arguments"

serializeProgram :: Program -> [Word16]
serializeProgram = concatMap serializeOp

serializeOp :: Op -> [Word16]
serializeOp op = case op of
  Halt -> [0]
  Set a b -> 1 : map serializeAddr [a, b]
  Push a -> [2, serializeAddr a]
  Pop a -> [3, serializeAddr a]
  Eq a b c -> 4 : map serializeAddr [a, b, c]
  Gt a b c -> 5 : map serializeAddr [a, b, c]
  Jmp a -> [6, serializeAddr a]
  Jt a b -> 7 : map serializeAddr [a, b]
  Jf a b -> 8 : map serializeAddr [a, b]
  Add a b c -> 9 : map serializeAddr [a, b, c]
  Mult a b c -> 10 : map serializeAddr [a, b, c]
  Mod a b c -> 11 : map serializeAddr [a, b, c]
  And a b c -> 12 : map serializeAddr [a, b, c]
  Or a b c -> 13 : map serializeAddr [a, b, c]
  Not a b -> 14 : map serializeAddr [a, b]
  RMem a b -> 15 : map serializeAddr [a, b]
  WMem a b -> 16 : map serializeAddr [a, b]
  Call a -> [17, serializeAddr a]
  Ret -> [18]
  Out a -> [19, serializeAddr a]
  In a -> [20, serializeAddr a]
  Noop -> [21]
  Unknown v -> [unVal v]

serializeAddr :: Addr -> Word16
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
  | WMem Addr Addr
  | Call Addr
  | Ret
  | Out Addr
  | In Addr
  | Noop
  | Unknown Val
  deriving (Show, Eq)
