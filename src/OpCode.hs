module OpCode where

-- data Op
--   = Halt
--   | Set Reg Val
--   | Push Val
--   | Pop Val
--   | Eq Val Val Val
--   | Gt Val Val Val
--   | Jmp Val
--   | Jt Val Val
--   | Jf Val Val
--   | Add Val Val Val
--   | Mult Val Val Val
--   | Mod Val Val Val
--   | And Val Val Val
--   | Or Val Val Val
--   | Not Val Val
--   | RMem Val Val
--   | Wmem Val Val
--   | Call Val
--   | Ret
--   | Out Val
--   | In Val
--   | Noop
--   deriving (Show, Eq)
 
data OpCode
  = Halt
  | Set
  | Push
  | Pop
  | Eq
  | Gt
  | Jmp
  | Jt
  | Jf
  | Add
  | Mult
  | Mod
  | And
  | Or
  | Not
  | RMem
  | Wmem
  | Call
  | Ret
  | Out
  | In
  | Noop
  deriving (Show, Eq, Bounded, Enum)
