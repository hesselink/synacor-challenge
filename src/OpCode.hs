module OpCode where

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
  | WMem
  | Call
  | Ret
  | Out
  | In
  | Noop
  deriving (Show, Eq, Bounded, Enum)
