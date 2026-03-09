module CPUSim where

import Data.Map (Map)
import Data.Map qualified as Map

-- PC - Program counter
-- Registers
-- RAM

data Instr
  = CONST RegIdx Int
  | ADD RegIdx RegIdx RegIdx
  | MUL RegIdx RegIdx RegIdx
  | JUMP MemAddr
  | JUMP0 RegIdx MemAddr
  | HALT


data RegIdx
  = PC
  | R1
  | R2
  | R3
  | R4

type MemAddr = Int

type Registers = Map RegIdx Int

type Memory = Map MemAddr Int
-- type Memory = Map MemAddr (Either Instr Int)
type EncodeMap = Map Instr Int
