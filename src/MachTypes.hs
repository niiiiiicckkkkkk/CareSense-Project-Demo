{-# LANGUAGE DataKinds #-}

module MachTypes where

{-
Define various datatypes used across modules
-}
    
import Binary
import Data.Array

-- words are 16 bits wide
type MachWord = Binary 16

-- address are 8 bits wide
type MachAddr = Binary 8

-- instructions are read as 8 bit opcode 8 bit payload
type InsnPayload = Binary 8

memSize :: Integer
memSize = 256

data Opcode = Load
            | Store
            | Jump
            | JumpZ
            | JumpN
            | JumpNZ
            | Add
            | Sub
            | Mul
            | Out deriving (Eq, Show)

{-
Programs represent the assembly file in memory.
Bindings associate addresses to values (instructions or constants)
-}
data Program = Program 
    { bindings :: [(MachAddr, MachWord)]
    , start :: MachAddr
    , end :: MachAddr
    }

-- Simulator's model of instructions
data MachInsn = MachInsn Opcode InsnPayload | NOP deriving Show

-- Loader's model of Instructions, Labels, Operands and Definitions
type Label = String
data Operand    
    = L Label 
    | B Integer 
    deriving (Eq, Show)

data ASM 
    = Insn (Maybe Label) Opcode Operand 
    | Def Label Integer 
    deriving (Eq, Show)