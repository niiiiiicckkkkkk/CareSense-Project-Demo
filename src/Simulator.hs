{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Simulator where

{-
Defines a typeclass MachCmp which expresses the idea of some clocked hardware
component with IO.

Simulates 
    1. CPU as 3 registers (PC, AC, IR)
    2. RAM as an Array of memory locations, select line, input line, output line

MachState defines the concrete state threaded through the program

Uses the "State Monad" from the mtl library to define the classic Von Neumann state 
transitions in the simulator.

    1. Fetch
    2. Decode
    3. Execute
    4. Store2 -- an extra state for memory writes


Composes various state transformations into larger ones via the >>= operator
-}

import Prelude hiding (read)

import Binary
import Data.Array
import Data.Proxy
import Data.Maybe

import qualified Control.Monad.State as ST
import qualified Control.Monad as M

import MachTypes

-- states for the underlying FSA
data Event = Fetch | Decode | Execute | Store2 deriving Eq

class MachCmp f where
    tick :: f a -> f a
    write :: a -> f a -> f a
    read :: f a -> a

data DFF a = DFF { d :: a, q :: a}

instance MachCmp DFF where
    tick :: DFF a -> DFF a
    tick dff' = dff' {q = d dff'}

    read :: DFF a -> a
    read = q

    write :: a -> DFF a -> DFF a
    write a dff' = dff' {d = a}

data Reg a = Reg {
    dff :: DFF a,
    we :: Bool
}

instance MachCmp Reg where
    tick :: Reg a -> Reg a
    tick r
        | we r = r {dff = tick (dff r), we = False}
        | otherwise = r
    
    read :: Reg a -> a
    read = read . dff

    write :: a -> Reg a -> Reg a
    write a r = r {we = True, dff = write a (dff r)}

data RAM a w = 
    RAM 
    { mem :: Array a (Reg w)
    , select :: a
    , memIn :: w
    , memOut :: w
    }



instance Ix a => MachCmp (RAM a) where
    tick :: RAM a w -> RAM a w
    tick r =
        let
            r' = r { mem = tick <$> mem r }
        in
            r' { memOut = read $ mem r' ! select r' }

    read :: RAM a w -> w
    read = memOut

    write :: w -> RAM a w -> RAM a w
    write w r =
        let
            reg' = write w $ mem r ! select r
            mem' = mem r // [(select r, reg')]
        in
            r { mem = mem', memIn = w}

data CPU a w i = CPU {
    pc :: Reg a,
    ac :: Reg w,
    ir :: Reg i
}

transition :: Event -> ST.State MachState ()
transition e
    | Fetch <- e = fetch
    | Decode <- e = decode
    | Execute <- e = execute
    | Store2 <- e = store2

-- S ("state") threaded through the State Monad
data S a w i = S {
    ram :: RAM a w,
    cpu :: CPU a w i,
    prev :: Maybe (S a w i),
    next :: Event
}

type MachState = S MachAddr MachWord MachInsn

-- loader output to initial state
initState :: Program -> MachState
initState p = S {
    ram = RAM {
        mem = array (0, 255) 
            [fromMaybe (i, regOfVal 0) $ flip (,) <$> fmap regOfVal (lookup i (bindings p)) <*> pure i | i <- fromIntegral <$> [0..255]],
        select = 0,
        memIn = 0,
        memOut = 0
    },
    cpu = CPU {
        pc = regOfVal (start p),
        ac = regOfVal 0,
        ir = regOfVal NOP
    },
    prev = Nothing,
    next = Fetch
}
    where
        regOfVal :: a -> Reg a
        regOfVal a = Reg { dff = DFF{d = a, q = a}, we = False}

tickRegs :: ST.State MachState ()
tickRegs = 
    ST.get >>=
        \s -> ST.put s{
            cpu = updateRegs (cpu s)
        }
    where
    updateRegs :: CPU a w i -> CPU a w i
    updateRegs c = c {
        pc = tick (pc c),
        ac = tick (ac c),
        ir = tick (ir c)
    }

-- simulate a rising clock edge by calling "tick" on clocked components
risingEdge :: ST.State MachState ()
risingEdge = 
    tickRegs >> ST.get >>=
        \s -> ST.put $ s { ram = tick (ram s) }

-- PC := PC + 1
-- MEMOUT := MEM[PC]
fetch :: ST.State MachState ()
fetch = do
    risingEdge
    s <- ST.get
    r <- ST.gets ram
    c <- ST.gets cpu
    idx <- ST.gets (read . pc . cpu)

    ST.put s {
        cpu = c { pc = write (idx + 1) (pc c)},
        ram = r { select = idx },
        prev = Just s,
        next = Decode
    }
    
-- IR := Decode (MEMOUT)
decode :: ST.State MachState ()
decode = do
    risingEdge
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    ireg <- ST.gets (ir . cpu)
    mout <- ST.gets (memOut . ram)

    let opcode = extract @8 @15 mout
    let operand = extract @0 @7 mout
    let insn = fromMaybe NOP (MachInsn <$> lookup opcode opcodes <*> pure operand)

    ST.put s {
        cpu = c { ir = write insn ireg },
        ram = r { select = operand},
        prev = Just s,
        next = Execute
    }

    where
        opcodes :: [(Binary 8, Opcode)]
        opcodes = [
                        (0, Load),
                        (1, Store),
                        (2, Jump),
                        (3, JumpZ),
                        (4, JumpN),
                        (5, Add),
                        (6, Sub),
                        (7, Mul),
                        (8, Out),
                        (9, JumpNZ)
                ]

{- 

"Run" an instruction
    1. control (Jump, JumpZ, JumpNZ, JumpN) -> PC := payload
    2. arithmetic (Add, Sub, Mul) -> AC := MEMOUT `op` AC
    3. mem
        STORE -> MEMIN := AC
        LOAD -> AC := MEMOUT
-}
execute :: ST.State MachState ()
execute = do
    risingEdge
    s <- ST.get
    c <- ST.gets cpu
    r <- ST.gets ram
    mdata <- ST.gets (read . ram)
    pcreg <- ST.gets (pc . cpu)
    acreg <- ST.gets (ac . cpu)
    acval <- ST.gets (read . ac . cpu)
    insn <- ST.gets (read . ir . cpu)

    let s' = s { prev = Just s, next = Fetch }

    case insn of
        NOP -> ST.put s'
        MachInsn Load _ ->
            ST.put s' { cpu = c {ac = write mdata acreg} }
        MachInsn Store a -> do
            let r' = write acval $ r { select = a }
            ST.put s' { ram = r', next = Store2}
        MachInsn Jump a -> do
            ST.put s' { cpu = c {pc = write a pcreg } }
        MachInsn JumpZ a ->
            if acval == 0 
                then ST.put s' { cpu = c {pc = write a pcreg } }
                else ST.put s'
        MachInsn JumpN a ->
            if acval < 0
                then ST.put s' { cpu = c {pc = write a pcreg } }
                else ST.put s'
        MachInsn JumpNZ a ->
            if acval /= 0
                then ST.put s' { cpu = c {pc = write a pcreg } }
                else ST.put s'
        MachInsn Add _ ->
            ST.put s' { cpu = c {ac = write (mdata + acval) acreg} }
        MachInsn Sub _ ->
            ST.put s' { cpu = c {ac = write (mdata - acval) acreg} }
        MachInsn Mul _ ->
            ST.put s' { cpu = c {ac = write (mdata * acval) acreg} }
        MachInsn Out _ -> error "TODO"

-- tick one extra time on stores so values propagate through
store2 :: ST.State MachState ()
store2 = do 
    risingEdge
    s <- ST.get
    ST.put s { prev = Just s, next = Fetch }

step :: ST.State MachState ()
step = ST.gets next >>= transition

stepN :: Int -> ST.State MachState ()
stepN 0 = pure ()
stepN n = step >> stepN (n - 1)

stepWhile :: (MachState -> Bool) -> ST.State MachState ()
stepWhile p = do
    s <- ST.get
    M.when (p s) (step >> stepWhile p)

run :: ST.State MachState a -> MachState -> MachState
run = ST.execState



