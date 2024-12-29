{-# LANGUAGE DataKinds #-}
module Loader where

import Binary
import MachTypes

{-
Loader takes a list of parsed ASM

    1. align into memory

    2. resolve labels

    3. return a "Program" representation for use in the simulator

-}

opcodes :: [(Opcode, Binary 8)]
opcodes = [
        (Load, 0),
        (Store, 1),
        (Jump, 2),
        (JumpZ, 3),
        (JumpN, 4),
        (JumpNZ, 9),
        (Add, 5),
        (Sub, 6),
        (Mul, 7),
        (Out, 8)
    ]

-- align assembly to memory locations
memLayout :: Integral a => a -> [ASM] -> [(MachAddr, ASM)]
memLayout sz asm = zip (fromIntegral <$> [sz - 1, sz - 2..0]) asm

-- use label assocs to convert ASM to machine code
encodeASM :: [(Label, MachAddr)] -> ASM -> Maybe MachWord
encodeASM labels (Insn _ op arg) =
    case arg of
        B b -> (`append` fromIntegral b) <$> lookup op opcodes 
        L l -> append <$> lookup op opcodes <*> lookup l labels
encodeASM _ (Def _ b) = Just (fromIntegral b)

-- generate label - address associations
labelLayout :: [(MachAddr, ASM)] -> Maybe [(Label, MachAddr)]
labelLayout = traverse getLabel . filter (hasLabel . snd)

hasLabel :: ASM -> Bool
hasLabel (Insn (Just _) _ _) = True
hasLabel (Def _ _) = True
hasLabel _ = False

getLabel :: (MachAddr, ASM) -> Maybe (Label, MachAddr)
getLabel (addr, Insn (Just l) _ _) = Just (l, addr)
getLabel (addr, Def l _) = Just (l, addr)
getLabel _ = Nothing

loader :: [ASM] -> Maybe Program
loader asm = do
    let constants = filter (\asm -> case asm of {Def _ _ -> True; _ -> False}) asm
    let insns = filter (\asm -> case asm of {Insn _ _ _ -> True; _ -> False}) asm
    let layout = memLayout memSize (reverse constants ++ reverse insns)
    labels <- labelLayout layout
    as <- traverse (traverse (encodeASM labels)) layout
    s <- lookup "start" labels
    e <- lookup "done" labels
    return Program { bindings = as, start = s, end = e }
