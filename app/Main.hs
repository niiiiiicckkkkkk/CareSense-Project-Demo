module Main where

{-
Module main

Usage ex:

    cabal run 3364mach "path-to-assembly-file"

    help (list out available commands)

    step 3 ("clock" ticks 3 times)

    xReg PC (examine the PC reg)

    xReg AC (examine the AC (accumulator) reg)

    xReg IR (examine the IR reg)

    xMem 0 255 (spit out memory binding for address 0-255)

    undo 7 (step back 7 clock ticks)

    quit
-}

import Parser
import qualified Simulator as Sim
import System.Environment
import System.IO
import MachTypes
import Binary
import Data.Array ((!), elems, indices, assocs, range)
import Text.Read(readMaybe)
import Data.Maybe(fromMaybe)


data RegKind = IR | PC | AC deriving Read

data Command 
    = Step Int
    | Undo Int
    | Help
    | Quit
    | InspectReg RegKind
    | InspectMem (MachAddr, MachAddr)

getCommand :: IO (Maybe Command)
getCommand = do
    putStr "enter a command: "
    input <- getLine
    return $
        case words input of
            ["step", arg] -> Step <$> readMaybe arg
            ["undo", arg] -> Undo <$> readMaybe arg
            ["quit"] -> pure Quit
            ["help"] -> pure Help
            ["xReg", arg] -> InspectReg <$> readMaybe arg
            ["xMem", lo, hi] -> curry InspectMem <$> (fromInteger <$> readMaybe lo) <*> (fromInteger <$> readMaybe hi)
            _ -> pure Help



simulate :: Sim.MachState -> IO ()
simulate s = do
    cmd <- fromMaybe Help <$> getCommand
    case cmd of
        InspectMem (lo, hi) ->
            let bindings = (,) <$> id <*> (Sim.read . ((Sim.mem . Sim.ram) s !)) <$> range (lo, hi)
            in do
                putStrLn $ "mem input: " <> (show . sBinToInteger . Sim.memIn . Sim.ram) s
                putStrLn $ "mem output: " <> (show . sBinToInteger . Sim.memOut . Sim.ram) s
                putStrLn $ "select line: " <> (show . uBinToInteger . Sim.select . Sim.ram) s
                print $ (,) <$> (uBinToInteger . fst) <*> (sBinToInteger . snd) <$> bindings
                simulate s
        Step n -> simulate $ Sim.run (Sim.stepN n) s
        Undo n -> reverse n s >>= simulate
        InspectReg r -> do
            case r of
                IR -> do
                    let reg = Sim.ir . Sim.cpu $ s
                    putStrLn $ "reg input: " <> printInsn ((Sim.d . Sim.dff) reg)
                    putStrLn $ "reg output: " <> printInsn ((Sim.q . Sim.dff) reg)
                    putStrLn $ "reg WE: " <> show (Sim.we reg)
                PC -> do
                    let reg = Sim.pc . Sim.cpu $ s
                    putStrLn $ "reg input: " <> (show . uBinToInteger) ((Sim.d . Sim.dff) reg)
                    putStrLn $ "reg output: " <> (show . uBinToInteger) ((Sim.q . Sim.dff) reg)
                    putStrLn $ "reg WE: " <> show (Sim.we reg)
                AC -> do
                    let reg = Sim.ac . Sim.cpu $ s
                    putStrLn $ "reg input: " <> (show . sBinToInteger) ((Sim.d . Sim.dff) reg)
                    putStrLn $ "reg output: " <> (show . sBinToInteger) ((Sim.q . Sim.dff) reg)
                    putStrLn $ "reg WE: " <> show (Sim.we reg)
            simulate s
        Help -> putStr helpText >> simulate s
        Quit -> return ()
    where
        printInsn :: MachInsn -> String
        printInsn (MachInsn opcode payload) = show opcode <> " " <> (show . uBinToInteger) payload
        printInsn NOP = "NOP"

        reverse :: Int -> Sim.MachState -> IO Sim.MachState
        reverse 0 st = pure st
        reverse n st
            | Just st' <- Sim.prev st = reverse (n - 1) st'
            | otherwise = putStrLn "cannot reverse state s0" >> return st

        helpText :: String
        helpText = unlines 
            [ "command options:"
            , "step <numsteps>"
            , "undo <numsteps>"
            , "xReg <PC/AC/IR>"
            , "xMem <lowerbound> <upperbound>"
            , "help"
            , "quit"
            ]

loadSim :: String -> IO ()
loadSim file = do
    asm <- readFile file

    let program = parseASM asm

    case program of
        Nothing -> putStrLn "parse fail"
        Just p -> simulate $ Sim.initState p


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    args <- getArgs

    case args of
        [] -> putStrLn "no file provided"
        [f] -> loadSim f
        args@(_ : _) -> 
            putStrLn $ "recieved " ++ show (length args) ++ " arguments but expected 1"

