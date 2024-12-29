module Main where

    import Test.HUnit (runTestTT, Test, (~?=))
    import qualified Parser
    import qualified Binary
    import qualified Simulator as Sim
    import MachTypes
    import Data.Array ((!))

    testProgram :: String
    testProgram = unlines
        [ "# Count forever (used for testing)"
        , "start:  load    one     # ac := one"
        , "add     count           # ac := count + 1"
        , "store   count           # count := count + 1"
        , "jump    start           # count again!"
        , "done:   jump    start   # go back to start"
        , "count:   .data   0      # accumulate the results here"
        , "one:     .data   1      # constant 1"
        ]

    testState :: Sim.MachState
    testState =
        case Sim.initState <$> Parser.parseASM testProgram of
            Nothing -> error "failed to parse test program"
            Just st -> st


    -- every 13 ticks is one loop
    -- definetly better ways to test the simulation
    test_fetchSetPC :: Test
    test_fetchSetPC =
        let machStates = (,) <$> (Sim.d . Sim.dff . Sim.pc . Sim.cpu) <*> (Sim.read . (! 254) . Sim.mem  . Sim.ram) <$> go 100 testState
        in machStates ~?= ([(249, fromIntegral i) | i <- [0..100]] :: [(MachAddr, MachWord)])
        where
            go :: Int -> Sim.MachState -> [Sim.MachState]
            go 0 s = [s]
            go n s = s : go (n - 1) (Sim.run (Sim.stepN 13) s)



    main :: IO ()
    main = do
        putStrLn "running parser module tests"
        Parser.test_asmP
        putStrLn "running binary module tests"
        Binary.test_binary
        putStrLn "running test on counting program"
        runTestTT test_fetchSetPC
        return ()