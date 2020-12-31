import IntProgram
import Program1
import Program2

import Lang
import Data.Foldable
import Data.IORef
import Control.Monad
import System.Exit


main :: IO ()
main = do   
    putStrLn "--- SIMPLE ---\n-------------"
    
    results <- newIORef []
    totalFails <- newIORef 0
    
    for_ (zip3 [1..] programs tests) $ \(index, program, tests) -> do 
        putStrLn $ ">> Program #" ++ show index
        putStrLn ">> Text:"
        print program
        putStrLn ""

        success <- newIORef 0
        fail <- newIORef 0

        for_ (zip [1..] tests) $ \(index, (scArgs, runArgs, expected)) -> do
            putStrLn ("#> Test " ++ show index) 
            putStrLn "   SuperCompilation arguments:"
            for_ scArgs $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ show val
            putStrLn "   Evaluation arguments:"
            for_ runArgs $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ show val
            
            let compiled = compile program scArgs
            let evaluated = eval compiled runArgs
            let withoutSC = eval program (scArgs ++ runArgs)

            putStrLn $ "   Eval: " ++ show withoutSC


            putStr "   Result: "
            if evaluated == expected 
            then do putStrLn $ "SUCCESS (" ++ show expected ++ " = " ++ show evaluated ++ ")" 
                    modifyIORef success (+1) 
            else do putStrLn $ "FAIL (expected: " ++ show expected ++ ", got: " ++ show evaluated ++ ")"
                    putStrLn "   Compiled:"
                    print compiled
                    modifyIORef fail (+1) 

            putStrLn "\n"

        
        res <- (,) <$> readIORef success <*> readIORef fail
        modifyIORef results (res :)

        putStrLn "-------------"
    putStrLn "---  End  ---"
    
    res <- readIORef results
    for_ (zip [1..] (reverse res)) $ \(i, (succ, fail)) -> do
        putStrLn $ "Test " ++ show i ++ ": " ++ show succ ++ " successful, " ++ show fail ++ " failed"
        modifyIORef totalFails (+ fail)



    putStrLn "\n\n---MatrixTests---"

    results <- newIORef []

    for_ (zip [1..] matrixTests) $ \(i, (compileArgs, evalArgs, printCompiled)) -> 
        for_ compileArgs $ \ca -> do
            putStrLn "Compilation args: "
            for_ ca $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ repr val
            let scProg = compile program2 ca
            print scProg

            success <- newIORef 0
            fail <- newIORef 0

            putStrLn ""

            for_ evalArgs $ \ea -> do 
                let got = eval scProg ea
                let expected = eval program2 (ea ++ ca)
                putStr "Test case : "
                for_ ea $ \(name, val) -> putStr $ name ++ " = " ++ repr val ++ "   "
                
                putStrLn $ if expected == got then ": SUCCESS (" ++ show got ++ ")" else ": FAIL"
                if expected == got
                    then modifyIORef success (+1)
                    else modifyIORef fail (+1)


            res <- (,) <$> readIORef success <*> readIORef fail
            modifyIORef results (res :)

            putStrLn ""


    res <- readIORef results
    for_ (zip [1..] (reverse res)) $ \(i, (succ, fail)) -> do
        putStrLn $ "Test " ++ show i ++ ": " ++ show succ ++ " successful, " ++ show fail ++ " failed"
        modifyIORef totalFails (+ fail)

    putStrLn "----ALLDONE----"
    
    shouldBeZero <- readIORef totalFails
    
    when (shouldBeZero > 0) $ die $ "FAILED (" ++ show shouldBeZero ++ " errors total)"

    kmpTestCountSteps

    where
        -- Simple test 
        programs = [program1, program2]
        tests = [prog1Tests, prog2Tests ]

        -- Matrix test
        matrixTests = [prog2MatrixTests, prog2MatrixTestsRev] :: [TESTMATRIX]

        kmpTestCountSteps = do
            putStrLn "Performance test"

            let (p, s, _) = prog2MatrixTests
            let ss = snd . head <$> s
            let ps = snd . head <$> p
            
            let minCellWidth = 9
            let cellWidths = map (max minCellWidth . length . repr) ps
            let fstWid = foldl max 0 (length . repr <$> ss)

            putStr $ replicate fstWid ' '
            for_ (zip ps cellWidths) $ \(pat, wid) -> do
                let patStr = repr pat
                putStr $ '|' : patStr ++ replicate (wid - length patStr) ' '
            putStrLn ""

            let Program defs _ = program2
            let line = replicate fstWid '─' ++ foldl (\s w -> s ++ '┼' : replicate w '─') "" cellWidths

            for_ ss $ \str -> do
                let strStr = repr str
                putStrLn line
                putStr $ strStr ++ replicate (fstWid - length strStr) ' '
                for_ (zip ps cellWidths) $ \(pat, wid) -> do
                    let compiled = compile program2 [("p", pat)]
                    let evaled = eval program2 [("p", pat)]
                    let (res1, compCount) = evalAndCountSteps compiled [("s", str)]
                    let (res2, evalCount) = evalAndCountSteps (Program (Def "" evaled:defs) "") [("s", str)]
                    let resStr = show compCount ++ "/" ++ show evalCount
                    putStr $ '|' : resStr ++ replicate (wid - length resStr) ' '
                putStrLn ""
                    

