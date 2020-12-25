import IntProgram
import Program1
import Program2

import Lang
import Data.Foldable
import Data.IORef



strings = [ "AAAAAAA"
          , "AAAABBBBBBCCCCC"
          , "BABABABABACBABABABAB"
          , "CCCCCCAAAAAAAACCCCCCAAA"
          , "BABABABABABCAABBBABABAB"
          , "CCCCCAAAAABAAAABACACCC"
          , "BACBBACAAABACBBAC"
          ]

patterns = [ "A"
           , "AA"
           , "AAA"
           , "AB"
           , "ABB"
           , "AAB"
           , "ABC"
           , "ABABAB"
           , "ABCAABB"
           , "AAABAAA"
           , "AAABACA"
           , "AAABACBB"
           ]

maxLenStr = foldl max 0 (length <$> strings)


main :: IO ()
main = do
    for_ patterns $ \pat -> do
            putStrLn $ "Pattern " ++ pat ++ ":"
            let scProg = compile program2 [("p", strToTerm pat)]
            print $ scProg

            for_ strings $ \str -> do 
                putStr $ "str> " ++ str ++ replicate (maxLenStr - length str) ' ' ++ ": " 
                print $ eval scProg [("s", strToTerm str)]

            putStrLn ""

        
    -- putStrLn "--- Start ---\n-------------"
    
    -- results <- newIORef []
    
    -- for_ (zip3 [1..] programs tests) $ \(index, program, tests) -> do 
    --     putStrLn $ ">> Program #" ++ show index
    --     putStrLn ">> Text:"
    --     print program
    --     putStrLn ""

    --     putStrLn "SC without arguments:"
    --     -- print $ compile program []
    --     putStrLn ""

    --     success <- newIORef 0
    --     fail <- newIORef 0

    --     for_ (zip [1..] tests) $ \(index, (scArgs, runArgs, expected)) -> do
    --         putStrLn ("#> Test " ++ show index) 
    --         putStrLn "   SuperCompilation arguments:"
    --         for_ scArgs $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ show val
    --         putStrLn "   Evaluation arguments:"
    --         for_ runArgs $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ show val
            
    --         let compiled = compile program scArgs
    --         let evaluated = eval compiled runArgs
    --         let withoutSC = eval program (scArgs ++ runArgs)

    --         putStrLn $ "   Eval: " ++ show withoutSC


    --         putStr $ "   Result: "
    --         if evaluated == expected 
    --         then do putStrLn $ "SUCCESS (" ++ show expected ++ " = " ++ show evaluated ++ ")" 
    --                 modifyIORef success (+1) 
    --         else do putStrLn $ "FAIL (expected: " ++ show expected ++ ", got: " ++ show evaluated ++ ")"
    --                 putStrLn "   Compiled:"
    --                 print compiled
    --                 modifyIORef fail (+1) 

    --         putStrLn "\n"

        
    --     s <- readIORef success
    --     f <- readIORef fail
    --     modifyIORef results ((s, f) :)

    --     putStrLn "-------------"
    -- putStrLn "---  End  ---"
    
    -- res <- readIORef results
    -- for_ (zip [1..] (reverse res)) $ \(i, (succ, fail)) -> do
    --     putStrLn $ "Test " ++ show i ++ ": " ++ show succ ++ " successful, " ++ show fail ++ " failed"
    
    -- return ()
    -- where 
    --     programs = [program1, program2]
    --     tests = [prog1Tests, prog2Tests ]