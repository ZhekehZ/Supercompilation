import IntProgram
import Program1
import Program2

import Lang
import Data.Foldable


main :: IO ()
main = do
    putStrLn "--- Start ---\n-------------"
    for_ (zip3 [1..] programs tests) $ \(index, program, tests) -> do 
        putStrLn $ ">> Program #" ++ show index
        putStrLn ">> Text:"
        print program
        putStrLn ""

        for_ (zip [1..] tests) $ \(index, (scArgs, runArgs, expected)) -> do
            putStrLn ("#> Test " ++ show index) 
            putStrLn "   SuperCompilation arguments:"
            for_ scArgs $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ show val
            putStrLn "   Evaluation arguments:"
            for_ runArgs $ \(name, val) -> putStrLn $ "     " ++ name ++ " = " ++ show val
            
            let compiled = compile program scArgs
            let evaluated = eval compiled runArgs

            putStrLn $ "   Result: " ++ if evaluated == expected 
                                        then "SUCCESS (" ++ show expected ++ " = " ++ show evaluated ++ ")"
                                        else "FAIL (epected: " ++ show expected ++ ", got: " ++ show evaluated ++ ")"
            putStrLn "\n"

        putStrLn "-------------"
    putStrLn "---  End  ---"
    return ()
    where 
        programs = [program1, program2]
        tests = [prog1Tests, prog2Tests ]