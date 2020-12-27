import Parser
import IntProgram
import System.Environment
import Lang
import Data.Char
import System.Exit

printHelp = do
    putStrLn "Usage:"
    putStrLn "    sc <input file> <output file> args"
    putStrLn ""
    putStrLn "    args format: "
    putStrLn "        key1 val1 [key2 val2 ...]"
    putStrLn ""
    putStrLn "    value must be a string or integer"
    putStrLn ""
    putStrLn "Example:"
    putStrLn "    sc input.txt output.txt p AAA"

invalidCall = do
    printHelp
    exitFailure

parseIntOrString :: String -> Either String Int
parseIntOrString s = if all isDigit s then Right $ read s else Left s

parseArgs :: [String] -> Maybe COMPILEARGUMENTS
parseArgs [] = Just []
parseArgs [x] = Nothing
parseArgs (k:v:as) = case parseIntOrString v of 
                        Left s -> ((k, strToTerm s) :) <$> parseArgs as
                        Right i -> ((k, Val i) :) <$> parseArgs as

main :: IO ()
main = do
    args <- getArgs
    case args of 
        input : output : args | Just cArgs <- parseArgs args -> do
            text <- readFile input
            let program = parseProg text
            let compiled = compile program cArgs
            writeFile output (show compiled)
        _ -> invalidCall
    