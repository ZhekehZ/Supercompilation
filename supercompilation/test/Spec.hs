import TestProgram
import Eval
import Lang

main :: IO ()
main = print $ evalProgram testProgram [("arg0", 3)]
