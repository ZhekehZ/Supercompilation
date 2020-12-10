import TestProgram
import Eval

main :: IO ()
main = print $ evalProgram testProgram (EC [("arg0", 2)] evalIntF evalIntP)
