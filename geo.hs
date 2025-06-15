-- a main module with the main function which then computes a desired result

import System.Environment (getArgs)
import Convex
import Closest
import Intersect


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["convex", nStr] -> case reads nStr of -- reading an input and parsing the value n to the integer
            [(n, "")] -> convex n --if parsed successfully, then we perform an operation
            _         -> putStrLn "Please provide a valid integer."
        ["closest", nStr] -> case reads nStr of
            [(n, "")] -> closest n
            _         -> putStrLn "Please provide a valid integer."  
        ["intersect", nStr] -> case reads nStr of
            [(n, "")] -> intersect n
            _         -> putStrLn "Please provide a valid integer."
