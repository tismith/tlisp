module Main where
import System.Environment

main :: IO ()
main = do 
        args <- getArgs
        let arg1 = read $ args !! 0
        let arg2 = read $ args !! 1
        let total = arg1 + arg2 :: Int
        putStrLn ("Hello, the total is " ++ (show total))
