module Main where
import System.Environment

main :: IO ()
main = do 
        putStrLn "what's your name?"
        name <- getLine
        putStrLn $ "hello, " ++ name
