{-# LANGUAGE CPP #-}
module Main where
import Parse
import Eval
import LispVals
import LispEnvironment

import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Control.Monad (liftM, when)
import Control.Monad.Error (throwError)
#ifdef LIBEDITLINE
import System.Console.Editline.Readline (readline, addHistory, setInhibitCompletion)
#else
import System.Console.Readline (readline, addHistory, setInhibitCompletion)
#endif
import Data.Char (isSpace)

main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> runOne $ args !! 0
              otherwise -> putStrLn "Program takes only 0 or 1 argument"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = do
    setInhibitCompletion True
    env <- nullEnv
    replLoop env

doQuit :: IO Bool
doQuit = putStrLn "Leaving tlisp" >> return False

doHelp :: IO Bool
doHelp = putStrLn "Welcome to tlisp!\n\t:quit\t\tExits the repl\n\t:help\t\tThis message" >> return True

handleCommand :: String -> IO Bool
handleCommand s = case s of
    "quit" -> doQuit
    "q" -> doQuit
    "help" -> doHelp
    "h" -> doHelp
    _ -> putStrLn ("Unknown command :" ++ s) >> return True

replLoop :: Env -> IO ()
replLoop env = do
    maybeLine <- readline "tlisp>>> "
    case maybeLine of
        Nothing -> return ()
        Just line -> do
            let trimmedLine = dropWhile (isSpace) line
            if (not $ null trimmedLine)
            then do
                addHistory trimmedLine
                case trimmedLine of
                    (':':command) -> do
                        continue <- handleCommand command
                        if continue then
                            replLoop env
                        else
                            return ()
                    _ -> evalAndPrint env trimmedLine >> replLoop env
            else
                replLoop env
