{-# LANGUAGE CPP #-}
module Main where
import Parse
import Eval
import LispVals
import Primitives
import LispEnvironment

import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Environment (getArgs)
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
          if null args then runRepl else runOne $ args

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
         >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
    setInhibitCompletion True
    env <- primitiveBindings
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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                            ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)
