module Main where
import Parse
import Eval
import LispVals
import Primitives
import LispEnvironment

import Control.Monad.Trans (lift, liftIO)
-- need the strict state monad for monadexception
import Control.Monad.State.Strict (StateT, put, get, evalStateT)
import Control.Monad.Cont (runContT)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Control.Monad (liftM, when)
import System.Console.Haskeline (InputT, runInputT, getInputLine, defaultSettings, setComplete, Completion, CompletionFunc)
import System.Console.Haskeline.Completion (completeWord, completeQuotedWord, simpleCompletion)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args

evalString :: Env -> String -> IO (String, Env)
evalString env expr = runIOThrows (liftM show $ runEval ((lift $ liftThrows (readExpr expr)) >>= eval)) env

evalAndPrint :: Env -> String -> IO Env
evalAndPrint env expr = do
    (out, newEnv) <- evalString env expr
    putStrLn out
    return newEnv

runOne :: [String] -> IO ()
runOne args = do
    (out, _) <- runIOThrows (liftM show $ runEval
            (bindVars [("args", List $ map String $ drop 1 args)] >> eval (List [Atom "load", String (head args)])))
        primitiveBindings
    hPutStrLn stderr out

runRepl :: IO ()
runRepl = evalStateT (runInputT (setComplete replComplete defaultSettings) replLoop) primitiveBindings

replComplete :: CompletionFunc (StateT Env IO)
replComplete = completeQuotedWord Nothing "\"\'" (const $ return []) symbolComplete

symbolComplete :: CompletionFunc (StateT Env IO)
symbolComplete = completeWord Nothing "() \t" completeEnv

completeEnv :: String -> (StateT Env IO) [Completion]
completeEnv s = do
    env <- get
    let keys = envSymbols env
    return $ map simpleCompletion $ filter (isPrefixOf s) keys

doQuit :: IO Bool
doQuit = putStrLn "Leaving tlisp" >> return False

doHelp :: IO ()
doHelp = putStrLn "Welcome to tlisp!\n\t:quit\t\tExits the repl\n\t:help\t\tThis message"

showBinding :: (String, LispVal) -> String
showBinding (s,l) = s ++ " -> " ++ show l

doEnv :: Env -> IO ()
doEnv e = putStrLn $ unlines $ map showBinding $ envToList e

handleCommand :: Env -> String -> IO Bool
handleCommand e s = case s of
    "quit" -> doQuit
    "q" -> doQuit
    "help" -> doHelp >> return True
    "h" -> doHelp >> return True
    "env" -> doEnv e >> return True
    "e" -> doEnv e >> return True
    _ -> putStrLn ("Unknown command :" ++ s) >> return True

replLoop :: InputT (StateT Env IO) ()
replLoop = do
    maybeLine <- getInputLine "tlisp>>> "
    case maybeLine of
        Nothing -> return ()
        Just line -> do
            let trimmedLine = dropWhile isSpace line
            if not $ null trimmedLine
            then case trimmedLine of
                    (':':command) -> do
                        env <- lift get
                        continue <- liftIO $ handleCommand env command
                        when continue replLoop
                    _ -> lift get >>= \e -> liftIO (evalAndPrint e trimmedLine) >>= lift . put >> replLoop
            else replLoop

primitiveBindings :: Env
primitiveBindings = envFromList (map (mF IOFunc) ioPrimitives ++ map (mF PrimitiveFunc) primitives)
    where mF constructor (var, func) = (var, constructor func)
