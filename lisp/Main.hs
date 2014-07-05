module Main where
import Parse
import Eval
import System.IO hiding (try)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Control.Monad
import Control.Monad.Error

main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              otherwise -> putStrLn "Program takes only 0 or 1 argument"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalAndPrint expr =  evalString expr >>= putStrLn

until' :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until' pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until' pred prompt action

runRepl :: IO ()
runRepl = until' (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
