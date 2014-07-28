{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LispVals (
    Env,
    ThrowsError,
    EnvThrowsError,
    IOThrowsError,
    LispVal(..),
    LispError(..),
    LispEval
  ) where

import Data.Ratio (Ratio, numerator, denominator)
import Data.Complex (Complex((:+)))
import qualified Data.Vector as V (toList, Vector)
import qualified Data.Map as M (Map)
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (ErrorT, Error, noMsg, strMsg, MonadError, throwError, catchError)
import Control.Monad.State (State, StateT)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.Trans (lift)
import System.IO (Handle)

type Env = M.Map String LispVal
type ThrowsError = Either LispError
-- ErrorT e m a ~ m (Either e a)
-- State s a ~ s -> (a, s)
-- ErrorT e (State s a) ~ s -> (Either e a, s)
type EnvThrowsError = ErrorT LispError (State Env)
-- StateT s m a ~ s -> m (a, s)
-- ErrorT e (StateT s m) a ~ s -> m (Either e a, s)
-- StateT s (ErrorT e m) a ~ s -> m (Either e (a, s))
type IOThrowsError = ErrorT LispError (StateT Env IO)

type LispEval = ContT LispVal IOThrowsError LispVal
-- mtl doesn't define a MonadError instances for ContT
-- http://stackoverflow.com/questions/10742151/why-cant-contt-be-made-an-instance-of-monaderror
instance MonadError e m => MonadError e (ContT r m) where
  throwError = lift . throwError
  catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)

data LispVal = Atom String
        | List [LispVal]
        | Vector (V.Vector LispVal)
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool
        | Char Char
        | Float Float
        | Complex (Complex Float)
        | Ratio (Ratio Integer)
        | Port Handle
        | Continuation (LispVal -> LispEval)
        | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
        | IOFunc ([LispVal] -> IOThrowsError LispVal)
        | Func
            { params :: [String]
            , vararg :: Maybe String
            , body :: [LispVal], closure :: Env}

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (Continuation _) = "<continuation>"
showVal (String contents) = "\"" ++ unescapeString contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char contents) = "#\\" ++ case contents of
                            '\n' -> "newline"
                            ' ' -> "space"
                            _ -> [contents]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Vector contents) =
    "#(" ++ (unwordsList . V.toList) contents ++ ")"
showVal (Float contents) = show contents
showVal (Complex (r :+ i))
    | i > 0 = show r ++ "+" ++ show i ++ "i"
    | i < 0 = show r ++ show i ++ "i"
    | otherwise = show r
showVal (Ratio contents) = show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (Port _) = "<port>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<io primitive>"
showVal (Func {params = args, vararg = varargs}) =
  "(lambda (" ++ unwords (map show args) ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Unspecified String
               | WrongClause
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Unspecified message) = "Unspecified operation: " ++ message
showError (WrongClause) = "No matching clause"
showError (Default message) = "Unknown error: " ++ message

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unescapeString :: String -> String
unescapeString [] = []
unescapeString (x:xs)
    | x == '\n' = "\\n" ++ unescapeString xs
    | x == '\t' = "\\t" ++ unescapeString xs
    | x == '\r' = "\\r" ++ unescapeString xs
    | x == '\"' = "\\\"" ++ unescapeString xs
    | x == '\\' = "\\\\" ++ unescapeString xs
    | otherwise = x : unescapeString xs

