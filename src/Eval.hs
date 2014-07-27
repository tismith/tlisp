{-# LANGUAGE FlexibleContexts #-}
module Eval (eval) where
import Primitives
import LispVals
import LispEnvironment

import Control.Monad.Trans (lift)
import Control.Monad (liftM)
import Control.Monad.Cont (ContT)
import Control.Monad.Error (throwError, MonadError)
import Data.Maybe (isNothing)

eval :: LispVal -> ContT LispVal IOThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Complex _) = return val
eval val@(Ratio _) = return val
eval val@(Float _) = return val
eval val@(Char _) = return val
eval val@(Vector _) = return val
eval (Atom i) = lift $ getVar i
eval (List [Atom "quote", val]) = return val
eval e@(List [Atom "quasiquote", _]) = lift $ throwError $ BadSpecialForm "Quasiquotes not implemented" e
eval e@(List [Atom "unquote", _]) = lift $ throwError $ BadSpecialForm "Unquotes not implemented" e
eval (List [Atom "if", p, conseq, alt]) =
    do result <- eval p
       case result of
         Bool False -> eval alt
         _ -> eval conseq
----------------------------------------------------
-- eval c (List [Atom "if", p, conseq]) =
--     do result <- eval c p
--        case result of
--          Bool False -> throwError $ Unspecified "if without an else"
--          _ -> eval c conseq
---------------------------------------------------- 
----------------------------------------------------
-- eval c (List (Atom "cond":(clause:cs))) =
--     do result <- foldl (chainEvalClause c evalCondClause) (evalCondClause clause) cs
--        case result of
--             Just l -> c l
--             Nothing -> throwError $ Unspecified "no matching cond"
-- eval c (List (Atom "case":key:clause:cs)) =
--     do evalKey <- eval c key
--        result <- foldl (chainEvalClause c (evalCaseClause evalKey)) (evalCaseClause c evalKey clause) cs
--        case result of
--             Just l -> c l
--             Nothing -> throwError $ Unspecified "no matching case"
---------------------------------------------------- 
eval (List [Atom "set!", Atom var, form]) = eval form >>= lift . setVar var
eval (List [Atom "string-set!", Atom var, i, ch]) =
    do i' <- eval i
       index <- lift . liftThrows $ unpackNum i'
       ch' <- eval ch
       char <- lift . liftThrows $ unpackChar ch'
       v <- lift $ getVar var
       str <- lift . liftThrows $ unpackStr v
       let (f,s) = splitAt (fromIntegral index) str
       case s of
            [] -> lift $ throwError $ Unspecified "invalid index"
            _ -> lift $ setVar var (String $ f ++ [char] ++ drop 1 s)
eval (List [Atom "string-fill!", Atom var, ch]) =
    do v <- lift $ getVar var
       ch' <- eval ch
       char <- lift . liftThrows $ unpackChar ch'
       str <- lift . liftThrows $ unpackStr v
       lift $ setVar var (String $ map (const char) str)
eval (List [Atom "define", Atom var, form]) = eval form >>= lift . defineVar var
----------------------------------------------------
-- eval c (List (Atom "define" : List (Atom var : p) : b)) =
--     makeNormalFunc p b >>= defineVar var >>= c
-- eval c (List (Atom "define" : DottedList (Atom var : p) varargs : b)) =
--     makeVarargs varargs p b >>= defineVar var >>= c
-- eval c (List (Atom "lambda" : List p : b)) =
--     makeNormalFunc p b >>= c
-- eval c (List (Atom "lambda" : DottedList p varargs : b)) =
--     makeVarargs varargs p b >>= c
-- eval c (List (Atom "lambda" : varargs@(Atom _) : b)) =
--     makeVarargs varargs [] b >>= c
---------------------------------------------------- 
----------------------------------------------------
-- eval c (List (Atom "apply" : args)) = do
--     argVals <- mapM eval args
--     applyProc argVals >>= c
-- eval c (List [Atom "load", String filename]) =
--     load filename >>= liftM last . mapM eval >>= c
---------------------------------------------------- 
----------------------------------------------------
-- eval c (List (function : args)) = do
--     func <- eval c function
--     argVals <- mapM (eval c) args
--     apply func argVals >>= c
---------------------------------------------------- 
eval badForm = lift $ throwError $ BadSpecialForm "Unrecognized special form" badForm

----------------------------------------------------
-- chainEvalClause :: (LispVal -> IOThrowsError (Maybe LispVal)) -> IOThrowsError (Maybe LispVal) -> LispVal -> IOThrowsError (Maybe LispVal)
-- chainEvalClause evalFunc evalC unevalC = do
--                         clause <- evalC
--                         case clause of
--                             Just _ -> return clause
--                             Nothing -> evalFunc unevalC
-- 
-- evalCaseClause :: LispVal -> LispVal -> IOThrowsError (Maybe LispVal)
-- evalCaseClause key (List (datums:(exprs@(_:_)))) = --exprs can't be []
--     case datums of
--         Atom "else" -> do
--                 evalExprs <- mapM eval exprs
--                 return $ Just $ last evalExprs
--         List l -> do success <- liftThrows . liftM or $ mapM (\x -> eqv [x, key] >>= unpackBool) l
--                      if success
--                         then do
--                             evalExprs <- mapM eval exprs
--                             return $ Just $ last evalExprs
--                         else return Nothing
--         e -> throwError $ TypeMismatch "list" e
-- evalCaseClause _ badForm = throwError $ BadSpecialForm "Unrecognized case clause form" badForm
-- 
-- evalCondClause :: LispVal -> IOThrowsError (Maybe LispVal)
-- evalCondClause (List (test:[])) =
--     do success <- eval test
--        case success of
--             Bool True -> return $ Just success
--             Bool False -> return Nothing
--             _ -> throwError $ TypeMismatch "boolean" success
-- evalCondClause (List (test:exprs)) =
--     case test of
--         Atom "else" -> do
--             evalExprs <- mapM eval exprs
--             return $ Just $ last evalExprs
--         _ -> do success <- eval test
--                 case success of
--                     Bool True -> do
--                         evalExprs <- mapM eval exprs
--                         return $ Just $ last evalExprs
--                     Bool False -> return Nothing
--                     _ -> throwError $ TypeMismatch "boolean" success
-- evalCondClause badForm = throwError $ BadSpecialForm "Unrecognized cond clause form" badForm
-- 
-- apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
-- apply (IOFunc func) args = func args
-- apply (PrimitiveFunc func) args = liftThrows $ func args
-- apply (Func ps vs b c) args = do
--     originalEnv <- getEnv
--     if num ps /= num args && isNothing vs
--        then throwError $ NumArgs (num ps) args
--        else do
--         -- this is not a nice way to handle a stack frame, just splatting over
--         -- the top
--         bindVars $ envToList c
--         bindVars $ zip ps args
--         bindVarArgs vs
--         r <- evalBody
--         -- and then manually unwinding it
--         putEnv originalEnv
--         return r
--     where remainingArgs = drop (length ps) args
--           num = toInteger . length
--           evalBody = liftM last $ mapM eval b
--           bindVarArgs arg = case arg of
--               Just argName -> bindVars [(argName, List remainingArgs)]
--               Nothing -> return ()
-- apply e _ = throwError $ TypeMismatch "function" e
-- 
-- makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
-- makeFunc varargs p b = do
--     env <- getEnv
--     return $ Func (map show p) varargs b env
-- 
-- makeNormalFunc :: [LispVal] -> [LispVal] -> IOThrowsError LispVal
-- makeNormalFunc = makeFunc Nothing
-- 
-- makeVarargs :: LispVal -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
-- makeVarargs = makeFunc . Just . show
-- 
-- applyProc :: [LispVal] -> IOThrowsError LispVal
-- applyProc [func, List args] = apply func args
-- applyProc (func : args) = apply func args
-- applyProc e = throwError $ NumArgs 2 e
-- 
---------------------------------------------------- 
