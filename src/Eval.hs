{-# LANGUAGE FlexibleContexts #-}
module Eval (eval) where
import Primitives
import LispVals
import LispEnvironment

import Control.Monad.Trans (lift)
import Control.Monad (liftM)
import Control.Monad.Error (throwError, MonadError)
import Control.Monad.Cont (callCC)
import Data.Maybe (isNothing)

eval :: LispVal -> LispEval
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
eval (List [Atom "if", p, conseq]) =
    do result <- eval p
       case result of
         Bool False -> lift $ throwError $ Unspecified "if without an else"
         _ -> eval conseq
eval (List (Atom "cond":(clause:cs))) =
    do result <- foldl (chainEvalClause evalCondClause) (evalCondClause clause) cs
       case result of
            WrongClause -> lift . throwError $ Unspecified "no matching cond"
            _ -> return result
eval (List (Atom "case":key:clause:cs)) =
    do evalKey <- eval key
       result <- foldl (chainEvalClause (evalCaseClause evalKey)) (evalCaseClause evalKey clause) cs
       case result of
            WrongClause -> lift . throwError $ Unspecified "no matching case"
            _ -> return result
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
eval (List (Atom "define" : List (Atom var : p) : b)) =
    lift $ makeNormalFunc p b >>= lift . defineVar var
eval (List (Atom "define" : DottedList (Atom var : p) varargs : b)) =
    lift $ makeVarargs varargs p b >>= lift . defineVar var
eval (List (Atom "lambda" : List p : b)) =
    lift $ makeNormalFunc p b
eval (List (Atom "lambda" : DottedList p varargs : b)) =
    lift $ makeVarargs varargs p b
eval (List (Atom "lambda" : varargs@(Atom _) : b)) =
    lift $ makeVarargs varargs [] b
eval (List (Atom "apply" : args)) = do
    argVals <- mapM eval args
    applyProc argVals
eval (List [Atom "load", String filename]) =
    (lift . load) filename >>= liftM last . mapM eval
eval (List [Atom "call-with-current-continuation", proc]) =
    callCC $ \cont -> do
        f <- eval proc
        applyProc [f, Continuation cont]
eval (List (function : args)) = do
    func <- eval function
    argVals <- mapM eval args
    apply func argVals
eval badForm = lift $ throwError $ BadSpecialForm "Unrecognized special form" badForm

chainEvalClause :: (LispVal -> LispEval) -> LispEval -> LispVal -> LispEval
chainEvalClause evalFunc evalC unevalC = do
                        clause <- evalC
                        case clause of
                            WrongClause -> evalFunc unevalC
                            _ -> return clause

evalCaseClause :: LispVal -> LispVal -> LispEval
evalCaseClause key (List (datums:(exprs@(_:_)))) = --exprs can't be []
    case datums of
        Atom "else" -> do
                evalExprs <- mapM eval exprs
                return $ last evalExprs
        List l -> do success <- lift . liftThrows . liftM or $ mapM (\x -> eqv [x, key] >>= unpackBool) l
                     if success
                        then do
                            evalExprs <- mapM eval exprs
                            return $ last evalExprs
                        else return WrongClause
        e -> lift . throwError $ TypeMismatch "list" e
evalCaseClause _ badForm = lift . throwError $ BadSpecialForm "Unrecognized case clause form" badForm

evalCondClause :: LispVal -> LispEval
evalCondClause (List (test:[])) =
    do success <- eval test
       case success of
            Bool True -> return success
            Bool False -> return WrongClause
            _ -> lift . throwError $ TypeMismatch "boolean" success
evalCondClause (List (test:exprs)) =
    case test of
        Atom "else" -> do
            evalExprs <- mapM eval exprs
            return $ last evalExprs
        _ -> do success <- eval test
                case success of
                    Bool True -> do
                        evalExprs <- mapM eval exprs
                        return $ last evalExprs
                    Bool False -> return WrongClause
                    _ -> lift . throwError $ TypeMismatch "boolean" success
evalCondClause badForm = lift . throwError $ BadSpecialForm "Unrecognized cond clause form" badForm

apply :: LispVal -> [LispVal] -> LispEval
apply (Continuation c) [arg] = c arg
apply (Continuation _) e = lift . throwError $ NumArgs 1 e
apply (IOFunc func) args = lift $ func args
apply (PrimitiveFunc func) args = lift . liftThrows $ func args
apply (Func ps vs b c) args = do
    originalEnv <- getEnv
    if num ps /= num args && isNothing vs
       then lift . throwError $ NumArgs (num ps) args
       else do
        -- this is not a nice way to handle a stack frame, just splatting over
        -- the top
        bindVars $ envToList c
        bindVars $ zip ps args
        bindVarArgs vs
        r <- evalBody
        -- and then manually unwinding it
        putEnv originalEnv
        return r
    where remainingArgs = drop (length ps) args
          num = toInteger . length
          evalBody = liftM last $ mapM eval b
          bindVarArgs arg = case arg of
              Just argName -> bindVars [(argName, List remainingArgs)]
              Nothing -> return ()
apply e _ = lift . throwError $ TypeMismatch "function" e

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs p b = do
    env <- getEnv
    return $ Func (map show p) varargs b env

makeNormalFunc :: [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . show

applyProc :: [LispVal] -> LispEval
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc e = lift . throwError $ NumArgs 2 e
