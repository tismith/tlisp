{-# LANGUAGE FlexibleContexts #-}
module Eval where
import Primitives
import LispVals
import LispEnvironment

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)

eval :: LispVal -> IOThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Complex _) = return val
eval val@(Ratio _) = return val
eval val@(Float _) = return val
eval val@(Char _) = return val
eval val@(Vector _) = return val
eval (Atom id) = getVar id
eval (List [Atom "quote", val]) = return val
eval e@(List [Atom "quasiquote", val]) = throwError $ BadSpecialForm "Quasiquotes not implemented" e
eval e@(List [Atom "unquote", val]) = throwError $ BadSpecialForm "Unquotes not implemented" e
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
eval (List [Atom "if", pred, conseq]) =
    do result <- eval pred
       case result of
         Bool False -> throwError $ Unspecified "if without an else"
         otherwise -> eval conseq
eval (List ((Atom "cond"):(c:cs))) =
    do result <- foldl (chainEvalClause evalCondClause) (evalCondClause c) cs
       case result of
            Just l -> return l
            Nothing -> throwError $ Unspecified "no matching cond"
eval (List ((Atom "case"):key:c:cs)) =
    do evalKey <- eval key
       result <- foldl (chainEvalClause (evalCaseClause evalKey)) (evalCaseClause evalKey c) cs
       case result of
            Just l -> return l
            Nothing -> throwError $ Unspecified "no matching case"
eval (List ([Atom "set!", Atom var, form])) = eval form >>= setVar var
eval (List ([Atom "string-set!", Atom var, i, c])) =
    do i' <- eval i
       index <- liftThrows $ unpackNum i'
       c' <- eval c
       char <- liftThrows $ unpackChar c'
       v <- getVar var
       str <- liftThrows $ unpackStr $ v
       let (f,s) = splitAt (fromIntegral index) str
       case (s) of
            [] -> throwError $ Unspecified "invalid index"
            _ -> setVar var (String $ f ++ [char] ++ (drop 1 s))
eval (List ([Atom "string-fill!", Atom var, c])) =
    do c' <- eval c
       char <- liftThrows $ unpackChar c'
       v <- getVar var
       str <- liftThrows $ unpackStr $ v
       setVar var (String $ (map (const char) str))
eval (List ([Atom "define", Atom var, form])) = eval form >>= defineVar var
eval (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc params body >>= defineVar var
eval (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs params body >>= defineVar var
eval (List (Atom "lambda" : List params : body)) =
    makeNormalFunc params body
eval (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs params body
eval (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs [] body
eval (List (Atom "apply" : args)) = do
    argVals <- mapM (eval) args
    applyProc argVals
eval (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval)
eval (List (function : args)) = do
    func <- eval function
    argVals <- mapM (eval) args
    apply func argVals
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

chainEvalClause :: (LispVal -> IOThrowsError (Maybe LispVal)) -> IOThrowsError (Maybe LispVal) -> LispVal -> IOThrowsError (Maybe LispVal)
chainEvalClause evalFunc evalC unevalC = do
                        clause <- evalC
                        case clause of
                            Just l -> return clause
                            Nothing -> evalFunc unevalC

evalCaseClause :: LispVal -> LispVal -> IOThrowsError (Maybe LispVal)
evalCaseClause key c@(List (datums:(exprs@(_:_)))) = --exprs can't be []
    case datums of
        Atom "else" -> do
                evalExprs <- mapM (eval) exprs
                return $ Just $ last evalExprs
        List l -> do success <- liftThrows . liftM or $ mapM (\x -> (eqv [x, key]) >>= unpackBool) l
                     if success
                        then do
                            evalExprs <- mapM (eval) exprs
                            return $ Just $ last evalExprs
                        else return Nothing
evalCaseClause _ badForm = throwError $ BadSpecialForm "Unrecognized case clause form" badForm

evalCondClause :: LispVal -> IOThrowsError (Maybe LispVal)
evalCondClause (List (test:[])) =
    do success <- eval test
       case success of
            Bool True -> return $ Just success
            Bool False -> return Nothing
            _ -> throwError $ TypeMismatch "boolean" success
evalCondClause (List (test:exprs)) =
    case test of
        Atom "else" -> do
            evalExprs <- mapM (eval) exprs
            return $ Just $ last evalExprs
        _ -> do success <- eval test
                case success of
                    Bool True -> do
                        evalExprs <- mapM (eval) exprs
                        return $ Just $ last evalExprs
                    Bool False -> return Nothing
                    _ -> throwError $ TypeMismatch "boolean" success
evalCondClause badForm = throwError $ BadSpecialForm "Unrecognized cond clause form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = do
    originalEnv <- getEnv
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else do
        -- this is not a nice way to handle a stack frame, just splatting over
        -- the top
        bindVars $ envToList closure
        bindVars $ zip params args
        bindVarArgs varargs
        r <- evalBody
        -- and then manually unwinding it
        putEnv originalEnv
        return r
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody = liftM last $ mapM (eval) body
          bindVarArgs arg = case arg of
              Just argName -> bindVars [(argName, List $ remainingArgs)]
              Nothing -> return ()
apply e _ = throwError $ TypeMismatch "function" e

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs params body = do
    env <- getEnv
    return $ Func (map showVal params) varargs body env

makeNormalFunc :: [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . showVal

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

