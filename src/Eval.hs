module Eval where
import Primitives (primitives, unpackBool, eqv)
import LispVals

import Control.Monad (liftM)
import Control.Monad.Error (throwError)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Complex _) = return val
eval val@(Ratio _) = return val
eval val@(Float _) = return val
eval val@(Char _) = return val
eval val@(Vector _) = return val
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
    do result <- foldl (chainEvalClause (evalCondClause)) (evalCondClause c) cs
       case result of
            Just l -> return l
            Nothing -> throwError $ Unspecified "no matching cond"
eval (List ((Atom "case"):key:c:cs)) =
    do evalKey <- eval key
       result <- foldl (chainEvalClause (evalCaseClause evalKey)) (evalCaseClause evalKey c) cs
       case result of
            Just l -> return l
            Nothing -> throwError $ Unspecified "no matching case"
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

chainEvalClause :: (LispVal -> ThrowsError (Maybe LispVal)) -> ThrowsError (Maybe LispVal) -> LispVal -> ThrowsError (Maybe LispVal)
chainEvalClause evalFunc evalC unevalC = do
                        clause <- evalC
                        case clause of
                            Just l -> return clause
                            Nothing -> evalFunc unevalC

evalCaseClause :: LispVal -> LispVal -> ThrowsError (Maybe LispVal)
evalCaseClause key c@(List (datums:(exprs@(_:_)))) = --exprs can't be []
    case datums of
        Atom "else" -> do
                evalExprs <- mapM eval exprs
                return $ Just $ last evalExprs
        List l -> do success <- liftM or $ mapM (\x -> (eqv [x, key]) >>= unpackBool) l
                     if success
                        then do
                            evalExprs <- mapM eval exprs
                            return $ Just $ last evalExprs
                        else return Nothing
evalCaseClause _ badForm = throwError $ BadSpecialForm "Unrecognized case clause form" badForm

evalCondClause :: LispVal -> ThrowsError (Maybe LispVal)
evalCondClause (List (test:[])) =
    do success <- eval test
       case success of
            Bool True -> return $ Just success
            Bool False -> return Nothing
            _ -> throwError $ TypeMismatch "boolean" success
evalCondClause (List (test:exprs)) =
    case test of
        Atom "else" -> do
            evalExprs <- mapM eval exprs
            return $ Just $ last evalExprs
        _ -> do success <- eval test
                case success of
                    Bool True -> do
                        evalExprs <- mapM eval exprs
                        return $ Just $ last evalExprs
                    Bool False -> return Nothing
                    _ -> throwError $ TypeMismatch "boolean" success
evalCondClause badForm = throwError $ BadSpecialForm "Unrecognized cond clause form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

