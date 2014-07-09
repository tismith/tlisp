module Eval where
import Primitives
import LispVals
import LispEnvironment

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Control.Monad.Error (throwError)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Complex _) = return val
eval _ val@(Ratio _) = return val
eval _ val@(Float _) = return val
eval _ val@(Char _) = return val
eval _ val@(Vector _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval _ e@(List [Atom "quasiquote", val]) = throwError $ BadSpecialForm "Quasiquotes not implemented" e
eval _ e@(List [Atom "unquote", val]) = throwError $ BadSpecialForm "Unquotes not implemented" e
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "if", pred, conseq]) =
    do result <- eval env pred
       case result of
         Bool False -> throwError $ Unspecified "if without an else"
         otherwise -> eval env conseq
eval env (List ((Atom "cond"):(c:cs))) =
    do result <- foldl (chainEvalClause (evalCondClause env)) (evalCondClause env c) cs
       case result of
            Just l -> return l
            Nothing -> throwError $ Unspecified "no matching cond"
eval env (List ((Atom "case"):key:c:cs)) =
    do evalKey <- eval env key
       result <- foldl (chainEvalClause (evalCaseClause env evalKey)) (evalCaseClause env evalKey c) cs
       case result of
            Just l -> return l
            Nothing -> throwError $ Unspecified "no matching case"
eval env (List ([Atom "set!", Atom var, form])) = eval env form >>= setVar env var
eval env (List ([Atom "string-set!", Atom var, i, c])) =
    do i' <- eval env i
       index <- liftThrows $ unpackNum i'
       c' <- eval env c
       char <- liftThrows $ unpackChar c'
       v <- getVar env var
       str <- liftThrows $ unpackStr $ v
       let (f,s) = splitAt (fromIntegral index) str
       case (s) of
            [] -> throwError $ Unspecified "invalid index"
            _ -> setVar env var (String $ f ++ [char] ++ (drop 1 s))
eval env (List ([Atom "string-fill!", Atom var, c])) =
    do c' <- eval env c
       char <- liftThrows $ unpackChar c'
       v <- getVar env var
       str <- liftThrows $ unpackStr $ v
       setVar env var (String $ (map (const char) str))
eval env (List ([Atom "define", Atom var, form])) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (Atom "apply" : args)) = do
    argVals <- mapM (eval env) args
    applyProc argVals
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

chainEvalClause :: (LispVal -> IOThrowsError (Maybe LispVal)) -> IOThrowsError (Maybe LispVal) -> LispVal -> IOThrowsError (Maybe LispVal)
chainEvalClause evalFunc evalC unevalC = do
                        clause <- evalC
                        case clause of
                            Just l -> return clause
                            Nothing -> evalFunc unevalC

evalCaseClause :: Env -> LispVal -> LispVal -> IOThrowsError (Maybe LispVal)
evalCaseClause env key c@(List (datums:(exprs@(_:_)))) = --exprs can't be []
    case datums of
        Atom "else" -> do
                evalExprs <- mapM (eval env) exprs
                return $ Just $ last evalExprs
        List l -> do success <- liftThrows . liftM or $ mapM (\x -> (eqv [x, key]) >>= unpackBool) l
                     if success
                        then do
                            evalExprs <- mapM (eval env) exprs
                            return $ Just $ last evalExprs
                        else return Nothing
evalCaseClause _ _ badForm = throwError $ BadSpecialForm "Unrecognized case clause form" badForm

evalCondClause :: Env -> LispVal -> IOThrowsError (Maybe LispVal)
evalCondClause env (List (test:[])) =
    do success <- eval env test
       case success of
            Bool True -> return $ Just success
            Bool False -> return Nothing
            _ -> throwError $ TypeMismatch "boolean" success
evalCondClause env (List (test:exprs)) =
    case test of
        Atom "else" -> do
            evalExprs <- mapM (eval env) exprs
            return $ Just $ last evalExprs
        _ -> do success <- eval env test
                case success of
                    Bool True -> do
                        evalExprs <- mapM (eval env) exprs
                        return $ Just $ last evalExprs
                    Bool False -> return Nothing
                    _ -> throwError $ TypeMismatch "boolean" success
evalCondClause _ badForm = throwError $ BadSpecialForm "Unrecognized cond clause form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . showVal

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

