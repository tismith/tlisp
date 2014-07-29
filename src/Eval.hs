module Eval (eval, apply) where
import LispVals
import LispEnvironment

import Control.Monad.Trans (lift)
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
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
eval val@(Continuation _) = return val
eval (Atom i) = getVar i
eval (List [Atom "quote", val]) = return val
eval e@(List [Atom "quasiquote", _]) = throwError $ BadSpecialForm "Quasiquotes not implemented" e
eval e@(List [Atom "unquote", _]) = throwError $ BadSpecialForm "Unquotes not implemented" e
eval (List (function : args)) = do
    func <- eval function
    apply func args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> LispEval
apply (SpecialFormFunc f) args = f args
apply (Continuation c) [arg] = eval arg >>= c
apply (Continuation _) e = throwError $ NumArgs 1 e
apply (IOFunc func) args = mapM eval args >>= lift . func
apply (PrimitiveFunc func) args = mapM eval args >>= liftThrows . func
apply (Func ps varargs b c) args = do
    evalArgs <- mapM eval args
    originalEnv <- getEnv
    let remainingArgs = drop (length ps) evalArgs
    if num ps /= num evalArgs && isNothing varargs
       then throwError $ NumArgs (num ps) evalArgs
       else do
        -- this is not a nice way to handle a stack frame, just splatting over
        -- the top
        bindVars $ envToList c
        bindVars $ zip ps evalArgs
        bindVarArgs varargs remainingArgs
        r <- evalBody
        -- and then manually unwinding it
        putEnv originalEnv
        return r
    where num = toInteger . length
          evalBody = liftM last $ mapM eval b
          bindVarArgs arg remArgs = case arg of
              Just argName -> bindVars [(argName, List remArgs)]
              Nothing -> return ()
apply e _ = throwError $ TypeMismatch "function" e

