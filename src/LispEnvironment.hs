{-# LANGUAGE FlexibleContexts #-}
module LispEnvironment (
    envSymbols,
    envFromList,
    envToList,
    liftThrows,
    liftEnvThrows,
    runIOThrows,
    runEval,
    getEnv,
    putEnv,
    getVar,
    defineVar,
    setVar,
    bindVars,
    newFrame,
    dropFrame
  ) where

import LispVals

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (get, put, runState, runStateT, MonadState)
import Control.Monad.Error (ErrorT, runErrorT, throwError, MonadError, catchError)
import Control.Monad.Cont (runContT)
import qualified Data.Map as M (lookup, insert, fromList, union, toList, keys, unions, singleton, empty)

newFrame :: Env -> Env
newFrame envs = M.empty : envs

dropFrame :: Env -> Env
dropFrame = drop 1

envSymbols :: Env -> [String]
envSymbols = M.keys . M.unions

envFromList :: [(String, LispVal)] -> Env
envFromList l = [M.fromList l]

envToList :: Env -> [(String, LispVal)]
envToList = M.toList . M.unions

liftEnvThrows :: (MonadError LispError m, MonadState Env m) => EnvThrowsError a -> m a
liftEnvThrows action = do
    env <- get
    let (a, s) = runState (runErrorT action) env
    put s
    case a of
        Left err -> throwError err
        Right val -> return val

liftThrows :: (MonadError LispError m) => ThrowsError a -> m a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> Env -> IO (String, Env)
runIOThrows action env = do
        (a, s) <- runStateT (runErrorT (trapError action)) env
        return (extractValue a, s)
    where trapError act = catchError act (return . show)

runEval :: LispEval -> IOThrowsError LispVal
runEval e = runContT e return

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = undefined

getEnv :: MonadState Env m => m Env
getEnv = get

putEnv :: MonadState Env m => Env -> m ()
putEnv = put

getVar :: (MonadState Env m, MonadError LispError m) => String -> m LispVal
getVar var = get >>= maybe (throwError $ UnboundVar "Getting an unbound variable" var) return . M.lookup var . M.unions

setVar :: (MonadState Env m, MonadError LispError m) => String -> LispVal -> m LispVal
setVar var value = do
    envs <- get
    case setVar' envs of
        Nothing -> throwError $ UnboundVar "Setting an unbound variable" var
        Just e -> put e
    return Void
    where setVar' envs = case envs of
                        [] -> Nothing
                        (e:es) -> maybe ((:) <$> Just e <*> setVar' es)
                                        (const $ Just (M.insert var value e:es))
                                        (M.lookup var e)

defineVar :: (MonadState Env m) => String -> LispVal -> m LispVal
defineVar var value = do
    envs <- get
    case envs of
        [] -> put [M.singleton var value]
        (e:es) -> put (M.insert var value e:es)
    return value

bindVars :: (MonadState Env m) => [(String, LispVal)] -> m ()
bindVars bindings = do
    envs <- get
    case envs of
        [] -> put $ envFromList bindings
        (e:es) -> put ((M.fromList bindings `M.union` e):es)
    return ()

