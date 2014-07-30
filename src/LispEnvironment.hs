{-# LANGUAGE FlexibleContexts #-}
module LispEnvironment (
    envSymbols,
    frameFromList,
    initEnv,
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
    addNewFrame,
    dropFrame
  ) where

import LispVals

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State (get, put, runState, runStateT, MonadState)
import Control.Monad.Error (ErrorT, runErrorT, throwError, MonadError, catchError)
import Control.Monad.Cont (runContT)
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as M (lookup, insert, fromList, union, toList, keys, unions, singleton, empty)

addNewFrame :: Env -> IO Env
addNewFrame envs = do
    newFrame <- newIORef M.empty
    return $ newFrame:envs

dropFrame :: Env -> Env
dropFrame = drop 1

envSymbols :: Env -> IO [String]
envSymbols = liftM (M.keys . M.unions) . mapM readIORef

frameFromList :: [(String, LispVal)] -> Frame
frameFromList = M.fromList

initEnv :: Frame -> IO Env
initEnv frame = do
    ioFrame <- newIORef frame
    return [ioFrame]

envToList :: Env -> IO [(String, LispVal)]
envToList = liftM (M.toList . M.unions) . mapM readIORef

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

getVar :: (MonadState Env m, MonadIO m, MonadError LispError m) => String -> m LispVal
getVar var = do
    envs <- get
    maybeVar <- liftM (M.lookup var . M.unions) . liftIO . mapM readIORef $ envs
    maybe (throwError $ UnboundVar "Getting an unbound variable" var) return
        maybeVar

setVar :: (MonadState Env m, MonadIO m, MonadError LispError m) => String -> LispVal -> m LispVal
setVar var value = do
    envs <- get
    setVar' var value envs
    return Void

setVar' :: (MonadState Env m, MonadIO m, MonadError LispError m) => String -> LispVal -> Env -> m ()
setVar' var _ [] = throwError $ UnboundVar "Setting an unbown variable" var
setVar' var value (e:es) = do
    frame <- liftIO $ readIORef e
    case M.lookup var frame of
        Nothing -> setVar' var value es
        Just _ -> liftIO $ modifyIORef' e (M.insert var value)

defineVar :: (MonadState Env m, MonadIO m) => String -> LispVal -> m LispVal
defineVar var value = do
    envs <- get
    case envs of
        [] -> liftIO (newIORef (M.singleton var value)) >>= put . (:[])
        (e:_) -> liftIO $ modifyIORef' e (M.insert var value)
    return value

bindVars :: (MonadState Env m, MonadIO m) => [(String, LispVal)] -> m ()
bindVars bindings = do
    envs <- get
    case envs of
        [] -> liftIO (newIORef $ frameFromList bindings) >>= put . (:[])
        (e:_) -> liftIO $ modifyIORef' e (M.union (M.fromList bindings))
    return ()

