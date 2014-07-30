{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Primitives (
    primitiveBindings
  ) where
import LispVals
import LispEnvironment
import Parse
import Eval

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad (liftM, foldM, zipWithM)
import Control.Monad.Error (throwError, catchError, MonadError)
import Control.Monad.Cont (MonadCont, callCC)
import Data.Ratio (Ratio, (%))
import Data.Complex (Complex((:+)))
import Data.Char (toLower)
import System.IO (IOMode(ReadMode, WriteMode), hPrint, hClose, openFile, stdin, stdout)
import Data.Function (on)

primitiveBindings :: Frame
primitiveBindings = frameFromList (
        map (makeF IOFunc) ioPrimitives ++
        map (makeF PrimitiveFunc) primitives ++
        map (makeF SpecialFormFunc) specialFormPrimitives)
    where makeF constructor (var, func) = (var, constructor func)

specialFormPrimitives :: [(String, [LispVal] -> LispEval)]
specialFormPrimitives = [("if", ifProc),
                         ("case", caseProc),
                         ("cond", condProc),
                         ("set!", setProc),
                         ("string-set!", stringSetProc),
                         ("string-fill!", stringFillProc),
                         ("define", defineProc),
                         ("call-with-current-continuation", callCCProc),
                         ("lambda", lambdaProc),
                         ("load", loadProc),
                         ("apply", applyProc)]

ioPrimitives :: (MonadError LispError m, MonadIO m) => [(String, [LispVal] -> m LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitives :: (MonadError LispError m) => [(String, [LispVal] -> m LispVal)]
primitives = [("+", anyNumListOp (+)),
              ("-", anyNumListMinus),
              ("*", anyNumListOp (*)),
              ("/", anyNumListDiv),
              ("mod", onlyNumListOp mod),
              ("quotient", onlyNumListOp quot),
              ("remainder", onlyNumListOp rem),
              ("=", anyEqBoolListOp (==)),
              ("<", anyOrdBoolListOp (<)),
              (">", anyOrdBoolListOp (>)),
              ("/=", anyEqBoolListOp (/=)),
              (">=", anyOrdBoolListOp (>=)),
              ("<=", anyOrdBoolListOp (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string-ci=?", strBoolBinop (on (==) (map toLower))),
              ("string<=?", strBoolBinop (<=)),
              ("string-ci<=?", strBoolBinop (on (<=) (map toLower))),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci>=?", strBoolBinop (on (>=) (map toLower))),
              ("string<?", strBoolBinop (<)),
              ("string-ci<?", strBoolBinop (on (<) (map toLower))),
              ("string>?", strBoolBinop (>)),
              ("string-ci>?", strBoolBinop (on (>) (map toLower))),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("string?", isLispValTest isLispValString),
              ("exact?", isLispValTest isLispValExact),
              ("inexact?", isLispValTest (not . isLispValExact)),
              ("boolean?", isLispValTest isLispValBool),
              ("number?", isLispValTest isLispValNum),
              ("complex?", isLispValTest isLispValComplex),
              ("real?", isLispValTest isLispValReal),
              ("rational?", isLispValTest isLispValRational),
              ("integer?", isLispValTest isLispValInteger),
              ("vector?", isLispValTest isLispValVector),
              ("char?", isLispValTest isLispValChar),
              ("port?", isLispValTest isLispValPort),
              ("procedure?", isLispValTest isLispValProcedure),
              ("pair?", isLispValTest isLispValDottedList),
              ("symbol?", isLispValTest isLispValAtom),
              ("list?", isLispValTest isLispValList),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("make-string", makeString),
              ("string", charsToString),
              ("string-ref", stringRef),
              ("substring", subString),
              ("string-append", stringAppend),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("string-copy", stringCopy)]

stringCopy :: (MonadError LispError m) => [LispVal] -> m LispVal
stringCopy (s:[]) = do
    rawString <- unpackStr s
    return $ String rawString
stringCopy e = throwError $ NumArgs 1 e

listToString :: (MonadError LispError m) => [LispVal] -> m LispVal
listToString (l:[]) = do
    cs <- unpackList l
    rawChars <- mapM unpackChar cs
    return $ String rawChars
listToString e = throwError $ NumArgs 1 e

stringToList :: (MonadError LispError m) => [LispVal] -> m LispVal
stringToList (s:[]) = do
    rawString <- unpackStr s
    return $ List $ map Char rawString
stringToList e = throwError $ NumArgs 1 e

stringAppend :: (MonadError LispError m) => [LispVal] -> m LispVal
stringAppend ss@(_:_) = do
    rawStrings <- mapM unpackStr ss
    return $ String $ concat rawStrings
stringAppend e = throwError $ NumArgs 1 e

subString :: (MonadError LispError m) => [LispVal] -> m LispVal
subString (s:b:e:[]) = do
    rawString <- unpackStr s
    rawBegin <- unpackNum b
    rawEnd <- unpackNum e
    return $ String (take (fromIntegral $ rawEnd - rawBegin) (drop (fromIntegral rawBegin) rawString))
subString e = throwError $ NumArgs 3 e

stringRef :: (MonadError LispError m) => [LispVal] -> m LispVal
stringRef (s:k:[]) = do
    rawString <- unpackStr s
    rawIndex <- unpackNum k
    return $ Char (rawString !! fromIntegral rawIndex)
stringRef e = throwError $ NumArgs 2 e

makeString :: (MonadError LispError m) => [LispVal] -> m LispVal
makeString (k:[]) = do
    size <- unpackNum k
    return $ String (replicate (fromIntegral size) '.')
makeString (k:c:[]) = do
    size <- unpackNum k
    character <- unpackChar c
    return $ String (replicate (fromIntegral size) character)
makeString e = throwError $ NumArgs 2 e

charsToString :: (MonadError LispError m) => [LispVal] -> m LispVal
charsToString [] = throwError $ NumArgs 1 []
charsToString chars = do
    newString <- mapM unpackChar chars
    return $ String newString

isLispValExact :: LispVal -> Bool
isLispValExact (Number _) = True
isLispValExact (Ratio _) = True
isLispValExact _ = False

stringToSymbol :: (MonadError LispError m) => [LispVal] -> m LispVal
stringToSymbol (String s:[]) = return $ Atom s
stringToSymbol (e:[]) = throwError $ TypeMismatch "string" e
stringToSymbol e = throwError $ NumArgs 1 e

symbolToString :: (MonadError LispError m) => [LispVal] -> m LispVal
symbolToString (Atom s:[]) = return $ String s
symbolToString (e:[]) = throwError $ TypeMismatch "symbol" e
symbolToString e = throwError $ NumArgs 1 e

isLispValDottedList :: LispVal -> Bool
isLispValDottedList (DottedList _ _) = True
isLispValDottedList _ = False

isLispValProcedure :: LispVal -> Bool
isLispValProcedure (PrimitiveFunc _) = True
isLispValProcedure (IOFunc _) = True
isLispValProcedure (Func {})  = True
isLispValProcedure _ = False

isLispValPort :: LispVal -> Bool
isLispValPort (Port _) = True
isLispValPort _ = False

isLispValList :: LispVal -> Bool
isLispValList (List _) = True
isLispValList _ = False

isLispValAtom :: LispVal -> Bool
isLispValAtom (Atom _) = True
isLispValAtom _ = False

isLispValVector :: LispVal -> Bool
isLispValVector (Vector _) = True
isLispValVector _ = False

isLispValChar :: LispVal -> Bool
isLispValChar (Char _) = True
isLispValChar _ = False

isLispValInteger :: LispVal -> Bool
isLispValInteger (Number _) = True
isLispValInteger _ = False

isLispValRational :: LispVal -> Bool
isLispValRational (Number _) = True
isLispValRational (Ratio _) = True
isLispValRational _ = False

isLispValReal :: LispVal -> Bool
isLispValReal (Number _) = True
isLispValReal (Float _) = True
isLispValReal (Ratio _) = True
isLispValReal _ = False

isLispValComplex :: LispVal -> Bool
isLispValComplex (Number _) = True
isLispValComplex (Float _) = True
isLispValComplex (Ratio _) = True
isLispValComplex (Complex _) = True
isLispValComplex _ = False

isLispValNum :: LispVal -> Bool
isLispValNum (Number _) = True
isLispValNum (Float _) = True
isLispValNum (Ratio _) = True
isLispValNum (Complex _) = True
isLispValNum _ = False

isLispValBool :: LispVal -> Bool
isLispValBool (Bool _) = True
isLispValBool _ = False

isLispValString :: LispVal -> Bool
isLispValString (String _) = True
isLispValString _ = False

isLispValTest :: (MonadError LispError m) => (LispVal -> Bool) -> [LispVal] -> m LispVal
isLispValTest f (l:[]) = return $ Bool $ f l
isLispValTest _ e = throwError $ NumArgs 1 e

onlyNumListOp :: (MonadError LispError m) => (forall a. Integral a => a -> a -> a) -> [LispVal] -> m LispVal
onlyNumListOp f (l:ls@(_:_)) = foldM (onlyNumBinOp f) l ls
onlyNumListOp _ badArgList = throwError $ NumArgs 2 badArgList

onlyNumBinOp :: (MonadError LispError m) => (forall a. Integral a => a -> a -> a) -> LispVal -> LispVal -> m LispVal
onlyNumBinOp f (Number a) (Number b) = return $ Number (f a b)
onlyNumBinOp _ (Number _) e = throwError $ TypeMismatch "integral" e
onlyNumBinOp _ e _ = throwError $ TypeMismatch "integral" e

anyNumListDiv :: (MonadError LispError m) => [LispVal] -> m LispVal
anyNumListDiv (l:ls@(_:_)) = foldM anyNumBinDiv l ls
anyNumListDiv (l:[]) = anyNumBinDiv (Float 1.0) l
anyNumListDiv badArgList = throwError $ NumArgs 1 badArgList

anyNumBinDiv :: (MonadError LispError m) => LispVal -> LispVal -> m LispVal
anyNumBinDiv (Number a) (Number b) = return $ Number (div a b)
anyNumBinDiv (Number a) (Float b) = return $ Float (fromIntegral a / b)
anyNumBinDiv (Number a) (Complex b) = return $ Complex ((fromIntegral a :+ 0) / b)
anyNumBinDiv (Number a) (Ratio b) = return $ Ratio ((a % 1) / b)
anyNumBinDiv (Float a) (Number b) = return $ Float (a / fromIntegral b)
anyNumBinDiv (Float a) (Float b) = return $ Float (a / b)
anyNumBinDiv (Float a) (Complex b) = return $ Complex ((a :+ 0) / b)
anyNumBinDiv (Float a) (Ratio b) = return $ Float (a / fromRational b)
anyNumBinDiv (Complex a) (Number b) = return $ Complex (a / (fromIntegral b :+ 0))
anyNumBinDiv (Complex a) (Float b) = return $ Complex (a / (b :+ 0))
anyNumBinDiv (Complex a) (Complex b) = return $ Complex (a / b)
anyNumBinDiv (Complex a) (Ratio b) = return $ Complex (a / (fromRational b :+ 0))
anyNumBinDiv (Ratio a) (Number b) = return $ Ratio (a / (b % 1))
anyNumBinDiv (Ratio a) (Float b) = return $ Float (fromRational a / b)
anyNumBinDiv (Ratio a) (Complex b) = return $ Complex (fromRational a / b)
anyNumBinDiv (Ratio a) (Ratio b) = return $ Ratio (a / b)
anyNumBinDiv e _ = throwError $ TypeMismatch "number" e

anyNumListOp :: (MonadError LispError m) => (forall a. Num a => a -> a -> a) -> [LispVal] -> m LispVal
anyNumListOp f (l:ls@(_:_)) = foldM (anyNumBinOp f) l ls
anyNumListOp _ badArgList = throwError $ NumArgs 2 badArgList

anyNumListMinus :: (MonadError LispError m) => [LispVal] -> m LispVal
anyNumListMinus (l:ls@(_:_)) = foldM (anyNumBinOp (-)) l ls
anyNumListMinus (l:[]) = anyNumBinOp (-) (Number 0) l
anyNumListMinus badArgList = throwError $ NumArgs 1 badArgList

anyNumBinOp :: (MonadError LispError m) => (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> m LispVal
anyNumBinOp f (Number a) (Number b) = return $ Number (f a b)
anyNumBinOp f (Number a) (Float b) = return $ Float (f (fromIntegral a) b)
anyNumBinOp f (Number a) (Complex b) = return $ Complex (f (fromIntegral a :+ 0) b)
anyNumBinOp f (Number a) (Ratio b) = return $ Ratio (f (a % 1) b)
anyNumBinOp f (Float a) (Number b) = return $ Float (f a (fromIntegral b))
anyNumBinOp f (Float a) (Float b) = return $ Float (f a b)
anyNumBinOp f (Float a) (Complex b) = return $ Complex (f (a :+ 0) b)
anyNumBinOp f (Float a) (Ratio b) = return $ Float (f a (fromRational b))
anyNumBinOp f (Complex a) (Number b) = return $ Complex (f a (fromIntegral b :+ 0))
anyNumBinOp f (Complex a) (Float b) = return $ Complex (f a (b :+ 0))
anyNumBinOp f (Complex a) (Complex b) = return $ Complex (f a b)
anyNumBinOp f (Complex a) (Ratio b) = return $ Complex (f a (fromRational b :+ 0))
anyNumBinOp f (Ratio a) (Number b) = return $ Ratio (f a (b % 1))
anyNumBinOp f (Ratio a) (Float b) = return $ Float (f (fromRational a) b)
anyNumBinOp f (Ratio a) (Complex b) = return $ Complex (f (fromRational a) b)
anyNumBinOp f (Ratio a) (Ratio b) = return $ Ratio (f a b)
anyNumBinOp _ e _ = throwError $ TypeMismatch "number" e

anyEqBoolListOp :: (MonadError LispError m) => (forall a. Eq a => a -> a -> Bool) -> [LispVal] -> m LispVal
anyEqBoolListOp f ls@(_:_:_) = do
                    sequencedLispBools <- zipWithM (anyEqBoolBinOp f) ls (drop 1 ls)
                    sequencedBools <- mapM unpackBool sequencedLispBools
                    return $ Bool $ and sequencedBools
anyEqBoolListOp _ badArgList = throwError $ NumArgs 2 badArgList

anyEqBoolBinOp :: (MonadError LispError m) => (forall a. Eq a => a -> a -> Bool) -> LispVal -> LispVal -> m LispVal
anyEqBoolBinOp f (Number a) (Number b) = return $ Bool (f a b)
anyEqBoolBinOp f (Number a) (Float b) = return $ Bool (f (fromIntegral a) b)
anyEqBoolBinOp f (Number a) (Complex b) = return $ Bool (f (fromIntegral a :+ 0) b)
anyEqBoolBinOp f (Number a) (Ratio b) = return $ Bool (f (a % 1) b)
anyEqBoolBinOp f (Float a) (Number b) = return $ Bool (f a (fromIntegral b))
anyEqBoolBinOp f (Float a) (Float b) = return $ Bool (f a b)
anyEqBoolBinOp f (Float a) (Complex b) = return $ Bool (f (a :+ 0) b)
anyEqBoolBinOp f (Float a) (Ratio b) = return $ Bool (f a (fromRational b))
anyEqBoolBinOp f (Complex a) (Number b) = return $ Bool (f a (fromIntegral b :+ 0))
anyEqBoolBinOp f (Complex a) (Float b) = return $ Bool (f a (b :+ 0))
anyEqBoolBinOp f (Complex a) (Complex b) = return $ Bool (f a b)
anyEqBoolBinOp f (Complex a) (Ratio b) = return $ Bool (f a (fromRational b :+ 0))
anyEqBoolBinOp f (Ratio a) (Number b) = return $ Bool (f a (b % 1))
anyEqBoolBinOp f (Ratio a) (Float b) = return $ Bool (f (fromRational a) b)
anyEqBoolBinOp f (Ratio a) (Complex b) = return $ Bool (f (fromRational a) b)
anyEqBoolBinOp f (Ratio a) (Ratio b) = return $ Bool (f a b)
anyEqBoolBinOp _ e _ = throwError $ TypeMismatch "number" e

anyOrdBoolListOp :: (MonadError LispError m) => (forall a. Ord a => a -> a -> Bool) -> [LispVal] -> m LispVal
anyOrdBoolListOp f ls@(_:_:_) = do
                    sequencedLispBools <- zipWithM (anyOrdBoolBinOp f) ls (drop 1 ls)
                    sequencedBools <- mapM unpackBool sequencedLispBools
                    return $ Bool $ and sequencedBools
anyOrdBoolListOp _ badArgList = throwError $ NumArgs 2 badArgList

anyOrdBoolBinOp :: (MonadError LispError m) => (forall a. Ord a => a -> a -> Bool) -> LispVal -> LispVal -> m LispVal
anyOrdBoolBinOp f (Number a) (Number b) = return $ Bool (f a b)
anyOrdBoolBinOp f (Number a) (Float b) = return $ Bool (f (fromIntegral a) b)
anyOrdBoolBinOp f (Number a) (Ratio b) = return $ Bool (f (a % 1) b)
anyOrdBoolBinOp f (Float a) (Number b) = return $ Bool (f a (fromIntegral b))
anyOrdBoolBinOp f (Float a) (Float b) = return $ Bool (f a b)
anyOrdBoolBinOp f (Float a) (Ratio b) = return $ Bool (f a (fromRational b))
anyOrdBoolBinOp _ e@(Complex _) _ = throwError $ TypeMismatch "ordered" e
anyOrdBoolBinOp _ _ e@(Complex _) = throwError $ TypeMismatch "ordered" e
anyOrdBoolBinOp f (Ratio a) (Number b) = return $ Bool (f a (b % 1))
anyOrdBoolBinOp f (Ratio a) (Float b) = return $ Bool (f (fromRational a) b)
anyOrdBoolBinOp f (Ratio a) (Ratio b) = return $ Bool (f a b)
anyOrdBoolBinOp _ e _ = throwError $ TypeMismatch "number" e

boolBinop :: (MonadError LispError m) => (LispVal -> m a) -> (a -> a -> Bool) -> [LispVal] -> m LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

strBoolBinop :: (MonadError LispError m) => (String -> String -> Bool) -> [LispVal] -> m LispVal
strBoolBinop = boolBinop unpackStr
boolBoolBinop :: (MonadError LispError m) => (Bool -> Bool -> Bool) -> [LispVal] -> m LispVal
boolBoolBinop = boolBinop unpackBool

unpackChar :: (MonadError LispError m) => LispVal -> m Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "character" notChar

unpackList :: (MonadError LispError m) => LispVal -> m [LispVal]
unpackList (List l) = return l
unpackList notList = throwError $ TypeMismatch "list" notList

unpackNum :: (MonadError LispError m) => LispVal -> m Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackComplex :: (MonadError LispError m) => LispVal -> m (Complex Float)
unpackComplex (Complex n) = return n
unpackComplex (Number n) = return (fromIntegral n :+ 0)
unpackComplex (Float n) = return (n :+ 0)
unpackComplex (Ratio n) = return (fromRational n :+ 0)
unpackComplex (List [n]) = unpackComplex n
unpackComplex notNum = throwError $ TypeMismatch "number" notNum

unpackRatio :: (MonadError LispError m) => LispVal -> m (Ratio Integer)
unpackRatio (Ratio n) = return n
unpackRatio (Number n) = return (n % 1)
unpackRatio (List [n]) = unpackRatio n
unpackRatio notNum = throwError $ TypeMismatch "number" notNum

unpackFloat :: (MonadError LispError m) => LispVal -> m Float
unpackFloat (Float n) = return n
unpackFloat (Ratio n) = return (fromRational n)
unpackFloat (Number n) = return (fromIntegral n)
unpackFloat (List [n]) = unpackFloat n
unpackFloat notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: (MonadError LispError m) => LispVal -> m String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackCoerceStr :: (MonadError LispError m) => LispVal -> m String
unpackCoerceStr (String s) = return s
unpackCoerceStr (Number s) = return $ show s
unpackCoerceStr (Complex s) = return $ show s
unpackCoerceStr (Float s) = return $ show s
unpackCoerceStr (Ratio s) = return $ show s
unpackCoerceStr b@(Bool _) = return $ show b
unpackCoerceStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: (MonadError LispError m) => LispVal -> m Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: (MonadError LispError m) => [LispVal] -> m LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: (MonadError LispError m) => [LispVal] -> m LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList _ x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: (MonadError LispError m) => [LispVal] -> m LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: (MonadError LispError m) => [LispVal] -> m LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [Ratio arg1, Ratio arg2] = return $ Bool $ arg1 == arg2
eqv [Float arg1, Float arg2] = return $ Bool $ arg1 == arg2
eqv [Complex arg1, Complex arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                                    all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left _ -> False
                               Right (Bool val) -> val
                               Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data EqUnpacker m = forall a. Eq a => AnyEqUnpacker (LispVal -> m a)

unpackEquals :: (MonadError LispError m) => LispVal -> LispVal -> EqUnpacker m -> m Bool
unpackEquals arg1 arg2 (AnyEqUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: (MonadError LispError m) => [LispVal] -> m LispVal
equal [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                                    all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case equal [x1, x2] of
                               Right (Bool val) -> val
                               _ -> False
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyEqUnpacker unpackNum, AnyEqUnpacker unpackRatio, AnyEqUnpacker unpackFloat,
                        AnyEqUnpacker unpackComplex, AnyEqUnpacker unpackCoerceStr, AnyEqUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

makePort ::(MonadError LispError m, MonadIO m) =>  IOMode -> [LispVal] -> m LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [e] = throwError $ TypeMismatch "string" e
makePort _ e = throwError $ NumArgs 1 e

closePort :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
readProc [] = readProc [Port stdin]
readProc [Port _] = liftIO getLine >>= readExpr
readProc [e] = throwError $ TypeMismatch "port" e
readProc e = throwError $ NumArgs 1 e

writeProc :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc [_, e] = throwError $ TypeMismatch "port" e
writeProc e = throwError $ NumArgs 2 e

readContents :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [e] = throwError $ TypeMismatch "string" e
readContents e = throwError $ NumArgs 1 e

readAll :: (MonadError LispError m, MonadIO m) => [LispVal] -> m LispVal
readAll [String filename] = liftM List $ load filename
readAll [e] = throwError $ TypeMismatch "string" e
readAll e = throwError $ NumArgs 1 e

load :: (MonadIO m, MonadError LispError m) => String -> m [LispVal]
load filename = liftIO (readFile filename) >>= readExprList

ifProc :: [LispVal] -> LispEval
ifProc [p, conseq, alt] =
    do result <- eval p
       case result of
         Bool False -> eval alt
         _ -> eval conseq
ifProc [p, conseq] =
    do result <- eval p
       case result of
         Bool False -> return Void --if without an else - no value
         _ -> eval conseq
ifProc e = throwError $ NumArgs 2 e

condProc :: [LispVal] -> LispEval
condProc (clause:cs) =
    foldl (chainEvalClause evalCondClause) (evalCondClause clause) cs
        `catchError` (\e -> case e of
            WrongClause -> return Void -- no matching cond - no value
            _ -> throwError e)
condProc e = throwError $ NumArgs 1 e
caseProc :: [LispVal] -> LispEval
caseProc (key:clause:cs) = do
    evalKey <- eval key
    foldl (chainEvalClause (evalCaseClause evalKey)) (evalCaseClause evalKey clause) cs
        `catchError` (\e -> case e of
            WrongClause -> return Void -- no matching case - no value
            _ -> throwError e)
caseProc e = throwError $ NumArgs 2 e

chainEvalClause :: (LispVal -> LispEval) -> LispEval -> LispVal -> LispEval
chainEvalClause evalFunc evalC unevalC =
    evalC `catchError` (\e ->
        case e of
            WrongClause -> evalFunc unevalC
            _ -> throwError e)

evalCaseClause :: LispVal -> LispVal -> LispEval
evalCaseClause key (List (datums:(exprs@(_:_)))) = --exprs can't be []
    case datums of
        Atom "else" -> do
                evalExprs <- mapM eval exprs
                return $ last evalExprs
        List l -> do success <- liftM or $ mapM (\x -> eqv [x, key] >>= unpackBool) l
                     if success
                        then do
                            evalExprs <- mapM eval exprs
                            return $ last evalExprs
                        else throwError WrongClause
        e -> throwError $ TypeMismatch "list" e
evalCaseClause _ badForm = throwError $ BadSpecialForm "Unrecognized case clause form" badForm

evalCondClause :: LispVal -> LispEval
evalCondClause (List (test:[])) =
    do success <- eval test
       case success of
            Bool True -> return success
            Bool False -> throwError WrongClause
            _ -> throwError $ TypeMismatch "boolean" success
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
                    Bool False -> throwError WrongClause
                    _ -> throwError $ TypeMismatch "boolean" success
evalCondClause badForm = throwError $ BadSpecialForm "Unrecognized cond clause form" badForm

setProc :: [LispVal] -> LispEval
setProc [Atom var, form] = eval form >>= setVar var
setProc e = throwError $ NumArgs 2 e

stringSetProc :: [LispVal] -> LispEval
stringSetProc [Atom var, i, ch] =
    do i' <- eval i
       index <- unpackNum i'
       ch' <- eval ch
       char <- unpackChar ch'
       v <- getVar var
       str <- unpackStr v
       let (f,s) = splitAt (fromIntegral index) str
       case s of
            [] -> throwError $ Unspecified "invalid index"
            _ -> setVar var (String $ f ++ [char] ++ drop 1 s)
stringSetProc e = throwError $ NumArgs 3 e

stringFillProc :: [LispVal] -> LispEval
stringFillProc [Atom var, ch] =
    do v <- getVar var
       ch' <- eval ch
       char <- unpackChar ch'
       str <- unpackStr v
       setVar var (String $ map (const char) str)
stringFillProc e = throwError $ NumArgs 2 e

defineProc :: [LispVal] -> LispEval
defineProc [Atom var, form] = eval form >>= defineVar var
defineProc (List (Atom var : p) : b) =
    makeNormalFunc p b >>= defineVar var
defineProc (DottedList (Atom var : p) varargs : b) =
    makeVarargs varargs p b >>= defineVar var
defineProc e = throwError $ NumArgs 2 e

callCCProc :: [LispVal] -> LispEval
callCCProc [proc] =
    callCC $ \cont -> do
        f <- eval proc
        applyProc' [f, Continuation cont]
callCCProc e = throwError $ NumArgs 1 e

lambdaProc :: [LispVal] -> LispEval
lambdaProc (List p : b) = makeNormalFunc p b
lambdaProc (DottedList p varargs : b) =
    makeVarargs varargs p b
lambdaProc (varargs@(Atom _) : b) =
    makeVarargs varargs [] b
lambdaProc e = throwError $ NumArgs 2 e

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> LispEval
makeFunc varargs p b = do
    env <- getEnv
    return $ Func (map show p) varargs b env

makeNormalFunc :: [LispVal] -> [LispVal] -> LispEval
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> [LispVal] -> [LispVal] -> LispEval
makeVarargs = makeFunc . Just . show

loadProc :: [LispVal] -> LispEval
loadProc [String filename] =
    load filename >>= liftM last . mapM eval
loadProc e = throwError $ NumArgs 1 e

applyProc :: [LispVal] -> LispEval
applyProc args = do
    argVals <- mapM eval args
    applyProc' argVals

applyProc' :: [LispVal] -> LispEval
applyProc' [func, List args] = apply func args
applyProc' (func : args) = apply func args
applyProc' e = throwError $ NumArgs 2 e
