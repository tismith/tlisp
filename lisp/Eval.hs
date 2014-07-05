{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Eval where
import Parse
import Text.ParserCombinators.Parsec
import System.IO hiding (try)
import Control.Monad
import Control.Monad.Error
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex
import Data.Maybe
import qualified Data.Vector as V

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Complex _) = return val
eval val@(Ratio _) = return val
eval val@(Float _) = return val
eval val@(Char _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numBinOp (+)),
              ("-", numBinOp (-)),
              ("*", numBinOp (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

--Begin magic to handle type promotion around my numerical types
class Num a => NumToLispVal a where
    toLispNum :: a -> LispVal
instance NumToLispVal (Integer) where
    toLispNum = Number
instance NumToLispVal (Float) where
    toLispNum = Float
--needs flexible instances here to write an instances for Complex Float instead of just Complex a
instance NumToLispVal (Complex Float) where
    toLispNum = Complex
instance NumToLispVal (Ratio Integer) where
    toLispNum = Ratio

--needs existential quantification for this forall
data NumUnpacker = forall a. (NumToLispVal a, Num a) => AnyNumUnpacker (LispVal -> ThrowsError a)

--needs rank2types for this forall
unpackBinNumOp :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> NumUnpacker -> ThrowsError (Maybe LispVal)
unpackBinNumOp f arg1 arg2 (AnyNumUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ Just . toLispNum $ f unpacked1 unpacked2
        `catchError` (const $ return Nothing)

--this defines the type promotion order.
numUnpackers :: [NumUnpacker]
numUnpackers = [AnyNumUnpacker unpackNum, AnyNumUnpacker unpackRatio, AnyNumUnpacker unpackFloat,
                AnyNumUnpacker unpackComplex]

--we try to interpret each
--argument using the unpack* function. Some types will ThrowError when they can't be cast
--we catch that error in unpackBinNumOp and return Nothing in that case. We then pick
--the first unpacker that works.
--
--needs to be a rank2type (the forall a. ...) since that first argument is not specialised
--at the call site to numBinOp. i.e. if numBinOp was
--      forall a. (a->a->a) -> [... (the default
--then at the callsite to numBinOp ghc would pick a type for that argument, whereas, we need
--unpackBinNumOp to do that for us.
--
--Can see this with ghci -- :type (+) returns
--(+) :: Num a => a -> a -> a
--which is actually:
--forall a. Num a => a -> a -> a
--so if we want to pass in the fully polymorphic (+), we need to take it's full type signature
numBinOp :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numBinOp f [arg1, arg2] = do
    primitive <- liftM (firstJust) $ mapM (unpackBinNumOp f arg1 arg2) numUnpackers
    case primitive of
        Nothing -> throwError $ TypeMismatch "number" arg1
        Just a -> return $ a
numBinOp _ badArgList = throwError $ NumArgs 2 badArgList

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\x y -> case x of
                    Just a -> x
                    Nothing -> y) Nothing

--end magic section, but don't forget the varied unpack* routines below

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackComplex :: LispVal -> ThrowsError (Complex Float)
unpackComplex (Complex n) = return n
unpackComplex (Number n) = return ((fromIntegral n) :+ 0)
unpackComplex (Float n) = return (n :+ 0)
unpackComplex (Ratio n) = return ((fromRational n) :+ 0)
unpackComplex (List [n]) = unpackComplex n
unpackComplex notNum = throwError $ TypeMismatch "number" notNum

unpackRatio :: LispVal -> ThrowsError (Ratio Integer)
unpackRatio (Ratio n) = return n
unpackRatio (Number n) = return (n % 1)
unpackRatio (List [n]) = unpackRatio n
unpackRatio notNum = throwError $ TypeMismatch "number" notNum

unpackFloat :: LispVal -> ThrowsError Float
unpackFloat (Float n) = return n
unpackFloat (Ratio n) = return (fromRational n)
unpackFloat (Number n) = return (fromIntegral n)
unpackFloat (List [n]) = unpackFloat n
unpackFloat notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList _ x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data EqUnpacker = forall a. Eq a => AnyEqUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> EqUnpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyEqUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyEqUnpacker unpackNum, AnyEqUnpacker unpackStr, AnyEqUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
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

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
