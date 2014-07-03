{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import System.IO hiding (try)
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex
import Data.Maybe
import qualified Data.Vector as V

main :: IO ()
main = do args <- getArgs
          case length args of
              0 -> runRepl
              1 -> evalAndPrint $ args !! 0
              otherwise -> putStrLn "Program takes only 0 or 1 argument"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
        | List [LispVal]
        | Vector (V.Vector LispVal)
        | DottedList [LispVal] LispVal
        | String String
        | Bool Bool
        | Char Char
        | Number {
            fromNum:: Integer,
            unpackToNum:: LispVal -> ThrowsError Integer,
            unpackToFloat:: LispVal -> ThrowsError Float,
            unpackToComplex:: LispVal -> ThrowsError Complex Float,
            unpackToRatio:: LispVal -> ThrowsError Ratio Integer}
        | Float {
            fromNum:: Float,
            unpackToNum:: LispVal -> ThrowsError Integer,
            unpackToFloat:: LispVal -> ThrowsError Float,
            unpackToComplex:: LispVal -> ThrowsError Complex Float,
            unpackToRatio:: LispVal -> ThrowsError Ratio Integer}
        | Complex {
            fromNum:: Complex Float,
            unpackToNum:: LispVal -> ThrowsError Integer,
            unpackToFloat:: LispVal -> ThrowsError Float,
            unpackToComplex:: LispVal -> ThrowsError Complex Float,
            unpackToRatio:: LispVal -> ThrowsError Ratio Integer}
        | Ratio {
            fromNum:: Ratio Integer,
            unpackToNum:: LispVal -> ThrowsError Integer,
            unpackToFloat:: LispVal -> ThrowsError Float,
            unpackToComplex:: LispVal -> ThrowsError Complex Float,
            unpackToRatio:: LispVal -> ThrowsError Ratio Integer}

makeNumber :: Integer -> LispVal
makeNumber i = Number {
                    fromNum = i
                ,   unpackToNum l = case l of
                                    Number i -> return i
                                    _ -> TypeMismatch "number" l
                ,   unpackToFloat l = case l of
                                    Number i -> return $ fromIntegral i
                                    _ -> TypeMismatch "number" l
                ,   unpackToComplex l = case l of
                                    Number i -> return $ (fromIntegral i) :+ 0
                                    _ -> TypeMismatch "number" l
                ,   unpackToRatio l = case l of
                                    Number i -> return $ i % 1
                                    _ -> TypeMismatch "number" l
                }

makeFloat i = Float {
                    fromNum = i
                ,   unpackToNum = TypeMismatch "number"
                ,   unpackToFloat l = case l of
                                    Float i -> return i
                                    _ -> TypeMismatch "number" l
                ,   unpackToComplex l = case l of
                                    Float i -> return $ i :+ 0
                                    _ -> TypeMismatch "number" x
                ,   unpackToRatio = TypeMismatch "number"
                }

makeComplex i = Complex {
                    fromNum = i
                ,   unpackToNum = TypeMismatch "number"
                ,   unpackToFloat = TypeMismatch "number"
                ,   unpackToComplex l = case l of
                                    Complex i -> return i
                                    _ -> TypeMismatch "number" x
                ,   unpackToRatio = TypeMismatch "number"
                }

makeRatio i = Ratio {
                    fromNum = i
                ,   unpackToNum = TypeMismatch "number"
                ,   unpackToFloat l = case l of
                                    Ratio i -> return $ fromRational i
                ,   unpackToComplex l = case l of
                                    Ratio i -> return $ (fromRational i) :+ 0
                                    _ -> TypeMismatch "number" x
                ,   unpackToRatio l = case l of
                                    Ratio i -> return i
                                    _ -> TypeMismatch "number" l
                }


instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ (unescapeString contents) ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char contents) = "#\\" ++ case contents of
                            '\n' -> "newline"
                            ' ' -> "space"
                            _ -> [contents]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents) =
    "#(" ++ (unwordsList . V.toList) contents ++ ")"
showVal (Float contents) = show contents
showVal (Complex (r :+ i))
    | i > 0 = show r ++ "+" ++ show i ++ "i"
    | i < 0 = show r ++ show i ++ "i"
    | i == 0 = show r
showVal (Ratio contents) = show (numerator contents) ++ "/" ++ show (denominator contents)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unescapeString :: String -> String
unescapeString [] = []
unescapeString (x:xs)
    | x == '\n' = "\\n" ++ unescapeString xs
    | x == '\t' = "\\t" ++ unescapeString xs
    | x == '\r' = "\\r" ++ unescapeString xs
    | x == '\"' = "\\\"" ++ unescapeString xs
    | x == '\\' = "\\\\" ++ unescapeString xs
    | otherwise = x : unescapeString xs

-- |
-- >>> parse parseString "lisp" "\"he\\nllo\""
-- Right "he\nllo"
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (many1 (noneOf "\\\"") <|> do
        char '\\'
        s <- oneOf "nrt\"\\"
        case s of
            '\"' -> return "\""
            '\\' -> return "\\"
            't' -> return "\t"
            'n' -> return "\n"
            'r' -> return "\r")
    char '"'
    return $ String $ join x

-- |
-- >>> parse parseAtom "lisp" "hello"
-- Right hello
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first : rest

-- |
-- >>> parse parseBool "lisp" "#t"
-- Right #t
--
-- >>> parse parseBool "lisp" "#f"
-- Right #f
parseBool :: Parser LispVal
parseBool = try $ do
    char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

-- |
-- >>> parse parseSignedNumber "lisp" "+#x123"
-- Right 291
--
-- >>> parse parseSignedNumber "lisp" "+#o123"
-- Right 83
--
-- >>> parse parseSignedNumber "lisp" "+#d123"
-- Right 123
--
-- >>> parse parseSignedNumber "lisp" "-#b101"
-- Right -5
--
-- >>> parse parseSignedNumber "lisp" "-123"
-- Right -123
--
-- >>> parse parseSignedNumber "lisp" "-3+2i"
-- Right -3.0+2.0i
--
-- >>> parse parseSignedNumber "lisp" "-3/2"
-- Right -3/2
parseSignedNumber :: Parser LispVal
parseSignedNumber = try $ do
    signChar <- oneOf "+-"
    let sign = case signChar of
                '+' -> 1 :: Integer
                '-' -> -1
    ureal <- parseComplex <|> parseRatio <|> parseFloat <|> parsePrefixNumber <|> parseDecimal
    return $ case ureal of
                Ratio {fromNum = r} -> makeRatio (r * (fromIntegral sign))
                Complex {fromNum = (r :+ i)} -> makeComplex $ (fromIntegral sign * r) :+ i
                Float {fromNum = f} -> makeFloat $ (fromIntegral sign) * f
                Number {fromNum = n} -> makeNumber $ sign * n

-- |
-- >>> parse parseUnsignedNumber "lisp" "#x123"
-- Right 291
--
-- >>> parse parseUnsignedNumber "lisp" "#o123"
-- Right 83
--
-- >>> parse parseUnsignedNumber "lisp" "#d123"
-- Right 123
--
-- >>> parse parseUnsignedNumber "lisp" "#b101"
-- Right 5
--
-- >>> parse parseUnsignedNumber "lisp" "123"
-- Right 123
parseUnsignedNumber :: Parser LispVal
parseUnsignedNumber = parsePrefixNumber <|> parseDecimal

parsePrefixNumber :: Parser LispVal
parsePrefixNumber = parseOctal
    <|> parseHexadecimal
    <|> parseBinary
    <|> (do
        try $ string "#d"
        parseDecimal)

parseOctal :: Parser LispVal
parseOctal = do
    try $ string "#o"
    os <- many1 octDigit
    return $ (makeNumber . fst . head . readOct) os

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
    try $ string "#x"
    os <- many1 hexDigit
    return $ (makeNumber . fst . head . readHex) os

parseBinary :: Parser LispVal
parseBinary = do
    try $ string "#b"
    bs <- many1 (oneOf "01")
    return $ (makeNumber . readBinary) bs

readBinary :: Num a => String -> a
readBinary = foldl (\x y -> x * 2 + (fromIntegral . digitToInt) y) 0

parseDecimal :: Parser LispVal
parseDecimal = do
    ds <- many1 digit
    return $ (makeNumber . read) ds

-- |
-- >>> parse parseChar "list" "#\\c"
-- Right #\c
--
-- >>> parse parseChar "lisp" "#\\space"
-- Right #\space
parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    cs <- parseCharLiteral <|> anyChar
    return $ Char cs

parseCharLiteral :: Parser Char
parseCharLiteral = try $ do
    cs <- string "space" <|> string "newline"
    case cs of
        "space" -> return ' '
        "newline" -> return '\n'

parseFloat :: Parser LispVal
parseFloat = try $ do
    integer <- many1 digit
    char '.'
    fractional <- many1 digit
    return $ (makeFloat . fst . head . readFloat) (integer ++ "." ++ fractional)

parseRatio :: Parser LispVal
parseRatio = try $ do
    numerator <- many1 digit
    char '/'
    denominator <- many1 digit
    return $ makeRatio $ (read numerator) % (read denominator)

-- |
-- >>> parse parseComplex "lisp" "3.2+2i"
-- Right 3.2+2.0i
--
-- >>> parse parseComplex "lisp" "+2i"
-- Right 0.0+2.0i
--
-- >>> parse parseComplex "lisp" "-2i"
-- Right 0.0-2.0i
parseComplex :: Parser LispVal
parseComplex = try parseImaginary <|> (try $ do
    real <- many1 digit
    realFrac <- (char '.' >> many1 digit) <|> (return "0")
    sign <- oneOf "+-"
    complex <- many digit
    complexFrac <- (char '.' >> many1 digit) <|> (return "0")
    char 'i'
    let okComplex = case complex of
                        [] -> "1"
                        _ -> complex
    return $ makeComplex $ ((fst . head . readFloat) (real ++ "." ++ realFrac) :+
                        ((fst . head . readFloat) (okComplex ++ "." ++ complexFrac)) * case sign of
                            '-' -> -1
                            '+' -> 1))

parseImaginary :: Parser LispVal
parseImaginary = try $ do
    sign <- (oneOf "+-" >>= \s -> return [s]) <|> return ""
    complex <- many1 digit
    complexFrac <- (char '.' >> many1 digit) <|> (return "0")
    char 'i'
    let okComplex = case complex of
                        [] -> "1"
                        _ -> complex
    return $ makeComplex $ (0.0 :+ ((fst . head . readFloat) (okComplex ++ "." ++ complexFrac)) * case sign of
                            [] -> 1
                            ('-':_) -> -1
                            ('+':_) -> 1)

-- |
-- >>> parse parseList "lisp" "1 2 3"
-- Right (1 2 3)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- |
-- >>> parse parseDottedList "lisp" "1 . 2"
-- Right (1 . 2)
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

-- |
-- >>> parse parseVector "lisp" "#(1 2 3)"
-- Right #(1 2 3)
parseVector :: Parser LispVal
parseVector = try $ do
    string "#("
    x <- sepBy parseExpr spaces
    string ")"
    return $ Vector $ V.fromList x

-- |
-- >>> parse parseExpr "lisp" "hello"
-- Right hello
--
-- >>> parse parseExpr "lisp" "#(1 2 3)"
-- Right #(1 2 3)
--
-- >>> parse parseExpr "lisp" "3+2i"
-- Right 3.0+2.0i
--
-- >>> parse parseExpr "lisp" "3.2+2i"
-- Right 3.2+2.0i
--
-- >>> parse parseExpr "lisp" "3/2"
-- Right 3/2
--
-- >>> parse parseExpr "lisp" "3.2"
-- Right 3.2
--
-- >>> parse parseExpr "lisp" "-3.2"
-- Right -3.2
--
-- >>> parse parseExpr "lisp" "+#x123"
-- Right 291
--
-- >>> parse parseExpr "lisp" "#x123"
-- Right 291
--
-- >>> parse parseExpr "lisp" "+#o123"
-- Right 83
--
-- >>> parse parseExpr "lisp" "+#d123"
-- Right 123
--
-- >>> parse parseExpr "lisp" "123"
-- Right 123
--
-- >>> parse parseExpr "lisp" "-#b101"
-- Right -5
--
-- >>> parse parseExpr "lisp" "#t"
-- Right #t
--
-- >>> parse parseExpr "lisp" "#f"
-- Right #f
--
-- >>> parse parseExpr "list" "#\\c"
-- Right #\c
--
-- >>> parse parseExpr "lisp" "#\\space"
-- Right #\space
--
-- >>> parse parseExpr "lisp" "atom"
-- Right atom
--
-- >>> parse parseExpr "lisp" "'(a list)"
-- Right (quote (a list))
--
-- >>> parse parseExpr "lisp" "`(a list)"
-- Right (quasiquote (a list))
--
-- >>> parse parseExpr "lisp" "`(a ,(+ 1 2))"
-- Right (quasiquote (a (unquote (+ 1 2))))
--
-- >>> parse parseExpr "lisp" "#(1 2 3)"
-- Right #(1 2 3)
--
-- >>> parse parseExpr "lisp" "(1 2 3)"
-- Right (1 2 3)
--
-- >>> parse parseExpr "lisp" "(1 2 . 3)"
-- Right (1 2 . 3)
--
-- >>> parse parseExpr "lisp" "\"he\\nllo\""
-- Right "he\nllo"
parseExpr :: Parser LispVal
parseExpr = parseString
    <|> parseComplex
    <|> parseRatio
    <|> parseFloat
    <|> parseSignedNumber
    <|> parseUnsignedNumber
    <|> parseBool
    <|> parseChar
    <|> parseAtom
    <|> parseQuoted
    <|> parseQuasiQuoted
    <|> parseUnquoted
    <|> parseVector
    <|> do
            char '('
            x <- (try parseList) <|> parseDottedList
            char ')'
            return x

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
primitives = [("+", add),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
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

--urgh.... fixme, ugly but handles promotion
numericAdd :: [LispVal] -> ThrowsError LispVal
numericAdd singleVal@[_] = throwError $ NumArgs 2 singleVal
numericAdd [(Number arg1), (Number arg2)] = return $ Number $ arg1 + arg2
numericAdd [(Float arg1), (Float arg2)] = return $ Float $ arg1 + arg2
numericAdd [(Float arg1), (Number arg2)] = return $ Float $ arg1 + (fromIntegral arg2)
numericAdd [(Number arg1), (Float arg2)] = return $ Float $ (fromIntegral arg1) + arg2
numericAdd [(Complex arg1), (Number arg2)] = return $ Complex $ arg1 + (fromIntegral arg2)
numericAdd [(Number arg1), (Complex arg2)] = return $ Complex $ (fromIntegral arg1) + arg2
numericAdd [(Complex arg1), (Float arg2)] = return $ Complex $ arg1 + (arg2 :+ 0)
numericAdd [(Float arg1), (Complex arg2)] = return $ Complex $ (arg1 :+ 0) + arg2
numericAdd [(Ratio arg1), (Ratio arg2)] = return $ Ratio $ arg1 + arg2
numericAdd [(Number arg1), (Ratio arg2)] = return $ Ratio $ (arg1 % 1) + arg2
numericAdd [(Ratio arg1), (Number arg2)] = return $ Ratio $ arg1 + (arg2 % 1)
numericAdd [(Ratio arg1), (Float arg2)] = return $ Float $ (fromRational arg1) + arg2
numericAdd [(Float arg1), (Ratio arg2)] = return $ Float $ arg1 + (fromRational arg2)
numericAdd [(Ratio arg1), (Complex arg2)] = return $ Complex $ (fromRational arg1) + arg2
numericAdd [(Complex arg1), (Ratio arg2)] = return $ Complex $ arg1 + (fromRational arg2)
numericAdd (notNum:_) = throwError $ TypeMismatch "number" notNum

-- not sure I understand all this, but nicer, but doesn't yet handle promotion
data NumUnpacker = forall a. (NumToLispVal a, Num a) => AnyNumUnpacker (LispVal -> ThrowsError a)

-- this pattern is the problem - it only uses a single unpacker at a time
unpackAdd :: LispVal -> LispVal -> NumUnpacker -> ThrowsError (Maybe LispVal)
unpackAdd arg1 arg2 (AnyNumUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ Just . toLispNum $ unpacked1 + unpacked2
        `catchError` (const $ return Nothing)

numUnpackers :: [NumUnpacker]
numUnpackers = [AnyNumUnpacker unpackFloat, AnyNumUnpacker unpackComplex, AnyNumUnpacker unpackNum,
                AnyNumUnpacker unpackRatio]

firstJust :: [Maybe a] -> Maybe a
firstJust = foldl (\x y -> case x of
                    Just a -> Just a
                    Nothing -> y) Nothing

add :: [LispVal] -> ThrowsError LispVal
add [arg1, arg2] = do
    primitiveAdd <- liftM (firstJust) $ mapM (unpackAdd arg1 arg2) numUnpackers
    case primitiveAdd of
        Nothing -> throwError $ TypeMismatch "number" arg1
        Just a -> return $ a
add badArgList = throwError $ NumArgs 2 badArgList

class Num a => NumToLispVal a where
    toLispNum :: a -> LispVal
instance NumToLispVal (Integer) where
    toLispNum = Number
instance NumToLispVal (Float) where
    toLispNum = Float
instance NumToLispVal (Complex Float) where
    toLispNum = Complex
instance NumToLispVal (Ratio Integer) where
    toLispNum = Ratio

----------------------------------------------------
-- numericBinopNew :: Num a => (a -> a -> a) -> [LispVal] -> ThrowsError LispVal
-- numericBinopNew f singleVal@[_] = throwError $ NumArgs 2 singleVal
-- numericBinOpNew f [(Number arg1), (Number arg2)] = return $ Number $ f arg1 arg2
-- numericBinOpNew f [(Float arg1), (Float arg2)] = return $ Float $ f arg1 arg2
-- numericBinOpNew f [(Float arg1), (Number arg2)] = return $ Float $ f arg1 arg2
-- numericBinOpNew f [(Number arg1), (Float arg2)] = return $ Float $ f arg1 arg2
-- numericBinOpNew f [(Complex arg1), (Number arg2)] = return $ Complex $ f arg1 arg2
-- numericBinOpNew f [(Number arg1), (Complex arg2)] = return $ Complex $ f arg1 arg2
-- numericBinOpNew f [(Complex arg1), (Float arg2)] = return $ Complex $ f arg1 arg2
-- numericBinOpNew f [(Float arg1), (Complex arg2)] = return $ Complex $ f arg1 arg2
-- numericBinOpNew f [(Ratio arg1), (Ratio arg2)] = return $ Ratio $ f arg1 arg2
-- numericBinOpNew f [(Number arg1), (Ratio arg2)] = return $ Ratio $ f arg1 arg2
-- numericBinOpNew f [(Ratio arg1), (Number arg2)] = return $ Ratio $ f arg1 arg2
-- numericBinOpNew f [(Float arg1), (Ratio arg2)] = return $ Float $ f arg1 arg2
-- numericBinOpNew f [(Ratio arg1), (Float arg2)] = return $ Float $ f (approxRational arg1) arg2
-- numericBinOpNew f [(Float arg1), (Ratio arg2)] = return $ Float $ f arg1 (approxRational arg2)
-- numericBinOpNew f [(Ratio arg1), (Complex arg2)] = return $ Complex $ f (approxRational arg1) arg2
-- numericBinOpNew f [(Complex arg1), (Ratio arg2)] = return $ Complex $ f arg1 (approxRational arg2)
-- numericBinOpNew _ (notNum:_) = throwError $ TypeMismatch "number" notNum
----------------------------------------------------

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
unpackComplex (List [n]) = unpackComplex n
unpackComplex notNum = throwError $ TypeMismatch "number" notNum

unpackRatio :: LispVal -> ThrowsError (Ratio Integer)
unpackRatio (Ratio n) = return n
unpackRatio (List [n]) = unpackRatio n
unpackRatio notNum = throwError $ TypeMismatch "number" notNum

unpackFloat :: LispVal -> ThrowsError Float
unpackFloat (Float n) = return n
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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
