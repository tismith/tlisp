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
        | Number Integer
        | String String
        | Bool Bool
        | Char Char
        | Float Float
        | Complex (Complex Integer)
        | ComplexFloat (Complex Float)
        | Ratio (Ratio Integer)
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
showVal (ComplexFloat (r :+ i))
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
-- Right -3+2i
--
-- >>> parse parseSignedNumber "lisp" "-3/2"
-- Right -3/2
parseSignedNumber :: Parser LispVal
parseSignedNumber = try $ do
    signChar <- oneOf "+-"
    let sign = case signChar of
                '+' -> 1
                '-' -> -1
    ureal <- parseComplex <|> parseComplexFloat <|> parseRatio <|> parseFloat <|> parsePrefixNumber <|> parseDecimal
    return $ case ureal of
                Ratio r -> Ratio (r * (fromIntegral sign))
                Complex (r :+ i) -> Complex $ (sign * r) :+ i
                Float f -> Float $ (fromIntegral sign) * f
                Number n -> Number $ sign * n

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
    return $ (Number . fst . head . readOct) os

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
    try $ string "#x"
    os <- many1 hexDigit
    return $ (Number . fst . head . readHex) os

parseBinary :: Parser LispVal
parseBinary = do
    try $ string "#b"
    bs <- many1 (oneOf "01")
    return $ (Number . readBinary) bs

readBinary :: Num a => String -> a
readBinary = foldl (\x y -> x * 2 + (fromIntegral . digitToInt) y) 0

parseDecimal :: Parser LispVal
parseDecimal = do
    ds <- many1 digit
    return $ (Number . read) ds

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
    return $ (Float . fst . head . readFloat) (integer ++ "." ++ fractional)

parseRatio :: Parser LispVal
parseRatio = try $ do
    numerator <- many1 digit
    char '/'
    denominator <- many1 digit
    return $ Ratio $ (read numerator) % (read denominator)

-- |
-- >>> parse parseComplexFloat "lisp" "3.2+2i"
-- Right 3.2+2.0i
parseComplexFloat :: Parser LispVal
parseComplexFloat = try $ do
    real <- many1 digit
    realFrac <- (char '.' >> many1 digit) <|> (return "0")
    sign <- oneOf "+-"
    complex <- many digit
    complexFrac <- (char '.' >> many1 digit) <|> (return "0")
    char 'i'
    let okComplex = case complex of
                        [] -> "1"
                        _ -> complex
    return $ ComplexFloat $ ((fst . head . readFloat) (real ++ "." ++ realFrac) :+
                        ((fst . head . readFloat) (okComplex ++ "." ++ complexFrac)) * case sign of
                            '-' -> -1
                            '+' -> 1)
parseComplex :: Parser LispVal
parseComplex = try $ do
    real <- many1 digit
    sign <- oneOf "+-"
    complex <- many digit
    char 'i'
    let okComplex = case complex of
                        [] -> "1"
                        _ -> complex
    return $ Complex $ (read real) :+ (read okComplex * case sign of
                        '-' -> -1
                        '+' -> 1)

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
-- Right 3+2i
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
    <|> parseComplexFloat
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
eval val@(ComplexFloat _) = return val
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
primitives = [("+", numericBinop (+)),
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

numericBinop :: (Num a, Num b, Num c) => (a -> b -> c) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . toLispVal . foldl1 op

class Num a => NumToLispVal a where
    toLispVal :: a -> LispVal
instance NumToLispVal (Complex Integer) where
    toLispVal = Complex
instance NumToLispVal (Float) where
    toLispVal = Float
instance NumToLispVal (Integer) where
    toLispVal = Number
instance NumToLispVal (Ratio Integer) where
    toLispVal = Ratio
instance NumToLispVal (Complex Float) where
    toLispVal = ComplexFloat

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: Num a => LispVal -> ThrowsError a
unpackNum (Number n) = return n
unpackNum (Float n) = return n
unpackNum (Complex n) = return n
unpackNum (ComplexFloat n) = return n
unpackNum (Ratio n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
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
