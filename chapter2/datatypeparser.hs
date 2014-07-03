module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding ( spaces )
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex
import qualified Data.Vector as V

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

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
        | Ratio (Ratio Integer)
        deriving (Show)

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
-- Right (Atom "hello")
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first : rest

-- |
-- >>> parse parseBool "lisp" "#t"
-- Right (Bool True)
--
-- >>> parse parseBool "lisp" "#f"
-- Right (Bool False)
parseBool :: Parser LispVal
parseBool = try $ do
    char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

-- |
-- >>> parse parseSignedNumber "lisp" "+#x123"
-- Right (Number 291)
--
-- >>> parse parseSignedNumber "lisp" "+#o123"
-- Right (Number 83)
--
-- >>> parse parseSignedNumber "lisp" "+#d123"
-- Right (Number 123)
--
-- >>> parse parseSignedNumber "lisp" "-#b101"
-- Right (Number (-5))
--
-- >>> parse parseSignedNumber "lisp" "-123"
-- Right (Number (-123))
--
-- >>> parse parseSignedNumber "lisp" "-3+2i"
-- Right (Complex ((-3) :+ 2))
--
-- >>> parse parseSignedNumber "lisp" "-3/2"
-- Right (Ratio ((-3) % 2))
parseSignedNumber :: Parser LispVal
parseSignedNumber = try $ do
    signChar <- oneOf "+-"
    let sign = case signChar of
                '+' -> 1
                '-' -> -1
    ureal <- parseComplex <|> parseRatio <|> parseFloat <|> parsePrefixNumber <|> parseDecimal
    return $ case ureal of
                Ratio r -> Ratio (r * (fromIntegral sign))
                Complex (r :+ i) -> Complex $ (sign * r) :+ i
                Float f -> Float $ (fromIntegral sign) * f
                Number n -> Number $ sign * n

-- |
-- >>> parse parseUnsignedNumber "lisp" "#x123"
-- Right (Number 291)
--
-- >>> parse parseUnsignedNumber "lisp" "#o123"
-- Right (Number 83)
--
-- >>> parse parseUnsignedNumber "lisp" "#d123"
-- Right (Number 123)
--
-- >>> parse parseUnsignedNumber "lisp" "#b101"
-- Right (Number 5)
--
-- >>> parse parseUnsignedNumber "lisp" "123"
-- Right (Number 123)
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
-- Right (Char 'c')
--
-- >>> parse parseChar "lisp" "#\\space"
-- Right (Char ' ')
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
-- Right (List [Number 1,Number 2,Number 3])
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- |
-- >>> parse parseDotted "lisp" "1 . 2"
-- Right (DottedList [Number 1] (Number 2))
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
-- Right (Vector (fromList [Number 1,Number 2,Number 3]))
parseVector :: Parser LispVal
parseVector = try $ do
    string "#("
    x <- sepBy parseExpr spaces
    string ")"
    return $ Vector $ V.fromList x

-- |
-- >>> parse parseExpr "lisp" "hello"
-- Right (Atom "hello")
--
-- >>> parse parseExpr "lisp" "#(1 2 3)"
-- Right (Vector (fromList [Number 1,Number 2,Number 3]))
--
-- >>> parse parseExpr "lisp" "3+2i"
-- Right (Complex (3 :+ 2))
--
-- >>> parse parseExpr "lisp" "3/2"
-- Right (Ratio (3 % 2))
--
-- >>> parse parseExpr "lisp" "3.2"
-- Right (Float 3.2)
--
-- >>> parse parseExpr "lisp" "-3.2"
-- Right (Float (-3.2))
--
-- >>> parse parseExpr "lisp" "+#x123"
-- Right (Number 291)
--
-- >>> parse parseExpr "lisp" "#x123"
-- Right (Number 291)
--
-- >>> parse parseExpr "lisp" "+#o123"
-- Right (Number 83)
--
-- >>> parse parseExpr "lisp" "+#d123"
-- Right (Number 123)
--
-- >>> parse parseExpr "lisp" "123"
-- Right (Number 123)
--
-- >>> parse parseExpr "lisp" "-#b101"
-- Right (Number (-5))
--
-- >>> parse parseExpr "lisp" "#t"
-- Right (Bool True)
--
-- >>> parse parseExpr "lisp" "#f"
-- Right (Bool False)
--
-- >>> parse parseExpr "list" "#\\c"
-- Right (Char 'c')
--
-- >>> parse parseExpr "lisp" "#\\space"
-- Right (Char ' ')
--
-- >>> parse parseExpr "lisp" "atom"
-- Right (Atom "atom")
--
-- >>> parse parseExpr "lisp" "'(a list)"
-- Right (List [Atom "quote",List [Atom "a",Atom "list"]])
--
-- >>> parse parseExpr "lisp" "`(a list)"
-- Right (List [Atom "quasiquote",List [Atom "a",Atom "list"]])
--
-- >>> parse parseExpr "lisp" "`(a ,(+ 1 2))"
-- Right (List [Atom "quasiquote",List [Atom "a",List [Atom "unquote",List [Atom "+",Number 1,Number 2]]]])
--
-- >>> parse parseExpr "lisp" "#(1 2 3)"
-- Right (Vector (fromList [Number 1,Number 2,Number 3]))
--
-- >>> parse parseExpr "lisp" "(1 2 3)"
-- Right (List [Number 1,Number 2,Number 3])
--
-- >>> parse parseExpr "lisp" "(1 2 . 3)"
-- Right (DottedList [Number 1,Number 2] (Number 3))
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
