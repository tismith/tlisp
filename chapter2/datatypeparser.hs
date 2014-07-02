module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding ( spaces )
import Numeric
import Data.Char
import Data.Ratio
import Data.Complex

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
            '\"' -> return [s]
            '\\' -> return [s]
            't' -> return "\t"
            'n' -> return "\n"
            'r' -> return "\r")
    char '"'
    return $ String $ join x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom $ first : rest

parseBool :: Parser LispVal
parseBool = try $ do
    char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

{-
 - A number may be written in binary, octal, decimal, or hex- adecimal by the use of a radix prefix.
 - The radix prefixes are #b (binary), #o (octal), #d (decimal), and #x (hexadec- imal).
 - With no radix prefix, a number is assumed to be expressed in decimal.
 -}
-- original
--parseNumber = liftM (Number . read) $ many1 digit
-- 2.1b
--parseNumber = (many1 digit) >>= (\ds -> return $ (Number . read) ds)

parseUnsignedNumber :: Parser LispVal
parseUnsignedNumber = parsePrefixNumber <|> parseDecimal

parseSignedNumber :: Parser LispVal
parseSignedNumber = do
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
