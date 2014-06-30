module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding ( spaces )
import Numeric
import Data.Char

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool
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
parseBool = do
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

parseNumber :: Parser LispVal
parseNumber = parsePrefixNumber <|> parseDecimal

parsePrefixNumber :: Parser LispVal
parsePrefixNumber = parseOctal
    <|> parseHexadecimal
    <|> parseBinary
    <|> (do
        string "#d"
        parseDecimal)

parseOctal :: Parser LispVal
parseOctal = do
    string "#o"
    os <- many1 octDigit
    return $ (Number . fst . head . readOct) os

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
    string "#x"
    os <- many1 hexDigit
    return $ (Number . fst . head . readHex) os

parseBinary :: Parser LispVal
parseBinary = do
    string "#b"
    bs <- many1 (oneOf "01")
    return $ (Number . readBinary) bs

readBinary :: Num a => String -> a
readBinary = foldl (\x y -> x * 2 + (fromIntegral . digitToInt) y) 0

parseDecimal :: Parser LispVal
parseDecimal = do
    ds <- many1 digit
    return $ (Number . read) ds

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseBool
