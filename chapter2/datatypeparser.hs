module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding ( spaces )

main :: IO ()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

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

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (many (noneOf "\"") <|> do
        e <- char '\\'
        s <- oneOf "\"nrt\\"
        return $ [e,s])
    char '"'
    return $ String $ join x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many ( letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom

parseNumber :: Parser LispVal
-- original
--parseNumber = liftM (Number . read) $ many1 digit
-- 2.1a
parseNumber = do
    ds <- many1 digit
    return $ (Number . read) ds
-- 2.1b
--parseNumber = (many1 digit) >>= (\ds -> return $ (Number . read) ds)

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
