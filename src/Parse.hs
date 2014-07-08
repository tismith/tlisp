module Parse where
import Control.Monad (liftM, join)
import Text.ParserCombinators.Parsec (
    char
    , try
    , (<|>)
    , many
    , many1
    , string
    , sepEndBy
    , endBy
    , Parser
    , oneOf
    , noneOf
    , skipMany1
    , space
    , anyChar
    , letter
    , digit
    , octDigit
    , hexDigit)
import Numeric (readOct, readHex, readFloat)
import Data.Char (digitToInt)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Complex (Complex((:+)))
import qualified Data.Vector as V (toList, fromList, Vector)

-- $setup
-- >>> import Text.ParserCombinators.Parsec (parse)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

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
        | Complex (Complex Float)
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
    char '\"'
    x <- many (many1 (noneOf "\\\"") <|> do
        char '\\'
        s <- oneOf "nrt\"\\"
        case s of
            '\"' -> return "\""
            '\\' -> return "\\"
            't' -> return "\t"
            'n' -> return "\n"
            'r' -> return "\r")
    char '\"'
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
                Ratio r -> Ratio (r * (fromIntegral sign))
                Complex (r :+ i) -> Complex $ (fromIntegral sign * r) :+ i
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
-- >>> parse parseComplex "lisp" "3.2+2i"
-- Right 3.2+2.0i
--
-- >>> parse parseComplex "lisp" "+2i"
-- Right 0.0+2.0i
--
-- >>> parse parseComplex "lisp" "-2i"
-- Right 0.0-2.0i
parseComplex :: Parser LispVal
parseComplex = parseImaginary <|> (try $ do
    real <- many1 digit
    realFrac <- (char '.' >> many1 digit) <|> (return "0")
    sign <- oneOf "+-"
    complex <- many digit
    complexFrac <- (char '.' >> many1 digit) <|> (return "0")
    char 'i'
    let okComplex = case complex of
                        [] -> "1"
                        _ -> complex
    return $ Complex $ ((fst . head . readFloat) (real ++ "." ++ realFrac) :+
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
    return $ Complex $ (0.0 :+ ((fst . head . readFloat) (okComplex ++ "." ++ complexFrac)) * case sign of
                            [] -> 1
                            ('-':_) -> -1
                            ('+':_) -> 1)

-- |
-- >>> parse parseList "lisp" "1 2 3"
-- Right (1 2 3)
parseList :: Parser LispVal
parseList = liftM List $ sepEndBy parseExpr spaces

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
    x <- sepEndBy parseExpr spaces
    string ")"
    return $ Vector $ V.fromList x

    <|> do
            char '('
            many (space)
            x <- (try parseList) <|> parseDottedList
            char ')'
            return x

parseBracketedParser :: Parser LispVal -> Parser LispVal
parseBracketedParser p = try $ do
    char '('
    many (space)
    x <- p
    char ')'
    return x

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
-- >>> parse parseExpr "lisp" "`(a ,( + 1 2))"
-- Right (quasiquote (a (unquote (+ 1 2))))
--
-- >>> parse parseExpr "lisp" "#(1 2 3 )"
-- Right #(1 2 3)
--
-- >>> parse parseExpr "lisp" "(1 2 3)"
-- Right (1 2 3)
--
-- >>> parse parseExpr "lisp" "( 1 2 . 3)"
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
    <|> parseBracketedParser (parseList)
    <|> parseBracketedParser (parseDottedList)
