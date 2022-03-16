{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module MizParser
    ( parse
    , Parser (..)
    , Input (..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Numeric
import System.Exit
import System.IO

import MizTypes

parse :: String -> IO ()
parse file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    case runParser mizMission $ Input 0 contents of
      Right (input, actualAst) -> do
        print actualAst
      Left (ParseError loc msg) -> do
        putStrLn $
          "[ERROR] Parser failed at character " ++ show loc ++ ": " ++ msg
        exitFailure

inputUncons :: Input                  -- input to check
            -> Maybe (Char, Input)
inputUncons (Input _ [])       = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

data Input = Input
    { inputLoc :: Int
    , inputStr :: String
    } deriving (Show, Eq)

data ParseError = ParseError Int String deriving (Show)

newtype Parser a = Parser
    { runParser :: Input -> Either ParseError (Input, a)
    }


instance Functor Parser where
    fmap f (Parser p)= Parser $ \input -> do
       (input', x) <- p input
       return (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    (Parser p1) <*> (Parser p2) = 
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            return (input'', f a)

instance Alternative (Either ParseError) where
    empty = Left $ ParseError 0 "Empty"
    Left _ <|> e2 = e2
    e1 <|>_ = e1

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = 
        Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser f 
    where 
        f input@(inputUncons -> Just (y,ys))
            | y == c = Right (ys, c)
            | otherwise =
                Left $ 
                ParseError 
                    (inputLoc input)
                    ("Expected '" ++ [c] ++ "' but found '" ++ [y] ++ "' at " ++ take 10 (inputStr ys))
        f input =
            Left $ 
            ParseError
                (inputLoc input)
                ("Expected '" ++ [c] ++ "' but reached end of string")

stringP :: String         -- String to find in the input
        -> Parser String
stringP str =
  Parser $ \input ->
    case runParser (traverse charP str) input of
      Left _ ->
        Left $
        ParseError
          (inputLoc input)
          ("Expected \"" ++ str ++ "\", but found \"" ++ inputStr input ++ "\"")
      result -> result


-- | Create a parser for boolean values
mizBool :: Parser MizValue
mizBool = mizTrue <|> mizFalse
  where
    mizTrue = MizBool True <$ stringP "true"
    mizFalse = MizBool False <$ stringP "false"

-- | Parser of strings where all characters satifsfy a predicate
spanP :: String           -- description
      -> (Char -> Bool)   -- predicate
      -> Parser String
spanP desc = many . parseIf desc

-- | Parser of a character that satisfies a predicate
parseIf :: String         -- Description of the predicate
        -> (Char -> Bool) -- predicate
        -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParseError
            (inputLoc input)
            ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ ->
        Left $
        ParseError
          (inputLoc input)
          ("Expected " ++ desc ++ ", but reached end of string")

{-
See page 12 of
http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
-}
-- | Parser for doubles
doubleLiteral :: Parser Double
doubleLiteral =
  doubleFromParts
    <$> (minus <|> pure 1)
    <*> (read <$> digits)
    <*> (read <$> (('0':) <$> ((:) <$> charP '.' <*> digits)))
    <*> ((e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0)
  where
    digits = some $ parseIf "digit" isDigit
    minus = (-1) <$ charP '-'
    plus = 1 <$ charP '+'
    e = charP 'e' <|> charP 'E'

-- | Build a Double from its parts (sign, integral part, decimal part, exponent)
doubleFromParts :: Integer  -- sign
                -> Integer  -- integral part
                -> Double   -- decimal part
                -> Integer  -- exponent
                -> Double
doubleFromParts sign int dec expo =
  fromIntegral sign * (fromIntegral int + dec) * (10 ^^ expo)

intLiteral :: Parser Integer
intLiteral =
    intFromParts 
      <$> (minus <|> pure 1)
      <*> (read <$> digits)
    where
      digits = some $ parseIf "digit" isDigit
      minus = (-1) <$ charP '-'

-- | Build a Double from its parts (sign, integral part)
intFromParts :: Integer  -- sign
                -> Integer  -- integral part
                -> Integer
intFromParts sign int =
  fromIntegral sign * fromIntegral int

-- | Parser for json number values
mizNumber :: Parser MizValue
mizNumber = MizNumber <$> doubleLiteral

-- | Parser for characters as unicode in input
escapeUnicode :: Parser Char
escapeUnicode = chr . fst . head . readHex <$> replicateM 4 (parseIf "hex digit" isHexDigit)

-- | Parser for characters that are scaped in the input
escapeChar :: Parser Char
escapeChar = ('"' <$ stringP "\\\"") <|>
             ('\\' <$ stringP "\\\\") <|>
             ('/' <$ stringP "\\/") <|>
             ('\b' <$ stringP "\\b") <|>
             ('\f' <$ stringP "\\f") <|>
             ('\n' <$ stringP "\\n") <|>
             ('\r' <$ stringP "\\r") <|>
             ('\t' <$ stringP "\\t") <|>
             (stringP "\\u" *> escapeUnicode)

-- | Parser of a character that is not " or \\
normalChar :: Parser Char
normalChar = parseIf "non-special character" ((&&) <$> (/= '"') <*> (/= '\\'))

-- | Parser of a string that is between double quotes (not considering any double quots that are scaped)
stringLiteral :: Parser String
stringLiteral = charP '"' *> many (normalChar <|> escapeChar) <* charP '"'

mizString :: Parser MizValue
mizString = MizString <$> stringLiteral

mizInt :: Parser MizValue
mizInt = MizInt <$> intLiteral

ws :: Parser String
ws = spanP "whitespace character" isSpace

-- | Creates a parser for a string of type "element1 sep1 element2 sep2 element3"
-- from a parser for separators (sep1, sep2) and and a parser form elements (element1, element2, element3).
sepBy :: Parser a   -- Parser for the separators
      -> Parser b   -- Parser for elements
      -> Parser [b]
sepBy sep element = many (element <* sep) 


mizStringTag :: Parser MizTag
mizStringTag = MizStringTag <$> 
    (charP '[' *> stringLiteral <* charP ']')

mizIntTag :: Parser MizTag
mizIntTag = MizIntTag <$> 
    (charP '[' *> intLiteral <* charP ']')

mizTag :: Parser MizTag
mizTag = mizStringTag <|> mizIntTag

mizSeparator :: Parser String
mizSeparator = ws <* charP ',' <* ws <* stringP "--" <* spanP "no line break" (/= '\n') *> ws

stdSeparator :: Parser String
stdSeparator = ws *> charP ',' *> ws

separator :: Parser String
separator = mizSeparator <|> stdSeparator

mizValue :: Parser MizValue
mizValue = mizString <|> mizNumber <|> mizInt <|> mizArray <|> mizBool

mizElem :: Parser MizItem
mizElem = 
    liftA2 MizElem
        mizTag
        (ws *> charP '=' *> ws *> mizValue)

mizArray :: Parser MizValue
mizArray = 
    MizList 
        <$> (ws *> charP '{' *> ws *> many (mizElem <* separator) <* ws <* charP '}')

mizMission :: Parser MizItem
mizMission =
    liftA2 MizElem
        (MizMission <$ (stringP "mission" *> ws *> charP '=' *> ws ))
        mizArray

-- | Apply parser to content of file
parseFile :: FilePath                 -- File path to parse
          -> Parser a                 -- Parser to use
          -> IO (Either ParseError a)
parseFile fileName parser = do
  input <- readFile fileName
  case runParser parser $ Input 0 input of
    Left e       -> return $ Left e
    Right (_, x) -> return $ Right x


