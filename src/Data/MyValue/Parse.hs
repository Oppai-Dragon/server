module Data.MyValue.Parse
  ( parseIntegerStr
  , parseIntegerArrStr
  , parseStringStr
  , parseStringArrStr
  , parseBoolStr
  , parseBoolArrStr
  , parseDateStr
  , parseDateArrStr
  , parseNextvalStr
  ) where

import Text.Parsec

parseIntegerStr, parseIntegerArrStr, parseStringStr, parseStringArrStr, parseBoolStr, parseBoolArrStr, parseDateStr, parseDateArrStr, parseNextvalStr ::
     Parsec String String String
parseIntegerStr = many1 digit
parseIntegerArrStr = parseArr digit
parseStringStr =
parseStringArrStr =
parseBoolStr =
parseBoolArrStr =
parseDateStr =
parseDateArrStr =
parseNextvalStr =

parseArr :: Parsec String String a -> Parsec String String String
parseArr parserX = do
  _ <- char '[' <|> char '{'
  arr <- many parserX `sepBy` char ','
  _ <- char ']' <|> char '}'
  return arr
