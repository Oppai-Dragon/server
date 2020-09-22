module Data.MyValue.Parse
  ( parseIntegerStr
  , parseIntegerArrStr
  , parseStringStr
  , parseStringArrStr
  , parseBoolStr
  , parseBoolArrStr
  , parseDateStr
  , parseDateArrStr
  , parseUriStr
  , parseNextvalStr
  , parseArr
  ) where

import Data.Base.Parsec

import Data.Char
import Text.Parsec

parseIntegerStr, parseStringStr, parseBoolStr, parseDateStr, parseUriStr, parseNextvalStr ::
     Parsec String String String
parseIntegerArrStr, parseStringArrStr, parseBoolArrStr, parseDateArrStr ::
     Parsec String String [String]
parseIntegerStr = many1 digit

parseIntegerArrStr = parseArr digit

parseStringStr = many1 $ noneOf "[]{}:;\'\",<.>/?\\=+()*&^%$#@!~`\n\t\r"

parseStringArrStr = do
  strings <- parseArr $ noneOf "[]{}:;\',<.>/?\\=+()*&^%$#@!~`\n\t\r"
  let arr = map (filter (/= '\"')) strings
  return arr

parseBoolStr =
  fmap (map toUpper) $
  try (string "False") <|> string "FALSE" <|> string "false" <|>
  try (string "True") <|>
  string "TRUE" <|>
  string "true"

parseBoolArrStr = concat <$> parseArr parseBoolStr

parseDateStr = do
  num1 <- try (count 4 digit) <|> count 2 digit
  sep1 <- char '-'
  num2 <- count 2 digit
  sep2 <- char '-'
  num3 <- try (count 4 digit) <|> count 2 digit
  return $ num1 <> (sep1 : num2) <> (sep2 : num3)

parseDateArrStr = concat <$> parseArr parseDateStr

parseUriStr = do
  part1 <- string "http"
  part2 <- many1 anyChar
  return $ part1 <> part2

parseNextvalStr = do
  let parseStr = do
        part1 <- string "nextval("
        part2 <- many anyChar
        return $ part1 <> part2
  let parseAccessKey = do
        part1 <- letterDigitN 8
        sep1 <- char '-'
        part2 <- letterDigitN 4
        sep2 <- char '-'
        part3 <- letterDigitN 4
        sep3 <- char '-'
        part4 <- letterDigitN 4
        sep4 <- char '-'
        part5 <- letterDigitN 12
        return $
          part1 <>
          (sep1 : part2) <> (sep2 : part3) <> (sep3 : part4) <> (sep4 : part5)
  try parseStr <|> parseAccessKey

parseArr :: Parsec String String a -> Parsec String String [[a]]
parseArr parserX = do
  _ <- char '[' <|> char '{'
  arr <- many parserX `sepBy` char ','
  _ <- char ']' <|> char '}'
  return arr
