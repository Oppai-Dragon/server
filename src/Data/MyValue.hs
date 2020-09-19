{-# LANGUAGE FlexibleContexts #-}

module Data.MyValue
  ( MyValue(..)
  , parseInteger
  , parseString
  , parseBool
  , parseIntegers
  , parseStrings
  , parseDate
  , parseNextval
  , parseMyValue
  , fromBS
  , fromValue
  , fromStr
  , toStr
  , toValue
  ) where

import Data.Base hiding (toStr)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec

data MyValue
  = MyString String
  | MyStrings [String]
  | MyInteger Integer
  | MyIntegers [Integer]
  | MyBool Bool
  | MyBools [Bool]
  | MyNextval String
  | MyDate String
  | MyDates [String]
  | MyEmpty
  deriving (Read, Show, Eq)

parseInteger, parseString, parseBool, parseIntegers, parseStrings, parseDate, parseNextval ::
     Parsec String String MyValue
parseInteger = do
  field <- many1 digit
  return . MyInteger $
    case reads field of
      [(l, _)] -> l
      _ -> 0

parseString = do
  field <- many1 $ noneOf "[]{}:;\'\",<.>/?\\=+()*&^%$#@!~`\n\t\r"
  return $ MyString field

parseBool = do
  field <-
    try (string "False") <|> string "FALSE" <|> string "false" <|>
    try (string "True") <|>
    string "TRUE" <|>
    string "true"
  return . MyBool $
    case map toUpper field of
      "FALSE" -> False
      "TRUE" -> True
      _ -> False

parseDate = do
  num1 <- try (count 4 digit) <|> count 2 digit
  sep1 <- char '-'
  num2 <- count 2 digit
  sep2 <- char '-'
  num3 <- try (count 4 digit) <|> count 2 digit
  return . MyDate $ num1 <> (sep1 : num2) <> (sep2 : num3)

parseIntegers = do
  _ <- char '[' <|> char '{'
  integers <- many $ digit <|> char ','
  _ <- char ']' <|> char '}'
  let arr = '[' : integers <> "]"
  return . MyIntegers $
    case reads arr :: [([Integer], String)] of
      [(l, _)] -> l
      _ -> []

parseStrings = do
  _ <- char '[' <|> char '{'
  strings <-
    many (noneOf "[]{}:;\'<,.>/?\\=+()*&^%$#@!~`\n\t\r") `sepBy` char ','
  _ <- char ']' <|> char '}'
  let arr = map (filter (/= '\"')) strings
  return $ MyStrings arr

parseLDtimes :: Int -> Parsec String String String
parseLDtimes 0 = return []
parseLDtimes n = (letter <|> digit) >>= (\ch -> (ch :) <$> parseLDtimes (n - 1))

parseNextval = do
  let parseStr = do
        part1 <- string "nextval("
        part2 <- many anyChar
        return $ part1 <> part2
  let parseAccessKey = do
        part1 <- parseLDtimes 8
        sep1 <- char '-'
        part2 <- parseLDtimes 4
        sep2 <- char '-'
        part3 <- parseLDtimes 4
        sep3 <- char '-'
        part4 <- parseLDtimes 4
        sep4 <- char '-'
        part5 <- parseLDtimes 12
        return $
          part1 <>
          (sep1 : part2) <> (sep2 : part3) <> (sep3 : part4) <> (sep4 : part5)
  result <- try parseStr <|> parseAccessKey
  return $ MyNextval result

parseMyValue :: Parsec String String MyValue
parseMyValue =
  try parseBool <|> try parseDate <|> try parseNextval <|> try parseInteger <|>
  try parseString <|>
  try parseIntegers <|>
  parseStrings

fromBS :: BS.ByteString -> MyValue
fromBS = fromStr . BSC8.unpack

fromValue :: A.Value -> MyValue
fromValue value =
  case value of
    A.Number num -> MyInteger . valueToInteger $ A.Number num
    A.String text -> fromStr $ T.unpack text
    A.Bool bool -> MyBool bool
    A.Array vector ->
      if V.null vector
        then MyEmpty
        else case head (V.toList vector) of
               A.String _ ->
                 MyStrings . map (\(A.String x) -> T.unpack x) $ V.toList vector
               A.Number _ -> MyIntegers . map valueToInteger $ V.toList vector
               A.Bool _ -> MyBools . map (\(A.Bool x) -> x) $ V.toList vector
               _ -> MyEmpty
    A.Object obj -> MyStrings . map T.unpack $ HM.keys obj
    A.Null -> MyEmpty

fromStr :: String -> MyValue
fromStr str =
  case runParser parseMyValue "" "MyValue" str of
    Right x -> x
    Left _ -> MyEmpty

toStr :: MyValue -> String
toStr myValue =
  case myValue of
    MyInteger num -> show num
    MyString str -> str
    MyBool bool -> show bool
    MyIntegers intArr -> show intArr
    MyStrings strArr -> filter (/= '\"') $ show strArr
    MyBools boolArr -> show boolArr
    MyNextval val -> val
    MyDate date -> date
    MyDates dateArr -> filter (/= '\"') $ show dateArr
    MyEmpty -> ""

toValue :: MyValue -> A.Value
toValue myValue =
  case myValue of
    MyInteger num -> A.Number $ Scientific.scientific num 0
    MyString str -> A.String $ T.pack str
    MyBool bool -> A.Bool bool
    MyIntegers arr ->
      A.Array . V.fromList $ map (A.Number . flip Scientific.scientific 0) arr
    MyStrings arr -> A.Array . V.fromList $ map (A.String . T.pack) arr
    MyBools arr -> A.Array . V.fromList $ map A.Bool arr
    MyNextval val -> A.String $ T.pack val
    MyDate date -> A.String $ T.pack date
    MyDates arr -> A.Array . V.fromList $ map (A.String . T.pack) arr
    MyEmpty -> A.Null
