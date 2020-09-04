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

import Data.Base

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
  | MyNextval String
  | MyDate String
  | MyEmpty
  deriving (Show, Eq)

instance Read MyValue where
  readsPrec _ input =
    case input of
      "int" -> [(MyInteger 0, "")]
      "array int" -> [(MyIntegers [], "")]
      "string" -> [(MyString [], "")]
      "array string" -> [(MyStrings [], "")]
      "date" -> [(MyDate [], "")]
      "bool" -> [(MyBool False, "")]
      "uuid" -> [(MyString [], "")]
      _ -> [(MyEmpty, "")]

parseInteger, parseString, parseBool, parseIntegers, parseStrings, parseDate, parseNextval ::
     Parsec String String MyValue
parseInteger = do
  field <- many1 digit
  let num = read field
  return $ MyInteger num

parseString = do
  field <- many1 $ noneOf "[]{}:;\'\",<.>/?\\=+()*&^%$#@!~`\n\t\r"
  return $ MyString field

parseBool = do
  field <- many1 letter
  let bool =
        case map toUpper field of
          "FALSE" -> False
          "TRUE" -> True
          _ -> False
  return $ MyBool bool

parseDate = do
  let case1 = do
        num1 <- digit
        num2 <- digit
        sep1 <- char '-'
        num3 <- digit
        num4 <- digit
        sep2 <- char '-'
        num5 <- digit
        num6 <- digit
        num7 <- digit
        num8 <- digit
        return $
          num1 :
          num2 : sep1 : num3 : num4 : sep2 : num5 : num6 : num7 : num8 : []
  let case2 = do
        num1 <- digit
        num2 <- digit
        num3 <- digit
        num4 <- digit
        sep1 <- char '-'
        num5 <- digit
        num6 <- digit
        sep2 <- char '-'
        num7 <- digit
        num8 <- digit
        return $
          num1 :
          num2 : num3 : num4 : sep1 : num5 : num6 : sep2 : num7 : num8 : []
  field <- try case1 <|> case2
  return $ MyDate field

parseIntegers = do
  _ <- char '[' <|> char '{'
  integers <- many $ digit <|> char ','
  _ <- char ']' <|> char '}'
  let arr = '[' : integers <> "]"
  return . MyIntegers $ read arr

parseStrings = do
  _ <- char '[' <|> char '{'
  strings <- many (noneOf "[]{}:;\'<,.>/?\\=+()*&^%$#@!~`\n\t\r") `sepBy` char ','
  _ <- char ']' <|> char '}'
  let arr = map (filter (/='\"')) strings
  return $ MyStrings arr

parseNextval = do
  part1 <- string "nextval("
  part2 <- many anyChar
  let nextval = part1 <> part2
  return $ MyNextval nextval

parseMyValue :: Parsec String String MyValue
parseMyValue =
  try parseBool <|> try parseDate <|> try parseNextval <|> try parseInteger <|>
  try parseString <|>
  try parseIntegers <|>
  try parseStrings

fromBS :: BS.ByteString -> MyValue
fromBS = fromStr . BSC8.unpack

fromValue :: A.Value -> MyValue
fromValue value =
  case value of
    A.Number num -> MyInteger $ scientificToInteger num
    A.String text -> fromStr $ T.unpack text
    A.Bool bool -> MyBool bool
    A.Array vector ->
      if V.null vector
        then MyEmpty
        else case head (V.toList vector) of
               A.String _ ->
                 MyStrings . map (\(A.String x) -> T.unpack x) $ V.toList vector
               A.Number _ ->
                 MyIntegers . map (\(A.Number x) -> scientificToInteger x) $
                 V.toList vector
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
    MyNextval val -> val
    MyDate date -> date
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
    MyNextval val -> A.String $ T.pack val
    MyDate date -> A.String $ T.pack date
    MyEmpty -> A.Null
