{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.MyValue
  ( MyValue(..)
  , parseInteger
  , parseIntegerArr
  , parseString
  , parseStringArr
  , parseBool
  , parseBoolArr
  , parseDate
  , parseDateArr
  , parseUri
  , parseNextval
  , parseMyValue
  , fromBS
  , fromValue
  , fromStr
  , toStr
  , toValue
  ) where

import Data.Base hiding (toStr)
import Data.MyValue.Parse

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec

data MyValue
  = MyString String
  | MyStringArr [String]
  | MyInteger Integer
  | MyIntegerArr [Integer]
  | MyBool Bool
  | MyBoolArr [Bool]
  | MyNextval String
  | MyDate String
  | MyDateArr [String]
  | MyUri String
  | MyEmpty
  deriving (Read, Show, Eq)

parseInteger, parseIntegerArr, parseString, parseStringArr, parseBool, parseBoolArr, parseDate, parseDateArr, parseUri, parseNextval ::
     Parsec String String MyValue
parseInteger = do
  field <- parseIntegerStr
  return . MyInteger $
    case reads field of
      [(l, _)] -> l
      _ -> 0

parseIntegerArr = do
  integers <- parseIntegerArrStr
  let readInt str =
        case reads str of
          [(l, _)] -> l
          _ -> 0
  return . MyIntegerArr $ map readInt integers

parseString = MyString <$> parseStringStr

parseStringArr = MyStringArr <$> parseStringArrStr

parseBool = do
  field <- parseBoolStr
  return . MyBool $
    case field of
      "FALSE" -> False
      "TRUE" -> True
      _ -> False

parseBoolArr = do
  arr <- parseBoolArrStr
  let boolArr =
        map
          (\case
             "FALSE" -> False
             "TRUE" -> True
             _ -> False)
          arr
  return $ MyBoolArr boolArr

parseDate = MyDate <$> parseDateStr

parseDateArr = MyDateArr <$> parseDateArrStr

parseUri = MyUri <$> parseUriStr

parseNextval = MyNextval <$> parseNextvalStr

parseMyValue :: Parsec String String MyValue
parseMyValue =
  try parseBool <|> try parseBoolArr <|> try parseDate <|> try parseDateArr <|>
  try parseNextval <|>
  try parseInteger <|>
  try parseUri <|>
  try parseString <|>
  try parseIntegerArr <|>
  parseStringArr

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
                 MyStringArr . map (\(A.String x) -> T.unpack x) $
                 V.toList vector
               A.Number _ -> MyIntegerArr . map valueToInteger $ V.toList vector
               A.Bool _ -> MyBoolArr . map (\(A.Bool x) -> x) $ V.toList vector
               _ -> MyEmpty
    A.Object obj -> MyStringArr . map T.unpack $ HM.keys obj
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
    MyIntegerArr intArr -> show intArr
    MyString str -> str
    MyStringArr strArr -> filter (/= '\"') $ show strArr
    MyBool bool -> show bool
    MyBoolArr boolArr -> show boolArr
    MyNextval val -> val
    MyUri uri -> uri
    MyDate date -> date
    MyDateArr dateArr -> filter (/= '\"') $ show dateArr
    MyEmpty -> ""

toValue :: MyValue -> A.Value
toValue myValue =
  case myValue of
    MyInteger num -> A.Number $ Scientific.scientific num 0
    MyIntegerArr arr ->
      A.Array . V.fromList $ map (A.Number . flip Scientific.scientific 0) arr
    MyString str -> A.String $ T.pack str
    MyStringArr arr -> A.Array . V.fromList $ map (A.String . T.pack) arr
    MyBool bool -> A.Bool bool
    MyBoolArr arr -> A.Array . V.fromList $ map A.Bool arr
    MyNextval val -> A.String $ T.pack val
    MyUri uri -> A.String $ T.pack uri
    MyDate date -> A.String $ T.pack date
    MyDateArr arr -> A.Array . V.fromList $ map (A.String . T.pack) arr
    MyEmpty -> A.Null
