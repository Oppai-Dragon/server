module Data.MyValue
  ( MyValue(..)
  , parseIntegers
  , parseStrings
  , parseBool
  , myInteger
  , myString
  , myBool
  , myIntegers
  , myStrings
  , myDate
  , myNextval
  , chooseMyValue
  , fromBS
  , fromValue
  , fromStr
  , toStr
  , toValue
  ) where

import Data.Base

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

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

parseIntegers :: String -> String
parseIntegers [] = []
parseIntegers (x:xs) =
  case x of
    '[' -> "[" <> parseIntegers xs
    '{' -> "[" <> parseIntegers xs
    ',' -> "," <> parseIntegers xs
    ']' -> "]" <> parseIntegers xs
    '}' -> "]" <> parseIntegers xs
    _ -> x : parseIntegers xs

parseStrings :: String -> String
parseStrings [] = []
parseStrings (x:xs) =
  case x of
    '\"' -> parseStrings xs
    '[' -> "[\"" <> parseStrings xs
    '{' -> "[\"" <> parseStrings xs
    ',' -> "\",\"" <> parseStrings xs
    ']' -> "\"]" <> parseStrings xs
    '}' -> "\"]" <> parseStrings xs
    _ -> x : parseStrings xs

parseBool :: String -> String
parseBool str =
  let letter = toUpper $ head str
      word = map toLower $ tail str
   in letter : word

myInteger, myString, myBool, myIntegers, myStrings, myDate, myNextval ::
     String -> MyValue
myInteger = MyInteger . read

myString = MyString

myBool str =
  MyBool $
  case parseBool str of
    "False" -> False
    "True" -> True
    _ -> False

myDate = MyDate

myIntegers str =
  let parsedStr = parseIntegers str
   in MyIntegers $
      case parsedStr of
        '[':rest ->
          if all (\x -> x == ',' || isDigit x) $ init rest
            then read parsedStr
            else []
        _ -> []

myStrings str =
  let parsedStr = parseStrings str
   in MyStrings $
      case parsedStr of
        '[':rest ->
          case last rest of
            ']' -> read parsedStr
            _ -> []
        _ -> []

myNextval = MyNextval

chooseMyValue :: String -> (String -> MyValue)
chooseMyValue str =
  case str of
    '[':x:_ ->
      if isDigit x
        then myIntegers
        else myStrings
    '{':x:_ ->
      if isDigit x
        then myIntegers
        else myStrings
    "FALSE" -> myBool
    "TRUE" -> myBool
    _:_:_:_:'-':_:_:'-':_:_ -> myDate
    _:_:'-':_:_:'-':_:_:_:_ -> myDate
    'N':'E':'X':'T':'V':'A':'L':'(':_ -> myNextval
    [] -> const MyEmpty
    arr ->
      if all isDigit arr
        then myInteger
        else myString

fromBS :: BS.ByteString -> MyValue
fromBS valueBS =
  let valueStr = map toUpper $ BSC8.unpack valueBS
   in chooseMyValue valueStr (BSC8.unpack valueBS)

fromValue :: Value -> MyValue
fromValue value =
  case value of
    Number num -> MyInteger $ scientificToInteger num
    String text -> fromStr $ T.unpack text
    Bool bool -> MyBool bool
    Array vector ->
      if V.null vector
        then MyEmpty
        else case head (V.toList vector) of
               String _ ->
                 MyStrings . map (\(String x) -> T.unpack x) $ V.toList vector
               Number _ ->
                 MyIntegers . map (\(Number x) -> scientificToInteger x) $
                 V.toList vector
               _ -> MyEmpty
    Object obj -> MyStrings . map T.unpack $ HM.keys obj
    Null -> MyEmpty

fromStr :: String -> MyValue
fromStr str =
  let valueStr = map toUpper str
   in chooseMyValue valueStr str

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

toValue :: MyValue -> Value
toValue myValue =
  case myValue of
    MyInteger num -> Number $ scientific num 0
    MyString str -> String $ T.pack str
    MyBool bool -> Bool bool
    MyIntegers arr -> Array . V.fromList $ map (Number . flip scientific 0) arr
    MyStrings arr -> Array . V.fromList $ map (String . T.pack) arr
    MyNextval val -> String $ T.pack val
    MyDate date -> String $ T.pack date
    MyEmpty -> Null
