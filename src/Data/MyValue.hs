module Data.MyValue
    ( MyValue (..)
    , fromArray
    , parseStrings
    , parseBool
    , fromBS
    , myInteger
    , myString
    , myBool
    , myIntegers
    , myStrings
    , fromBS
    , fromValue
    , toValue
    , fromString
    , toString
    , scientificToInteger
    ) where

import Data.Aeson
import Data.Scientific
import qualified Data.Vector           as V
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text             as T
import qualified Data.Char             as C
import qualified Data.List             as L

data MyValue
    = MyString      String
    | MyStrings     [String]
    | MyInteger     Integer
    | MyIntegers    [Integer]
    | MyBool        Bool
    | MyNextval     String
    | MyDate        String
    | MyEmpty
    deriving (Show, Eq)
instance Read MyValue where
    readsPrec _ input = case input of
        "int"           -> [(MyInteger 0,"")]
        "array int"     -> [(MyIntegers [],"")]
        "string"        -> [(MyString [],"")]
        "array string"  -> [(MyStrings [],"")]
        "date"          -> [(MyString [],"")]
        "bool"          -> [(MyDate [],"")]
        "uuid"          -> [(MyString [],"")]

unpack :: Read a => MyValue -> a
unpack = read . toString

fromArray :: [MyValue] -> MyValue
fromArray myValueArr = case myValueArr of
    MyString _ : rest  -> MyStrings  $ map unpack myValueArr
    MyInteger _ : rest -> MyIntegers $ map unpack myValueArr
    _                  -> MyEmpty

parseStrings :: String -> String
parseStrings [] = []
parseStrings (x:xs) =
    case x of
        '\"' -> parseStrings xs
        '['  -> "[\"" <> parseStrings xs
        ','  -> "\",\"" <> parseStrings xs
        ']'  -> "\"]" <> parseStrings xs
        _    -> x : parseStrings xs

parseBool :: String -> String
parseBool str =
    let
        letter = C.toUpper $ head str
        word   = map C.toLower $ tail str
    in letter : word

myInteger, myString, myBool, myIntegers, myStrings,myDate ::
    String -> MyValue
myInteger = MyInteger . read
myString = MyString
myBool = MyBool . read . parseBool
myDate = MyDate
myIntegers = MyIntegers . read
myStrings = MyStrings . read . parseStrings
myNextval = MyNextval

chooseMyValue :: String -> (String -> MyValue)
chooseMyValue str = case str of
    '[':x:xs ->
        if C.isDigit x
            then myIntegers
            else myStrings
    "FALSE"  -> myBool
    "TRUE"   -> myBool
    date@(x1:x2:x3:x4:'-':x5:x6:'-':x7:x8) -> myDate
    date@(x1:x2:'-':x3:x4:'-':x5:x6:x7:x8) -> myDate
    'N':'E':'X':'T':'V':'A':'L':'(':rest -> myNextval
    x:xs     ->
        if C.isDigit x
            then myInteger
            else myString

fromBS :: BS.ByteString -> MyValue
fromBS valueBS =
    let valueStr = map C.toUpper $ BSC8.unpack valueBS
    in chooseMyValue valueStr (BSC8.unpack valueBS)

fromValue :: Value -> MyValue
fromValue value = case value of
    Number num   -> MyInteger $ scientificToInteger (Number num)
    String text  -> MyString $ T.unpack text
    Bool bool    -> MyBool bool
    Array vector -> fromArray . map fromValue $ V.toList vector

fromString :: String -> MyValue
fromString str =
    let valueStr = map C.toUpper str
    in chooseMyValue valueStr str

toString :: MyValue -> String
toString myValue = case myValue of
    MyInteger num     -> show num
    MyString str      -> str
    MyBool bool       -> show bool
    MyIntegers intArr -> show intArr
    MyStrings strArr  -> show strArr
    MyNextval val     -> val
    MyDate date       -> date
    MyEmpty           -> ""

toValue :: MyValue -> Value
toValue myValue = case myValue of
    MyInteger num -> Number $ scientific num 0
    MyString str  -> String $ T.pack str
    MyBool bool   -> Bool bool

scientificToInteger :: Value -> Integer
scientificToInteger (Number num) =
    let
        numStr = show num
        exponenta = 10 ^ (read . tail . dropWhile (/='e')) numStr
        division = toInteger $ 10 ^ (length . tail . dropWhile (/='.') . takeWhile (/='e')) numStr
    in case L.find (=='e') (show num) of
        Just _  ->
            case last $ takeWhile (/='e') numStr of
                '0' -> (read . takeWhile (/='.')) numStr * exponenta
                _ -> (read . L.delete '.' . takeWhile (/='e')) numStr * exponenta `div` division
        Nothing -> read $ takeWhile (/='.') numStr