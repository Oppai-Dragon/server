module Data.MyValue
    ( MyValue (..)
    , parseStrings
    , parseBool
    , bsToStr
    , myInteger
    , myString
    , myBool
    , myIntegers
    , myStrings
    , chooseMyValue
    , fromValue
    ) where

import Data.Aeson
import qualified Data.ByteString as BS  (ByteString)
import Data.ByteString.Char8 (unpack)
import           Data.Char              (isDigit, toUpper, toLower)

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
        letter = toUpper $ head str
        word   = map toLower $ tail str
    in letter : word

bsToStr :: BS.ByteString -> String
bsToStr = unpack
myInteger, myString, myBool, myIntegers, myStrings,myDate ::
    BS.ByteString -> MyValue
myInteger = MyInteger . read . bsToStr
myString = MyString . bsToStr
myBool = MyBool . read . parseBool . bsToStr
myDate = MyDate . bsToStr
myIntegers = MyIntegers . read . bsToStr
myStrings = MyStrings . read . parseStrings . bsToStr
myNextval = MyNextval . bsToStr

chooseMyValue :: BS.ByteString -> MyValue
chooseMyValue valueBS =
    let valueStr = map toUpper $ bsToStr valueBS
    in case valueStr of
        '[':x:xs ->
            if isDigit x
                then myIntegers valueBS
                else myStrings valueBS
        "FALSE"  -> myBool valueBS
        "TRUE"   -> myBool valueBS
        date@(x1:x2:x3:x4:'-':x5:x6:'-':x7:x8) -> myDate valueBS
        date@(x1:x2:'-':x3:x4:'-':x5:x6:x7:x8) -> myDate valueBS
        'N':'E':'X':'T':'V':'A':'L':'(':rest -> myNextval valueBS
        x:xs     ->
            if isDigit x
                then myInteger valueBS
                else myString valueBS

fromValue :: Value -> MyValue
fromValue = undefined