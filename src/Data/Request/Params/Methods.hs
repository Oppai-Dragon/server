module Data.Request.Params.Methods
    ( isRightParams
    , queryBSWithoutMaybe
    , isTypeParamsCorrect
    , compareValueType
    ) where

import Config
import Data.Base
import Data.Essence
import Data.Essence.Methods
import Data.Required
import Data.Required.Methods
import Data.MyValue


import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString     as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]
type QueryBS = [(BS.ByteString, BS.ByteString)]

isRightParams :: Essence DB -> QueryMBS -> Bool
isRightParams (EssenceDB name action hashMap) queryMBS =
    let
        queryBS = queryBSWithoutMaybe queryMBS
        iterateBS []     = []
        iterateBS arr@((field,valueBS):rest) =
            case HM.lookup (BSC8.unpack field) hashMap of
                Just _ -> iterateBS rest
                _      -> arr
    in case queryBS of
        [] -> True
        _  -> null $ iterateBS queryBS

queryBSWithoutMaybe :: QueryMBS -> QueryBS
queryBSWithoutMaybe []                = []
queryBSWithoutMaybe ((l,maybeR):rest) =
    case maybeR of
        Just valueBS -> (l,valueBS) : queryBSWithoutMaybe rest
        Nothing      -> queryBSWithoutMaybe rest

isTypeParamsCorrect :: Essence DB -> [(String,MyValue)] -> [Bool]
isTypeParamsCorrect _                                     []                     = []
isTypeParamsCorrect essence@(EssenceDB name action hashMap) ((field,myValue):rest) =
    case HM.lookup field hashMap of
        Just description -> compareValueType (valueTypeOf description) myValue
            : isTypeParamsCorrect essence rest
        Nothing          -> isTypeParamsCorrect essence rest

compareValueType :: MyValue -> MyValue -> Bool
compareValueType (MyInteger _) (MyInteger _)     = True
compareValueType (MyString _) (MyString _)       = True
compareValueType (MyIntegers _) (MyIntegers _)   = True
compareValueType (MyStrings _) (MyStrings _)     = True
compareValueType (MyBool _) (MyBool _)           = True
compareValueType (MyDate _) (MyDate _)           = True
compareValueType _     _                         = False