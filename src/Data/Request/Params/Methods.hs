module Data.Request.Params.Methods
    where

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

isRequiredParams :: Essence DB -> QueryMBS -> Bool
isRequiredParams essenceDB queryMBS =
    let
        queryBS = queryBSWithoutMaybe queryMBS
        requiredParams = getRequiredFields essenceDB
    in case queryBS of
        [] -> True
        _  -> iterateRequiredParams requiredParams queryBS

iterateRequiredParams :: Required [String] -> QueryBS -> Bool
iterateRequiredParams params queryBS =
    let toBSArr = map (TE.encodeUtf8 . T.pack)
    in case params of
        NullFields                  -> True
        AND params                  ->
            and $ iterateParams (toBSArr params) queryBS
        OR  params                  ->
            or $ iterateParams (toBSArr params) queryBS
        Required requiredArr        ->
            and $
            map (\x -> iterateRequiredParams x queryBS)
            requiredArr
        _                           -> False

iterateParams :: [BS.ByteString] -> QueryBS -> [Bool]
iterateParams []             _       = []
iterateParams (param:params) queryBS =
    let isHere =
            case lookup param queryBS of
                Just value -> True
                _          -> False
    in isHere : iterateParams params queryBS

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