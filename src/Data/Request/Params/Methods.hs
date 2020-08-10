module Data.Request.Params.Methods
    ( isRequiredParams
    , iterateRequiredParams
    , iterateParams
    , queryBSWithoutMaybe
    , isParamsCorrect
    , isUniqueParams
    , isRightRelationsParams
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
import Database.Get

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString     as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class          (lift)

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]
type QueryBS = [(BS.ByteString, BS.ByteString)]

isRequiredParams :: Essence DB -> QueryMBS -> Api -> Bool
isRequiredParams (EssenceDB "news" "create" _) queryMBS _   =
    case lookup "id" queryMBS of
        Just (Just x) -> True
        _             -> False
isRequiredParams essenceDB                     queryMBS api =
    let
        queryBS = queryBSWithoutMaybe queryMBS
        requiredParams = getRequiredFields essenceDB api
    in case requiredParams of
        NullFields -> True
        _          -> iterateRequiredParams requiredParams queryBS

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

isParamsCorrect :: Essence DB -> [(String,MyValue)] -> WriterT [Bool] (ReaderT Config IO) ()
isParamsCorrect _                             []                                = tell [True]
isParamsCorrect (EssenceDB _     "get"    _)  _                                 = tell [True]
isParamsCorrect (EssenceDB _     "delete" _)  _                                 = tell [True]
isParamsCorrect (EssenceDB table action   hm) (("tag_ids",MyIntegers arr):rest) = do
    (Object pageObj) <- lift $ dbGetArray (EssenceList "tag" "get" [("id",MyIntegers arr)])
    let bool = length (HM.keys pageObj) == length arr
    tell [bool]
isParamsCorrect (EssenceDB table action   hm) ((field,myValue):rest)            =
    case HM.lookup field hm of
        Just description -> do
            isUniqueParams table (field,myValue) (constraintOf description)
            isRightRelationsParams (relationsOf description) myValue
            isParamsCorrect (EssenceDB table action hm) rest
        Nothing          -> isParamsCorrect (EssenceDB table action hm) rest

-- Tell True, if essence with unique value doesn't exist
isUniqueParams :: T.Text -> (String,MyValue) -> Maybe Constraint -> WriterT [Bool] (ReaderT Config IO) ()
isUniqueParams table pare (Just UNIQUE) = do
    let name = T.unpack table
    (Object obj) <- lift $ dbGetOne (EssenceList name "get" [pare])
    if HM.null obj
        then tell [True]
        else tell [False]
isUniqueParams _     _    _             = return ()

-- Tell True, if essence with dependent value exist
isRightRelationsParams :: Maybe Relations -> MyValue -> WriterT [Bool] (ReaderT Config IO) ()
isRightRelationsParams Nothing                         _       = return ()
isRightRelationsParams (Just (Relations table fieldT)) myValue = do
    let name = T.unpack table
    let field = T.unpack fieldT
    let pare = (field,myValue)
    (Object obj) <- lift $ dbGetOne (EssenceList name "get" [pare])
    if HM.null obj
        then tell [False]
        else tell [True]

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