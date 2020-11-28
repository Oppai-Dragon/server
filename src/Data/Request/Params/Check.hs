module Data.Request.Params.Check
  ( isRequiredParams
  , iterateRequiredParams
  , iterateParams
  , queryBSWithoutMaybe
  , isConstraintCorrect
  , isCorrectLengthText
  , isUniqueParams
  , isRightRelationsParams
  , isTypeParamsCorrect
  , compareValueType
  ) where

import Config
import Data.Base
import Data.Essence
import Data.Essence.Column
import Data.MyValue
import Data.Required
import Database.Get

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]

type QueryBS = [(BS.ByteString, BS.ByteString)]

isRequiredParams :: Essence Column -> QueryMBS -> Api -> Bool
isRequiredParams EssenceColumn {eColName = "news", eColAction = "create"} queryMBS _ =
  case lookup "id" queryMBS of
    Just (Just _) -> True
    _ -> False
isRequiredParams essenceColumn queryMBS api =
  let queryBS = queryBSWithoutMaybe queryMBS
      requiredParams = getRequiredFields essenceColumn api
   in case requiredParams of
        NullFields -> True
        _ -> iterateRequiredParams requiredParams queryBS

iterateRequiredParams :: Required [String] -> QueryBS -> Bool
iterateRequiredParams required queryBS =
  let toBSArr = map (TE.encodeUtf8 . T.pack)
   in case required of
        NullFields -> True
        AND params -> and $ iterateParams (toBSArr params) queryBS
        OR params -> or $ iterateParams (toBSArr params) queryBS
        Required requiredArr ->
          all (`iterateRequiredParams` queryBS) requiredArr

iterateParams :: [BS.ByteString] -> QueryBS -> [Bool]
iterateParams [] _ = []
iterateParams (param:params) queryBS =
  let isHere =
        case lookup param queryBS of
          Just _ -> True
          _ -> False
   in isHere : iterateParams params queryBS

queryBSWithoutMaybe :: QueryMBS -> QueryBS
queryBSWithoutMaybe [] = []
queryBSWithoutMaybe ((l, maybeR):rest) =
  case maybeR of
    Just valueBS -> (l, valueBS) : queryBSWithoutMaybe rest
    Nothing -> queryBSWithoutMaybe rest

isConstraintCorrect :: Essence Column -> [(String, MyValue)] -> WApp ()
isConstraintCorrect _ [] = tellWApp $ All True
isConstraintCorrect EssenceColumn {eColAction = "get"} _ = tellWApp $ All True
isConstraintCorrect EssenceColumn {eColAction = "delete"} _ =
  tellWApp $ All True
isConstraintCorrect _ (("tag_ids", MyIntegerArr arr):_) = do
  (A.Object pageObj) <-
    liftUnderApp $
    dbGetArray
      EssenceList
        {elName = "tag", elAction = "get", elList = [("id", MyIntegerArr arr)]}
  let bool = length (HM.keys pageObj) == length arr
  tellWApp $ All bool
isConstraintCorrect essenceColumn@(EssenceColumn { eColName = table
                                                 , eColHashMap = hm
                                                 }) ((field, myValue):rest) =
  case HM.lookup (T.pack field) hm of
    Just column -> do
      isCorrectLengthText (cValueType column) myValue
      isUniqueParams table (field, myValue) (cConstraint column)
      isRightRelationsParams (cRelations column) myValue
      isConstraintCorrect essenceColumn rest
    Nothing -> isConstraintCorrect essenceColumn rest

isCorrectLengthText :: ValueType -> MyValue -> WApp ()
isCorrectLengthText valueType (MyString str) =
  let tellBool x = tellWApp . All $ x >= length str
   in case valueType of
        CHAR len -> tellBool len
        VARCHAR len -> tellBool len
        _ -> return ()
isCorrectLengthText valueType (MyStringArr arr) =
  let tellBool x = tellWApp . All $ all ((>=) x . length) arr
   in case valueType of
        CHAR_ARR len -> tellBool len
        VARCHAR_ARR len -> tellBool len
        _ -> return ()
isCorrectLengthText _ _ = return ()

-- Tell True, if essence with unique value doesn't exist
isUniqueParams :: T.Text -> (String, MyValue) -> Maybe Constraint -> WApp ()
isUniqueParams table pare (Just UNIQUE) = do
  let name = T.unpack table
  A.Object obj <-
    liftUnderApp $
    dbGetOne EssenceList {elName = name, elAction = "get", elList = [pare]}
  tellWApp . All $ HM.null obj
isUniqueParams _ _ _ = return ()

-- Tell True, if essence with dependent value exist
isRightRelationsParams :: Maybe Relations -> MyValue -> WApp ()
isRightRelationsParams Nothing _ = return ()
isRightRelationsParams (Just (Relations table fieldT)) myValue = do
  let name = T.unpack table
  let field = T.unpack fieldT
  let pare = (field, myValue)
  A.Object obj <-
    liftUnderApp $
    dbGetOne EssenceList {elName = name, elAction = "get", elList = [pare]}
  tellWApp . All . not $ HM.null obj

isTypeParamsCorrect :: Essence Column -> [(String, MyValue)] -> All
isTypeParamsCorrect _ [] = All True
isTypeParamsCorrect essenceColumn@(EssenceColumn {eColHashMap = hashMap}) ((field, myValue):rest) =
  case HM.lookup (T.pack field) hashMap of
    Just column ->
      All (compareValueType (cValueType column) myValue) <>
      isTypeParamsCorrect essenceColumn rest
    Nothing -> isTypeParamsCorrect essenceColumn rest

compareValueType :: ValueType -> MyValue -> Bool
compareValueType valueType (MyInteger _) =
  case valueType of
    SERIAL -> True
    BIGSERIAL -> True
    SMALLINT -> True
    BIGINT -> True
    INTEGER -> True
    INT -> True
    _ -> False
compareValueType valueType (MyString _) =
  case valueType of
    CHAR _ -> True
    VARCHAR _ -> True
    TEXT -> True
    UUID -> True
    _ -> False
compareValueType valueType (MyIntegerArr _) =
  case valueType of
    SMALLINT_ARR -> True
    BIGINT_ARR -> True
    INTEGER_ARR -> True
    INT_ARR -> True
    _ -> False
compareValueType valueType (MyStringArr _) =
  case valueType of
    CHAR_ARR _ -> True
    VARCHAR_ARR _ -> True
    TEXT_ARR -> True
    _ -> False
compareValueType BOOLEAN (MyBool _) = True
compareValueType BOOLEAN_ARR (MyBoolArr _) = True
compareValueType DATE (MyDate _) = True
compareValueType DATE_ARR (MyDateArr _) = True
compareValueType valueType (MyUri _) =
  case valueType of
    CHAR _ -> True
    VARCHAR _ -> True
    TEXT -> True
    _ -> False
compareValueType valueType (MyNextval _) =
  case valueType of
    SERIAL -> True
    BIGSERIAL -> True
    _ -> False
compareValueType _ _ = False
