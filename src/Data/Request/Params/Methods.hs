module Data.Request.Params.Methods
  ( isRequiredParams
  , iterateRequiredParams
  , iterateParams
  , queryBSWithoutMaybe
  , isConstraintCorrect
  , isUniqueParams
  , isRightRelationsParams
  , isTypeParamsCorrect
  , compareValueType
  ) where

import Config
import Data.Essence
import Data.MyValue
import Data.Required
import Database.Get

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.CPS

type QueryMBS = [(BS.ByteString, Maybe BS.ByteString)]

type QueryBS = [(BS.ByteString, BS.ByteString)]

isRequiredParams :: Essence DB -> QueryMBS -> Api -> Bool
isRequiredParams (EssenceDB "news" "create" _) queryMBS _ =
  case lookup "id" queryMBS of
    Just (Just _) -> True
    _ -> False
isRequiredParams essenceDB queryMBS api =
  let queryBS = queryBSWithoutMaybe queryMBS
      requiredParams = getRequiredFields essenceDB api
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

isConstraintCorrect ::
     Essence DB -> [(String, MyValue)] -> WriterT All (ReaderT Config IO) ()
isConstraintCorrect _ [] = tell $ All True
isConstraintCorrect (EssenceDB _ "get" _) _ = tell $ All True
isConstraintCorrect (EssenceDB _ "delete" _) _ = tell $ All True
isConstraintCorrect EssenceDB {} (("tag_ids", MyIntegers arr):_) = do
  (A.Object pageObj) <-
    lift $ dbGetArray (EssenceList "tag" "get" [("id", MyIntegers arr)])
  let bool = length (HM.keys pageObj) == length arr
  tell $ All bool
isConstraintCorrect (EssenceDB table action hm) ((field, myValue):rest) =
  case HM.lookup field hm of
    Just description -> do
      isUniqueParams table (field, myValue) (dConstraint description)
      isRightRelationsParams (dRelations description) myValue
      isConstraintCorrect (EssenceDB table action hm) rest
    Nothing -> isConstraintCorrect (EssenceDB table action hm) rest

-- Tell True, if essence with unique value doesn't exist
isUniqueParams ::
     T.Text
  -> (String, MyValue)
  -> Maybe Constraint
  -> WriterT All (ReaderT Config IO) ()
isUniqueParams table pare (Just UNIQUE) = do
  let name = T.unpack table
  (A.Object obj) <- lift $ dbGetOne (EssenceList name "get" [pare])
  tell . All $ HM.null obj
isUniqueParams _ _ _ = return ()

-- Tell True, if essence with dependent value exist
isRightRelationsParams ::
     Maybe Relations -> MyValue -> WriterT All (ReaderT Config IO) ()
isRightRelationsParams Nothing _ = return ()
isRightRelationsParams (Just (Relations table fieldT)) myValue = do
  let name = T.unpack table
  let field = T.unpack fieldT
  let pare = (field, myValue)
  (A.Object obj) <- lift $ dbGetOne (EssenceList name "get" [pare])
  tell . All . not $ HM.null obj

isTypeParamsCorrect :: Essence DB -> [(String, MyValue)] -> All
isTypeParamsCorrect _ [] = All True
isTypeParamsCorrect essence@(EssenceDB _ _ hashMap) ((field, myValue):rest) =
  case HM.lookup field hashMap of
    Just description ->
      All (compareValueType (dValueType description) myValue) <>
      isTypeParamsCorrect essence rest
    Nothing -> isTypeParamsCorrect essence rest

compareValueType :: MyValue -> MyValue -> Bool
compareValueType (MyInteger _) (MyInteger _) = True
compareValueType (MyString _) (MyString _) = True
compareValueType (MyIntegers _) (MyIntegers _) = True
compareValueType (MyStrings _) (MyStrings _) = True
compareValueType (MyBool _) (MyBool _) = True
compareValueType (MyDate _) (MyDate _) = True
compareValueType _ _ = False
