module Database.Get
  ( dbGet
  , dbGetOne
  , dbGetArray
  , addOffsetLimit
  , nesteEssences
  , iterateObj
  , nesteEssence
  ) where

import Config
import Data.Base hiding (deletePair)
import Data.Essence
import Data.Essence.Methods
import Data.MyValue
import Data.SQL
import Data.SQL.ShowSql
import Data.SQL.ToValue (sqlValuesArrToValue)

import qualified Data.Aeson as A
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PSQL

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

dbGet :: StateT (Essence List) (ReaderT Config IO) A.Value
dbGet = do
  config <- lift $ ask
  addOffsetLimit
  essenceList <- get
  let getQuery = showSql essenceList
  let uriDB = getUri config
  conn <- lift . lift $ PSQL.connectPostgreSQL uriDB
  sqlValues <- lift . lift $ HDBC.quickQuery' conn getQuery []
  let pageObj =
        case sqlValuesArrToValue essenceList sqlValues config of
          A.Object obj -> obj
          _ -> HM.empty
  lift . lift $ HDBC.disconnect conn
  case elName essenceList of
    "news" -> (lift . nesteEssences) pageObj >>= pure
    _ -> pure $ A.Object pageObj

dbGetOne :: Essence List -> ReaderT Config IO A.Value
dbGetOne (EssenceList _ _ [(_, MyEmpty)]) = return (A.Object HM.empty)
dbGetOne (EssenceList table action [pair]) = do
  config <- ask
  let uriDB = getUri config
  let getQuery = showSql $ Get table [Where pair]
  conn <- lift $ PSQL.connectPostgreSQL uriDB
  sqlValuesArr <- lift $ HDBC.quickQuery' conn getQuery []
  let value =
        sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
  lift $ HDBC.disconnect conn
  pure value
dbGetOne _ = return (A.Object HM.empty)

dbGetArray :: Essence [(String, MyValue)] -> ReaderT Config IO A.Value
dbGetArray (EssenceList table action [(field, MyIntegers arr)]) = do
  config <- ask
  let uri = getUri config
  let myStrArray = map show arr
  conn <- lift $ PSQL.connectPostgreSQL uri
  let wherePart = L.intercalate " OR " $ map (\x -> field <> "=" <> x) myStrArray
  let getQuery = showSql . Get table $ [Filter wherePart]
  sqlValuesArr <- lift $ HDBC.quickQuery' conn getQuery []
  lift $ HDBC.disconnect conn
  let value =
        sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
  pure value
dbGetArray _ = return (A.Object HM.empty)

addOffsetLimit :: StateT (Essence List) (ReaderT Config IO) ()
addOffsetLimit = do
  psql <- fromStateT setPsql
  essenceList <- get
  let pageCounter =
        case lookup "page" (elList essenceList) of
          Just (MyInteger num) -> fromInteger num
          _ -> 1
  let offsetLimit = getOffsetLimit pageCounter psql
  modify $ deletePair "page"
  modify $ addList [("page", MyString offsetLimit)]

nesteEssences :: A.Object -> ReaderT Config IO A.Value
nesteEssences pageObj = do
  let essences = HM.keys pageObj
  objArr <- iterateObj essences pageObj
  return . A.Object $ HM.unions objArr

iterateObj :: [T.Text] -> A.Object -> ReaderT Config IO [A.Object]
iterateObj [] _ = return []
iterateObj (essence:rest) pageObj = do
  config <- ask
  api <- lift setApi
  let name = T.pack . takeWhile (not . isDigit) $ T.unpack essence
  let relationsFields =
        map (\(field, descr) -> (field, fromJust $ dRelations descr)) .
        HM.toList .
        HM.filter
          (\descr ->
             case dRelations descr of
               Just _ -> True
               _ -> False) .
        edbHashmap $
        getEssenceDB name "get" config api
  nestedEssence <-
    case HM.lookup essence pageObj of
      Just (A.Object fieldsObj) ->
        execStateT (nesteEssence relationsFields) fieldsObj
      _ -> return HM.empty
  (:) (HM.singleton essence $ A.Object nestedEssence) <$>
    iterateObj rest pageObj

nesteEssence :: [(String, Relations)] -> StateT A.Object (ReaderT Config IO) ()
nesteEssence [] = do
  fieldsObj <- get
  let table = "tag"
  let field = "id"
  case HM.lookup "tag_ids" fieldsObj of
    Just value ->
      lift (dbGetArray (EssenceList table "get" [(field, fromValue value)])) >>= \valueArr ->
        modify (HM.delete "tag_ids") >>
        modify (HM.union (HM.singleton "tags" valueArr))
    Nothing -> return ()
nesteEssence ((field, Relations table tableField):rest) = do
  fieldsObj <- get
  let listOfPair =
        case HM.lookup (T.pack field) fieldsObj of
          Just value -> [(T.unpack tableField, fromValue value)]
          Nothing -> []
  (A.Object essenceObj) <-
    lift $ dbGetOne (EssenceList (T.unpack table) "get" listOfPair)
  (A.Object completeObj) <- lift $ nesteEssences essenceObj
  if HM.null completeObj
    then modify $ HM.union essenceObj
    else modify (HM.delete (T.pack field)) >> modify (HM.union completeObj)
  nesteEssence rest
