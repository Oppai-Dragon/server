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

dbGet :: SApp A.Value
dbGet = do
  (Config.Handle config _ _) <- liftUnderApp askUnderApp
  addOffsetLimit
  essenceList <- getSApp
  let getQuery = showSql essenceList
  let uriDB = getUri config
  conn <- liftUnderApp . liftIO $ PSQL.connectPostgreSQL uriDB
  sqlValues <- liftUnderApp . liftIO $ HDBC.quickQuery' conn getQuery []
  let pageObj =
        case sqlValuesArrToValue essenceList sqlValues config of
          A.Object obj -> obj
          _ -> HM.empty
  liftUnderApp . liftIO $ HDBC.disconnect conn
  case elName essenceList of
    "news" -> (liftUnderApp . nesteEssences) pageObj >>= pure
    _ -> pure $ A.Object pageObj

dbGetOne :: Essence List -> UnderApp A.Value
dbGetOne (EssenceList _ _ [(_, MyEmpty)]) = return (A.Object HM.empty)
dbGetOne (EssenceList table action [pair]) = do
  (Config.Handle config _ _) <- askUnderApp
  let uriDB = getUri config
  let getQuery = showSql $ Get table [Where pair]
  conn <- liftIO $ PSQL.connectPostgreSQL uriDB
  sqlValuesArr <- liftIO $ HDBC.quickQuery' conn getQuery []
  let value =
        sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
  liftIO $ HDBC.disconnect conn
  pure value
dbGetOne _ = return (A.Object HM.empty)

dbGetArray :: Essence [(String, MyValue)] -> UnderApp A.Value
dbGetArray (EssenceList table action [(field, MyIntegers arr)]) = do
  (Config.Handle config _ _) <- askUnderApp
  let uri = getUri config
  let myStrArray = map show arr
  conn <- liftIO $ PSQL.connectPostgreSQL uri
  let wherePart =
        L.intercalate " OR " $ map (\x -> field <> "=" <> x) myStrArray
  let getQuery = showSql . Get table $ [Filter wherePart]
  sqlValuesArr <- liftIO $ HDBC.quickQuery' conn getQuery []
  liftIO $ HDBC.disconnect conn
  let value =
        sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
  pure value
dbGetArray _ = return (A.Object HM.empty)

addOffsetLimit :: SApp ()
addOffsetLimit = do
  psql <- liftUnderApp $ liftIO setPsql
  essenceList <- getSApp
  let pageCounter =
        case lookup "page" (elList essenceList) of
          Just (MyInteger num) -> fromInteger num
          _ -> 1
  let offsetLimit = getOffsetLimit pageCounter psql
  modifySApp $ deletePair "page"
  modifySApp $ addList [("page", MyString offsetLimit)]

nesteEssences :: A.Object -> UnderApp A.Value
nesteEssences pageObj = do
  let essences = HM.keys pageObj
  objArr <- iterateObj essences pageObj
  return . A.Object $ HM.unions objArr

iterateObj :: [T.Text] -> A.Object -> UnderApp [A.Object]
iterateObj [] _ = return []
iterateObj (essence:rest) pageObj = do
  (Config.Handle config _ _) <- askUnderApp
  api <- liftIO setApi
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
        execSApp (nesteEssence relationsFields) fieldsObj
      _ -> return HM.empty
  (:) (HM.singleton essence $ A.Object nestedEssence) <$>
    iterateObj rest pageObj

nesteEssence :: [(String, Relations)] -> S A.Object UnderApp ()
nesteEssence [] = do
  fieldsObj <- getSApp
  let table = "tag"
  let field = "id"
  case HM.lookup "tag_ids" fieldsObj of
    Just value ->
      liftUnderApp
        (dbGetArray (EssenceList table "get" [(field, fromValue value)])) >>= \valueArr ->
        modifySApp (HM.delete "tag_ids") >>
        modifySApp (HM.union (HM.singleton "tags" valueArr))
    Nothing -> return ()
nesteEssence ((field, Relations table tableField):rest) = do
  fieldsObj <- getSApp
  let listOfPair =
        case HM.lookup (T.pack field) fieldsObj of
          Just value -> [(T.unpack tableField, fromValue value)]
          Nothing -> []
  (A.Object essenceObj) <-
    liftUnderApp $ dbGetOne (EssenceList (T.unpack table) "get" listOfPair)
  (A.Object completeObj) <- liftUnderApp $ nesteEssences essenceObj
  if HM.null completeObj
    then modifySApp $ HM.union essenceObj
    else modifySApp (HM.delete (T.pack field)) >>
         modifySApp (HM.union completeObj)
  nesteEssence rest
