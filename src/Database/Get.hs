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
import Database.Exception
import Log

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
  (Config.Handle config _ _ logHandle) <- liftUnderApp askUnderApp
  liftUnderApp . liftIO $ debugM logHandle "Start dbGet"
  addOffsetLimit
  essenceList@(EssenceList name _ _) <- getSApp
  liftUnderApp . liftIO . debugM logHandle $ "Essence List: " <> show essenceList
  let getQuery = showSql essenceList
  let uriDB = getUri config
  maybeConn <- liftUnderApp . tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftUnderApp . liftIO $ HDBC.runRaw conn setEng
      liftUnderApp . liftIO . debugM logHandle $ "PSQL request: " <> getQuery
      sqlValues <-
        liftUnderApp . tryQuickQuery $ HDBC.quickQuery' conn getQuery []
      pageObj <-
        liftUnderApp . liftIO $
        case sqlValuesArrToValue essenceList sqlValues config of
          A.Object obj -> do
            infoM logHandle $ name <> " was getted"
            return obj
          _ -> do
            infoM logHandle $ name <> " wasn't getted"
            return HM.empty
      liftUnderApp . liftIO $ HDBC.disconnect conn
      let endFunc = liftUnderApp . liftIO $ debugM logHandle "End dbGet"
      liftUnderApp . liftIO . debugM logHandle $ "Get " <> show pageObj
      case name of
        "news" ->
          (liftUnderApp . liftIO)
            (debugM logHandle $ "Other enssencies are nested in the " <> name) >>
          (liftUnderApp . nesteEssences) pageObj >>= \value ->
            endFunc >> pure value
        _ -> endFunc >> pure (A.Object pageObj)

dbGetOne :: Essence List -> UnderApp A.Value
dbGetOne (EssenceList table action [pair]) = do
  (Config.Handle config _ _ logHandle) <- askUnderApp
  liftIO $ debugM logHandle "Start dbGetOne"
  let uriDB = getUri config
  let getQuery = showSql $ Get table [Where pair]
  maybeConn <- tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftIO $ HDBC.runRaw conn setEng
      liftIO . debugM logHandle $ "PSQL request: " <> getQuery
      sqlValuesArr <- tryQuickQuery $ HDBC.quickQuery' conn getQuery []
      let value =
            sqlValuesArrToValue
              (EssenceList table action [])
              sqlValuesArr
              config
      liftIO $ HDBC.disconnect conn
      liftIO . debugM logHandle $ "Get " <> show value
      liftIO $ debugM logHandle "End dbGetOne"
      pure value
dbGetOne (EssenceList _ _ list) = do
  (Config.Handle _ _ _ logHandle) <- askUnderApp
  liftIO . warningM logHandle $ "dbGetOne takes bad argumants : " <> show list
  return $ A.Object HM.empty

dbGetArray :: Essence List -> UnderApp A.Value
dbGetArray (EssenceList table action [(field, MyIntegers arr)]) = do
  (Config.Handle config _ _ logHandle) <- askUnderApp
  liftIO $ debugM logHandle "Start dbGetArray"
  let uri = getUri config
  let myStrArray = map show arr
  maybeConn <- tryConnect $ PSQL.connectPostgreSQL uri
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftIO $ HDBC.runRaw conn setEng
      let wherePart =
            L.intercalate " OR " $ map (\x -> field <> "=" <> x) myStrArray
      let getQuery = showSql . Get table $ [Filter wherePart]
      liftIO . debugM logHandle $ "PSQL request: " <> getQuery
      sqlValuesArr <- tryQuickQuery $ HDBC.quickQuery' conn getQuery []
      liftIO $ HDBC.disconnect conn
      let value =
            sqlValuesArrToValue
              (EssenceList table action [])
              sqlValuesArr
              config
      liftIO . debugM logHandle $ "Get " <> show value
      liftIO $ debugM logHandle "End dbGetArray"
      pure value
dbGetArray (EssenceList _ _ list) = do
  (Config.Handle _ _ _ logHandle) <- askUnderApp
  liftIO . warningM logHandle $ "dbGetArray takes bad argumants : " <> show list
  return $ A.Object HM.empty

addOffsetLimit :: SApp ()
addOffsetLimit = do
  (Config.Handle _ _ _ logHandle) <- liftUnderApp askUnderApp
  psql <- liftUnderApp $ liftIO setPsql
  essenceList <- getSApp
  let pageCounter =
        case lookup "page" (elList essenceList) of
          Just (MyInteger num) -> fromInteger num
          _ -> 1
  liftUnderApp . liftIO $ debugM logHandle $ "Page is " <> show pageCounter
  let offsetLimit = getOffsetLimit pageCounter psql
  modifySApp $ deletePair "page"
  modifySApp $ addList [("page", MyString offsetLimit)]

nesteEssences :: A.Object -> UnderApp A.Value
nesteEssences pageObj = do
  (Config.Handle _ _ _ logHandle) <- askUnderApp
  liftIO $ debugM logHandle "Start nesteEssences"
  let essences = HM.keys pageObj
  liftIO $ debugM logHandle $ "iterateObj " <> show essences
  objArr <- iterateObj essences pageObj
  liftIO $ debugM logHandle "End nesteEssences"
  return . A.Object $ HM.unions objArr

iterateObj :: [T.Text] -> A.Object -> UnderApp [A.Object]
iterateObj [] _ = return []
iterateObj (essence:rest) pageObj = do
  (Config.Handle config api _ logHandle) <- askUnderApp
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
        getEssenceColumn name "get" config api
  nestedEssence <-
    case HM.lookup essence pageObj of
      Just (A.Object fieldsObj) ->
        liftIO (debugM logHandle $ T.unpack essence <> " is nesting") >>
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
