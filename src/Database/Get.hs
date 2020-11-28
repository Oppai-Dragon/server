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
import Data.Essence.Column
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
  Config.Handle {hConfig = config, hLogHandle = logHandle} <-
    liftUnderApp askUnderApp
  liftUnderApp . liftIO $ logDebug logHandle "Start dbGet"
  addOffsetLimit
  essenceList@(EssenceList {elName = name}) <- getSApp
  liftUnderApp . liftIO . logDebug logHandle $
    "Essence List: " <> show essenceList
  let getQuery = showSql essenceList
  let uriDB = getUri config
  maybeConn <- liftUnderApp . tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftUnderApp . liftIO $ HDBC.runRaw conn setEng
      liftUnderApp . liftIO . logDebug logHandle $ "PSQL request: " <> getQuery
      sqlValues <-
        liftUnderApp . tryQuickQuery $ HDBC.quickQuery' conn getQuery []
      pageObj <-
        liftUnderApp . liftIO $
        case sqlValuesArrToValue essenceList sqlValues config of
          A.Object obj -> do
            logInfo logHandle $ name <> " was getted"
            return obj
          _ -> do
            logInfo logHandle $ name <> " wasn't getted"
            return HM.empty
      liftUnderApp . liftIO $ HDBC.disconnect conn
      let endFunc = liftUnderApp . liftIO $ logDebug logHandle "End dbGet"
      liftUnderApp . liftIO . logDebug logHandle $ "Get " <> show pageObj
      case name of
        "news" ->
          (liftUnderApp . liftIO)
            (logDebug logHandle $ "Other enssencies are nested in the " <> name) >>
          (liftUnderApp . nesteEssences) pageObj >>= \value ->
            endFunc >> pure value
        _ -> endFunc >> pure (A.Object pageObj)

dbGetOne :: Essence List -> UnderApp A.Value
dbGetOne essenceList@(EssenceList {elName = table, elList = [pair]}) = do
  Config.Handle {hConfig = config, hLogHandle = logHandle} <- askUnderApp
  liftIO $ logDebug logHandle "Start dbGetOne"
  let uriDB = getUri config
  let getQuery = showSql $ Get table [Where pair]
  maybeConn <- tryConnect $ PSQL.connectPostgreSQL uriDB
  case maybeConn of
    Nothing -> return A.Null
    Just conn -> do
      liftIO $ HDBC.runRaw conn setEng
      liftIO . logDebug logHandle $ "PSQL request: " <> getQuery
      sqlValuesArr <- tryQuickQuery $ HDBC.quickQuery' conn getQuery []
      let value =
            sqlValuesArrToValue essenceList {elList = []} sqlValuesArr config
      liftIO $ HDBC.disconnect conn
      liftIO . logDebug logHandle $ "Get " <> show value
      liftIO $ logDebug logHandle "End dbGetOne"
      pure value
dbGetOne EssenceList {elList = list} = do
  Config.Handle {hLogHandle = logHandle} <- askUnderApp
  liftIO . logWarning logHandle $ "dbGetOne takes bad argumants : " <> show list
  return $ A.Object HM.empty

dbGetArray :: Essence List -> UnderApp A.Value
dbGetArray essenceList@(EssenceList { elName = table
                                    , elList = [(field, MyIntegerArr arr)]
                                    }) = do
  Config.Handle {hConfig = config, hLogHandle = logHandle} <- askUnderApp
  liftIO $ logDebug logHandle "Start dbGetArray"
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
      liftIO . logDebug logHandle $ "PSQL request: " <> getQuery
      sqlValuesArr <- tryQuickQuery $ HDBC.quickQuery' conn getQuery []
      liftIO $ HDBC.disconnect conn
      let value =
            sqlValuesArrToValue essenceList {elList = []} sqlValuesArr config
      liftIO . logDebug logHandle $ "Get " <> show value
      liftIO $ logDebug logHandle "End dbGetArray"
      pure value
dbGetArray EssenceList {elList = list} = do
  Config.Handle {hLogHandle = logHandle} <- askUnderApp
  liftIO . logWarning logHandle $
    "dbGetArray takes bad argumants : " <> show list
  return $ A.Object HM.empty

addOffsetLimit :: SApp ()
addOffsetLimit = do
  Config.Handle {hLogHandle = logHandle} <- liftUnderApp askUnderApp
  psql <- liftUnderApp $ liftIO setPsql
  essenceList <- getSApp
  let pageCounter =
        case lookup "page" (elList essenceList) of
          Just (MyInteger num) -> fromInteger num
          _ -> 1
  liftUnderApp . liftIO $ logDebug logHandle $ "Page is " <> show pageCounter
  let offsetLimit = getOffsetLimit pageCounter psql
  modifySApp $ deletePair "page"
  modifySApp $ addList [("page", MyString offsetLimit)]

nesteEssences :: A.Object -> UnderApp A.Value
nesteEssences pageObj = do
  Config.Handle {hLogHandle = logHandle} <- askUnderApp
  liftIO $ logDebug logHandle "Start nesteEssences"
  let essences = HM.keys pageObj
  liftIO $ logDebug logHandle $ "iterateObj " <> show essences
  objArr <- iterateObj essences pageObj
  liftIO $ logDebug logHandle "End nesteEssences"
  return . A.Object $ HM.unions objArr

iterateObj :: [T.Text] -> A.Object -> UnderApp [A.Object]
iterateObj [] _ = return []
iterateObj (essence:rest) pageObj = do
  Config.Handle {hConfig = config, hApi = api, hLogHandle = logHandle} <-
    askUnderApp
  let name = T.pack . takeWhile (not . isDigit) $ T.unpack essence
  let relationsFields =
        HM.toList .
        HM.map (fromJust . cRelations) .
        HM.filter
          (\x ->
             case cRelations x of
               Just _ -> True
               _ -> False) .
        eColHashMap $
        getEssenceColumn name "get" config api
  nestedEssence <-
    case HM.lookup essence pageObj of
      Just (A.Object fieldsObj) ->
        liftIO (logDebug logHandle $ T.unpack essence <> " is nesting") >>
        execSApp (nesteEssence relationsFields) fieldsObj
      _ -> return HM.empty
  (:) (HM.singleton essence $ A.Object nestedEssence) <$>
    iterateObj rest pageObj

nesteEssence :: [(T.Text, Relations)] -> StateT A.Object UnderApp ()
nesteEssence [] = do
  fieldsObj <- getSApp
  let table = "tag"
  let field = "id"
  case HM.lookup "tag_ids" fieldsObj of
    Just value ->
      liftUnderApp
        (dbGetArray
           EssenceList
             { elName = table
             , elAction = "get"
             , elList = [(field, fromValue value)]
             }) >>= \valueArr ->
        modifySApp (HM.delete "tag_ids") >>
        modifySApp (HM.union (HM.singleton "tags" valueArr))
    Nothing -> return ()
nesteEssence ((field, Relations table tableField):rest) = do
  fieldsObj <- getSApp
  let listOfPair =
        case HM.lookup field fieldsObj of
          Just value -> [(T.unpack tableField, fromValue value)]
          Nothing -> []
  (A.Object essenceObj) <-
    liftUnderApp $
    dbGetOne
      EssenceList
        {elName = (T.unpack table), elAction = "get", elList = listOfPair}
  (A.Object completeObj) <- liftUnderApp $ nesteEssences essenceObj
  if HM.null completeObj
    then modifySApp $ HM.union essenceObj
    else modifySApp (HM.delete field) >> modifySApp (HM.union completeObj)
  nesteEssence rest
