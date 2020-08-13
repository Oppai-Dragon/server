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
import Data.Handler
import Data.Empty
import Data.Essence
import Data.Essence.Methods
import Data.Essence.Parse.Clause
import Data.MyValue
import Data.SQL
import Data.SQL.Actions
import Data.SQL.ToValue
    ( sqlValuesArrToValue )

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.Text           as T
import qualified Data.List           as L
import Database.HDBC
    ( disconnect
    , quickQuery'
    )
import Database.HDBC.PostgreSQL
    ( connectPostgreSQL
    , Connection
    )

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

dbGet :: StateT (Essence List) (ReaderT Config IO) Value
dbGet = do
    config <- lift $ ask
    addOffsetLimit
    essenceList <- get
    let getQuery = showSql essenceList
    let uriDB = getUri config
    conn <- lift . lift $ connectPostgreSQL uriDB
    sqlValues <- lift . lift $ quickQuery' conn getQuery []
    let pageObj = case sqlValuesArrToValue essenceList sqlValues config of
            Object obj -> obj
            _          -> HM.empty
    lift . lift $ disconnect conn
    case (name essenceList) of
        "news" -> (lift . nesteEssences) pageObj
            >>= pure
        _      -> pure $ Object pageObj

dbGetOne :: Essence List -> ReaderT Config IO Value
dbGetOne (EssenceList _     _      [(_,    MyEmpty )]) = return (Object HM.empty)
dbGetOne (EssenceList table action [pair]) = do
    config <- ask
    let uriDB = getUri config
    let getQuery = showSql $ Get table [Where pair]
    conn <- lift $ connectPostgreSQL uriDB
    sqlValuesArr <- lift $ quickQuery' conn getQuery []
    let value = sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
    lift $ disconnect conn
    pure value

dbGetArray :: Essence [(String,MyValue)] -> ReaderT Config IO Value
dbGetArray (EssenceList table action [(field,MyIntegers arr)]) = do
    config <- ask
    let uri = getUri config
    let myStrArray = map show arr
    conn <- lift $ connectPostgreSQL uri
    let wherePart = L.intercalate " OR " $ map (\x -> field <> "=" <> x) myStrArray
    let getQuery = showSql . Get table $ [Filter wherePart]
    sqlValuesArr <- lift $ quickQuery' conn getQuery []
    lift $ disconnect conn
    let value = sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
    pure value
dbGetArray _                                                 = return (Object HM.empty)

addOffsetLimit :: StateT (Essence List) (ReaderT Config IO) ()
addOffsetLimit = do
    psql <- fromStateT setPsql
    essenceList <- get
    let pageCounter = case lookup "page" (list essenceList) of
            Just (MyInteger num) -> fromInteger num
            Nothing              -> 1
    let offsetLimit = getOffsetLimit pageCounter psql
    modify $ deletePair "page"
    modify $ addList [("page",MyString offsetLimit)]

nesteEssences :: Object -> ReaderT Config IO Value
nesteEssences pageObj = do
    let essences = HM.keys pageObj
    objArr <- iterateObj essences pageObj
    return . Object $ HM.unions objArr

iterateObj :: [T.Text] -> Object -> ReaderT Config IO [Object]
iterateObj []             _       = return []
iterateObj (essence:rest) pageObj = do
    config <- ask
    api <- lift setApi
    let name = T.pack . takeWhile (not . isDigit) $ T.unpack essence
    let relationsFields =
            map (\(field,descr) -> (field,fromJust $ relationsOf descr)) .
            HM.toList .
            HM.filter (\descr -> case relationsOf descr of {Just _ -> True; _ -> False;}) .
            hashMapOf $ getEssenceDB name "get" config api
    nestedEssence <- case HM.lookup essence pageObj of
        Just (Object fieldsObj) ->
            execStateT (nesteEssence relationsFields) fieldsObj
        _                       -> return HM.empty
    (:) (HM.singleton essence $ Object nestedEssence) <$> iterateObj rest pageObj

nesteEssence :: [(String,Relations)] -> StateT Object (ReaderT Config IO) ()
nesteEssence []                                           = do
    fieldsObj <- get
    let table = "tag"
    let field = "id"
    case HM.lookup "tag_ids" fieldsObj of
        Just value ->
            lift (dbGetArray (EssenceList table "get" [(field, fromValue value)]))
            >>= \value -> modify (HM.delete "tag_ids") >>
            modify (HM.union (HM.singleton "tags" value))
        Nothing    -> return ()
nesteEssence ((field,Relations table tableField) : rest) = do
    fieldsObj <- get
    let listOfPair = case HM.lookup (T.pack field) fieldsObj of
            Just value -> [(T.unpack tableField, fromValue value)]
            Nothing    -> []
    (Object essenceObj) <- lift $ dbGetOne (EssenceList (T.unpack table) "get" listOfPair)
    (Object completeObj) <- lift $ nesteEssences essenceObj
    if HM.null completeObj
        then modify $ HM.union essenceObj
        else modify (HM.delete (T.pack field))
            >> modify (HM.union completeObj)
    nesteEssence rest