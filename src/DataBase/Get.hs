module DataBase.Get
    ( dbGet
    , dbGetOne
    ) where

import Config
import Data.Base
    ( ifElseThen )
import Data.Handler
import Data.Empty
import Data.Essence
import Data.Essence.Methods
import Data.MyValue
import qualified Data.SQL            as SQL
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
    essenceList@(EssenceList name action list) <- get
    let pageCounter = case lookup "page" list of
            Just num -> read num
            Nothing  -> 1
    let getQuery = init (show essenceList) <> " " <> getOffsetLimit pageCounter config
    let uriDB = getUri config
    let essence = T.pack name
    conn <- lift . lift $ connectPostgreSQL uriDB
    sqlValues <- lift . lift $ quickQuery' conn getQuery []
    let pageObj = case sqlValuesArrToValue essenceList sqlValues config of
            Object obj -> obj
            _          -> HM.empty
    lift . lift $ disconnect conn
    value <- lift $ nesteEssences pageObj
    pure value

dbGetOne :: Essence [(String,Value)] -> ReaderT Config IO Value
dbGetOne (EssenceValue _     _      [(_,    Null )]) = return (Object HM.empty)
dbGetOne (EssenceValue table action [(field,value)]) = do
    config <- ask
    let uriDB = getUri config
    let myValue = fromValue value
    let wherePart = field <> "=" <> (parseValue myValue)
    let getQuery = SQL.get table "*" wherePart
    conn <- lift $ connectPostgreSQL uriDB
    sqlValuesArr <- lift $ quickQuery' conn getQuery []
    let value = sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
    lift $ disconnect conn
    pure value

nesteEssences :: Object -> ReaderT Config IO Value
nesteEssences pageObj = do
    let essences = HM.keys pageObj
    objArr <- iterateObj essences pageObj
    return . Object $ HM.unions objArr

iterateObj :: [T.Text] -> Object -> ReaderT Config IO [Object]
iterateObj []             _       = return []
iterateObj (essence:rest) pageObj = do
    config <- ask
    let name = T.pack . takeWhile (not . isDigit) $ T.unpack essence
    let relationsFields =
            map (\(field,descr) -> (field,fromJust $ relationsOf descr)) .
            HM.toList .
            HM.filter (\descr -> case relationsOf descr of {Just _ -> True; _ -> False;}) .
            fieldsOf $ getEssenceDB name "get" config
    nestedEssence <- case HM.lookup essence pageObj of
        Just (Object fieldsObj) ->
            execStateT (nesteEssence relationsFields) fieldsObj
        _                       -> return HM.empty
    (:) (HM.singleton essence $ Object nestedEssence) <$> iterateObj rest pageObj

dbGetArray :: Essence [(String,Value)] -> ReaderT Config IO Value
dbGetArray (EssenceValue table action [(field,value)]) = do
    config <- ask
    let uri = getUri config
    conn <- lift $ connectPostgreSQL uri
    let wherePart = L.intercalate " OR " $ vectorToStrList (object ["arr" .= value])
    let getQuery = SQL.get table "*" wherePart
    sqlValuesArr <- lift $ quickQuery' conn getQuery []
    lift $ disconnect conn
    let value = sqlValuesArrToValue (EssenceList table action []) sqlValuesArr config
    pure value

vectorToStrList :: Value -> [String]
vectorToStrList (Object obj) =
    case parseMaybe (.: "arr") obj of
        Just arr -> arr
        _        -> []

nesteEssence :: [(String,Relations)] -> StateT Object (ReaderT Config IO) ()
nesteEssence []                                           = do
    fieldsObj <- get
    let table = "tag"
    let field = "id"
    let objWithoutField = HM.delete "tag_ids" fieldsObj
    case HM.lookup "tag_ids" fieldsObj of
        Just value ->
            lift (dbGetArray (EssenceValue table "get" [(field,value)]))
            >>= \value ->
            put (HM.union (HM.singleton "tags" value) objWithoutField)
        Nothing    -> return ()
nesteEssence ((field,Relations table tableField) : rest) = do
    fieldsObj <- get
    let listOfPair = case HM.lookup (T.pack field) fieldsObj of
            Just value -> [(T.unpack tableField,value)]
            Nothing    -> []
    (Object essenceObj) <- lift $ dbGetOne (EssenceValue (T.unpack table) "get" listOfPair)
    (Object completeObj) <- lift $ nesteEssences essenceObj
    let objWithoutField = HM.delete (T.pack field) fieldsObj
    if HM.null completeObj
        then put $ HM.union essenceObj fieldsObj
        else put $ HM.union completeObj objWithoutField
    nesteEssence rest