module Data.Essence.RelationsTree.Methods
    ( isEssenceRelations
    , updateRelationsFields
    , relationsHandler
    , iterateRelations
    , findEssence
    , unpackLeafs
    , beforeUnderscore
    , parseObjEssence
    , afterUnderscore
    , getListOfPairFromObj
    , checkList
    , getNextField
    , isRightRelations
    , getIdPairFromObj
    , ifExisteAddEssenceId
    ) where

import Prelude hiding (null, tail, takeWhile, dropWhile)

import Config
import Data.Base
import Data.SQL.Actions
import Data.Essence
import Data.Essence.RelationsTree
import Data.Essence.Methods
import Data.Empty
import qualified Data.MyValue           as MyValue
import qualified Data.Value             as Value
import DataBase.Get

import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.HashMap.Strict    as HM

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

type QueryBS = [(BS.ByteString, Maybe BS.ByteString)]
type Name   = T.Text
type Field  = T.Text
type Fields = [Field]

isEssenceRelations :: Name -> Api -> Bool
isEssenceRelations essence api =
    let relations = getRelationsTree essence api
    in case relations of
        Root r (Trunk t rlt)               -> True
        Root r (Branch b (Leaf l : leafs)) -> True
        _                                  -> False

updateRelationsFields :: StateT (Essence List) (ReaderT Config IO) Object
updateRelationsFields = do
    (EssenceList name action list) <- get
    config <- lift ask
    let essence = T.pack name
    if isEssenceRelations essence config
        then relationsHandler essence
        else return $ HM.singleton "result" (Number 1)

relationsHandler :: Name -> StateT (Essence List) (ReaderT Config IO) Object
relationsHandler name = do
    api <- lift . lift $ setApi
    essenceList <- get
    let relations = getRelationsTree name api
    let listOfPair field obj = case HM.lookup field obj of
            Just value -> [(T.unpack field, MyValue.fromValue value)]
            Nothing    -> []
    case relations of
        Root rEssence trunk@(Trunk tEssence rlt)            ->
            do
                obj1 <- relationsHandler (beforeUnderscore rEssence)
                (Object obj2) <- lift $ findEssence tEssence (listOfPair rEssence obj1)
                iterateRelations trunk obj2
        Root rEssence branch@(Branch bEssence leafs)        ->
            do
                obj1 <- relationsHandler (beforeUnderscore rEssence)
                iterateRelations (Trunk rEssence branch) obj1
        Root rEssence (Leaf key)                            ->
            case lookup (T.unpack key) (list essenceList) of
                (Just accessKey) -> do
                    (Object obj) <- lift $ dbGetOne (EssenceList (T.unpack rEssence) "get" [(T.unpack key, accessKey)])
                    return obj
                _              -> return HM.empty
        _                                                   -> return HM.empty

iterateRelations :: RelationsTree -> Object -> StateT (Essence List) (ReaderT Config IO) Object
iterateRelations (Trunk t (Branch b leafs)) objOld = do
    essenceList@(EssenceList name action list) <- get
    let listOfPair = checkList t list
    case listOfPair of
        [(field, value)] ->
            case b of
                "news" -> do
                    (Object objNew) <- lift $ dbGetOne (EssenceList (T.unpack $ beforeUnderscore t) "get" listOfPair)
                    if isRightRelations objOld objNew t b
                        then put (addList
                            (unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objNew)
                            essenceList) >> return objNew
                        else return HM.empty
                _      -> do
                    (Object objNew) <- lift $ dbGetOne (EssenceList (T.unpack b) "get" [(field, value)])
                    if isRightRelations objOld objNew t b
                        then put (addList
                            (unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld)
                            essenceList) >> return objNew
                        else return HM.empty
        []                   ->
            put (addList (unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld) essenceList)
            >> return HM.empty
iterateRelations (Trunk t1 trunk@(Trunk t2 rlt)) objOld = do
    (Object objNew) <- lift $ findEssence t2 (getListOfPairFromObj t1 objOld)
    iterateRelations trunk objNew

findEssence :: Name -> [(String,MyValue.MyValue)] -> ReaderT Config IO Value
findEssence name listOfPair =
    case listOfPair of
        [(field, value)] ->
            dbGetOne (EssenceList (T.unpack $ beforeUnderscore name) "get" listOfPair)
        []                    ->
            return . Object . HM.singleton "result" $ Number 0

unpackLeafs :: Name -> [RelationsTree] -> Object -> [(String, MyValue.MyValue)]
unpackLeafs _       []            _   = []
unpackLeafs root (Leaf field :rest) obj =
    let
        key = afterUnderscore field
        parseFunc = parseFieldsFunc [root,key]
    in case parseMaybe parseFunc obj of
        Just value -> (T.unpack field, MyValue.fromValue value) : unpackLeafs root rest obj
        Nothing    -> unpackLeafs root rest obj

beforeUnderscore :: Name -> Name
beforeUnderscore = T.takeWhile (/='_')

parseObjEssence :: Name -> Name
parseObjEssence name = name <> "1"

afterUnderscore :: Name -> Field
afterUnderscore = T.tail . T.dropWhile (/='_')

getListOfPairFromObj :: Field -> Object -> [(String,MyValue.MyValue)]
getListOfPairFromObj field obj =
    let
        key1 = parseObjEssence $ beforeUnderscore field
        key2 = afterUnderscore field
        parseFunc = parseFieldsFunc [key1,key2]
    in case parseMaybe parseFunc obj of
        Just value -> [(T.unpack key2, MyValue.fromValue value)]
        Nothing    -> []

checkList :: Field -> [(String,MyValue.MyValue)] -> [(String,MyValue.MyValue)]
checkList field listOfPair =
    let
        findKey = T.unpack . afterUnderscore
        key = findKey field
    in case lookup key listOfPair of
        (Just value) -> [(key, value)]
        Nothing      -> []

getNextField :: RelationsTree a -> a
getNextField relations =
    case relations of
        Root r (Branch b leafs)  -> b
        Root r (Trunk t rlt)     -> t
        Trunk t1 (Trunk t2 rlt)  -> t2
        Trunk t (Branch b leafs) -> b
        Branch b l               -> b

isRightRelations :: Object -> Object -> Name -> Name -> Bool
isRightRelations rootObj branchObj rootEssence "news"        =
    let
        k1 = beforeUnderscore rootEssence
        k2 = afterUnderscore rootEssence
        parseFunc n = parseFieldsFunc [k1 <> (T.pack . show) n, k2]

        iterateObjs obj = iterateObjs' obj 1
        iterateObjs' obj n =
            case parseMaybe (parseFunc n) obj of
                Just value -> value : iterateObjs' obj (n+1)
                Nothing    -> []

        rootArrValue = iterateObjs rootObj
        (branchValue:rest) = iterateObjs branchObj
    in or $ Prelude.map (branchValue==) rootArrValue

isRightRelations rootObj branchObj rootEssence branchEssence =
    let
        rKey1 = parseObjEssence $ beforeUnderscore rootEssence
        rKey2 = afterUnderscore rootEssence

        bKey1 = parseObjEssence branchEssence
        bKey2 = rootEssence

        getValue k1 k2 obj =
            case parseMaybe (parseFieldsFunc [k1,k2]) obj of
                Just value -> value
                Nothing    -> Null

        rootValue = getValue rKey1 rKey2 rootObj
        branchValue = getValue bKey1 bKey2 branchObj
    in (==) rootValue branchValue

getIdPairFromObj :: Name -> Object -> [(String,MyValue.MyValue)]
getIdPairFromObj name obj =
    case getListOfPairFromObj (name <> "_id") obj of
        [(field,value)] -> [(field, value)]
        _               -> []

ifExisteAddEssenceId :: StateT (Essence List) (ReaderT Config IO) ()
ifExisteAddEssenceId = do
    api <- lift . lift $ setApi
    essenceList@(EssenceList name action list) <- get
    let essence = T.pack name
    let parseFunc = parseFieldsFunc ["relations",essence]
    let getRoot = T.unpack . head . HM.keys
    let changeAction = if action == "create" then "edit" else action
    case parseMaybe parseFunc api of
        Just (Object obj1) ->
            case lookup (getRoot obj1) list of
                Just value -> do
                    (Object obj2) <- lift $ dbGetOne (EssenceList name "get" [(getRoot obj1, value)])
                    if HM.null obj2
                        then return ()
                        else put $ addList (getIdPairFromObj (T.pack name) obj2) (EssenceList name changeAction list)
                _          -> return ()
        _                  -> return ()