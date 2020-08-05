module Data.Essence.RelationsTree.Methods
    ( isEssenceRelations
    , addRelationsFields
    , relationsHandler
    , iterateRelations
    , ifFieldsFill
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
    , isNewsExiste
    ) where

import Prelude hiding (null, tail, takeWhile, dropWhile)

import Config
import Data.Base    hiding (deletePair)
import Data.SQL.Actions
import Data.Essence
import Data.Essence.RelationsTree
import Data.Essence.Methods
import Data.Empty
import qualified Data.MyValue           as MyValue
import qualified Data.Value             as Value
import Database.Get
import Data.Required
import Data.Required.Methods

import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.ByteString        as BS
import qualified Data.List              as L
import qualified Data.Text              as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.HashMap.Strict    as HM

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

type QueryBS = [(BS.ByteString, Maybe BS.ByteString)]
type Name   = String
type Field  = String
type Fields = [Field]

isEssenceRelations :: Name -> Api -> Bool
isEssenceRelations essence api =
    let relations = getRelationsTree (T.pack essence) api
    in case relations of
        Root r (Trunk t rlt)               -> True
        Root r (Branch b (Leaf l : leafs)) -> True
        _                                  -> False

addRelationsFields :: StateT (Essence List) (ReaderT Config IO) Object
addRelationsFields = do
    (EssenceList name action list) <- get
    config <- lift ask
    api <- lift . lift $ setApi
    if isEssenceRelations name api
        then relationsHandler name
        else return $ HM.singleton "result" (Number 1)

relationsHandler :: Name -> StateT (Essence List) (ReaderT Config IO) Object
relationsHandler name = do
    api <- lift . lift $ setApi
    essenceList <- get
    let relations = getRelationsTree (T.pack name) api
    case relations of
        Root rEssence trunk@(Trunk tEssence rlt)            ->
            do
                obj1 <- relationsHandler (beforeUnderscore rEssence)
                (Object obj2) <- lift . findEssence tEssence $ getListOfPairFromObj rEssence obj1
                iterateRelations trunk obj2
        Root rEssence branch@(Branch bEssence leafs)        ->
            do
                obj1 <- relationsHandler (beforeUnderscore rEssence)
                iterateRelations (Trunk rEssence branch) obj1
        Root rEssence (Leaf key)                            ->
            case lookup key (list essenceList) of
                (Just accessKey) -> do
                    (Object obj) <- lift $ dbGetOne (EssenceList rEssence "get" [(key, accessKey)])
                    return obj
                _              -> return HM.empty
        _                                                   -> return HM.empty

iterateRelations :: RelationsTree -> Object -> StateT (Essence List) (ReaderT Config IO) Object
iterateRelations (Trunk t (Branch b leafs)) objOld = do
    essenceList@(EssenceList name action list) <- get
    let listOfPair = checkList t list
    case listOfPair of
        [(field, value)] -> case b of
            "news" -> do
                (Object objNew) <- lift $ dbGetOne (EssenceList (beforeUnderscore t) "get" listOfPair)
                -- Draft have only one "not null" field for creating - "name"
                -- But news, which copies draft values, need more then just "name"
                config <- lift ask
                api <- fromStateT setApi
                let essenceDB = getEssenceDB (T.pack b) (T.pack action) config api
                let addedFields = unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objNew
                let requiredFields = toFields $ getRequiredFields essenceDB api
                let bool = ifFieldsFill requiredFields addedFields
                if and [isRightRelations objOld objNew t b, bool]
                    then put (addList addedFields $ deletePair "id" essenceList)
                        >> return (HM.singleton "result" (Number 1))
                    else return HM.empty
            _      -> do
                (Object objNew) <- lift $ dbGetOne (EssenceList b "get" listOfPair)
                let addedFields = unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld
                if isRightRelations objOld objNew t b
                    then put (addList addedFields essenceList)
                        >> return (HM.singleton "result" (Number 1))
                    else return HM.empty
        []                   ->
            put (addList (unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld) essenceList)
            >> return (HM.singleton "result" (Number 1))
iterateRelations (Trunk t1 trunk@(Trunk t2 rlt)) objOld = do
    (Object objNew) <- lift $ findEssence t2 (getListOfPairFromObj t1 objOld)
    iterateRelations trunk objNew

ifFieldsFill :: [Field] -> [(Field,MyValue.MyValue)] -> Bool
ifFieldsFill []           _   = True
ifFieldsFill (field:rest) arr = case lookup field arr of
    Just MyValue.MyEmpty -> False
    Just _               -> ifFieldsFill rest arr
    Nothing              -> ifFieldsFill rest arr

findEssence :: Name -> [(String,MyValue.MyValue)] -> ReaderT Config IO Value
findEssence name listOfPair =
    case listOfPair of
        [(field, value)] ->
            dbGetOne (EssenceList (beforeUnderscore name) "get" listOfPair)
        []                    ->
            return . Object . HM.singleton "result" $ Number 0

unpackLeafs :: Name -> [RelationsTree] -> Object -> [(String, MyValue.MyValue)]
unpackLeafs _        []                 _   = []
unpackLeafs "draft1" (Leaf field' :rest) obj =
    let
        field = if field' == "draft_id" then "id" else field'
        parseFunc = parseFieldsFunc $ map T.pack ["draft1",field]
    in case parseMaybe parseFunc obj of
        Just value -> (field', MyValue.fromValue value) : unpackLeafs "draft1" rest obj
        Nothing    -> unpackLeafs "draft1" rest obj
unpackLeafs root     (Leaf field :rest) obj =
    let
        key = afterUnderscore field
        parseFunc = parseFieldsFunc $ map T.pack [root,key]
    in case parseMaybe parseFunc obj of
        Just value -> (field, MyValue.fromValue value) : unpackLeafs root rest obj
        Nothing    -> unpackLeafs root rest obj

beforeUnderscore :: Name -> Name
beforeUnderscore = L.takeWhile (/='_')

parseObjEssence :: Name -> Name
parseObjEssence name = name <> "1"

afterUnderscore :: Name -> Field
afterUnderscore = tailCase . L.dropWhile (/='_')

getListOfPairFromObj :: Field -> Object -> [(String,MyValue.MyValue)]
getListOfPairFromObj field obj =
    let
        key1 = parseObjEssence $ beforeUnderscore field
        key2 = afterUnderscore field
        parseFunc = parseFieldsFunc $ map T.pack [key1,key2]
    in case parseMaybe parseFunc obj of
        Just value -> [(field, MyValue.fromValue value)]
        Nothing    -> []

checkList :: Field -> [(String,MyValue.MyValue)] -> [(String,MyValue.MyValue)]
checkList field listOfPair =
    let key = afterUnderscore field
    in case lookup key listOfPair of
        (Just value) -> [(key, value)]
        Nothing      -> []

getNextField :: RelationsTree -> String
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
        parseFunc n = parseFieldsFunc $ map T.pack [k1 <> show n, k2]

        iterateObjs obj = iterateObjs' obj 1
        iterateObjs' obj n =
            case parseMaybe (parseFunc n) obj of
                Just value -> value : iterateObjs' obj (n+1)
                Nothing    -> []

        --List of drafts for specific author
        rootArrValue = iterateObjs rootObj
        --Specific draft, getting by id from queryString
        branchValue = case iterateObjs branchObj of
            [x]      -> x
            (x:rest) -> x
            _        -> Null
    in or $ Prelude.map (branchValue==) rootArrValue
isRightRelations rootObj branchObj rootEssence branchEssence =
    let
        rKey1 = parseObjEssence $ beforeUnderscore rootEssence
        rKey2 = afterUnderscore rootEssence

        bKey1 = parseObjEssence branchEssence
        bKey2 = rootEssence

        parseFunc k1 k2 = parseFieldsFunc $ map T.pack [k1,k2]

        getValue k1 k2 obj =
            case parseMaybe (parseFunc k1 k2) obj of
                Just value -> value
                Nothing    -> Null

        rootValue = getValue rKey1 rKey2 rootObj
        branchValue = getValue bKey1 bKey2 branchObj
    in (==) rootValue branchValue

getIdPairFromObj :: Name -> Object -> [(String,MyValue.MyValue)]
getIdPairFromObj name obj =
    case getListOfPairFromObj (name <> "_id") obj of
        [(field,value)] -> [("id", value)]
        _               -> []

isNewsExiste :: StateT (Essence List) (ReaderT Config IO) Bool
isNewsExiste = do
    essenceList@(EssenceList name action list) <- get
    let isNews = name == "news"
    let isExiste = case lookup "draft_id" list of
            Just myValue -> do
                (Object obj) <- lift $ dbGetOne (EssenceList name "get" [("draft_id",myValue)])
                let isGet = not $ HM.null obj
                if isGet
                    then put (EssenceList name "edit" list) >> return True
                    else return False
            _            -> return False
    if isNews
        then isExiste
        else return False