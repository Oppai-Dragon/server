module Data.Essence.RelationsTree.Methods
    ( isEssenceRelations
    , getRelationsFields
    , beforeUnderscore
    , parseObjEssence
    , afterUnderscore
    , findEssence
    , getQueryBSFromObj
    , checkQueryBS
    , unpackLeafs
    , iterateRelations
    , getNextField
    , relationsHandler
    , isRightRelations
    , getIdFromQueryBS
    , ifExisteGetPairBoolQueryBS
    ) where

import Prelude hiding (null, tail, takeWhile, dropWhile)

import Config
import Data.Base
import Data.SQL.Actions
import Data.Essence
import Data.Essence.Relations
import Data.Essence.Parse
import Data.Empty
import Data.MyValue
import Data.FromValue (toQueryBS, toBS)
import DataBase.Get (dbGetOne)

import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import Data.ByteString (ByteString)
import Data.Text hiding (null, head, singleton)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.HashMap.Strict hiding (lookup)

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class          (lift)

import System.IO.Unsafe (unsafePerformIO)

type QueryBS = [(ByteString, Maybe ByteString)]
type Name   = Text
type Field  = Text
type Fields = [Field]

isEssenceRelations :: Essence -> Config -> Bool
isEssenceRelations essence conf =
    let relations = getEssenceRelations essence conf
    in case relations of
        Root r (Trunk t rlt)               -> True
        Root r (Branch b (Leaf l : leafs)) -> True
        _                                  -> False

getRelationsFields :: Name -> StateT (Essence List) (ReaderT Config IO) ()
getRelationsFields name = do
    api <- lift . lift $ setApi
    essenceList <- get
    let relations = getRelationsTree name api
    let listOfPair field obj = case HM.lookup field obj of
            Just value -> [(T.unpack tableField,value)]
            Nothing    -> []
    case relations of
        Root rEssence trunk@(Trunk tEssence rlt)            ->
            do
                obj1 <- getRelationsFields (beforeUnderscore rEssence)
                (Object obj2) <- lift $ findEssence tEssence (listOfPair rEssence obj1)
                iterateRelations trunk obj2
        Root rEssence branch@(Branch bEssence leafs)        ->
            do
                obj1 <- getRelationsFields (beforeUnderscore rEssence) queryBS conf
                iterateRelations (Trunk rEssence branch) queryBS conf obj1
        Root rEssence (Leaf key)                            ->
            case lookup (encodeUtf8 key) queryBS of
                Just (Just accessKey) ->
                    dbGetOne rEssence [(encodeUtf8 key, Just accessKey)] conf
                    >>= \(Object obj) -> return obj
                _              -> put essenceList
        _                                                   -> put essenceList

iterateRelations :: Relations Field -> Object -> StateT (Essence List) (ReaderT Config IO) ()
iterateRelations (Trunk t (Branch b leafs)) objOld = do
    essenceList@(EssenceList name action list) <- get
    let listOfPair = checkList t list
    case listOfPair of
        [(field, value)] ->
            case b of
                "news" -> do
                    (Object objNew) <- lift $ dbGetOne (EssenceValue (beforeUnderscore t) action listOfPair)
                    if isRightRelations objOld objNew t b
                        then put $ unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objNew
                        else put essenceList
                _      ->
                    dbGetOne b [(field, Just value)] conf
                    >>= \(Object objNew) ->
                    if isRightRelations objOld objNew t b
                        then put $ unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld
                        else put essenceList
        []                   ->
            put $ unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld
iterateRelations (Trunk t1 trunk@(Trunk t2 rlt)) queryBS conf objOld =
    findEssence t2 (getQueryBSFromObj t1 objOld) conf
    >>= \(Object objNew) -> iterateRelations trunk queryBS conf objNew

findEssence :: Name -> [(String,Value)] -> Config -> ReaderT Config IO Value
findEssence name listOfPair conf =
    case listOfPair of
        [(field, value)] ->
            dbGetOne (EssenceValue (beforeUnderscore essence) "get" listOfPair)
        []                    ->
            return . Object . singleton "result" $ Number 0

unpackLeafs :: Name -> [Relations Field] -> Object -> [(String, String)]
unpackLeafs _       []            _   = []
unpackLeafs root (Leaf field :rest) obj =
    let
        key = afterUnderscore field
        parseFunc = parseFieldsFunc [root,key]
    in case parseMaybe parseFunc obj of
        Just value -> (T.unpack field, valueToStr value) : unpackLeafs root rest obj
        Nothing    -> unpackLeafs root rest obj

beforeUnderscore :: Name -> Name
beforeUnderscore = takeWhile (/='_')

parseObjEssence :: Name -> Name
parseObjEssence root = ifElseThen [root=="user"] [root,"users"] <> "1"

afterUnderscore :: Name -> Field
afterUnderscore = tail . dropWhile (/='_')

getQueryBSFromObj :: Field -> Object -> QueryBS
getQueryBSFromObj field obj =
    let
        key1 = parseObjEssence $ beforeUnderscore field
        key2 = afterUnderscore field
        parseFunc = parseFieldsFunc [key1,key2]
    in case parseMaybe parseFunc obj of
        Just value -> [(encodeUtf8 field,Just $ toBS value)]
        Nothing    -> []

checkList :: Field -> [(String,String)] -> [(String,Value)]
checkList field listOfPair =
    let
        findKey = T.unpack . afterUnderscore . T.pack
        key = findKey field
    in case lookup key listOfPair of
        (Just value) -> [(key, strToValue value)]
        Nothing      -> []

strToValue :: String -> Value
strToValue = toValue . fromString

valueToStr :: Value -> String
valueToStr = parseEmpty . fromValue

getNextField :: Relations a -> a
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
        parseFunc n = parseFieldsFunc [k1 <> (pack . show) n, k2]

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

getIdFromQueryBS :: Name -> Object -> QueryBS
getIdFromQueryBS name obj =
    let parseFieldBS = encodeUtf8 . afterUnderscore . decodeUtf8
    in case getQueryBSFromObj (essence <> "_id") obj of
        [(fieldBS, Just valueBS)] -> [(parseFieldBS fieldBS, Just valueBS)]
        _                         -> []

ifExisteGetPairBoolQueryBS :: Name -> QueryBS -> Config -> (Bool,QueryBS)
ifExisteGetPairBoolQueryBS _       []      _    = (False, [])
ifExisteGetPairBoolQueryBS name queryBS conf =
    let
        parseFunc = parseFieldsFunc
            ["psql","relations",essence,"find"]
        getRoot = head . keys
        getFindField obj field = getRoot obj <> "_" <> field
    in case parseMaybe parseFunc conf of
        Just (Object obj1) ->
            case parseMaybe (.: getRoot obj1) obj1 of
                Just (String text) ->
                    case lookup (encodeUtf8 text) queryBS of
                        Just (Just valueBS) ->
                            (\(Object obj2) ->
                            (not $ null obj2,getIdFromQueryBS name obj2)
                            ) $ unsafePerformIO
                            (dbGetOne essence
                            [(encodeUtf8 $ getFindField obj1 text, Just valueBS)]
                            conf)
                        _                   -> (False, [])
                _            -> (False, [])
        _          -> (False, [])