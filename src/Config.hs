module Config
    ( Config
    , Api
    , Psql
    , set
    , setPsql
    , setConfig
    , setApi
    , setPath
    , setConfigPath
    , setPsqlPath
    , setApiPath
    , parsePath
    , getApiActions
    , parseFieldsFunc
    , getEssences
    , getAccess
    , getUri
    , getUriDB
    , getMethodActions
    , getApiDBMethod
    , getRelationFields
    , getRelationsTree
    , getRelationsTree'
    , getOffsetLimit
    , testApi
    , testConfig
    , testPsql
    ) where

import           Data.Base
import           Data.Request.Access
import           Data.Essence.RelationsTree
import           Data.Value

import           Data.Aeson
import qualified Data.Aeson.Types       as AT
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V

import System.Directory                         (getCurrentDirectory)
import System.IO.Unsafe                         (unsafePerformIO)

type Action         = T.Text
type Actions        = [Action]
type Config         = Object
type Api            = Object
type Psql           = Object
type EssenceName    = T.Text
type Field          = T.Text
type Fields         = [Field]
type Method         = BS.ByteString

set :: IO FilePath -> IO Object
set ioPath =
    ioPath
    >>= BSL.readFile
    >>= pure . decode
    >>= \x ->
        case x of
            Just hm -> pure hm
            Nothing -> pure HM.empty

setPsql,setConfig,setApi :: IO Object
setPsql = set setPsqlPath
setConfig = set setConfigPath
setApi = set setApiPath

setPath :: FilePath -> IO FilePath
setPath path =
    fmap (flip (<>) $ "\\server\\src\\" <> path)
    $ parsePath <*> getCurrentDirectory
setConfigPath,setPsqlPath,setApiPath :: IO FilePath
setConfigPath = setPath "Config.json"
setPsqlPath = setPath "Psql.json"
setApiPath  = setPath "Api.json"

parsePath :: IO (FilePath -> FilePath)
parsePath = return
    ( L.intercalate "\\"
    . takeWhile (/="server")
    . L.words
    . L.intercalate ""
    . map (\x -> if x == "\\" then " " else x)
    . L.group
    )

getApiActions :: Api -> Actions
getApiActions api =
    case AT.parseMaybe (.: "api") api of
        Just (Object obj) -> HM.keys obj
        Nothing           -> []

parseFieldsFunc :: Fields -> Object -> AT.Parser Value
parseFieldsFunc [field] obj      = obj .: field
parseFieldsFunc (field:rest) obj = obj .: field
    >>= parseFieldsFunc rest

getEssences :: Api -> [T.Text]
getEssences api = case AT.parseMaybe (.: "essences") api of
    Just arr@(Array vector) -> toTextArr arr
    _                       -> []

getAccess :: EssenceName -> Action -> Api -> Access
getAccess essence action api =
    let parseFunc = parseFieldsFunc ["access",essence,action]
    in case AT.parseMaybe parseFunc api of
        Just (String access) -> read $ T.unpack access
        _                    -> maxBound

getUri :: Config -> String
getUri conf =
    case AT.parseMaybe (.: "uriDB") conf of
        Just str -> str
        _        -> ""

getUriDB :: Psql -> String
getUriDB psql =
    let
        getValue field obj =
            case HM.lookup field obj of
                Just (String value) -> value
                Nothing             -> ""
        userName = getValue "userName"
        password = getValue "password"
        server = getValue "server"
        port = getValue "port"
        database = getValue "database"
    in T.unpack $
    "postgresql://" <> userName psql
    <> ":" <> password psql
    <> "@" <> server psql
    <> ":" <> port psql
    <> "/" <> database psql

getMethodActions :: Field -> Api -> Actions
getMethodActions method api =
    let parseFunc obj = obj .: "method" >>= (.: method)
    in case AT.parseMaybe parseFunc api of
        Just actions -> actions
        Nothing      -> []
getApiDBMethod :: Action -> Api -> Action
getApiDBMethod action api =
    let parseFunc = parseFieldsFunc ["api",action]
    in case AT.parseMaybe parseFunc api of
        Just (String text) -> text
        _                  -> ""

getRelationFields :: RelationsTree -> [String]
getRelationFields relationsTree = case relationsTree of
    Root r (Trunk t tree)    -> getRelationFields tree
    Root r (Branch b leafs)  -> concat $ map getRelationFields leafs
    Trunk t tree             -> getRelationFields tree
    Branch b leafs           -> concat $ map getRelationFields leafs
    Leaf key                 -> [key]
    _                        -> []

getRelationsTree :: EssenceName -> Api -> RelationsTree
getRelationsTree essence api = getRelationsTree' essence 0 essence api

getRelationsTree' :: EssenceName -> Int -> Field -> Api -> RelationsTree
getRelationsTree' essence n field api =
    let
        parseFind = parseFieldsFunc
            ["relations", essence]
        parseFill name = parseFieldsFunc
            ["relations", essence, name]
        getRootName =  T.takeWhile (/='_') . getRoot
        getRoot = head . HM.keys
        getLeafs name = case AT.parseMaybe (parseFill name) api of
            Just arr@(Array vector) -> map (Leaf . T.unpack) $ toTextArr arr
            _                   -> []
    in case AT.parseMaybe parseFind api of
        Just (Object obj) ->
            getRelationsTree' (getRootName obj) 1 (getRoot obj) api
            <> if n == 0
                then Branch (T.unpack essence) (getLeafs $ getRoot obj)
                else Branch (T.unpack field) (getLeafs $ getRoot obj)
        Just (String key) ->
            Root (T.unpack field) . Leaf $ T.unpack key
        _                 -> Ground

getOffsetLimit :: Int -> Psql -> String
getOffsetLimit pageCounter psql =
    let
        limit = show . scientificToInteger
        offset var = show $ read (limit var) * (pageCounter - 1)
    in case AT.parseMaybe (.: "page") psql of
        Just value@(Number pageLimit) ->
            "OFFSET " <> offset pageLimit
            <> " LIMIT " <> limit pageLimit
        Nothing                       -> []

testApi,testConfig,testPsql :: Config
testApi = unsafePerformIO setApi
testConfig = unsafePerformIO setConfig
testPsql = unsafePerformIO setPsql