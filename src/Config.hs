module Config
    ( Config
    , setConfig
    , setPsql
    , setApi
    , set
    , setPath
    , setConfigPath
    , setPsqlPath
    , setApiPath
    , parsePath
    , getEssenceFields
    , parseFieldsFunc
    , getEssences
    , getActionsForEssence
    , getAccess
    , getActionsAccess
    , getUri
    , getUriDB
    , getMethodActions
    , getApiDBMethod
    , getRelationsTree
    , getOffsetLimit
    , testConfig
    , testPsql
    , testApi
    ) where

import           Data.Request.Access
import           Data.Essence.RelationsTree
import           Data.FromValue                 (toTextArr,toInt,toBS)

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
type Essence        = T.Text
type Essences       = [Essence]
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
    fmap (flip (<>) $ "\\src\\" <> path)
    $ parsePath <*> getCurrentDirectory
setConfigPath,setPsqlPath,setApiPath :: IO FilePath
setConfigPath = setPath "Config.json"
setPsqlPath = setPath "Psql.json"
setApiPath  = setPath "Api.json"

parsePath :: IO (FilePath -> FilePath)
parsePath = return
    ( L.intercalate "\\"
    . takeWhile (/="src")
    . L.words
    . L.intercalate ""
    . map (\x -> if x == "\\" then " " else x)
    . L.group
    )

getEssenceFields :: Essence -> Config -> Fields
getEssenceFields essence conf =
    case AT.parseMaybe (.: essence) conf of
        Just (Object obj) -> HM.keys obj
        Nothing           -> []

parseFieldsFunc :: Fields -> Object -> AT.Parser Value
parseFieldsFunc [field] obj      =
    obj .: field
parseFieldsFunc (field:rest) obj =
    obj .: field >>= parseFieldsFunc rest

getEssences :: Config -> [T.Text]
getEssences obj = case AT.parseMaybe (.: "essences") obj of
    Just arr@(Array vector) -> toTextArr arr
    _                       -> []

getActionsForEssence :: Essence -> Api -> Actions
getActionsForEssence essence api =
    let parseFunc = parseFieldsFunc ["access",essence]
    in case AT.parseMaybe parseFunc api of
        Just (Object obj) -> HM.keys obj
        Nothing           -> []

getAccess :: Essence -> Action -> Api -> Access
getAccess essence action api =
    case lookup action (getActionsAccess essence api) of
        Just access -> access
        Nothing     -> maxBound

getActionsAccess :: Essence -> Api -> [(Action, Access)]
getActionsAccess essence api =
    let
        parseFunc = parseFieldsFunc
            ["access",essence]
        essenceActions = getActionsForEssence essence api
    in case AT.parseMaybe parseFunc api of
        Just (Object obj) ->
            map (\(l, String text) -> (l, read $ T.unpack text))
            $ HM.toList obj
        Nothing           -> []

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

getMethodActions :: Config -> [(Method, Actions)]
getMethodActions conf =
    let
        parseFunc = parseFieldsFunc
            ["request","method"]

        iterateMethods []               _   = []
        iterateMethods (method:methods) obj =
            case HM.lookup method obj of
                Just arr@(Array vector) ->
                    (TE.encodeUtf8 method, toTextArr arr) :
                    iterateMethods methods obj
                Nothing             ->
                    iterateMethods methods obj
    in case AT.parseMaybe parseFunc conf of
        Just (Object obj) -> iterateMethods (HM.keys obj) obj
        Nothing           -> []

getApiDBMethod :: Action -> Config -> Action
getApiDBMethod action conf =
    let parseFunc = parseFieldsFunc ["api",action]
    in case AT.parseMaybe parseFunc conf of
        Just (String text) -> text
        _                  -> ""

getRelationsTree :: Essence -> Config -> RelationsTree Field
getRelationsTree essence conf = getRelationsTree' essence 0 essence conf

getRelationsTree' :: Essence -> Int -> Field -> Config -> RelationsTree Field
getRelationsTree' essence n field conf =
    let
        parseFind = parseFieldsFunc
            ["psql", "relations",essence,"find"]
        parseFill name = parseFieldsFunc
            ["psql", "relations",essence,"fill",name]
        getRootName = head . HM.keys
        getRoot obj = case AT.parseMaybe (.: getRootName obj) obj of
            Just (String text) -> getRootName obj <> "_" <> text
            _                  -> getRootName obj
        getLeafs name = case AT.parseMaybe (parseFill name) conf of
            Just arr@(Array vector) ->
                map (\x -> Leaf $ name <> "_" <> x)
                $ toTextArr arr
            _                   -> []
    in case AT.parseMaybe parseFind conf of
        Just (Object obj) ->
            getRelationsTree' (getRootName obj) 1 (getRoot obj) conf
            <> if n == 0
                then Branch essence (getLeafs $ getRootName obj)
                else Branch field (getLeafs $ getRootName obj)
        Just (String key) ->
            Root field
            $ Leaf key
        _                 -> Ground

getOffsetLimit :: Int -> Psql -> String
getOffsetLimit pageCounter psql =
    let
        limit = show . toInt
        offset var = show $ read (limit var) * (pageCounter - 1)
    in case AT.parseMaybe (.: "page") psql of
        Just value@(Number pageLimit) ->
            "OFFSET " <> offset value
            <> " LIMIT " <> limit value
        Nothing                       -> []

testApi,testConfig,testPsql :: Config
testApi = unsafePerformIO setApi
testConfig = unsafePerformIO setConfig
testPsql = unsafePerformIO setPsql