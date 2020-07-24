module Config
    ( Config
    , Api
    , Psql
    , setConfig
    , setPsql
    , setApi
    , set
    , setPath
    , setConfigPath
    , setPsqlPath
    , setApiPath
    , parsePath
    , getApiActions
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
getEssences conf = case AT.parseMaybe (.: "essences") conf of
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

getRelationsTree :: Essence -> Api -> RelationsTree Field
getRelationsTree essence api = getRelationsTree' essence 0 essence api

getRelationsTree' :: Essence -> Int -> Field -> Api -> RelationsTree Field
getRelationsTree' essence n field api =
    let
        parseFind = parseFieldsFunc
            ["relations", essence]
        parseFill name = parseFieldsFunc
            ["relations", essence, name]
        getRootName =  T.takeWhile (/='_') . getRoot
        getRoot = head . HM.keys
        getLeafs name = case AT.parseMaybe (parseFill name) api of
            Just arr@(Array vector) -> map (\x -> Leaf x) $ toTextArr arr
            _                   -> []
    in case AT.parseMaybe parseFind api of
        Just (Object obj) ->
            getRelationsTree' (getRootName obj) 1 (getRoot obj) api
            <> if n == 0
                then Branch essence (getLeafs $ getRoot obj)
                else Branch field (getLeafs $ getRoot obj)
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