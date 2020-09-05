module Data.Essence.RelationsTree.Methods
  ( isEssenceRelations
  , addRelationsFields
  , relationsHandler
  , iterateRelations
  , ifFieldsFill
  , findEssence
  , getAddedFields
  , unpackLeafs
  , beforeUnderscore
  , parseObjEssence
  , afterUnderscore
  , getListOfPairFromObj
  , getNextField
  , isRightRelations
  , getIdPairFromObj
  , isNewsExiste
  ) where

import Prelude hiding (dropWhile, null, tail, takeWhile)

import Config
import Data.Base hiding (deletePair)
import Data.Essence
import Data.Essence.Methods
import Data.Essence.RelationsTree
import Data.MyValue as MyValue
import Data.Required.Methods
import Database.Get
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

type Name = String

type Field = String

goodResult :: A.Object
goodResult = HM.singleton "result" $ A.Number 1

isEssenceRelations :: Name -> Api -> Bool
isEssenceRelations essence api =
  let relations = getRelationsTree (T.pack essence) api
   in case relations of
        Root _ (Trunk _ _) -> True
        Root _ (Branch _ (Leaf _:_)) -> True
        _ -> False

addRelationsFields :: SApp A.Object
addRelationsFields = do
  (Config.Handle _ api _ logHandle) <- liftUnderApp askUnderApp
  (EssenceList name action _) <- getSApp
  case [name, action] of
    ["news", "delete"] -> return goodResult
    ["news", "get"] -> return goodResult
    ["comment", "get"] -> return goodResult
    _ ->
      if isEssenceRelations name api
        then (liftUnderApp . liftIO)
               (debugM logHandle "Will be added relations fields") >>
             relationsHandler name
        else return goodResult

relationsHandler :: Name -> SApp A.Object
relationsHandler name = do
  (Config.Handle _ api _ logHandle) <- liftUnderApp askUnderApp
  essenceList <- getSApp
  let relations = getRelationsTree (T.pack name) api
  liftUnderApp . liftIO . debugM logHandle $
    "Realtions tree is " <> show relations
  case relations of
    Root rEssence trunk@(Trunk tEssence _) -> do
      obj1 <- relationsHandler (beforeUnderscore rEssence)
      (A.Object obj2) <-
        liftUnderApp . findEssence (beforeUnderscore tEssence) $
        getListOfPairFromObj rEssence obj1
      iterateRelations trunk obj2
    Root rEssence branch@(Branch _ _) -> do
      obj1 <- relationsHandler (beforeUnderscore rEssence)
      iterateRelations (Trunk rEssence branch) obj1
    Root rEssence (Leaf key) ->
      case lookup key (elList essenceList) of
        (Just accessKey) -> do
          liftUnderApp . liftIO . debugM logHandle $
            "Find " <> rEssence <> " by " <> key <> " from queryString"
          (A.Object obj) <-
            liftUnderApp $
            dbGetOne (EssenceList rEssence "get" [(key, accessKey)])
          return obj
        _ -> return HM.empty
    _ -> return HM.empty

iterateRelations :: RelationsTree -> A.Object -> SApp A.Object
iterateRelations (Trunk t (Branch b leafs)) objOld = do
  (Config.Handle config api _ logHandle) <- liftUnderApp askUnderApp
  (EssenceList _ action list) <- getSApp
  let name = beforeUnderscore t
  let field = afterUnderscore t
  case lookup field list of
    Just value ->
      case b of
        "news" -> do
          liftUnderApp . liftIO . debugM logHandle $
            "Find " <> name <> " by " <> field <> " from queryString"
          (A.Object objNew) <-
            liftUnderApp $ dbGetOne (EssenceList name "get" [(field, value)])
                -- Draft have only one "not null" field for creating - "name"
                -- But news, which copies draft values, need more then just "name"
          let essenceDB = getEssenceDB (T.pack b) (T.pack action) config api
          let addedFields = getAddedFields name leafs objNew
          let requiredFields = toFields $ getRequiredFields essenceDB api
          let bool1 = isRightRelations objOld objNew t b
          let bool2 = ifFieldsFill requiredFields addedFields
          liftUnderApp . liftIO . debugM logHandle $
            "isRightRelations - " <>
            show bool1 <> ", isFieldsFill -  " <> show bool2
          if bool1 && bool2
            then do
              liftUnderApp . liftIO . debugM logHandle $
                "Add these fields " <> show leafs <> " from " <> name
              -- Need delete "id" from Essence List, because it was needed only
              -- for getting from database, and now it'll be crush PSQL request
              modifySApp $ deletePair "id"
              modifySApp $ addList addedFields
              return goodResult
            else return HM.empty
        _ -> do
          liftUnderApp . liftIO . debugM logHandle $
            "Find " <> b <> " by " <> field <> " from queryString"
          (A.Object objNew) <-
            liftUnderApp $ dbGetOne (EssenceList b "get" [(field, value)])
          let addedFields = getAddedFields name leafs objOld
          let bool = isRightRelations objOld objNew t b
          liftUnderApp . liftIO . debugM logHandle $
            "isRightRelations - " <> show bool
          if bool
            then modifySApp (addList addedFields) >> return goodResult
            else return HM.empty
    Nothing -> do
      liftUnderApp . liftIO . debugM logHandle $
        "Add these fields " <> show leafs <> " from " <> name
      let addedFields = getAddedFields name leafs objOld
      modifySApp $ addList addedFields
      return goodResult
iterateRelations (Trunk t1 trunk@(Trunk t2 _)) objOld = do
  let findName = beforeUnderscore t2
  (A.Object objNew) <-
    liftUnderApp $ findEssence findName (getListOfPairFromObj t1 objOld)
  iterateRelations trunk objNew
iterateRelations _ _ = return HM.empty

ifFieldsFill :: [Field] -> [(Field, MyValue.MyValue)] -> Bool
ifFieldsFill [] _ = True
ifFieldsFill (field:rest) arr =
  case lookup field arr of
    Just MyValue.MyEmpty -> False
    _ -> ifFieldsFill rest arr

findEssence :: Name -> [(String, MyValue.MyValue)] -> UnderApp A.Value
findEssence name listOfPair =
  case listOfPair of
    [(field, _)] -> do
      (Config.Handle _ _ _ logHandle) <- askUnderApp
      liftIO . debugM logHandle $ "Find " <> name <> " by " <> field
      dbGetOne $ EssenceList name "get" listOfPair
    _ -> return . A.Object . HM.singleton "result" $ A.Number 0

getAddedFields ::
     Name -> [RelationsTree] -> A.Object -> [(String, MyValue.MyValue)]
getAddedFields name leafs essenceObj =
  let objName = parseObjEssence name
      fieldsObj =
        case getValue [T.pack objName] essenceObj of
          A.Object obj -> obj
          _ -> HM.empty
      idValue = getValue ["id"] fieldsObj
   in unpackLeafs leafs $ HM.insert (T.pack $ name <> "_id") idValue fieldsObj

unpackLeafs :: [RelationsTree] -> A.Object -> [(String, MyValue.MyValue)]
unpackLeafs [] _ = []
unpackLeafs (Leaf l:rest) fieldsObj =
  let value = getValue [T.pack l] fieldsObj
   in (l, MyValue.fromValue value) : unpackLeafs rest fieldsObj
unpackLeafs _ _ = []

beforeUnderscore :: Name -> Name
beforeUnderscore = L.takeWhile (/= '_')

parseObjEssence :: Name -> Name
parseObjEssence name = name <> "1"

afterUnderscore :: Name -> Field
afterUnderscore = tailCase . L.dropWhile (/= '_')

getListOfPairFromObj :: Field -> A.Object -> [(String, MyValue.MyValue)]
getListOfPairFromObj field obj =
  let key1 = parseObjEssence $ beforeUnderscore field
      key2 = afterUnderscore field
      fields = map T.pack [key1, key2]
      value = getValue fields obj
   in [(field, MyValue.fromValue value)]

getNextField :: RelationsTree -> String
getNextField relations =
  case relations of
    Root _ (Branch b _) -> b
    Root _ (Trunk t _) -> t
    Trunk _ (Trunk t2 _) -> t2
    Trunk _ (Branch b _) -> b
    Branch b _ -> b
    _ -> ""

isRightRelations :: A.Object -> A.Object -> Name -> Name -> Bool
isRightRelations rootObj branchObj rootEssence "news" =
  let k1 = beforeUnderscore rootEssence
      k2 = afterUnderscore rootEssence
      parseFunc :: Int -> A.Object -> A.Value
      parseFunc n = getValue (map T.pack [k1 <> show n, k2])
      iterateObjs obj n lim
        | n <= lim = parseFunc n obj : iterateObjs obj (n + 1) lim
        | otherwise = []
        --List of drafts for specific author
      rootArrValue = iterateObjs rootObj 1 (length $ HM.keys rootObj)
        --Specific draft, getting by id from queryString
      branchValue = parseFunc 1 branchObj
   in elem branchValue rootArrValue
isRightRelations rootObj branchObj rootEssence branchEssence =
  let rKey1 = T.pack . parseObjEssence $ beforeUnderscore rootEssence
      rKey2 = T.pack $ afterUnderscore rootEssence
      bKey1 = T.pack $ parseObjEssence branchEssence
      bKey2 = T.pack rootEssence
      rootValue = getValue [rKey1, rKey2] rootObj
      branchValue = getValue [bKey1, bKey2] branchObj
   in (==) rootValue branchValue

getIdPairFromObj :: Name -> A.Object -> [(String, MyValue.MyValue)]
getIdPairFromObj name obj =
  case getListOfPairFromObj (name <> "_id") obj of
    [(_, value)] -> [("id", value)]
    _ -> []

isNewsExiste :: SApp Bool
isNewsExiste = do
  (EssenceList name _ list) <- getSApp
  let isNews = name == "news"
  let isExiste =
        case lookup "draft_id" list of
          Just myValue -> do
            (A.Object obj) <-
              liftUnderApp $
              dbGetOne (EssenceList name "get" [("draft_id", myValue)])
            let isGet = not $ HM.null obj
            if isGet
              then putSApp (EssenceList name "edit" list) >> return True
              else return False
          _ -> return False
  if isNews
    then isExiste
    else return False
