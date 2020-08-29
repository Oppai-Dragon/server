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

import Prelude hiding (dropWhile, null, tail, takeWhile)

import Config
import Data.Base hiding (deletePair)
import Data.Essence
import Data.Essence.Methods
import Data.Essence.RelationsTree
import Data.MyValue as MyValue
import Data.Required.Methods
import Database.Get

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

type Name = String

type Field = String

isEssenceRelations :: Name -> Api -> Bool
isEssenceRelations essence api =
  let relations = getRelationsTree (T.pack essence) api
   in case relations of
        Root _ (Trunk _ _) -> True
        Root _ (Branch _ (Leaf _:_)) -> True
        _ -> False

addRelationsFields :: SApp A.Object
addRelationsFields = do
  (EssenceList name action _) <- getSApp
  api <- liftUnderApp $ liftIO setApi
  case [name, action] of
    ["news", "delete"] -> return $ HM.singleton "result" (A.Number 1)
    _ ->
      if isEssenceRelations name api
        then relationsHandler name
        else return $ HM.singleton "result" (A.Number 1)

relationsHandler :: Name -> SApp A.Object
relationsHandler name = do
  api <- liftUnderApp $ liftIO setApi
  essenceList <- getSApp
  let relations = getRelationsTree (T.pack name) api
  case relations of
    Root rEssence trunk@(Trunk tEssence _) -> do
      obj1 <- relationsHandler (beforeUnderscore rEssence)
      (A.Object obj2) <-
        liftUnderApp . findEssence tEssence $ getListOfPairFromObj rEssence obj1
      iterateRelations trunk obj2
    Root rEssence branch@(Branch _ _) -> do
      obj1 <- relationsHandler (beforeUnderscore rEssence)
      iterateRelations (Trunk rEssence branch) obj1
    Root rEssence (Leaf key) ->
      case lookup key (elList essenceList) of
        (Just accessKey) -> do
          (A.Object obj) <-
            liftUnderApp $
            dbGetOne (EssenceList rEssence "get" [(key, accessKey)])
          return obj
        _ -> return HM.empty
    _ -> return HM.empty

iterateRelations :: RelationsTree -> A.Object -> SApp A.Object
iterateRelations (Trunk t (Branch b leafs)) objOld = do
  (EssenceList _ action list) <- getSApp
  let listOfPair = checkList t list
  case listOfPair of
    [(_, _)] ->
      case b of
        "news" -> do
          (A.Object objNew) <-
            liftUnderApp $
            dbGetOne (EssenceList (beforeUnderscore t) "get" listOfPair)
                -- Draft have only one "not null" field for creating - "name"
                -- But news, which copies draft values, need more then just "name"
          (Config.Handle config _ _) <- liftUnderApp askUnderApp
          api <- liftUnderApp $ liftIO setApi
          let essenceDB = getEssenceDB (T.pack b) (T.pack action) config api
          let addedFields =
                unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objNew
          let requiredFields = toFields $ getRequiredFields essenceDB api
          let bool = ifFieldsFill requiredFields addedFields
          if isRightRelations objOld objNew t b && bool
            then modifySApp (deletePair "id") >>
                 modifySApp (addList addedFields) >>
                 return (HM.singleton "result" (A.Number 1))
            else return HM.empty
        _ -> do
          (A.Object objNew) <-
            liftUnderApp $ dbGetOne (EssenceList b "get" listOfPair)
          let addedFields =
                unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld
          if isRightRelations objOld objNew t b
            then modifySApp (addList addedFields) >>
                 return (HM.singleton "result" (A.Number 1))
            else return HM.empty
    _ ->
      modifySApp
        (addList
           (unpackLeafs (parseObjEssence $ beforeUnderscore t) leafs objOld)) >>
      return (HM.singleton "result" (A.Number 1))
iterateRelations (Trunk t1 trunk@(Trunk t2 _)) objOld = do
  (A.Object objNew) <-
    liftUnderApp $ findEssence t2 (getListOfPairFromObj t1 objOld)
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
    [(_, _)] -> dbGetOne $ EssenceList (beforeUnderscore name) "get" listOfPair
    _ -> return . A.Object . HM.singleton "result" $ A.Number 0

unpackLeafs ::
     Name -> [RelationsTree] -> A.Object -> [(String, MyValue.MyValue)]
unpackLeafs _ [] _ = []
unpackLeafs root (Leaf l:rest) obj =
  let field = afterUnderscore l
      fields = map T.pack [root, field]
      value = getValue fields obj
   in (l, MyValue.fromValue value) : unpackLeafs root rest obj
unpackLeafs _ _ _ = []

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

checkList :: Field -> [(String, MyValue.MyValue)] -> [(String, MyValue.MyValue)]
checkList field listOfPair =
  let key = afterUnderscore field
   in case lookup key listOfPair of
        (Just value) -> [(key, value)]
        Nothing -> []

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
      iterateObjs obj n = parseFunc n obj : iterateObjs obj (n + 1)
        --List of drafts for specific author
      rootArrValue = iterateObjs rootObj 1
        --Specific draft, getting by id from queryString
      branchValue =
        case iterateObjs branchObj 1 of
          [x] -> x
          (x:_) -> x
          _ -> A.Null
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
