{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Required.Methods
    ( getRequiredFields
    , GetFields(..)
    , requiredSequenceA
    , requiredApply
    ) where

import Config

import Data.Essence
import Data.Required
import Data.Value

import           Data.Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.List              as L

type Action = T.Text
type Field = String

getRequiredFields :: Essence DB -> Api -> Required [Field]
getRequiredFields essenceDB api =
    let
        relationsTree = getRelationsTree (nameOf essenceDB) api
        relationFields = getRelationFields relationsTree
    in fmap (flip (L.\\) relationFields) $ getFields essenceDB

instance GetFields (Required [Field]) where
    getFields :: Essence DB -> Required [Field]
    getFields (EssenceDB name action hashMap) =
        let
            arr = iterateHM (HM.toList hashMap) action
            andFields = requiredSequenceA $ filter (\case { AND _ -> True; _ -> False }) arr
            orFields = requiredSequenceA $ filter (\case { OR _ -> True; _ -> False }) arr
        in Required [andFields,orFields]
    iterateHM :: [(Field,Description)] -> Action -> [Required [Field]]
    iterateHM []  _      = []
    iterateHM arr action = case action of
        "create" -> iterateHMCreate arr
        "get"    -> iterateHMGet arr
        "edit"   -> iterateHMEdit arr
        "delete" -> iterateHMDelete arr
    iterateHMCreate,iterateHMGet,iterateHMEdit,iterateHMDelete ::
        [(Field,Description)] -> [Required [Field]]
    iterateHMCreate []                            = []
    iterateHMCreate (("id",_):rest)               = iterateHMCreate rest
    iterateHMCreate (("date_of_creation",_):rest) = iterateHMCreate rest
    iterateHMCreate ((field,description):rest)    =
        case valueOf description of
            Just (NOT NULL) -> AND [field] : iterateHMCreate rest
            _               -> iterateHMCreate rest
    iterateHMGet _ = []
    iterateHMEdit []                            = []
    iterateHMEdit ((field,description):rest)    =
        case field of
            "id"               -> AND [field] : iterateHMEdit rest
            "access_key"       -> AND [field] : iterateHMEdit rest
            "date_of_creation" -> iterateHMEdit rest
            _                  -> OR [field] : iterateHMEdit rest
    iterateHMDelete []                         = []
    iterateHMDelete ((field,description):rest) =
        case field of
            "id"         -> AND [field] : iterateHMDelete rest
            "access_key" -> AND [field] : iterateHMDelete rest
            _            -> iterateHMDelete rest

requiredSequenceA :: [Required [a]] -> Required [a]
requiredSequenceA arrRequired = case arrRequired of
    []      -> NullFields
    [AND x] -> AND x
    [OR x]  -> OR x
    x:xs    -> fmap (<>) x `requiredApply` requiredSequenceA xs

requiredApply :: Required (a -> a) -> Required a -> Required a
requiredApply required x = case required of
    AND func -> fmap func x
    OR func  -> fmap func x