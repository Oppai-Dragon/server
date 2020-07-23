{-# LANGUAGE LambdaCase #-}
module Data.Required.Methods
    ( getRequiredFields
    ) where

import Data.Essence
import Data.Essence.Methods
import Data.Required
import Data.FromValue (toTextArr)

import           Data.Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T

type Action = T.Text
type Field = String
type Fields = [String]

getRequiredFields :: Essence DB -> Required Fields
getRequiredFields (EssenceDB name action hashMap) =
    let
        arr = iterateHM (HM.toList hashMap) action
        andFields = mySequenceA $ filter (\case { AND _ -> True; _ -> False }) arr
        orFields = mySequenceA $ filter (\case { OR _ -> True; _ -> False }) arr
    in Required [andFields,orFields]

iterateHM :: [(String,Description)] -> Action -> [Required Field]
iterateHM []  _        = []
iterateHM arr "create" = iterateHMCreate arr
iterateHM arr "get"    = iterateHMGet arr
iterateHM arr "edit"   = iterateHMEdit arr
iterateHM arr "delete" = iterateHMDelete arr

iterateHMCreate,iterateHMGet,iterateHMEdit,iterateHMDelete :: [(String,Description)] -> [Required Field]
iterateHMCreate []                            = []
iterateHMCreate (("id",_):rest)               = iterateHMCreate rest
iterateHMCreate (("date_of_creation",_):rest) = iterateHMCreate rest
iterateHMCreate ((field,description):rest)    =
    case valueOf description of
        Just (NOT NULL) -> AND field : iterateHMCreate rest
        _               -> OR field : iterateHMCreate rest
iterateHMGet _ = []
iterateHMEdit []                            = []
iterateHMEdit (("date_of_creation",_):rest) = iterateHMEdit rest
iterateHMEdit ((field,description):rest)    =
    case field of
        "id"         -> AND field : iterateHMEdit rest
        "access_key" -> AND field : iterateHMEdit rest
        _            -> OR field : iterateHMEdit rest
iterateHMDelete []                         = []
iterateHMDelete ((field,description):rest) =
    case field of
        "id"         -> AND field : iterateHMDelete rest
        "access_key" -> AND field : iterateHMDelete rest
        _            -> iterateHMDelete rest

mySequenceA :: [Required a] -> Required [a]
mySequenceA []      = NullFields
mySequenceA [AND x] = AND [x]
mySequenceA [OR x]  = OR [x]
mySequenceA (x:xs)  = fmap (:) x `myApply` mySequenceA xs

myApply :: Required (a -> a) -> Required a -> Required a
myApply required x = case required of
    AND func -> fmap func x
    OR func  -> fmap func x