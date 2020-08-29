{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Essence.GetFields
  ( GetFields(..)
  ) where

import Data.Essence
import Data.Required
import Data.Required.Methods

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Action = T.Text

type Field = String

class GetFields a where
  getFields :: Essence DB -> a
  iterateHM :: [(Field, Description)] -> Action -> [a]
  iterateHMCreate, iterateHMGet, iterateHMEdit, iterateHMDelete ::
       [(Field, Description)] -> [a]

instance GetFields (Required [Field]) where
  getFields :: Essence DB -> Required [Field]
  getFields (EssenceDB _ action hashMap) =
    let arr = iterateHM (HM.toList hashMap) action
        andFields =
          requiredSequenceA $
          filter
            (\case
               AND _ -> True
               _ -> False)
            arr
        orFields =
          requiredSequenceA $
          filter
            (\case
               OR _ -> True
               _ -> False)
            arr
     in Required [andFields, orFields]
  iterateHM :: [(Field, Description)] -> Action -> [Required [Field]]
  iterateHM [] _ = []
  iterateHM arr action =
    case action of
      "create" -> iterateHMCreate arr
      "get" -> iterateHMGet arr
      "edit" -> iterateHMEdit arr
      "delete" -> iterateHMDelete arr
      _ -> []
  iterateHMCreate, iterateHMGet, iterateHMEdit, iterateHMDelete ::
       [(Field, Description)] -> [Required [Field]]
  iterateHMCreate [] = []
  iterateHMCreate ((field, description):rest) =
    case field of
      "id" -> iterateHMCreate rest
      "date_of_creation" -> iterateHMCreate rest
      "access_key" -> iterateHMCreate rest
      _ ->
        case dValue description of
          Just (NOT NULL) -> AND [field] : iterateHMCreate rest
          _ -> iterateHMCreate rest
  iterateHMGet _ = []
  iterateHMEdit [] = []
  iterateHMEdit ((field, _):rest) =
    case field of
      "id" -> AND [field] : iterateHMEdit rest
      "access_key" -> AND [field] : iterateHMEdit rest
      "date_of_creation" -> iterateHMEdit rest
      _ -> OR [field] : iterateHMEdit rest
  iterateHMDelete [] = []
  iterateHMDelete ((field, _):rest) =
    case field of
      "id" -> AND [field] : iterateHMDelete rest
      "access_key" -> AND [field] : iterateHMDelete rest
      _ -> iterateHMDelete rest

instance GetFields [Field] where
  getFields :: Essence DB -> [Field]
  getFields (EssenceDB _ action hashMap) =
    let arr = iterateHM (HM.toList hashMap) action
     in concat arr
  iterateHM :: [(Field, Description)] -> Action -> [[Field]]
  iterateHM [] _ = []
  iterateHM arr action =
    case action of
      "create" -> iterateHMCreate arr
      "get" -> ["page"] : iterateHMGet arr
      "edit" -> iterateHMEdit arr
      "delete" -> iterateHMDelete arr
      _ -> []
  iterateHMCreate, iterateHMGet, iterateHMEdit, iterateHMDelete ::
       [(Field, Description)] -> [[Field]]
  iterateHMCreate [] = []
  iterateHMCreate ((field, _):rest) =
    case field of
      "id" -> iterateHMCreate rest
      "date_of_creation" -> iterateHMCreate rest
      _ -> [field] : iterateHMCreate rest
  iterateHMGet = flip (:) [] . map fst
  iterateHMEdit [] = []
  iterateHMEdit ((field, _):rest) =
    case field of
      "date_of_creation" -> iterateHMEdit rest
      _ -> [field] : iterateHMEdit rest
  iterateHMDelete [] = []
  iterateHMDelete ((field, _):rest) =
    case field of
      "id" -> [[field]]
      _ -> iterateHMDelete rest
