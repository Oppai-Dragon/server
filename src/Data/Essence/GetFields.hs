{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Essence.GetFields
  ( GetFields(..)
  ) where

import Data.Base
import Data.Essence
import Data.Essence.Column hiding (Action(..))
import Data.Required
import Data.Required.Methods

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Action = T.Text

type Field = String

class GetFields a where
  getFields :: Essence Column -> a
  iterateHM :: [(Field, Column)] -> Action -> [a]
  iterateHMCreate, iterateHMGet, iterateHMEdit, iterateHMDelete ::
       [(Field, Column)] -> [a]

instance GetFields (Required [Field]) where
  getFields :: Essence Column -> Required [Field]
  getFields EssenceColumn {eColAction = action, eColHashMap = hashMap} =
    let arr = iterateHM (HM.toList $ toStringKeys hashMap) action
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
  iterateHM :: [(Field, Column)] -> Action -> [Required [Field]]
  iterateHM [] _ = []
  iterateHM arr action =
    case action of
      "create" -> iterateHMCreate arr
      "get" -> iterateHMGet arr
      "edit" -> iterateHMEdit arr
      "delete" -> iterateHMDelete arr
      _ -> []
  iterateHMCreate, iterateHMGet, iterateHMEdit, iterateHMDelete ::
       [(Field, Column)] -> [Required [Field]]
  iterateHMCreate [] = []
  iterateHMCreate ((field, column):rest) =
    case field of
      "id" -> iterateHMCreate rest
      "date_of_creation" -> iterateHMCreate rest
      "access_key" -> iterateHMCreate rest
      "is_admin" -> iterateHMCreate rest
      _ ->
        case cNULL column of
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
  getFields :: Essence Column -> [Field]
  getFields EssenceColumn {eColAction = action, eColHashMap = hashMap} =
    let arr = iterateHM (HM.toList $ toStringKeys hashMap) action
     in concat arr
  iterateHM :: [(Field, Column)] -> Action -> [[Field]]
  iterateHM [] _ = []
  iterateHM arr action =
    case action of
      "create" -> iterateHMCreate arr
      "get" -> ["page"] : iterateHMGet arr
      "edit" -> iterateHMEdit arr
      "delete" -> iterateHMDelete arr
      _ -> []
  iterateHMCreate, iterateHMGet, iterateHMEdit, iterateHMDelete ::
       [(Field, Column)] -> [[Field]]
  iterateHMCreate [] = []
  iterateHMCreate ((field, _):rest) =
    case field of
      "id" -> iterateHMCreate rest
      "date_of_creation" -> iterateHMCreate rest
      "is_admin" -> iterateHMCreate rest
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
