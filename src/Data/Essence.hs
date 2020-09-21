{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Essence
  ( Essence(..)
  , Column(..)
  , List
  ) where

import Data.Base
import Data.Essence.Column
import Data.MyValue
import Data.SQL

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Field = String

type List = [(Field, MyValue)]

data family Essence a

data instance  Essence (Clause String) = EssenceClause{ecName ::
                                                       [Field],
                                                       ecClauseList :: [Clause String]}
                                           deriving (Show, Eq)

instance Monoid (Essence (Clause String)) where
  mempty = EssenceClause [] []

instance Semigroup (Essence (Clause String)) where
  EssenceClause name1 listOfPairs1 <> EssenceClause name2 listOfPairs2 =
    let name = name1 <> name2
        list = listOfPairs1 <> listOfPairs2
     in EssenceClause name list

data instance  Essence Column = EssenceColumn{eColName :: T.Text,
                                              eColAction :: T.Text,
                                              eColHashMap :: HM.HashMap T.Text Column}
                                  deriving (Show, Eq)

instance A.FromJSON (Essence Column) where
  parseJSON =
    let getName = head . HM.keys
        toColumn v =
          case AT.parseMaybe A.parseJSON v of
            Just column -> column
            Nothing -> defaultColumn
        getHashMap obj = HM.map toColumn . fromObj $ getValue [getName obj] obj
     in A.withObject "From JSON Data.Essence.Essence Column" $ \x ->
          pure
            EssenceColumn
              { eColName = getName x
              , eColAction = "create"
              , eColHashMap = getHashMap x
              }

data instance  Essence List = EssenceList{elName :: Field,
                                          elAction :: Field, elList :: List}
                                deriving (Show, Eq)

instance Monoid (Essence List) where
  mempty = EssenceList "" "" []

instance Semigroup (Essence List) where
  EssenceList name action list1 <> EssenceList _ _ list2 =
    EssenceList name action $ list1 <> list2
