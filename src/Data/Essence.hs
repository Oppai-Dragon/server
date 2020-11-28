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
import Data.Maybe
import qualified Data.Text as T

type Field = String

type List = [(Field, MyValue)]

data family Essence a

data instance  Essence (Clause String) = EssenceClause{ecNameList
                                                       :: [Field],
                                                       ecClauseList :: [Clause String]}
                                           deriving (Show, Eq)

instance Monoid (Essence (Clause String)) where
  mempty = EssenceClause {ecNameList = [], ecClauseList = []}

instance Semigroup (Essence (Clause String)) where
  EssenceClause {ecNameList = name1, ecClauseList = listOfPairs1} <> EssenceClause { ecNameList = name2
                                                                                   , ecClauseList = listOfPairs2
                                                                                   } =
    EssenceClause
      {ecNameList = name1 <> name2, ecClauseList = listOfPairs1 <> listOfPairs2}

data instance  Essence Column = EssenceColumn{eColName :: T.Text,
                                              eColAction :: T.Text,
                                              eColHashMap :: HM.HashMap T.Text Column}
                                  deriving (Show, Eq)

instance Monoid (Essence Column) where
  mempty =
    EssenceColumn {eColName = "", eColAction = "", eColHashMap = HM.empty}

instance Semigroup (Essence Column) where
  essenceColumn@(EssenceColumn {eColHashMap = hm1}) <> EssenceColumn {eColHashMap = hm2} =
    essenceColumn {eColHashMap = hm1 `HM.union` hm2}

instance A.FromJSON (Essence Column) where
  parseJSON =
    let getName = head . HM.keys
        toColumn v = fromMaybe defaultColumn $ AT.parseMaybe A.parseJSON v
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
  mempty = EssenceList {elName = "", elAction = "", elList = []}

instance Semigroup (Essence List) where
  essenceList@(EssenceList {elList = list1}) <> EssenceList {elList = list2} =
    essenceList {elList = list1 <> list2}
