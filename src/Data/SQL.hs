{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.SQL
  ( SqlQuery(..)
  , Clause(..)
  ) where

import Data.MyValue

data SqlQuery
  = Insert
      { insertTable :: String
      , insertFields :: [String]
      , insertMyValues :: [MyValue]
      }
  | Edit
      { editTable :: String
      , editSetPart :: [Clause String]
      , editWherePart :: [Clause String]
      }
  | Get
      { getTable :: String
      , getClauses :: [Clause String]
      }
  | Delete
      { deleteTable :: String
      , deleteWherePart :: [Clause String]
      }
  deriving (Show, Eq)

data family Clause a

data instance  Clause String = Set{setPair :: (String, MyValue)}
                             | Where{wherePair :: (String, MyValue)}
                             | Filter{filterClause :: String}
                             | OrderBy{orderByClause :: String}
                             | OffsetLimit{offsetLimitClause :: String}
                                 deriving (Show, Eq)

data instance  Clause [String] = SetList{setList ::
                                         [(String, String)]}
                               | WhereList{whereList :: [(String, String)]}
                               | FilterList{filterClauseList :: [String]}
                               | OrderByList{orderByClauseList :: [String]}
                               | OffsetLimitList{offsetLimitClauseList :: [String]}
                                   deriving (Show, Eq)

instance Ord (Clause String) where
  compare (Set _) (Set _) = EQ
  compare (Where _) (Where _) = EQ
  compare (Filter _) (Filter _) = EQ
  compare (OrderBy _) (OrderBy _) = EQ
  compare (OffsetLimit _) (OffsetLimit _) = EQ
  compare (Set _) _ = LT
  compare (Where _) (Set _) = GT
  compare (Where _) _ = LT
  compare (Filter _) (Set _) = GT
  compare (Filter _) _ = LT
  compare (OrderBy _) (Set _) = GT
  compare (OrderBy _) (Where _) = GT
  compare (OrderBy _) (Filter _) = GT
  compare (OrderBy _) _ = LT
  compare (OffsetLimit _) (Set _) = GT
  compare (OffsetLimit _) (Where _) = GT
  compare (OffsetLimit _) (Filter _) = GT
  compare (OffsetLimit _) (OrderBy _) = GT

instance Ord (Clause [String]) where
  compare (SetList _) (SetList _) = EQ
  compare (WhereList _) (WhereList _) = EQ
  compare (FilterList _) (FilterList _) = EQ
  compare (OrderByList _) (OrderByList _) = EQ
  compare (SetList _) _ = LT
  compare (WhereList _) (SetList _) = GT
  compare (WhereList _) _ = LT
  compare (FilterList _) (SetList _) = GT
  compare (FilterList _) (WhereList _) = GT
  compare (FilterList _) _ = LT
  compare (OrderByList _) (SetList _) = GT
  compare (OrderByList _) (WhereList _) = GT
  compare (OrderByList _) (FilterList _) = GT
  compare (OrderByList _) _ = LT
  compare (OffsetLimitList _) (SetList _) = GT
  compare (OffsetLimitList _) (WhereList _) = GT
  compare (OffsetLimitList _) (FilterList _) = GT
  compare (OffsetLimitList _) (OrderByList _) = GT
  compare (OffsetLimitList _) _ = LT
