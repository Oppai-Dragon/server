{-# LANGUAGE FlexibleInstances #-}

module Data.SQL.AlterTable
  ( AlterAction
  , AlterTable(..)
  , getAlterQueries
  , getAlterAction
  ) where

import Data.Essence.Column

import Control.Monad
import Control.Monad.Trans.Writer.CPS

type SqlQuery = String

type Table = String

type ColumnName = String

type ColumnLocal = Column

type ColumnDatabase = Column

data AlterAction
  = ADD
  | DROP
  | ALTER
  | NOTHING

class AlterTable a where
  alterTable :: Table -> ColumnName -> AlterAction -> a -> SqlQuery

instance AlterTable ValueType where
  alterTable table column _ x =
    "ALTER TABLE " <>
    table <> " ALTER COLUMN " <> column <> " SET DATA TYPE " <> show x <> ";"

instance AlterTable (Maybe NULL) where
  alterTable table column ADD (Just (NOT NULL)) =
    "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " SET NOT NULL;"
  alterTable table column ADD (Just NULL) =
    alterTable table column DROP (Just (NOT NULL))
  alterTable table column DROP _ =
    "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " DROP NOT NULL;"
  alterTable _ _ NOTHING _ = ""
  alterTable _ _ _ x = "Can't Data.SQL.AlterTable.alterTable " <> show x

instance AlterTable (Maybe Default) where
  alterTable table column ADD (Just x) =
    "ALTER TABLE " <>
    table <> " ALTER COLUMN " <> column <> " SET " <> show x <> ";"
  alterTable table column DROP Nothing =
    "ALTER TABLE " <> table <> " ALTER COLUMN " <> column <> " DROP DEFAULT;"
  alterTable _ _ NOTHING _ = ""
  alterTable _ _ _ x = "Can't Data.SQL.AlterTable.alterTable " <> show x

instance AlterTable (Maybe Relations) where
  alterTable table column ADD (Just x) =
    "ALTER TABLE " <>
    table <>
    " ADD CONSTRAINT " <>
    table <>
    "_" <>
    column <> ";" <> "_fkey FOREIGN KEY (" <> column <> ") " <> show x <> ";"
  alterTable table column DROP _ =
    "ALTER TABLE " <>
    table <> " DROP CONSTRAINT IF EXISTS " <> table <> "_" <> column <> "_fkey;"
  alterTable _ _ NOTHING _ = ""
  alterTable _ _ _ x = "Can't Data.SQL.AlterTable.alterTable " <> show x

instance AlterTable (Maybe Constraint) where
  alterTable table column ADD (Just PrimaryKey) =
    "ALTER TABLE " <>
    table <>
    " ADD CONSTRAINT " <> table <> "_pkey PRIMARY KEY (" <> column <> ");"
  alterTable table _ DROP (Just PrimaryKey) =
    "ALTER TABLE " <> table <> " DROP CONSTRAINT " <> table <> "_pkey;"
  alterTable table column ADD (Just UNIQUE) =
    "ALTER TABLE " <>
    table <>
    " ADD CONSTRAINT " <>
    table <> "_" <> "column" <> "_unique UNIQUE (" <> column <> ");"
  alterTable table _ DROP (Just UNIQUE) =
    "ALTER TABLE " <>
    table <> " DROP CONSTRAINT " <> table <> "_" <> "column" <> "_unique;"
  alterTable _ _ NOTHING _ = ""
  alterTable _ _ _ x = "Can't Data.SQL.AlterTable.alterTable " <> show x

getAlterQueries ::
     Table -> ColumnName -> ColumnLocal -> ColumnDatabase -> Writer String ()
getAlterQueries table columnName columnLoc columnDb = do
  when (cValueType columnLoc /= cValueType columnDb) .
    tell . alterTable table columnName ALTER $
    cValueType columnLoc
  tell .
    alterTable
      table
      columnName
      (getAlterAction (cNULL columnLoc) $ cNULL columnDb) $
    cNULL columnLoc
  tell .
    alterTable
      table
      columnName
      (getAlterAction (cDefault columnLoc) $ cDefault columnDb) $
    cDefault columnLoc
  case getAlterAction (cConstraint columnLoc) $ cConstraint columnDb of
    ADD -> tell . alterTable table columnName ADD $ cConstraint columnLoc
    DROP -> tell . alterTable table columnName DROP $ cConstraint columnDb
    ALTER -> do
      tell . alterTable table columnName DROP $ cConstraint columnDb
      tell . alterTable table columnName ADD $ cConstraint columnLoc
    NOTHING -> return ()
  case getAlterAction (cConstraint columnLoc) $ cConstraint columnDb of
    ADD -> tell . alterTable table columnName ADD $ cRelations columnLoc
    DROP -> tell . alterTable table columnName DROP $ cRelations columnDb
    ALTER -> do
      tell . alterTable table columnName DROP $ cRelations columnDb
      tell . alterTable table columnName ADD $ cRelations columnLoc
    NOTHING -> return ()
  case getAlterAction (cConstraint columnLoc) $ cConstraint columnDb of
    DROP -> do
      tell . alterTable table columnName DROP $ cRelations columnLoc
      tell . alterTable table columnName ADD $ cRelations columnLoc
    NOTHING -> return ()
    _ -> do
      tell . alterTable table columnName DROP $ cRelations columnLoc
      tell $
        init (alterTable table columnName ADD $ cRelations columnLoc) <>
        (show . cAction) columnLoc <> ";"

getAlterAction :: Eq a => Maybe a -> Maybe a -> AlterAction
getAlterAction Nothing (Just _) = DROP
getAlterAction (Just _) Nothing = ADD
getAlterAction (Just x1) (Just x2) =
  if x1 /= x2
    then ALTER
    else NOTHING
getAlterAction Nothing Nothing = NOTHING
