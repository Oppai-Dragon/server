{-# LANGUAGE LambdaCase #-}

module Data.Base.Path
  ( getEssenceArr
  , setPath
  , parsePath
  , getRepDir
  ) where

import Data.List
import qualified System.Directory as Dir

getEssenceArr :: FilePath -> IO [FilePath]
getEssenceArr = (=<<) (fmap (map $ takeWhile (/= '.')) . Dir.listDirectory) . setPath

setPath :: FilePath -> IO FilePath
setPath path = fmap (flip (<>) $ "\\src\\" <> path) getRepDir

parsePath :: FilePath -> FilePath
parsePath =
  intercalate "\\" .
  takeWhile (/= "src") .
  words .
  intercalate "" .
  map
    (\case
       "\\" -> " "
       x -> x) .
  group

getRepDir :: IO FilePath
getRepDir = parsePath <$> Dir.getCurrentDirectory
