{-# LANGUAGE LambdaCase #-}

module Data.Base
  ( notFoundWith
  , notFound
  , set
  , setPath
  , parsePath
  , getRepDir
  , getValue
  , ifElseThen
  , reverseMap
  , map2Var
  , ioStack
  , deletePair
  , insertPair
  , mapToListOfPair
  , findText
  , scientificToInteger
  , replaceBy
  , ordToBool
  , fromStateT
  , getRandomInteger
  , getTime
  , lookup2
  , tailCase
  , fst3
  , fst4
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Builder.Internal as BSBuilder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Time.LocalTime as LocalTime

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import qualified System.Directory as Dir
import qualified System.Random as Random

import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai

notFoundWith :: BSBuilder.Builder -> Wai.Response
notFoundWith = Wai.responseBuilder HTTPTypes.status404 []

notFound :: Wai.Response
notFound = notFoundWith "Not found"

set :: IO FilePath -> IO A.Object
set ioPath =
  ioPath >>= BSL.readFile >>= pure . A.decode >>= \x ->
    case x of
      Just hm -> pure hm
      Nothing -> pure HM.empty

setPath :: FilePath -> IO FilePath
setPath path = fmap (flip (<>) $ "\\src\\" <> path) getRepDir

parsePath :: FilePath -> FilePath
parsePath =
  L.intercalate "\\" .
  takeWhile (/= "src") .
  L.words .
  L.intercalate "" .
  map
    (\case
       "\\" -> " "
       x -> x) .
  L.group

getRepDir :: IO FilePath
getRepDir = parsePath <$> Dir.getCurrentDirectory

getValue :: [T.Text] -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null

ifElseThen :: Applicative f => [Bool] -> [f a] -> f a
ifElseThen [] [act] = act
ifElseThen (bool:bools) (act:acts) =
  if bool
    then ifElseThen bools acts
    else act
ifElseThen _ _ = error "ifElseThen bad arguments"

reverseMap :: a -> [(a -> b)] -> [b]
reverseMap _ [] = []
reverseMap var (func:rest) = func var : reverseMap var rest

map2Var :: (a -> a -> a) -> [a] -> [a]
map2Var _ [] = []
map2Var _ [x] = [x]
map2Var func (x1:x2:rest) = x1 : map2Var func (func x1 x2 : rest)

ioStack :: (a -> IO b) -> [a] -> IO ()
ioStack _ [] = return ()
ioStack func (x:xs) = func x >> ioStack func xs

deletePair :: Eq a => (a, b) -> [(a, c)] -> [(a, c)]
deletePair _ [] = []
deletePair (l, r) ((lX, rX):rest) =
  if l == lX
    then rest
    else (lX, rX) : deletePair (l, r) rest

insertPair :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
insertPair _ [] = []
insertPair (field, value) ((fieldX, valueX):rest) =
  if field == fieldX
    then (field, value) : rest
    else (fieldX, valueX) : insertPair (field, value) rest

mapToListOfPair :: (a -> b) -> [(a, a)] -> [(b, b)]
mapToListOfPair _ [] = []
mapToListOfPair func ((l, r):rest) =
  (func l, func r) : mapToListOfPair func rest

findText :: T.Text -> [T.Text] -> Maybe T.Text
findText _ [] = Nothing
findText text (textX:rest) =
  if text == textX
    then Just textX
    else findText text rest

scientificToInteger :: Scientific.Scientific -> Integer
scientificToInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON . A.Number

replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy _ _ [] = []
replaceBy func newX (x:xs) =
  if func x
    then newX : xs
    else x : replaceBy func newX xs

ordToBool :: Ordering -> Bool
ordToBool EQ = True
ordToBool _ = False

fromStateT :: (Monad m) => m a -> StateT s (ReaderT c m) a
fromStateT = lift . lift

getRandomInteger :: IO Integer
getRandomInteger = Random.getStdRandom (Random.randomR (1, 100000000000))

getTime :: IO String
getTime = do
  zonedTime <- LocalTime.getZonedTime
  let zonedTimeStr = show zonedTime
  let time = L.takeWhile (/= '.') . tail $ L.dropWhile (/= ' ') zonedTimeStr
  return time

lookup2 :: (Eq a, Eq b) => a -> b -> [(a, [(b, c)])] -> Maybe c
lookup2 field1 field2 arr1 =
  case lookup field1 arr1 of
    Just arr2 -> lookup field2 arr2
    Nothing -> Nothing

tailCase :: [a] -> [a]
tailCase arr =
  case arr of
    [] -> []
    _ -> tail arr

fst3 :: (a, b, c) -> a
fst3 (x1, _, _) = x1

fst4 :: (a, b, c, d) -> a
fst4 (x1, _, _, _) = x1
