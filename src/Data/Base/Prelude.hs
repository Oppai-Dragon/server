module Data.Base.Prelude
  ( ifElseThen
  , reverseMap
  , map2Var
  , ioStack
  , deletePair
  , insertPair
  , mapToListOfPair
  , replaceBy
  , ordToBool
  , lookup2
  , tailCase
  , fst3
  , fst4
  ) where

ifElseThen :: Applicative f => [Bool] -> [f a] -> f a
ifElseThen [] [act] = act
ifElseThen (bool:bools) (act:acts) =
  if bool
    then ifElseThen bools acts
    else act
ifElseThen _ _ = error "ifElseThen bad arguments"

reverseMap :: a -> [a -> b] -> [b]
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

replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy _ _ [] = []
replaceBy func newX (x:xs) =
  if func x
    then newX : xs
    else x : replaceBy func newX xs

ordToBool :: Ordering -> Bool
ordToBool EQ = True
ordToBool _ = False

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
