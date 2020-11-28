module Data.Base.Prelude
  ( deletePair
  , replaceBy
  , ordToBool
  , maybeToBool
  , lookup2
  , tailCase
  ) where

deletePair :: Eq a => (a, b) -> [(a, c)] -> [(a, c)]
deletePair _ [] = []
deletePair (l, r) ((lX, rX):rest) =
  if l == lX
    then rest
    else (lX, rX) : deletePair (l, r) rest

replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy _ _ [] = []
replaceBy func newX (x:xs) =
  if func x
    then newX : xs
    else x : replaceBy func newX xs

maybeToBool :: Maybe a -> Bool
maybeToBool Nothing = False
maybeToBool (Just _) = True

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
