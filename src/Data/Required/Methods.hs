module Data.Required.Methods
  ( toFields
  , requiredSequenceA
  , requiredApply
  ) where

import Data.Required

type Field = String

toFields :: Required [Field] -> [Field]
toFields required =
  case required of
    AND fields -> fields
    OR fields -> fields
    Required arr -> concatMap toFields arr
    NullFields -> []

requiredSequenceA :: [Required [a]] -> Required [a]
requiredSequenceA arrRequired =
  case arrRequired of
    [] -> NullFields
    [AND x] -> AND x
    [OR x] -> OR x
    x:xs -> fmap (<>) x `requiredApply` requiredSequenceA xs

requiredApply :: Required (a -> a) -> Required a -> Required a
requiredApply required x =
  case required of
    AND func -> fmap func x
    OR func -> fmap func x
    _ -> NullFields
