module Data.Base
    ( ifElseThen
    , reverseMap
    , map2Var
    , ioStack
    , deletePair
    , insertPair
    , mapToListOfPair
    , findText
    , scientificToInteger
    , replaceBy
    ) where

import           Data.List       as L
import           Data.Scientific
import qualified Data.Text       as T

ifElseThen :: [Bool] -> [a] -> a
ifElseThen []           [act]      = act
ifElseThen (bool:bools) (act:acts) =
    if bool
        then ifElseThen bools acts
        else act

reverseMap :: a -> [(a -> b)] -> [b]
reverseMap _   []          = []
reverseMap var (func:rest) =
    func var : reverseMap var rest

map2Var :: (a -> a -> a) -> [a] -> [a]
map2Var _    []           = []
map2Var _    [x]          = [x]
map2Var func (x1:x2:rest) =
    x1 : map2Var func (func x1 x2 : rest)

ioStack :: (a -> IO b) -> [a] -> IO ()
ioStack _    []     = return ()
ioStack func (x:xs) = func x >> ioStack func xs

deletePair :: Eq a => (a,b) -> [(a,c)] -> [(a,c)]
deletePair _     []             = []
deletePair (l,r) ((lX,rX):rest) =
    if l == lX
        then rest
        else (lX,rX) : deletePair (l,r) rest

insertPair :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
insertPair _             []                     = []
insertPair (field,value) ((fieldX,valueX):rest) =
    if field == fieldX
        then (field,value) : rest
        else (fieldX,valueX) : insertPair (field,value) rest

mapToListOfPair :: (a -> b) -> [(a,a)] -> [(b,b)]
mapToListOfPair _    []           = []
mapToListOfPair func ((l,r):rest) =
    (func l, func r) : mapToListOfPair func rest

findText :: T.Text -> [T.Text] -> Maybe T.Text
findText _    []           = Nothing
findText text (textX:rest) =
    if text == textX
        then Just textX
        else findText text rest

scientificToInteger :: Scientific -> Integer
scientificToInteger num =
    let
        numStr = show num
        exponenta = 10 ^ (read . tail . dropWhile (/='e')) numStr
        division = toInteger $ 10 ^ (length . tail . dropWhile (/='.') . takeWhile (/='e')) numStr
    in case L.find (=='e') (show num) of
        Just _  ->
            case last $ takeWhile (/='e') numStr of
                '0' -> (read . takeWhile (/='.')) numStr * exponenta
                _ -> (read . L.delete '.' . takeWhile (/='e')) numStr * exponenta `div` division
        Nothing -> read $ takeWhile (/='.') numStr

replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy func newX (x:xs) =
    if func x
        then newX:xs
        else x : replaceBy func newX xs