module Data.Essence.RelationsTree
    ( RelationsTree (..)
    ) where

data RelationsTree a
    = Root a (RelationsTree a)
    | Trunk a (RelationsTree a)
    | Branch a [RelationsTree a]
    | Leaf a
    | Ground
    deriving (Show, Eq)

instance Semigroup (RelationsTree a) where
    Branch f1 l1 <> Branch f2 l2 = Trunk f1 (Branch f2 l2)
    Trunk t1 branch1@(Branch b1 l1) <> branch2@(Branch b2 l2) = Trunk t1 (branch1 <> branch2)
    Trunk t1 (Trunk t2 rlt) <> branch@(Branch b2 l2) = Trunk t1 (Trunk t2 (rlt <> branch))
    Root r1 trunk@(Trunk t1 rlt) <> branch@(Branch b2 l2) = Root r1 (Trunk t1 (rlt <> branch))
    Root r1 branch1@(Branch b1 l1) <> branch2@(Branch b2 l2) = Root r1 (branch1 <> branch2)
    Root f1 (Leaf l1) <> Branch f2 l2 = Root f1 (Branch f2 l2)