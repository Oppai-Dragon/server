module Data.Essence.RelationsTree
  ( RelationsTree(..)
  ) where

data RelationsTree
  = Root String RelationsTree
  | Trunk String RelationsTree
  | Branch String [RelationsTree]
  | Leaf String
  | Ground
  deriving (Show, Eq)

instance Semigroup RelationsTree where
  Branch f1 _ <> Branch f2 l2 = Trunk f1 (Branch f2 l2)
  Trunk t1 branch1@(Branch _ _) <> branch2@(Branch _ _) =
    Trunk t1 (branch1 <> branch2)
  Trunk t1 (Trunk t2 rlt) <> branch@(Branch _ _) =
    Trunk t1 (Trunk t2 (rlt <> branch))
  Root r1 (Trunk t1 rlt) <> branch@(Branch _ _) =
    Root r1 (Trunk t1 (rlt <> branch))
  Root r1 branch1@(Branch _ _) <> branch2@(Branch _ _) =
    Root r1 (branch1 <> branch2)
  Root f1 (Leaf _) <> Branch f2 l2 = Root f1 (Branch f2 l2)
  _ <> _ = Ground
