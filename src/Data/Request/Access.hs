module Data.Request.Access
    where

data Access
    = Admin
    | Author
    | Person
    | Everyone
    deriving (Eq, Show, Read)
instance Bounded Access where
    maxBound = Admin
    minBound = Everyone
instance Enum Access where
    fromEnum x = case x of
        Admin    -> 4
        Author   -> 3
        Person     -> 2
        Everyone -> 1
    toEnum x = case x of
        4 -> Admin
        3 -> Author
        2 -> Person
        1 -> Everyone
instance Ord Access where
    compare x1 x2 = compare (fromEnum x1) (fromEnum x2)