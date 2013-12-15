{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}


{-

 > fromList [10,9..1]
[1]
    L|-[2]
        L|-[3]
            L|-[4]
                L|-[5]
                    L|-[6]
                        L|-[7]
                            L|-[8]
                                L|-[9]
                                    L|-[10]


 > fromList [1..10]
[1]
    L|-[3]
        L|-[4]
        R|-[7]
            L|-[8]
    R|-[2]
        L|-[6]
            L|-[9]
        R|-[5]
            L|-[10]

-}


-- | from `Fun with BinaryHeapTrees`
module MaxiphobicBinaryHeapTree where

import           Data.Maybe
import           Text.Printf

data Tree a where
    Null :: (Ord a) => Tree a
    Fork :: (Ord a) => Int -> a -> Tree a -> Tree a -> Tree a

--deriving instance (Show a) => Show (Tree a)
deriving instance (Eq a)   => Eq   (Tree a)
instance (Show a) => Show (Tree a) where
    show = prettyPrint

-- O(1)
isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty Fork{} = False

-- O(1)
minElem :: Tree a -> a
minElem (Fork _ v _ _ ) = v

-- as merge
deleteMin :: Tree a -> Tree a
deleteMin Null = Null
deleteMin (Fork s v l r) = merge l r

-- as merge
insert :: (Ord a) => a -> Tree a -> Tree a
insert x = merge (Fork 1 x Null Null)

-- O(1)
singleton :: (Ord a) => a -> Tree a
singleton = flip insert Null

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Null

-- O(1)
size :: Tree a -> Int
size Null = 0
size (Fork n _ _ _) = n

-- N - (N-1)/3 = (2N-2)/3
-- O(log1.5N)
merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge Null t = t
merge t Null = t
merge left right
    | minElem left <= minElem right = join left right
    | otherwise = join right left


join :: (Ord a) => Tree a -> Tree a -> Tree a
join (Fork n v a b) c = Fork (n + size c) v x (merge y z)
    where
        (x, y, z) = orderBySize a b c


orderBySize a b c
    | size a == biggest = (a, b, c)
    | size b == biggest = (b, a, c)
    | size c == biggest = (c, a, b)
    where
        biggest = size a `max` size b `max` size c


prettyPrint :: (Show a) => Tree a -> String
prettyPrint t = padding 0 ' ' t
    where
        padding n c Null = "" -- printf "%s Nil\n" (prefix n c)
        padding n c (Fork s v l r) = printf "%s[%s]\n" (prefix n c) (show v)
                ++ padding (n+4) 'L' l
                ++ padding (n+4) 'R' r
        prefix n c
            | n > 0 = replicate n ' ' ++ (c:"|-")
            | otherwise = replicate n c



-- exercise 1-2
{--
fromList [1..7]
[1]
    L|-[2]
        L|-[5]
        R|-[6]
    R|-[3]
        L|-[4]
        R|-[7]


fromList [1,7,2,6,3,5,4]
[1]
    L|-[3]
        L|-[7]
        R|-[5]
    R|-[2]
        L|-[6]
        R|-[4]


deleteMin it
[2]
    L|-[3]
        L|-[4]
        R|-[7]
    R|-[5]
        L|-[6]

--}

-- exercise 1-3
-- | Explain why an unbalanced maxiphobic heap (in the extreme, a
-- | completely linear heap) is preferable to a balanced one
-- merge become faster maybe could be O(1)



