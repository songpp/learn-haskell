
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module SkewTree where

import           Text.Printf

data Tree a where
  Null :: ( Ord a) => Tree a
  Fork :: ( Ord a) => a -> Tree a -> Tree a -> Tree a

deriving instance (Eq a) => Eq (Tree a)
instance (Show a) => Show (Tree a) where
	show = prettyPrint


-- O(1)
isEmpty :: Ord a => Tree a -> Bool
isEmpty Null = True
isEmpty (Fork v l r) = False

-- O(1)
-- minElem :: Ord a => Tree a -> a
minElem (Fork value left right) = value


-- insert :: Ord a => a -> Tree a -> Tree a
insert x = merge (Fork x Null Null)


-- deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork v l r) = merge l r


fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Null

merge left Null = left
merge Null right = right
merge left right
  | minElem left <= minElem right  = join left right
  | otherwise                      = join right left

join(Fork x a b) c = Fork x b (merge a c)                                  


prettyPrint :: (Show a) => Tree a -> String
prettyPrint t = padding 0 ' ' t
    where
        padding n c Null = "" -- printf "%s Nil\n" (prefix n c)
        padding n c (Fork v l r) = printf "%s[%s]\n" (prefix n c) (show v)
                ++ padding (n+4) 'L' l
                ++ padding (n+4) 'R' r
        prefix n c
            | n > 0 = replicate n ' ' ++ (c:"|-")
            | otherwise = replicate n c
