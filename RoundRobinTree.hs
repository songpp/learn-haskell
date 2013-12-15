{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module RoundRobinTree where

import           Text.Printf

data Colour = Red | Blue deriving (Show, Eq)
data Tree a where
    Null :: (Ord a) => Tree a
    Fork :: (Ord a) => Colour -> a -> Tree a -> Tree a -> Tree a


--deriving instance (Show a) => Show (Tree a)
deriving instance (Eq a) => Eq (Tree a)
instance (Show a) => Show (Tree a) where
	show = prettyPrint


isEmpty Null = True
isEmpty (Fork col x a b) = False

minElem(Fork col x a b) = x

deleteMin(Fork col x a b) = merge a b


fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Null

insert x a = merge (Fork Blue x Null Null) a

-- O(logN)
--merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge a Null = a
merge Null b = b
merge a b
    | minElem a <= minElem b = join a b
    | otherwise = join b a


join (Fork Blue x a b) c = Fork Red x (merge a c) b
join (Fork Red x a b) c = Fork Blue x a (merge b c)


prettyPrint :: (Show a) => Tree a -> String
prettyPrint t = padding 0 ' ' t
    where
        padding n c Null = "" -- printf "%s Nil\n" (prefix n c)
        padding n c (Fork s v l r) = printf "%s[%s][%s]\n" (prefix n c) (show v) (show s)
                ++ padding (n+4) 'L' l
                ++ padding (n+4) 'R' r
        prefix n c
            | n > 0 = replicate n ' ' ++ (c:"|-")
            | otherwise = replicate n c
