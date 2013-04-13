{-#LANGUAGE GADTs #-}
module BinaryHeapTree where


data Tree a where
  Nil  :: ( Ord a) => Tree a
  Node :: ( Ord a) => a -> Tree a -> Tree a -> Tree a



-- O(1)
isEmpty :: Ord a => Tree a -> Bool
isEmpty Nil = True
isEmpty (Node v l r) = False

-- O(1)
-- minElem :: Ord a => Tree a -> a
minElem (Node value left right) = value


-- insert :: Ord a => a -> Tree a -> Tree a
insert x = merge (Node x Nil Nil)


-- deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Node v l r) = merge l r


merge ::  Ord a => Tree a -> Tree a -> Tree a
merge left Nil = left
merge Nil right = right
merge left right
  | minElem left <= minElem right  = join left right
  | otherwise                      = join right left

join = undefined                                     
