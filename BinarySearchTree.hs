{-#LANGUAGE GADTs,StandaloneDeriving #-}
module BinarySearchTree where

import System.Vacuum.Cairo
import qualified Data.IntMap as IntMap

data  Tree a  where
  Nil :: Tree a
  Node :: (Ord a) =>  a  -> Tree a -> Tree a -> Tree a

-- data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)
deriving instance Show a => Show  (Tree a)
deriving instance Eq a => Eq (Tree a)
deriving instance (Eq a, Ord a) => Ord (Tree a)
-- instance (Show a) => Show (Tree a) where
--    show Nil = "Nil"
--    show (Node v l r) = "Node " ++ show v ++ " ( " ++ show l ++ " ) " ++ " ( " ++ show r ++ " )"

-- isEmpty :: Tree a -> Bool
isEmpty Nil = True
isEmpty (Node{}) = False
                   
data Term a where
  Zero    :: Term Int
  NonZero :: Int -> Term Int
  Succ    :: Term Int -> Term Int
  IsZero  :: Term Int -> Term Bool
  If      :: Term Bool -> Term a -> Term a -> Term a

deriving instance Show a => Show (Term a)

eval :: Term a -> a
eval Zero         = 0
eval (NonZero a)  = a
eval (Succ a)     = eval a + 1
eval (IsZero b)   = eval b == 0
eval (If b t f)
  | eval b = eval t
  | not $ eval b = eval f

